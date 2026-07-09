;;;; escapes.lisp --- C-string escape decoding and printf-style formatting.

(in-package #:magic)

(defun hex-digit-p (ch)
  (digit-char-p ch 16))

(defun decode-string-escapes (string)
  "Decode the C-style escapes in STRING (as they appear in a magic test value)
into an octet-vector.  Handles \\n \\r \\t \\f \\v \\a \\b, \\xHH, \\ooo octal,
escaped space, and \\\\."
  (let ((out (make-array (length string) :element-type 'octet
                                          :adjustable t :fill-pointer 0))
        (i 0)
        (n (length string)))
    (flet ((emit (b) (vector-push-extend (logand #xff b) out)))
      (loop while (< i n) do
        (let ((ch (char string i)))
          (if (char/= ch #\\)
              (progn (emit (char-code ch)) (incf i))
              (progn
                (incf i)
                (when (>= i n) (emit (char-code #\\)) (return))
                (let ((c (char string i)))
                  (case c
                    (#\n (emit 10) (incf i))
                    (#\r (emit 13) (incf i))
                    (#\t (emit 9) (incf i))
                    (#\f (emit 12) (incf i))
                    (#\v (emit 11) (incf i))
                    (#\a (emit 7) (incf i))
                    (#\b (emit 8) (incf i))
                    (#\\ (emit (char-code #\\)) (incf i))
                    (#\Space (emit 32) (incf i))
                    ((#\x #\X)
                     (incf i)
                     (let ((start i))
                       (loop while (and (< i n) (< (- i start) 2)
                                        (hex-digit-p (char string i)))
                             do (incf i))
                       (if (> i start)
                           (emit (parse-integer string :start start :end i :radix 16))
                           (emit (char-code #\x)))))
                    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
                     (let ((start i))
                       (loop while (and (< i n) (< (- i start) 3)
                                        (digit-char-p (char string i) 8))
                             do (incf i))
                       (emit (parse-integer string :start start :end i :radix 8))))
                    (t (emit (char-code c)) (incf i)))))))))
    (coerce out 'octet-vector)))

(defun decode-message-escapes (string)
  "Decode escapes in a message STRING into a display string.  Returns two
values: the decoded text and a flag that is true when the message began with
\\b (meaning: emit it with no separating space)."
  (let ((no-space nil)
        (start 0))
    (when (and (>= (length string) 2)
               (char= (char string 0) #\\)
               (char= (char string 1) #\b))
      (setf no-space t start 2))
    (let ((out (make-string-output-stream))
          (i start)
          (n (length string)))
      (loop while (< i n) do
        (let ((ch (char string i)))
          (if (char/= ch #\\)
              (progn (write-char ch out) (incf i))
              (progn
                (incf i)
                (when (>= i n) (write-char #\\ out) (return))
                (let ((c (char string i)))
                  (case c
                    (#\n (write-char #\Newline out) (incf i))
                    (#\r (write-char #\Return out) (incf i))
                    (#\t (write-char #\Tab out) (incf i))
                    (#\b (write-char #\Backspace out) (incf i))
                    (#\\ (write-char #\\ out) (incf i))
                    (#\Space (write-char #\Space out) (incf i))
                    ((#\x #\X)
                     (incf i)
                     (let ((s i))
                       (loop while (and (< i n) (< (- i s) 2) (hex-digit-p (char string i)))
                             do (incf i))
                       (if (> i s)
                           (write-char (code-char (parse-integer string :start s :end i :radix 16)) out)
                           (write-char #\x out))))
                    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
                     (let ((s i))
                       (loop while (and (< i n) (< (- i s) 3) (digit-char-p (char string i) 8))
                             do (incf i))
                       (write-char (code-char (parse-integer string :start s :end i :radix 8)) out)))
                    (t (write-char c out) (incf i))))))))
      (values (get-output-stream-string out) no-space))))

;;; ---------------------------------------------------------------------------
;;; printf subset

(defparameter *printf-scanner*
  (cl-ppcre:create-scanner "%([-+ 0#]*)([0-9]*)(?:\\.([0-9]+))?(hh|h|ll|l|L|q|j|z|t)?([diouxXeEfgGcsp%])")
  "Matches a single printf conversion, capturing flags, width, precision,
length modifier, and the conversion character.")

(defun %render-conversion (conv flags width precision value)
  "Render VALUE for a single printf CONVERSION character with FLAGS/WIDTH/
PRECISION strings (as captured from the format)."
  (let* ((width (and (plusp (length width)) (parse-integer width)))
         (precision (and precision (parse-integer precision)))
         (left (find #\- flags))
         (zero (and (find #\0 flags) (not left)))
         (plus (find #\+ flags))
         (space (find #\Space flags))
         (alt (find #\# flags)))
    (labels ((pad (s)
               (if (and width (< (length s) width))
                   (let ((fill (make-string (- width (length s))
                                            :initial-element (if zero #\0 #\Space))))
                     (if left (concatenate 'string s fill)
                         (if (and zero (plusp (length s))
                                  (member (char s 0) '(#\- #\+ #\Space)))
                             ;; keep sign in front of zero padding
                             (concatenate 'string (subseq s 0 1) fill (subseq s 1))
                             (concatenate 'string fill s))))
                   s))
             (signed (n s)
               (cond ((and (>= n 0) plus) (concatenate 'string "+" s))
                     ((and (>= n 0) space) (concatenate 'string " " s))
                     (t s))))
      (case conv
        (#\% "%")
        ((#\d #\i)
         (let ((n (round-to-int value)))
           (pad (signed n (format nil "~D" n)))))
        (#\u (pad (format nil "~D" (max 0 (round-to-int value)))))
        (#\x (let ((s (format nil "~(~X~)" (round-to-int value))))
               (pad (if alt (concatenate 'string "0x" s) s))))
        (#\X (let ((s (format nil "~:@(~X~)" (round-to-int value))))
               (pad (if alt (concatenate 'string "0X" s) s))))
        (#\o (let ((s (format nil "~O" (round-to-int value))))
               (pad (if alt (concatenate 'string "0" s) s))))
        (#\c (pad (etypecase value
                    (integer (string (code-char (logand #xff value))))
                    (character (string value))
                    (string value))))
        (#\s (let ((s (if (stringp value) value (princ-to-string value))))
               (pad (if precision (subseq s 0 (min precision (length s))) s))))
        (#\p (pad (format nil "0x~(~X~)" (round-to-int value))))
        ((#\e #\E #\f #\g #\G)
         (pad (format nil "~F" (float (if (numberp value) value 0) 1d0))))
        (t (format nil "~A" value))))))

(defun round-to-int (value)
  (cond ((integerp value) value)
        ((numberp value) (round value))
        ((characterp value) (char-code value))
        (t 0)))

(defun printf-format (fmt value)
  "Substitute VALUE into the printf-style directives of FMT, returning a string.
Every conversion in FMT (normally there is exactly one) receives VALUE."
  (cl-ppcre:regex-replace-all
   *printf-scanner* fmt
   (lambda (match &rest regs)
     (declare (ignore match))
     (destructuring-bind (flags width precision length conv) regs
       (declare (ignore length))
       (%render-conversion (char conv 0)
                           (or flags "") (or width "") precision value)))
   :simple-calls t))

(defun message-has-directive-p (fmt)
  "True when FMT contains a printf conversion that consumes a value."
  (cl-ppcre:scan "%[-+ 0#]*[0-9]*(?:\\.[0-9]+)?(?:hh|h|ll|l|L|q|j|z|t)?[diouxXeEfgGcsp]" fmt))
