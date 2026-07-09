;;;; parser.lisp --- Parse the file(1) magic source DSL into entry trees.

(in-package #:magic)

(define-condition magic-parse-error (magic-error)
  ((line :initarg :line :reader magic-parse-error-line :initform nil)
   (source :initarg :source :reader magic-parse-error-source :initform nil)))

;;; ---------------------------------------------------------------------------
;;; Small string helpers

(declaim (inline ws-char-p))
(defun ws-char-p (ch) (or (char= ch #\Space) (char= ch #\Tab)))

(defun read-field (line start)
  "Read one whitespace-delimited field from LINE beginning at or after START.
Whitespace inside a field must be backslash-escaped and is kept verbatim.
Returns (values token end-index); TOKEN is NIL when only whitespace remains."
  (let ((n (length line)) (i start))
    (loop while (and (< i n) (ws-char-p (char line i))) do (incf i))
    (when (>= i n) (return-from read-field (values nil i)))
    (let ((out (make-string-output-stream)))
      (loop while (< i n) do
        (let ((ch (char line i)))
          (cond
            ((char= ch #\\)
             (write-char ch out)
             (incf i)
             (when (< i n) (write-char (char line i) out) (incf i)))
            ((ws-char-p ch) (return))
            (t (write-char ch out) (incf i)))))
      (values (get-output-stream-string out) i))))

(defun split-on (string char)
  "Split STRING into a list of substrings on each occurrence of CHAR."
  (let ((parts '()) (start 0))
    (loop for pos = (position char string :start start)
          do (push (subseq string start pos) parts)
             (if pos (setf start (1+ pos)) (return)))
    (nreverse parts)))

;;; ---------------------------------------------------------------------------
;;; Offset representation
;;;
;;; See MAGIC(5) and file's apprentice.c/softmagic.c.  An offset is either a
;;; constant, a value relative to the end of the parent match, or an indirect
;;; read from the file whose result may be adjusted arithmetically.

(defstruct (moffset (:conc-name moff-))
  (base 0 :type integer)        ; the literal offset value
  (from-end nil)                ; negative level-0 offset counted from EOF
  (relative nil)                ; add parent's end offset to the final result
  (base-relative nil)           ; add parent's end offset to the indirect base
  (indirect nil)                ; read the true offset out of the file
  ;; indirect read description
  (in-size 4) (in-endian :native) (in-signed nil)
  (in-invert nil)               ; complement the value read (~)
  (in-op nil)                   ; arithmetic op char applied to the read value
  (in-op-indirect nil)          ; the displacement is itself read from the file
  (disp 0 :type integer))       ; displacement / operand

;;; Indirect read type characters, per apprentice.c: (size endian signed-hint)
(defparameter *indirect-type-chars*
  '((#\l 4 :little) (#\L 4 :big) (#\m 4 :middle)
    (#\h 2 :little) (#\s 2 :little) (#\H 2 :big) (#\S 2 :big)
    (#\c 1 :native) (#\b 1 :native) (#\C 1 :native) (#\B 1 :native)
    (#\i 4 :little) (#\I 4 :big)
    (#\q 8 :little) (#\Q 8 :big)
    (#\e 8 :little) (#\f 8 :little) (#\g 8 :little)
    (#\E 8 :big) (#\F 8 :big) (#\G 8 :big)))

(defun parse-c-integer (string &key (start 0))
  "Parse a C-style integer literal from STRING beginning at START.  Recognises
0x hex, leading-0 octal, decimal, a leading sign, and a leading char literal
'c'.  Returns (values integer end-position)."
  (let ((n (length string)))
    (when (>= start n) (return-from parse-c-integer (values 0 start)))
    (when (char= (char string start) #\')      ; character constant
      (let ((bytes (decode-string-escapes
                    (subseq string (1+ start)
                            (or (position #\' string :start (1+ start)) n)))))
        (return-from parse-c-integer
          (values (if (plusp (length bytes)) (aref bytes 0) 0)
                  (min n (+ 2 start (1- (or (position #\' string :start (1+ start))
                                            (1- n)))))))))
    (let* ((sign 1) (i start))
      (case (and (< i n) (char string i))
        (#\- (setf sign -1) (incf i))
        (#\+ (incf i)))
      (let ((radix 10))
        (when (and (< (1+ i) n) (char= (char string i) #\0)
                   (member (char string (1+ i)) '(#\x #\X)))
          (setf radix 16) (incf i 2))
        (when (and (= radix 10) (< (1+ i) n) (char= (char string i) #\0)
                   (digit-char-p (char string (1+ i)) 8))
          (setf radix 8) (incf i))
        (let ((digit-start i))
          (loop while (and (< i n) (digit-char-p (char string i) radix)) do (incf i))
          (if (> i digit-start)
              (values (* sign (parse-integer string :start digit-start :end i :radix radix)) i)
              (values 0 i)))))))

(defun parse-offset (string level)
  "Parse the offset field STRING (at continuation LEVEL) into an MOFFSET."
  (let ((off (make-moffset))
        (i 0) (n (length string))
        (pre-amp nil) (post-amp nil))
    (when (and (< i n) (char= (char string i) #\&))
      (setf pre-amp t) (incf i))
    (when (and (< i n) (char= (char string i) #\())
      (setf (moff-indirect off) t) (incf i)
      (when (and (< i n) (char= (char string i) #\&))
        (setf post-amp t) (incf i)))
    ;; the C code: '&' before '(' becomes INDIROFFADD (add to final result);
    ;; '&' after '(' is OFFADD on the base.
    (if (moff-indirect off)
        (setf (moff-relative off) pre-amp
              (moff-base-relative off) post-amp)
        (setf (moff-relative off) pre-amp))
    ;; base value
    (multiple-value-bind (val end) (parse-c-integer string :start i)
      (setf (moff-base off) val i end)
      (when (and (< val 0) (= level 0) (not (moff-relative off)))
        (setf (moff-from-end off) t (moff-base off) (- val))))
    (when (moff-indirect off)
      ;; [.,type][~][op][(]disp
      (when (and (< i n) (member (char string i) '(#\. #\,)))
        (when (char= (char string i) #\,) (setf (moff-in-signed off) t))
        (incf i)
        (when (< i n)
          (let ((spec (assoc (char string i) *indirect-type-chars*)))
            (when spec
              (setf (moff-in-size off) (second spec)
                    (moff-in-endian off) (third spec)))
            (incf i))))
      (when (and (< i n) (char= (char string i) #\~))
        (setf (moff-in-invert off) t) (incf i))
      (when (and (< i n) (member (char string i) '(#\+ #\- #\* #\/ #\% #\& #\| #\^)))
        (setf (moff-in-op off) (char string i)) (incf i))
      (when (and (< i n) (char= (char string i) #\())
        (setf (moff-in-op-indirect off) t) (incf i))
      (when (and (< i n) (or (digit-char-p (char string i))
                             (member (char string i) '(#\+ #\-))))
        (multiple-value-bind (val end) (parse-c-integer string :start i)
          (setf (moff-disp off) val i end))))
    off))

;;; ---------------------------------------------------------------------------
;;; Entry representation

(defstruct (entry (:conc-name ent-))
  (level 0 :type fixnum)
  offset                        ; an MOFFSET
  type                          ; an MTYPE
  (unsigned nil)                ; force unsigned comparison
  (mask nil)                    ; numeric AND-mask
  (str-flags "")                ; flag characters for string/search/regex
  (str-range nil)               ; search count / regex length
  (operator #\=)                ; comparison operator character
  test-value                    ; parsed comparison value
  (raw-type "")                 ; original type token (for diagnostics)
  message                       ; raw message string (may be NIL)
  name                          ; referenced name for name/use, or der type
  (use-flip nil)                ; USE with ^ prefix: swap endianness
  (mask-op #\&)                 ; operator combining the value with the mask
  (mask-invert nil)             ; complement after masking (~)
  ;; annotations
  mime ext apple strength
  (compiled nil)                ; cached cl-ppcre scanner for regex entries
  (children nil))               ; list of child entries (level+1)

(defun parse-type-token (token entry)
  "Fill the type-related slots of ENTRY from the type field TOKEN."
  (setf (ent-raw-type entry) token)
  (let* ((parts (split-on token #\/))
         (head (first parts))
         (flag-parts (rest parts)))
    ;; numeric mask:  type<op>[~]mask, where op is one of & | ^ + - * / %
    (let ((opos (position-if (lambda (c) (member c '(#\& #\| #\^ #\+ #\- #\* #\/ #\%)))
                             head :start 1)))
      (when opos
        (setf (ent-mask-op entry) (char head opos))
        (let ((mstart (1+ opos)))
          (when (and (< mstart (length head)) (char= (char head mstart) #\~))
            (setf (ent-mask-invert entry) t) (incf mstart))
          (setf (ent-mask entry) (parse-c-integer head :start mstart)))
        (setf head (subseq head 0 opos))))
    ;; base type, with unsigned-prefix fallback
    (let ((mt (lookup-type head)))
      (when (and (null mt) (> (length head) 1) (char= (char head 0) #\u))
        (let ((inner (lookup-type (subseq head 1))))
          (when inner (setf mt inner (ent-unsigned entry) t))))
      (unless mt
        (error 'magic-parse-error :message (format nil "unknown type ~S" token)))
      (setf (ent-type entry) mt)
      (when (member (mtype-category mt) '(:numeric :date))
        (unless (mtype-signed mt) (setf (ent-unsigned entry) t))))
    ;; flag / range parts (for string, search, regex, pstring)
    (dolist (part flag-parts)
      (let ((digits (make-string-output-stream))
            (letters (make-string-output-stream)))
        (loop for ch across part
              do (if (digit-char-p ch)
                     (write-char ch digits)
                     (write-char ch letters)))
        (let ((d (get-output-stream-string digits))
              (l (get-output-stream-string letters)))
          (when (plusp (length d)) (setf (ent-str-range entry) (parse-integer d)))
          (setf (ent-str-flags entry)
                (concatenate 'string (ent-str-flags entry) l)))))))

(defun string-type-p (entry)
  (member (mtype-category (ent-type entry))
          '(:string :pstring :search :regex :der :guid)))

(defun parse-test-token (token entry)
  "Parse the test field TOKEN, setting operator and test value on ENTRY."
  (let ((cat (mtype-category (ent-type entry))))
    (case cat
      ((:name :use)
       (let ((name token))
         (when (and (plusp (length name)) (char= (char name 0) #\^))
           (setf (ent-use-flip entry) t name (subseq name 1)))
         (setf (ent-name entry) name (ent-operator entry) #\=)))
      ((:default :clear :indirect)
       (setf (ent-operator entry) #\x))
      (:der
       (setf (ent-name entry) token (ent-operator entry) #\x))
      (t
       ;; leading relational / bitwise operator
       (let ((op #\=) (start 0))
         (when (plusp (length token))
           (let ((c (char token 0)))
             (cond ((string= token "x") (setf op #\x start 1))
                   ((member c '(#\= #\< #\> #\& #\^ #\~ #\!))
                    (setf op c start 1)))))
         (setf (ent-operator entry) op)
         (cond
           ((char= op #\x) (setf (ent-test-value entry) nil))
           ((member cat '(:numeric :date :offset))
            (setf (ent-test-value entry)
                  (parse-c-integer token :start start)))
           ((eq cat :float)
            (setf (ent-test-value entry)
                  (let ((*read-default-float-format* 'double-float))
                    (ignore-errors (read-from-string (subseq token start) nil 0)))))
           ((eq cat :regex)
            (multiple-value-bind (text) (decode-message-escapes (subseq token start))
              (setf (ent-test-value entry) text)))
           (t                          ; string-like: decode to bytes
            (setf (ent-test-value entry)
                  (decode-string-escapes (subseq token start))))))))))

(defun parse-magic-line (line)
  "Parse a single non-comment magic LINE into an ENTRY, or NIL if it is a
blank line.  Signals MAGIC-PARSE-ERROR on malformed input."
  (let ((n (length line)) (i 0))
    ;; leading '>' continuation markers
    (let ((level 0))
      (loop while (and (< i n) (char= (char line i) #\>)) do (incf level) (incf i))
      (multiple-value-bind (offset-tok j) (read-field line i)
        (when (null offset-tok) (return-from parse-magic-line nil))
        (multiple-value-bind (type-tok k) (read-field line j)
          (when (null type-tok)
            (error 'magic-parse-error :line line :message "missing type field"))
          (multiple-value-bind (test-tok m) (read-field line k)
            (let* ((entry (make-entry :level level)))
              (setf (ent-offset entry) (parse-offset offset-tok level))
              (parse-type-token type-tok entry)
              ;; some types (name/use/default/clear) still consume a test token
              (parse-test-token (or test-tok "x") entry)
              ;; message is the remainder
              (let ((msg (and m (string-left-trim '(#\Space #\Tab)
                                                  (subseq line (min m n))))))
                (when (and msg (plusp (length msg)))
                  (setf (ent-message entry) msg)))
              entry)))))))

(defun apply-directive (line entry)
  "Apply a !: annotation LINE to the most recent ENTRY."
  (when entry
    (let* ((rest (string-left-trim '(#\Space #\Tab) (subseq line 2)))
           (sp (position-if (lambda (c) (member c '(#\Space #\Tab))) rest))
           (key (subseq rest 0 sp))
           (val (and sp (string-trim '(#\Space #\Tab) (subseq rest sp)))))
      (cond
        ((string= key "mime") (setf (ent-mime entry) val))
        ((string= key "ext") (setf (ent-ext entry) val))
        ((string= key "apple") (setf (ent-apple entry) val))
        ((string= key "strength")
         (when (and val (plusp (length val)))
           (setf (ent-strength entry)
                 (cons (char val 0)
                       (or (ignore-errors
                            (parse-integer (string-trim '(#\Space #\Tab) (subseq val 1))))
                           0)))))))))

(defun parse-magic-source (text)
  "Parse an entire magic source TEXT (a string) into a list of top-level
entries, nesting continuation lines under their parents."
  (let ((roots '())
        (stack (make-array 40 :initial-element nil))  ; entry per level
        (last-entry nil))
    (with-input-from-string (in text)
      (loop for line = (read-line in nil :eof)
            for lineno from 1
            until (eq line :eof)
            do (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
                 (cond
                   ((zerop (length trimmed)))            ; blank
                   ((char= (char trimmed 0) #\#))        ; comment
                   ((and (>= (length trimmed) 2)         ; annotation
                         (char= (char trimmed 0) #\!)
                         (char= (char trimmed 1) #\:))
                    (apply-directive trimmed last-entry))
                   (t
                    (handler-case
                        (let ((entry (parse-magic-line line)))
                          (when entry
                            (let ((lvl (ent-level entry)))
                              (if (zerop lvl)
                                  (push entry roots)
                                  (let ((parent (aref stack (1- lvl))))
                                    (if parent
                                        (setf (ent-children parent)
                                              (nconc (ent-children parent) (list entry)))
                                        ;; orphan continuation: treat as root
                                        (push entry roots))))
                              (when (< lvl (length stack))
                                (setf (aref stack lvl) entry))
                              (setf last-entry entry))))
                      (magic-parse-error (e)
                        (declare (ignore e))
                        ;; skip unparseable lines but keep going
                        nil)))))))
    (nreverse roots)))
