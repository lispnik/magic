;;;; evaluator.lisp --- Match a parsed magic entry tree against a byte buffer.

(in-package #:magic)

;;; ---------------------------------------------------------------------------
;;; Match state and output accumulator

(defstruct (mstate (:conc-name ms-))
  buffer
  database
  (bias 0)                             ; offset added to absolute offsets (USE)
  (flip nil)                           ; swap be/le (USE with ^name)
  (level-off (make-array 48 :initial-element 0))  ; end offset of last match/level
  (depth 0))                           ; recursion guard for use/indirect

(defstruct (acc (:conc-name acc-))
  (stream (make-string-output-stream))
  (printed nil)
  (desc nil)
  mime ext apple
  (strength 0))

(defun acc-emit (acc text no-space)
  "Append TEXT to ACC, inserting a separating space unless NO-SPACE or nothing
has been printed yet."
  (when (and text (plusp (length text)))
    (when (and (acc-printed acc) (not no-space))
      (write-char #\Space (acc-stream acc)))
    (write-string text (acc-stream acc))
    (setf (acc-printed acc) t)))

(defun acc-description (acc)
  "The accumulated description text.  Cached, since draining the underlying
string stream is destructive."
  (or (acc-desc acc)
      (setf (acc-desc acc)
            (string-right-trim '(#\Space)
                               (get-output-stream-string (acc-stream acc))))))

;;; ---------------------------------------------------------------------------
;;; Endianness and typed reads

(declaim (inline effective-endian))
(defun effective-endian (endian flip)
  (if (and flip (member endian '(:little :big)))
      (if (eq endian :little) :big :little)
      endian))

(defun read-typed (buffer offset mtype flip &key signed)
  "Read the value described by MTYPE at OFFSET.  Returns NIL when out of range.
For numeric/date/offset types returns an integer (unsigned unless SIGNED); for
floats a double-float."
  (let ((size (mtype-size mtype))
        (endian (effective-endian (mtype-endian mtype) flip)))
    (case (mtype-category mtype)
      ((:numeric :date :offset)
       (read-int buffer offset size endian :signed signed))
      (:float (read-ieee-float buffer offset size endian))
      (:guid (read-uint buffer offset 16 :big))
      (t nil))))

;;; ---------------------------------------------------------------------------
;;; Offset resolution

(defun apply-offset-op (op lhs rhs)
  (case op
    (#\+ (+ lhs rhs)) (#\- (- lhs rhs)) (#\* (* lhs rhs))
    (#\/ (if (zerop rhs) lhs (truncate lhs rhs)))
    (#\% (if (zerop rhs) lhs (rem lhs rhs)))
    (#\& (logand lhs rhs)) (#\| (logior lhs rhs)) (#\^ (logxor lhs rhs))
    (t lhs)))

(defun resolve-offset (off level state)
  "Resolve the MOFFSET OFF at continuation LEVEL to an absolute buffer offset,
or NIL if it cannot be computed."
  (let* ((buf (ms-buffer state))
         (flip (ms-flip state))
         (parent-end (if (> level 0) (aref (ms-level-off state) (1- level)) 0)))
    (cond
      ((moff-indirect off)
       (let ((base (moff-base off)))
         (when (moff-base-relative off) (incf base parent-end))
         (incf base (ms-bias state))
         (let* ((endian (effective-endian (moff-in-endian off) flip))
                (lhs (read-int buf base (moff-in-size off) endian
                               :signed (moff-in-signed off))))
           (when (null lhs) (return-from resolve-offset nil))
           (when (moff-in-invert off) (setf lhs (lognot lhs)))
           (let ((d (moff-disp off)))
             (when (moff-in-op-indirect off)
               (let ((dd (read-int buf (+ base d) (moff-in-size off) endian
                                   :signed (moff-in-signed off))))
                 (if dd (setf d dd) (return-from resolve-offset nil))))
             (let ((result (if (moff-in-op off)
                               (apply-offset-op (moff-in-op off) lhs d)
                               lhs)))
               (when (moff-relative off) (incf result parent-end))
               result)))))
      (t
       (let ((base (moff-base off)))
         (cond ((moff-from-end off) (setf base (- (buffer-length buf) base)))
               ((moff-relative off) (incf base parent-end))
               (t (incf base (ms-bias state))))
         base)))))

;;; ---------------------------------------------------------------------------
;;; Numeric comparison

(declaim (inline mask-to-size))
(defun mask-to-size (v size)
  (logand v (1- (ash 1 (* 8 size)))))

(defun apply-num-mask (v entry size)
  "Apply ENTRY's mask operation to the size-wrapped unsigned value V."
  (if (ent-mask entry)
      (let ((r (mask-to-size
                (apply-offset-op (ent-mask-op entry) v (ent-mask entry)) size)))
        (if (ent-mask-invert entry) (mask-to-size (lognot r) size) r))
      v))

(defun numeric-match-p (op file-val test size unsigned)
  "Compare FILE-VAL against TEST (both size-wrapped unsigned) per OP."
  (let ((lu (mask-to-size test size)))
    (ecase op
      (#\x t)
      (#\= (= file-val lu))
      (#\! (/= file-val lu))
      (#\& (= (logand file-val lu) lu))
      (#\^ (/= (logand file-val lu) lu))
      (#\~ (= file-val (mask-to-size (lognot test) size)))
      (#\> (if unsigned (> file-val lu)
               (> (sign-extend file-val size) (sign-extend lu size))))
      (#\< (if unsigned (< file-val lu)
               (< (sign-extend file-val size) (sign-extend lu size)))))))

;;; ---------------------------------------------------------------------------
;;; String / search comparison

(declaim (inline ws-byte-p lower-byte-p upper-byte-p to-lower-byte to-upper-byte))
(defun ws-byte-p (b) (or (= b 32) (<= 9 b 13)))     ; space, tab, nl, vt, ff, cr
(defun lower-byte-p (b) (<= (char-code #\a) b (char-code #\z)))
(defun upper-byte-p (b) (<= (char-code #\A) b (char-code #\Z)))
(defun to-lower-byte (b) (if (upper-byte-p b) (+ b 32) b))
(defun to-upper-byte (b) (if (lower-byte-p b) (- b 32) b))

(defun magic-strncmp (buffer offset magic flags)
  "A port of file(1)'s file_strncmp: compare the MAGIC octet-vector against
BUFFER at OFFSET, honouring the string flags c/C (asymmetric case folding),
W (compact whitespace), w (optional whitespace) and f (full word).  Returns an
integer like strncmp -- 0 means the magic matched."
  (let* ((len (length magic))
         (n (buffer-length buffer))
         (ci-low (and (find #\c flags) t))
         (ci-up (and (find #\C flags) t))
         (compact (and (find #\W flags) t))
         (optional (and (find #\w flags) t))
         (fullword (and (find #\f flags) t))
         (eb (min n (+ offset (if (or compact optional) n len))))
         (ai 0) (bi offset) (v 0))
    (flet ((bat (i) (if (< i n) (aref buffer i) 0)))
      (block cmp
        (dotimes (k len)
          (when (>= bi eb) (setf v 1) (return-from cmp))
          (let ((a (aref magic ai)))
            (cond
              ((and ci-low (lower-byte-p a))
               (setf v (- (to-lower-byte (bat bi)) a)) (incf bi) (incf ai)
               (unless (zerop v) (return-from cmp)))
              ((and ci-up (upper-byte-p a))
               (setf v (- (to-upper-byte (bat bi)) a)) (incf bi) (incf ai)
               (unless (zerop v) (return-from cmp)))
              ((and compact (ws-byte-p a))
               (incf ai)
               (if (and (< bi eb) (ws-byte-p (bat bi)))
                   (progn (incf bi)
                          (when (and (< ai len) (not (ws-byte-p (aref magic ai))))
                            (loop while (and (< bi eb) (ws-byte-p (bat bi))) do (incf bi))))
                   (progn (setf v 1) (return-from cmp))))
              ((and optional (ws-byte-p a))
               (incf ai)
               (loop while (and (< bi eb) (ws-byte-p (bat bi))) do (incf bi)))
              (t
               (setf v (- (bat bi) a)) (incf bi) (incf ai)
               (unless (zerop v) (return-from cmp)))))))
      (when (and fullword (zerop v) (< bi n) (not (ws-byte-p (bat bi))))
        (setf v 1)))
    v))

(defun string-bytes-match-p (buffer offset test flags)
  "Does the TEST octet-vector match BUFFER at OFFSET for a '=' string test?"
  (zerop (magic-strncmp buffer offset test flags)))

(defun string-relational (buffer offset test op flags)
  "Handle a string test with operator OP (= < > !).  Returns the match length
on success or NIL on failure."
  (let ((v (magic-strncmp buffer offset test flags))
        (tlen (length test)))
    (case op
      (#\= (when (zerop v) tlen))
      (#\! (when (/= v 0) tlen))
      (#\> (when (> v 0) tlen))
      (#\< (when (< v 0) tlen))
      (t (when (zerop v) tlen)))))

(defun trim-if (string flags)
  "Trim leading/trailing whitespace from STRING when the T flag is present."
  (if (find #\T flags) (string-trim '(#\Space #\Tab #\Newline #\Return) string) string))

(defun extract-string (buffer offset &key (limit 96) (flags ""))
  "Return a display string from BUFFER at OFFSET up to a NUL, newline, or LIMIT,
trimmed when the T flag is set."
  (trim-if
   (with-output-to-string (s)
     (loop for i from offset below (min (buffer-length buffer) (+ offset limit))
           for b = (aref buffer i)
           until (or (= b 0) (= b 10) (= b 13))
           do (write-char (code-char b) s)))
   flags))

;;; ---------------------------------------------------------------------------
;;; Pascal strings

(defun pstring-prefix (flags)
  "Return (values PREFIX-SIZE ENDIAN INCLUDES-SELF) for a pstring's length field,
per its /[BHhLlJ] modifiers.  B (1 byte) is the default."
  (multiple-value-bind (size endian)
      (cond ((find #\H flags) (values 2 :big))
            ((find #\h flags) (values 2 :little))
            ((find #\L flags) (values 4 :big))
            ((find #\l flags) (values 4 :little))
            (t (values 1 :big)))
    (values size endian (and (find #\J flags) t))))

(defun extract-bytes-string (buffer offset len)
  "Return LEN bytes from BUFFER at OFFSET as a string (not NUL-terminated)."
  (with-output-to-string (s)
    (loop for i from offset below (min (buffer-length buffer) (+ offset len))
          do (write-char (code-char (aref buffer i)) s))))

;;; ---------------------------------------------------------------------------
;;; GUIDs

(defun read-guid-string (buffer offset endian)
  "Read a 16-byte GUID at OFFSET and render it canonically (uppercase
XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX), or NIL if out of range.  ENDIAN selects
the byte order of the first three fields (little for `guid'/`leguid', big for
`beguid')."
  (when (in-bounds-p buffer offset 16)
    (flet ((b (i) (aref buffer (+ offset i))))
      (format nil "~8,'0X-~4,'0X-~4,'0X-~2,'0X~2,'0X-~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X"
              (read-uint buffer offset 4 endian)
              (read-uint buffer (+ offset 4) 2 endian)
              (read-uint buffer (+ offset 6) 2 endian)
              (b 8) (b 9) (b 10) (b 11) (b 12) (b 13) (b 14) (b 15)))))

(defun try-guid (entry off state)
  "Match a GUID: format the 16 file bytes canonically and compare (as an
upper-cased string) to the magic's GUID value."
  (let ((g (read-guid-string (ms-buffer state) off
                             (effective-endian (mtype-endian (ent-type entry)) (ms-flip state)))))
    (when g
      (let ((op (ent-operator entry)) (want (ent-test-value entry)))
        (when (case op
                (#\x t)
                (#\= (and want (string= g want)))
                (#\! (or (null want) (not (string= g want))))
                (t nil))
          (make-hit :value g :end (+ off 16)))))))

(defun try-pstring (entry off state)
  "Match a Pascal string: read the length prefix, then compare/extract the
content that follows it."
  (let ((buf (ms-buffer state))
        (flags (ent-str-flags entry)))
    (multiple-value-bind (psize pendian incl) (pstring-prefix flags)
      (let ((len (read-uint buf off psize pendian)))
        (when (null len) (return-from try-pstring nil))
        (let* ((content-off (+ off psize))
               (content-len (max 0 (if incl (- len psize) len)))
               (end (+ content-off content-len))
               (test (ent-test-value entry)))
          (if (eq (ent-operator entry) #\x)
              (make-hit :value (extract-bytes-string buf content-off content-len) :end end)
              (when (string-relational buf content-off test (ent-operator entry) flags)
                (make-hit :value (extract-bytes-string buf content-off content-len)
                          :end end))))))))

;;; ---------------------------------------------------------------------------
;;; Regex

(defun entry-regex-scanner (entry)
  (or (ent-compiled entry)
      (setf (ent-compiled entry)
            (handler-case
                (cl-ppcre:create-scanner
                 (ent-test-value entry)
                 :case-insensitive-mode (and (find #\c (ent-str-flags entry)) t)
                 :multi-line-mode t)
              (error () :bad)))))

;;; ---------------------------------------------------------------------------
;;; Field end offset (for relative children), per moffset() in softmagic.c

(defun field-end (entry offset match-len)
  "Compute the offset just past the field matched by ENTRY at OFFSET."
  (let ((cat (mtype-category (ent-type entry))))
    (case cat
      ((:numeric :date :float :offset) (+ offset (or (mtype-size (ent-type entry)) 0)))
      (:guid (+ offset 16))
      ((:string :pstring :search :regex) (+ offset (or match-len 0)))
      (t offset))))

;;; ---------------------------------------------------------------------------
;;; Trying a single entry

(defstruct hit value end)

(defun try-entry (entry state acc)
  "Attempt to match ENTRY against the buffer.  Returns a HIT (with the value to
format into the message and the end offset for children) or NIL.  ACC is the
current output accumulator; USE/INDIRECT write their sub-messages into it
directly so that word spacing (the \\b no-space marker) works across the join."
  (let* ((cat (mtype-category (ent-type entry)))
         (level (ent-level entry))
         (off (resolve-offset (ent-offset entry) level state)))
    ;; a NIL or negative offset (e.g. a from-EOF offset larger than the file)
    ;; can never match
    (when (or (null off) (minusp off)) (return-from try-entry nil))
    (case cat
      ((:numeric :date :offset)
       (let* ((size (mtype-size (ent-type entry)))
              (raw (read-uint (ms-buffer state) off size
                              (effective-endian (mtype-endian (ent-type entry))
                                                 (ms-flip state)))))
         (when (null raw) (return-from try-entry nil))
         (let* ((mv (apply-num-mask raw entry size))
                (unsigned (ent-unsigned entry)))
           (when (numeric-match-p (ent-operator entry) mv (or (ent-test-value entry) 0)
                                  size unsigned)
             (make-hit :value (if unsigned mv (sign-extend mv size))
                       :end (field-end entry off nil))))))
      (:float
       (let ((v (read-typed (ms-buffer state) off (ent-type entry) (ms-flip state))))
         (when (and v (not (eq v :nan)))
           (let ((op (ent-operator entry)) (tv (or (ent-test-value entry) 0)))
             (when (case op (#\x t) (#\= (= v tv)) (#\! (/= v tv))
                     (#\> (> v tv)) (#\< (< v tv)) (t nil))
               (make-hit :value v :end (field-end entry off nil)))))))
      (:string
       (let ((test (ent-test-value entry))
             (flags (ent-str-flags entry)))
         (when (eq (ent-operator entry) #\x)
           (return-from try-entry
             (when (in-bounds-p (ms-buffer state) off 1)
               (make-hit :value (extract-string (ms-buffer state) off :flags flags)
                         :end (field-end entry off 0)))))
         (let ((mlen (string-relational (ms-buffer state) off test
                                        (ent-operator entry) flags)))
           (when mlen
             ;; For =/! the printed value is exactly the matched region (like
             ;; file(1)); for </> it is the whole string read from the file.
             (make-hit :value (if (member (ent-operator entry) '(#\= #\!))
                                  (trim-if (extract-bytes-string (ms-buffer state) off mlen) flags)
                                  (extract-string (ms-buffer state) off :flags flags))
                       :end (field-end entry off mlen))))))
      (:pstring (try-pstring entry off state))
      (:guid (try-guid entry off state))
      (:search
       (let* ((test (ent-test-value entry))
              (range (or (ent-str-range entry) 1))
              (buf (ms-buffer state))
              (tlen (length test)))
         (loop for p from off to (+ off range)
               when (string-bytes-match-p buf p test (ent-str-flags entry))
                 do (return (make-hit :value (extract-string buf p)
                                      :end (+ p tlen)))
               finally (return nil))))
      (:regex
       (let ((scanner (entry-regex-scanner entry))
             (buf (ms-buffer state)))
         (unless (eq scanner :bad)
           (let* ((limit (min (buffer-length buf)
                              (+ off (or (ent-str-range entry) 8192))))
                  (text (map 'string #'code-char (subseq buf (min off (buffer-length buf))
                                                         limit))))
             (multiple-value-bind (ms me) (cl-ppcre:scan scanner text)
               (when ms
                 (make-hit :value (subseq text ms me)
                           :end (+ off (if (find #\s (ent-str-flags entry)) ms me)))))))))
      (:use (eval-use entry off state acc))
      (:name (make-hit :value nil :end off))     ; only reached directly; always true
      (:default (make-hit :value nil :end off))  ; group logic gates this
      (:clear (make-hit :value nil :end off))
      (:indirect (eval-indirect entry off state acc))
      (t nil))))                                  ; :der, :guid unsupported → no match

;;; ---------------------------------------------------------------------------
;;; USE / INDIRECT recursion

(defun eval-use (entry off state acc)
  "Recursively evaluate the named magic referenced by ENTRY, at offset OFF,
writing its messages directly into ACC.  Named-magic direct offsets are taken
relative to OFF (implemented via the state bias)."
  (when (>= (ms-depth state) 30) (return-from eval-use nil))
  (let ((root (db-lookup-name (ms-database state) (ent-name entry))))
    (when root
      (let ((saved-bias (ms-bias state))
            (saved-flip (ms-flip state))
            (saved-off (aref (ms-level-off state) 0)))
        (setf (ms-bias state) off)
        (when (ent-use-flip entry) (setf (ms-flip state) (not (ms-flip state))))
        (setf (aref (ms-level-off state) 0) off)
        (incf (ms-depth state))
        (unwind-protect
             (progn
               (when (ent-message root) (emit-entry-message root nil acc))
               (eval-group (ent-children root) state acc)
               (make-hit :value nil :end off))
          (decf (ms-depth state))
          (setf (ms-bias state) saved-bias
                (ms-flip state) saved-flip
                (aref (ms-level-off state) 0) saved-off))))))

(defun eval-indirect (entry off state acc)
  "Re-scan the whole database at OFF (the `indirect' type)."
  (declare (ignore entry))
  (when (>= (ms-depth state) 30) (return-from eval-indirect nil))
  (incf (ms-depth state))
  (unwind-protect
       (let ((saved-bias (ms-bias state)))
         (setf (ms-bias state) off)
         (let ((sub (match-buffer-1 (ms-database state) state)))
           (setf (ms-bias state) saved-bias)
           (when sub
             (acc-emit acc (acc-description sub) nil)
             (make-hit :value nil :end off))))
    (decf (ms-depth state))))

;;; ---------------------------------------------------------------------------
;;; Message emission

(defun emit-entry-message (entry value acc)
  "Format and append ENTRY's message (if any) to ACC."
  (let ((msg (ent-message entry)))
    (when msg
      (multiple-value-bind (text no-space) (decode-message-escapes msg)
        (let ((rendered
                (if (message-has-directive-p msg)
                    (printf-format text (message-value entry value))
                    text)))
          (acc-emit acc rendered no-space))))))

(defun message-value (entry value)
  "Coerce VALUE into something printf-format can render for ENTRY."
  (let ((cat (mtype-category (ent-type entry))))
    (case cat
      (:date (format-magic-date value (ent-type entry)))
      (t value))))

(defparameter +day-names+
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
(defparameter +month-names+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
(defconstant +filetime-epoch-offset+ 11644473600
  "Seconds between 1601-01-01 (Windows FILETIME epoch) and 1970-01-01 (Unix).")

(defun date-type-local-p (name)  (and name (search "ldate" name) t))
(defun date-type-windows-p (name) (and name (search "wdate" name) t))

(defun format-magic-date (value mtype)
  "Render VALUE as a date, matching file(1)'s asctime/ctime output.  The MTYPE
name selects UTC vs. local time and Unix vs. Windows-FILETIME epoch:
`*ldate' types are local, `*wdate' types are 100 ns ticks since 1601."
  (let* ((name (and mtype (mtype-name mtype)))
         (unix (if (date-type-windows-p name)
                   (- (truncate (or value 0) 10000000) +filetime-epoch-offset+)
                   (or value 0))))
    (handler-case
        (multiple-value-bind (s mi h d mo y dow)
            (if (date-type-local-p name)
                (decode-universal-time (+ unix 2208988800))
                (decode-universal-time (+ unix 2208988800) 0))
          ;; asctime: "Www Mmm DD HH:MM:SS YYYY" (day is width-3, space padded)
          (format nil "~A ~A~3,' D ~2,'0D:~2,'0D:~2,'0D ~D"
                  (svref +day-names+ dow) (svref +month-names+ (1- mo))
                  d h mi s y))
      (error () (princ-to-string value)))))

;;; ---------------------------------------------------------------------------
;;; Group evaluation (siblings at one continuation level)

(defun eval-group (entries state acc)
  "Evaluate a list of sibling ENTRIES at one continuation level, threading the
default/clear match tracking.  Returns T if any entry matched."
  (let ((got nil) (any nil))
    (dolist (e entries)
      (let ((cat (mtype-category (ent-type e))))
        (cond
          ((eq cat :clear) (setf got nil))
          ((and (eq cat :default) got) nil)  ; suppressed
          (t
           (let ((h (try-entry e state acc)))
             (when h
               (unless (eq cat :clear) (setf got t))
               (setf any t)
               ;; USE/INDIRECT emit their sub-magic directly; their own message
               ;; field is a placeholder file(1) never prints (e.g. "not_printed").
               (unless (member cat '(:use :indirect))
                 (emit-entry-message e (hit-value h) acc))
               (when (ent-mime e) (setf (acc-mime acc) (ent-mime e)))
               (when (ent-ext e) (setf (acc-ext acc) (ent-ext e)))
               (when (ent-apple e) (setf (acc-apple acc) (ent-apple e)))
               (let ((lvl (ent-level e)))
                 (when (< lvl (length (ms-level-off state)))
                   (setf (aref (ms-level-off state) lvl) (hit-end h))))
               (when (ent-children e)
                 (eval-group (ent-children e) state acc))))))))
    any))

;;; ---------------------------------------------------------------------------
;;; Strength (mirrors apprentice_magic_strength_1 + file_magic_strength)

(defconstant +mult+ 10)

(defun nonmagic-length (string)
  (max 1 (count-if #'alphanumericp string)))

(defun base-strength (entry)
  (let ((val (* 2 +mult+))
        (cat (mtype-category (ent-type entry))))
    (case cat
      (:default (return-from base-strength 0))
      ((:numeric :date :float :offset :guid)
       (incf val (* (or (mtype-size (ent-type entry)) 4) +mult+)))
      ((:string :pstring)
       (incf val (* (length (or (ent-test-value entry) #())) +mult+)))
      (:search
       (let ((vl (length (or (ent-test-value entry) #()))))
         (when (plusp vl) (incf val (* vl (max (truncate +mult+ vl) 1))))))
      (:regex
       (let ((v (nonmagic-length (or (ent-test-value entry) ""))))
         (incf val (* v (max (truncate +mult+ v) 1)))))
      ((:indirect :name :use :clear) nil)
      (t nil))
    (case (ent-operator entry)
      ((#\x #\!) (setf val 0))
      (#\= (incf val +mult+))
      ((#\> #\<) (decf val (* 2 +mult+)))
      ((#\& #\^) (decf val +mult+)))
    val))

(defun entry-strength (entry)
  "The final sort strength for the top-level ENTRY."
  (let ((val (base-strength entry)))
    (when (ent-strength entry)
      (destructuring-bind (op . factor) (ent-strength entry)
        (setf val (case op
                    (#\+ (+ val factor)) (#\- (- val factor))
                    (#\* (* val factor)) (#\/ (if (zerop factor) val (truncate val factor)))
                    (t val)))))
    (when (<= val 0) (setf val 1))
    (unless (ent-message entry) (incf val))
    val))

;;; ---------------------------------------------------------------------------
;;; First-byte fingerprint (a necessary-condition index for fast rejection)

(defun entry-fingerprint (entry)
  "Return a cons (OFFSET . BYTE) such that BUFFER[OFFSET] must equal BYTE for
ENTRY to have any chance of matching, or NIL when no single-byte precondition
at a constant absolute offset can be determined.  The condition must be
*necessary*, so it is only derived from `=' tests at fixed offsets."
  (let ((off (ent-offset entry)))
    (when (and (not (moff-indirect off)) (not (moff-relative off))
               (not (moff-from-end off)) (not (moff-base-relative off))
               (>= (moff-base off) 0)
               (char= (ent-operator entry) #\=))
      (case (mtype-category (ent-type entry))
        (:string
         ;; a determinable first byte requires no case folding or whitespace
         ;; compaction to alter it
         (unless (find-if (lambda (c) (member c '(#\c #\C #\W #\w))) (ent-str-flags entry))
           (let ((tv (ent-test-value entry)))
             (when (and (typep tv '(array octet (*))) (plusp (length tv)))
               (cons (moff-base off) (aref tv 0))))))
        (:numeric
         (unless (ent-mask entry)
           (let* ((size (mtype-size (ent-type entry)))
                  (endian (effective-endian (mtype-endian (ent-type entry)) nil))
                  (tv (mask-to-size (or (ent-test-value entry) 0) size)))
             (unless (eq endian :middle)
               (cons (moff-base off)
                     (if (eq endian :little)
                         (logand tv #xff)
                         (logand (ash tv (- (* 8 (1- size)))) #xff)))))))
        (t nil)))))

(defun fingerprint-ok-p (fp buffer bias)
  "True when ENTRY's fingerprint FP is satisfied by BUFFER (offset shifted by
BIAS), or when FP is NIL (no precondition)."
  (or (null fp)
      (let ((o (+ (car fp) bias)))
        (and (in-bounds-p buffer o 1)
             (= (aref buffer o) (cdr fp))))))

;;; ---------------------------------------------------------------------------
;;; Binary vs. text classification
;;;
;;; Per MAGIC(5): regex and search are text tests unless their pattern contains
;;; non-printable bytes; every other test is binary.  file(1) runs the binary
;;; tests first and only tries the text tests when the data looks like text.

(defun printable-pattern-p (pattern)
  "True when PATTERN (a string or octet-vector) contains only text bytes."
  (etypecase pattern
    (null nil)
    (string (every (lambda (c) (text-octet-p (char-code c))) pattern))
    (vector (every #'text-octet-p pattern))))

(defun entry-text-p (entry)
  "True when ENTRY is a text test (a regex/search with a printable pattern)."
  (and (member (mtype-category (ent-type entry)) '(:regex :search))
       (printable-pattern-p (ent-test-value entry))))

(defun buffer-textual-p (buffer &optional (limit 4096))
  "True when the first LIMIT bytes of BUFFER are all printable text bytes."
  (let ((n (min (buffer-length buffer) limit)))
    (loop for i below n always (text-octet-p (aref buffer i)))))
