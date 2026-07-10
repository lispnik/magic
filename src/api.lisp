;;;; api.lisp --- Public interface, mirroring file(1)'s common uses.

(in-package #:magic)

(defstruct (match-result (:constructor %make-match-result))
  (description "" :type string)
  (mime-type nil)
  (extensions nil)
  (strength 0))

(defun result-description (r) (match-result-description r))
(defun result-mime-type (r) (match-result-mime-type r))
(defun result-extensions (r) (match-result-extensions r))
(defun result-strength (r) (match-result-strength r))

(defun acc->result (acc)
  (when acc
    (%make-match-result
     :description (acc-description acc)
     :mime-type (acc-mime acc)
     :extensions (and (acc-ext acc)
                      (split-on (acc-ext acc) #\/))
     :strength (acc-strength acc))))

(defparameter *max-read-bytes* (* 7 1024 1024)
  "Upper bound on how many leading bytes of a file are read for identification.
Defaults to 7 MiB, matching file(1)'s FILE_BYTES_MAX; larger files are examined
only up to this prefix.")

;;; ---------------------------------------------------------------------------
;;; Text classification / encoding detection (a port of file(1)'s encoding.c)

(defparameter *encoding-max* 1048576
  "How many leading bytes are examined for encoding classification.")

;;; text_chars table: 0 = never text (F), 1 = ASCII (T), 2 = ISO-8859 (I),
;;; 3 = non-ISO extended / Mac / IBM-PC (X).
(defparameter +text-chars+
  (let ((a (make-array 256 :element-type '(unsigned-byte 8) :initial-element 0)))
    (flet ((row (base &rest vals) (loop for v in vals for i from base do (setf (aref a i) v))))
      (row #x00 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0)  ; BEL BS HT LF VT FF CR text
      (row #x10 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0)  ; ESC, SUB(?) - only 0x1A? 0x1B
      (row #x20 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
      (row #x30 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
      (row #x40 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
      (row #x50 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
      (row #x60 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
      (row #x70 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0)
      (row #x80 3 3 3 3 3 1 3 3 3 3 3 3 3 3 3 3)  ; NEL (0x85) counts as text
      (row #x90 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3))
    (loop for i from #xa0 to #xff do (setf (aref a i) 2))
    a))

(declaim (inline text-class))
(defun text-class (b) (aref +text-chars+ b))

(defun looks-class-p (buffer end lo hi)
  "True when every byte in BUFFER[0,END) has a text class in [LO,HI]."
  (loop for i below end always (<= lo (text-class (aref buffer i)) hi)))

;;; EBCDIC -> (8-bit) ASCII, per file(1)'s POSIX dd(1) table.
(defparameter +ebcdic-to-ascii+
  (make-array 256 :element-type '(unsigned-byte 8) :initial-contents
    '(  0   1   2   3 156   9 134 127 151 141 142  11  12  13  14  15
       16  17  18  19 157 133   8 135  24  25 146 143  28  29  30  31
      128 129 130 131 132  10  23  27 136 137 138 139 140   5   6   7
      144 145  22 147 148 149 150   4 152 153 154 155  20  21 158  26
       32 160 161 162 163 164 165 166 167 168 213  46  60  40  43 124
       38 169 170 171 172 173 174 175 176 177  33  36  42  41  59 126
       45  47 178 179 180 181 182 183 184 185 203  44  37  95  62  63
      186 187 188 189 190 191 192 193 194  96  58  35  64  39  61  34
      195  97  98  99 100 101 102 103 104 105 196 197 198 199 200 201
      202 106 107 108 109 110 111 112 113 114  94 204 205 206 207 208
      209 229 115 116 117 118 119 120 121 122 210 211 212  91 214 215
      216 217 218 219 220 221 222 223 224 225 226 227 228  93 230 231
      123  65  66  67  68  69  70  71  72  73 232 233 234 235 236 237
      125  74  75  76  77  78  79  80  81  82 238 239 240 241 242 243
       92 159  83  84  85  86  87  88  89  90 244 245 246 247 248 249
       48  49  50  51  52  53  54  55  56  57 250 251 252 253 254 255)))

(defun decode-utf8 (buffer end &optional (start 0))
  "Port of file(1)'s file_looks_utf8.  Returns (values KIND CODEPOINTS) where
KIND is :ASCII (7-bit), :UTF8 (valid, with multibyte), or NIL (invalid or uses
weird control chars); CODEPOINTS is a fill-pointer vector of decoded values."
  (let ((cps (make-array 256 :adjustable t :fill-pointer 0))
        (i start) (ctrl nil) (gotone nil))
    (loop while (< i end) do
      (let ((b (aref buffer i)))
        (cond
          ((< b #x80)
           (unless (= (text-class b) 1) (setf ctrl t))
           (vector-push-extend b cps) (incf i))
          ((< b #xc0) (return-from decode-utf8 (values nil nil)))     ; stray 10xxxxxx
          (t
           (multiple-value-bind (following c)
               (cond ((< b #xe0) (values 1 (logand b #x1f)))
                     ((< b #xf0) (values 2 (logand b #x0f)))
                     ((< b #xf8) (values 3 (logand b #x07)))
                     (t (return-from decode-utf8 (values nil nil))))
             (incf i)
             (let ((truncated nil))
               (dotimes (k following)
                 (when (>= i end) (setf truncated t) (return))
                 (let ((cb (aref buffer i)))
                   (unless (<= #x80 cb #xbf) (return-from decode-utf8 (values nil nil)))
                   (setf c (logior (ash c 6) (logand cb #x3f))))
                 (incf i))
               (unless truncated
                 (vector-push-extend c cps) (setf gotone t))
               (when truncated (return))))))))
    (cond (ctrl (values nil nil))
          (gotone (values :utf8 cps))
          (t (values :ascii cps)))))

(defun line-terminators (get count)
  "Return file(1)'s line-terminator suffix, scanning COUNT code units via
(funcall GET i).  Empty for plain LF-only text; reports otherwise."
  (let ((crlf 0) (cr 0) (lf 0) (nel 0) (seen-cr nil))
    (dotimes (i count)
      (let ((c (funcall get i)))
        (cond ((= c 10) (if seen-cr (incf crlf) (incf lf)))
              (seen-cr (incf cr)))
        (setf seen-cr (= c 13))
        (when (= c #x85) (incf nel))))
    (when (and seen-cr (zerop cr) (zerop crlf)) (incf cr))
    (if (or (and (zerop crlf) (zerop cr) (zerop nel) (zerop lf))
            (plusp crlf) (plusp cr) (plusp nel))
        (let ((parts (append (when (plusp crlf) '("CRLF")) (when (plusp cr) '("CR"))
                             (when (plusp lf) '("LF")) (when (plusp nel) '("NEL")))))
          (if parts
              (format nil ", with ~{~A~^, ~} line terminators" parts)
              ", with no line terminators"))
        "")))

(defun bytes-terminators (buffer end) (line-terminators (lambda (i) (aref buffer i)) end))
(defun cp-terminators (cps) (line-terminators (lambda (i) (aref cps i)) (length cps)))

(defun utf7-p (buffer end)
  (and (> end 4) (= (aref buffer 0) 43) (= (aref buffer 1) 47) (= (aref buffer 2) 118)
       (member (aref buffer 3) '(56 57 43 47))))          ; +/v then 8 9 + /

(defun ucs-nottext-p (uc)
  (and (< uc 128) (/= (text-class uc) 1) (/= uc 0)))

(defun try-ucs16 (buffer end)
  (when (and (>= end 2)
             (or (and (= (aref buffer 0) #xff) (= (aref buffer 1) #xfe))
                 (and (= (aref buffer 0) #xfe) (= (aref buffer 1) #xff))))
    (let ((big (= (aref buffer 0) #xfe))
          (cps (make-array 256 :adjustable t :fill-pointer 0)))
      (loop for i from 2 below (1- end) by 2 do
        (let ((uc (if big (logior (ash (aref buffer i) 8) (aref buffer (1+ i)))
                      (logior (aref buffer i) (ash (aref buffer (1+ i)) 8)))))
          (when (or (= uc #xfffe) (= uc #xffff) (<= #xfdd0 uc #xfdef) (ucs-nottext-p uc))
            (return-from try-ucs16 nil))
          (vector-push-extend uc cps)))
      (values (format nil "Unicode text, UTF-16, ~A-endian text~A"
                      (if big "big" "little") (cp-terminators cps))
              (if big "utf-16be" "utf-16le")))))

(defun try-ucs32 (buffer end)
  (when (and (>= end 4)
             (or (and (= (aref buffer 0) #xff) (= (aref buffer 1) #xfe)
                      (= (aref buffer 2) 0) (= (aref buffer 3) 0))
                 (and (= (aref buffer 0) 0) (= (aref buffer 1) 0)
                      (= (aref buffer 2) #xfe) (= (aref buffer 3) #xff))))
    (let ((big (= (aref buffer 0) 0))
          (cps (make-array 256 :adjustable t :fill-pointer 0)))
      (loop for i from 4 below (- end 3) by 4 do
        (let ((uc (if big (logior (ash (aref buffer i) 24) (ash (aref buffer (+ i 1)) 16)
                                  (ash (aref buffer (+ i 2)) 8) (aref buffer (+ i 3)))
                      (logior (aref buffer i) (ash (aref buffer (+ i 1)) 8)
                              (ash (aref buffer (+ i 2)) 16) (ash (aref buffer (+ i 3)) 24)))))
          (when (or (= uc #xfffe) (ucs-nottext-p uc)) (return-from try-ucs32 nil))
          (vector-push-extend uc cps)))
      (values (format nil "Unicode text, UTF-32, ~A-endian text~A"
                      (if big "big" "little") (cp-terminators cps))
              (if big "utf-32be" "utf-32le")))))

(defun try-ebcdic (buffer end)
  (let ((tr (make-array end :element-type '(unsigned-byte 8))))
    (dotimes (i end) (setf (aref tr i) (aref +ebcdic-to-ascii+ (aref buffer i))))
    (cond
      ((looks-class-p tr end 1 1)
       (values (format nil "EBCDIC text~A" (bytes-terminators tr end)) "ebcdic"))
      ((looks-class-p tr end 1 2)
       (values (format nil "International EBCDIC text~A" (bytes-terminators tr end)) "ebcdic")))))

(defun classify-text (buffer)
  "If BUFFER looks like text, return (values description mime-type charset),
else NIL.  Mirrors file(1)'s file_encoding ladder: ASCII/UTF-7, UTF-8 (with or
without BOM), UTF-32/UTF-16 (by BOM), ISO-8859, non-ISO extended-ASCII, and
EBCDIC, with file-style line-terminator reporting."
  (let ((n (min (buffer-length buffer) *encoding-max*)))
    (when (plusp n)
      (macrolet ((ret (desc charset) `(return-from classify-text (values ,desc "text/plain" ,charset))))
        ;; 1. pure 7-bit ASCII (or UTF-7)
        (when (looks-class-p buffer n 1 1)
          (if (utf7-p buffer n)
              (ret "Unicode text, UTF-7" "utf-7")
              (ret (format nil "ASCII text~A" (bytes-terminators buffer n)) "us-ascii")))
        ;; 2. UTF-8 with BOM
        (when (and (> n 3) (= (aref buffer 0) #xef) (= (aref buffer 1) #xbb) (= (aref buffer 2) #xbf))
          (multiple-value-bind (kind cps) (decode-utf8 buffer n 3)
            (when kind
              (ret (format nil "Unicode text, UTF-8 (with BOM) text~A" (cp-terminators cps)) "utf-8"))))
        ;; 3. UTF-8 (with multibyte sequences)
        (multiple-value-bind (kind cps) (decode-utf8 buffer n)
          (when (eq kind :utf8)
            (ret (format nil "Unicode text, UTF-8 text~A" (cp-terminators cps)) "utf-8")))
        ;; 4/5. UTF-32 / UTF-16 (BOM required)
        (multiple-value-bind (desc cs) (try-ucs32 buffer n) (when desc (ret desc cs)))
        (multiple-value-bind (desc cs) (try-ucs16 buffer n) (when desc (ret desc cs)))
        ;; 6. ISO-8859
        (when (looks-class-p buffer n 1 2)
          (ret (format nil "ISO-8859 text~A" (bytes-terminators buffer n)) "iso-8859-1"))
        ;; 7. non-ISO extended-ASCII
        (when (looks-class-p buffer n 1 3)
          (ret (format nil "Non-ISO extended-ASCII text~A" (bytes-terminators buffer n)) "unknown-8bit"))
        ;; 8. EBCDIC
        (multiple-value-bind (desc cs) (try-ebcdic buffer n) (when desc (ret desc cs)))
        nil))))

;;; ---------------------------------------------------------------------------
;;; Buffer-oriented API

(defun buffer-match (buffer &key (database (default-database)))
  "Return a MATCH-RESULT for BUFFER (an octet-vector / string / byte sequence),
or NIL if nothing matched.  Falls back to text classification, and -- like
file(1) -- appends the text encoding to a matched text-type description."
  (let* ((buf (bytes->buffer buffer))
         (r (acc->result (match-buffer database buf))))
    (cond
      (r
       ;; A text-type match (its description mentions "text") over textual data
       ;; gets the encoding appended, e.g. "XML 1.0 document text, ASCII text".
       ;; Skip descriptions that are themselves an encoding label (the
       ;; Magdir/unicode BOM rules), which would otherwise duplicate.
       (let ((desc (match-result-description r)))
         (when (and (search "text" desc :test #'char-equal)
                    (not (eql 0 (search "Unicode text" desc))))
           (let ((enc (classify-text buf)))
             (when enc
               (setf (match-result-description r)
                     (concatenate 'string desc ", " enc))))))
       r)
      (t
       (multiple-value-bind (desc mime) (classify-text buf)
         (when desc
           (%make-match-result :description desc :mime-type mime :strength 1)))))))

(defun buffer-type (buffer &key (database (default-database)))
  "Return the human-readable description string for BUFFER, or a default."
  (let ((r (buffer-match buffer :database database)))
    (if r (match-result-description r) "data")))

(defun buffer-mime-type (buffer &key (database (default-database)))
  "Return the MIME type string for BUFFER, defaulting to application/octet-stream."
  (let ((r (buffer-match buffer :database database)))
    (or (and r (match-result-mime-type r)) "application/octet-stream")))

;;; ---------------------------------------------------------------------------
;;; File-oriented API

(defun file-match (pathname &key (database (default-database)))
  "Return a MATCH-RESULT describing PATHNAME, or NIL if nothing matched.
Handles the empty-file and directory cases the way file(1) does."
  (cond
    ((uiop:directory-pathname-p pathname)
     (%make-match-result :description "directory" :mime-type "inode/directory"))
    (t
     (let ((buf (read-file-into-buffer pathname :max-bytes *max-read-bytes*)))
       (if (zerop (buffer-length buf))
           (%make-match-result :description "empty" :mime-type "inode/x-empty")
           (buffer-match buf :database database))))))

(defun file-type (pathname &key (database (default-database)))
  "Return the human-readable type description of PATHNAME (like `file`)."
  (let ((r (file-match pathname :database database)))
    (if r (match-result-description r) "data")))

(defun mime-type (pathname &key (database (default-database)))
  "Return the MIME type of PATHNAME (like `file --mime-type`)."
  (let ((r (file-match pathname :database database)))
    (or (and r (match-result-mime-type r)) "application/octet-stream")))

(defun describe-file (pathname &key (database (default-database)) (stream *standard-output*))
  "Print a `file`-style line for PATHNAME and return the MATCH-RESULT."
  (let ((r (file-match pathname :database database)))
    (format stream "~A: ~A~@[ [~A]~]~%"
            (file-namestring pathname)
            (if r (match-result-description r) "data")
            (and r (match-result-mime-type r)))
    r))

;;; ---------------------------------------------------------------------------
;;; Command-line entry point (a small `file`-like driver)

(defparameter *cli-version* "0.1.0")

(defun cli-usage (&optional (stream *standard-output*))
  (format stream "usage: magic [--mime|-i] [--extension] [-b|--brief] FILE...~%~
                  ~7@T magic ...  -                 read from standard input~%~
                  ~7@T magic --version | --help~%"))

(defun cli-result-text (r mode)
  "Render MATCH-RESULT R (or NIL) for output MODE (:desc, :mime, or :ext)."
  (ecase mode
    (:desc (if r (match-result-description r) "data"))
    (:mime (or (and r (match-result-mime-type r)) "application/octet-stream"))
    (:ext  (let ((exts (and r (match-result-extensions r))))
             (if exts (format nil "~{~A~^/~}" exts) "???")))))

(defun run-cli (args)
  "Drive identification from command-line ARGS (a list of strings).  Supports
--mime/-i, --extension, -b/--brief, --version, and `-' for standard input.
Prints one line per file and returns an exit code."
  (let ((mode :desc) (brief nil) (paths '()))
    (dolist (a args)
      (cond
        ((member a '("--mime" "--mime-type" "-i") :test #'string=) (setf mode :mime))
        ((member a '("--extension" "--ext") :test #'string=) (setf mode :ext))
        ((member a '("-b" "--brief") :test #'string=) (setf brief t))
        ((member a '("-v" "--version") :test #'string=)
         (format t "magic ~A (pure Common Lisp reimplementation of file(1))~%" *cli-version*)
         (return-from run-cli 0))
        ((member a '("-h" "--help") :test #'string=) (cli-usage) (return-from run-cli 0))
        ((and (> (length a) 1) (char= (char a 0) #\-))
         (format *error-output* "magic: unknown option `~A'~%" a)
         (return-from run-cli 2))
        (t (push a paths))))
    (setf paths (nreverse paths))
    (when (null paths) (cli-usage *error-output*) (return-from run-cli 2))
    (let ((db (default-database)) (code 0))
      (dolist (p paths)
        (handler-case
            (multiple-value-bind (r label)
                (if (string= p "-")
                    (values (buffer-match (read-file-into-buffer "/dev/stdin"
                                                                 :max-bytes *max-read-bytes*)
                                          :database db)
                            "/dev/stdin")
                    (values (and (probe-file p) (file-match p :database db)) p))
              (if (and (not (string= p "-")) (not (probe-file p)))
                  (progn
                    (format t "~A: cannot open `~A' (No such file or directory)~%" p p)
                    (setf code 1))
                  (let ((text (cli-result-text r mode)))
                    (if brief (format t "~A~%" text) (format t "~A: ~A~%" label text)))))
          (error (e)
            (format t "~A: ERROR: ~A~%" p e)
            (setf code 1))))
      code)))

(defun cli-toplevel ()
  "Toplevel for a saved standalone image: identify the argument files, then exit.
The magic database is already resident in the image, so start-up is immediate."
  (uiop:quit
   (handler-case (run-cli (uiop:command-line-arguments))
     (error (e) (format *error-output* "magic: ~A~%" e) 1))))
