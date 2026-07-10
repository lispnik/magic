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

(defparameter *max-read-bytes* 1048576
  "Upper bound on how many leading bytes of a file are read for identification.")

;;; ---------------------------------------------------------------------------
;;; Text classification (a small stand-in for file(1)'s separate ascmagic pass)

(defun utf8-continuation-p (buffer start count)
  (and (<= (+ start count) (buffer-length buffer))
       (loop for j from start below (+ start count)
             always (<= #x80 (aref buffer j) #xbf))))

(defun utf8-classify (buffer)
  "Return :ASCII if BUFFER is printable 7-bit text, :UTF8 if it is valid UTF-8
with at least one multibyte sequence, or NIL otherwise (binary or not UTF-8)."
  (let ((n (buffer-length buffer)) (i 0) (multibyte nil))
    (loop while (< i n) do
      (let ((b (aref buffer i)))
        (cond
          ((< b #x80) (unless (text-octet-p b) (return-from utf8-classify nil)) (incf i))
          ((<= #xc2 b #xdf) (unless (utf8-continuation-p buffer (1+ i) 1)
                              (return-from utf8-classify nil))
                            (setf multibyte t) (incf i 2))
          ((<= #xe0 b #xef) (unless (utf8-continuation-p buffer (1+ i) 2)
                              (return-from utf8-classify nil))
                            (setf multibyte t) (incf i 3))
          ((<= #xf0 b #xf4) (unless (utf8-continuation-p buffer (1+ i) 3)
                              (return-from utf8-classify nil))
                            (setf multibyte t) (incf i 4))
          (t (return-from utf8-classify nil)))))
    (if multibyte :utf8 :ascii)))

(defun line-terminator-suffix (buffer)
  "Return file(1)'s line-terminator suffix for BUFFER (empty for plain LF)."
  (let ((crlf nil) (cr nil) (lf nil) (n (buffer-length buffer)))
    (dotimes (i n)
      (case (aref buffer i)
        (13 (if (and (< (1+ i) n) (= (aref buffer (1+ i)) 10)) (setf crlf t) (setf cr t)))
        (10 (setf lf t))))
    (cond (crlf ", with CRLF line terminators")
          (cr ", with CR line terminators")
          (lf "")
          (t ", with no line terminators"))))

(defun classify-text (buffer)
  "If BUFFER looks like text, return (values description mime-type charset),
otherwise NIL.  Detects UTF-16 (by BOM), UTF-8, ASCII, and ISO-8859, and
appends file-style line-terminator information."
  (let ((n (buffer-length buffer)))
    (when (plusp n)
      (cond
        ((and (>= n 2) (= (aref buffer 0) #xff) (= (aref buffer 1) #xfe))
         (values "Little-endian UTF-16 Unicode text" "text/plain" "utf-16le"))
        ((and (>= n 2) (= (aref buffer 0) #xfe) (= (aref buffer 1) #xff))
         (values "Big-endian UTF-16 Unicode text" "text/plain" "utf-16be"))
        (t
         (multiple-value-bind (name charset)
             (case (utf8-classify buffer)
               (:utf8  (values "UTF-8 Unicode text" "utf-8"))
               (:ascii (values "ASCII text" "us-ascii"))
               (t (when (loop for b across buffer always (text-octet-p b))
                    (values "ISO-8859 text" "iso-8859-1"))))
           (when name
             (values (concatenate 'string name (line-terminator-suffix buffer))
                     "text/plain" charset))))))))

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
       (when (search "text" (match-result-description r) :test #'char-equal)
         (let ((enc (classify-text buf)))
           (when enc
             (setf (match-result-description r)
                   (concatenate 'string (match-result-description r) ", " enc)))))
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
