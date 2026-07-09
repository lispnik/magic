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

(defun text-byte-p (b)
  "True for bytes file(1) considers part of printable text."
  (or (<= 32 b 126)                     ; printable ASCII
      (member b '(9 10 12 13 27))       ; tab, nl, ff, cr, esc
      (<= 160 b 255)))                  ; high ISO-8859 range

(defun classify-text (buffer)
  "If BUFFER looks like text, return (values description mime-type), else NIL."
  (let ((n (buffer-length buffer)))
    (when (plusp n)
      (let ((high nil))
        (dotimes (i n)
          (let ((b (aref buffer i)))
            (unless (text-byte-p b) (return-from classify-text nil))
            (when (>= b 128) (setf high t))))
        (if high
            (values "ISO-8859 text" "text/plain")
            (values "ASCII text" "text/plain"))))))

;;; ---------------------------------------------------------------------------
;;; Buffer-oriented API

(defun buffer-match (buffer &key (database (default-database)))
  "Return a MATCH-RESULT for BUFFER (an octet-vector / string / byte sequence),
or NIL if nothing matched.  Falls back to text classification."
  (let* ((buf (bytes->buffer buffer))
         (r (acc->result (match-buffer database buf))))
    (or r
        (multiple-value-bind (desc mime) (classify-text buf)
          (when desc
            (%make-match-result :description desc :mime-type mime :strength 1))))))

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

(defun run-cli (args)
  "Drive identification from command-line ARGS (a list of strings).  Supports
--mime / -i and prints one `path: type` line per file.  Returns an exit code."
  (let ((mime nil) (paths '()))
    (dolist (a args)
      (cond ((member a '("--mime" "--mime-type" "-i") :test #'string=) (setf mime t))
            ((member a '("-h" "--help") :test #'string=)
             (format t "usage: magic [--mime] FILE...~%")
             (return-from run-cli 0))
            (t (push a paths))))
    (setf paths (nreverse paths))
    (when (null paths)
      (format *error-output* "usage: magic [--mime] FILE...~%")
      (return-from run-cli 2))
    (let ((db (default-database)) (code 0))
      (dolist (p paths)
        (handler-case
            (if (probe-file p)
                (format t "~A: ~A~%" p
                        (if mime (mime-type p :database db) (file-type p :database db)))
                (progn (format t "~A: cannot open (No such file or directory)~%" p)
                       (setf code 1)))
          (error (e)
            (format t "~A: ERROR: ~A~%" p e)
            (setf code 1))))
      code)))
