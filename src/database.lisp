;;;; database.lisp --- Load magic sources into a database and drive matching.

(in-package #:magic)

(defstruct (database (:constructor %make-database))
  (entries (make-array 0 :adjustable t :fill-pointer 0))  ; top-level, sorted
  (names (make-hash-table :test 'equal))                  ; name -> NAME entry
  (binary (make-array 0 :adjustable t :fill-pointer 0))   ; binary tests, sorted
  (text (make-array 0 :adjustable t :fill-pointer 0))     ; text tests, sorted
  (sorted nil))

(defun make-database () (%make-database))

(defun db-lookup-name (db name)
  (gethash name (database-names db)))

(defun database-entry-count (db)
  (length (database-entries db)))

(defun db-add-entry (db entry)
  (if (eq (mtype-category (ent-type entry)) :name)
      (setf (gethash (ent-name entry) (database-names db)) entry)
      (vector-push-extend entry (database-entries db)))
  (setf (database-sorted db) nil))

(defun database-add-source (db text)
  "Parse magic source TEXT and add all of its entries to DB."
  (dolist (entry (parse-magic-source text))
    (db-add-entry db entry))
  db)

(defun ensure-sorted (db)
  "Sort the top-level entries by descending strength (matching file's order),
precompute each entry's first-byte fingerprint, and partition them into the
binary and text passes."
  (unless (database-sorted db)
    (let ((vec (database-entries db))
          (bin (make-array 0 :adjustable t :fill-pointer 0))
          (txt (make-array 0 :adjustable t :fill-pointer 0)))
      (sort vec #'> :key #'entry-strength)
      (loop for e across vec
            do (setf (ent-fp e) (entry-fingerprint e))
               (if (entry-text-p e)
                   (vector-push-extend e txt)
                   (vector-push-extend e bin)))
      (setf (database-binary db) bin
            (database-text db) txt
            (database-sorted db) t)))
  db)

;;; ---------------------------------------------------------------------------
;;; Loading from files / directories

(defun load-magic-file (db pathname)
  "Add the magic fragment file at PATHNAME to DB."
  (database-add-source db (uiop:read-file-string pathname))
  db)

(defun load-magic-directory (db directory)
  "Add every magic fragment file in DIRECTORY (non-recursively) to DB."
  (dolist (file (sort (uiop:directory-files (uiop:ensure-directory-pathname directory))
                      #'string< :key #'namestring))
    ;; skip editor backups / dotfiles
    (let ((name (file-namestring file)))
      (unless (or (zerop (length name)) (char= (char name 0) #\.)
                  (char= (char name (1- (length name))) #\~))
        (handler-case (load-magic-file db file)
          (error () nil)))))
  (ensure-sorted db)
  db)

(defvar *default-magic-directory* nil
  "When non-NIL, the directory the default database loads its magic from.
Defaults to the vendored copy of file's Magdir.")

(defun vendored-magic-directory ()
  "Path to the vendored file(1) magic source directory."
  (asdf:system-relative-pathname "magic" "vendor/file/magic/Magdir/"))

(defvar *default-database* nil)

(defun default-database ()
  "Return (loading on first use) the shared database built from the vendored
file(1) magic sources, or *DEFAULT-MAGIC-DIRECTORY* when set."
  (or *default-database*
      (setf *default-database*
            (let ((db (make-database)))
              (load-magic-directory db (or *default-magic-directory*
                                           (vendored-magic-directory)))
              db))))

;;; ---------------------------------------------------------------------------
;;; Matching

(defun eval-top (entry state)
  "Evaluate one top-level ENTRY, returning its ACC when it produced a non-empty
description, else NIL."
  (fill (ms-level-off state) 0)
  (setf (ms-flip state) nil)
  (let ((acc (make-acc)))
    (when (eval-group (list entry) state acc)
      (let ((desc (acc-description acc)))
        (when (plusp (length desc))
          (setf (acc-strength acc) (entry-strength entry))
          acc)))))

(defvar *fingerprint-index* t
  "When true, skip entries whose first-byte fingerprint cannot be satisfied
before evaluating them.  Purely an optimization; results are identical either
way.  Bound to NIL in tests to check that invariant.")

(defun scan-entries (entries state)
  "Evaluate ENTRIES (a strength-sorted vector) against STATE, returning the
first ACC with a non-empty description, or NIL.  Entries whose first-byte
fingerprint cannot be satisfied are skipped without the full evaluation."
  (let ((buffer (ms-buffer state))
        (bias (ms-bias state)))
    (loop for entry across entries
          when (or (not *fingerprint-index*)
                   (fingerprint-ok-p (ent-fp entry) buffer bias))
            do (let ((acc (eval-top entry state)))
                 (when acc (return acc)))
          finally (return nil))))

(defun match-buffer-1 (db state)
  "Match STATE's buffer against DB: the binary tests first (in strength order),
then -- only when the data looks like text -- the text (regex/search) tests.
This mirrors file(1) and avoids scanning hundreds of text patterns over binary
data.  Returns an ACC or NIL."
  (ensure-sorted db)
  (or (scan-entries (database-binary db) state)
      (when (buffer-textual-p (ms-buffer state))
        (scan-entries (database-text db) state))))

(defun match-buffer (db buffer)
  "Match BUFFER against DB.  Returns an ACC or NIL."
  (let ((state (make-mstate :buffer buffer :database db)))
    (match-buffer-1 db state)))
