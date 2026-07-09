;;;; database.lisp --- Load magic sources into a database and drive matching.

(in-package #:magic)

(defstruct (database (:constructor %make-database))
  (entries (make-array 0 :adjustable t :fill-pointer 0))  ; top-level, sorted
  (names (make-hash-table :test 'equal))                  ; name -> NAME entry
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
  "Sort the top-level entries by descending strength (matching file's order)."
  (unless (database-sorted db)
    (let ((vec (database-entries db)))
      (sort vec #'> :key #'entry-strength)
      (setf (database-sorted db) t))
    db)
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

(defun match-buffer-1 (db state)
  "Try DB's entries in strength order against STATE; return the first ACC that
yields a non-empty description, or NIL."
  (ensure-sorted db)
  (loop for entry across (database-entries db)
        for acc = (eval-top entry state)
        when acc do (return acc)
        finally (return nil)))

(defun match-buffer (db buffer)
  "Match BUFFER against DB.  Returns an ACC or NIL."
  (let ((state (make-mstate :buffer buffer :database db)))
    (match-buffer-1 db state)))
