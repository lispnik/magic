;;;; packages.lisp

(defpackage #:magic
  (:use #:cl)
  (:nicknames #:cl-magic)
  (:export
   ;; High-level API
   #:file-type
   #:mime-type
   #:describe-file
   #:buffer-type
   #:buffer-mime-type
   ;; Database
   #:database
   #:make-database
   #:load-magic-directory
   #:load-magic-file
   #:default-database
   #:*default-magic-directory*
   #:vendored-magic-directory
   #:database-entry-count
   ;; Result object
   #:match-result
   #:result-description
   #:result-mime-type
   #:result-extensions
   #:result-strength
   ;; Conditions
   #:magic-error
   #:magic-parse-error))
