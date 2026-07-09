;;;; tests/packages.lisp

(defpackage #:magic/tests
  (:use #:cl #:fiveam)
  (:export #:all-tests #:run-all))
