;;;; magic.asd --- A pure Common Lisp reimplementation of file(1)'s magic engine.

(asdf:defsystem "magic"
  :description "Read file(1)'s magic database and identify files, in pure Common Lisp."
  :author "Generated with Claude Code"
  :license "BSD-2-Clause"
  :version "0.1.0"
  :depends-on ("cl-ppcre")
  :serial t
  :components ((:file "src/packages")
               (:file "src/buffer")
               (:file "src/escapes")
               (:file "src/types")
               (:file "src/parser")
               (:file "src/evaluator")
               (:file "src/database")
               (:file "src/api"))
  ;; `asdf:make :magic` builds a standalone executable via uiop:dump-image.
  :build-operation "program-op"
  :build-pathname "bin/magic.bin"
  :entry-point "magic::cli-toplevel"
  :in-order-to ((asdf:test-op (asdf:test-op "magic/tests"))))

(asdf:defsystem "magic/tests"
  :description "Test suite for the magic system."
  :depends-on ("magic" "fiveam")
  :serial t
  :pathname "tests"
  :components ((:file "packages")
               (:file "fixtures")
               (:file "tests"))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :all-tests :magic/tests))))
