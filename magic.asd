;;;; magic.asd --- A pure Common Lisp reimplementation of file(1)'s magic engine.

(asdf:defsystem "magic"
  :description "Read file(1)'s magic database and identify files, in pure Common Lisp."
  :author "Generated with Claude Code"
  :license "BSD-2-Clause"
  :version "0.1.0"
  :depends-on ("cl-ppcre")
  :serial t
  :pathname "src"
  :components ((:file "packages")
               (:file "buffer")
               (:file "escapes")
               (:file "types")
               (:file "parser")
               (:file "evaluator")
               (:file "database")
               (:file "api"))
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
