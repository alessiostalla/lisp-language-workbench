(defsystem "treep"
  :version "0.1.0"
  :author "Alessio Stalla"
  :license "AGPL"
  :depends-on ("closer-mop" "cl-unification" "fset")
  :components ((:module "src/level0"
                :components
                ((:file "packages") (:file "symbols")
		 (:file "forms") (:file "evaluator")
		 (:file "s-expressions")
		 (:file "load") (:file "repl")
		 (:file "lisp") (:file "level1-boot"))))
  :description ""
  :in-order-to ((test-op (test-op "treep/tests"))))

(defsystem "treep/tests"
  :author "Alessio Stalla"
  :license "AGPL"
  :depends-on ("treep" "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for treep"
  :perform (test-op (op c) (symbol-call :rove :run c)))
