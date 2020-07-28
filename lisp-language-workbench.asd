(defsystem "lisp-language-workbench"
  :version "0.1.0"
  :author "Alessio Stalla"
  :license "AGPL"
  :depends-on ("closer-mop" "cl-unification" "fset")
  :components ((:module "src"
                :components
                ((:file "packages") (:file "main") (:file "symbols")
		 (:file "forms") (:file "evaluator") (:file "s-expressions"))))
  :description ""
  :in-order-to ((test-op (test-op "lisp-language-workbench/tests"))))

(defsystem "lisp-language-workbench/tests"
  :author "Alessio Stalla"
  :license "AGPL"
  :depends-on ("lisp-language-workbench"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for lisp-language-workbench"
  :perform (test-op (op c) (symbol-call :rove :run c)))
