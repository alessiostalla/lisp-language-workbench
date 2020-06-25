(defsystem "lisp-language-workbench"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("closer-mop" "fset")
  :components ((:module "src"
                :components
                ((:file "main") (:file "forms") (:file "evaluator"))))
  :description ""
  :in-order-to ((test-op (test-op "lisp-language-workbench/tests"))))

(defsystem "lisp-language-workbench/tests"
  :author ""
  :license ""
  :depends-on ("lisp-language-workbench"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for lisp-language-workbench"
  :perform (test-op (op c) (symbol-call :rove :run c)))
