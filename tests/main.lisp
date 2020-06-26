(defpackage lisp-language-workbench/tests/main
  (:use :cl
        :lisp-language-workbench
        :rove)
  (:shadowing-import-from :lisp-language-workbench find-symbol intern symbol))
(in-package :lisp-language-workbench/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :lisp-language-workbench)' in your Lisp.

(deftest smoke-test
  (testing "(= 1 1) should eval to true"
    (ok (= 1 1))))

(deftest evaluator
  (testing "(bind ((var a)) a) should eval to nil"
    (let* ((var (intern "a" *root-symbol*))
	   (form (make-instance 'binding :name var :spec (make-instance 'variable-binding-spec)
				:body (make-instance 'variable-read :name var))))
      (ok (null (transform (make-instance 'simple-evaluator) form *global-environment*))))))
