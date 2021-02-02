(defpackage treep/tests/object-system
  (:use :cl
        :treep
        :rove)
  (:shadowing-import-from :treep class find-symbol function intern load quote symbol))
(in-package :treep/tests/object-system)

;; NOTE: To run this test file, execute `(asdf:test-system :treep)' in your Lisp.

(deftest methods
  (testing "Simple method on symbol, implicit gf declaration"
    (let* ((name (intern "m" *root-symbol*))
	   (form (make-instance 'binding
				:definition (make-instance 'method-definition
							   :name name
							   :lambda-list (fset:seq (make-instance 'specialized-function-argument
												 :name name
												 :specializer (find-class 'symbol)))
							   :expression (make-instance 'variable-read :name name))
				:body (make-instance 'function-call :function name :arguments (fset:seq name))))
	   (result (transform (make-instance 'simple-evaluator) form *environment*)))
      (ok (eq name result)))))
