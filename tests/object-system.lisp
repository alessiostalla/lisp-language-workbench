(defpackage treep/tests/object-system
  (:use :cl
        :treep
        :rove)
  (:shadowing-import-from :treep class find-symbol function intern load quote symbol))
(in-package :treep/tests/object-system)

;; NOTE: To run this test file, execute `(asdf:test-system :treep)' in your Lisp.

(deftest classes
  (testing "Empty class definition"
    (let* ((name (intern "c" *root-symbol*))
	   (form (make-instance 'binding
				:definition (make-instance 'class-definition :name name)
				:body (make-instance 'class-reference :class-name name)))
	   (result (transform (make-instance 'simple-evaluator) form *environment*)))
      (ok (typep result 'cl:class))
      (ok (equal (closer-mop:class-direct-superclasses result) (list (find-class 'standard-object))))
      (ok (typep (make-instance result) 'cl:standard-object))))
  (testing "Class definition with one slot"
    (let* ((name (intern "c" *root-symbol*))
	   (form (make-instance 'binding
				:definition (make-instance 'class-definition
							   :name name
							   :slots (fset:seq (make-instance 'slot-definition :name name)))
				:body (make-instance 'class-reference :class-name name)))
	   (result (make-instance (transform (make-instance 'simple-evaluator) form *environment*))))
      (ok (not (slot-boundp result (treep::lisp-symbol name)))))))

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
