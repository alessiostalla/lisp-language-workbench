(defpackage treep/tests/object-system
  (:use :cl
        :treep-impl
        :rove)
  (:shadowing-import-from :treep-impl #:class #:find-symbol #:function #:intern #:load #:quote #:symbol))
(in-package :treep/tests/object-system)

;; NOTE: To run this test file, execute `(asdf:test-system :treep)' in your Lisp.

(deftest classes-and-slots
  (testing "Empty class definition"
    (let* ((name (intern "x" (intern "treep/tests/object-system" +root-symbol+)))
	   (form (make-instance 'binding
				:definition (make-instance 'class-definition :name name)
				:body (make-instance 'class-reference :class-name name)))
	   (result (transform (make-instance 'simple-evaluator) form *environment*)))
      (ok (typep result 'cl:class))
      (ok (equal (closer-mop:class-direct-superclasses result) (list (find-class 'standard-object))))
      (ok (typep (make-instance result) 'cl:standard-object))))
  (testing "Class definition with one slot"
    (let* ((name (intern "x" (intern "treep/tests/object-system" +root-symbol+)))
	   (form (make-instance 'binding
				:definition (make-instance 'class-definition
							   :name name
							   :slots (fset:seq (make-instance 'slot-definition :name name)))
				:body (make-instance 'class-reference :class-name name)))
	   (result (make-instance (transform (make-instance 'simple-evaluator) form *environment*))))
      (ok (not (slot-boundp result (lisp-symbol name))))
      (ok (equal (list (lisp-symbol name)) (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (class-of result)))))))
    (testing "Reading and writing slots"
      (let* ((name (intern "x" (intern "treep/tests/object-system" +root-symbol+)))
	     (form (make-instance 'binding
				  :definition (make-instance 'class-definition
							     :name name
							     :slots (fset:seq (make-instance 'slot-definition :name name)))
				  :body (make-instance 'binding
						       :definition (make-instance 'variable-definition
										  :name name
										  :init-form (make-instance 'new-instance
													    :class (make-instance 'class-reference :class-name name)))
						       :body (fset:seq
							      (make-instance 'slot-write :object (make-instance 'variable-read :name name) :slot-name name :new-value 42)
							      (make-instance 'slot-read :object (make-instance 'variable-read :name name) :slot-name name)))))
	   (result (transform (make-instance 'simple-evaluator) form *environment*)))
      (ok (= result 42)))))

(deftest methods
  (testing "Simple method on symbol, implicit gf declaration"
    (let* ((name (intern "m" +root-symbol+))
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
