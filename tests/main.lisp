(defpackage treep/tests/main
  (:use :cl
        :treep-impl
        :rove)
  (:shadowing-import-from :treep-impl class find-symbol function intern load quote symbol))
(in-package :treep/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :treep)' in your Lisp.

(deftest smoke-test
  (testing "(= 1 1) should eval to true"
    (ok (= 1 1))))

(deftest forms-test
  (testing "Nested forms have the enclosing form as parent"
    (let* ((def (make-instance 'variable-definition))
	   (var (intern "a" +root-symbol+))
	   (form (make-instance 'binding :name var :definition def)))
      (ok (eq (form-parent def) form)))))

(deftest evaluator/basic
  (testing "Lisp objects (e.g., numbers) should eval to themselves"
    (ok (= 1 (transform (make-instance 'simple-evaluator) 1 *environment*))))
  
  (testing "(binding ((var a)) a) should eval to nil"
    (let* ((var (intern "a" +root-symbol+))
	   (form (make-instance 'binding :definition (make-instance 'variable-definition :name var)
				:body (make-instance 'variable-read :name var)))
	   (result (transform (make-instance 'simple-evaluator) form *environment*)))
      (ok (null result))))

  (testing "(binding ((var a 1)) a) should eval to 1"
    (let* ((var (intern "a" +root-symbol+))
	   (value 1)
	   (form (make-instance 'binding :definition (make-instance 'variable-definition :name var :init-form value)
				:body (make-instance 'variable-read :name var)))
	   (result (transform (make-instance 'simple-evaluator) form *environment*)))
      (ok (= value result))))
  (testing "(if t 1) should eval to 1"
    (let* ((value 1)
	   (form (make-instance 'conditional :condition t :then value))
	   (result (transform (make-instance 'simple-evaluator) form *environment*)))
      (ok (= value result)))))

(deftest evaluator+reader
  (testing "Evaluating the form (binding (variable-definition a 1) (variable-read a)) should eval to 1"
    (with-read-symbol-syntax ()
      (let* ((form (with-input-from-string (s "(binding (variable-definition a 1) (variable-read a))") (read-form s *environment*)))
	     (result (transform (make-instance 'simple-evaluator) form *environment*)))
	(ok (= 1 result)))))
  (testing "Evaluating the form (binding (function-definition f (function [(function-argument x)] (variable-read x))) (function-call f [1])) should eval to 1"
    (with-read-symbol-syntax ()
      (let* ((form (with-input-from-string (s "(binding (function-definition f (function [(function-argument x)] (variable-read x))) (function-call f [1]))") (read-form s *environment*))))
	(ok (typep form 'standard-object))
	(ok (= 1 (transform (make-instance 'simple-evaluator) form *environment*)))))))

(deftest evaluator-function-call-protocol
  (testing "An optional function argument with no default, which is not provided, evaluates to nil"
    (with-read-symbol-syntax ()
      (let* ((*package* (find-package :treep-impl))
	     (form (sexp-->form (read-from-string "(binding (function-definition #^f (function ((optional-function-argument #^x)) (variable-read #^x))) (function-call #^f))")))
	     (result (transform (make-instance 'simple-evaluator) form *environment*)))
	(ok (null result)))))
  (testing "An optional function argument which is not provided evaluates to its default value"
    (with-read-symbol-syntax ()
      (let* ((*package* (find-package :treep-impl))
	     (form (sexp-->form (read-from-string "(binding (function-definition #^f (function ((optional-function-argument #^x 1)) (variable-read #^x))) (function-call #^f))")))
	     (result (transform (make-instance 'simple-evaluator) form *environment*)))
	(ok (= 1 result)))))
  (testing "A rest function argument accumulates all the remaining arguments into a list"
    (with-read-symbol-syntax ()
      (let* ((*package* (find-package :treep-impl))
	     (form (sexp-->form (read-from-string "(binding (function-definition #^f (function ((rest-function-argument #^x)) (variable-read #^x))) (function-call #^f (1 2 3)))")))
	     (result (transform (make-instance 'simple-evaluator) form *environment*)))
	(ok (typep result 'fset:seq))
	(ok (= (fset:size result) 3))
	(ok (= (fset:@ result 0) 1))))))
