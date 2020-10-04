(defpackage treep/tests/main
  (:use :cl
        :treep
        :rove)
  (:shadowing-import-from :treep find-symbol intern quote symbol))
(in-package :treep/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :treep)' in your Lisp.

(deftest smoke-test
  (testing "(= 1 1) should eval to true"
    (ok (= 1 1))))

(deftest forms-test
  (testing "Nested forms have the enclosing form as parent"
    (let* ((def (make-instance 'variable-definition))
	   (var (intern "a" *root-symbol*))
	   (form (make-instance 'binding :name var :definition def)))
      (ok (eq (form-parent def) form)))))

(deftest evaluator/basic
  (testing "Lisp objects (e.g., numbers) should eval to themselves"
    (ok (= 1 (transform (make-instance 'simple-evaluator) 1 *environment*))))
  
  (testing "(binding ((var a)) a) should eval to nil"
    (let* ((var (intern "a" *root-symbol*))
	   (form (make-instance 'binding :name var :definition (make-instance 'variable-definition)
				:body (make-instance 'variable-read :name var)))
	   (result (transform (make-instance 'simple-evaluator) form *environment*)))
      (ok (null result))))

  (testing "(binding ((var a 1)) a) should eval to 1"
    (let* ((var (intern "a" *root-symbol*))
	   (value 1)
	   (form (make-instance 'binding :name var :definition (make-instance 'variable-definition :init-form value)
				:body (make-instance 'variable-read :name var)))
	   (result (transform (make-instance 'simple-evaluator) form *environment*)))
      (ok (= value result))))
  (testing "(if t 1) should eval to 1"
    (let* ((value 1)
	   (form (make-instance 'conditional :condition t :then value))
	   (result (transform (make-instance 'simple-evaluator) form *environment*)))
      (ok (= value result)))))

(deftest evaluator+reader
  (testing "Evaluating the form read from '(binding #^a (variable-definition 1) (variable-read #^a)) should eval to 1"
    (with-read-symbol-syntax ()
      (let* ((*package* (find-package :treep))
	     (form (read-form (read-from-string "(binding #^a (variable-definition 1) (variable-read #^a))")))
	     (result (transform (make-instance 'simple-evaluator) form *environment*)))
	(ok (= 1 result)))))
  (testing "Evaluating the form read from '(binding #^f (function-definition (function ((function-argument #^x)) (variable-read #^x))) (function-call #^f (1))) should eval to 1"
    (with-read-symbol-syntax ()
      (let* ((*package* (find-package :treep))
	     (form (read-form (read-from-string "(binding #^f (function-definition (function ((function-argument #^x)) (variable-read #^x))) (function-call #^f (1)))")))
	     (result (transform (make-instance 'simple-evaluator) form *environment*)))
	(ok (= 1 result))))))

(deftest evaluator+environment
  (testing "(the-global-environment) yields the global environment"
    (with-read-symbol-syntax ()
      (let* ((*package* (find-package :treep))
	     (form (read-form (read-from-string "(function-call #^the-global-environment)")))
	     (result (transform (make-instance 'simple-evaluator) form *environment*)))
	(ok (eq *environment* result))))))


(deftest evaluator-function-call-protocol
  (testing "An optional function argument with no default, which is not provided, evaluates to nil"
    (with-read-symbol-syntax ()
      (let* ((*package* (find-package :treep))
	     (form (read-form (read-from-string "(binding #^f (function-definition (function ((optional-function-argument #^x)) (variable-read #^x))) (function-call #^f ()))")))
	     (result (transform (make-instance 'simple-evaluator) form *environment*)))
	(ok (null result)))))
  (testing "An optional function argument which is not provided evaluates to its default value"
    (with-read-symbol-syntax ()
      (let* ((*package* (find-package :treep))
	     (form (read-form (read-from-string "(binding #^f (function-definition (function ((optional-function-argument #^x (function-call #^the-global-environment))) (variable-read #^x))) (function-call #^f ()))")))
	     (result (transform (make-instance 'simple-evaluator) form *environment*)))
	(ok (eq *environment* result))))))
