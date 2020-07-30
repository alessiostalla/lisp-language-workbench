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

(deftest evaluator/basic
  (testing "Lisp objects (e.g., numbers) should eval to themselves"
    (ok (= 1 (transform (make-instance 'simple-evaluator) 1 *global-environment*))))
  
  (testing "(binding ((var a)) a) should eval to nil"
    (let* ((var (intern "a" *root-symbol*))
	   (form (make-instance 'binding :name var :spec (make-instance 'variable-binding-spec)
				:body (make-instance 'variable-read :name var)))
	   (result (transform (make-instance 'simple-evaluator) form *global-environment*)))
      (ok (null result))))

  (testing "(binding ((var a 1)) a) should eval to 1"
    (let* ((var (intern "a" *root-symbol*))
	   (value 1)
	   (form (make-instance 'binding :name var :spec (make-instance 'variable-binding-spec :init-form value)
				:body (make-instance 'variable-read :name var)))
	   (result (transform (make-instance 'simple-evaluator) form *global-environment*)))
      (ok (= value result))))
  (testing "(if t 1) should eval to 1"
    (let* ((value 1)
	   (form (make-instance 'conditional :condition t :then value))
	   (result (transform (make-instance 'simple-evaluator) form *global-environment*)))
      (ok (= value result)))))

(deftest evaluator+reader
  (testing "Evaluating the form read from '(binding a (variable-binding-spec 1) (variable-read a)) should eval to 1"
    (with-read-symbol-syntax ()
      (let* ((*package* (find-package :lisp-language-workbench))
	     (form (read-form (read-from-string "(binding #^a (variable-binding-spec 1) (variable-read #^a))")))
	     (result (transform (make-instance 'simple-evaluator) form *global-environment*)))
	(ok (= 1 result)))))
  (testing "Evaluating the form read from '(binding #^f (function-binding-spec (function (#^x) (variable-read #^x))) (function-call #^f (1))) should eval to 1"
    (with-read-symbol-syntax ()
      (let* ((*package* (find-package :lisp-language-workbench))
	     (form (read-form (read-from-string "(binding #^f (function-binding-spec (function (#^x) (variable-read #^x))) (function-call #^f (1)))")))
	     (result (transform (make-instance 'simple-evaluator) form *global-environment*)))
	(ok (= 1 result))))))

