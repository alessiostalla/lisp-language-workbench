(in-package :lisp-language-workbench)

(defclass simple-evaluator () ())

(defmethod transform ((transformer simple-evaluator) form environment)
  (if (typep form 'form)
      (error "Don't know how to eval: ~S" form)
      ;Lisp objects are self-evaluating
      form)) 

(defmethod transform ((transformer simple-evaluator) (form constant) environment)
  (constant-value form))

(defmethod transform ((transformer simple-evaluator) (form binding) environment)
  (values
   (transform transformer (binding-body form)
	      (augment-environment environment
				   (binding-name form)
				   (transform transformer (binding-spec form) environment)))
   environment))

(defclass local-variable (binding-spec)
  ((value :initarg :value :reader local-variable-value)))
(defclass local-function (binding-spec)
  ((value :initarg :value :reader local-function-value)))

(defmethod transform ((transformer simple-evaluator) (form variable-binding-spec) environment)
  (make-instance 'local-variable :value (transform transformer (variable-binding-init-form form) environment)))
(defmethod transform ((transformer simple-evaluator) (form function-binding-spec) environment)
  (make-instance 'local-function :value (transform transformer (function-binding-init-form form) environment)))

(defmethod transform ((transformer simple-evaluator) (form variable-read) environment)
  (let* ((variable (accessed-variable-name form))
	 (meaning (meaning variable 'local-variable environment)))
    (if meaning
	(local-variable-value meaning)
	(error (format nil "Unknown variable: ~A" (with-output-to-string (out) (print-symbol variable out))))))) ;TODO proper condition class

(defmethod transform ((transformer simple-evaluator) (form function) environment)
  (let* ((args (function-arguments form))
	 (body (function-expression form))
	 (lisp-args (map 'list (lambda (s) (make-symbol (symbol-name s))) args))
	 (fn `(lambda ,lisp-args
		(transform ,transformer ,body
			   (let ((env ,environment))
			     ,@(loop :for i :from 0 :to (1- (length args))
				  :collect `(setf env (augment-environment
						       env ,(nth i args)
						       (make-instance 'local-variable :value ,(nth i lisp-args)))))
			     env)))))
    (compile nil fn)))


(defmethod transform ((transformer simple-evaluator) (form conditional) environment)
  (if (transform transformer (conditional-if form)   environment)
      (transform transformer (conditional-then form) environment)
      (transform transformer (conditional-else form) environment)))
