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

(defclass lexical-variable (binding-spec)
  ((value :initarg :value :reader lexical-variable-value)))
(defclass lexical-function (binding-spec)
  ((value :initarg :value :reader lexical-function-value)))

(defmethod transform ((transformer simple-evaluator) (form variable-binding-spec) environment)
  (make-instance 'lexical-variable :value (transform transformer (variable-binding-init-form form) environment)))
(defmethod transform ((transformer simple-evaluator) (form function-binding-spec) environment)
  (make-instance 'lexical-function :value (transform transformer (function-binding-init-form form) environment)))

(defmethod transform ((transformer simple-evaluator) (form variable-read) environment)
  (let* ((variable (accessed-variable-name form))
	 (meaning (meaning variable 'lexical-variable environment)))
    (if meaning
	(lexical-variable-value meaning)
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
						       (make-instance 'lexical-variable :value ,(nth i lisp-args)))))
			     env)))))
    (compile nil fn)))

(defmethod transform ((transformer simple-evaluator) (form function-call) environment)
  (let* ((function-designator (accessed-function-designator form))
	 (function
	  (typecase function-designator
	    (cl:function function-designator)
	    (symbol (let ((meaning (meaning function-designator 'lexical-function environment)))
		      (if meaning
			  (lexical-function-value meaning)
			  (error (format nil "Unknown function: ~A" (with-output-to-string (out) (print-symbol function-designator out))))))) ;TODO proper condition class
	    (t (transform transformer function-designator environment)))))
    (if (functionp function)
	(apply function (map 'list (lambda (a) (transform transformer a environment)) (function-call-arguments form)))
	(error (format nil "Not a function: ~S" function))))) ;TODO proper condition class

(defmethod transform ((transformer simple-evaluator) (form conditional) environment)
  (if (transform transformer (conditional-if form)   environment)
      (transform transformer (conditional-then form) environment)
      (transform transformer (conditional-else form) environment)))
