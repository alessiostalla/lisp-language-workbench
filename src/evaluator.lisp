(in-package :lisp-language-workbench)

(defclass simple-evaluator () ())

(defmethod transform ((transformer simple-evaluator) form environment)
  (if (typep form 'form)
      (error "Don't know how to eval: ~S" form)
      ;Lisp objects are self-evaluating
      form))

(defmethod transform ((transformer simple-evaluator) (form binding) environment)
  (default-transform-binding transformer form environment))

(defclass box ()
  ((value :initarg :value :accessor box-value)))

(defclass interpreted-function (function)
  ((lisp-function :initarg :lisp-function :reader lisp-function)))

(defmethod transform ((transformer simple-evaluator) (form variable-binding-spec) environment)
  (let ((value (transform transformer (variable-binding-init-form form) environment)))
    (make-instance 'box :value value)))
(defmethod transform ((transformer simple-evaluator) (form function-binding-spec) environment)
  (let ((function (transform transformer (function-binding-init-form form) environment)))
    (typecase function
      (function (make-instance 'interpreted-function :lisp-function (transform transformer function environment))) ;TODO copy function metadata
      (cl:function (make-instance 'interpreted-function :lisp-function function))
      (t (error "Not a function: ~S" function))))) ;TODO specific condition

(defmethod transform ((transformer simple-evaluator) (form variable-read) environment)
  (let* ((variable (accessed-variable-name form))
	 (meaning (meaning variable 'variable environment)))
    (if meaning
	(if (typep meaning 'box)
	    (box-value meaning)
	    (error "BUG! Not a variable: ~S" meaning)) ;TODO proper condition class
	(error (format nil "Unknown variable: ~A" (with-output-to-string (out) (print-symbol variable out))))))) ;TODO proper condition class

(defmethod transform ((transformer simple-evaluator) (form function) environment)
  (let* ((args (function-arguments form))
	 (body (function-expression form))
	 (lisp-args (map 'list (lambda (s) (make-symbol (symbol-name s))) args)) ;TODO function argument forms!
	 (fn `(lambda ,lisp-args
		(transform ,transformer ,body
			   (let ((env ,environment))
			     ,@(loop :for i :from 0 :to (1- (length args))
				  :collect `(setf env (augment-environment
						       env ,(nth i args) 'variable
						       (make-instance 'box :value ,(nth i lisp-args))))) ;TODO should they be constant?
			     env)))))
    (compile nil fn)))

(defmethod transform ((transformer simple-evaluator) (form function-call) environment)
  (flet ((to-lisp-function (designator)
	   (typecase designator
	     (interpreted-function (lisp-function designator))
	     (function (lisp-function (transform transformer designator environment)))
	     (cl:function designator)
	     (t (error "Not a function designator: ~S" designator))))) ;TODO proper condition class
    (let* ((function-designator (accessed-function-designator form))
	   (lisp-function
	    (typecase function-designator	    
	      (symbol (let ((meaning (meaning function-designator 'function environment)))
			(unless meaning
			  (error (format nil "Unknown function: ~A" (with-output-to-string (out) (print-symbol function-designator out))))) ;TODO proper condition class
			(to-lisp-function meaning)))
	      (t (to-lisp-function (transform transformer function-designator environment))))))
      (apply lisp-function (map 'list (lambda (a) (transform transformer a environment)) (function-call-arguments form))))))

(defmethod transform ((transformer simple-evaluator) (form conditional) environment)
  (if (transform transformer (conditional-if form)   environment)
      (transform transformer (conditional-then form) environment)
      (transform transformer (conditional-else form) environment)))

(defmethod transform ((transformer simple-evaluator) (form lisp) environment)
  (declare (ignore environment transformer))
  (eval (lisp-expression form)))
