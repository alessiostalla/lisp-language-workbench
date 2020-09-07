(in-package :treep)

(defclass simple-evaluator () ())

(defmethod transform ((transformer simple-evaluator) form environment)
  (if (typep form 'form)
      (error "Don't know how to eval: ~S" form)
      ;Lisp objects are self-evaluating
      form))

(defmethod transform ((transformer simple-evaluator) (form binding) environment)
  (default-transform-binding transformer form environment))
(defmethod transform ((transformer simple-evaluator) (form install-definition!) environment)
  (default-transform-definition! transformer form environment))


(defclass box ()
  ((value :initarg :value :accessor box-value)))

(defclass interpreted-function (function)
  ((lisp-function :initarg :lisp-function :reader lisp-function)))

(defmethod transform ((transformer simple-evaluator) (form variable-definition) environment)
  (let ((value (transform transformer (variable-definition-init-form form) environment)))
    (make-instance 'box :value value)))
(defmethod transform ((transformer simple-evaluator) (form function-definition) environment)
  (let ((function (transform transformer (function-definition-init-form form) environment)))
    (typecase function
      (interpreted-function function)
      (function (transform transformer function environment))
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
  (let* ((lambda-list (function-lambda-list form))
	 (body (function-expression form))
	 (lisp-args (fset:convert 'list (fset:image
					 (lambda (a) (make-symbol (symbol-name (function-argument-name a))))
					 lambda-list)))
	 (fn `(lambda ,lisp-args
		;;TODO declare args ignorable?
		(transform ,transformer ,body
			   (let ((env ,environment))
			     ,@(cl:loop :for i :from 0 :to (1- (fset:size lambda-list))
				  :collect `(setf env (augment-environment
						       env ,(function-argument-name (fset:@ lambda-list i)) 'variable
						       (make-instance 'box :value ,(nth i lisp-args))))) ;TODO should they be constant?
			     env)))))
    (make-instance 'interpreted-function
		   :lambda-list (function-lambda-list form)
		   :lisp-function (compile nil fn))))

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
      (apply lisp-function (fset:convert
			    'list
			    (fset:image
			     (lambda (a) (transform transformer a environment))
			     (function-arguments form)))))))

(defmethod transform ((transformer simple-evaluator) (form conditional) environment)
  (if (transform transformer (conditional-if   form) environment)
      (transform transformer (conditional-then form) environment)
      (transform transformer (conditional-else form) environment)))

(defmethod transform ((transformer simple-evaluator) (form loop) environment)
  (catch (loop-name form)    
    (cl:loop (transform transformer (loop-body form) environment))))

(defmethod transform ((transformer simple-evaluator) (form loop-break) environment)
  (throw (loop-name form) (transform transformer (return-form form) environment)))

(defmethod transform ((transformer simple-evaluator) (form lisp) environment)
  (declare (ignore environment transformer))
  (eval (lisp-expression form)))
