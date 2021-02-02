(in-package :treep)

(defclass simple-evaluator () ())

(defmethod transform ((transformer simple-evaluator) form environment)
  (if (typep form 'form)
      (call-next-method)
      form)) ;Lisp objects are self-evaluating

(defclass box ()
  ((value :initarg :value :accessor box-value)))

(defclass interpreted-function (function closer-mop:funcallable-standard-object) () (:metaclass closer-mop:funcallable-standard-class))

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
	 (meaning (meaning variable +kind-variable+ environment)))
    (if meaning
	(if (typep meaning 'box)
	    (box-value meaning)
	    (error "BUG! Not a variable: ~S" meaning)) ;TODO proper condition class
	(error (format nil "Unknown variable: ~A" (with-output-to-string (out) (print-symbol variable out))))))) ;TODO proper condition class

(defun check-function-lambda-list (ll)
  (let (found-optional found-rest)
    (fset:do-seq (arg ll)
      (when found-rest
	(error "No further arguments allowed after the rest argument: ~A" arg)) ;TODO specific condition class
      (etypecase arg
	(optional-function-argument (setf found-optional t))
	(rest-function-argument (setf found-rest t))
	(function-argument
	 (when found-optional
	   (error "No further regular arguments allowed after first optional argument: ~A" arg))))) ;TODO specific condition class
    ll))

(defun to-lisp-lambda-list (lambda-list transformer environment)
  (let ((result (list)) (symbols (list)) &optional-p rest-var)
    (fset:do-seq (arg lambda-list)
      (let ((symbol (make-symbol (symbol-name (function-argument-name arg)))))
	(when (typep arg 'optional-function-argument)
	  (when (not &optional-p)
	    (push '&optional result)
	    (setf &optional-p t))
	  (when (function-argument-default-value arg)
	    (push (list symbol`(transform ,transformer ,(function-argument-default-value arg) ,environment))
		  result)
	    (push symbol symbols)
	    (return)))
	(when (typep arg 'rest-function-argument)
	  (push '&rest result)
	  (setf rest-var symbol))
	(push symbol result)
	(push symbol symbols)))
    (values
     (nreverse result)
     (nreverse symbols)
     rest-var)))

(defmethod transform ((transformer simple-evaluator) (form interpreted-function) environment)
  (declare (ignorable transformer environment))
  form)

(defun make-interpreted-lambda-expression (lambda-list body transformer environment)
  (multiple-value-bind (lisp-args variables rest-var)
      (to-lisp-lambda-list lambda-list transformer environment)
    `(lambda ,lisp-args
       ;;TODO declare args ignorable?
       (transform ,transformer ,body
		  (let ((env ,environment))
		    ,@(cl:loop :for i :from 0 :to (1- (fset:size lambda-list))
			       :collect `(setf env (augment-environment
						    env ,(function-argument-name (fset:@ lambda-list i)) +kind-variable+
						    (make-instance 'box :value ,(let ((var (nth i variables)))
										  (if (eq var rest-var)
										      `(fset:convert 'fset:seq ,var)
										      var)))))) ;TODO should they be constant?
		    env)))))

(defmethod transform ((transformer simple-evaluator) (form function) environment)
  (let* ((lambda-list (check-function-lambda-list (function-lambda-list form)))
	 (body (function-expression form))
	 (fn (make-interpreted-lambda-expression lambda-list body transformer environment))
	 (interpreted-function (make-instance 'interpreted-function :lambda-list lambda-list)))
    (closer-mop:set-funcallable-instance-function interpreted-function (compile nil fn))
    interpreted-function))

(defmethod transform ((transformer simple-evaluator) (form function-call) environment)
  (flet ((to-lisp-function (designator)
	   (typecase designator
	     ((or interpreted-function cl:function closer-mop:funcallable-standard-object) designator)
	     (function (transform transformer designator environment))
	     (t (error "Not a function designator: ~S" designator))))) ;TODO proper condition class
    (let* ((function-designator (accessed-function-designator form))
	   (lisp-function
	    (typecase function-designator	    
	      (symbol (let ((meaning (meaning function-designator +kind-function+ environment)))
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
  (common-lisp:eval `(let ,(fset:convert
			    'list
			    (fset:image
			     (lambda (v)
			       (list (fset:@ v 0)
				     (transform transformer (make-instance 'variable-read :name (fset:@ v 1)) environment)))
			     (lisp-variables form)))
		       ,(lisp-expression form))))

(defun eval (form &optional (*environment* *environment*))
  (transform (make-instance 'simple-evaluator) form *environment*))

