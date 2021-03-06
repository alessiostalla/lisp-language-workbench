(in-package :treep-impl)

(defclass class-definition (definition)
  ((superclasses :initform (fset:seq) :initarg :superclasses :accessor class-definition-superclasses)
   (slots :initform (fset:seq) :initarg :slots :accessor class-definition-slots)
   (metaclass :initform 'standard-class :initarg :metaclass :accessor class-definition-metaclass)))

(defclass class-reference (form)
  ((class-name :initarg :class-name :reader referenced-class)))

(defclass slot-definition (definition) ())

(defclass generic-function-definition (definition)
  ((lambda-list :initarg :lambda-list :initform (fset:seq) :reader function-lambda-list)))

(defclass generic-function (form)
  ((lambda-list :initarg :lambda-list :initform (fset:seq) :reader function-lambda-list)
   (methods :initarg :methods :initform (fset:seq) :reader generic-function-methods)))

(defclass method-definition (definition)
  ((lambda-list :initarg :lambda-list :initform (fset:seq) :reader method-lambda-list)
   (expression :initarg :expression :reader method-expression :type form)))

(defclass specialized-function-argument (function-argument)
  ((specializer :initarg :specializer :reader argument-specializer)))

(defclass slot-access (form)
  ((object :initarg :object :reader accessed-object)
   (slot-name :initarg :slot-name :reader accessed-slot-name)))

(defclass slot-read (slot-access) ())

(defclass slot-write (slot-access)
  ((new-value :initarg :new-value :reader slot-write-new-value)))

(defclass new-instance (form)
  ((class :initarg :class :reader new-instance-class)
   (slot-values :initarg :slot-values :initform (fset:seq) :reader new-instance-slots)))

(defclass slot-value (form)
  ((name :initarg :name :reader slot-name)
   (value :initarg :value :reader slot-initial-value)))

(defmethod definition-kind (transformer (definition class-definition))
  +kind-class+)
(defmethod transform (transformer (form class-definition) environment)
  (make-instance 'standard-class ;;TODO metaclass
		 :direct-superclasses (or (fset:convert 'list (class-definition-superclasses form)) (list (find-class 'standard-object)))
		 :direct-slots (fset:convert 'list (fset:image (lambda (def)
								 (list :name (lisp-symbol (definition-name def)) :initargs (list (lisp-symbol (definition-name def)))))
							       (class-definition-slots form))))) ;;TODO accessors, etc.

(defmethod definition-kind (transformer (definition generic-function-definition))
  +kind-function+)

(defmethod compute-new-environment-for-definition (transformer (definition method-definition) environment)
  (let ((gf (meaning (definition-name definition) +kind-function+ environment)))
    (if gf
	(unless (typep gf 'generic-function) ;;TODO check that lambda lists match!
	  (error "Not a generic function: ~A" gf)) ;;TODO proper conditions
	(setf gf (transform transformer
			    (make-instance 'generic-function-definition
					   :name (definition-name definition)
					   :lambda-list (fset:image #'as-generic-argument (method-lambda-list definition)))
			    environment)))
    (augment-environment environment
			 (definition-name definition)
			 +kind-function+
			 (transform transformer 
				    (make-instance 'generic-function
						   :lambda-list (function-lambda-list gf)
						   ;;TODO replace when specializers match!
						   :methods (fset:with-last (generic-function-methods gf) definition))
				    environment))))

(defgeneric as-generic-argument (argument))
(defmethod as-generic-argument ((argument function-argument))
  argument)
(defmethod as-generic-argument ((argument specialized-function-argument))
  (make-instance 'function-argument :name (function-argument-name argument)))

(defgeneric compute-argument-specializer (argument environment))
(defmethod compute-argument-specializer ((argument function-argument) environment)
  (declare (ignore environment))
  (find-class 't))
(defmethod compute-argument-specializer ((argument optional-function-argument) environment)
  (declare (ignore environment))
  nil)
(defmethod compute-argument-specializer ((argument rest-function-argument) environment)
  (declare (ignore environment))
  nil)
(defmethod compute-argument-specializer ((argument specialized-function-argument) environment)
  (let ((specializer (argument-specializer argument)))
    (typecase specializer
      (symbol
       (let ((class (meaning specializer +kind-class+ environment)))
	 (if class class (error "There is no class named ~A" (function-argument-name argument))))) ;;TODO proper condition
      (cl:class specializer)
      (t (error "Not a valid specializer: ~A" specializer))))) ;;TODO proper condition

;;Evaluation
(defmethod transform ((transformer simple-evaluator) (form class-reference) environment)
  (let ((class (transform transformer (referenced-class form) environment)))
    (typecase class
      (cl:class class)
      (symbol
       (or (meaning class +kind-class+ environment)
	   (error "There is no class named ~A" class))) ;;TODO proper condition
      (t (error "Not a class designator: ~A" class))))) ;;TODO proper condition

(defclass interpreted-generic-function (interpreted-function generic-function) () (:metaclass closer-mop:funcallable-standard-class))

(defmethod transform ((transformer simple-evaluator) (form generic-function-definition) environment)
  (transform transformer (make-instance 'interpreted-generic-function :lambda-list (function-lambda-list form)) environment))
(defmethod transform ((transformer simple-evaluator) (form generic-function) environment)
  (transform transformer (make-instance 'interpreted-generic-function
					:lambda-list (function-lambda-list form)
					:methods (generic-function-methods form))
	     environment))

(defmethod transform ((transformer simple-evaluator) (form generic-function) environment)
  (let* ((lambda-list (check-function-lambda-list (function-lambda-list form)))
	 (lisp-lambda-list (to-lisp-lambda-list lambda-list transformer environment))
	 (gf (make-instance 'closer-mop:standard-generic-function :name (make-symbol "generic-function") :lambda-list lisp-lambda-list)) ;;TODO carry a meaningful name
	 (interpreted-function (make-instance 'interpreted-generic-function :lambda-list lambda-list)))
    (fset:do-seq (m (generic-function-methods form))
      ;;TODO check lambda list matches here too?
      (let* ((lambda-list (check-function-lambda-list (method-lambda-list m)))
	     (body (method-expression m))
	     (fn (make-interpreted-lambda-expression lambda-list body transformer environment)))
	(closer-mop:ensure-method gf fn
				  :specializers (fset:convert 'list
							      (fset:remove nil
									   (fset:image
									    (lambda (arg) (compute-argument-specializer arg environment))
									    lambda-list))))))
    (closer-mop:set-funcallable-instance-function interpreted-function gf)
    interpreted-function))

(defmethod transform ((transformer simple-evaluator) (form new-instance) environment)
  (let ((class (transform transformer (new-instance-class form) environment)))
    (apply #'make-instance
	   (typecase class
	     (cl:class class)
	     (symbol (or (meaning class +kind-class+ environment) (error "Unknown class: ~A" class))) ;;TODO proper conditions!
	     (t (error "Not a class designator: ~A" class))) ;;TODO proper conditions!
	   (fset:convert 'list (fset:reduce #'fset:concat
					    (fset:image (lambda (slot) (fset:seq (slot-name slot) (slot-initial-value slot)))
							(new-instance-slots form))
					    :initial-value (fset:seq))))))

(defmethod transform ((transformer simple-evaluator) (form slot-read) environment)
  (cl:slot-value (transform transformer (accessed-object form) environment) (lisp-symbol (transform transformer (accessed-slot-name form) environment))))

(defmethod transform ((transformer simple-evaluator) (form slot-write) environment)
  (setf (cl:slot-value (transform transformer (accessed-object form) environment) (lisp-symbol (transform transformer (accessed-slot-name form) environment)))
	(transform transformer (slot-write-new-value form) environment)))
