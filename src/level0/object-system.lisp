(in-package :treep)

(defconstant +kind-class+ (intern "class" +symbol-treep+))

(defclass class-definition (definition)
  ((superclasses :initform (fset:seq (find-class 'standard-object)) :initarg :superclasses :accessor class-definition-superclasses)
   (slots :initform (fset:seq) :initarg :slots :accessor class-definition-slots)))

(defclass slot-definition (definition) ())

(defclass generic-function-definition (definition)
  ((lambda-list :initarg :lambda-list :initform (fset:seq) :reader function-lambda-list)))

(defclass generic-function (function)
  ((methods :initform (fset:seq) :accessor generic-function-methods)))

(defclass method-definition (definition)
  ((lambda-list :initarg :lambda-list :initform (fset:seq) :reader method-lambda-list)
   (expression :initarg :expression :reader method-expression :type form)))

(defclass specialized-argument (function-argument)
  ((specializer :initarg :specializer :reader argument-specializer)))

(defmethod definition-kind (transformer (definition class-definition))
  +kind-class+)
(defmethod transform (transformer (form class-definition) environment)
  (make-instance 'standard-class
		 :direct-superclasses (fset:convert 'list (class-definition-superclasses form))
		 :direct-slots (fset:convert 'list (fset:image (lambda (def)
								 (list :name (lisp-symbol (definition-name def)))) (class-definition-slots form))))) ;;TODO

(defmethod definition-kind (transformer (definition generic-function-definition))
  +kind-function+)
;;TODO transform gf

(defmethod compute-new-environment-for-definition (transformer (definition method-definition) environment)
  (let ((gf (meaning (definition-name definition) +kind-function+ environment)))
    (if gf
	(unless (typep gf 'generic-function) ;;TODO check that lambda lists match!
	  (error "Not a generic function: ~A" gf)) ;;TODO proper conditions
	(setf gf (make-instance 'generic-function :lambda-list (fset:image #'as-generic-argument (method-lambda-list definition)))))
    (augment-environment environment (definition-name definition) +kind-function+
			 (make-instance 'generic-function
					:lambda-list (function-lambda-list gf)
					;;TODO replace when specializers match!
					:methods (fset:with-last (generic-function-methods gf) (transform transformer definition environment))))))

(defgeneric as-generic-argument (argument))
(defmethod as-generic-argument ((argument function-argument))
  argument)
(defmethod as-generic-argument ((argument specialized-argument))
  (make-instance 'function-argument :name (function-argument-name argument)))
