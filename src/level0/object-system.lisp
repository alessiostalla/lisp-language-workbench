(in-package :treep)

(defconstant +kind-class+ (intern "class" +symbol-treep+))

(defclass class-definition (definition)
  ((superclasses :initform (fset:seq (find-class 'standard-object)) :initarg :superclasses :accessor class-definition-superclasses)
   (slots :initform (fset:seq) :initarg :slots :accessor class-definition-slots)))

(defclass slot-definition (definition)
  ((name :initarg :name :accessor slot-definition-name)))

(defclass generic-function-definition (definition)
  ((lambda-list :initarg :lambda-list :initform (fset:seq) :reader function-lambda-list)
   (methods :initform (fset:seq) :accessor generic-function-methods)))

(defclass method-definition (definition)
  ((lambda-list :initarg :lambda-list :initform (fset:seq) :reader method-lambda-list)
   (expression :initarg :expression :reader method-expression)))

(defclass specialized-argument (function-argument)
  ((specializer :initarg :specializer :reader argument-specializer)))

(defmethod definition-kind (transformer (definition class-definition))
  +kind-class+)
(defmethod transform (transformer (form class-definition) environment)
  (make-instance 'standard-class
		 :direct-superclasses (fset:convert 'list (class-definition-superclasses form))
		 :direct-slots (fset:convert 'list (fset:image (lambda (def)
								 (list :name (lisp-symbol (slot-definition-name def)))) (class-definition-slots form))))) ;;TODO

(defmethod definition-kind (transformer (definition generic-function-definition))
  +kind-function+)
;;TODO transform gf

;;TODO how to record methods in environment without destructively modifying the gf? --> maybe record them in the (copied) gf definition and only turn those into CLOS objects at some later time, possibly overriding. Not the most efficient perhaps but otoh it happens rarely.
