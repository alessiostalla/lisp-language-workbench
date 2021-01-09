(in-package :treep)

(defclass class-definition (form)
  ((superclasses :initform (fset:seq (find-class 'standard-object)) :initarg :superclasses :accessor class-definition-superclasses)
   (slots :initform (fset:seq) :initarg :slots :accessor class-definition-slots)))

(defclass slot-definition (form)
  ((name :initarg :name :accessor slot-definition-name)))

(defmethod definition-kind (transformer (definition class-definition))
  'class)
(defmethod transform (transformer (form class-definition) environment)
  (make-instance 'standard-class
		 :direct-superclasses (fset:convert 'list (class-definition-superclasses form))
		 :direct-slots (fset:convert 'list (fset:image (lambda (def)
								 (list :name (lisp-symbol (slot-definition-name def)))) (class-definition-slots form))))) ;;TODO
