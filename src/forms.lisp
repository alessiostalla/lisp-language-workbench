(in-package :lisp-language-workbench)

(defclass variable-access (form)
  ((variable-name :initarg :name :reader accessed-variable-name)))

(defclass variable-read (variable-access) ())

(defclass variable-write (variable-access)
  ((form :initarg :form :type form)))

(defclass function-access (form)
  ((function-designator :initarg :name :reader accessed-function-designator)))

(defclass function-reference (function-access) ())

(defclass function-call (function-access)
  ((arguments :initarg :arguments :reader function-call-arguments)))

(defclass binding (form)
  ((name :initarg :name :reader binding-name)
   (spec :initarg :spec :reader binding-spec :type binding-spec)
   (body :initarg :body :reader binding-body)))

(defclass binding-spec (form) ())

(defgeneric binding-spec-key (spec))
(defmethod binding-spec-key ((spec binding-spec))
  (class-name (class-of spec)))

(defun augment-environment (environment name binding-spec)
  (flet ((compute-meanings ()
	   (fset:with (or (fset:@ (environment-bindings environment) name)
			  (fset:map))
		      (binding-spec-key binding-spec)
		      binding-spec)))
    (make-instance 'environment :bindings
		   (fset:with (environment-bindings environment)
			      name
			      (compute-meanings)))))

(defclass variable-binding-spec (binding-spec)
  ((init-form :initarg :init-form :initform nil :reader variable-binding-init-form)))
