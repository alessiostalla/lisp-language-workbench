(in-package :lisp-language-workbench)

(defclass constant (form)
  ((value :initarg :value :reader constant-value)))

(defclass variable-access (form)
  ((variable-name :initarg :name :reader accessed-variable-name)))

(defclass variable-read (variable-access) ())

(defclass variable-write (variable-access)
  ((form :initarg :form)))

(defclass function (form)
  ((arguments :initform :arguments :reader function-arguments)
   (expression :initform :expression :reader function-expression)))

(defclass function-access (form)
  ((function-designator :initarg :function :reader accessed-function-designator)))

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
(defclass function-binding-spec (binding-spec)
  ((init-form :initarg :init-form :initform nil :reader function-binding-init-form)))

(defclass conditional-branch ()
  ())

(defclass conditional (form)
  ((condition :initarg :condition :reader conditional-if)
   (then :initarg :then :reader conditional-then)
   (else :initarg :else :reader conditional-else)))
