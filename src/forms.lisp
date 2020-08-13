(in-package :lisp-language-workbench)

(defclass environment ()
  ((bindings :initform (fset:map) :initarg :bindings :reader environment-bindings)))

(defun meaning (symbol kind environment)
  (fset:@ (fset:@ (environment-bindings environment) symbol) kind))

(defclass form () ())
(defclass macro-form (form) ())

(defgeneric transform (transformer form environment))
(defmethod transform (transformer (form macro-form) environment)
  (transform transformer (transform 'expand form environment) environment))

(defclass constant (form)
  ((value :initarg :value :reader constant-value)))

(defclass variable-access (form)
  ((variable-name :initarg :name :reader accessed-variable-name)))

(defclass variable-read (variable-access) ())

(defclass variable-write (variable-access)
  ((form :initarg :form)))

(defclass function (form)
  ((arguments :initarg :arguments :reader function-arguments)
   (expression :initarg :expression :reader function-expression)))

(defclass variable (form)
  ((init-form :initarg :init-form :initform nil :reader variable-init-form)))

(defclass constant (form)
  ((value :initarg :value :reader constant-value)))

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

(defun augment-environment (environment name kind meaning)
  (flet ((compute-meanings ()
	   (fset:with (or (fset:@ (environment-bindings environment) name)
			  (fset:map))
		      kind
		      meaning)))
    (make-instance 'environment :bindings
		   (fset:with (environment-bindings environment)
			      name
			      (compute-meanings)))))

(defclass variable-binding-spec (binding-spec)
  ((init-form :initarg :init-form :initform nil :reader variable-binding-init-form)
   (mutable? :initarg :mutable :initform nil :reader mutable?)))
(defclass function-binding-spec (binding-spec)
  ((init-form :initarg :init-form :initform nil :reader function-binding-init-form)))

(defgeneric binding-spec-kind (transformer binding-spec))
(defmethod binding-spec-kind (transformer (binding-spec variable-binding-spec))
  'variable)
(defmethod binding-spec-kind (transformer (binding-spec function-binding-spec))
  'function)

(defun default-transform-binding (transformer form environment)
  (values
   (transform transformer (binding-body form)
	      (augment-environment environment
				   (binding-name form)
				   (binding-spec-kind transformer (binding-spec form))
				   (transform transformer (binding-spec form) environment)))
   environment))

(defclass conditional-branch ()
  ())

(defclass conditional (form)
  ((condition :initarg :condition :reader conditional-if)
   (then :initarg :then :reader conditional-then)
   (else :initarg :else :reader conditional-else)))

(defclass lisp (form)
  ((expression :initarg :expression :initform nil :reader lisp-expression)))
