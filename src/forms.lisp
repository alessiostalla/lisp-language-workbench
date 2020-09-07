(in-package :treep)

(defclass environment ()
  ((bindings :initform (fset:map) :initarg :bindings :reader environment-bindings)))

(defun meaning (symbol kind environment)
  (fset:@ (fset:@ (environment-bindings environment) symbol) kind))

(defclass form () ())

(defgeneric transform (transformer form environment))

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

;; TODO a macro is not like in Lisp superficially like a function call.
;; It's a new form (class).
;;(defclass macro (form)
;;  ((arguments :initarg :arguments :reader macro-arguments)
;;   (expression :initarg :expression :reader macro-expression)))

(defclass function-access (form)
  ((function-designator :initarg :function :reader accessed-function-designator)))

(defclass function-reference (function-access) ())

(defclass function-call (function-access)
  ((arguments :initarg :arguments :reader function-call-arguments)))

;;Environment and definitions
(defclass definition (form) ())

(defclass binding (form)
  ((name :initarg :name :reader binding-name)
   (definition :initarg :spec :reader binding-definition :type definition)
   (body :initarg :body :reader binding-body)))

(defclass install-definition! (form)
  ((name :initarg :name :reader definition-name)
   (definition :initarg :spec :reader definition-definition :type definition)))

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

(defclass variable-definition (definition)
  ((init-form :initarg :init-form :initform nil :reader variable-definition-init-form)
   (mutable? :initarg :mutable :initform nil :reader mutable?)))
(defclass function-definition (definition)
  ((init-form :initarg :init-form :initform nil :reader function-definition-init-form)))

(defgeneric definition-kind (transformer definition))
(defmethod definition-kind (transformer (definition variable-definition))
  'variable)
(defmethod definition-kind (transformer (definition function-definition))
  'function)

(defclass lisp (form)
  ((expression :initarg :expression :initform nil :reader lisp-expression)))

(defun initial-environment ()
  (let ((env (make-instance 'environment)))
    (setf env (augment-environment
	       env
	       (intern "the-global-environment" *root-symbol*)
	       'function
	       (make-instance 'function
			      :arguments nil
			      :expression (make-instance 'lisp :expression '*environment*))))
    env))
(defvar *environment* (initial-environment))

(defun default-transform-binding (transformer form environment)
  (transform transformer (binding-body form)
	     (augment-environment environment
				  (binding-name form)
				  (definition-kind transformer (binding-definition form))
				  (transform transformer (binding-definition form) environment))))

(defun default-transform-definition! (transformer form environment)
  (setf *environment*
	(augment-environment *environment*
			     (definition-name form)
			     (definition-kind transformer (definition-definition form))
			     (transform transformer (definition-definition form) environment))))

;;Common forms
(defclass conditional (form)
  ((condition :initarg :condition :reader conditional-if)
   (then :initarg :then :reader conditional-then :type form :initform nil)
   (else :initarg :else :reader conditional-else :type form :initform nil)))

(defclass loop (form)
  ((name :initarg :name :reader loop-name :initform nil)
   (body :initarg :body :reader loop-body)))

(defclass loop-break (form)
  ((loop-name :initarg :loop-name :reader loop-name :initform nil)
   (return-form :initarg :return :reader return-form :initform nil)))
