(in-package :treep)

(defconstant +symbol-treep+ (intern "treep" *root-symbol*))
(defconstant +kind-function+ (intern "function" +symbol-treep+))
(defconstant +kind-variable+ (intern "variable" +symbol-treep+))

(defclass environment ()
  ((bindings :initform (fset:map) :initarg :bindings :accessor environment-bindings)))

(defclass form ()
  ((parent :accessor form-parent)))

(defgeneric init-subform (parent name child))
(defmethod init-subform ((parent form) (name (eql 'parent)) (child form))
  nil)
(defmethod init-subform ((parent form) name (child form))
  (setf (form-parent child) parent)) ;;TODO check it doesn't already have a parent

(defmethod initialize-instance :after ((instance form) &key &allow-other-keys)
  (cl:loop
     :for slot :in (closer-mop:class-slots (class-of instance))
     :do (let ((name (closer-mop:slot-definition-name slot)))
	   (if (and (slot-boundp instance name) (typep (slot-value instance name) 'form))
	       (init-subform instance name (slot-value instance name)))))) ;;TODO lists of children

(defgeneric transform (transformer form environment))

(defclass quote (form)
  ((form :initarg :form :reader quoted-form)))

(defmethod transform (transformer (form quote) environment)
  (declare (ignore transformer environment))
  (quoted-form form))

(defclass constant (form)
  ((value :initarg :value :reader constant-value)))

(defclass variable-access (form)
  ((variable-name :initarg :name :reader accessed-variable-name)))

(defclass variable-read (variable-access) ())

(defclass variable-write (variable-access)
  ((form :initarg :form)))

(defclass function-argument (form)
  ((name :initarg :name :reader function-argument-name :type symbol)))

(defclass optional-function-argument (function-argument)
  ((default-value :initarg :default-value :initform nil :reader function-argument-default-value)))

(defclass rest-function-argument (function-argument) ())

(defclass function (form)
  ((lambda-list :initarg :lambda-list :initform (fset:seq) :reader function-lambda-list)
   (expression :initarg :expression :reader function-expression)))

(defclass variable (form)
  ((init-form :initarg :init-form :initform nil :reader variable-init-form)))

(defclass constant (form)
  ((value :initarg :value :reader constant-value)))

;; A macro is not like in Lisp, i.e., superficially like a function call.
;; It's a new form (class).
(defclass macro (form)
  ((expansion :initarg :expansion :reader macro-expansion)))

(defun make-form-class (name &optional (superclass (find-class 'form)))
  (make-instance 'standard-class :name name :direct-superclasses (list superclass)))

(defmethod transform (transformer (form macro) environment)
  (let ((expanded (funcall (macro-expansion form) form environment)))
    (transform transformer expanded environment)))

(defclass function-access (form)
  ((function-designator :initarg :function :reader accessed-function-designator)))

(defclass function-reference (function-access) ())

(defclass function-call (function-access)
  ((arguments :initarg :arguments :reader function-arguments)))

;;Environment and definitions
(defclass definition (form)
  ((name :initarg :name :reader definition-name)))

(defclass binding (form)
  ((definition :initarg :definition :reader binding-definition :type definition)
   (body :initarg :body :reader binding-body)))

(defclass define (form)
  ((definition :initarg :definition :reader define-definition :type definition)))

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

;;TODO Should this inherit from variable?
(defclass variable-definition (definition)
  ((init-form :initarg :init-form :initform nil :reader variable-definition-init-form)
   (mutable? :initarg :mutable :initform nil :reader mutable?)))

(defclass function-definition (definition)
  ((init-form :initarg :init-form :initform nil :reader function-definition-init-form)))
(defclass macro-definition (definition)
  ((slot-definitions :initarg :slot-definitions :reader macro-definition-slots)
   (expander-function :initarg :expander-function :reader macro-definition-expander-function)))

(defgeneric definition-kind (transformer definition))
(defmethod definition-kind (transformer (definition variable-definition))
  +kind-variable+)
(defmethod definition-kind (transformer (definition function-definition))
  +kind-function+)

(defclass lisp (form)
  ((expression :initarg :expression :initform nil :reader lisp-expression)
   (variables :initarg :variables :initform (fset:seq) :reader lisp-variables)))

(defun initial-environment ()
  (let ((env (make-instance 'environment)))
    (setf env (augment-environment
	       env
	       (intern "the-global-environment" *root-symbol*)
	       +kind-function+
	       (make-instance 'function
			      :expression (make-instance 'lisp :expression '*environment*))))
    env))

(defvar *environment* (initial-environment))

(defun meaning (symbol kind &optional (environment *environment*))
  (let ((meanings (fset:@ (environment-bindings environment) symbol)))
    (when meanings
      (fset:@ meanings kind))))

(defun copy-environment (&optional (environment *environment*))
  (make-instance 'environment :bindings (environment-bindings environment)))

(defgeneric compute-new-environment-for-definition (transformer definition environment))

(defmethod compute-new-environment-for-definition (transformer (definition definition) environment)
  (augment-environment environment
		       (definition-name definition)
		       (definition-kind transformer definition)
		       (transform transformer definition environment)))

(defmethod transform (transformer (form binding) environment)
  (let ((def (binding-definition form)))
    (transform transformer (binding-body form) (compute-new-environment-for-definition transformer def environment))))

(defmethod transform (transformer (form define) environment)
  (let ((def (define-definition form)))
    (setf (environment-bindings environment)
	  (environment-bindings (compute-new-environment-for-definition transformer def environment)))))

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
