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

(defvar *global-environment* (make-instance 'environment))
