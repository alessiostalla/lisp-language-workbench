(defpackage lisp-language-workbench
  (:use :cl)
  (:shadow cl:find-symbol cl:function cl:intern cl:symbol cl:symbol-name)
  (:export #:find-symbol #:intern #:*root-symbol* #:symbol
	   #:binding #:conditional #:variable-binding-spec
	   #:variable-access #:variable-read #:variable-write
	   #:environment #:*global-environment* #:transform
	   #:simple-evaluator))
(in-package :lisp-language-workbench)

(defclass symbol-space ()
  ((contents :accessor symbol-space-contents :initform (fset:map))))

(defclass symbol ()
  ((name :accessor symbol-name :initarg :name :type string)
   (container :accessor symbol-container :initarg :container :type symbol :initform nil)
   (space :accessor symbol-space :initarg :space :type symbol-space :initform nil)))

(defgeneric intern (name space))

(defmethod intern (name (space symbol-space))
  (let ((the-name (string name)))
    (or (fset:@ (symbol-space-contents space) the-name)
	(let ((symbol (make-instance 'symbol :name name :container space)))
	  (setf (symbol-space-contents space)
		(fset:with (symbol-space-contents space) name symbol))
	  symbol))))

(defmethod intern (name (space symbol))
  (intern name (or (symbol-space space)
		   (setf (symbol-space space) (make-instance 'symbol-space)))))

(defgeneric find-symbol (name space))

(defmethod find-symbol (name (space symbol-space))
  (let ((the-name (string name)))
    (fset:@ (symbol-space-contents space) the-name)))

(defmethod find-symbol (name (space symbol))
  (when (symbol-space space) (find-symbol name (symbol-space space))))

(defun print-symbol (symbol &optional (stream *standard-output*))
  (when (symbol-container symbol)
    (print-symbol (symbol-container symbol) stream))
  (princ ":" stream)
  (princ (symbol-name symbol) stream))

(defclass environment ()
  ((bindings :initform (fset:map) :initarg :bindings :reader environment-bindings)))

(defun meaning (symbol kind environment)
  (fset:@ (fset:@ (environment-bindings environment) symbol) kind))

(defclass form () ())
(defclass macro-form (form) ())

(defgeneric transform (transformer form environment))
(defmethod transform (transformer (form macro-form) environment)
  (transform transformer (transform 'expand form environment) environment))

(defvar *root-symbol* (make-instance 'symbol :name ""))
(defvar *symbol-space* *root-symbol*)
(defvar *global-environment* (make-instance 'environment))
