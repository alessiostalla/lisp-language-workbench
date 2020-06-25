(in-package :lisp-language-workbench)

(defclass simple-evaluator () ())

(defmethod transform ((transformer simple-evaluator) form environment)
  (if (typep form 'form)
      (error "Don't know how to eval: ~S" form)
      ;Lisp objects are self-evaluating
      form)) 

(defmethod transform ((transformer simple-evaluator) (form binding-form) environment)
  (values
   (transform transformer (binding-form-body form)
	      (augment-environment environment (transform transformer form environment)))
   environment))

(defclass local-variable (binding-spec)
  ((value :initarg :value :reader local-variable-value)))

(defmethod transform ((transformer simple-evaluator) (form variable-binding-spec) environment)
  (make-instance 'local-variable :value (transform transformer (variable-binding-init-form form) environment)))

(defmethod transform ((transformer simple-evaluator) (form variable-read) environment)
  (let ((meaning (meaning (accessed-variable-name form) 'local-variable environment)))
    (if meaning
	(local-variable-value meaning)
	(error (format nil "Unknown variable: ~A" (with-output-to-string (out) (print-symbol (accessed-variable-name form) out))))))) ;TODO proper condition class
