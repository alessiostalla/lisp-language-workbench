(in-package :lisp-language-workbench)

(defgeneric form-template (form))

(defmethod form-template ((form form))
  (let ((class (class-of form)))
    `(,(class-name class) ,@(closer-mop:class-slots class))))

(defun read-form (sexp)
  (let* ((form-class (find-class (car sexp)))
	 (template (form-template (closer-mop:class-prototype form-class)))
	 (slot-map (list))
	 (compiled-template (mapcar (lambda (x)
				      (if (typep x 'closer-mop:slot-definition)
					  (let* ((slot-name (closer-mop:slot-definition-name x))
						 (sym (make-symbol (format nil "?~A" slot-name))))
					    (push (cons sym slot-name) slot-map)
					    sym)
					  x))
				    template))
	 (matching-sexp (mapcar (lambda (x) (if (consp x) (read-form x) x))
				(ensure-list-length sexp (length compiled-template))))
	 (unification (cl-unification:unify compiled-template matching-sexp))
	 (result (make-instance form-class)))
    (map nil (lambda (f)
	       (map nil (lambda (b)
			  (setf (slot-value result (cdr (assoc (car b) slot-map))) (cdr b)))
		    (cl-unification::frame-bindings f)))
	 (cl-unification::environment-frames unification))
    result))

(defun ensure-list-length (list length &optional fill-with)
  (let ((diff (- length (length list))))
    (if (> diff 0)
	(append list (make-list diff :initial-element fill-with))
	list)))
