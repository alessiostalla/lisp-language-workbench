(in-package :treep-impl)

(defgeneric form-template (form))

(defmethod form-template ((form form))
  (let ((class (class-of form)))
    `(,(class-name class) ,@(remove-if (lambda (x) (eq 'parent (closer-mop:slot-definition-name x)))
				       (closer-mop:class-slots class)))))

(defun sexp-->form (sexp)
  (if (consp sexp)
      (cond
	((eq (car sexp) 'list)
	 (fset:convert 'fset:seq (mapcar #'sexp-->form (cdr sexp))))
	((symbolp (car sexp)) (translate-complex-form sexp (find-class (car sexp))))
	((symbol? (car sexp)) (translate-complex-form sexp (meaning (car sexp) +kind-class+)))
	(t (fset:convert 'fset:seq (mapcar #'sexp-->form sexp))))
      sexp))

(defun translate-complex-form (sexp form-class)
  (unless (closer-mop:class-finalized-p form-class)
    (closer-mop:finalize-inheritance form-class))
  (let* ((template (form-template (closer-mop:class-prototype form-class)))
	 (slot-map (list))
	 (compiled-template (mapcar (lambda (x)
				      (if (typep x 'closer-mop:slot-definition)
					  (let* ((slot-name (closer-mop:slot-definition-name x))
						 (sym (make-symbol (format nil "?~A" slot-name))))
					    (push (cons sym slot-name) slot-map)
					    sym)
					  x))
				    template))
	 (matching-sexp (mapcar (lambda (x) (if (consp x) (sexp-->form x) x))
				(ensure-list-length sexp (length compiled-template))))
	 (unification (cl-unification:unify compiled-template matching-sexp))
	 (result (make-instance form-class)))
    (map nil (lambda (f)
	       (map nil (lambda (b)
			  (setf (cl:slot-value result (cdr (assoc (car b) slot-map))) (cdr b)))
		    (cl-unification::frame-bindings f)))
	 (cl-unification::environment-frames unification))
    result))

(defun ensure-list-length (list length &optional fill-with)
  (let ((diff (- length (length list))))
    (if (> diff 0)
	(append list (make-list diff :initial-element fill-with))
	list)))

(defclass sexp-transformer () ())
(defmethod transform ((transformer sexp-transformer) form environment)
  form)
(defmethod transform ((transformer sexp-transformer) (form list) environment)
  `(list ,@(mapcar (lambda (form) (transform transformer form environment)) form)))
(defmethod transform ((transformer sexp-transformer) (form fset:seq) environment)
  `(list ,@(fset:convert 'list (fset:image (lambda (form) (transform transformer form environment)) form))))
(defmethod transform ((transformer sexp-transformer) (form form) environment)
  (let ((template (form-template form)))
    (mapcar (lambda (x)
	      (if (typep x 'closer-mop:slot-definition)
		  (if (slot-boundp form (closer-mop:slot-definition-name x))
		      (transform transformer (cl:slot-value form (closer-mop:slot-definition-name x)) environment)
		      nil)
		  x))
	    template)))

(defmethod print-object ((object form) stream)
  (print `(sexp-->form ',(transform (make-instance 'sexp-transformer) object *environment*)) stream))

(defmethod cl-unification::occurs-in-p ((var cl:symbol) (pat form) env)
  nil) ;To avoid WARNING: Occurrence test unimplemented for pattern...
(defmethod cl-unification::occurs-in-p ((var cl:symbol) (pat symbol) env)
  nil) ;To avoid WARNING: Occurrence test unimplemented for pattern...
(defmethod cl-unification::occurs-in-p ((var cl:symbol) (pat fset:seq) env)
  (fset:find var pat))
(defmethod cl-unification::occurs-in-p ((var cl:symbol) (pat fset:wb-seq) env)
  (fset:find var pat))
