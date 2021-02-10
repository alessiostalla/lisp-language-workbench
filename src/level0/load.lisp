(in-package :treep)

(defclass s-expression-reader () ())
(defgeneric read (reader stream))
(defmethod read ((reader s-expression-reader) stream)
  (with-read-symbol-syntax ()
    (let* ((eof 'eof)
	   (sexp (cl:read stream nil eof)))
      (when (eq sexp eof)
	(return-from read))
      (read-form sexp))))

(defun load (stream &key (evaluator (make-instance 'simple-evaluator)) (reader (make-instance 's-expression-reader)))
  (let ((*package* (find-package :treep))
	(environment (copy-environment))) ;So we don't side-effect it
    (cl:loop
     (let ((form (read reader stream)))
       (unless form (return))
       (transform evaluator form environment)))
    environment))

(defun load-file (file &key (evaluator (make-instance 'simple-evaluator)) (reader (make-instance 's-expression-reader)))
  (with-open-file (f file)
    (load f :evaluator evaluator :reader reader)))
