(in-package :treep-impl)

(defclass quit (form) ())

(defun repl (&key (evaluator (make-instance 'simple-evaluator)) (reader (make-instance 's-expression-reader)))
  (cl:loop
   (format t "~A> " (package-name *package*))
   (force-output)
   (restart-case
       (let ((form (read-form reader *standard-input*)))
	 (when (typep form 'quit) (return))
	 (with-read-symbol-syntax ()
	   (print (transform evaluator form *environment*)))
	 (terpri))
     (ignore-and-continue () :report "Ignore the error.")
     (quit () :report "Quit the Treep REPL." (return)))))
