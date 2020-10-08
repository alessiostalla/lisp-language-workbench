(in-package :treep)

(defun import-lisp-package (package &key (space (intern (package-name package) *symbol-space*)) (environment *environment*))
  (do-symbols (s package)
    (setf environment (import-lisp-symbol s :space space :environment environment)))
  environment)

(defun import-lisp-symbol (symbol &key (space *symbol-space*) (environment *environment*))
  (let ((treep-symbol (intern (cl:symbol-name symbol) space)))
    (when (fboundp symbol)
      (let* ((rest (intern "args" treep-symbol)) 
	     (function (make-instance 'function
				     :lambda-list (fset:seq (make-instance 'rest-function-argument :name rest))
				     :expression (make-instance 'lisp
								:expression `(apply ',symbol (fset:convert 'list args))
								:variables (fset:seq (fset:seq 'args rest))))))
	;;TODO handle more specific lambda lists if possible
	(setf environment (augment-environment environment treep-symbol 'function function)))))
  environment)
