(in-package :treep)

(defun import-lisp-package (package &key
				      (space (intern (string-downcase (package-name package)) *symbol-space*))
				      (environment *environment*))
  (do-symbols (s package)
    (setf environment (import-lisp-symbol s :space space :environment environment)))
  environment)

(defun import-lisp-symbol (symbol &key
				    (space *symbol-space*)
				    (into (intern (string-downcase (cl:symbol-name symbol)) space))
				    (environment *environment*))
  (when (fboundp symbol)
    (let* ((rest (make-instance 'symbol :name "args"))
	   (function (make-instance 'function
				    :lambda-list (fset:seq (make-instance 'rest-function-argument :name rest))
				    :expression (make-instance 'lisp
							       :expression `(apply ',symbol (fset:convert 'list args))
							       :variables (fset:seq (fset:seq 'args rest))))))
      ;;TODO handle more specific lambda lists if possible
      (setf environment (augment-environment environment into 'function function))))
  ;;TODO handle variables, classes
  environment)

(setf *environment* (import-lisp-package (find-package :common-lisp)))
(setf *environment* (import-lisp-package (find-package :treep) :space (intern "treep-impl" *root-symbol*)))
