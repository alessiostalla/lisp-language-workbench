(in-package :treep-impl)

(defclass lisp (form)
  ((expression :initarg :expression :initform nil :reader lisp-expression)
   (variables :initarg :variables :initform (fset:seq) :reader lisp-variables)))

(defmethod transform ((transformer simple-evaluator) (form lisp) environment)
  (common-lisp:eval `(let ,(fset:convert
			    'list
			    (fset:image
			     (lambda (v)
			       (list (fset:@ v 0)
				     (transform transformer (make-instance 'variable-read :name (fset:@ v 1)) environment)))
			     (lisp-variables form)))
		       ,(lisp-expression form))))

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
      (setf environment (augment-environment environment into +kind-function+ function))))
  ;;TODO handle variables
  (let ((class (find-class symbol nil)))
    (when class
      (setf environment (augment-environment environment into +kind-class+ class)))) ;;TODO handle redefinition through indirection?
  environment)

(defun lisp-symbol (symbol)
  (let ((parent (symbol-parent symbol)))
    (if (and parent (not (eq parent +root-symbol+)))
	(if (or (null (symbol-parent parent)) (eq (symbol-parent parent) +root-symbol+))
	    (cl:intern (string-upcase (symbol-name symbol)) (string-upcase (symbol-name parent)))
	    (error "The symbol ~A doesn't represent any Lisp symbol" symbol)) ;TODO
	(cl:intern (string-upcase (symbol-name symbol)) (find-package :keyword)))))
