(in-package :treep-impl)

(setf *environment* (import-lisp-package (find-package :common-lisp)))
(setf *environment* (import-lisp-package (find-package :treep-impl) :space +symbol-treep+))
(setf *environment* (import-lisp-package (find-package :closer-mop) :space (intern "mop" +symbol-treep+)))
(setf *environment* (import-lisp-package (find-package :fset) :space (intern "collections" +symbol-treep+)))

(defclass namespace (form)
  ((symbol :initarg :symbol :reader namespace-symbol)))

(defmethod transform ((transformer simple-evaluator) (form namespace) environment)
  (let ((ns (namespace-symbol form)))
    (unless (symbol? ns)
      (error "Not a symbol: ~A" ns))
    (setf *symbol-space* ns)))
