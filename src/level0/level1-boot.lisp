(in-package :treep)

(setf *environment* (import-lisp-package (find-package :common-lisp)))
(setf *environment* (import-lisp-package (find-package :treep) :space (intern "impl" +symbol-treep+)))
(setf *environment* (import-lisp-package (find-package :closer-mop) :space (intern "mop" +symbol-treep+)))

