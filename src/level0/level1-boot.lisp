(in-package :treep)

(setf *environment* (import-lisp-package (find-package :common-lisp)))
(setf *environment* (import-lisp-package (find-package :treep) :space (intern "impl" (intern "treep" *root-symbol*))))
(setf *environment* (import-lisp-package (find-package :closer-mop) :space (intern "mop" (intern "treep" *root-symbol*))))

