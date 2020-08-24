(in-package :lisp-language-workbench)

(defun initial-environment ()
  (let ((env (make-instance 'environment)))
    (setf env (augment-environment
	       env
	       (intern "the-global-environment" *root-symbol*)
	       'function
	       (make-instance 'function
			      :arguments nil
			      :expression (make-instance 'lisp :expression '*environment*))))
    env))
(defvar *environment* (initial-environment))
