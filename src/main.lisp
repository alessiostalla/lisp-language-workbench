(in-package :lisp-language-workbench)

(defun initial-environment ()
  (let ((env (make-instance 'environment)))
    (setf env (augment-environment
	       env
	       (intern "the-global-environment" *root-symbol*)
	       'function
	       (make-instance 'function-binding-spec
			      :init-form (make-instance 'function
							:arguments nil
							:expression (make-instance 'lisp :expression '*global-environment*)))))
    env))
(defvar *global-environment* (initial-environment))
