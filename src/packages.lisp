(defpackage lisp-language-workbench
  (:use :cl)
  (:shadow cl:find-symbol cl:function cl:intern cl:symbol cl:symbol-name)
  (:export #:find-symbol #:intern #:*root-symbol* #:symbol
	   #:binding #:conditional #:variable-definition
	   #:variable-access #:variable-read #:variable-write
	   #:environment #:*environment* #:read-form #:transform
	   #:simple-evaluator #:with-read-symbol-syntax))
