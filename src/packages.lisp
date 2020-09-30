(defpackage treep
  (:use :cl)
  (:shadow cl:find-symbol cl:function cl:intern cl:loop cl:quote cl:symbol cl:symbol-name cl:variable)
  (:export #:binding #:conditional
	   #:find-symbol #:form-parent #:intern
	   #:environment #:*environment*
	   #:*root-symbol* #:symbol
	   #:quote #:variable-definition
	   #:variable-access #:variable-read #:variable-write
	   #:read-form #:transform
	   #:simple-evaluator #:with-read-symbol-syntax))
