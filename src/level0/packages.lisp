(defpackage treep
  (:use :cl)
  (:shadow cl:eval cl:find-symbol cl:function cl:intern cl:load cl:loop cl:quote cl:read cl:symbol cl:symbol-name cl:variable)
  (:export #:binding #:conditional
	   #:environment #:*environment*
	   #:find-symbol #:form-parent #:intern
	   #:load #:load-file
	   #:quote 
	   #:*root-symbol* #:symbol
	   #:variable-access #:variable-definition
	   #:variable-read #:variable-write
	   #:read-form #:transform
	   #:simple-evaluator #:with-read-symbol-syntax))
