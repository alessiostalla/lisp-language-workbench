(defpackage treep
  (:use :cl)
  (:shadow cl:class cl:eval cl:find-symbol cl:function cl:generic-function cl:intern cl:load cl:loop cl:quote cl:read cl:symbol cl:symbol-name cl:variable)
  (:export #:binding
	   #:class #:conditional
	   #:environment #:*environment*
	   #:find-symbol #:form #:form-parent #:function #:function-argument #:function-call
	   #:intern
	   #:load #:load-file
	   #:method-definition
	   #:quote 
	   #:read-form #:*root-symbol*
	   #:symbol
	   #:simple-evaluator #:specialized-function-argument
	   #:transform
	   #:variable-access #:variable-definition
	   #:variable-read #:variable-write
	   
	   #:with-read-symbol-syntax))
