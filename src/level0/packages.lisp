(defpackage treep
  (:use :cl)
  (:shadow cl:class cl:eval cl:find-symbol cl:function cl:generic-function cl:intern cl:load cl:loop cl:quote cl:read cl:symbol cl:symbol-name cl:variable)
  (:export #:binding
	   #:class #:class-definition #:class-reference #:conditional
	   #:environment #:*environment*
	   #:find-symbol #:form #:form-parent #:function #:function-argument #:function-call
	   #:intern
	   #:load #:load-file
	   #:meaning #:method-definition
	   #:new-instance
	   #:quote 
	   #:read-form #:read-symbol #:read-symbol-from-string #:*root-symbol*
	   #:sexp-->form #:simple-evaluator #:slot-definition #:slot-read #:slot-write #:specialized-function-argument #:symbol #:+symbol-treep+
	   #:transform
	   #:variable-access #:variable-definition
	   #:variable-read #:variable-write
	   
	   #:with-read-symbol-syntax))
