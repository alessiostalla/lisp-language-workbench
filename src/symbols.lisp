(in-package :lisp-language-workbench)

(defclass symbol-space ()
  ((name :reader symbol-space-name :initarg :name :type symbol)
   (contents :accessor symbol-space-contents :initform (fset:map))))

(defclass symbol ()
  ((name :reader symbol-name :initarg :name :type string)
   (container :reader symbol-container :initarg :container :type symbol :initform nil)
   (space :accessor symbol-space :initarg :space :type symbol-space :initform nil)))

(defvar *root-symbol* (make-instance 'symbol :name ""))
(defvar *symbol-space* *root-symbol*)

(defgeneric intern (name space))

(defmethod intern (name (space symbol-space))
  (let ((the-name (string name)))
    (or (fset:@ (symbol-space-contents space) the-name)
	(let ((symbol (make-instance 'symbol :name name :container (symbol-space-name space))))
	  (setf (symbol-space-contents space)
		(fset:with (symbol-space-contents space) name symbol))
	  symbol))))

(defmethod intern (name (space symbol))
  (intern name (or (symbol-space space)
		   (setf (symbol-space space) (make-instance 'symbol-space :name space)))))

(defgeneric find-symbol (name space))

(defmethod find-symbol (name (space symbol-space))
  (let ((the-name (string name)))
    (fset:@ (symbol-space-contents space) the-name)))

(defmethod find-symbol (name (space symbol))
  (when (symbol-space space) (find-symbol name (symbol-space space))))

(defun print-symbol (symbol &optional (stream *standard-output*))
  (let ((container (symbol-container symbol)))
    (when (and container (not (eq container *symbol-space*)))
      (print-symbol container stream)
      (princ ":" stream)))
  (princ (symbol-name symbol) stream)
  symbol)

(defun read-symbol (stream)
  (let* ((separator #\:)
	 (symbol-space (if (eql (peek-char t stream) separator)
			   (progn (read-char stream) *root-symbol*)
			   *symbol-space*))
	 continue
	 (symbol-name (with-output-to-string (s)
			(loop
			   (let ((whitespace '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout))
				 (ch (read-char stream nil)))
			     (cond
			       ((eql ch separator)
				(setf continue t)
				(return))
			       ((or (null ch) (member ch whitespace :test #'eql))
				(return))
			       (t (princ ch s)))))))
	 (symbol (intern symbol-name symbol-space)))
    (if continue
	(let ((*symbol-space* symbol))
	  (read-symbol stream))
	symbol)))
