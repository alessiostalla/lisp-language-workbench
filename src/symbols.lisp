(in-package :treep)

(defclass symbol-space ()
  ((name :reader symbol-space-name :initarg :name :type symbol)
   (contents :accessor symbol-space-contents :initform (fset:map))))

(defclass symbol ()
  ((name :reader symbol-name :initarg :name :type string)
   (container :reader symbol-container :initarg :container :type symbol :initform nil)
   (space :accessor symbol-space :initarg :space :type symbol-space :initform nil)))

(defvar *root-symbol* (make-instance 'symbol :name ""))
(defvar *symbol-space* *root-symbol*)
(defvar *read-symbol-syntax* nil)

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
			(cl:loop
			   (let ((whitespace '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout))
				 (terminating-chars '(#\( #\) #\#))
				 (ch (read-char stream nil)))
			     (cond
			       ((eql ch separator)
				(setf continue t)
				(return))
			       ((or (null ch) (member ch whitespace :test #'eql) (member ch terminating-chars :test #'eql))
				(when ch (unread-char ch stream))
				(return))
			       (t (princ ch s)))))))
	 (symbol (intern symbol-name symbol-space)))
    (if continue
	(let ((*symbol-space* symbol))
	  (read-symbol stream))
	symbol)))

(defun read-symbol-from-string (s)
  (with-input-from-string (s s)
    (read-symbol s)))

(defvar *symbol-dispatch-macro-character* nil)
(defvar *symbol-dispatch-sub-character* nil)

(defmethod print-object ((object symbol) stream)
  (cond
    (*symbol-dispatch-macro-character*
     (princ *symbol-dispatch-macro-character* stream)
     (when *symbol-dispatch-sub-character*
       (princ *symbol-dispatch-sub-character* stream))
     (print-symbol object stream))
    (*read-eval*
     (princ "#.(" stream)
     (write 'read-symbol-from-string :stream stream)
     (princ " \"" stream)
     (print-symbol object stream)
     (princ "\")" stream))
    (t (print-unreadable-object (object stream :type t :identity t)
	 (print-symbol object stream))))
  nil)

(defmacro with-read-symbol-syntax ((&optional (dispatch-char #\#) (sub-char #\^)) &body body)
  `(let ((*readtable* (copy-readtable))
	 (*symbol-dispatch-macro-character* ,dispatch-char)
	 (*symbol-dispatch-sub-character* ,sub-char))
     ,(if sub-char
	  `(set-dispatch-macro-character ,dispatch-char ,sub-char
					 (lambda (stream sub-char infix)
					   (declare (ignore sub-char infix))
					   (read-symbol stream)))
	  `(set-macro-character ,dispatch-char
				(lambda (stream char)
				  (declare (ignore char))
				  (read-symbol stream))))
     ,@body))
