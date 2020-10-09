(in-package :treep)

(defclass symbol-space ()
  ((name :reader symbol-space-name :initarg :name :type symbol)
   (contents :accessor symbol-space-contents :type fset:map :initform (fset:map))
   (search-path :accessor symbol-space-search-path :initarg :search-path :type fset:seq :initform (fset:seq))))

(defclass symbol ()
  ((name :reader symbol-name :initarg :name :type string)
   (container :reader symbol-container :initarg :container :type symbol :initform nil)
   (space :accessor symbol-space :initarg :space :type symbol-space :initform nil)))

(defvar *root-symbol* (make-instance 'symbol :name ""))
(defvar *symbol-space* *root-symbol*)
(defvar *read-symbol-syntax* nil)

(defun intern (name &optional (space *symbol-space*))
  (let ((the-name (string name))
	(space (typecase space
		 (symbol-space space)
		 (symbol (or (symbol-space space)
			     (setf (symbol-space space) (make-instance 'symbol-space :name space))))
		 (t (error "Not a symbol space designator: ~S" space))))) ;TODO dedicated condition
    (or (find-symbol the-name space)
	(let ((symbol (make-instance 'symbol :name the-name :container (symbol-space-name space))))
	  (setf (symbol-space-contents space)
		(fset:with (symbol-space-contents space) the-name symbol))
	  symbol))))

(defun find-symbol (name &optional (space *symbol-space*) exclude)
  (let ((the-name (string name))
	(space (typecase space
		 (symbol-space space)
		 (symbol (or (symbol-space space) (return-from find-symbol)))
		 (t (error "Not a symbol space designator: ~S" space))))) ;TODO dedicated condition
    (when (member space exclude)
      (return-from find-symbol))
    (let ((symbol (fset:@ (symbol-space-contents space) the-name)))
      (if symbol
	  symbol
	  (fset:do-seq (s (symbol-space-search-path space))
	    (unless (member s exclude)
	      (push s exclude)
	      (let ((symbol (find-symbol name s exclude)))
		(when symbol (return-from find-symbol symbol)))))))))

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
