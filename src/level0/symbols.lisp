(in-package :treep-impl)

(defclass symbol-space ()
  ((name :reader symbol-space-name :initarg :name :type symbol)
   (contents :accessor symbol-space-contents :type fset:map :initform (fset:map))
   (search-path :accessor symbol-space-search-path :initarg :search-path :type fset:seq :initform (fset:seq))))

(defclass symbol ()
  ((name :reader symbol-name :initarg :name :type string)
   (parent :reader symbol-parent :initarg :parent :type symbol :initform nil)
   (space :accessor symbol-space :initarg :space :type symbol-space :initform nil)))

(defun %intern (name space)
  (let ((the-name (string name))
	(space (typecase space
		 (symbol-space space)
		 (symbol (or (symbol-space space)
			     (setf (symbol-space space) (make-instance 'symbol-space :name space))))
		 (t (error "Not a symbol space designator: ~S" space))))) ;TODO dedicated condition
    (or (%find-symbol the-name space)
	(let ((symbol (make-instance 'symbol :name the-name :parent (symbol-space-name space))))
	  (setf (symbol-space-contents space)
		(fset:with (symbol-space-contents space) the-name symbol))
	  symbol))))

(defun %find-symbol (name space &optional exclude)
  (let ((the-name (string name))
	(space (typecase space
		 (symbol-space space)
		 (symbol (or (symbol-space space) (return-from %find-symbol)))
		 (t (error "Not a symbol space designator: ~S" space))))) ;TODO dedicated condition
    (when (member space exclude)
      (return-from %find-symbol))
    (let ((symbol (fset:@ (symbol-space-contents space) the-name)))
      (if symbol
	  symbol
	  (fset:do-seq (s (symbol-space-search-path space))
	    (unless (member s exclude)
	      (push s exclude)
	      (let ((symbol (%find-symbol name s exclude)))
		(when symbol (return-from %find-symbol symbol)))))))))

(defvar +root-symbol+ (make-instance 'symbol :name "")) ;;Note this should be a constant but making it a constant is complex
(defvar +symbol-treep+ (%intern "treep" +root-symbol+))
(defvar *symbol-space* +symbol-treep+)
(defvar *read-symbol-syntax* nil)

(defun intern (name &optional (space *symbol-space*))
  (%intern name space))

(defun find-symbol (name &optional (space *symbol-space*) exclude)
  (%find-symbol name space exclude))

(defun print-symbol (symbol &optional (stream *standard-output*))
  (let ((parent (symbol-parent symbol)))
    (when (and parent (not (eq parent *symbol-space*)))
      (print-symbol parent stream)
      (princ ":" stream)))
  (princ (symbol-name symbol) stream)
  symbol)

(defun read-symbol (stream &optional (intern-function #'intern))
  (let* ((separator #\:)
	 (symbol-space (if (eql (peek-char t stream) separator)
			   (progn (read-char stream) +root-symbol+)
			   *symbol-space*))
	 continue
	 (symbol-name (with-output-to-string (s)
			(cl:loop
			   (let ((whitespace '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout))
				 (terminating-chars '(#\( #\) #\# #\[ #\]))
				 (ch (read-char stream nil)))
			     (cond
			       ((eql ch separator)
				(setf continue t)
				(return))
			       ((or (null ch) (member ch whitespace :test #'eql) (member ch terminating-chars :test #'eql))
				(when ch (unread-char ch stream))
				(return))
			       (t (princ ch s)))))))
	 (symbol (funcall intern-function symbol-name symbol-space)))
    (if continue
	(let ((*symbol-space* symbol))
	  (read-symbol stream intern-function))
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

(defun symbol? (object)
  (typep object 'symbol))
