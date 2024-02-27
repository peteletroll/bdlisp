;
;	PPRINT.LSP - 2006 - Bigdogs Internescional.
;
;	Stampa in formato leggibile di oggetti complessi.
;

(defun pp (sym &optional (stream *standard-output*))
  (terpri stream)
  (pp1 sym stream)
  (terpri stream)
  (terpri stream)
  (values))

(defun pp1 (sym &optional (stream *standard-output*))
  (pprin1 (symbol-function-definition sym) stream)
  (values))

(defun pprint (obj &optional (stream *standard-output*))
  (prog1
    (pprin1 obj stream)
    (terpri stream)))

(defun pprin1 (obj &optional (stream *standard-output*))
  (redirect-output stream
    (prog1
      obj
      (or
	(pprin1-closure obj)
	(pprin1-atom obj)
	(pprin1-macro-character obj)
	(pprin1-macro-call obj)
	(pprin1-function-call obj)
	(pprin1-list obj)
	(error "can't pprin1")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun symbol-function-definition (sym)
  (let ((def (getd sym)))
    (if (atom def)
      def
      (let* ((head (car def))
	     (args (cadr def))
	     (body (cddr def))
	     (doc (get sym 'documentation)))
	(append (list (get head 'define-macro head) sym args)
		(if doc (list doc) ())
		body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'quote     'macro-character "'")
(put 'backquote 'macro-character "`")
(put 'comma     'macro-character ",")
(put 'comma-at  'macro-character ",@")
(put 'function  'macro-character "#'")

(put 'lambda 'define-macro 'defun)
(put 'macro  'define-macro 'defmacro)

(put 'catch                'pprint-indent-after 1)
(put 'cond                 'pprint-indent-after nil)
(put 'defsetf              'pprint-indent-after 1)
(put 'do-loop              'pprint-indent-after 0)
(put 'do                   'pprint-indent-after 1)
(put 'do*                  'pprint-indent-after 1)
(put 'if                   'pprint-indent-after nil)
(put 'lambda               'pprint-indent-after 1)
(put 'macro                'pprint-indent-after 1)
(put 'multiple-value-prog1 'pprint-indent-after 0)
(put 'prog1                'pprint-indent-after 0)
(put 'progn                'pprint-indent-after 0)
(put 'setf                 'pprint-indent-after nil)
(put 'unwind-protect       'pprint-indent-after 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *pprint-single-row* nil)

(defparameter *pprint-current-indent* 0)

(defun pprint-nl ()
  (cond 
    (*pprint-single-row*
      (pprint-sp))
    (t
      (terpri)
      (pprint-spaces *pprint-current-indent*))))

(defun pprint-spaces (n)
  (cond
    ((>= n 8)
     (princ "\t")
     (pprint-spaces (- n 8)))
    (t
      (spaces n))))

(defun pprint-sp ()
  (princ " "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *pprint-single-row-nodes-limit* 6)

(defun pprin1-single-row (obj)
  (or *pprint-single-row*
      (nodes-limit obj *pprint-single-row-nodes-limit*)))

(defun nodes-limit (obj limit)
  (cond
    ((closurep obj)
     (nodes-limit (getd obj) (- limit 1)))
    ((atom obj) limit)
    ((and (symbolp (car obj))
	  (get (car obj) 'macro-character)
	  (null (cddr obj)))
     (nodes-limit (cadr obj) limit))
    ((< limit 1) nil)
    (t
      (let ((check-car (nodes-limit (car obj) (- limit 1))))
	(and check-car
	     (nodes-limit (cdr obj) check-car))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *pprint-closures* t)

(defun pprin1-closure (obj)
  (when (and *pprint-closures* (closurep obj))
    (pprin1-after "#<closure "
      (pprin1 (getd obj))
      (princ ">")
      t)))

(defun pprin1-atom (obj)
  (when (atom obj)
     (prin1 obj)
     t))

(defun pprin1-macro-character (obj)
  (when (and (consp obj)
	     (consp (cdr obj))
	     (null (cddr obj))
	     (symbolp (car obj)))
    (let ((m (get (car obj) 'macro-character)))
      (when m
	(pprin1-after m
	  (pprin1 (cadr obj)))
	t))))

(defun pprin1-macro-call (obj)
  (when (and (consp obj)
	     (consp (cdr obj))
	     (symbolp (car obj))
	     (not *pprint-single-row*)
	     (macro-indent-after (car obj)))
    (let* ((head (car obj))
	   (body (cdr obj))
	   (i (macro-indent-after head)))
      (pprin1-after (format nil "(~s" head)
	(setq body (pprin1-macro-call-aux body i)))
      (when body
      	(pprint-nl)
	(pprin1-after "  "
	  (pprin1-iterate body
	    (pprint-nl))))
      (princ ")")
      t)))

(defun macro-indent-after (sym)
  (cond
    ((not (symbolp sym)) nil)
    ((get sym 'pprint-indent-after))
    ((and (macrop sym) (consp (getd sym)))
     (find-indent-after (cadr (getd sym))))))

(defun find-indent-after (lambda-list &optional (count 0))
  (cond
    ((endp lambda-list)
     nil)
    ((member (car lambda-list) '(&rest))
     count)
    ((member (car lambda-list) '(&optional &key &aux))
     (find-indent-after (cdr lambda-list) count))
    (t
      (find-indent-after (cdr lambda-list) (1+ count)))))
	
(defun pprin1-macro-call-aux (body pprint-indent-after)
  (cond
    ((and (> pprint-indent-after 0)
	  (consp body))
       (if (pprin1-single-row (car body))
         (pprin1-after (output-string
			 (let ((*pprint-single-row* t))
			   (pprint-sp)
			   (pprin1 (car body))))
       	   (pprin1-macro-call-aux (cdr body) (1- pprint-indent-after)))
	 (pprin1-after " "
	   (pprin1 (car body))
	   (cdr body))))
    (t
      body)))

(defun pprin1-function-call (obj)
  (when (and (consp obj)
	     (consp (cdr obj))
	     (symbolp (car obj))
	     (getd (car obj)))
    (let* ((*pprint-single-row* (pprin1-single-row obj)))
      (pprin1-after (format nil "(~s " (car obj))
	(pprin1-iterate (cdr obj)
	  (pprint-nl))
	(princ ")"))
      t)))

(defun pprin1-list (obj)
  (let ((inter (if (pprin1-single-row obj) #'pprint-sp #'pprint-nl)))
    (pprin1-after "("
      (pprin1-iterate obj
	(funcall inter))
      (princ ")"))
    t))

(defmacro pprin1-after (head-string &rest body)
  `(let* ((hs ,head-string)
	  (*pprint-current-indent* (+ (length hs) *pprint-current-indent*)))
     (princ hs)
     ,@body))

(defmacro pprin1-iterate (list &rest inter-body)
  `(let ((iter-list ,list))
     (do-loop
       (when (atom iter-list)
	 (return))
       (pprin1 (pop iter-list))
       (unless (atom iter-list)
	 ,@inter-body))
     (when iter-list
       ,@inter-body
       (pprin1-after ". "
       	 (pprin1 iter-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun describe (sym &optional pretty)
  (when (getprops sym)
    (format t "~s:~%" sym)
    (let* ((props (sort (getprops sym)))
	   (l (max-width props)))
      (dolist (prop props sym)
	(let ((prefix (format nil "  ~vs = " l prop)))
	  (if pretty
	    (pprin1-after prefix
			  (pprin1 (get sym prop))
			  (terpri))
	    (format t "~a~a~%"
		    prefix
		    (truncate-string (unexpand-macro-characters (get sym prop))
				     (- 70 l))))))))
    (values))

(defun max-width (lst)
  (apply #'max
	 (mapcar #'(lambda (sym) (length (string sym))) lst)))

(defun truncate-string (str len &aux (suf "..."))
  (if (<= (length str) len)
    str
    (concat (substr str 0 (- len (length suf))) suf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unexpand-macro-characters (obj)
  (let ((*pprint-single-row* t))
    (output-string (pprin1 obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide :pprint)

