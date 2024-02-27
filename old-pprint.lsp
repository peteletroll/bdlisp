;
;	PPRINT.LSP - 1997 - Bigdogs Internescional.
;
;	Stampa in formato leggibile di oggetti complessi.
;

(defvar *pprint-indent* 4)
(defvar *pprint-width* 60)

(defun pp (sym)
	(if (not (symbolp sym))
		nil 
		(let ((def (copy-tree (getd sym))))
			(cond
				((null def)
					nil )
				((atom def)
					(print def)
					(values) )
				(t
					(format t "\n(~s ~s ~:s~%"
						(get (car def) 'define-macro)
						sym 
						(cadr def) )
					(when (get sym 'documentation)
						(pprint (get sym 'documentation) *pprint-indent*) )
					(pprint-tasks (cddr def) *pprint-indent*)
					(princ " )\n\n")
					(values) ) ) ) ) )

(put 'lambda 'define-macro 'defun)
(put 'macro 'define-macro 'defmacro)

(defun pprint (obj &optional (indent 0) dont-indent-first)
	(pprin1 obj indent dont-indent-first)
	(terpri)
	(values) )

(defun pprin1 (obj &optional (indent 0) dont-indent-first)
	(unless dont-indent-first
		(go-column indent) )
	(if (atom obj)
		(prin1 obj)
		(let ((head (car obj)) (body (cdr obj)))
			(cond
				((symbolp head)
					(cond
						((get head 'macro-character)
							(cond
								((neql (length body) 1)
									(prin1 obj) )
								(t
									(princ (get head 'macro-character))
									(pprin1 (car body) indent t) ) ) )
						(t
							(in-parentheses
								(prin1 head)
								(let ((indent-after (get head 'indent-after 0)))
									(dotimes (i indent-after)
										(unless (atom body)
											(spaces 1)
											(pprin1
												(pop body)
												(+ indent *pprint-indent*)
												t ) ) )
									(funcall
										(get
											head
											'pprint-body
											(if (plusp indent-after)
												'pprint-multitask-body
												'pprint-default-body ) )
										body
										indent ) ) ) ) ) )
				(t
					(pprint-list obj indent nil)
					nil ) ) ) ) )

(defun fits-in-a-line (obj indent)
	(and
		(< (count-leaves obj) 10)
		(<= (+ indent (length (string obj))) *pprint-width*) ) ) 

(defun count-leaves (obj)
	(if (atom obj)
		1
		(+
			(count-leaves (car obj))
			(count-leaves (cdr obj)) ) ) )

(put 'quote     'macro-character "'")
(put 'backquote 'macro-character "`")
(put 'comma     'macro-character ",")
(put 'comma-at  'macro-character ",@")
(put 'function  'macro-character "#'")

(put 'and                  'indent-after 0)
(put 'or                   'indent-after 0)
(put 'do-loop              'indent-after 0)
(put 'prog1                'indent-after 0)
(put 'multiple-value-prog1 'indent-after 0)
(put 'progn                'indent-after 0)
(put 'unwind-protect       'indent-after 0)
(put 'cond                 'indent-after 0)

(put 'when    'indent-after 1)
(put 'unless  'indent-after 1)
(put 'if      'indent-after 1)
(put 'case    'indent-after 1)
(put 'case    'indent-after 1)
(put 'dotimes 'indent-after 1)
(put 'dolist  'indent-after 1)

(put 'catch 'indent-after 1)

(put 'redirect-input  'indent-after 1)
(put 'redirect-output 'indent-after 1)

(put 'multiple-value-bind 'indent-after 2)

(put 'defun         'indent-after 2)
(put 'defun-special 'indent-after 2)
(put 'defmacro      'indent-after 2)
(put 'defsetf       'indent-after 3)

(put 'cond 'pprint-body 'pprint-cond-like-body)
(put 'case 'pprint-body 'pprint-cond-like-body)

(put 'lambda 'pprint-body 'pprint-lambda-like-body)
(put 'macro  'pprint-body 'pprint-lambda-like-body)

(put 'let    'pprint-body 'pprint-let-like-body)
(put 'let*   'pprint-body 'pprint-let-like-body)
(put 'do     'pprint-body 'pprint-let-like-body)
(put 'do*    'pprint-body 'pprint-let-like-body)

(defun pprint-list (lst indent dont-indent-first &aux elem)
	(in-parentheses
		(if (atom-list lst)
			(pprint-atom-list lst indent)
			(do-loop
				(when (null lst)
				  	(return) )
				(setq elem (pop lst))
				(pprin1 elem (+ indent *pprint-indent*) t)
				(when lst
					(princ " ") ) ) ) ) )

(defun pprint-atom-list (lst indent &aux (n 0))
	(do-loop
		(prin1 (pop lst))
		(if (null lst)
		  	(return) )
		(incf n)
		(if (>= n 8)
			(progn
				(terpri)
				(spaces (+ indent *pprint-indent*))
				(setq n 0) )
			(spaces 1) ) ) )

(defun pprint-tasks (lst indent &aux task)
	(do-loop
		(if (null lst)
		  	(return) )
		(setq task (pop lst))
		(if (cond-task-p task)
			(pprint-cond-task task indent)
			(pprin1 task indent) )
		(when lst
			(terpri)) ) )

(defun cond-task-p (task)
	(cond
		((atom task)
			nil )
		((atom (car task))
			nil )
		((member (caar task) '(lambda macro))
			nil )
		(t
			t ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun go-column (col)
	(when (< col (stream-column))	
		(terpri) )
	(spaces (- col (stream-column))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro in-parentheses (&rest body)
	`(let ((start-row (stream-row)))
		(princ "(")
		(prog1
			(progn ,@body)
			(princ (if (eql (stream-row) start-row) ")" " )")) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pprint-default-body (body indent)
	(if (fits-in-a-line body indent)
		(dolist (form body)
			(princ " ")
			(pprin1 form (+ indent *pprint-indent*) t) )
		(pprint-multitask-body body indent) ) )

(defun pprint-multitask-body (body indent)
	(dolist (form body)
		(terpri)
		(pprin1 form (+ indent *pprint-indent*)) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pprint-lambda-like-body (body indent)
	(princ " ")
	(let ((head (pop body)))
		(if (fits-in-a-line head indent)
			(print head)
			(in-parentheses
				(pprint-tasks head (stream-column)) ) )
	)
	(pprint-tasks body (+ indent *pprint-indent*)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pprint-let-like-body (body indent)
	(princ " ")
	(in-parentheses
		(pprint-tasks (pop body) (stream-column)) )
	(pprint-tasks body (+ indent *pprint-indent*)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pprint-cond-like-body (body indent)
	(terpri)
	(pprint-cond-list body (+ indent *pprint-indent*)) )

(defun pprint-cond-list (lst indent)
	(do-loop
		(when (endp lst)
			(return) )
		(let ((task (pop lst)))
			(cond
				((atom task)
					(go-column indent)
					(prin1 task) )
				(t
					(pprint-cond-task task indent) ) ) )
		(when lst
			(terpri)) ) )

(defun pprint-cond-task (task indent)
	(go-column indent)
	(in-parentheses
		(pprin1 (pop task) (+ indent *pprint-indent*) t)
		(when task
			(terpri) )
		(pprint-tasks task (+ indent *pprint-indent*)) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun atom-list (lst)
	(if (atom lst)
		nil
		(every #'atom lst) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun describe (sym)
	(format t "~s:~%" sym)
	(let* ((props (sort (getprops sym)))
			(l (max-width props)) )
		(dolist (prop (sort (getprops sym)))
			(format t "~a~%"
				(truncate-string
					(format nil "  ~vs = ~s" l prop (get sym prop))
					*pprint-width* ) ) ) )
	(values) )

(defun max-width (lst)
	(apply #'max
		(mapcar #'(lambda (sym) (length (string sym))) lst) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun truncate-string (str len &aux (suf "..."))
	(if (<= (length str) len)
		str
		(concat (substr str 0 (- len (length suf))) suf) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unexpand-macro-characters (obj &aux cmacro)
	(cond
		((atom obj)
			(format nil "~s" obj) )
		(t
			(when (symbolp (car obj))
				(setq cmacro
					(get (car obj) 'macro-character) ) )
			(if (and cmacro (not (cddr obj)))
				(concat cmacro (unexpand-macro-characters (cadr obj)))
				(concat "(" (unexpand-macro-characters-aux obj) ")") ) ) ) )

(defun unexpand-macro-characters-aux (lst)
	(cond
		((atom lst)
			(if (null lst)
				""
				(concat ". " (string lst)) ) )
		((null (cdr lst))
			(unexpand-macro-characters (car lst)) )
		(t
			(concat
				(unexpand-macro-characters (car lst))
				" "
				(unexpand-macro-characters-aux (cdr lst)) ) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide :pprint)

