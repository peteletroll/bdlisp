;
;	LEXICAL.LSP - 1998 - Bigdogs Internescional.
;
;	Scope lessicale.
;

(defun make-lexical-closure (fun)
	((symbolp fun)
		((atom (getd fun))
			nil )
		(format t "Closing ~s~%" fun)
		(putd fun (lambda-closure (getd fun))) )
	nil )

(defun lambda-closure (def &optional vars)
	(replace-variables def (add-new-variables (lex-arglist (cadr def)) vars)) )

(defun replace-variables (def vars)
	(cons (car def) (cons (replace-arglist (cadr def) vars) (replace-tasklist (cddr def) vars))) )

(defun replace-tasklist (tasks vars)
	((endp tasks)
		nil )
	(cons (replace-task (car tasks) vars) (replace-tasklist (cdr tasks) vars)) )

(defun replace-task (task vars)
	(setq task (macroexpand task))
	((atom task)
		((symbolp  task)
			(let ((new (assoc task vars)))
				(if new (cadr new) task) ) )
		task )
	(let ((head (car task)) (body (cdr task)))
		((symbolp head)
			((eq head 'quote)
				(if (eq (caar body) 'lambda)
					(list 'quote (lambda-closure (car body) vars))
					task ) )
			((eq head 'backquote)
				(replace-backquote task vars) )
			((eq head 'cond)
				(cons head
					(mapcar '(lambda (clause)
						(replace-tasklist clause vars)
					) body)) )
			(cons head (replace-tasklist body vars)) )
		(replace-tasklist task vars) ) )

(defun replace-backquote (task vars)
	((atom task)
		task )
	(let ((head (car task)) (body (cdr task)))
		((member head '(comma comma-at))
			(list head (replace-task (car body) vars)) )
		(cons
			(replace-backquote head vars)
			(replace-backquote body vars) ) ) )

(defun replace-arglist (args vars)
	((atom args)
		(let ((new1 (assoc args vars)))
			(if new1 (cadr new1) args) ) )
	(cons (replace-arglist (car args) vars) (replace-arglist (cdr args) vars)) )

(defun add-new-variables (varlist &optional oldlist)
	((endp varlist)
		oldlist )
	(let ((var (car varlist)))
		((get var '*special-variable*)
			(add-new-variables (cdr varlist) oldlist) )
		(add-new-variables (cdr varlist) (cons (list var (new-var-symbol var)) oldlist)) ) )

(defun new-var-symbol (var)
	(gensym (concat (symbol-name var) "-")) )

(putd 'putd-hook '(lambda (sym def)
	((valid-fun-def def)
		(putd sym def)
		(make-lexical-closure sym) )
	(format t "Warning: bad function definition for ~s~%" sym) ))

;;;
;;; estrazione lista argomenti
;;;

(defun lex-arglist (args)
	((null args)
		nil )
	((atom args)
		(list args) )
	((eq (car args) '&optional)
		(lex-optional-arglist (cdr args)) )
	((eq (car args) '&rest)
		(lex-rest-arglist (cdr args)) )
	((eq (car args) '&key)
		(lex-key-arglist (cdr args)) )
	((eq (car args) '&aux)
		(lex-aux-arglist (cdr args)) )
	(cons (car args) (lex-arglist (cdr args))) )

(defun lex-optional-arglist (args)
	((atom args)
		nil )
	((eq (car args) '&rest)
		(lex-rest-arglist (cdr args)) )
	((eq (car args) '&key)
		(lex-key-arglist (cdr args)) )
	((eq '&aux (car args))
		(lex-aux-arglist (cdr args)) )
	((consp (car args))
		((> (length (car args)) 2)
			(append (list (first (car args)) (third (car args)))
				(lex-optional-arglist (cdr args))) )
		(cons (caar args) (lex-optional-arglist (cdr args))) )
	(cons (car args) (lex-optional-arglist (cdr args))) )

(defun lex-rest-arglist (args)
	((> (length args) 1)
		((eq (cadr args) '&key)
			(cons (car args) (lex-key-arglist (cddr args))) )
		((eq (cadr args) '&aux)
			(cons (car args) (lex-aux-arglist (cddr args))) ) )
	(car args) )

; gli argomenti &key vanno ignorati

(defun lex-key-arglist (args)
	((atom args)
		nil )
	((eq '&aux (car args))
		(lex-aux-arglist (cdr args)) )
	(lex-key-arglist (cdr args)) )

(defun lex-aux-arglist (args)
	((atom args)
		nil )
	((consp (car args))
		(cons (caar args) (lex-aux-arglist (cdr args))) )
	(cons (car args) (lex-aux-arglist (cdr args))) )

; (mapc 'make-lexical-closure (oblist))

