;
;	MCCARTHY.LSP - 1997 - Bigdogs Internescional.
;
;	Funzioni tratte da McCarthy(1960).
;

(defun diff (y x)
  (cond
    ((atom y)
     (cond
       ((eq y x) 1)
       (t 0)))
    ((eq (car y) '+)
     (cons '+
	   (mapcar #'(lambda (z)
		       (diff z x))
		   (cdr y))))
    ((eq (car y) '*)
     (cons '+
	   (maplist #'(lambda (z)
		        (cons '*
			      (maplist #'(lambda (w)
					   (cond
					     ((neq z w)
					      (car w))
					     (t
					      (diff (car w) x))))
				       (cdr y))))
		    (cdr y))))))


; semplificazione - Bigdogs Internescional.

(defun collect-constants (expr)
  (if (atom expr)
      expr
      (let ((head (car expr))
	    (body
	     (mapcar #'collect-constants (cdr expr))))
        (case head
	  (+ (collect-constants-aux body '+ #'+ 0))
	  (* (collect-constants-aux body '* #'* 1))
	  (otherwise (cons head body))))))

(defun collect-constants-aux (expr
			      opsym
			      opfun
			      noop
			      &optional
			      (const noop)
			      var)
  (cond
    ((endp expr)
     (collect-constants-final opsym
			     noop
			     const
			     (reverse var)))
    ((numberp (car expr))
     (collect-constants-aux (cdr expr)
			    opsym
			    opfun
			    noop
			    (funcall opfun (car expr) const)
			    var))
    (t
     (collect-constants-aux (cdr expr)
			    opsym
			    opfun
			    noop
			    const
			    (cons (car expr) var)))))

(defun collect-constants-final (opsym noop const var)
  (let ((args
	 (if (eql const noop)
	     var
	     (cons const var))))
    (cond
      ((endp args) noop)
      ((and (eql const 0) (eq opsym '*))
       0)
      ((endp (cdr args))
       (car args))
      (t (cons opsym args)))))

(defun collect-operators (expr)
  (if (atom expr)
    expr
    (let ((op (car expr))
	  (args (mapcar #'collect-operators (cdr expr))))
      (cons op
	    (mapcan #'(lambda (arg)
			(cond
			  ((atom arg) (list arg))
			  ((eq (car arg) op)
			   (copy-list (cdr arg)))
			  (t
			   (list arg))))
		    args)))))

