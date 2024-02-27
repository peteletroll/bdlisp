;
;	LINT.LSP - 1997 - Bigdogs Internescional.
;
;	Controlli sulle funzioni.
;

(defparameter *lint-context* nil)

(defun lint-all ()
  (mapc #'lint (sort (oblist)))
  (values))

(defun lint (fun)
  (when (symbolp fun)
    (let ((*lint-context* fun) (def (getd fun)))
      (unless (atom def)
	; (format t "Checking ~s~%" fun)
	(lint-lambda def nil)
	(values)))))

(defun lint-lambda (def vars)
  (lint-tasklist (cddr def) (append (lint-arglist (cadr def)) vars)))

(defun lint-warning (msg &rest args)
  (format t "~s: ~a~%"
	  *lint-context*
	  (apply #'format nil msg args)))

(defun lint-tasklist (tasklist vars)
  (dolist (task tasklist)
    (lint-task task vars)))

(defun lint-task (task vars)
  (setq task (macroexpand task))
  (cond
    ((symbolp task)
     (cond
       ((constantp task) nil)
       ((member task vars) nil)
       ((boundp task) nil)
       (t (lint-warning "unbound variable ~s" task))))
    ((atom task) nil)
    ((atom (car task))
     (cond
       ((member (car task) '(function quote))
	(when (eq (caadr task) 'lambda)
	  (lint-lambda (cadr task) vars))
	nil)
       ((eq (car task) 'backquote)
	(lint-backquote task vars))
       ((member (car task) '(lambda macro))
	(lint-lambda task vars))

       ((eq (car task) 'cond)
	(dolist (clause (cdr task))
	  (lint-tasklist clause vars)))

       ((getd (car task))
	(lint-tasklist (cdr task) vars))
       (t
	 (lint-warning "undefined function ~s" (car task))
	 (lint-tasklist (cdr task) vars))))
    (t
      (lint-warning "conditional task ~s" task)
      (lint-tasklist task vars))))

(defun lint-backquote (task vars)
  (cond
    ((atom task)
     nil)
    ((member (car task) '(comma comma-at))
     (lint-task (cadr task) vars))
    (t
      (lint-backquote (car task) vars)
      (lint-backquote (cdr task) vars))))

;;;
;;; estrazione lista argomenti
;;;

(defun lint-arglist (args)
  (cond
    ((null args)
     nil)
    ((atom args)
     (list args))
    ((eq (car args) '&optional)
     (lint-optional-arglist (cdr args)))
    ((eq (car args) '&rest)
     (lint-rest-arglist (cdr args)))
    ((eq (car args) '&key)
     (lint-key-arglist (cdr args)))
    ((eq (car args) '&aux)
     (lint-aux-arglist (cdr args)))
    (t
      (cons (car args) (lint-arglist (cdr args))))))

(defun lint-optional-arglist (args)
  (cond
    ((atom args)
     nil)
    ((eq (car args) '&rest)
     (lint-rest-arglist (cdr args)))
    ((eq (car args) '&key)
     (lint-key-arglist (cdr args)))
    ((eq '&aux (car args))
     (lint-aux-arglist (cdr args)))
    ((consp (car args))
     (cond
       ((> (length (car args)) 2)
	(append (list (first (car args)) (third (car args)))
		(lint-optional-arglist (cdr args))))
       (t
	 (cons (caar args) (lint-optional-arglist (cdr args))))))
    (t
      (cons (car args) (lint-optional-arglist (cdr args))))))

(defun lint-rest-arglist (args)
  (cond
    ((> (length args) 1)
     (cond
       ((eq (cadr args) '&key)
	(cons (car args) (lint-key-arglist (cddr args))))
       ((eq (cadr args) '&aux)
	(cons (car args) (lint-aux-arglist (cddr args))))))
    (t
      (list (car args)))))

(defun lint-key-arglist (args)
  (cond
    ((atom args)
     nil)
    ((eq '&aux (car args))
     (lint-aux-arglist (cdr args)))
    ((consp (car args))
     (cond
       ((> (length (car args)) 2)
	(append (list (first (car args)) (third (car args)))
		(lint-key-arglist (cdr args))))
       (t
	 (cons (caar args) (lint-key-arglist (cdr args))))))
    (t
      (cons (car args) (lint-key-arglist (cdr args))))))

(defun lint-aux-arglist (args)
  (cond
    ((atom args)
     nil)
    ((consp (car args))
     (cons (caar args) (lint-aux-arglist (cdr args))))
    (t
      (cons (car args) (lint-aux-arglist (cdr args))))))

(provide :lint)

