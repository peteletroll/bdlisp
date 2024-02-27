;
;	SOLVE.LSP -  1997 - Bigdogs Internescional.
;
;	Risoluzione equazioni con bisezione.
;

(defun solve (fun &optional a b)
	(cond
		((not (numberp a))
			(setq a 0)
			(setq b 1) )
		((not (numberp b))
			(setq b (* 2 a)) ) )
	(catch 'limit
		(solve-aux fun a b) ) )

(defun solve-aux (fun a b &aux fa fb c fc n)
	(setq n 0)
	(loop
		(incf n)
		(when (> n 1000)
			(throw 'limit 'expansion) )
		(setq fa (funcall fun a))
		(setq fb (funcall fun b))
		((minusp (* fa fb)) )
		(if (< (abs fa) (abs fb))
			(setq a (- a (- b a)))
			(setq b (+ b (- b a))) ) )
	(loop
		(incf n)
		(when (> n 1000)
			(throw 'limit 'reduction) )
		(setq c (+ a (/ (- b a) 2)))
		((= a c) c)
		((= b c) c)
		(setq fc (funcall fun c))
		(if (minusp (* fa fc))
			(mapc 'set '(b fb) (list c fc))
			(mapc 'set '(a fa) (list c fc)) ) ) )

