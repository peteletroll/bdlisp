;;;
;;; An infix to prefix converter for algebraic expressions.
;;; From Winston and Horn, Second Edition, pp 185-189.
;;;

; converted to bdlisp - 1997 - Bigdogs Internescional.

(defun weight (operator)
	(case operator
		(= 0)
		(+ 1)
		(- 1)
		(* 2)
		(/ 2)
		(mod 2)
		(** 3)
		(^ 3)
		(t 4) ) )

(defun opcode (operator)
	(case operator
		(= 'setq)
		(+ '+)
		(- '-)
		(* '*)
		(/ '/)
		(mod 'rem)
		(** 'expt)
		(^ 'expt)
		(t (print (list operator 'is 'an 'invalid 'operator))) ) )
	
(defun inf-aux (ae operators operands)
	(inf-iter (cdr ae)
		operators
		(cons (inf-to-pre (car ae)) operands)) )

(defun inf-iter (ae operators operands)
	(cond
		((and (null ae) (null operators))
			(car operands) )
		((and
				(not (null ae))
				(or
					(null operators)
					(> (weight (car ae)) (weight (car operators))) ) )
			(inf-aux (cdr ae)
			(cons (car ae) operators)
			operands) )
		(t (inf-iter ae
			(cdr operators)
			(cons (list (opcode (car operators)) (cadr operands) (car operands))
				(cddr operands))) ) ) )

(defun inf-to-pre (ae)
	(if (atom ae)
		ae
		(inf-aux ae nil nil) ) )

