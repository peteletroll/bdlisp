; "Norman & Cattell Page 120"

(defun printc (&rest x)
	(princ x)
	(terpri) )

(defun gc (x)
  (cond
	((numberp x)
		(printc 'LDA '@ x))
	((atom x)
		(printc 'LDA x))
	(t
	(gc (cadr x))
	(printc 'PUSHA)
	(gc (caddr x))
	(printc 'STOREA 'TEMP)
	(printc 'POPA)
	(printc (get (car x) 'opcode (list 'OP (car x))) 'TEMP))))

(put '+ 'opcode 'ADC)
(put '- 'opcode 'SBC)
(put '& 'opcode 'AND)
(put '\| 'opcode 'ORA)

(setq testexpression '(+ (& A B) (- 2 B)))
(printc "Compilation of:")
(princ testexpression)
(gc testexpression)
