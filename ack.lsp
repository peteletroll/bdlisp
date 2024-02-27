(defun ack (m n)
	(cond
		((<= m 0)
			(+ n 1) )
		((<= n 0)
			(ack (- m 1) 1) )
		(t
			(ack (- m 1) (ack m (- n 1))) ) ) )
