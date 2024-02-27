;
;	INFIX.LSP - 2002 - Bigdogs Internescional.
;
;	Conversione espressioni infisse.
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-infix (expr)
	((endp expr)
		(error "empty infix expression") )
	((endp (cdr expr)
)

