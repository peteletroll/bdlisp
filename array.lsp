;
;	ARRAY.LSP - 2006 - Bigdogs Internescional.
;
;	Arrays.
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-array (dim
		&key initial-element initial-contents
		&aux (ret (make-object 'array)))
	(put ret 'dim dim)
	(put ret 'rank (length dim))
	(put ret 'v (make-array-from-vectors dim initial-element))
	ret
)

(defun make-array-from-vectors (dim initial-element)
	(cond
		((endp dim)
			initial-element )
		(t
			(let ((ret (make-vector (car dim) initial-element)))
				(dotimes (i (vector-size ret) ret)
					(setf (vref ret i)
						(make-array-from-vectors (cdr dim) initial-element) )
				)
			)
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'array 'object-print-function #'array-instance-printer)

(defun array-instance-printer (object stream)
	(format stream "#~sA~s"
		(get object 'rank)
		(get object 'dim)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide :array)

