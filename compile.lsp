; LISP subset compiler
;
; compiles function applications with constant
; or nested function call arguments
;
; see B.A. Pumplin, "Compiling LISP Procedures"
; ACM SIGART Newsletter 99, January, 1987
;
; Converted from walk to bdlisp - 1997 - Bigdogs Internescional.

; primary functions

(defun compexp (exp)
	((isconst exp)
		(list (mksend 1 exp)) )
	((atom exp)
		(list (mkgetvar 1 exp)) )
	(compapply (func exp) (complis (arglist exp)) (length (arglist exp))) )

(defun complis (u)
	((null u)
		nil )
	((null (rest u))
		(compexp (first u)) )
	(append (compexp (first u)) (list (mkalloc 1)) (complis (rest u))) )

(defun compapply (fn vals n)
	(append vals (mklink n) (list (mkcall fn))) )


; recognizer function

(defun isconst (x)
	(or
		(numberp x)
		(eq x t)
		(eq x nil)
		(and
			(not (atom x))
			(eq (first x) 'quote) ) ) )

; selector functions

(defun func (x)
	(first x) )

(defun arglist (x)
	(rest x) )


; constructor functions
; (code generator)

(defun mksend (dest val)
	(list 'MOVEI dest val) )

(defun mkgetvar (dest var)
	(list 'MOVE dest var) )

(defun mkalloc (dest)
	(list 'PUSH 'sp dest) )

(defun mkcall (fn)
	(list 'CALL fn) )

(defun mklink (n) 
	((eqn n 1)
		nil )
	(concat-seq (mkmove n 1) (mklink1 (- n 1))) )

(defun mklink1 (n)
	((zerop n)
		nil )
	(concat-seq (mkpop n) (mklink1 (- n 1))) )

(defun mkpop (n)
	(list 'POP 'sp n) )

(defun mkmove (dest val)
	(list 'MOVE dest val) )

; auxiliary functions

(defun concat-seq (element sequence)
	((listp sequence)
		(cons element sequence) )
	nil )

(defun eqn (x y)
	(eq x y) )

