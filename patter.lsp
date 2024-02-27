;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	Pattern matching routines taken from the book:
;	"LISP" Second Edition, by Patrick Henry Winston &
;	Berthold Klaus Paul Horn, Addison-Wesley, 1984
;	page 259.
;
;

(defun match (p d assignments)
	(cond
		((and (null p) (null d)) 
			(cond ((null assignments) t)
				(t assignments) ) )

		((or (null p) (null d)) nil)
		((or (equal (car p) '*)
			(equal (car p) (car d)) ) 
			(match (cdr p) (cdr d) assignments))

		((equal (car p) '+)
			(or (match (cdr p) (cdr d) assignments)
				(match p (cdr d) assignments) ) )

		((equal (pattern-indicator (car p)) '>)
			(match (cdr p) (cdr d)
				(shove-gr (pattern-variable (car p))
					(car d)
					assignments ) ) )

		((equal (pattern-indicator (car p)) '<)
			(match 
				(cons 
					(pull-value (pattern-variable (car p)) assignments)
					(cdr p))
				d
				assignments))

		((equal (pattern-indicator (car p)) '+)
			(funcall #'(lambda (new-assignments)
				(or (match (cdr p) (cdr d) new-assignments)
					(match p (cdr d) new-assignments) ) )
			(shove-pl (pattern-variable (car p)) (car d) assignments) ) )

		((and (equal (pattern-indicator (car p))
			'restrict)
			(equal (restriction-indicator (car p)) '*)
			(test (restriction-predicates (car p)) (car d)))
		  (match (cdr p) (cdr d) assignments))

		(t nil) ) )
;;
(defun restriction-indicator (pattern-item) (cadr pattern-item))
(defun restriction-predicates (pattern-item) (cddr pattern-item))
(defun test (predicates argument)
	(cond ((null predicates) t)					; All tests T?
		((funcall (car predicates) argument)	; This test T?
		 (test (cdr predicates) argument))
		(t nil)))								; This test NIL?

(defun pattern-indicator (l) (cond ((consp l) (car l)) (t l)))
(defun pattern-variable (l) (cond ((consp l) (cadr l)) (t l)))
(defun shove-gr (var item a-list)
	(append a-list (list (list var item)))
)
(defun match-value (key a-list)
	(cadr (assoc key a-list))
)
(defun pull-value (vari a-list)
	(cadr (assoc vari a-list))
)
(defun shove-pl (var item a-list)
	(cond
		((null a-list) (list (list var (list item))))
		((equal var (caar a-list))
			(cons (list var (append (cadar a-list) (list item)))
				(cdr a-list)
			)
		)
		(t (cons (car a-list)
				(shove-pl var item (cdr a-list)) ) ) )	 )

;;;;;;;;;;;;;;;;;;;;;
;
;	Test cases from the book.
;
'(match '(expt (> a) (> b)) '(expt 2 3) nil)
(match '(expt (> a) (> b)) '(expt 2 3) nil)
'(match '((+ l) mother (+ r)) '(since my mother spoke) nil)
(match '((+ l) mother (+ r)) '(since my mother spoke) nil)
'(match '((> this) + (< this)) '(abc is the same as abc) nil)
(match '((> this) + (< this)) '(abc is the same as abc) nil)
'(match '((> this) + (< this)) '(abc is the same as xyz) nil)
(match '((> this) + (< this)) '(abc is the same as xyz) nil)

