;
;	MATCH.LSP - 2000 - Bigdogs Internescional.
;
;	Symbolic pattern matching.
;	LISP - Winston & Horn - 2nd ed. - chapter 24.
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun match (p d &optional bindings)
	(cond
		((elements-p p d)
			(match-atoms p d bindings) )
		((variable-p p)
			(match-variable p d bindings) )
		((recursive-p p d)
			(match-pieces p d bindings) )
		(t
			'fail ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun elements-p (p d)
	(and (atom p) (atom d)) )

(defun variable-p (p)
	(and (listp p) (eq '? (first p))) )

(defun recursive-p (p d)
	(and (listp p) (listp d)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun match-atoms (p d bindings)
	(if (eql p d)
		bindings
		'fail ) )

(defun match-variable (p d bindings)
	(if (predicates-satisfied-p (extract-predicates p) d)
		(let ((binding (find-binding p bindings)))
			(if binding
				(match (extract-value binding) d bindings)
				(add-binding p d bindings) ) )
		'fail ) )

(defun match-pieces (p d bindings)
		(let ((result (match (first p) (first d) bindings)))
			(if (eq 'fail result)
				'fail
				(match (rest p) (rest d) result) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun predicates-satisfied-p (predicates argument)
	(cond
		((endp predicates)
			t )
		((funcall (first predicates) argument)
			(predicates-satisfied-p (rest predicates) argument) )
		(t
			nil ) ) )

(defun extract-predicates (p)
	(rest (rest p)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-binding (pattern-variable-expression datum bindings)
	(if (eq '_ (extract-variable pattern-variable-expression))
		bindings
		(cons
			(make-binding
				(extract-variable pattern-variable-expression)
				datum )
			bindings ) ) )

(defun find-binding (pattern-variable-expression binding)
	(unless (eq '_ (extract-variable pattern-variable-expression))
		(assoc (extract-variable pattern-variable-expression) binding) ) )

(defun extract-variable (pattern-variable-expression)
	(second pattern-variable-expression) )

(defun make-binding (variable datum)
	(list variable datum) )

(defun extract-key (binding)
	(first binding) )

(defun extract-value (binding)
	(second binding) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unify (p1 p2 &optional bindings)
	(cond
		((elements-p p1 p2)
			(unify-atoms p1 p2 bindings) )
		((variable-p p1)
			(unify-variable p1 p2 bindings) )
		((variable-p p2)
			(unify-variable p2 p1 bindings) )
		((recursive-p p1 p2)
			(unify-pieces p1 p2 bindings) )
		(t
			'fail ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unify-atoms (p1 p2 bindings)
	(if (eql p1 p2)
		bindings
		'fail ) )

(defun unify-variable (p1 p2 bindings)
	(let ((binding (find-binding p1 bindings)))
		(if binding
			(unify (extract-value binding) p2 bindings)
			(if (insidep p1 p2 bindings)
				'fail
				(add-binding p1 p2 bindings) ) ) ) )

(defun unify-pieces (p1 p2 bindings)
		(let ((result (unify (first p1) (first p2) bindings)))
			(if (eq 'fail result)
				'fail
				(unify (rest p1) (rest p2) result) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insidep (variable expression bindings)
	(if (equal variable expression)
		nil
		(inside-or-equal-p variable expression bindings) ) )

(defun inside-or-equal-p (variable expression bindings)
	(cond
		((equal variable expression)
			t )
		((atom expression)
			nil )
		((eq '? (first expression))
			(let ((binding (find-binding expression bindings)))
				(when binding
					(inside-or-equal-p
						variable
						(extract-value binding)
						bindings ) ) ) )
		(t
			(or
				(inside-or-equal-p
					variable
					(first expression)
					bindings )
				(inside-or-equal-p
					variable
					(rest expression)
					bindings ) ) ) ) )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide :match)

