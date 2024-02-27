;
;	MANAGER.LSP - 1986 - R. Wilensky.
;
;	Gestione di un database associativo.
;	da R. Wilensky - Common Lispcraft - cap. 22.
;	Adattamenti a BDLISP - 1997 - Bigdogs Internescional.
;

(load "match.lsp")

(setq *d-b* nil)

(defun learn (item)
	(setq *d-b* (cons (replace-variables item) *d-b*)) )

(defun retrieve (request)
	(nconc
		(query request)
		(mapcan '(lambda (bindings)
			(mapcar '(lambda (rbindings)
				(append rbindings bindings))
			(retrieve
				(substitute-vars
					(get-binding '?antecedent bindings)
					bindings)
					*d-b* )) )
				(query (list '< request '?antecedent) ))))

(defun substitute-vars (item bindings &aux binding)
	(cond
		((atom item) item)
		((pattern-var-p item)
			(setq binding (get-binding item bindings))
			(if binding
				(substitute-vars binding bindings)
				item ) )
		(t
			(cons (substitute-vars (car item) bindings)
				(substitute-vars (cdr item) bindings))) ) )

(defun query (request)
	(mapcan '(lambda (item &aux flag bindings)
		(mapc 'set '(flag bindings) (match item request))
		(if flag (list bindings)) ) *d-b*) )

(defun replace-variables (item)
	(car (replace-variables-with-bindings item nil)) )

(defun replace-variables-with-bindings (item bindings
	&aux var-binding newvar newlhs lhsbindings newrhs finalbindings)
	(cond
		((atom item) (list item bindings))
		((pattern-var-p item)
			(setq var-binding (get-binding item bindings))
			(if var-binding
				(list var-binding bindings)
				(add-new-var (makevar (gensym "var")) item bindings)))
		(t
			(mapc 'set '(newlhs lhsbindings)
				(replace-variables-with-bindings (car item) bindings))
			(mapc 'set '(newrhs finalbindings)
				(replace-variables-with-bindings (cdr item) lhsbindings))
			(list (cons newlhs newrhs) finalbindings)) ) )

(defun makevar (sym)
	(list '*var* sym) )

(defun add-new-var (newvar item bindings)
	(list newvar (add-binding item newvar bindings)) )

(learn '(< (mammal ?x) (dog ?x)))
(learn '(< (dog ?x) (poodle ?x)))
(learn '(poodle fido))

