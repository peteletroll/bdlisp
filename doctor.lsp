;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	DOCTOR
;
;	Example of pattern matching taken from the book:
;			"LISP" Second Edition, by Patrick Henry Winston & 
;			Berthold Klaus Paul Horn, Addison-Wesley, 1984
;			Problem 17-4 (page 266).
;
;	RESTRICT is not implemented.
;
;	See also: "ELIZA - A Computer Program For the Study of Natural Language
;			Communication Between Man and Machine", Joseph Weizenbaum,
;			Communications of the ACM, Vol. 9, No. 1, January 1966
;
;
;

(defun doctor ()
	(setq mother nil)
	(setq not-finished t)
	(princ "Type in a sentence enclosed in brackets \(\) ")
	(terpri)
	(do-while  not-finished
		(setq s (read))
		(cond
			((setq a-list (match '(+ worried (+ L)) s nil))
				(print (append '(how long have you been worried)
					(match-value 'L a-list))))

			((setq a-list (match '(i have (+ L)) s nil))
				(print (append '(how long have you had)
					(match-value 'L a-list))))

			((match '(+ mother +) s nil)
				(setq mother t)
				(print '(tell me more about your family)))


			((match '(+ computers +) s nil)
				(print '(do machines frighten you )))

			((or (match '(no) s nil)
				(match '(yes) s nil))
				(print '(please do not be so short with me)))

			((match '(+ (restrict * badwordp) +) s nil)
				(print '(please do not use words like that)))

			(mother (setq mother nil)	
				(print '(earlier\, you spoke of your mother)))

			(t (print '(I am sorry \, our time is up))
				(print 'goodbye)
				(setq not-finished nil) ) )	 
		(terpri)) )

(defun badwordp (word) (member word '(bollocks damm blast)))
(load "patter.lsp")
(doctor)
