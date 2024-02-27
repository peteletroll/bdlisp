;
;	CGI.LSP - 1999 - Bigdogs Internescional.
;
;	Libreria CGI.
;

(princ "Content-type: text/html\n\n")

(defparameter query-vars (make-tconc))
(let ((query-string
		(case (getenv "REQUEST_METHOD")
			("GET"
				(setq query-tmp (getenv "QUERY_STRING")) )
			("POST"
				(setq query-tmp (read-line)) )
			(t
				"" ) ) ) )
	(unless (stringp query-string)
		(setq query-string "") )
	(dolist (qelem (tokstr query-string "&;"))
		(apply #'(lambda (name value)
				(set (intern name) (cgi-decode value))
				(tconc-add query-vars (intern name)) )
			(tokstr qelem "=") ) ) )
(setq query-vars (tconc-list query-vars))

(defun parameter-string (parms &aux (ret (make-string-output-stream)))
	(dolist (p parms)
		(let ((var (car p)) (val (string (cadr p))))
			(if (re-match (re "^[0-9]+%?") val)
				(format ret " ~a=~a" var val)
				(format ret " ~a=~s" var val) ) ) )
	(get-output-stream-string ret) )

(defmacro html-block (name parms &rest body)
	`(unwind-protect
		(progn
			(format t "<~a~a>" ',name (parameter-string ,parms))
			,@body )
		(format t "</~a>" ',name) ) )

(defmacro html-tag (name &rest parms)
	`(format t "<~a~a>" ',name (parameter-string ,parms)) )

(defmacro def-html-block (name)
	`(defmacro ,name (parms &rest body)
		`(html-block ,name ,',parms ,',@body) ) )

(def-html-block html)
(def-html-block body)
(def-html-block blockquote)

(provide :cgi)

(html ()
	(body '((bgcolor white))
		(princ "Lisp-CGI!<hr>\n")
		(format t "GetCWD: ~s<br>~%" (getcwd))
		(format t "Path: ~s<br>~%" *path*)
		(format t "Features: ~s<br>~%" *features*)
		(blockquote ()
			(format t "Variables: ~:s<br>~%" query-vars)
			(dolist (query-var query-vars)
				(format t "~s=~s<br>\n" query-var (eval query-var)) ) )
	)

)

