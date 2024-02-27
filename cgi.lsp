;
;	CGI.LSP - 2002 - Bigdogs Internescional.
;
;	Libreria CGI - nuova versione.
;

(defun tag (name &rest parms &aux pname pvalue)
  (format t "<~a" name)
  (do-loop
    (when (endp parms)
      (return))
    (setq pname (pop parms))
    (setq pval (pop parms))
    (cond
      ((eq pval nil))
      ((eq pval t)
       (format t " ~a" pname))
      (t
	(format t " ~a=\"~a\"" pname (html-escape pval)))))
  (format t ">")
  name)

(defun end-tag (name)
  (format t "</~a>" name))

(defmacro block-tag (tag &rest body)
  `(unwind-protect
     (progn
       (tag ,@tag)
       ,@body)
     (end-tag ,(car tag))))

(provide :cgi)

