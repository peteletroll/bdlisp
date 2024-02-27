;
;	XSTRING.LSP - 2000 - Bigdogs Internescional.
;
;	Tool di gestione stringhe.
;

(defun replace-string-patterns (str pat fun
				    &aux patpos (pos 0)
				    (ret (make-string-output-stream)))
  (do-loop
    (setq patpos (find-string-pattern str pat pos))
    (when (not patpos)
      (princ (substr str pos) ret)
      (return))
    (princ (substr str pos (car patpos)) ret)
    (princ (apply fun (replace-arguments str pat patpos)) ret)
    (setq pos
	  (+ (car (last patpos)) (length (car (last pat))))))
  (get-output-stream-string ret))

(defun replace-arguments (str pat patpos)
  (maplist
    #'(lambda (lpat lpos)
	(substr str
		(+ (car lpos) (length (car lpat)))
		(cadr lpos)))
    (butlast pat)
    patpos))

(defun find-string-pattern (str pat &optional (start 0))
  (if (endp pat)
    nil
    (let ((pos (findstr str (car pat) start)))
      (cond
	((not pos)
	 nil)
	((endp (cdr pat))
	 (list pos))
	(t
	  (let ((rest (find-string-pattern str
					   (cdr pat)
					   (+ pos (length (car pat))))))
	    (if rest
	      (cons pos rest)
	      nil)))))))

