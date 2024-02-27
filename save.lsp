;
;	SAVE.LSP - 2006 - Bigdogs Internescional.
;
;	Salvataggio sessione.
;

(require :pprint)

(defparameter *last-save*)

(defun save (&optional (filename *last-save*))
  (when (not filename)
    (error "file name needed"))
  (let* ((filename (string filename))
	 (now (get-decoded-time))
	 (stream (fopen filename "wt")))
    (unless stream
      (error "can't open file ~s" filename))

    (format t "; saving session on ~s~%" filename)
    (format stream "; saved bdlisp session~%~%")
    (pprint `(format t "; restoring session saved on ~s ~s~%"
		     ',filename
		     ',now)
	    stream)
    (terpri stream)

    (dolist (m *modules*)
      (pprint `(require ,m) stream))
    (terpri stream)

    (dolist (sym (sort (oblist)))
      (save-symbol sym stream))
    (redirect-output stream
      (pprint `(setq *last-save*
		     (if (stringp *load-source*)
		         *load-source*
			 nil)))
      (pprint `(format t "; restore completed from ~s~%" *load-source*))
      (terpri))
    (setq *last-save* filename)
    (format t "; saved session on ~s~%" filename)
    (values)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun save-symbol (sym stream)
  (save-symbol-function sym stream)
  (save-symbol-value sym stream))

(defun save-symbol-function (sym stream)
  (cond
    ((neq t (get sym 'source)))
    ((not (consp (getd sym))))
    ((not (readable (getd sym)))
     (format t
	     "; *** not saving function ~s - contains ureadable objects~%"
	     sym))
    (t
     (format t "; saving function ~s~%" sym)
     (redirect-output stream
       (pprint `(format t "; restoring function ~s~%" ',sym))
       (pprint `(put ',sym 'redefinable t))
       (pp1 sym)
       (terpri)
       (pprint `(put ',sym 'source t))
       (terpri)))))

(defun save-symbol-value (sym stream)
  (cond
    ((get sym 'source))
    ((constantp sym))
    ((not (boundp sym)))
    ((not (readable (symbol-value sym)))
     (format t
	     "; *** not saving variable ~s - contains ureadable objects~%"
	     sym))
    (t
     (format t "; saving variable ~s~%" sym)
     (redirect-output stream
       (pprint `(format t "; restoring variable ~s~%" ',sym))
       (let ((value (symbol-value sym)))
	 (pprint (if (constantp value)
		   `(setq ,sym ,value)
		   `(setq ,sym ',value))))
       (terpri)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun readable (obj)
  (loop
    (cond
      ((stringp obj)
       (return t))
      ((atom obj)
       (return (neql "#<" (substr (string obj) 0 2))))
      ((not (readable (car obj)))
       (return nil))
      (t
       (setq obj (cdr obj))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide :save)

