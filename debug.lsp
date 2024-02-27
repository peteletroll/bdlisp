;
;	DEBUG.LSP - 1997 - Bigdogs Internescional.
;
;	Debugger.
;

(defvar *editor* (or (getenv "EDITOR") "vi"))

(defvar *call-stack*)

(defvar *trace-indent-width* 4)

(defun current-trace-indent ()
	(* *trace-indent-width* (length *call-stack*)))

(defvar *trace-output* *standard-output*)

(defvar *last-ed*)

(defun ed (&optional (name *last-ed*))
  (if (null name)
    (shell *editor*)
    (shell (concat *editor*
		   " "
		   (setq *last-ed* name)))))

(defvar *last-edfun*)

(defun edfun (&optional (name *last-edfun*) &aux (filename "edfuntmp.lsp") newdef *verbose-defun*)
  (cond
    ((not name)
     nil)
    ((atom (getd name))
     nil)
    (t
      (redirect-output (fopen filename "wt")
		       (pp (setq *last-edfun* name)))
      (let ((*last-ed* nil))
	(ed filename))
      (setq newdef (read (fopen filename "rt")))
      (shell (concat "rm " filename))
      (cond
	((atom newdef)
	 nil)
	((not (member (car newdef) '(defun defmacro)))
	 nil)
	((not (symbolp (cadr newdef)))
	 nil)
	((not (valid-lambda (cons 'lambda (cddr newdef))))
	 nil)
	(t
	  (let ((sym (cadr newdef)))
	    (eval newdef)
	    (put sym 'source t)))))))

(defmacro time (form)
  `(let* ((start-time (clock)) (return ,form) (stop-time (clock)))
     (format *trace-output*
	     "Elapsed time ~1,3f sec.~%" (- stop-time start-time))
     return))

(defun eat (&rest args)
  (princ "; return value discarded by (eat)\n")
  (values))

(defun trace (sym)
  (put sym '*trace* t)
  t)

(defun untrace (sym)
  (remprop sym '*trace*)
  t)

(defun trace-all ()
  (dolist (fun (remove-if-not 'getd (oblist)) t)
    (trace fun)))

(defun untrace-all ()
  (dolist (fun (remove-if-not 'getd (oblist)) t)
    (untrace fun)))

(defun backtrace (&optional (depth 999999))
  (backtrace-aux (cdr *call-stack*) depth)
  (values))

(defun backtrace-aux (stack depth)
  (do-loop
    (when (< depth 0)
      (return t))
    (setq depth (- depth 1))
    (when (atom stack)
      (return t))
    (trace-call (caar stack) (cdar stack) 0)
    (setq stack (cdr stack))))

(defmacro debug (&rest forms)
  `(let ((*debughook* 'debug-debug-driver))
     (evalhook '(progn ,@forms) nil 'debug-apply-hook)))

(defun debug-apply-hook (fun args &aux (ret 'ret-value))
  (let ((*call-stack* (cons (cons fun args) *call-stack*)))
    (when (get fun '*trace*)
      (trace-call fun args (current-trace-indent)))
    (cond
      ((macrop fun)
       (format *trace-output* "************* Macro ~s !~%" fun)
       (eval (macroexpand (cons fun args))))
      (t
	(setq ret
	      (multiple-value-list
		(applyhook fun args nil 'debug-apply-hook)))
	(cond
	  ((get fun '*trace*)
	   (trace-return fun ret (current-trace-indent))))
	(values-list ret)))))

(defun trace-call (sym args indent)
  (format *trace-output* "!~3@a " indent)
  (trace-indent indent)
  (prin1 sym *trace-output*)
  (princ " [" *trace-output*)
  (trace-args args)
  (princ "]\n" *trace-output*))

(defun trace-return (sym ret indent)
  (format *trace-output* "!~3@a " indent)
  (trace-indent indent)
  (prin1 sym *trace-output*)
  (princ " = " *trace-output*)
  (trace-args ret)
  (terpri *trace-output*))

(defun trace-args (args)
  (cond
    ((atom args))
    (t
      (do-loop
	(prin1 (car args) *trace-output*)
	(setq args (cdr args))
	(when (atom args)
	  (return nil))
	(princ "; " *trace-output*)))))

(defun trace-indent (indent)
  (if (< indent 50)
    (format *trace-output* "~v@a" indent "")
    (format *trace-output* "~v@a" 50 "... ")))

(defun debug-debug-driver ()
  (backtrace 4)
  (format t "(backtrace) available.~%")
  (debug-driver))

(provide :debug)

