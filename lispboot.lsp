;
;	LISPBOOT.LSP - 1999 - Bigdogs Internescional.
;
;	File di avvio interprete BDLisp.
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((progname (car *args*))
      (args (cdr *args*))
      (modlist nil)
      (filelist nil))

  (setq *displace-macros* t)

  ;;;
  ;;; command line parsing
  ;;;

  (do-loop
    (when (endp args)
      (return))
    (let ((arg (pop args)))
      (case arg
	("-m"
	 (push (intern (pop args)) modlist))
	("-M"
	 (setq *displace-macros* nil))
	("-s"
	 (setq *args* (cdr args))
	 (mapc 'require modlist)
	 (unless (load (car args) :verbose nil)
	   (error "~a: can't load ~a~%" progname (car args)))
	 (exit))
	("-e"
	 (setq *args* (cdr args))
	 (mapc 'require modlist)
	 (set-macro-character "^" (get-macro-character "'"))
	 (load (make-string-input-stream (car args)))
	 (exit))
	(otherwise
	  (push arg filelist)))))

  ;;;
  ;;; interactive mode setup
  ;;;

  (dolist (sym (oblist))
    (let ((fun (getd sym)))
      (when (builtinp fun)
        (put sym 'default-builtin-function fun))))

  (mapc 'require (reverse modlist))

  (mapc 'load (reverse filelist))

  (when (and *terminal-io* (endp filelist))
    (startup-banner))

  (setq *debughook* 'debug-driver))

