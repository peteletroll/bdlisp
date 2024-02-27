;
;	SYSTEM.LSP - 1998 - Bigdogs Internescional.
;
;	Interfaccia con comandi UNIX
;

(defun slurp-stream (stream &aux in (ret (make-tconc)))
  (do-loop
    (setq in (read-line stream))
    (when (eq in *eof*)
      (return (tconc-list ret)))
    (tconc-add ret in)))

(defun ls (&optional (arg ""))
  (slurp-stream (popen (concat "ls " arg) "r")))

(defun pwd ()
  (car (slurp-stream (popen "pwd" "r"))))


