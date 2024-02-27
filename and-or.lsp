; saved bdlisp session

(format t
	"; restoring session saved on ~s ~s~%"
	'"and-or.lsp"
	'(26
	  45
	  11
	  6
	  11
	  2017
	  1
	  nil
	  -1))

(require :lisplib)
(require :math)
(require :pprint)
(require :debug)
(require :save)
(require :apropos)

(format t "; restoring variable ~s~%" '*applyhook*)
(setq *applyhook* nil)

(format t "; restoring variable ~s~%" '*args*)
(setq *args* '("bdlisp" "-M" "and-or.lsp"))

(format t "; restoring variable ~s~%" '*debughook*)
(setq *debughook* 'debug-driver)

(format t "; restoring variable ~s~%" '*displace-macros*)
(setq *displace-macros* nil)

(format t "; restoring variable ~s~%" '*evalhook*)
(setq *evalhook* nil)

(format t "; restoring variable ~s~%" '*features*)
(setq *features*
      '(:bdlisp :popen :ansi :readline :dynlink :regexp))

(format t "; restoring variable ~s~%" '*library-version*)
(setq *library-version* "0")

(format t "; restoring variable ~s~%" '*load-source*)
(setq *load-source* t)

(format t "; restoring variable ~s~%" '*modules*)
(setq *modules*
      '(:lisplib :math :pprint :debug :save :apropos))

(format t "; restoring variable ~s~%" '*path*)
(setq *path* '("." "/usr/local/lib/bdlisp"))

(format t "; restoring variable ~s~%" '*verbose-defun*)
(setq *verbose-defun* t)

(format t "; restoring function ~s~%" 'and)
(put 'and 'redefinable t)
(defmacro and (&rest forms)
  (cond ((endp forms) t)
	((endp (cdr forms))
	 (car forms))
	(t
	 `(cond (,(car forms)
		 (and
		   ,@(cdr forms)))))))
(put 'and 'source t)

(format t "; restoring variable ~s~%" 'break)
(setq break nil)

(format t "; restoring function ~s~%" 'or)
(put 'or 'redefinable t)
(defmacro or (&rest forms)
  (cons 'cond (mapcar #'list forms)))
(put 'or 'source t)

(setq *last-save*
      (if (stringp *load-source*) *load-source* nil))
(format t "; restore completed from ~s~%" *load-source*)

