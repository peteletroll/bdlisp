;
;	PACKER.LSP - 1999 - Bigdogs Internescional.
;
;	Conversione da file .lsp in char[] .
;

(require :pprint)

(defparameter *cur-col* 0)
(defparameter *library-length* 0)

(defun file-content-string (name)
  (stream-content-string (fopen name "rt")))

(defun stream-content-string (stream &aux in (out (make-string-output-stream)))
  (do-loop
    (setq in (read stream))
    (when (eq in *eof*)
      (return (get-output-stream-string out)))
    (princ (unexpand-macro-characters in) out)
    ; (prin1 in out)
    (princ "\n" out)))

(defun pack-file (file)
  (warn "packing file ~a ...~%" file)
  (terpri)
  (format t "\t/**** file ~a ****/~%" file)
  (let ((str (file-content-string file)))
    (pack-string (string-scramble (file-content-string file)))))

(defun pack-string (str)
  (terpri)
  (setq *cur-col* 0)
  (dotimes (i (length str))
    (pack-char (elt str i)))
  (unless (zerop *cur-col*)
    (terpri)))

(defun pack-char (char)
  (when (zerop *cur-col*)
    (princ "\t"))
  (format t "~3d," (ascii char))
  (incf *cur-col*)
  (incf *library-length*)
  (cond
    ((> *cur-col* 12)
     (setq *cur-col* 0)
     (terpri))
    (t
      (princ " "))))

(when (< (length *args*) 2)
  (error "Usage: packer.lsp <name> <file> ..."))

(setq *name* (pop *args*))

(warn "generating static library ~a ...~%" *name*)

(princ "/* packer.lsp output */\n\n")

(princ "#include \"bdlisp.h\"\n\n")

(format t "static unsigned char ~a_blk[] = {~%" *name*)
(dolist (arg *args*)
  (pack-file arg))
(princ "\t/* end of static lisp source */\n")
(princ "\t255,   4,  3 } ;\n\n")

(format t "LISPpackedlib ~a = {~%" *name*)
(format t "\t~a,~%" *library-length*)
(format t "\t~a_blk~%" *name*)
(format t "};~%~%")

(warn "done: ~a characters.\n" *library-length*)

