;
;	LISPLIB.LSP - 1997 - Bigdogs Internescional.
;
;	Libreria base BDLisp.
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (setq *debughook* 'debug-driver)

(cond
  ((neq *debughook* 'error-exit)
   (format t "Debugging activated for startup sequence~%")))

(setq *library-version* "0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; temporary stuff, will be redefined soon
;;;

(set-macro-character "#"
  '(lambda (stream char &aux (char2 (read-char stream)))
     (cond
       ((eql char2 "'")
	(list 'function (read stream)))
       (t
	(error "'~a~a' not functional (... yet?)" char char2)))))

(putd 'defmacro
      '(macro (sym args &rest body)
	 (list 'putd
	       (list 'quote sym)
	       (list 'quote (list* 'macro
				   args
				   body)))))

(defmacro defun (sym args &rest body)
  (list 'putd
	(list 'quote sym)
	(list 'quote (list* 'lambda
			    args
			    body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; function definition macros
;;;

(setq *verbose-defun* nil)

(put 'define-hook 'documentation "internal - used by defun and defmacro")

(putd 'define-hook '(lambda (sym type args body &aux def doc)
  (cond
    ((not (symbolp sym))
     (error "symbol required for definition~%")))
  (cond
    ((and (stringp (car body))
	  (consp (cdr body)))
     (setq doc (car body))
     (setq body (cdr body))))
  (setq def (append (list type args) body))
  (cond
    ((not (valid-lambda def))
     (error "bad function definition for ~s~%" sym)))
  (cond
    ((and *verbose-defun*
	  (getd sym)
	  (not (get sym 'redefinable)))
     (warn "warning: redefined function ~s~%" sym)))
  (cond
    ((and *verbose-defun*
	  (member '&optional args)
	  (member '&key args))
     (warn "warning: both &optional and &key in function ~s~%" sym)))
  (putd sym def)
  (remprop sym 'redefinable)
  (remprop sym 'generic)
  (put sym 'source *load-source*)
  (cond
    (doc
     (put sym 'documentation doc))
    (t
     (remprop sym 'documentation)))
  sym))

(define-hook 'defmacro 'macro '(sym args &rest body)
  '("Macro definition macro"
    `(define-hook ',sym 'macro ',args ',body)))

(defmacro defun (sym args &rest body)
  "Function definition macro"
  `(define-hook ',sym 'lambda ',args ',body))

(setq *verbose-defun* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; *modules* handling
;;;

(setq *modules* nil)

(defun provide (module)
  "Appends a name to *modules* - use at the end of modules"
  (cond
    ((member module *modules*)
     (warn "warning: duplicated module ~s~%" module)
     *modules*)
    (t
     (setq *modules* (nconc *modules* (list module))))))

(defun require (module &aux filename)
  "Loads a module"
  (cond
    ((member module *modules*)
     t)
    ((not (keywordp module))
     (error "keyword requested"))
    (t
     (setq filename (concat (substr module 1) ".lsp"))
      (cond
	((load filename :verbose nil)
	 (if (member module *modules*)
	   t 
	   (error "can't find module ~s after loading ~s"
		  module filename module)))
	(t
	 (error "can't find module file ~s" filename))))))

(defmacro auto-require (module &rest functions)
  "Force a require at the first call of a function"
  (cons 'progn
	(mapcar #'(lambda (function)
		    `(progn
		       (defmacro ,function (&rest args)
			 ,(format nil "Automatically requires ~s" module)
			 (putd ',function nil)
			 (require ,module)
			 (cons ',function args))
		       (put ',function 'redefinable t)))
		functions)))

(auto-require :struct defstruct)
(auto-require :hash make-hash-table)
(auto-require :clos defclass defmethod)
(auto-require :debug debug trace time eat ed edfun)
(auto-require :apropos apropos find-symbol)
(auto-require :pprint pprint pp describe)
(auto-require :lint lint lint-all)
(auto-require :save save)

(provide :lisplib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun startup-banner ()
  "Shows a startup message"
  (format t ";;; bdlisp ~a.~a - ~a environment.~%"
	  *core-version*
	  *library-version*
	  *environment*)
  (when (member :rtc *features*)
    (format t ";;; warning: this is a slow debugging version~%"))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; tconc structure handling
;;;

(defun make-tconc ()
  (list nil))

(defun tconc-add (tconc obj)
  (cond
    ((null (car tconc))
     (rplaca tconc (list obj))
     (rplacd tconc (car tconc)))
    (t
     (nconc tconc (list obj))
     (rplacd tconc (cddr tconc)))))

(defun tconc-pop (tconc)
  (cond
    ((null (car tconc))
     nil)
    (t
     (prog1
      (caar tconc)
      (rplaca tconc (cdar tconc))))))

(defun tconc-list (tconc)
  (car tconc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; tree structure handling
;;;

(defun make-assoc-tree ()
  (list 'assoc-tree-head nil))

(defun tree-assoc (path tree)
  (cond
    ((endp tree)
     nil)
    ((endp path)
     (cadr tree))
    (t
     (tree-assoc (cdr path) (assoc (car path) (cddr tree))))))

(defun tree-assoc-subtrees (path tree)
  (cond
    ((endp tree)
     nil)
    ((endp path)
     (mapcar 'car (cddr tree)))
    (t
     (tree-assoc-subtrees (cdr path) (assoc (car path) (cddr tree))))))

(defun set-tree-assoc (path tree obj &aux sub new)
  (cond
    ((endp path)
     (rplaca (cdr tree) obj)
     obj)
    (t
     (setq sub (assoc (car path) (cddr tree)))
     (cond
       (sub
	(set-tree-assoc (cdr path) sub obj))
       (t
	(setq new (list (car path) nil))
	(rplacd (last tree) (list new))
	(set-tree-assoc (cdr path) new obj))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; input/output redirection
;;;

(defmacro redirect-input (stream &rest body)
  `(let ((*standard-input* ,stream))
     ,@body))

(defmacro redirect-output (stream &rest body)
  `(let ((*standard-output* ,stream))
     ,@body))

(defmacro output-string (&rest body)
  `(let ((output-accumulator (make-string-output-stream)))
     (redirect-output output-accumulator ,@body)
     (get-output-stream-string output-accumulator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Common Lisp compatibility library
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; dispatch macro characters
;;;

(setq *dispatch-macro-characters* (make-assoc-tree))

(defun set-dispatch-macro-character (char1 char2 fun &aux l)
  (set-macro-character char1 'process-dispatch-macro-character)
  (set-tree-assoc (list char1 (string-downcase char2))
		  *dispatch-macro-characters*
		  fun))

(defun get-dispatch-macro-character (char1 char2)
  (tree-assoc (list char1 (string-downcase char2))
	      *dispatch-macro-characters*))

(defun process-dispatch-macro-character (stream char &aux char2 (acc 0) fun)
  (do-loop
    (setq char2 (read-char stream))
    (cond
      ((or (not (stringp char2))
	   (not (findstr "0123456789" char2)))
       (return)))
    (setq
      acc
      (+ (* 10 acc)
	 (- (ascii char2) (ascii "0")))))
  (setq fun (get-dispatch-macro-character char char2))
  (cond
    (fun
      (funcall fun stream char2 acc))
    (t
      (error "undefined dispatch macro character ~a~a" char char2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-dispatch-macro-character "#" "'"
  #'(lambda (stream char n)
     (list 'function (read stream))))

(set-dispatch-macro-character "#" "<"
  #'(lambda (stream char n)
      (error "reading unreadable object")))

(set-dispatch-macro-character "#" "."
  #'(lambda (stream char n)
      (eval (read stream))))

(set-dispatch-macro-character "#" "!"
  #'(lambda (stream char n)
      (read-line stream)
      (values)))

(set-dispatch-macro-character "#" ":"
  #'(lambda (stream char n)
      (let ((sym (read stream)))
	(make-symbol sym))))

(set-dispatch-macro-character "#" "\\"
  #'(lambda (stream char n)
      (let* ((c (read-char stream)))
	(cond
	  ((or (<= "a" c "z")
	       (<= "A" c "Z"))
	   (unread-char c stream)
	   (let ((sym (read stream)))
	     (cond
	       ((symbolp sym)
		(cond
		  ((get (intern (string-downcase sym)) 'character))
		  ((= 1 (length (symbol-name sym)))
		    (symbol-name sym))
		  (t
		    (error "undefined character #\\~s" sym))))
	       (t
		(error "symbol or punctuation expected after #\\")))))
	  (t c)))))

(set-dispatch-macro-character "#" "|"
  #'(lambda (stream char n &aux c last (level 1))
      (do-loop
	(setq c (read-char stream))
	(cond
	  ((eq c *eof*)
	   (setq level 0))
	  ((and (eql last "#") (eql c "|"))
	   (setq level (1+ level)))
	  ((and (eql last "|") (eql c "#"))
	   (setq level (1- level))))
	(cond ((<= level 0)
	       (return (values))))
	(setq last c))))

(set-dispatch-macro-character "#" "+"
  #'(lambda (stream char n)
      (let ((feature (read stream)) (form (read stream)))
	(if (parse-feature-expr feature)
	  form
	  (values)))))

(set-dispatch-macro-character "#" "-"
  #'(lambda (stream char n)
      (let ((feature (read stream))
	    (form (read stream)))
	(if (parse-feature-expr feature)
	  (values)
	  form))))

(defun parse-feature-expr (feature)
  (if (atom feature)
    (or (member feature *features*)
	(member feature *modules*))
    (case (car feature)
      (not
	(not (parse-feature-expr (cadr feature))))
      (and
	(pop feature)
	(do-loop
	  (cond
	    ((atom feature)
	     (return t))
	    ((not (parse-feature-expr (car feature)))
	     (return)))
	  (pop feature)))
      (or
	(pop feature)
	(do-loop
	  (cond
	    ((atom feature)
	     (return))
	    ((parse-feature-expr (car feature))
	     (return t)))
	  (pop feature))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'space 'character " ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; defsetf & setf macros
;;;

(defmacro defsetf (access second &rest rest)
  (cond
    ((atom second)
     (put access 'setf-method second))
    (t
      (put access 'setf-method
	   (full-defsetf second (caar rest) (cadr rest)))))
  (list 'quote access))

(defun full-defsetf (place value form)
  (setq form
	(eval
	  `(apply
	     #'(lambda ,(cons value place)
		 ,form)
	     ',(cons value place))))
  (cond
    ((eq (car form) 'progn)
     `(lambda ,(cons value place)
	,@(cdr form)))
    (t
      `(lambda ,(cons value place)
	 ,form))))

(defmacro setf (&rest args)
  (multi-setf args))

(defun multi-setf (args &aux ret)
  (do-loop
    (cond ((endp args)
	   (return
	     (cond
	       ((eql (length ret) 1)
		(car ret))
	       (t
		 (cons 'progn (reverse ret)))))))
    (setq ret (cons (single-setf (car args) (cadr args)) ret))
    (setq args (cddr args))))

(defun single-setf (place value &aux form)
  (cond
    ((atom place)
     (if (symbolp place)
       `(setq ,place ,value)
       `(illegal-setf-atom ,place ,value)))
    ((atom (car place))
     (setq form (get (car place) 'setf-method))
     (cond
       ((null form)
	(error "undefined setf for ~a" place)
	`(undefined-setf ,place ,value))
       ((atom form)
	`(,form ,@(cdr place) ,value))
       (t
	 `(funcall #',form ,value ,@(cdr place)))))
    (t
      `(illegal-setf ,place ,value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; setf library
;;;

(defsetf get put)

(defsetf vref set-vref)

(defsetf tree-assoc set-tree-assoc)

(defsetf car (cell) (new)
  `(progn
     (rplaca ,cell ,new)
     ,new))

(defsetf cdr (cell) (new)
  `(progn
     (rplacd ,cell ,new)
     ,new))

(defsetf caar (cell) (new)
  `(progn
     (rplaca (car ,cell) ,new)
     ,new))

(defsetf cadr (cell) (new)
  `(progn
     (rplaca (cdr ,cell) ,new)
     ,new))

(defsetf cdar (cell) (new)
  `(progn
     (rplacd (car ,cell) ,new)
     ,new))

(defsetf cddr (cell) (new)
  `(progn
     (rplacd (cdr ,cell) ,new)
     ,new))

(defsetf first (cell) (new)
  `(progn
     (rplaca ,cell ,new)
     ,new))

(defsetf second (cell) (new)
  `(progn
     (rplaca (cdr ,cell) ,new)
     ,new))

(defsetf rest (cell) (new)
  `(progn
     (rplacd ,cell ,new)
     ,new))

(defsetf nth (n list) (new)
  `(progn
     (rplaca (nthcdr ,n ,list) ,new)
     ,new))

(defsetf nthcdr (n list) (new)
  `(progn
     (rplacd (nthcdr (- ,n 1) ,list) ,new)
     ,new))

; incf & decf

(defmacro incf (place &optional (value 1))
  (if (eql value 1)
    `(setf ,place (1+ ,place))
    `(setf ,place (+ ,place ,value))))

(defmacro decf (place &optional (value 1))
  (if (eql value 1)
    `(setf ,place (1- ,place))
    `(setf ,place (- ,place ,value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; miscellaneous functions
;;;

(defun caaaar (obj) (car (caaar obj)))
(defun caaadr (obj) (car (caadr obj)))
(defun caadar (obj) (car (cadar obj)))
(defun caaddr (obj) (car (caddr obj)))
(defun cadaar (obj) (car (cdaar obj)))
(defun cadadr (obj) (car (cdadr obj)))
(defun caddar (obj) (car (cddar obj)))
(defun cadddr (obj) (car (cdddr obj)))
(defun cdaaar (obj) (cdr (caaar obj)))
(defun cdaadr (obj) (cdr (caadr obj)))
(defun cdadar (obj) (cdr (cadar obj)))
(defun cdaddr (obj) (cdr (caddr obj)))
(defun cddaar (obj) (cdr (cdaar obj)))
(defun cddadr (obj) (cdr (cdadr obj)))
(defun cdddar (obj) (cdr (cddar obj)))
(defun cddddr (obj) (cdr (cdddr obj)))

(movd 'car 'first) 
(movd 'cadr 'second)
(movd 'caddr 'third)
(defun fourth (lst) (nth 3 lst))
(defun fifth (lst) (nth 4 lst))
(defun sixth (lst) (nth 5 lst))
(defun seventh (lst) (nth 6 lst))
(defun eighth (lst) (nth 7 lst))
(defun ninth (lst) (nth 8 lst))
(defun tenth (lst) (nth 9 lst))
(movd 'cdr 'rest)

(defun butlast (lst &optional (cut 1))
  (reverse (nthcdr cut (reverse lst))))

(defun string (obj)
  (cond
    ((stringp obj)
     obj)
    ((symbolp obj)
     (symbol-name obj))
    (t
     (format nil "~s" obj))))

(defun concatenate (type &rest args)
  (if (eq type 'string)
    (apply #'concat args)
    (apply #'append args)))

(movd 'getd 'symbol-function)
(movd 'getplist 'symbol-plist)

(defun vector (&rest elements)
  (let* ((l (length elements))
	 (ret (make-vector l)))
    (dotimes (i l ret)
      (setf (vref ret i) (pop elements)))))

(set-dispatch-macro-character "#" "v"
  #'(lambda (stream char n)
      (apply #'vector (read stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; dynamic binding
;;;

(defmacro defvar (name &optional value doc)
  `(unless (boundp ',name)
     (setq ,name ,value)
     (if ,doc
	 (put ',name 'documentation ,doc)
	 (remprop ',name 'documentation))
     (put ',name 'source *load-source*)
     ',name))

(defmacro defparameter (name &optional value doc)
  `(progn
     (if ,doc
       (put ',name 'documentation ,doc)
       (remprop ',name 'documentation))
     (put ',name 'source *load-source*)
     (setq ,name ,value)
     ',name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; scheme-like define
;;;

(defmacro define (form &rest body)
  (cond
    ((consp form)
      `(defun ,(car form) ,(define-funargs (cdr form)) ,@body))
    (t
      `(defparameter ,form ,@body))))

(defun define-funargs (funargs)
  (cond
    ((consp funargs)
      (cons (car funargs) (define-funargs (cdr funargs))))
    ((null funargs)
      nil)
    (t
      (list '&rest funargs))))

;;; scheme helpers

(set-dispatch-macro-character "#" "t" #'(lambda (stream char n) t))
(set-dispatch-macro-character "#" "f" #'(lambda (stream char n) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; some & every
;;;

(defun some (pred &rest args)
  (catch 'return-some
	 (apply #'mapc
		#'(lambda (&rest lst)
		    (when (apply pred lst)
		      (throw 'return-some t)))
		args)
	 nil))

(defun every (pred &rest args)
  (catch 'return-every
	 (apply #'mapc
		#'(lambda (&rest lst)
		    (unless (apply pred lst)
		      (throw 'return-every nil)))
		args)
	 t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; type handling functions
;;;

(defun typep (obj super)
  (if super
      (typep-aux (type-of obj) super)
      t))

(defun typep-aux (type class)
  (cond
    ((null type) t)
    ((eq type class) t)
    (t
      (typep-aux (get type 'supertype 'atom) class))))

(put 'atom 'supertype 't)
(put 'cons 'supertype 't)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; let & let*
;;;

(defmacro let (vardefs &rest tasks)
  `(funcall
     #'(lambda ,(mapcar 'car vardefs)
	 ,@tasks)
     ,@(mapcar 'cadr vardefs)))

(defmacro let* (vardefs &rest tasks)
  `(funcall
     #'(lambda (&aux ,@vardefs)
	 ,@tasks)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; pop & push
;;;

(defmacro pop (place)
  `(prog1
     (car ,place) 
     (setf ,place (cdr ,place))))

(defmacro push (value place)
  `(setf ,place (cons ,value ,place)))

(defmacro pushnew (value place)
  `(let ((newval ,value) (oldlst ,place))
     (if (member newval oldlst)
       oldlst
       (push newval ,place))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; flow control macros
;;;

(defmacro when (test &rest tasks)
  `(cond
     (,test
       ,@tasks)))

(defmacro unless (test &rest tasks)
  `(cond
     (,test
       nil)
     (t
       ,@tasks)))

(defmacro if (test iftrue &optional iffalse)
  `(cond
     (,test
       ,iftrue)
     (t
       ,iffalse)))

(defmacro case (val &rest clauses)
  `(funcall
     #'(lambda (case-value)
	 (cond
	   ,@(mapcar 'case-compile-clause clauses)))
     ,val))

(defun case-compile-clause (clause)
  (cond
    ((member (car clause) '(t otherwise))
     (cons t (cdr clause)))
    ((atom (car clause))
     (cons (list 'eql 'case-value (list 'quote (car clause))) (cdr clause)))
    (t
      (cons
	(list
	  'member
	  'case-value
	  (list 'quote (car clause)))
	(cdr clause)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; iteration macros
;;;

(defun compile-implicit-progn (tasks)
  (if (= 1 (length tasks))
    (car tasks)
    (cons 'progn tasks)))

(defmacro do (vardefs test &rest tasks)
  (if (null vardefs)
    `(do-loop
       ,(do-compile-test-clause test)
       ,@tasks)
    `(funcall
       #'(lambda ,(mapcar 'car vardefs)
	   (do-loop
	     ,(do-compile-test-clause test)
	     ,@tasks
	     ,(do-compile-change-vars (vars-to-change vardefs))))
       ,@(mapcar 'cadr vardefs))))

(defun do-compile-change-vars (vardefs)
  (cons 'psetq
	(mapcan 'list
		(mapcar 'car vardefs)
		(mapcar 'caddr vardefs))))

(defun vars-to-change (vardefs)
  (cond
    ((endp vardefs)
     nil)
    ((null (caddr (car vardefs)))
     (vars-to-change (cdr vardefs)))
    (t
     (cons (car vardefs) (vars-to-change (cdr vardefs))))))

(defun do-compile-test-clause (test)
  `(cond
     (,(car test)
       (return ,(compile-implicit-progn (cdr test))))))

(defmacro do* (vardefs test &rest tasks)
  (if (null vardefs)
    `(do-loop
       ,(do-compile-test-clause test)
       ,@tasks)
    `(funcall
       #'(lambda (&aux ,@(mapcar 'car vardefs))
	   (do-loop
	     ,(do-compile-test-clause test)
	     ,@tasks
	     ,(do*-compile-change-vars vardefs)))
       ,@(mapcar 'cadr vardefs))))

(defun do*-compile-change-vars (vardefs)
  (cons 'setq
	(mapcan 'list
		(mapcar 'car vardefs)
		(mapcar 'caddr vardefs))))

(defun do*-compile-initial-values (vardefs)
  (mapcar #'(lambda (a) nil) vardefs))

(defmacro dotimes (vardef &rest tasks)
  (let ((limit-var 'dotimes-limit))
    `(funcall
       #'(lambda (,(car vardef) ,limit-var)
	   (do-loop
	     (when (>= ,(car vardef) ,limit-var)
	       (return ,(caddr vardef)))
	     ,@tasks
	     (incf ,(car vardef))))
       0
       ,(cadr vardef))))

(defmacro dolist (vardef &rest tasks)
  (let ((i (car vardef))
	(l 'dolist-list)
	(final (caddr vardef)))
    `(funcall
       #'(lambda (,i ,l)
	   (do-loop
	     (cond ((endp ,l)
		    ,@(when final `((setq ,i nil)))
		    (return ,final)))
	     (setq ,i (pop ,l))
	     ,@tasks))
       nil
       ,(cadr vardef))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; loop
;;;

(defmacro loop (&rest body)
  (if (every #'consp body)
    (simple-loop-compile body)
    (extended-loop-compile body)))

(defun simple-loop-compile (body)
  `(do-loop ,@body))

(defun extended-loop-compile (body)
  (error "extended loop facility unavailable yet"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; multiple return values handling
;;;

(defmacro multiple-value-bind (vars form &rest body)
  `(apply
     #'(lambda ,vars
	 ,@body)
     (multiple-value-list ,form)))

(defmacro multiple-value-setq (vars form)
  `(funcall
     #'(lambda (vals)
	 ,@(mapcar
	     #'(lambda (var)
		 (list 'setq var '(pop vals)))
	     vars))
     (multiple-value-list ,form)))

(defmacro multiple-value-call (fun form)
  `(apply ,fun (multiple-value-list ,form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; sequence functions
;;;

(defun search (subseq seq &key (test 'eql))
  (if (stringp seq)
    (findstr seq subseq)
    (search-list seq subseq 0 test)))

(defun search-list (lst1 lst2 idx test)
  (cond
    ((search-begin lst1 lst2 test)
     idx)
    ((endp lst2)
     nil)
    (t
      (search-list lst1 (cdr lst2) (1+ idx) test))))

(defun search-begin (lst1 lst2 test)
  (cond
    ((endp lst1)
     t)
    ((funcall test (car lst1) (car lst2))
     (search-begin (cdr lst1) (cdr lst2) test))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; set handling functions
;;;

(defun union (set1 set2 &key (test #'eql) &aux (ret set2))
  (dolist (elt set1)
    (setq ret (adjoin elt ret :test test)))
  ret)

(defun intersection (set1 set2 &key (test #'eql) &aux ret)
  (dolist (elt set1)
    (when (member elt set2 :test test)
      (push elt ret)))
  ret)

(defun set-difference (in out &key (test #'eql) &aux ret)
  (dolist (elt in)
    (unless (member elt out :test test)
      (push elt ret)))
  ret)

(defun subsetp (set1 set2 &key (test #'eql) &aux (ret t))
  (dolist (elt set1)
    (unless (member elt set2 :test test)
      (setq ret nil)
      (return)))
  ret)

(defun samesetp (set1 set2 &key (test #'eql))
  (and
    (subsetp set1 set2 :test test)
    (subsetp set2 set1 :test test)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; declarations
;;;

(defun declare (&rest args))
(defun proclaim (&rest args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; end of Common Lisp compatibility library
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; regular expression definition macro
;;;

(when (member :regexp *features*)
  (defmacro re (string)
    `(quote ,(make-regexp string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *evalhook*)
(defvar *applyhook*)
(defvar *debughook*)

(defvar *args*)
(defvar *path*)
(defvar *load-source*)

(defvar *dispatch-macro-characters*)
(defvar *standard-error*)
(defvar *standard-input*)
(defvar *standard-output*)
(defvar *terminal-io*)

(defvar *features*)
(defvar *modules*)
(defvar *verbose-defun*)

(defvar *displace-macros*)
(defvar *library-version*)

(defvar break)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((path (getenv "LISP_PATH")))
  (if path
    (setq *path* (tokstr path *path-separator*))
    (setq *path* (list "/usr/local/lib/bdlisp"))))

(unless (member "." *path*)
  (setq *path* (cons "." *path*)))

