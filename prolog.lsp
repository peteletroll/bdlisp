;; The following is a tiny Prolog interpreter in MacLisp
;; written by Ken Kahn and modified for XLISP by David Betz.
;; It was inspired by other tiny Lisp-based Prologs of
;; Par Emanuelson and Martin Nilsson.
;; There are no side-effects anywhere in the implementation.
;; Though it is VERY slow of course.

;; Modified to run on RefLisp by Bill Birch 29 Oct 92
;;

;; Modified to run on bdlisp by Bigdogs Internescional

(defun prolog (database &optional goal)
       (do () ((not (progn (princ "Query? ") (setq goal (read)))))
              (prove (list (rename-variables goal '(0)))
                     '((bottom-of-environment))
                     database
                     1)))

;; prove - proves the conjunction of the list-of-goals
;;         in the current environment
(defun prove (list-of-goals environment database level)
	(cond ((null list-of-goals) ;; succeeded since there are no goals
             (print-bindings environment environment)
             (not (y-or-n-p "More? ")))
            (t (try-each database database
                         (cdr list-of-goals) (car list-of-goals)
                         environment level))))

(defun try-each (database-left database goals-left goal environment level
                  &optional assertion new-enviroment)
       (cond ((null database-left) nil) ;; fail since nothing left in database
             (t (setq assertion
                      (rename-variables (car database-left)
                                        (list level)))
                (setq new-environment
                      (unify goal (car assertion) environment))
                (cond ((null new-environment) ;; failed to unify
                       (try-each (cdr database-left) database
                                 goals-left goal
                                 environment level))
                      ((prove (append (cdr assertion) goals-left)
                              new-environment
                              database
                              (+ 1 level)))
                      (t (try-each (cdr database-left) database
                                   goals-left goal
                                   environment level))))))

(defun unify (x y environment &optional new-environment)
       (setq x (value x environment))
       (setq y (value y environment))
       (cond ((variable-p x) (cons (list x y) environment))
             ((variable-p y) (cons (list y x) environment))
             ((or (atom x) (atom y))
                  (cond ((equal x y) environment)
    	
                (t nil)))
             (t (setq new-environment (unify (car x) (car y) environment))
                (cond (new-environment (unify (cdr x) (cdr y) new-environment))
    		      (t nil)))))

(defun value (x environment &optional binding)
       (cond ((variable-p x)
	(setq binding (assoc x environment :test 'equal))
	(cond ((null binding) x)
	(t (value (cadr binding) environment))))
		(t x)))

(defun variable-p (x)
	(and x (consp x) (eq (car x) '*)))

(defun rename-variables (term list-of-level)
	(cond ((variable-p term) (append term list-of-level))
		((atom term) term)
		(t (cons (rename-variables (car term) list-of-level)
				 (rename-variables (cdr term) list-of-level)))))

(defun print-bindings (environment-left environment)
	(cond ((cdr environment-left)
		(cond ((equal 0 (nth 2 (caar environment-left)))
 				(prin1 (cadr (caar environment-left)))
 				(princ " = ")
				 (prin1 (value (caar environment-left) environment))
				 (terpri)))
	(print-bindings (cdr environment-left) environment))))
;; a sample database:

(setq db '(((father madelyn ernest)) 
	((mother madelyn virginia))
	 ((father david arnold))
	 ((mother david pauline))
	 ((father rachel david))
	 ((mother rachel madelyn))
	((grandparent (* grandparent) (* grandchild))
		(parent (* grandparent) (* parent))
		(parent (* parent) (* grandchild)))
	((parent (* parent) (* child))
		(mother (* parent) (* child)))
	((parent (* parent) (* child))	
		(father (* parent) (* child)))))
;; the following are utilities
(defun y-or-n-p (prompt)
 (princ prompt)
 (eq (read) 'y))

;; start things going
(princ ";\n")
(princ "; Welcome to MicroProlog by Ken Kahn\n")
(princ "; Try typing (grandparent (* A) (* B))\n")
(princ "; or   (mother (* C) (* D))\n")
(princ "; Type () to exit.\n")
(prolog db)
