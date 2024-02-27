;;;;
;;;; Expert Problem Solving Using Rules and Streams.
;;;;
;;;; from: "Lisp" by Patrick Henry Winston & 
;;;;                 Berthold Klaus Paul Horn, 
;;;;        Addison-Wesley, 2nd Edition, Chapter 18.
;;;;
;;;;


(setq assertions nil)

(defun remember (new)
    (cond ((member new assertions :test 'equal) nil)
        (t (setq assertions (cons new assertions))
            new)))

(defun combine-streams (s1 s2) (append s1 s2))
(defun add-to-stream (e s) (cons e s))
(defun first-of-stream (s) (car s))
(defun rest-of-stream (s) (cdr s))
(defun empty-stream-p (s) (null s))
(defun make-empty-stream () nil)

(defun filter-assertions (pattern initial-a-list)
    (do ((assertions assertions (cdr assertions))
        (a-list-stream (make-empty-stream)))
        ((null assertions) a-list-stream)
        (let ((new-a-list (match pattern (car assertions) initial-a-list)))
            (cond (new-a-list (setq a-list-stream
                                (add-to-stream new-a-list a-list-stream)))))))

(defun filter-a-list-stream (pattern a-list-stream)
    (cond ((empty-stream-p a-list-stream) (make-empty-stream))
        (t (combine-streams
            (filter-assertions pattern (first-of-stream a-list-stream))
            (filter-a-list-stream pattern (rest-of-stream a-list-stream))))))

(defun cascade-through-patterns (patterns a-list-stream)
    (cond ((null patterns) a-list-stream)
        (t (filter-a-list-stream (car patterns)
                                (cascade-through-patterns (cdr patterns)
                                    a-list-stream)))))

(defun spread-through-actions (rule-name actions a-list)
    (do ((actions actions (cdr actions))
         (action-stream (make-empty-stream)))
        ((null actions) action-stream)
      (let ((action (replace-variables (car actions) a-list)))
        (cond ((remember action)
               (print `(rule ,rule-name says ,@action))
               (setq action-stream (add-to-stream action action-stream)))))))

(defun replace-variables (s a-list)
    (cond ((atom s) s)
          ((equal (car s) '<)
                (cadr (assoc (pattern-variable s) a-list :test 'equal)))
          (t (cons (replace-variables (car s) a-list)
                   (replace-variables (cdr s) a-list)))))

(defun feed-to-actions (rule-name actions a-list-stream)
    (cond ((empty-stream-p a-list-stream) (make-empty-stream))
          (t (combine-streams
                (spread-through-actions rule-name
                                        actions
                                        (first-of-stream a-list-stream))
                (feed-to-actions rule-name
                                 actions
                                 (rest-of-stream a-list-stream))))))

(defun use-rule (rule)
    (let* ((rule-name (cadr rule))
          (ifs (reverse (cdr (caddr rule))))
          (thens (cdr (cadddr rule)))
          (a-list-stream (cascade-through-patterns
                            ifs
                            (add-to-stream nil (make-empty-stream))))
          (action-stream (feed-to-actions rule-name thens a-list-stream)))
      (not (empty-stream-p action-stream))))

(defun forward-chain ()
    (do ((rules-to-try rules (cdr rules-to-try))
         (progress-made nil))
        ((null rules-to-try) progress-made)
      (cond ((use-rule (car rules-to-try))
             (setq rules-to-try rules)
             (setq progress-made t)))))

(load "patter.lsp")

;;;-------------------------------------------------------------
;;; Regression Test Suite
;;;
(setq *testing* t)
(cond ((boundp '*testing*) ; set *testing* to T  for regression tests.

(setq test-total 0)
(setq test-passed 0)

(defun test-case (number code result)
    (setq test-total (+ test-total 1))
    (princ "Test # ")(princ number)
    (cond ((equal code result) (princ " Passed") 
            (setq test-passed (+ test-passed 1)) )
        (t  (princ " FAILED! Expected result is: ")
            (print result)
            (terpri)
            (princ "         actual result was: ")
            (print code)
           ))
    (terpri) )


(defun test-reset ()
    (setq assertions 
        '((bozo is a cheetah)
        (bozo is a parent of sugar)
        (bozo is a parent of billy)
        (sweekums is a penguin)
        (king is a parent of rex))))

(setq t1 '((> animal) is a (> type))) 

(test-reset)
(test-case 1 
        (filter-assertions t1  nil ) 
        '(((animal sweekums) (type penguin))
     ((animal bozo) (type cheetah)) ))

(setq t2 '((< animal) is a parent of (> child))) 
(setq t3 '((animal sweekums) (type penguin))) 
(test-reset)
(test-case 2 
    (filter-assertions t2 t3)  
    nil)

(setq t4 '((animal bozo) (type cheetah))) 
(setq r5 '(((animal bozo) (type cheetah) (child billy))
    ((animal bozo) (type cheetah)(child sugar))))

(test-reset)
(test-case 3 (filter-assertions t2 t4)   r5)

; 
(test-reset)
(test-case 4 
    (filter-a-list-stream t1 (add-to-stream nil (make-empty-stream)))
    '(((animal sweekums) (type penguin)) ((animal bozo) (type cheetah))))
;

(setq t5 '(((animal sweekums) (type penguin)) 
            ((animal bozo) (type cheetah)))) 

(test-reset)
(test-case 5 (filter-a-list-stream t2 t5) r5)

(test-reset)
(test-case 6 
    (cascade-through-patterns 
        (list t2 t1)
        (add-to-stream nil (make-empty-stream)))
    r5)

(test-reset)
(test-case 7 
    (spread-through-actions 'identify16
                '(((< child) is a (< type)))
                '((animal bozo) (type cheetah) (child billy)))
    '((billy is a cheetah)))

(test-reset)
(test-case 8 
    (spread-through-actions 'identify16
                '(((< child) is a (< type)))
                '((animal bozo) (type cheetah) (child sugar)))
    '((sugar is a cheetah)))

(test-reset)
(test-case 9 
    (feed-to-actions 'identify16
                '(((< child) is a (< type)))
                '(((animal bozo) (type cheetah) (child billy))
                  ((animal bozo) (type cheetah) (child sugar)))
    )
    '((billy is a cheetah) (sugar is a cheetah)))

(test-reset)
(test-case 10 
    (use-rule '(rule identify16
                (if ((> animal) is a (> type))
                    ((< animal) is a parent of (> child)))
                (then ((< child) is a (< type)))))
    t)


;;;-----------------------------------------------------------------
(setq rules '(
    (rule identify16
        (if ((> animal) is a (> type))
            ((< animal) is a parent of (> child)))
        (then ((< child) is a (< type))))
))
(test-reset)
(test-case 11 (forward-chain) t) 
;;;-----------------------------------------------------------------
(progn
    (princ test-passed) 
    (princ " tests passed out of ")(princ test-total)(terpri))


))
;;;-----------------------------------------------------------------
;;;
;;;    Many rules ...
;;;
(setq rules '(
(rule identify1
    (if ((> animal) has hair))
    (then ((< animal) is mammal)))
(rule identify2
    (if ((> animal) gives milk))
    (then ((< animal) is mammal)))
(rule identify3
    (if ((> animal) has feathers))
    (then ((< animal) is bird)))
(rule identify4
    (if ((> animal) flies)
        ((< animal) lays eggs))
    (then ((< animal) is bird)))
(rule identify5
    (if ((> animal) eats meat))
    (then ((< animal) is carnivore)))
(rule identify6
    (if ((> animal) has pointed teeth)
        ((< animal) has claws)
        ((< animal) has forward eyes))
    (then ((< animal) is carnivore)))
(rule identify7
    (if ((> animal) is mammal)
        ((< animal) has hoofs))
    (then ((< animal) is ungulate)))
(rule identify8
    (if ((> animal) is mammal)
        ((< animal) chews cud))
    (then ((< animal) is ungulate)
          ((< animal) is even toed)))
(rule identify9
    (if ((> animal) is mammal)
        ((< animal) is carnivore)
        ((< animal) has tawny color)
        ((< animal) has dark spots))
    (then ((< animal) is cheetah)))
(rule identify10
    (if ((> animal) is mammal)
        ((< animal) is carnivore)
        ((< animal) has tawny color)
        ((< animal) has black stripes))
    (then ((< animal) is tiger)))
(rule identify11
    (if ((> animal) is ungulate)
        ((< animal) has long neck)
        ((< animal) has long legs)
        ((< animal) has dark spots))
    (then ((< animal) is giraffe)))
(rule identify12
    (if ((> animal) is ungulate)
        ((< animal) has black stripes))
    (then ((< animal) is zebra)))
(rule identify13
    (if ((> animal) is bird)
        ((< animal) does not fly)
        ((< animal) has long neck)
        ((< animal) has long legs)
        ((< animal) is black and white))
    (then ((< animal) is ostrich)))
(rule identify14
    (if ((> animal) is bird)
        ((< animal) does not fly)
        ((< animal) swims)
        ((< animal) is black and white))
    (then ((< animal) is penguin)))
(rule identify15
    (if ((> animal) is bird)
        ((< animal) flies well))
    (then ((< animal) is albatross)))
(rule identify16
    (if ((> animal) is a (> type))
        ((< animal) is a parent of (> child)))
    (then ((< child) is a (< type))))
))

(setq assertions 
    '((robbie has dark spots)
      (robbie has tawny color)
      (robbie eats meat)
      (robbie has hair)
      (suzie has feathers)
      (suzie flies well)))

(terpri)
(princ "Given the following information:\n")
(mapc 'print assertions)
(princ "we can deduce the following:\n")

(forward-chain)
