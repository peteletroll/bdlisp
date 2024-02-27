'(see chapter (23 . 4) in Norman and Cattell)

'(generalized to allow arbitrary power of 10 as base)

'(to test try (b-power-of-2 12))

(defun princ* (&rest lst)
	(mapc 'princ lst))

(setq radix 10000)
(defun // (a b) (floor (/ a b))) ; keep everything integer


(defun b-print (n) (cond
   ((numberp n) (princ n))
   (t (b-print (cdr n))
             (b-digits (car n) radix))))

(defun b-digits (n r) (cond
   ((equal r 10) (princ n))
   (t (b-digits (// n 10) (// r 10))
      (princ (rem n 10)))))

(defun b-plus (a b) (cond
    ((numberp a) (s-plus-b a b))
    ((numberp b) (s-plus-b b a))
    (t (join-digit
         (+ (car a) (car b))
         (b-plus (cdr a) (cdr b))))))

(defun b-number (n) (cond
   ((< n radix) n)
    (t (cons (rem n radix)
             (b-number (// n radix))))))

(defun s-plus-b (a b) (cond
   ((numberp b) (b-number (+ a b)))
   (t (join-digit
         (+ a (car b))
         (cdr b)))))

(defun join-digit (n a) (cond
   ((< n radix) (cons n a))
   (t (cons
         (rem n radix)
         (s-plus-b (// n radix) a)))))


(defun b-times (a b) (cond
   ((numberp a) (s-times-b a b))
   ((numberp b) (s-times-b b a))
   (t (b-plus
         (s-times-b (car b) a)
         (cons 0 (b-times a (cdr b)))))))

(defun s-times-b (a b) (cond
   ((numberp b) (b-number (* a b)))
   (t (join-digit
         (* a (car b))
         (s-times-b a (cdr b))))))

(defun b-power-of-2 (n)
   (terpri)
   (princ*  "2 to the power " n " is: ")
   (b-print (b-expt 2 n))
   (terpri))

(defun b-expt (a n) (cond
   ((< n 1) 1)
   (t (_big-expt
          a
         (b-expt a (// n 2))
         (rem n 2)))))

(defun _big-expt (a apower nrem)
   (cond
      ((zerop nrem) (b-times apower apower))
      (t (b-times
             (b-times a apower)
             apower))))

(setq bignum '(s-plus-b join-digit b-times b-print 
	b-digits // b-plus b-number s-times-b b-power-of-2 b-expt _big-expt))

