;;;;
;;;; Taken from "Lisp" by Winston & Horn 2nd edition
;;;; Problem 11-9
;;;;
;;;;
;;;;

(defun queen (size) (queen-aux nil 0 size))

(defun queen-aux (board n size) ; Start on next Row
  (cond ((= n size)
	 (board-print (reverse board)))
	(t
	 (queen-sub board n 0 size))))

(defun queen-sub (board n m size)
  (cond ((= m size))
	(t (cond ((conflict n m board))
		 (t (queen-aux (cons (list n m) board) (+ n 1) size)))
	   (queen-sub board n (+ m 1) size))))

(defun conflict (Cn Cm Cboard)
  (cond ((null Cboard) nil)
	((or (threat Cn Cm (caar Cboard) (cadar Cboard))
	     (conflict Cn Cm (cdr Cboard))))))

(defun threat (i j a b)
  (or (= i a)
      (= j b)
      (= (- i j) (- a b))
      (= (+ i j) (+ a b))))


(defun board-print (board) (board-print-aux board (length board)))

(defun board-print-aux (board size)
  (terpri)
  (cond ((null board))
	(t (board-print-sub (cadar board) 0 size)
	   (board-print-aux (cdr board) size))))

(defun board-print-sub (column n size)
  (cond ((= n size))
	(t (cond ((= column n) (princ "Q"))
		 (t (princ ".")))
	   (princ " ")
	   (board-print-sub column (+ n 1) size))))

