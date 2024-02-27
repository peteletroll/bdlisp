;;;/* qtime.lsp	Displays time in real English, also chimes
;;; ** This version current from 1/7/92
;;; **
;;; ** 1 July 92 Bill Birch		Converted to Lisp from qtime.c
;;; ** 09/89	Ade Lovett		Complete rewrite
;;; ** 04/86	Mark Dapoz		Converted to C for UNIX
;;;** 12/79-12/82	Mike Cowlishaw
;;;**
;;;*/
;;
;;
;;
;; Print a list without braces.
;;
(defun no-brace-print (the-list)
	(cond
		((null the-list) nil)
		(t 
			
			(cond ((null (car the-list)) nil) 
				(t (princ (car the-list))
					(princ " ")
				) 
			)
			(no-brace-print (cdr the-list))
		)
	)
)
;; 
;; Static data
;;
(setq minutesaying `(,nil "just after" "a little after" "nearly" "almost"))
(setq fiveminsaying '(
    nil "five past" "ten past" "a quarter past" "twenty past" 
    "twenty-five past" "half past" "twenty-five to" "twenty to" 
    "a quarter to" "ten to" "five to" nil
))
(setq hoursaying '(
    "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" 
    "ten" "eleven" ""
))

;;; Functions to emulate the relational C integer behaviour.
(defun Clog (x) (cond (x 1) (t 0) ) ) ; t ==> 1 , nil ==> 0
(defun C> (a b) (Clog (> a b)))

; emulate strcat(qtime,..);
(defun append-qtime (u) (setq qtime (append qtime (list u))))

;;
;;	Function tells the time in English.
;;	It takes the time output from (get-decoded-time)
;;	as its param eg:
;;
;; (cute-time (get-decoded-time))
;;
(defun english-time () (cute-time (get-decoded-time)))
(defun cute-time (tm)
	(setq mn (+ (car (cdr tm)) 
		(C> (car tm) 29)
	))
	(setq hr (+ (caddr tm)  (C> mn 32) ))
	(setq qtime nil)
	(append-qtime "It's")
	(append-qtime (nth (rem mn 5) minutesaying))
	(append-qtime (nth (+ (/ mn 5)   (C> (rem mn 5) 2) ) fiveminsaying ))
	(cond
		((not (zerop (rem hr 12)))
			(append-qtime
				(nth (setq hr (- hr (+ 1 (* 12 (C> hr 12))))) hoursaying)
			)
			
			(append-qtime
				(cond ((not (zerop (rem mn 60))) ".") (t "o'clock."))
			)
		)
		(t (append-qtime (cond 
			((equal hr 12 ) "Noon.") 
			(t "Midnight")))
			(setq hr 12)
		)
	)
	(cond
		((zerop (rem mn 15))
			(cond
				((not (zerop (rem mn 60))) (princ "Ding-Dong!\n"))
				(t 
					(princ "[")
					(setq i hr)
					(do-while (>= i 0)
						(princ "Bong,")
						(setq i (- i 1))
					)
					(princ "]\n")
				)
			)
		)
		(t t)
	)
	(no-brace-print qtime)
	(terpri)
	t
)

(english-time)

