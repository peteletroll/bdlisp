;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
; (in-package :User)

;;;=========================================================================
;;; Simple towers of Hanoi program. Note that Start-Peg and Goal-Peg are
;;; integers from 1 to 3 indicating the peg number. Ie to move 4 discs, 
;;; starting on the first peg and finishing on the last one, execute
;;; (Towers 4 1 3)
;;;
;;; 1992 Marty Hall. hall@aplcenmp.apl.jhu.edu

(defun towers (number-of-discs start-peg goal-peg)
	(cond
		((= 1 number-of-discs)
			; (print (list 'move 'top 'disc 'from 'peg start-peg 'to 'peg goal-peg))
			(format t "Move Top Disc from peg ~a to peg ~a.~%" start-peg goal-peg)
		)
		(t
			(towers (1- number-of-discs)
				start-peg
				(remaining-peg start-peg goal-peg))
			(towers 1 start-peg goal-peg)
			(towers (1- number-of-discs)
				(remaining-peg start-peg goal-peg)
				goal-peg))))

;;;=========================================================================
;;; Given two peg numbers, what is the peg number of the third peg?

(defun remaining-peg (peg1 peg2)
  (- 6 peg1 peg2))

;;;=========================================================================
;;; Here is some sample output using the above code.

;Yes, Marty? (towers 1 1 2)
;Move Top Disc from peg 1 to peg 2.
;NIL
;Yes, Marty? (towers 2 1 2)
;Move Top Disc from peg 1 to peg 3.
;Move Top Disc from peg 1 to peg 2.
;Move Top Disc from peg 3 to peg 2.
;NIL
;Yes, Marty? (towers 3 1 2)
;Move Top Disc from peg 1 to peg 2.
;Move Top Disc from peg 1 to peg 3.
;Move Top Disc from peg 2 to peg 3.
;Move Top Disc from peg 1 to peg 2.
;Move Top Disc from peg 3 to peg 1.
;Move Top Disc from peg 3 to peg 2.
;Move Top Disc from peg 1 to peg 2.
;NIL
;Yes, Marty? (towers 4 1 2)
;Move Top Disc from peg 1 to peg 3.
;Move Top Disc from peg 1 to peg 2.
;Move Top Disc from peg 3 to peg 2.
;Move Top Disc from peg 1 to peg 3.
;Move Top Disc from peg 2 to peg 1.
;Move Top Disc from peg 2 to peg 3.
;Move Top Disc from peg 1 to peg 3.
;Move Top Disc from peg 1 to peg 2.
;Move Top Disc from peg 3 to peg 2.
;Move Top Disc from peg 3 to peg 1.
;Move Top Disc from peg 2 to peg 1.
;Move Top Disc from peg 3 to peg 2.
;Move Top Disc from peg 1 to peg 3.
;Move Top Disc from peg 1 to peg 2.
;Move Top Disc from peg 3 to peg 2.
;NIL
;						
