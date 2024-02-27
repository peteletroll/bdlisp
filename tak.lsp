;;
;; TAK
;;

(defun tak (x y z)
  (cond 
    ((< y x)
     (tak (tak (- x 1) y z)
	  (tak (- y 1) z x)
	  (tak (- z 1) x y)))
    (t z)))

