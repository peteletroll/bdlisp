(defun randomlist (n)
  (cond
    ((< n 1)
     nil)
    (t
      (cons (random 1000) (randomlist (- n 1))))))


(defun insertion-sort (lst)
  (cond
    ((null lst)
     lst)
    (t
      (insert (car lst) (insertion-sort (cdr lst))))))

(defun insert (obj lst)
  (cond
    ((null lst)
     (list obj))
    ((< obj (car lst))
     (cons obj lst))
    (t
      (cons (car lst) (insert obj (cdr lst))))))

(defun merge-sort (lst)
  (cond
    ((null (cdr lst))
     lst)
    (t
      (apply 'merge (mapcar 'merge-sort (split lst nil nil))))))

(defun split (lst ret1 ret2)
  (cond
    ((null lst)
     (list ret1 ret2))
    (t
      (split (cdr lst) (cons (car lst) ret2) ret1))))

(defun merge (lst1 lst2)
  (cond
    ((null lst1)
     lst2)
    ((null lst2)
     lst1)
    ((< (car lst1) (car lst2))
     (cons (car lst1) (merge (cdr lst1) lst2)))
    (t
      (cons (car lst2) (merge lst1 (cdr lst2))))))

