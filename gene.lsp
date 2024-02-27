;;;;
;;;; gene.lsp	Genetic Programming Example
;;;;
;;;;	Bill Birch  28 Oct 93
;;;;
;;; This example tries to breed a sequence of six numbers with the highest
;;; average. The initial set is generated randomly. Numbers are between
;;; 0 and 100, the program prints the average fitness each generation.
;;;
;;; Recombination does not allow numbers to change position in the 
;;; "gene" sequence.
;;;
;;; The fitness function uses a bit of simulated annealing, it
;;; increments the reproduction critera on each generation.
;;;
;;;
(setq pop-size 50)	; the number of individuals in every generation.

;;
;; Top -level demonstration function.
;; Prints the current sum of the population's fitness measures.
;;
;; (run) to invoke.
;; 
;;
(defun run (&optional (generations 100))
	(setq start (setq x (pop-new pop-size 6)))
	(setq anneal 50)				; start at a reasonable figure. 
	(do-while (> generations 0)
		(setq xd (genetic x))
		(if (equal xd x) (setq generations 0)) ; quit if stagnant
		(setq x xd)
		(setq anneal (+ anneal 1))
		(setq generations (- generations 1)))
	(print (car x)) )

;;
;; Essential genetic cycle.
;;
(defun genetic (population)

  ; calculate fitness of the current population
  ;
  (setq report (fitness population))
  (prin1 (/ (apply '+ report) (length report)))(terpri)

  ; Selection:
  ;  only allow those individuals with a certain fitness to reproduce.
  (setq studs (select population report))

  (if studs 
;  (prin1 studs)(terpri)

  ; Reproduction:
  ;   Allow the fittest individuals to reproduce.
  ;   Their children become the new generation.
  (setq population (reproduce pop-size studs))

;  (prin1 population)(terpri)
;  (princ "? ")
;  (read-char)
	)
  population
  )
;;
;;
;; Create a new (random) individual.
;;
(defun new-indiv (gene-length &aux result)
	(do-while (> gene-length 0)
			  (setq result (cons (floor (random 100)) result))
			  (setq gene-length (- gene-length 1)))
  result)
;;
;;	Create a randomly chosen new population
;;
(defun pop-new (number m &aux result) 
	(do-while (> number 0)
			  (setq result (cons (new-indiv m) result))
			  (setq number (- number 1)))
  result)

;;
;;	Calculate a vector of fitness values for a population.
;;
(defun fitness (pop)
  (cond
	((null pop) nil)
	(t (cons (fit (car pop))
			 (fitness (cdr pop))))))

;;
;;	Work out a measure of how deserving this individual is to reproduce.
;;
(defun fit (individu)
  ;
  ;	Calculate the average of the gene numbers.
  ;
  (/ (apply '+ individu) (length individu)))

;;
;;	Produce a subset of the population which pass the fitness
;;	criterea for re-production.
;;
(defun select (pop fitn)
	(cond 
		((null pop)
			nil )
		((goodun (car fitn))
			(cons (car pop) (select (cdr pop) (cdr fitn))) )
		(t
			(select (cdr pop) (cdr fitn))) ) )

;;
;;	Decide if this individual has enough fitness
;;
(defun goodun (f)
	(> f anneal))

;;
;;	Generate n children from a set of parents.
;;
(defun reproduce (n parents &aux result)
	(do-while (> n 0)
		   (setq result (cons (propogate parents) result))
		   (setq n (- n 1)))
  result)

;;
;;	Randomly choose two parents from the set and breed one child.
;;
(defun propogate (parents)
	(recombine 
		(nth (floor (random (length parents))) parents)
		(nth (floor (random (length parents))) parents)))

;;
;;	Creat a child with genes taken randomly from Mum and Dad.
;;
(defun recombine (mum dad)
	(cond ((null mum) nil)
		 (t  (cons 
			  (cond ((= (floor (random 2)) 1) (car mum)) ; coin toss for gene
					(t (car dad)))
			  (recombine (cdr mum) (cdr dad))))))

