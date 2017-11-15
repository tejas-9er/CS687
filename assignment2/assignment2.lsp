
;;;PREDEFINED FUNCTIONS ARE BEING PLACED IN HERE


;;; Useful Functions and Macros

(defmacro swap (elt1 elt2)
  "Swaps elt1 and elt2, using SETF.  Returns nil."
  (let ((temp (gensym)))
    `(let ((,temp ,elt1))
       (setf ,elt1 ,elt2)
       (setf ,elt2 ,temp)
       nil)))

(defmacro while (test return-value &body body)
  "Repeatedly executes body as long as test returns true.
Then evaluates and returns return-value"
  `(progn (loop while ,test do (progn ,@body)) ,return-value))

(defun random-elt (sequence)
  "Returns a random element from sequence"
  (elt sequence (random (length sequence))))

(defun random? (&optional (prob 0.5))
  "Tosses a coin of prob probability of coming up heads,
then returns t if it's heads, else nil."
  (< (random 1.0) prob))

(defun generate-list (num function &optional no-duplicates)
  "Generates a list of size NUM, with each element created by
  (funcall FUNCTION).  If no-duplicates is t, then no duplicates
are permitted (FUNCTION is repeatedly called until a unique
new slot is created).  EQUALP is the test used for duplicates."
  (let (bag)
    (while (< (length bag) num) bag
      (let ((candidate (funcall function)))
	(unless (and no-duplicates
		          (member candidate bag :test #'equalp))
	    (push candidate bag))))))
;;;;;; TOP-LEVEL EVOLUTIONARY COMPUTATION FUNCTIONS 


;;; TOURNAMENT SELECTION
(defparameter *tournament-size* 7)
(defun tournament-select-one (population fitnesses)
  "Does one tournament selection and returns the selected individual."
  (let ((fittest (random (length population))))
    (dotimes (i *tournament-size*)
      (let ((candidate (random (length population))))
	(if (> (elt fitnesses candidate) (elt fitnesses fittest))
	    (setf fittest candidate))))
    (elt population fittest)))



(defun tournament-selector (num population fitnesses)
  "Does NUM tournament selections, and puts them all in a list"
  (let ((next-gen '()))
    (dotimes (i num)
      (setf next-gen (append next-gen (list (tournament-select-one population fitnesses)))))
    next-gen))

(defun evolve (generations pop-size
			          &key setup creator selector modifier evaluator printer)
  "Evolves for some number of GENERATIONS, creating a population of size
POP-SIZE, using various functions"
  ;; The functions passed in are as follows:
  ;;(SETUP)                     called at the beginning of evolution, to set up
  ;;                            global variables as necessary
  ;;(CREATOR)                   creates a random individual
  ;;(SELECTOR num pop fitneses) given a population and a list of corresponding fitnesses,
  ;;                            selects and returns NUM individuals as a list.
  ;;                            An individual may appear more than once in the list.
  ;;(MODIFIER ind1 ind2)        modifies individuals ind1 and ind2 by crossing them
  ;;                            over and mutating them.  Returns the two children
  ;;                            as a list: (child1 child2).  Nondestructive to
  ;;                            ind1 and ind2.
  ;;(PRINTER pop fitnesses)     prints the best individual in the population, plus
  ;;                            its fitness, and any other interesting statistics
  ;;                            you think interesting for that generation.
  ;;(EVALUATOR individual)      evaluates an individual, and returns its fitness.
  ;;Pop will be guaranteed to be a multiple of 2 in size.
  ;;
  ;; HIGHER FITNESSES ARE BETTER

  ;; your function should call PRINTER each generation, and also print out or the
  ;; best individual discovered over the whole run at the end, plus its fitness
  ;; and any other statistics you think might be nifty.

    ;;; IMPLEMENT ME
(let ((pop '()) (fitnesses '()) (next-gen '()) (parent1 '()) (parent2 '()))
  (dotimes (i pop-size)
    (setf pop (append pop (cons (funcall creator) nil))))
  (dotimes (i generations)
    (setf fitnesses (mapcar evaluator pop))
    (setf parent1 (funcall  selector (/ pop-size 2) pop fitnesses))
    (setf parent2 (funcall selector (/ pop-size 2) pop fitnesses))
    (setf pop (mapcar modifier parent1 parent2))
    (setf pop (append (mapcar #'first pop) (mapcar #'second pop)))
    (funcall printer pop fitnesses))))


(defun simple-printer (pop fitnesses)  ;; I'm nice and am providing this for you.  :-)
  "Determines the individual in pop with the best (highest) fitness, then
prints that fitness and individual in a pleasing manner."
  (let (best-ind best-fit)
    (mapcar #'(lambda (ind fit)
		(when (or (not best-ind)
			    (< best-fit fit))
		    (setq best-ind ind)
		      (setq best-fit fit))) pop fitnesses)
    (format t "~%Best Individual of Generation...~%Fitness: ~a~%Individual:~a~%"
	        best-fit best-ind)
    fitnesses))


;;;Working on the floating point vector problem
(defun get1or-1 ()
  (if (> (random 1.0) 0.5)
      '1
      '-1))

(defparameter *float-vector-length* 100)

(defparameter *float-problem* :rastrigin)
(defparameter *float-min* -5.12)  ;; these will change based on the problem
(defparameter *float-max* 5.12)   ;; likewise

(defun float-vector-creator ()
  "Creates a floating-point-vector *float-vector-length* in size, filled with
random numbers in the range appropriate to the given problem"
  (let ((population-vector (make-array *float-vector-length*)))
    (dotimes (i *float-vector-length*)
      (setf (aref population-vector i) (* (get1or-1) (random 5.12))))
    population-vector))


(defparameter *float-crossover-probability* 0.2)
(defparameter *float-mutation-probability* 0.1)   ;; I just made up this number
(defparameter *float-mutation-variance* 0.01)     ;; I just made up this number

;;;Tweak the given vector by Gaussian convolution

(defun gaussian-mutation(vector min max p variance)
  "Tweaks the given vector by adding noice with gaussian convulution"
  (let ((tweaked (make-array (length vector))))
    (dotimes (i (length vector))
      (if (> p (random 1.0))
	  (let ((noise (random variance)))
	    (while (or (> (+ (aref vector i) noise) max) (< (+ (aref vector i) noise) min))
	      (setf noise (random variance)))
	    (setf (aref tweaked i) (+ (aref vector i) noise)))))
    tweaked))
   

(defun float-vector-modifier (ind1 ind2)
  "Copies and modifies ind1 and ind2 by crossing them over with a uniform crossover,
then mutates the children.  *crossover-probability* is the probability that any
given allele will crossover.  *mutation-probability* is the probability that any
given allele in a child will mutate.  Mutation does gaussian convolution on the allele."

    ;;; IMPLEMENT ME
    ;;; Note: crossover is probably identical to the bit-vector crossover
    ;;; See "Gaussian Convolution" (Algorithm 11) in the book for mutation
  (let ((child1 (make-array (length ind1))) (child2 (make-array (length ind2))))
    (dotimes (i (length ind1))
      (if (> (random 1.0) *float-crossover-probability*)
	  (progn
	    (setf (aref child1 i) (aref ind2 i))
	    (setf (aref child2 i) (aref ind1 i)))
	  (progn
	    (setf (aref child1 i) (aref ind1 i))
	    (setf (aref child2 i) (aref ind2 i)))))
    (list (gaussian-mutation child1 *float-min* *float-max* *float-mutation-probability* *float-mutation-variance*) (gaussian-mutation child2 *float-min* *float-max* *float-mutation-probability* *float-mutation-variance*))))

(defun rastrigin-evaluator(ind)
  (let ((fitness '0) (n (length ind)))
    (dotimes (i (length ind))
      (setf fitness (+ fitness (- (* (aref ind i) (aref ind i)) (* 10 (cos (* 2 pi (aref ind i))))))))
    (+ fitness (* 10 n))))
(defun float-vector-sum-evaluator(ind)
  (rastrigin-evaluator ind))

(defun float-vector-sum-setup(length min max problem cross-over-probability mutation-probability mutation-variance)

  (setf *float-vector-length* length)
  (setf *float-problem* problem)
  (setf *float-min* min)
  (setf *float-max* max)
  (setf *float-crossover-probability* cross-over-probability)
  (setf *float-mutation-probability* mutation-probability)
  (setf *float-mutation-variance* mutation-variance))
  
(defun make-queue ()
  "Makes a random-queue"
  (make-array '(0) :adjustable t :fill-pointer t))
(defun enqueue (elt queue)
  "Enqueues an element in the random-queue"
  (progn (vector-push-extend elt queue) queue))
(defun queue-empty-p (queue)
  "Returns t if random-queue is empty"
  (= (length queue) 0))
(defun random-dequeue (queue)
  "Picks a random element in queue and removes and returns it.
Error generated if the queue is empty."
  (let ((index (random (length queue))))
    (swap (elt queue index) (elt queue (1- (length queue))))
    (vector-pop queue)))
(defparameter *nonterminal-set* '((+ 2) (- 2) (* 2) (% 2) (sin 1) (cos 1) (exp 1)))
(defparameter *terminal-set* '(x))

(defun ptc2(size)
  "If size=1, just returns a random terminal.  Else builds and
returns a tree by repeatedly extending the tree horizon with
nonterminals until the total number of nonterminals in the tree,
plus the number of unfilled slots in the horizon, is >= size.
Then fills the remaining slots in the horizon with terminals.
Terminals like X should be added to the tree
in function form (X) rather than just X."
  (if (eql size 1)
      (return-from ptc2 (list (random-elt *terminal-set*))))
  (let ((tree (make-queue)) (count 0)(root (make-queue)) (remaining-slots 0))
    (loop while (> size (+ count (length tree)))
       do (progn
	    (let ((curr-elt (random-elt *nonterminal-set*)))
	      (if (queue-empty-p tree)
		  (progn
		    (enqueue curr-elt tree)
		    (enqueue (list (first curr-elt)) root)
		    (setf count (1+ count)))
		  (progn
		    (let ((slot (random (length tree))))
		      (loop while (eql (second (elt tree slot)) 0)
			 do (progn
			      (setf slot (random (length tree)))))
		      (enqueue curr-elt tree)
		      (enqueue (list (first curr-elt)) root)
		      (setf (elt tree slot) (list (first (elt tree slot)) (1- (second (elt tree slot)))))
		      (setf (elt root slot) (append (elt root slot) (cons (1- (length tree)) nil)))
		      (setf count (1+ count))))))))
	 (dotimes (i (length tree))
	   (setf remaining-slots (+ remaining-slots (second (elt tree i)))))
    (loop while (> remaining-slots 0)
       do (let ((slot (random (length tree))))
	  (loop while (or (eql (length (elt tree slot)) 1) (eql (second (elt tree slot)) 0))
	      do (progn
		(setf slot (random (length tree)))))
	  (let ((curr-elt (random-elt *terminal-set*)))
	    (enqueue  (list curr-elt) tree)
	    (enqueue (list curr-elt) root)
	    (setf (elt tree slot) (list (first (elt tree slot)) (1- (second (elt tree slot)))))
	    (setf (elt root slot) (append (elt root slot) (cons (1- (length tree)) nil)))))
	 (setf remaining-slots (1- remaining-slots)))
    (let ((tree-size (1- (length root))))
      (loop while (>= tree-size 0)
	  do (if (> (length (elt root tree-size)) 1)
	      (let ((temp '()) (curr-elt (elt root tree-size)))
		(dotimes (i (length (elt root tree-size)))
		  (if (= i 0)
		      (setf temp (append temp (list (first curr-elt))))
		      (setf temp (append temp (list (elt root (elt curr-elt i)))))))
		(setf (elt root tree-size) temp)))
	(setf tree-size (1- tree-size))))
    (elt root 0)))



(defparameter *size-limit* 20)

(defun gp-creator()
  (let ((tree-size (random *size-limit*)))
    (loop while (eql tree-size 0)
	while (setf tree-size (random *size-limit*)))
    (ptc2 tree-size)))
		  
(defun num-nodes (tree)
  (if (or (not (consp tree))(=  (length tree) 1))
      '0
      (let ((node-count 0))
	(dolist (i tree)
	  (setf node-count (+ node-count (num-nodes i))))
	(1+ node-count))))
(defun get-tree-size(tree size)
  (dolist (i tree)
    (if (consp i)
	(setf size (get-tree-size i size))
	(setf size (1+ size))))
  size)

(defun nth-subtree-parent-helper (tree count n)
  ;(format t "Received tree ~A ~%" tree)
  (let ((child-count -1))
    (dolist (i tree)
      (if (equal count n)
	  (return-from nth-subtree-parent-helper (list tree child-count))
	  (progn
	    (if (consp i)
		(let ((received (nth-subtree-parent-helper i count n)))
		  (if (consp received)
		      (return-from nth-subtree-parent-helper received)
		      (progn
			(setf count (+ count  (get-tree-size i 0)))
			(setf child-count (1+ child-count)))))
		(progn
		  (setf count (1+ count))
		  (setf child-count (1+ child-count)))))))
	(- n count)))

(defun nth-subtree-parent(tree n)
  ;(format t "I am being called ~%")
  (nth-subtree-parent-helper tree -1 n))

(defparameter *mutation-size-limit* 10)

(defun make-deep-copy (tree)
  (let ((copy-tree '()))
    (dolist (i tree)
      (if (consp i)
	  (setf copy-tree (append copy-tree (cons (make-deep-copy i) nil) ))
	  (setf copy-tree (append copy-tree (cons i nil)))))
    copy-tree))

(defun copy-modify-tree(tree fsub-tree msub-tree)
  (let ((child '()))
    (dolist (i tree)
     ;(format t "list element: ~A compared: ~A ~%" i (first fsub-tree))
      (if (and (consp i) (equal i (first fsub-tree)))
	  (let ((temp '()) (child-count -1))
	    (dolist (j i)
	      (if(equal child-count (second fsub-tree))
		 (setf temp (append temp (cons  msub-tree nil)))
		 (progn
		   (if (consp j)
		       (setf temp (append temp (cons (make-deep-copy j) nil)))
		       (setf temp (append temp (cons j nil))))))
	      (setf child-count (1+ child-count)))
	    (setf child (append child (cons temp nil))))
	  (if (consp i)
	      (setf child (append child (cons (copy-modify-tree i fsub-tree msub-tree) nil)))
	      (setf child (append child (cons i nil))))))
    child))
		   
(defun gp-modifier (ind1 ind2)
  ;(format t "Received ind1 : ~A ind2 :~A ~%" ind1 ind2)
  (if (random?)
      (let ((child1 '()) (child2 '()) (subtree1 '()) (subtree2 '()))
	(setf subtree1 (nth-subtree-parent ind1 (random (get-tree-size ind1 0) )))
	(setf subtree2 (nth-subtree-parent ind2 (random (get-tree-size ind2 0))))
	(if (and (consp subtree2) (consp subtree1))
	    (progn
	      (setf child1 (copy-modify-tree ind1 subtree1 (nth (+ (second subtree2) 1) (first subtree2))))
	      (setf child2 (copy-modify-tree ind2 subtree2 (nth (+ (second subtree1) 1) (first subtree1)))))
	    (progn
	      (setf child1 (make-deep-copy ind1))
	      (setf child2 (make-deep-copy ind2))))
	(list child1 child2))
      (let ((child1 '()) (child2 '()) (subtree1 '()) (subtree2 '()))
	(setf subtree1 (nth-subtree-parent ind1 (random (get-tree-size ind1 0))))
	(setf subtree2 (nth-subtree-parent ind2 (random (get-tree-size ind2 0))))
	(if (consp subtree1)
	    (setf child1 (copy-modify-tree ind1 subtree1 (ptc2 (1+ (random 5)))))
	    (setf child1 (make-deep-copy ind1)))
	(if (consp subtree2)
	    (setf child2 (copy-modify-tree ind2 subtree2 (ptc2 (1+ (random 5)))))
	    (setf child2 (make-deep-copy ind2)))
	(list child1 child2))))
	

;;; GP SYMBOLIC REGRESSION SETUP
;;; (I provide this for you)

(defparameter *num-vals* 20)
(defparameter *vals* nil) ;; gets set in gp-setup

(defun gp-symbolic-regression-setup ()
  "Defines the function sets, and sets up vals"

  (setq *nonterminal-set* '((+ 2) (- 2) (* 2) (% 2) (sin 1) (cos 1) (exp 1)))
  (setq *terminal-set* '(x))

  (setq *vals* nil)
  (dotimes (v *num-vals*)
    (push (1- (random 2.0)) *vals*)))

(defun poly-to-learn (x) (+ (* x x x x) (* x x x) (* x x) x))

;; define the function set
(defparameter *x* nil) ;; to be set in gp-evaluator
(defun x () *x*)
(defun % (x y) (if (= y 0) 0 (/ x y)))  ;; "protected division"
;;; the rest of the functions are standard Lisp functions


(defun gp-symbolic-regression-evaluator (ind)
  (let ((z 0))
    (dolist (i *vals*)
      (setf *x* i)
      (setf z (+ z (abs (- (poly-to-learn *x*) (handler-case (eval ind) (error (condition) (format t "~%Warning, ~a" condition) most-positive-fixnum)))))))
    (/ 1 (1+ z))))