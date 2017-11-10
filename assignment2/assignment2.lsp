
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


(defparameter *boolean-vector-length* 100)
(defparameter *boolean-problem* :max-ones)
;; perhaps other problems might include... 

(defun boolean-vector-creator ()
  "Creates a boolean-vector *boolean-vector-length* in size, filled with
random Ts and NILs, or with random 1s and 0s, your option."
    ;;; IMPLEMENT ME
  (let((boolean-vector (make-array *boolean-vector-length*)))
    (dotimes (i *boolean-vector-length*)
      (setf (aref boolean-vector i) (random 2)))
    boolean-vector))


(defparameter *boolean-crossover-probability* 0.2)
(defparameter *boolean-mutation-probability* 0.01)
(defun boolean-vector-modifier (ind1 ind2)
  "Copies and modifies ind1 and ind2 by crossing them over with a uniform crossover,
then mutates the children.  *crossover-probability* is the probability that any
given allele will crossover.  *mutation-probability* is the probability that any
given allele in a child will mutate.  Mutation simply flips the bit of the allele."

    ;;; IMPLEMENT ME
  (let ((child1 (make-array (length ind1))) (child2 (make-array (length ind2))))
    (dotimes (i (length ind1))
      (if (< (random 1.0) *boolean-crossover-probability*)
	  (progn
	    (setf (aref child1 i) (aref ind2 i))
	    (setf (aref child2 i) (aref ind1 i))))
      (if (< (random 1.0) *boolean-mutation-probability*)
	    (setf (aref child1 i) (if(= (aref child1 i) 0) 1 0));;flip bit for child 2
	    (setf (aref child2 i) (if(= (aref child2 i) 0) 1 0))));;flip bit for child 2
    (list child1 child2)))

(defun boolean-vector-evaluator (ind1)
  "Evaluates an individual, which must be a boolean-vector, and returns
its fitness."
(let ((sum 0))	;;implementation for max ones
    ;;; IMPLEMENT ME
    (dotimes (i (length ind1))
    (setf sum (+ sum (aref ind1 i))))
    sum))



(defun boolean-vector-sum-setup (length min max problem boolean-cross-over-probability boolean-mutation-probability mutation-variance)
  "Does nothing.  Perhaps you might use this to set up various
(ahem) global variables to define the problem being evaluated, I dunno."
(setf *boolean-vector-length* length)
  (setf *boolean-problem* problem)
  (setf *boolean-min* min)
  (setf *boolean-max* max)
  (setf *boolean-crossover-probability* cross-over-probability)
  (setf *boolean-mutation-probability* mutation-probability)
  (setf *boolean-mutation-variance* mutation-variance))

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
  
