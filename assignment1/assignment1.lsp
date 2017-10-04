
(defun random-elt (sequence)
  "Returns a random element from a sequence"
  (elt sequence (random (length sequence))))

(defun num-states (q-table)
  "Returns the number of states in a q-table"
  (first (array-dimensions q-table)))

(defun num-actions (q-table &optional state)
  "Returns the number of actions in a q-table"
  (second (array-dimensions q-table)))

(defun make-q-table (num-states num-actions)
  "Makes a q-table, with initial values all set to 0"
  (make-array (list num-states num-actions) :initial-element 0))

(defun max-q (q-table state)
  "Returns the highest q-value for a given state over all possible actions.
If the state is outside the range, then utility-for-outside-state-range is returned."
  (let* ((num-actions (num-actions q-table))
	 (best (aref q-table state (1- num-actions))))  ;; q of last action
    (dotimes (action (1- num-actions) best)  ;; all but last action...
      (setf best (max (aref q-table state action) best)))))

(defun max-action (q-table state &optional val)
  "Returns the action which provided the highest q-value.  If val is not provided, ties are broken at random;
else val is returned instead when there's a tie. If state is outside the range, then an error is generated
 (probably array-out-of-bounds)."
  ;; a little inefficient, but what the heck...
  (let ((num-actions (num-actions q-table))
	(best (max-q q-table state))
	bag)
    (dotimes (action num-actions)
      (when (= (aref q-table state action) best)
	(push action bag)))
    (if (and val (rest bag))
	val
      (random-elt bag))))

(defparameter *basic-alpha* 0.5 "A simple alpha constant")
(defun basic-alpha (iteration)
  (declare (ignore iteration)) ;; quiets compiler complaints
  *basic-alpha*)

(defun q-learner (q-table reward current-state action next-state gamma alpha-func iteration)
  "Modifies the q-table and returns it.  alpha-func is a function which must be called
to provide the current alpha value."

  ;;; IMPLEMENT ME
  )

(defun learn-nim (heap-size gamma alpha-func num-iterations)
  "Returns a q-table after learning how to play nim"
  (let ((q-table (make-q-table (+ heap-size 6) 3)))
    (dotimes (i num-iterations)
      (do ((state 0))
	  ((> state heap-size) q-table)
	(let ((old-state state) (my-action (max-action q-table state (random-elt '(0 1 2)))) (op-action (random-elt '(0 1 2))) (reward 0))
	  (setf state (+ state my-action 1))
	  (if (eq 1 (- heap-size state))
	      (setf reward 1)
	      (progn
		(setf state (+ state op-action 1))
		(if (eq 1 (- heap-size state))
		    (setf reward -1))))
	  (setf q-table (q-learner reward old-state my-action state gamma alpha-function i)))))))

(defun make-your-move()
  "This should be the move that the computer makes")

(defun ask-if-user-goes-first ()
  "Returns true if the user wants to go first"
  (y-or-n-p "Do you want to play first?"))

(defun make-user-move ()
  "Returns the number of sticks the user wants to remove"
  (let ((result))
    (loop
     (format t "~%Take how many sticks?  ")
     (setf result (read))
     (when (and (numberp result) (<= result 3) (>= result 1))
       (return result))
       (format t "~%Answer must be between 1 and 3"))))

(defun make-your-move(q-table heap-size)
  "Makes move for the agent and returns the 
move made"
  '1)

(defun play-nim (q-table heap-size)
  "Plays a game of nim.  Asks if the user wants to play first,
then has the user play back and forth with the game until one of
them wins.  Reports the winner."
  (let ((previous-play '()))
    (loop
       do(if (eql heap-size 1)
	     (progn
	       (if (eql 1 previous-play)
		   (format t "Congratualations you won!")
		   (format t "You lost!! think before you start again!!"))
	       (return-from play-nim "Bye!"))
	     (if (not previous-play)
		 (progn
		   (if (ask-if-user-goes-first)
		       (progn
			 (setf previous-play 1)
			 (setf heap-size (- heap-size (make-user-move))))
		       (progn
			 (setf previous-play 0)
			 (setf heap-size (- heap-size (make-your-move q-table heap-size))))))
		 (progn
		   (if (eql previous-play 1)
		       (progn
			 (setf previous-play 1)
			 (setf heap-size (- heap-size (make-user-move))))
		       (progn
			 (setf previous-play 0)
			 (setf heap-size (- heap-size (make-your-move q-table heap-size)))))))))))
      
  


(defun best-actions (q-table)
  "Returns a list of the best actions.  If there is no best action, this is indicated with a hyphen (-)"
  ;; hint: see optional value in max-action function

  ;;; IMPLEMENT ME
  )
