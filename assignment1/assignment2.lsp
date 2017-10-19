(defun shuffle (lis)
  "Shuffles a list.  Non-destructive.  O(length lis), so
pretty efficient.  Returns the shuffled version of the list."
  (let ((vec (apply #'vector lis)) bag (len (length lis)))
    (dotimes (x len)
      (let ((i (random (- len x))))
	(rotatef (svref vec i) (svref vec (- len x 1)))
	(push (svref vec (- len x 1)) bag)))
    bag))   ;; 65 s-expressions, by the way


(defparameter *verify* t)

;;; hmmm, openmcl keeps signalling an error of a different kind
;;; when I throw an error -- a bug in openmcl?  dunno...
(defun throw-error (str)
  (error (make-condition 'simple-error :format-control str)))

(defun verify-equal (funcname &rest matrices)
  ;; we presume they're rectangular -- else we're REALLY in trouble!
  (when *verify*
    (unless (and
	     (apply #'= (mapcar #'length matrices))
	     (apply #'= (mapcar #'length (mapcar #'first matrices))))
      (throw-error (format t "In ~s, matrix dimensions not equal: ~s"
			   funcname
			   (mapcar #'(lambda (mat) (list (length mat) 'by (length (first mat))))
				   matrices))))))

(defun verify-multiplicable (matrix1 matrix2)
  ;; we presume they're rectangular -- else we're REALLY in trouble!
  (when *verify*
    (if (/= (length (first matrix1)) (length matrix2))
	(progn
	  (print matrix1)
	  (print matrix2)
	  (throw-error (format t "In multiply, matrix dimensions not valid: ~s"
			     (list (list (length matrix1) 'by (length (first matrix1)))
				   (list (length matrix2) 'by (length (first matrix2))))))))))


;; Basic Operations

(defun map-m (function &rest matrices)
  "Maps function over elements in matrices, returning a new matrix"
  (apply #'verify-equal 'map-m  matrices)
  (apply #'mapcar #'(lambda (&rest vectors)       ;; for each matrix...
		      (apply #'mapcar #'(lambda (&rest elts)     ;; for each vector...
					  (apply function elts))
			     vectors)) 
	 matrices))   ;; pretty :-)

(defun transpose (matrix)
  "Transposes a matrix"
  (apply #'mapcar #'list matrix))  ;; cool, no?

(defun make-matrix (i j func)
  "Builds a matrix with i rows and j columns,
    with each element initialized by calling (func)"
  (map-m func (make-list i :initial-element (make-list j :initial-element nil))))

(defun make-random-matrix (i j val)
  "Builds a matrix with i rows and j columns,
    with each element initialized to a random
    floating-point number between -val and val"
  (make-matrix i j #'(lambda (x)
		       (declare (ignore x))  ;; quiets warnings about x not being used
		       (- (random (* 2.0 val)) val))))

(defun e (matrix i j)
  "Returns the element at row i and column j in matrix"
  ;; 1-based, not zero-based.  This is because it's traditional
  ;; for the top-left element in a matrix to be element (1,1),
  ;; NOT (0,0).  Sorry about that.  :-)
  (elt (elt matrix (1- i)) (1- j)))

(defun print-matrix (matrix)
  "Prints a matrix in a pleasing form, then returns matrix"
  (mapcar #'(lambda (vector) (format t "~%~{~8,4,,F~}" vector)) matrix) matrix)

;;; Matrix Multiplication

(defun multiply2 (matrix1 matrix2)
  "Multiplies matrix1 by matrix2 
    -- don't use this, use multiply instead"
  (verify-multiplicable matrix1 matrix2)
  (let ((tmatrix2 (transpose matrix2)))
    (mapcar #'(lambda (vector1)
		(mapcar #'(lambda (vector2)
			    (apply #'+ (mapcar #'* vector1 vector2))) tmatrix2))
	    matrix1)))  ;; pretty :-)

(defun multiply (matrix1 matrix2 &rest matrices)
  "Multiplies matrices together"
  (reduce #'multiply2 (cons matrix1 (cons matrix2 matrices))))

;;; Element-by-element operations

(defun add (matrix1 matrix2 &rest matrices)
  "Adds matrices together, returning a new matrix"
  (apply #'verify-equal 'add matrix1 matrix2 matrices)
  (apply #'map-m #'+ matrix1 matrix2 matrices))

(defun e-multiply (matrix1 matrix2 &rest matrices)
  "Multiplies corresponding elements in matrices together, 
        returning a new matrix"
  (apply #'verify-equal 'e-multiply matrix1 matrix2 matrices)
  (apply #'map-m #'* matrix1 matrix2 matrices))

(defun subtract (matrix1 matrix2 &rest matrices)
  "Subtracts matrices from the first matrix, returning a new matrix."
  (let ((all (cons matrix1 (cons matrix2 matrices))))
    (apply #'verify-equal 'subtract all)
    (apply #'map-m #'- all)))

(defun scalar-add (scalar matrix)
  "Adds scalar to each element in matrix, returning a new matrix"
  (map-m #'(lambda (elt) (+ scalar elt)) matrix))

(defun scalar-multiply (scalar matrix)
  "Multiplies each element in matrix by scalar, returning a new matrix"
  (map-m #'(lambda (elt) (* scalar elt)) matrix))

;;; This function could
;;; be done trivially with (scalar-add scalar (scalar-multiply -1 matrix))
(defun subtract-from-scalar (scalar matrix)
  "Subtracts each element in the matrix from scalar, returning a new matrix"
  (map-m #'(lambda (elt) (- scalar elt)) matrix))



(defun sigmoid (u)
  "Sigmoid function applied to the number u"
  (handler-case (/ 1 (+ (exp (* -1 u)) 1))
    (floating-point-overflow () '1.0)
    (floating-point-underflow () '0.0)))
  

;; output and correct-output are both column-vectors

;; IMPLEMENT THIS FUNCTION

(defun net-error (output correct-output)
  "Returns (as a scalar value) the error between the output and correct vectors"
  (let ((difference (subtract correct-output output)))
    (scalar-multiply 0.5 (multiply (transpose difference) difference))))
  


;; a single datum is of the form
;; (--input-column-vector--  -- output-column-vector--)
;;
;; Notice that this is different from the raw data provided in the problems below.
;; You can convert the raw data to this column-vector form using CONVERT-DATA

;; IMPLEMENT THIS FUNCTION

(defun forward-propagate (datum v w)
  "Returns as a vector the output of the OUTPUT units when presented
the datum as input."
  ;(print datum)
  ;(print v)
  ;(print w)
  (let ((h (multiply v (first datum))))
    (let ((wh (multiply w  (mapcar #'list  (mapcar #'sigmoid (mapcar #'first h))))))
      (mapcar #'list  (mapcar #'sigmoid (mapcar #'first wh))))))
  


;; IMPLEMENT THIS FUNCTION

(defun back-propagate (datum alpha v w)
  "Back-propagates a datum through the V and W matrices,
returning a list consisting of new, modified V and W matrices."
  ;; Consider using let*
  ;; let* is like let, except that it lets you initialize local
  ;; variables in the context of earlier local variables in the
  ;; same let* statement.
  (let* ((o (forward-propagate datum v w)) (c (first (last datum)))
	 (vi (multiply v (first datum)))
	 (h (mapcar #'list  (mapcar #'sigmoid (mapcar #'first vi))))
	 (odelta (e-multiply (subtract c o) o (subtract-from-scalar 1 o)))
	 (hdelta (e-multiply h (subtract-from-scalar 1 h) (multiply (transpose w) odelta))))
	 (setf w (add w (scalar-multiply alpha (multiply odelta (transpose h)))))
	 (setf v (add v (scalar-multiply alpha (multiply hdelta (transpose (first datum))))))
	 (list v w)))
	 


(defun optionally-print (x option)
  "If option is t, then prints x, else doesn't print it.
In any case, returns x"
  ;;; perhaps this might be a useful function for you
  (if option (print x) x))


(defparameter *a-good-minimum-error* 1.0e-9)

(defun net-build (data num-hidden-units alpha initial-bounds max-iterations modulo &optional print-all-errors)
  "Builds a neural network with num-hidden-units and the appropriate number
of input and output units based on the data.  Each element should be a random
value between -(INITIAL-BOUNDS) and +(INITIAL-BOUNDS).

Then performs the following loop MAX-ITERATIONS times, or until the error condition
is met (see below):

   1. For each data element in a randomized version of the data, perform
      backpropagation.
   2. Every modulo iterations,
          For every data element in the data, perform forward propagation and
          A.  If print-all-errors is true, then print the error for each element
          B.  At any rate, always print the worst error and the mean error
          C.  If the worst error is better (lower) than A-GOOD-MINIMUM-ERROR,
              quit all loops and prepare to exit the function --
              the error condition was met.

The function should return a list of two items: the final V matrix
and the final W matrix of the learned network."
  (let ((v (make-random-matrix num-hidden-units (1+ (length (first (first data)))) initial-bounds))
	  (w (make-random-matrix (length (last (first data))) num-hidden-units initial-bounds)))
	(dotimes (i max-iterations)
	  (let ((converted-data (shuffle (convert-data data))))
	    (dolist (datum converted-data)
	      (if (and (eql 0 (mod i modulo)) (not (eql 0 i)))
		  (progn
		    (let ((e '()))
		      (dolist (j converted-data)
			(setf e (append e (net-error (forward-propagate j v w) (last (second j))))))
		      (if (eql t print-all-errors)
			  (print e)
			  (progn
			    (let ((max (reduce #'max (mapcar #'first e))) (mean (average (mapcar #'first e))))
			      (format t "Worse error: ~A,Mean error: ~A~%" max mean)
			      (if (< max *a-good-minimum-error*)
				  (return-from net-build (list v w)))))))))
	      (progn
		(let ((temp (back-propagate datum alpha v w)))
		  (setf v (first temp))
		  (setf w (second temp)))))))
	(list v w)))
			  
       


(defun simple-generalization (data num-hidden-units alpha initial-bounds max-iterations)
  "Given a set of data, trains a neural network on the first half
of the data, then tests generalization on the second half, returning
the average error among the samples in the second half.  Don't print any errors,
and use a modulo of MAX-ITERATIONS."
  (let* ((data-count (length data)) (train-data (subseq data 0 (floor (/ data-count 2)))) (test-data (subseq data (ceiling (/ data-count 2))))
	 (weights (net-build train-data num-hidden-units alpha initial-bounds max-iterations max-iterations)) (e '()))
    (dolist (i (convert-data test-data))
      (setf e (append e (net-error (forward-propagate i (first weights) (second weights)) (last (second i))))))
    (average (mapcar #'first e))))


(defun k-fold-validation (data k num-hidden-units alpha initial-bounds max-iterations)
  "Given a set of data, performs k-fold validation on this data for
the provided value of k, by training the network on (k-1)/k of the data,
then testing generalization on the remaining 1/k of the data.  This is
done k times for different 1/k chunks (and building k different networks).
The average error among all tested samples is returned.  Don't print any errors,
and use a modulo of MAX-ITERATIONS."
  (let ((e '()))
    (dotimes (j k)
      (let* ((i (1+ j)) (train-data (concatenate 'list (subseq data 0 (* (1- i) k)) (subseq data (* (1+ i) k))))
	     (test-data (subseq data (* (1- i) k) (* i k)))
	     (weights (net-build train-data num-hidden-units alpha initial-bounds max-iterations max-iterations)))
	(dolist (i (convert-data test-data))
	  (setf e (append e (net-error (forward-propagate i (first weights) (second weights)) (last (second i))))))))
    (average (mapcar #'first e))))
	


(defun scale-list (lis)
  "Scales a list so the minimum value is 0.1 and the maximum value is 0.9.  Don't use this function, it's just used by scale-data."
  (let ((min (reduce #'min lis))
	(max (reduce #'max lis)))
    (mapcar (lambda (elt) (+ 0.1 (* 0.8 (/ (- elt min) (- max min)))))
	    lis)))

(defun scale-data (lis)
  "Scales all the attributes in a list of samples of the form ((attributes) (outputs))"
  (transpose (list (transpose (mapcar #'scale-list (transpose (mapcar #'first lis))))
		   (transpose (mapcar #'scale-list (transpose (mapcar #'second lis)))))))

(defun convert-data (raw-data)
  "Converts raw data into column-vector data of the form that
can be fed into NET-LEARN.  Also adds a bias unit of 0.5 to the input."
  (mapcar #'(lambda (datum)
	      (mapcar #'(lambda (vec)
			  (mapcar #'list vec))
		      (list (cons 0.5 (first datum))
			    (second datum))))
	  raw-data))

(defun average (lis)
  "Computes the average over a list of numbers.  Returns 0 if the list length is 0."
  (if (= (length lis) 0)
      0
      (/ (reduce #'+ lis) (length lis))))

