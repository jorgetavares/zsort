;;;; Copyright (c) 2012 Jorge Tavares <jorge.tavares@ieee.org>
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sell copies of the Software, and to
;;;; permit persons to whom the Software is furnished to do so, subject to
;;;; the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be included
;;;; in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :zsort)

;;;
;;; quicksort algorithm
;;;
;;; optimizations included:
;;; - hoare style partition to better deal with duplicate elements
;;; - tail call loop to avoid second recursive call
;;; - always sorts first the smallest partition 
;;;
;;; not included:
;;; - use of insertion sort for very small size partitions
;;;   reason: it didn't provided any major speedup
;;; 

(defmacro quicksort-body (type ref mpredicate mkey msequence mstart mend pick-pivot)
  (alexandria:with-gensyms (quicksort-call partition-loop predicate key sequence start end i j pivot pivot-data pivot-key)
    `(locally
	 (declare (optimize (speed 3) (space 0)))
       (labels ((,quicksort-call (,sequence ,start ,end ,predicate ,key)
		  ;; there is no need to declare ignore of key since it
		  ;; is needed on the recursive call of quicksort
		  (declare (type function ,predicate ,@(if mkey `(,key)))
			   (type fixnum ,start ,end)
			   (type ,type ,sequence))
		  ;; the while loop avoids the second recursive call 
		  ;; to quicksort made at the end of the loop body 
		  (loop while (< ,start ,end)
			do (let* ((,i ,start)
				  (,j (1+ ,end))
				  ;; picks the pivot according to the given strategy
				  (,pivot (,pick-pivot ,start ,end))
				  (,pivot-data (,ref ,sequence ,pivot))
				  ,@(if mkey 
					`((,pivot-key (funcall ,key ,pivot-data)))
					`((,pivot-key ,pivot-data))))
			     (declare (type fixnum ,i ,j ,pivot))
			     (rotatef (,ref ,sequence ,pivot) (,ref ,sequence ,start))
			     ;; two-way partitioning
			     (block ,partition-loop
			       (loop 
				 (loop 
				   (unless (> (decf ,j) ,i) (return-from ,partition-loop))
				   (when (funcall ,predicate 
						  ,(if mkey 
						       `(funcall ,key (,ref ,sequence ,j))
						       `(,ref ,sequence ,j))
						  ,pivot-key) (return)))
				 (loop
				   (unless (< (incf ,i) ,j) (return-from ,partition-loop))
				   (unless (funcall ,predicate 
						    ,(if mkey 
							 `(funcall ,key (,ref ,sequence ,i))
							 `(,ref ,sequence ,i))
						    ,pivot-key) (return)))
				 (rotatef (,ref ,sequence ,i) (,ref ,sequence ,j))))
			     (setf (,ref ,sequence ,start) (,ref ,sequence ,j)
				   (,ref ,sequence ,j) ,pivot-data)
			     ;; check each partition size and pick the smallest one
			     ;; this way the stack depth worst-case is Theta(lgn)
			     (if (< (- ,j ,start) (- ,end ,j))
				 (progn 
				   (,quicksort-call ,sequence ,start (1- ,j) ,predicate ,key)
				   (setf ,start (1+ ,j)))
				 (progn 
				   (,quicksort-call ,sequence (1+ ,j) ,end ,predicate ,key)
				   (setf ,end (1- ,j))))))))
	 (,quicksort-call ,msequence ,mstart ,mend ,mpredicate ,mkey)))))


;;;
;;; deterministic quicksort
;;;
;;; - the pivot is chosen with an adapted median-of-3 method
;;;   instead of picking 3 random numbers and use the middle
;;;   value, the pivot is picked by the median of start and
;;;   end. this way we avoid the use of the random generator

(defun quicksort (sequence predicate &key key)
  (let ((end (1- (length sequence))))
    (if key
	(sort-dispatch quicksort-body predicate key sequence 0 end median-pivot)
	(sort-dispatch quicksort-body predicate nil sequence 0 end median-pivot))
    sequence))
			
(defun median-pivot (start end)
  (declare (type fixnum start end)
	   (optimize (speed 3) (space 0)))
  (the fixnum (+ start (ash (- end start) -1))))


;;;
;;; randomized quicksort
;;;
;;; - the choice of the pivot is made with a true median-of-3
;;;   method where three pivots are randomly picked and the  
;;;   one in the middle is the chosen value. since this method uses 
;;;   the random number generator and insertion sort to determine
;;;   the pivot, it might not be the best opton for certain 
;;;   applications and/or enviroments

(defun randomized-quicksort  (sequence predicate &key key)
  (let ((end (1- (length sequence))))
    (if key
	(sort-dispatch quicksort-body predicate key sequence 0 end median-of-3-pivot)
	(sort-dispatch quicksort-body predicate nil sequence 0 end median-of-3-pivot))
    sequence))

(defun bounded-random (min max)
  (declare (type fixnum min max)
	   (optimize (speed 3) (safety 0)))
  (the fixnum (+ min (random (the fixnum (+ (- max min) 1))))))
		     
(defun median-of-3-pivot (start end)
  (declare (type fixnum start end)
	   (optimize (speed 3) (safety 0))
	   (inline bounded-random insertion-sort))
  (let ((pivots (vector (bounded-random start end)
			(bounded-random start end)
			(bounded-random start end))))
    (declare (type simple-vector pivots))
    (insertion-sort pivots #'<)
    (aref pivots 1)))
