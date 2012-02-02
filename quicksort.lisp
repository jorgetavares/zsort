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

(in-package :usort)

;;;
;;; quicksort
;;;
;;; optimizations included:
;;; - hoare style partition to better deal with duplicate elements
;;; - pivot picked with deterministic median-of-3 
;;; - tail call loop to avoid second recursive call
;;; - always sorts first the smallest partition 
;;;
;;; not included:
;;; - use of insertion sort for very small size partitions
;;;   reason: it didn't provided any major speedup
;;; 

(defun quicksort  (sequence predicate &key key)
  (%quicksort sequence 0 (1- (length sequence)) predicate (or key #'identity))
  sequence)

(defun %quicksort (sequence start end predicate key)
  (declare (type fixnum start end)
	   (type function predicate key)
	   (type simple-vector sequence)
	   (optimize (speed 3) (safety 0)))
  ;; the while loop avoids the second recursive call 
  ;; to quicksort made at the end of the loop body 
  (loop while (< start end)
	do (let* ((i start)
		  (j (1+ end))
		  ;; the pivot is chosen with an adapted median-of-3 method
		  ;; instead of picking 3 random numbers and use the middle
		  ;; value, the pivot is picked by the median of start and
		  ;; end. this way we avoid the use of the random generator
		  (p (+ start (ash (- end start) -1)))
		  (x (aref sequence p)))
	     (declare (type fixnum i j p))
	     (rotatef (aref sequence p) (aref sequence start))
	     (block partition-loop
	       (loop 
		 (loop
		   (unless (> (decf j) i) (return-from partition-loop))
		   (when (funcall predicate 
				  (funcall key (aref sequence j)) 
				  (funcall key x)) (return)))
		 (loop
		   (unless (< (incf i) j) (return-from partition-loop))
		   (unless (funcall predicate 
				    (funcall key (aref sequence i)) 
				    (funcall key x)) (return)))
		 (rotatef (aref sequence i) (aref sequence j))))
	     (setf (aref sequence start) (aref sequence j)
		   (aref sequence j) x)
	     ;; check each partition size and pick the smallest one
	     ;; this way the stack depth worst-case is Theta(lgn)
	     (if (< (- j start) (- end j))
		 (progn 
		   (%quicksort sequence start (1- j) predicate key)
		   (setf start (1+ j)))
		 (progn 
		   (%quicksort sequence (1+ j) end predicate key)
		   (setf end (1- j)))))))


;;;
;;; randomized quicksort
;;;
;;; - the only difference to the previous quicksort is in the 
;;;   choice of the pivot. this sort uses a true median-of-3
;;;   method where three pivots are randomly picked and uses 
;;;   the one with the middle value. since this method uses 
;;;   the random number generator and insertion sort to determine
;;;   the pivot, it might not be the best opton for certain 
;;;   applications and/or enviroments
;;;

(defun randomized-quicksort  (sequence predicate &key key)
  (%randomized-quicksort sequence 0 (1- (length sequence)) predicate (or key #'identity))
  sequence)

(defun %bounded-random (min max)
  (declare (type fixnum min max)
	   (optimize (speed 3) (safety 0)))
  (the fixnum (+ min (random (the fixnum (+ (- max min) 1))))))
		     
(defun %median-of-3-pivot (start end)
  (declare (type fixnum start end)
	   (optimize (speed 3) (safety 0))
	   (inline %bounded-random))
  (let ((pivots (vector (%bounded-random start end)
			(%bounded-random start end)
			(%bounded-random start end))))
    (declare (type simple-vector pivots))
    (%insertion-sort pivots 0 2 #'< #'identity)
    (aref pivots 1)))

(defun %randomized-quicksort (sequence start end predicate key)
  (declare (type fixnum start end)
	   (type function predicate key)
	   (type simple-vector sequence)
	   (inline %median-of-3-pivot)
	   (optimize (speed 3) (safety 0)))
  ;; the while loop avoids the second recursive call 
  ;; to quicksort made at the end of the loop body 
  (loop while (< start end)
	do (let* ((i start)
		  (j (1+ end))
		  ;; the pivot is chosen with median-of-3
		  ;; it must be noted that CL:RANDOM is used
		  ;; as well as insertion sort to determine
		  ;; the middle value
		  (p (%median-of-3-pivot start end))
		  (x (aref sequence p)))
	     (declare (type fixnum i j p))
	     (rotatef (aref sequence p) (aref sequence start))
	     (block partition-loop
	       (loop 
		 (loop
		   (unless (> (decf j) i) (return-from partition-loop))
		   (when (funcall predicate 
				  (funcall key (aref sequence j)) 
				  (funcall key x)) (return)))
		 (loop
		   (unless (< (incf i) j) (return-from partition-loop))
		   (unless (funcall predicate 
				    (funcall key (aref sequence i)) 
				    (funcall key x)) (return)))
		 (rotatef (aref sequence i) (aref sequence j))))
	     (setf (aref sequence start) (aref sequence j)
		   (aref sequence j) x)
	     ;; check each partition size and pick the smallest one
	     ;; this way the stack depth worst-case is Theta(lgn)
	     (if (< (- j start) (- end j))
		 (progn 
		   (%randomized-quicksort sequence start (1- j) predicate key)
		   (setf start (1+ j)))
		 (progn 
		   (%randomized-quicksort sequence (1+ j) end predicate key)
		   (setf end (1- j)))))))
