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
;;; quicksort optimizations 
;;;
;;; included:
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
	     (declare (fixnum i j p))
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
	     ;; check each partition is smaller and pick the smallest one
	     ;; this way the stack depth worst-case is Theta(lgn)
	     (if (< (- j start) (- end j))
		 (progn 
		   (%quicksort sequence start (1- j) predicate key)
		   (setf start (1+ j)))
		 (progn 
		   (%quicksort sequence (1+ j) end predicate key)
		   (setf end (1- j)))))))

