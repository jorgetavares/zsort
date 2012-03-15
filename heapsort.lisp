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
;;; heapsort algorithm
;;; - adapted from the original source in CMUCL 
;;;

;;; HEAPIFY, assuming both sons of root are heaps, percolates the root element
;;; through the sons to form a heap at root.  Root and max are zero based
;;; coordinates, but the heap algorithm only works on arrays indexed from 1
;;; through N (not 0 through N-1); This is because a root at I has sons at 2*I
;;; and 2*I+1 which does not work for a root at 0.  Because of this, boundaries,
;;; roots, and termination are computed using 1..N indexes.

(defmacro heapify (seq vector-ref root max pred &optional key)
  (with-gensyms (heap-root var-root heap-max root-ele root-key heap-max/2 
			   heap-l-son one-son one-son-ele one-son-key r-son-ele r-son-key)
    `(let* ((,var-root ,root) 
	    (,heap-root (1+ ,root))
	    (,heap-max (1+ ,max))
	    (,root-ele (,vector-ref ,seq ,root))
	    ,@(if key 
		  `((,root-key (funcall ,key ,root-ele)))
		  `((,root-key ,root-ele)))
	    (,heap-max/2 (ash ,heap-max -1))) ; (floor heap-max 2)
       (declare (fixnum ,var-root ,heap-root ,heap-max ,heap-max/2))
       (loop
	 (if (> ,heap-root ,heap-max/2) (return))
	 (let* ((,heap-l-son (ash ,heap-root 1)) ; (* 2 heap-root)
		;; l-son index in seq (0..N-1) is one less than heap computation
		(,one-son (1- ,heap-l-son))
		(,one-son-ele (,vector-ref ,seq ,one-son))
		,@(if key 
		      `((,one-son-key (funcall ,key ,one-son-ele)))
		      `((,one-son-key ,one-son-ele))))
	   (declare (fixnum ,heap-l-son ,one-son))
	   (if (< ,heap-l-son ,heap-max)
	       (let* ((,r-son-ele (,vector-ref ,seq ,heap-l-son))
		      ,@(if key 
		      `((,r-son-key (funcall ,key ,r-son-ele)))
		      `((,r-son-key ,r-son-ele))))
		 (when (funcall ,pred ,one-son-key ,r-son-key)
		   (setf ,one-son ,heap-l-son)
		   (setf ,one-son-ele ,r-son-ele)
		   (setf ,one-son-key ,r-son-key))))
	   (if (funcall ,pred ,one-son-key ,root-key) (return))
	   (setf (,vector-ref ,seq ,var-root) ,one-son-ele)
	   (setf ,heap-root (1+ ,one-son)) ; one plus to be in heap coordinates.
	   (setf ,var-root ,one-son)))     ; actual index into vector for root ele.
          (setf (,vector-ref ,seq ,var-root) ,root-ele))))


;; BUILD-HEAP rearranges seq elements into a heap to start heap sorting.
(defmacro build-heap (seq type len-1 pred &optional key)
  (let ((i (gensym)))
    `(do ((,i (floor ,len-1 2) (1- ,i)))
	 ((minusp ,i) ,seq)
       (declare (fixnum ,i))
       (heapify ,seq ,type ,i ,len-1 ,pred ,@(if key `(,key))))))


;;;
;;; heapsort
;;;

(defmacro heapsort-body (type ref predicate sequence key end)
  (with-gensyms (i i-1)
    `(locally
	 (declare (type fixnum ,end)
		  (type ,type ,sequence))
       (build-heap ,sequence ,ref ,end ,predicate ,@(if key `(,key)))
       (do* ((,i ,end ,i-1)
	     (,i-1 (1- ,i) (1- ,i-1)))
	    ((zerop ,i) ,sequence)
	 (declare (type fixnum ,i ,i-1))
	 (rotatef (,ref ,sequence 0) (,ref ,sequence ,i))
	 (heapify ,sequence ,ref 0 ,i-1 ,predicate ,@(if key `(,key)))))))
  
(defun heapsort (sequence predicate &key key)
  (let ((end (1- (length sequence))))
    (if key
	(sort-dispatch heapsort-body predicate sequence key end)
	(sort-dispatch heapsort-body predicate sequence nil end))
    sequence))
  
