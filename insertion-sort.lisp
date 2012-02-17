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
;;; insertion sort algorithm
;;;

(defmacro insertion-sort-body (type ref predicate key sequence start end)
  (with-gensyms (i j pivot data)
    `(locally
	 (declare (type function ,predicate ,@(if key `(,key)))
		  (type ,type ,sequence))
       ;; the start arg is actually not necessary but it is included
       ;; to make it easier to use insertion sort in other sorting 
       ;; algorithms such as quicksort
       (loop for ,j from (1+ ,start) below ,end 
	     do (let* ((,pivot (,ref ,sequence ,j))
		       ,@(if key 
			     `((,data (funcall ,key ,pivot)))
			     `((,data ,pivot)))
		       (,i (1- ,j)))
		  (declare (type fixnum ,i))
		  (loop while (and (>= ,i ,start) 
				   (funcall ,predicate 
					    ,data 
					    ,(if key 
						 `(funcall ,key (,ref ,sequence ,i))
						 `(,ref ,sequence ,i))))
			do (setf (,ref ,sequence (1+ ,i)) (,ref ,sequence ,i)
				 ,i (1- ,i)))
		  (setf (,ref ,sequence (1+ ,i)) ,pivot))))))


;;;
;;; insertion sort
;;;

(defun insertion-sort (sequence predicate &key key)
  (let ((end (length sequence)))
    (if key
	(sort-dispatch insertion-sort-body predicate key sequence 0 end)
	(sort-dispatch insertion-sort-body predicate nil sequence 0 end))
    sequence))
