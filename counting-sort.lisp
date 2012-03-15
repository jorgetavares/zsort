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
;;; counting sort algorithm
;;;

(defmacro counting-sort-body (type ref ascend sequence end min max) 
  (with-gensyms (count index i)
    `(locally 
	 (declare (type ,type ,sequence)
		  (type fixnum ,min ,max))
       (let ((,count (make-array (1+ (- ,max ,min))))
	     (,index 0))
	 (loop for ,i from 0 below ,end
	       do (incf (,ref ,count (- (,ref ,sequence ,i) ,min))))
	 (loop ,@(if ascend
		     `(for ,i from ,min to ,max)
		     `(for ,i downfrom ,max downto ,min))
	   do (loop while (> (,ref ,count (- ,i ,min)) 0)
		    do (progn
			 (setf (,ref ,sequence ,index) ,i) 
			 (incf ,index)
			 (decf (,ref ,count (- ,i ,min))))))))))


;;;
;;; counting sort
;;;

(defun counting-sort (sequence &key (ascend t))
  (let ((end (length sequence))
	(min (reduce #'min sequence))
	(max (reduce #'max sequence)))
    (if ascend
	(sort-dispatch counting-sort-body t   sequence end min max)
	(sort-dispatch counting-sort-body nil sequence end min max))
    sequence))
