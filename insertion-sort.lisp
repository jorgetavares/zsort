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
;;; insertion sort
;;;

(defun insertion-sort (sequence predicate &key key)
  (let ((end (length sequence))
	(valid-key (or key #'identity)))
    (sort-dispatch %insertion-sort-body predicate valid-key sequence 0 end)
    sequence))

(defmacro %insertion-sort-body (type ref mpredicate mkey msequence mstart mend)
  (alexandria:with-gensyms (i j pivot data sequence start end predicate key)
    `(labels ((insertion (,sequence ,start ,end ,predicate ,key)
		(declare (type function ,predicate ,key)
			 (type fixnum ,start ,end)
			 (type ,type ,sequence)
			 (optimize (speed 3) (safety 0)))
		;; the start arg is actually not necessary but it is included
		;; to make it easier to use insertion sort in other sorting 
		;; algorithms such as quicksort
		(loop for ,j from (1+ ,start) below ,end 
		      do (let* ((,pivot (,ref ,sequence ,j))
				(,data (funcall ,key ,pivot))
				(,i (1- ,j)))
			   (declare (type fixnum ,i))
			   (loop while (and (>= ,i ,start) 
					    (funcall ,predicate ,data (funcall ,key (,ref ,sequence ,i))))
				 do (setf (,ref ,sequence (1+ ,i)) (,ref ,sequence ,i)
					  ,i (1- ,i)))
			   (setf (,ref ,sequence (1+ ,i)) ,pivot)))))
       (insertion ,msequence ,mstart ,mend ,mpredicate ,mkey))))
