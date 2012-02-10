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
;;; insertion sort
;;;

(defmacro merge-sort-body (type ref mpredicate mkey msequence mstart mend)
    (alexandria:with-gensyms (i j pivot data sequence start end predicate key)
      `(locally
	   (declare (optimize (speed 3) (space 0)))
	 (labels ((insertion (,sequence ,start ,end ,predicate ,key)
		    (declare (type function ,predicate ,@(if mkey `(,key)))
			     (type fixnum ,start ,end)
			     (type ,type ,sequence)
			     ,@(unless mkey `((ignore ,key))))
		    ;; the start arg is actually not necessary but it is included
		    ;; to make it easier to use insertion sort in other sorting 
		    ;; algorithms such as quicksort
		    (loop for ,j from (1+ ,start) below ,end 
			  do (let* ((,pivot (,ref ,sequence ,j))
				    ,@(if mkey 
					  `((,data (funcall ,key ,pivot)))
					  `((,data ,pivot)))
				    (,i (1- ,j)))
			       (declare (type fixnum ,i))
			       (loop while (and (>= ,i ,start) 
						(funcall ,predicate 
							 ,data 
							 ,@(if mkey 
							       `((funcall ,key (,ref ,sequence ,i)))
							       `((,ref ,sequence ,i)))))
				     do (setf (,ref ,sequence (1+ ,i)) (,ref ,sequence ,i)
					      ,i (1- ,i)))
			       (setf (,ref ,sequence (1+ ,i)) ,pivot)))))
	   (insertion ,msequence ,mstart ,mend ,mpredicate ,mkey)))))

(defun merge-sort (sequence predicate &key key)
  (let ((end (length sequence)))
    (if key
	(sort-dispatch merge-sort-body predicate key sequence 0 end)
	(sort-dispatch merge-sort-body predicate nil sequence 0 end))
    sequence))

;;;;;;;;;;; adapted from ccl

(defun merge-ccl (sequence predicate key)
  (declare (type simple-vector sequence)
	   (type function predicate key)
	   (optimize (speed 3) (space 0)))
  (let ((start 0)
	(end (length sequence)))
    (declare (type fixnum start end))
    (when (> end 1)
      (let ((temp-array (make-array end)))
	(declare (type simple-vector temp-array))
	(merge-sort-sequence sequence start end predicate key temp-array nil))))
  sequence)

(defun merge-sort-sequence (sequence start end predicate key aux direction)
  (declare (type fixnum start end)
	   (type simple-vector sequence aux)
	   (type function predicate key)
	   (optimize (speed 3) (space 0)))
  (let ((mid (+ start (ash (- end start) -1))))
    (declare (type fixnum mid))
    (if (<= (- mid 1) start)
	(unless direction (setf (svref aux start) (svref sequence start)))
	(merge-sort-sequence sequence start mid predicate key aux (not direction)))
    (if (>= (+ mid 1) end)
	(unless direction (setf (svref aux mid) (svref sequence mid)))
	(merge-sort-sequence sequence mid end predicate key aux (not direction)))
    (unless direction (psetq sequence aux aux sequence))
    (merge-sequences sequence start mid sequence mid end aux start predicate key)))

(defun merge-sequences (a start-a end-a b start-b end-b aux start-aux predicate key)
  (declare (type fixnum start-a end-a start-b end-b start-aux)
	   (type simple-vector a b aux)
	   (type function predicate key)
	   (optimize (speed 3) (space 0)))
  (let ((i-a start-a)
	(i-b start-b)
	(i-aux start-aux)
	v-a v-b k-a k-b)
    (declare (type fixnum i-a i-b i-aux))
    (cond ((eq start-a end-a)
	   (when (eq start-b end-b)
	     (return-from merge-sequences aux))
	   (setf i-a start-b
		 end-a end-b
		 a b
		 v-a (svref a i-a)))
	  ((eq start-b end-b)
	   (setf i-a start-a
		 v-a (svref a i-a)))
	  (t
	   (setf v-a (svref a i-a)
		 v-b (svref b i-b)
		 k-a (if key (funcall key v-a) v-a)
		 k-b (if key (funcall key v-b) v-b))
	   (loop 
	     (if (funcall predicate k-b k-a)
		 (progn 
		   (setf (svref aux i-aux) v-b
			 i-aux (+ i-aux 1)
			 i-b (+ i-b 1))
		   (when (eq i-b end-b) (return))
		   (setf v-b (svref b i-b)
			 k-b (if key (funcall key v-b) v-b)))
		 (progn 
		   (setf (svref aux i-aux) v-a
			 i-aux (+ i-aux 1)
			 i-a (+ i-a 1))
		   (when (eq i-a end-a)
		     (setf a b 
			   i-a i-b 
			   end-a end-b 
			   v-a v-b)
		     (return))
		   (setf v-a (svref a i-a)
			 k-a (if key (funcall key v-a) v-a)))))))
    (loop
      (setf (svref aux i-aux) v-a
	    i-a (+ i-a 1))
      (when (eq i-a end-a) (return aux))
      (setf v-a (svref a i-a)
	    i-aux (+ i-aux 1)))))

