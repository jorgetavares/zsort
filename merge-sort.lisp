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
;;; merge sort algorithm
;;;
;;; - top-down merge sort based on CCL's implementation
;;; - composed by two main macros: merge-sequences-body and merge-sort-body 
;;; - merge-sequences handles the merge of two sequences into an auxiliary one
;;; - merge-sort-body is the top-down algorithm which calls merge-sequences
;;; 

(defmacro merge-sequences-body (type ref a start-a end-a b start-b end-b aux start-aux predicate &optional key)
  (with-gensyms (i-a i-b i-aux v-a v-b k-a k-b merge-block) 
    `(locally
	 (declare (type fixnum ,start-a ,end-a ,start-b ,end-b ,start-aux)
		  (type ,type ,a ,b)
		  (type simple-vector ,aux)
		  (type function ,predicate ,@(if key `(,key)))
		  (optimize (speed 3) (safety 0)))
       (block ,merge-block
	 (let ((,i-a ,start-a)
	       (,i-b ,start-b)
	       (,i-aux ,start-aux)
	       ,v-a ,v-b ,k-a ,k-b)
	   (declare (type fixnum ,i-a ,i-b ,i-aux))
	   (cond ((= ,start-a ,end-a)
		  (when (= ,start-b ,end-b)
		    (return-from ,merge-block))
		  (setf ,i-a ,start-b
			,end-a ,end-b
			,a ,b
			,v-a (,ref ,a ,i-a)))
		 ((= ,start-b ,end-b)
		  (setf ,i-a ,start-a
			,v-a (,ref ,a ,i-a)))
		 (t
		  (setf ,v-a (,ref ,a ,i-a)
			,v-b (,ref ,b ,i-b)
			,@(if key 
			      `(,k-a (funcall ,key ,v-a))
			      `(,k-a ,v-a))
			,@(if key 
			      `(,k-b (funcall ,key ,v-b))
			      `(,k-b ,v-b)))
		  (loop 
		    (if (funcall ,predicate ,k-b ,k-a)
			(progn 
			  (setf (svref ,aux ,i-aux) ,v-b
				,i-aux (+ ,i-aux 1)
				,i-b (+ ,i-b 1))
			  (when (= ,i-b ,end-b) (return))
			  (setf ,v-b (,ref ,b ,i-b)
				,@(if key 
				      `(,k-b (funcall ,key ,v-b))
				      `(,k-b ,v-b))))
			(progn 
			  (setf (svref ,aux ,i-aux) ,v-a
				,i-aux (+ ,i-aux 1)
				,i-a (+ ,i-a 1))
			  (when (= ,i-a ,end-a)
			    (setf ,a ,b 
				  ,i-a ,i-b 
				  ,end-a ,end-b 
				  ,v-a ,v-b)
			    (return))
			  (setf ,v-a (,ref ,a ,i-a)
				,@(if key 
				      `(,k-a (funcall ,key ,v-a))
				      `(,k-a ,v-a))))))))
	   (loop
	     (setf (svref ,aux ,i-aux) ,v-a
		   ,i-a (+ ,i-a 1))
	     (when (= ,i-a ,end-a) (return))
	     (setf ,v-a (,ref ,a ,i-a)
		   ,i-aux (+ ,i-aux 1))))))))


(defmacro merge-sort-body (type ref mpredicate mkey msequence mstart mend)
  (with-gensyms (merge-sort-call maux aux sequence start end predicate key mid direction)
    `(locally
	 (declare (optimize (speed 3) (safety 0)))
       (labels ((,merge-sort-call (,sequence ,start ,end ,predicate ,key ,aux ,direction)
		  (declare (type function ,predicate ,@(if mkey `(,key)))
			   (type fixnum ,start ,end)
			   (type ,type ,sequence))
		  (let ((,mid (+ ,start (ash (- ,end ,start) -1))))
		    (declare (type fixnum ,mid))
		    (if (<= (- ,mid 1) ,start)
			(unless ,direction (setf (,ref ,aux ,start) (,ref ,sequence ,start)))
			(,merge-sort-call ,sequence ,start ,mid ,predicate ,key ,aux (not ,direction)))
		    (if (>= (+ ,mid 1) ,end)
			(unless ,direction (setf (,ref ,aux ,mid) (,ref ,sequence ,mid)))
			(,merge-sort-call ,sequence ,mid ,end ,predicate ,key ,aux (not ,direction)))
		    (unless ,direction (psetq ,sequence ,aux ,aux ,sequence))
		    ,(if mkey
			  `(merge-sequences-body ,type ,ref ,sequence ,start ,mid ,sequence 
						 ,mid ,end ,aux ,start ,predicate ,key)
			  `(merge-sequences-body ,type ,ref ,sequence ,start ,mid ,sequence 
						 ,mid ,end ,aux ,start ,predicate)))))
	 (let ((,maux (make-array ,mend)))
	   (declare (type simple-vector ,maux))
	   (,merge-sort-call ,msequence ,mstart ,mend ,mpredicate ,mkey ,maux nil))))))


;;;
;;; stable merge sort
;;;

(defun merge-sort (sequence predicate &key key)
  (let ((end (length sequence)))
    (if key
	(sort-dispatch merge-sort-body predicate key sequence 0 end)
	(sort-dispatch merge-sort-body predicate nil sequence 0 end))
    sequence))
