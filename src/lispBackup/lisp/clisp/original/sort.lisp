#|#########################################################################
# Copyright (C) 1997-2011 Ufasoft                                         #
# http://ufasoft.com   mailto:support@ufasoft.com                         #
#                                                                         #
# This program is free software; you can redistribute it and/or modify it #
# under the terms of the GNU General Public License as published by the   #
# Free Software Foundation; either version 3, or (at your option) any     #
# later version.                                                          #
#                                                                         #
# This program is distributed in the hope that it will be useful,         #
# but WITHOUT ANY WARRANTY; without even the implied warranty of          #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           #
# GNU General Public License for more details.                            #
#                                                                         #
# You should have received a copy of the GNU General Public License       #
# along with this program; If not, see <http://www.gnu.org/licenses/>     #
########################################################################=|#

(in-package "SYS")

(defun _stable-sort (seq pk i s e)
  (if (< s e) (let ((x (elt seq i))
                    (y (elt seq s)))
                (if (funcall pk y x) (setf (elt seq i) y)
                                     (setf (elt seq s) x))
                (_stable-sort seq pk i (1+ s) e))))

(defun stable-sort (seq p &key key)
  (let ((e (1- (length seq)))
        (pk (_pk p key)))
    (dotimes (i e seq)
      (_stable-sort seq pk i (1+ i) e))))
      
;!!! optimize
#|
(defun _qsort (seq pk s e)
  (if (< s e) (do ((i (1- s))
                   (j (1+ e))              
                   (x (elt seq s))
                   y
                   z)
                  (nil)
                (do ()
                    ((progn (setq z (elt seq (decf j))) (not (funcall pk x z)))))
                (do ()
                    ((progn (setq y (elt seq (incf i))) (not (funcall pk y x)))))
                (if (< i j) (progn  (setf (elt seq i) z)
                                    (setf (elt seq j) y))
                            (return (progn (_qsort seq pk s j)
                                           (_qsort seq pk (1+ j) e)))))))
|#

#| buggy
(defun _qsort (seq pk s e)
  (if (< s e) (do ((i s)
                   (j (1+ e))              
                   (x (elt seq s))
                   y
                   z)
                  (nil) 
                (do ()
                    ((or (<= (decf j) i) (funcall pk (setq z (elt seq j)) x))))
                (do ()
                    ((or (>= (incf i) j) (funcall pk x (setq y (elt seq i))))))
                (if (< i j) (progn  (setf (elt seq i) z)
                                    (setf (elt seq j) y))
                            (return (progn (setf (elt seq s) z)
                                           (setf (elt seq j) x)
                                           (_qsort seq pk s (1- j))
                                           (_qsort seq pk (1+ j) e)))))))

(defun sort (seq p &key key)
  (_qsort seq (_pk p key) 0 (1- (length seq)))
  seq)
|#  
      
      
(defun sort (sequence predicate &key key)
  "Destructively sorts sequence.  Predicate should returns non-Nil if
   Arg1 is to precede Arg2."
  (typecase sequence
    (simple-vector
     (if (> (the fixnum (length (the simple-vector sequence))) 0)
	 (sort-simple-vector sequence predicate key)
	 sequence))
    (list
     (sort-list sequence predicate key))
    (vector
     (if (> (the fixnum (length sequence)) 0)
	 (sort-vector sequence predicate key)
	 sequence))
    (t
     (error "~S is not a sequence." sequence))))



;;; Sorting Vectors

;;; Sorting is done with a heap sort.

(eval-when (compile eval)

;;; HEAPIFY, assuming both sons of root are heaps, percolates the root element
;;; through the sons to form a heap at root.  Root and max are zero based
;;; coordinates, but the heap algorithm only works on arrays indexed from 1
;;; through N (not 0 through N-1); This is because a root at I has sons at 2*I
;;; and 2*I+1 which does not work for a root at 0.  Because of this, boundaries,
;;; roots, and termination are computed using 1..N indexes.

(defmacro heapify (seq vector-ref root max pred key)
  (let ((heap-root (gensym))   (heap-max (gensym))     (root-ele (gensym))
	(root-key (gensym))    (heap-max/2 (gensym))   (heap-l-son (gensym))
	(one-son (gensym))     (one-son-ele (gensym))  (one-son-key (gensym))
	(r-son-ele (gensym))   (r-son-key (gensym))    (var-root (gensym)))
    `(let* ((,var-root ,root) ; necessary to not clobber calling root var.
	    (,heap-root (1+ ,root))
	    (,heap-max (1+ ,max))
	    (,root-ele (,vector-ref ,seq ,root))
	    (,root-key (apply-key ,key ,root-ele))
	    (,heap-max/2 (ash ,heap-max -1))) ; (floor heap-max 2)
       (declare (fixnum ,var-root ,heap-root ,heap-max ,heap-max/2))
       (loop
	(if (> ,heap-root ,heap-max/2) (return))
	(let* ((,heap-l-son (ash ,heap-root 1)) ; (* 2 heap-root)
	       ;; l-son index in seq (0..N-1) is one less than heap computation
	       (,one-son (1- ,heap-l-son))
	       (,one-son-ele (,vector-ref ,seq ,one-son))
	       (,one-son-key (apply-key ,key ,one-son-ele)))
	  (declare (fixnum ,heap-l-son ,one-son))
	  (if (< ,heap-l-son ,heap-max)
	      ;; there is a right son.
	      (let* ((,r-son-ele (,vector-ref ,seq ,heap-l-son))
		     (,r-son-key (apply-key ,key ,r-son-ele)))
		;; choose the greater of the two sons.
		(when (funcall ,pred ,one-son-key ,r-son-key)
		  (setf ,one-son ,heap-l-son)
		  (setf ,one-son-ele ,r-son-ele)
		  (setf ,one-son-key ,r-son-key))))
	  ;; if greater son is less than root, then we've formed a heap again.
	  (if (funcall ,pred ,one-son-key ,root-key) (return))
	  ;; else put greater son at root and make greater son node be the root.
	  (setf (,vector-ref ,seq ,var-root) ,one-son-ele)
	  (setf ,heap-root (1+ ,one-son)) ; one plus to be in heap coordinates.
	  (setf ,var-root ,one-son)))     ; actual index into vector for root ele.
       ;; now really put percolated value into heap at the appropriate root node.
       (setf (,vector-ref ,seq ,var-root) ,root-ele))))


;;; BUILD-HEAP rearranges seq elements into a heap to start heap sorting.
(defmacro build-heap (seq type len-1 pred key)
  (let ((i (gensym)))
    `(do ((,i (floor ,len-1 2) (1- ,i)))
	 ((minusp ,i) ,seq)
       (declare (fixnum ,i))
       (heapify ,seq ,type ,i ,len-1 ,pred ,key))))

) ; eval-when


;;; Make simple-vector and miscellaneous vector sorting functions.
(macrolet ((frob-rob (fun-name vector-ref)
	`(defun ,fun-name (seq pred key)
		(let ((len-1 (1- (length (the vector seq)))))
		  (declare (fixnum len-1))
		  (build-heap seq ,vector-ref len-1 pred key)
		  (do* ((i len-1 i-1)
		      	(i-1 (1- i) (1- i-1)))
		       ((zerop i) seq)
		    (declare (fixnum i i-1))
		    (rotatef (,vector-ref seq 0) (,vector-ref seq i))
		    (heapify seq ,vector-ref 0 i-1 pred key))))))

  (frob-rob sort-vector aref)

  (frob-rob sort-simple-vector svref))



;;;; Stable Sorting

(defun stable-sort (sequence predicate &key key)
  "Destructively sorts sequence.  Predicate should returns non-Nil if
   Arg1 is to precede Arg2."
  (typecase sequence
    (simple-vector
     (stable-sort-simple-vector sequence predicate key))
    (list
     (sort-list sequence predicate key))
    (vector
     (stable-sort-vector sequence predicate key))
    (t
     (error "~S is not a sequence." sequence))))


;;; Stable Sorting Lists


;;; SORT-LIST uses a bottom up merge sort.  First a pass is made over
;;; the list grabbing one element at a time and merging it with the next one
;;; form pairs of sorted elements.  Then n is doubled, and elements are taken
;;; in runs of two, merging one run with the next to form quadruples of sorted
;;; elements.  This continues until n is large enough that the inner loop only
;;; runs for one iteration; that is, there are only two runs that can be merged,
;;; the first run starting at the beginning of the list, and the second being
;;; the remaining elements.

(defun sort-list (list pred key)
  (let ((head (cons :header list))  ; head holds on to everything
	(n 1)                       ; bottom-up size of lists to be merged
	unsorted		    ; unsorted is the remaining list to be
				    ;   broken into n size lists and merged
	list-1			    ; list-1 is one length n list to be merged
	last)			    ; last points to the last visited cell
    (declare (fixnum n))
    (loop
     ;; start collecting runs of n at the first element
     (setf unsorted (cdr head))
     ;; tack on the first merge of two n-runs to the head holder
     (setf last head)
     (let ((n-1 (1- n)))
       (declare (fixnum n-1))
       (loop
	(setf list-1 unsorted)
	(let ((temp (nthcdr n-1 list-1))
	      list-2)
	  (cond (temp
		 ;; there are enough elements for a second run
		 (setf list-2 (cdr temp))
		 (setf (cdr temp) nil)
		 (setf temp (nthcdr n-1 list-2))
		 (cond (temp
			(setf unsorted (cdr temp))
			(setf (cdr temp) nil))
		       ;; the second run goes off the end of the list
		       (t (setf unsorted nil)))
		 (multiple-value-bind (merged-head merged-last)
				      (merge-lists* list-1 list-2 pred key)
		   (setf (cdr last) merged-head)
		   (setf last merged-last))
		 (if (null unsorted) (return)))
		;; if there is only one run, then tack it on to the end
		(t (setf (cdr last) list-1)
		   (return)))))
       (setf n (ash n 1)) ; (+ n n)
       ;; If the inner loop only executed once, then there were only enough
       ;; elements for two runs given n, so all the elements have been merged
       ;; into one list.  This may waste one outer iteration to realize.
       (if (eq list-1 (cdr head))
	   (return list-1))))))


;;; APPLY-PRED saves us a function call sometimes.
(eval-when (compile eval)
  (defmacro apply-pred (one two pred key)
    `(if ,key
	 (funcall ,pred (funcall ,key ,one)
		  (funcall ,key  ,two))
	 (funcall ,pred ,one ,two)))
) ; eval-when

(defvar *merge-lists-header* (list :header))

;;; MERGE-LISTS*   originally written by Jim Large.
;;; 		   modified to return a pointer to the end of the result
;;; 		      and to not cons header each time its called.
;;; It destructively merges list-1 with list-2.  In the resulting
;;; list, elements of list-2 are guaranteed to come after equal elements
;;; of list-1.
(defun merge-lists* (list-1 list-2 pred key)
  (do* ((result *merge-lists-header*)
	(P result))                            ; P points to last cell of result
       ((or (null list-1) (null list-2))       ; done when either list used up	
	(if (null list-1)                      ; in which case, append the
	    (rplacd p list-2)                  ;   other list
	    (rplacd p list-1))
	(do ((drag p lead)
	     (lead (cdr p) (cdr lead)))
	    ((null lead)
	     (values (prog1 (cdr result)       ; return the result sans header
			    (rplacd result nil)) ; (free memory, be careful)
		     drag))))		       ; and return pointer to last element
    (cond ((apply-pred (car list-2) (car list-1) pred key)
	   (rplacd p list-2)           ; append the lesser list to last cell of
	   (setq p (cdr p))            ;   result.  Note: test must bo done for
	   (pop list-2))               ;   list-2 < list-1 so merge will be
	  (T (rplacd p list-1)         ;   stable for list-1
	     (setq p (cdr p))
	     (pop list-1)))))



;;; Stable Sort Vectors

;;; Stable sorting vectors is done with the same algorithm used for lists,
;;; using a temporary vector to merge back and forth between it and the
;;; given vector to sort.


(eval-when (compile eval)

;;; STABLE-SORT-MERGE-VECTORS* takes a source vector with subsequences,
;;;    start-1 (inclusive) ... end-1 (exclusive) and
;;;    end-1 (inclusive) ... end-2 (exclusive),
;;; and merges them into a target vector starting at index start-1.

(defmacro stable-sort-merge-vectors* (source target start-1 end-1 end-2
					     pred key source-ref target-ref)
  (let ((i (gensym))
	(j (gensym))
	(target-i (gensym)))
    `(let ((,i ,start-1)
	   (,j ,end-1) ; start-2
	   (,target-i ,start-1))
       (declare (fixnum ,i ,j ,target-i))
       (loop
	(cond ((= ,i ,end-1)
	       (loop (if (= ,j ,end-2) (return))
		     (setf (,target-ref ,target ,target-i)
			   (,source-ref ,source ,j))
		     (incf ,target-i)
		     (incf ,j))
	       (return))
	      ((= ,j ,end-2)
	       (loop (if (= ,i ,end-1) (return))
		     (setf (,target-ref ,target ,target-i)
			   (,source-ref ,source ,i))
		     (incf ,target-i)
		     (incf ,i))
	       (return))
	      ((apply-pred (,source-ref ,source ,j)
			   (,source-ref ,source ,i)
			   ,pred ,key)
	       (setf (,target-ref ,target ,target-i)
		     (,source-ref ,source ,j))
	       (incf ,j))
	      (t (setf (,target-ref ,target ,target-i)
		       (,source-ref ,source ,i))
		 (incf ,i)))
	(incf ,target-i)))))


;;; VECTOR-MERGE-SORT is the same algorithm used to stable sort lists, but
;;; it uses a temporary vector.  Direction determines whether we are merging
;;; into the temporary (T) or back into the given vector (NIL).

(defmacro vector-merge-sort (vector pred key vector-ref)
  (let ((vector-len (gensym)) 		(n (gensym))
	(direction (gensym)) 		(unsorted (gensym))
	(start-1 (gensym)) 		(end-1 (gensym))
	(end-2 (gensym)) 		(temp-len (gensym))
	(i (gensym)))
    `(let ((,vector-len (length (the vector ,vector)))
	   (,n 1)         ; bottom-up size of contiguous runs to be merged
	   (,direction t) ; t vector --> temp    nil temp --> vector
	   (,temp-len (length (the simple-vector *merge-sort-temp-vector*)))
	   (,unsorted 0)  ; unsorted..vector-len are the elements that need
			  ; to be merged for a given n
	   (,start-1 0))  ; one n-len subsequence to be merged with the next
       (declare (fixnum ,vector-len ,n ,temp-len ,unsorted ,start-1))
       (if (> ,vector-len ,temp-len)
	   (setf *merge-sort-temp-vector*
		 (make-array (max ,vector-len (+ ,temp-len ,temp-len)))))
       (loop
	;; for each n, we start taking n-runs from the start of the vector
	(setf ,unsorted 0)
	(loop
	 (setf ,start-1 ,unsorted)
	 (let ((,end-1 (+ ,start-1 ,n)))
	   (declare (fixnum ,end-1))
	   (cond ((< ,end-1 ,vector-len)
		  ;; there are enough elements for a second run
		  (let ((,end-2 (+ ,end-1 ,n)))
		    (declare (fixnum ,end-2))
		    (if (> ,end-2 ,vector-len) (setf ,end-2 ,vector-len))
		    (setf ,unsorted ,end-2)
		    (if ,direction
			(stable-sort-merge-vectors*
			 ,vector *merge-sort-temp-vector*
			 ,start-1 ,end-1 ,end-2 ,pred ,key ,vector-ref svref)
			(stable-sort-merge-vectors*
			 *merge-sort-temp-vector* ,vector
			 ,start-1 ,end-1 ,end-2 ,pred ,key svref ,vector-ref))
		    (if (= ,unsorted ,vector-len) (return))))
		 ;; if there is only one run, copy those elements to the end
		 (t (if ,direction
			(do ((,i ,start-1 (1+ ,i)))
			    ((= ,i ,vector-len))
			  (declare (fixnum ,i))
			  (setf (svref *merge-sort-temp-vector* ,i)
				(,vector-ref ,vector ,i)))
			(do ((,i ,start-1 (1+ ,i)))
			    ((= ,i ,vector-len))
			  (declare (fixnum ,i))
			  (setf (,vector-ref ,vector ,i)
				(svref *merge-sort-temp-vector* ,i))))
		    (return)))))
	;; If the inner loop only executed once, then there were only enough
	;; elements for two subsequences given n, so all the elements have
	;; been merged into one list.  Start-1 will have remained 0 upon exit.
	(when (zerop ,start-1)
	  (if ,direction
	      ;; if we just merged into the temporary, copy it all back
	      ;; to the given vector.
	      (dotimes (,i ,vector-len)
		(setf (,vector-ref ,vector ,i)
		      (svref *merge-sort-temp-vector* ,i))))
	  (return ,vector))
	(setf ,n (ash ,n 1)) ; (* 2 n)
	(setf ,direction (not ,direction))))))

) ; eval-when


;;; Temporary vector for stable sorting vectors.
(defvar *merge-sort-temp-vector*
  (make-array 50))

(proclaim '(simple-vector *merge-sort-temp-vector*))

(defun stable-sort-simple-vector (vector pred key)
  (declare (simple-vector vector))
  (vector-merge-sort vector pred key svref))

(defun stable-sort-vector (vector pred key)
  (vector-merge-sort vector pred key aref))
      
