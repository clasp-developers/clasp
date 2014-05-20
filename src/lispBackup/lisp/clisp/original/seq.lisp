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

(in-package "SYSTEM")

(defvar *_seq-types* nil)

(defun _seq-type (x)
	(svref x 0))
	
(defun _seq-init (x)
	(svref x 1))

(defun _seq-upd (x)
	(svref x 2))

(defun _seq-endtest (x)
	(svref x 3))

(defun _seq-fe_init (x)
	(svref x 4))

(defun _seq-fe_upd (x)
	(svref x 5))

(defun _seq-fe_endtest (x)
	(svref x 6))

(defun _seq-access (x)
	(svref x 7))

(defun _seq-access_set (x)
	(svref x 8))

(defun _seq-copy (x)
	(svref x 9))

(defun _seq-length (x)
	(svref x 10))

(defun _seq-make (x)
	(svref x 11))

(defun _seq-elt (x)
	(svref x 12))

(defun _seq-set_elt (x)
	(svref x 13))

(defun _seq-init_start (x)
	(svref x 14))

(defun _seq-fe_init_end (x)
	(svref x 15))	

(defun %defseq (x)
	(push x *_seq-types*)
	(_seq-type x))
	

#|												

(defun _get-seq-type (seq)
	(_find-seq-type (cond ((listp seq) 'list)
												((stringp seq) 'string)
												((vectorp seq) 'vector))))

(defun _get-valid-seq-type (seq)
  (or (_get-seq-type seq)
      (err)))
                
(defun _get-td-check-index (seq i)
	(prog1 (_get-valid-seq-type seq)
  			 (if (or (not (integerp i)) (minusp i))
  			 	 (err))
  			 (when (and (vectorp seq) (>= i (length seq)))
  			 	 (err))))
|#
	
#|!!!
(defun |(SETF ELT)| (n seq i)
  (funcall (_seq-set_elt (_get-td-check-index seq i)) seq i n)
	  n)
	  |#

(defun (SETF ELT) (n seq i) ;!!!
  (|(SETF ELT)| n seq i))
	  
(defun count-if (p seq &key from-end (start 0) end key)
  (let ((c 0))
    (_seq-test #'(lambda (i)
					(declare (ignore i))
					(incf c)) p seq from-end start end key)
    c))

(defun count-if-not (p seq &rest rest &key from-end (start 0) end key)
	(declare (ignore from-end start end key))
  (apply #'count-if (complement p) seq rest))

(defun count (item seq &rest rest &key from-end (start 0) end key (test #'eql) test-not)
	(declare (ignore from-end start end key))
  (apply #'count-if (_test item test test-not) seq :allow-other-keys t rest))

(defun find-if (p seq &key from-end (start 0) end key)
  (_seq-test #'(lambda (i) (return-from find-if (elt seq i))) p seq from-end start end key))

(defun find-if-not (p seq &rest rest &key from-end (start 0) end key)
	(declare (ignore from-end start end key))
  (apply #'find-if (complement p) seq rest))

(defun find (item seq &rest rest &key from-end (test #'eql) test-not (start 0) end key)
	(declare (ignore from-end start end key))
  (apply #'find-if (_test item test test-not) seq :allow-other-keys t rest))

(defun position-if (p seq &key from-end (start 0) end key)
  (_seq-test #'(lambda (i) (return-from position-if i)) p seq from-end start end key))

(defun position-if-not (p seq &rest rest &key from-end (start 0) end key)
	(declare (ignore from-end start end key))
  (apply #'position-if (complement p) seq rest))

(defun fill (seq item &key (start 0) end)
  (_seq-iterate #'(lambda (x z i)
						(declare (ignore x z))
                     (setf (elt seq i) item)) seq nil start end nil))

(defun nsubstitute-if (new p seq &key from-end (start 0) end count key)
  (_seq-iterate #'(lambda (x z i)
						(declare (ignore x))
                    (when (and (or (null count) (plusp count))
                               (funcall p z))
                      (setf (elt seq i) new)
                      (if count (decf count))))
                seq from-end start end key))

(defun nsubstitute-if-not (new p seq &rest rest &key from-end (start 0) end count key)
	(declare (ignore from-end start end count key))
  (apply #'nsubstitute-if new (complement p) seq rest))

(defun nsubstitute (new old seq &rest rest &key from-end (test #'eql) test-not (start 0) end count key)
	(declare (ignore from-end start end count key))
  (apply #'nsubstitute-if new (_test old test test-not) seq :allow-other-keys t rest))

(defun substitute (new old seq &rest rest &key from-end (test #'eql) test-not (start 0) end count key)
	(declare (ignore from-end test test-not start end count key))
  (apply #'nsubstitute new old (copy-seq seq) rest))

(defun substitute-if (new p seq &rest rest &key from-end (start 0) end count key)
	(declare (ignore from-end start end count key))
  (apply #'nsubstitute-if new p (copy-seq seq) rest))

(defun substitute-if-not (new p seq &rest rest &key from-end (start 0) end count key)
	(declare (ignore from-end start end count key))
  (apply #'nsubstitute-if-not new p (copy-seq seq) rest))
  
(defun remove-if-not (p seq &rest rest &key from-end (start 0) end count key)
	(declare (ignore from-end start end count key))
  (apply #'remove-if (complement p) seq rest))

(defun remove (item seq &rest rest &key from-end (test #'eql) test-not (start 0) end count key)
	(declare (ignore from-end start end count key))
  (apply #'remove-if (_test item test test-not) seq :allow-other-keys t rest))

;!!!
(defun delete-if (p seq &rest rest &key from-end (start 0) end count key)
	(declare (ignore from-end start end count key))
  (apply #'remove-if p seq rest))

(defun delete-if-not (p seq &rest rest &key from-end (start 0) end count key)
	(declare (ignore from-end start end count key))
  (apply #'delete-if (complement p) seq rest))

(defun delete (item seq &rest rest &key from-end (test #'eql) test-not (start 0) end count key)
	(declare (ignore from-end start end count key))
  (apply #'delete-if (_test item test test-not) seq :allow-other-keys t rest))

#|!!!  
(defun _list-to-seq (seq list from-end)
  (do ((last (1- (length seq)))
       (n 0 (1+ n))
       (x list (cdr x)))
      ((endp x) seq)
    (setf (elt seq (if from-end n (- last n))) (car x))))  
|#
  
(defun remove-duplicates (seq &key from-end (test #'eql) test-not (start 0) end key)
  (let (stack)
    (_seq-iterate #'(lambda (x z i)
						(declare (ignore i))
                      (if (not (find z stack :test test :test-not test-not :key key))
                        (push x stack)))
                  seq (not from-end) start end key)
    (concatenate (type-of seq) (subseq seq 0 start) (if from-end (nreverse stack) stack) (subseq seq (_end end seq)))))

(defun delete-duplicates (seq &rest rest &key from-end (test #'eql) test-not (start 0) end key)
		(declare (ignore from-end test test-not start end key))
  (apply #'remove-duplicates seq rest))

(defun search (seq1 seq2 &rest rest &key from-end (test #'eql) test-not key (start1 0) (start2 0) end1 end2)
		(declare (ignore test test-not key))
	(setq end2 (_end end2 seq2))
  (do ((i (if from-end (1- end2) start2) (if from-end (1- i) (1+ i)))
       (len1 (- (_end end1 seq1) start1)))  
        ((if from-end (< i start2) (>= i end2)) nil)
    (unless (apply #'mismatch seq1 seq2 :start2 i :end2 (min end2 (+ i len1)) rest)
      (return-from search i))))
      
(defun _copy-seqpart-into (seq1 td1 seq2 td2 count p1 p2)
  (let ((u1 (_seq-upd td1))
  			(u2 (_seq-upd td2))
  			(acc1 (_seq-access td1))
  			(accset2 (_seq-access_set td2)))  			
		(dotimes (i count seq2)
			(funcall accset2 seq2 p2 (funcall acc1 seq1 p1))
			(setq p1 (funcall u1 seq1 p1))
			(setq p2 (funcall u2 seq2 p2)))))
			
(defun _copy-seqpart-onto (seq1 td1 seq2 td2 count p1)
	(_copy-seqpart-into seq1 td1 seq2 td2 count p1 (funcall (_seq-init td2) seq2)))
	
(defun _merge (seq seq1 seq2 n1 n2 p k)
  (let* ((e1 (< n1 (length seq1)))
         (e2 (< n2 (length seq2)))
         s
         (x (if e1 (elt seq1 n1)))
         (y (if e2 (elt seq2 n2))))
    (cond ((and (not e1) (not e2)) (return-from _merge))
          ((not e1))
          ((not e2) (setq s t))
          ((let ((vx (funcall k x))
                 (vy (funcall k y)))
             (setq s (or (funcall p vx vy) (not (funcall p vy vx)))))))
    (setf (elt seq (+ n1 n2)) (if s x y))
    (_merge seq seq1 seq2 (if s (1+ n1) n1) (if s n2 (1+ n2)) p k)))

(defun merge (result-type seq1 seq2 p &key key)
  (let ((seq (make-sequence result-type (+ (length seq1) (length seq2)))))
    (_merge seq seq1 seq2 0 0 p (_key key))
    seq))
	
(defun copy-seq (seq)
	(subseq seq 0))
	
(defun _coerce-seq (seq typ error-p)
  (multiple-value-bind (td2 len)
			(if error-p (_valid-type typ) (_valid-type1 typ))			
		(when td2
			(let* ((td1 (_get-valid-seq-type seq))
			       (stl (funcall (_seq-length td1) seq)))
			  (when (and len
			             (if (eql len -1) (zerop stl)
			                              (not (eql len stl))))
			    (err))
			  (if (eq (_seq-type td1) (_seq-type td2))
			    seq
		    	(_copy-seqpart-onto seq td1 (funcall (_seq-make td2) stl)
		    	                        td2 stl (funcall (_seq-init td1) seq)))))))
		    

(%putd 'list-llength #'length)
(%putd 'vector-length #'length)

(defun list-upd (seq p)
		(declare (ignore seq))
	(cdr p))
	
(defun list-endtest (seq p)
		(declare (ignore seq))
	(endp p))	

(defun list-fe-init (seq)
	(revappend seq nil))
	
(defun list-access (seq p)
		(declare (ignore seq))
	(if (atom p)
			(err)
			(car p)))
	
(defun list-access-set (seq p v)
		(declare (ignore seq))
	(setf (car p) v))
	
(defun list-elt (seq i)
	(nth i seq))	

(defun list-set-elt (seq i v)
	(setf (nth i seq) v))
                  
(defun list-init-start (seq i)
	(nthcdr i seq))	 		

(defun list-fe-init-end (seq index)
	(if (<= 0 index)
    (do* ((L1 nil (cons (car L2) L1))
          (L2 seq (cdr L2))
          (i index (1- i)))
          ((zerop i) L1)
      (if (atom L2)
        (err)))
    (err)))
	
(defun vector-init (seq)
		(declare (ignore seq))
	0)

(defun vector-upd (seq p)
		(declare (ignore seq))
	(1+ p))
	
(defun vector-endtest (seq p)
	(= p (length seq)))
	
(defun vector-fe-init (seq)
	(1- (length seq)))
	
(defun vector-fe-upd (seq p)
		(declare (ignore seq))
	(1- p))
	
(defun vector-fe-endtest (seq p)
		(declare (ignore seq))
	(minusp p))
	
(defun vector-init-start (seq i)
	(if (<= 0 i (length seq))
		i
		(err)))
	    
(defun vector-fe-init-end (seq i)
	(if (<= 0 i (length seq))
		(1- i)
		(err)))
		
(defun make-bit-vector (len)
	(make-array len :element-type 'bit))		
		
(defun reduce (fun seq &key key from-end (start 0) end (initial-value nil init-suppl))
  (setq key (_key key)
        end (_end end seq))
  (if (= start end) (if init-suppl initial-value
                                   (funcall fun))
                    (do* ((i (if from-end (1- end) start) (if from-end (1- i) (1+ i)))
                          (r (if init-suppl initial-value
                                            (prog1 (funcall key (elt seq i))
                                                   (if from-end (decf i) (incf i))))))                             
                         ((if from-end (< i start) (>= i end)) r)
                      (setq r (let ((x (funcall key (elt seq i))))
                                (if from-end (funcall fun x r)
                                             (funcall fun r x)))))))

	
