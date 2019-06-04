;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;                           sequence routines

(in-package "SYSTEM")

#+ecl-min
(eval-when (:execute)
  (load (merge-pathnames "seqmacros.lsp" *load-truename*)))

(defun sequence-count (count)
  (cond ((null count)
         most-positive-fixnum)
        ((fixnump count)
         count)
        ((integerp count)
         (if (minusp count)
             -1
             most-positive-fixnum))
        (t
         (error 'simple-type-error
                :datum count
                :expected-type 'integer
                :format-control "The value of :COUNT is not a valid counter~%~4I~A"
                :format-arguments (list count)))))

(defun test-error()
  (error "both test and test-not are supplied"))

(defun unsafe-funcall1 (f x)
  (declare (function f)
	   (optimize (speed 3) (safety 0)))
  (funcall f x))

(defun reduce (function sequence
               &key from-end
                    (start 0)
                    end
                    key (initial-value nil ivsp))
  (let ((function (si::coerce-to-function function)))
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (with-start-end (start end sequence length)
      (declare (ignore length))
      (with-key (key)
        (cond ((>= start end)
               (if ivsp
                   initial-value
                   (funcall function)))
              ((listp sequence)
               (when from-end
                 (let* ((output nil))
                   (do-sublist (elt sequence start end)
                     (setf output (cons elt output)))
                   (setf sequence output
                         end (- end start) start 0)))
               (while (plusp start)
                 (setf sequence (cdr (the cons sequence))
                       start (1- start)
                       end (1- end)))
               (unless ivsp
                 (setf initial-value (key (car (the cons sequence)))
                       sequence (cdr (the cons sequence))
                       end (1- end)))
               (do-sublist (elt sequence 0 end :output initial-value)
                 (setf initial-value
                       (if from-end
                           (funcall function (key elt) initial-value)
                           (funcall function initial-value (key elt))))))
              (from-end
               (unless ivsp
                 (setf initial-value (key (aref sequence (1- end)))
                       end (1- end)))
               (do-subvector (elt sequence start end :from-end t
                                                     :output initial-value)
                 (setf initial-value
                       (funcall function (key elt) initial-value))))
              (t
               (unless ivsp
                 (setf initial-value (key (aref sequence start))
                       start (1+ start)))
               (do-subvector (elt sequence start end :output initial-value)
                 (setf initial-value
                       (funcall function initial-value (key elt))))
                 ))))))

(defun fill (sequence item &key (start 0) end)
  ;; INV: WITH-START-END checks the sequence type and size.
  (reckless
   (with-start-end (start end sequence)
     (if (listp sequence)
         (do* ((x (nthcdr start sequence) (cdr x))
               (i (- end start) (1- i)))
              ((zerop i)
               sequence)
           (declare (fixnum i) (cons x))
           (setf (first x) item))
         (si::fill-array-with-elt sequence item start end)))
   sequence))

(defun replace (sequence1 sequence2 &key (start1 0) end1 (start2 0) end2)
  (with-start-end (start1 end1 sequence1)
   (with-start-end (start2 end2 sequence2)
     (declare (optimize (speed 3) (safety 0) (debug 0)))
     (let ((length (min (- end2 start2) (- end1 start1))))
       (declare (fixnum length))
       ;; If the two sequences are arrays, we can use COPY-SUBARRAY.
       ;; Otherwise we have our own loop, which relies on sequence
       ;; iterators. It becomes inefficient when sequences overlap
       ;; because it has to save the data.
       (if (and (vectorp sequence1)
                (vectorp sequence2))
           (copy-subarray sequence1 start1 sequence2 start2 length)
           (do* ((data (if (and (eq sequence1 sequence2)
                                (> start1 start2))
                           (subseq sequence2 start2 end2)
                           sequence2))
                 (it2 (make-seq-iterator data start2)
                      (seq-iterator-next data it2))
                 (it1 (make-seq-iterator sequence1 start1)
                      (seq-iterator-next sequence1 it1)))
                ((or (<= length 0) (null it1) (null it2)))
             (seq-iterator-set sequence1 it1
                               (seq-iterator-ref sequence2 it2))
             (decf length))))))
  sequence1)

(defun filter-vector (which out in start end from-end count
                      test test-not key)
  (with-tests (test test-not key)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (with-start-end (start end in l)
      (with-count (%count count :output in)
	(let* ((existing 0))
          (declare (fixnum existing))
          ;; If the OUT is empty that means we REMOVE and we have to
          ;; create the destination array. For that we first count how
          ;; many elements are deletable and allocate the
          ;; corresponding amount of space.
          (unless (eq out in)
            (setf existing (count which in :start start :end end
                                   :test test :test-not test-not :key key))
            (when (zerop existing)
              (return-from filter-vector
                (values in l)))
            (setf out (make-array (- l (min existing %count))
                                  :element-type
                                  (array-element-type in))))
          ;; We begin by copying the elements in [0, start)
          (unless (eq out in)
            (copy-subarray out 0 in 0 start))
          ;; ... skip the elements in [start, end) which either
          ;; do not need to be filtered (because of :from-end)
          ;; or do not satisfy the test, 
          (let ((skip 0))
            (declare (fixnum skip))
            (when from-end
              (unless (plusp existing)
                (setf existing (count which in :start start :end end
                                      :test test :test-not test-not
                                      :key key)))
              (setf skip (if (< existing %count) 0 (- existing %count))))
            (if (eq out in)
                (do-subvector (elt in start end :index index)
                  (when (and (compare which (key elt))
                             (minusp (decf skip)))
                    (return))
                  (incf start))
                (do-subvector (elt in start end :index index)
                  (when (and (compare which (key elt))
                             (minusp (decf skip)))
                    (return))
                  (setf (aref (the vector out) start) elt
                        start (1+ start)))))
          ;; ... now filter the rest
          (do-subvector (elt in start end :index index)
            (if (compare which (key elt))
                (when (zerop (decf %count))
                  (setf end (1+ index))
                  (return))
                (setf (aref (the vector out) start) elt
                      start (1+ start))))
          ;; ... and copy the elements outside the limits
          (copy-subarray out start in end l)
          (values out (+ start (- l end))))))))

(defun remove-list (which sequence start end count test test-not key)
  (with-tests (test test-not key)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (with-start-end (start end sequence)
      (with-count (%count count :output sequence)
	(let* ((output nil)
	       (index 0))
	  (declare (fixnum index))
	  (while (and sequence (< index start))
	    (setf output (cons (car (the cons sequence)) output)
		  sequence (cdr (the cons sequence))
		  index (1+ index)))
          (loop
             (unless (< index end) (return))
             (let ((elt (car (the cons sequence))))
               (setf sequence (cdr (the cons sequence)))
               (if (compare which (key elt))
                   (when (zerop (decf %count))
                     (return))
                   (push elt output))
               (incf index)))
	  (nreconc output sequence))))))

(defun remove (which sequence &key test test-not (start 0) end
               from-end count key)
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (if (listp sequence)
      (if from-end
          (let ((l (length sequence)))
            (nreverse (delete which (reverse sequence)
                              :start (if end (- l end) 0) :end (- l start)
                              :from-end nil
                              :test test :test-not test-not :key key
                              :count count)))
          (remove-list which sequence start end count test test-not key))
      (values (filter-vector which nil sequence start end from-end count
                             test test-not key))))

(defun remove-if (predicate sequence &key (start 0) end from-end count key)
  (remove (si::coerce-to-function predicate) sequence
	  :start start :end end :from-end from-end :count count
	  :test #'unsafe-funcall1 :key key))

(defun remove-if-not (predicate sequence &key (start 0) end from-end count key)
  (remove (si::coerce-to-function predicate) sequence
	  :start start :end end :from-end from-end :count count
	  :test-not #'unsafe-funcall1 :key key))






(defun delete-list (which sequence start end count test test-not key)
  (with-tests (test test-not key)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (with-start-end (start end sequence)
      (with-count (%count count :output sequence)
	(let* ((splice (cons nil sequence))
               (output splice)
	       (index 0))
	  (declare (fixnum index)
		   (cons splice))
	  (while (and sequence (< index start))
	    (setf sequence (cdr (the cons sequence))
		  splice (cdr (the cons splice))
		  index (1+ index)))
	  (block nil
	    (tagbody
	     top
	       (unless (< index end)
		 (return))
	       (let ((elt (car (the cons sequence))))
		 (setf sequence (cdr (the cons sequence)))
		 (cond ((compare which (key elt))
			(setf (cdr splice) sequence)
			(when (zerop (decf %count))
			  (return)))
		       (t
			(setf splice (cdr splice))))
		 (incf index)
		 )
	       (go top)
	       ))
          (cdr output))))))

(defun delete (which sequence &key test test-not (start 0) end
               from-end count key)
  (declare (optimize (speed 3) (safety 1) (debug 0))
           (ext:check-arguments-type nil))
  (cond ((listp sequence)
         (if from-end
             (let ((l (length sequence)))
               (nreverse
                (delete-list which (nreverse sequence)
                             (if end (- l end) 0) (- l start)
                             count test test-not key)))
             (delete-list which sequence start end count test test-not key)))
        ((not (vectorp sequence))
         (signal-type-error sequence 'sequence))
        ((array-has-fill-pointer-p (the vector sequence))
         (multiple-value-bind (sequence l)
             (filter-vector which sequence sequence start end from-end count
                            test test-not key)
           (setf (fill-pointer (the vector sequence)) l)
           sequence))
        (t
         (values (filter-vector which nil sequence start end from-end count
                                test test-not key)))))


(defun delete-if (predicate sequence &key (start 0) end from-end count key)
  (delete (coerce-fdesignator predicate) sequence
	  :start start :end end :from-end from-end :count count
	  :test #'unsafe-funcall1 :key key))

(defun delete-if-not (predicate sequence &key (start 0) end
                      from-end count key)
  (delete (coerce-fdesignator predicate) sequence
	  :start start :end end :from-end from-end :count count
	  :test-not #'unsafe-funcall1 :key key))

(defun count (item sequence &key test test-not from-end (start 0) end key)
  (with-tests (test test-not key)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (with-start-end (start end sequence l)
      (let ((counter 0))
	(declare (fixnum counter))
	(if from-end
	    (if (listp sequence)
                (count item (reverse sequence)
                       :start (- l end) :end (- l start)
                       :test test :test-not test-not :key key)
		(do-subvector (elt sequence start end :from-end t
                                                      :output counter)
		  (when (compare item (key elt))
		    (incf counter))))
	    (do-subsequence (elt sequence start end :output counter)
	      (when (compare item (key elt))
		(incf counter))))))))

(defun count-if (predicate sequence &key from-end (start 0) end key)
  (count (coerce-fdesignator predicate) sequence
	 :from-end from-end :start start :end end
	 :test #'unsafe-funcall1 :key key))

(defun count-if-not (predicate sequence &key from-end (start 0) end key)
  (count (coerce-fdesignator predicate)
	 sequence :from-end from-end :start start :end end
	 :test-not #'unsafe-funcall1 :key key))

(defun substitute (new old sequence &key test test-not (start 0) end
                   from-end count key)
  (nsubstitute new old (copy-seq sequence) :start start :end end :from-end from-end
	       :count count :key key :test test :test-not test-not))

(defun substitute-if (new predicate sequence
		      &key (start 0) end from-end count key)
  (nsubstitute new (coerce-fdesignator predicate) (copy-seq sequence)
	       :key key :test #'unsafe-funcall1
               :start start :end end :from-end from-end :count count))

(defun substitute-if-not (new predicate sequence
			  &key (start 0) end from-end count key)
  (nsubstitute new (coerce-fdesignator predicate) (copy-seq sequence)
	       :key key :test-not #'unsafe-funcall1
               :start start :end end :from-end from-end :count count))

(defun nsubstitute (new old sequence &key test test-not (start 0) end
                    from-end count key)
  (with-tests (test test-not key)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (with-start-end (start end sequence l)
      (with-count (%count count :output sequence)
	;; FIXME! This could be simplified to (AND FROM-END COUNT)
	;; but the ANSI test suite complains because it expects always
	;; a from-end inspection order!
        (if from-end
            (if (listp sequence)
                (nreverse
                 (nsubstitute new old (nreverse sequence)
                              :start (- l end) :end (- l start)
                              :key key :test test :test-not test-not
                              :count count))
                (do-subvector (elt sequence start end :setter setf-elt
                                                      :from-end t :output sequence)
                  (when (compare old (key elt))
                    (setf-elt new)
                    (when (zerop (decf %count))
                      (return sequence)))))
            (do-subsequence (elt sequence start end :setter setf-elt
                                                    :output sequence)
              (when (compare old (key elt))
                (setf-elt new)
                (when (zerop (decf %count))
                  (return sequence)))))))))

(defun nsubstitute-if (new predicate sequence
                       &key (start 0) end from-end count key)
  (nsubstitute new (coerce-fdesignator predicate) sequence
	       :key key :test #'unsafe-funcall1
               :start start :end end :from-end from-end :count count))

(defun nsubstitute-if-not (new predicate sequence
                           &key (start 0) end from-end count key)
  (nsubstitute new (coerce-fdesignator predicate) sequence
	       :key key :test-not #'unsafe-funcall1
               :start start :end end :from-end from-end :count count))


(defun find (item sequence &key test test-not (start 0) end from-end key)
  (with-tests (test test-not key)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (with-start-end (start end sequence length)
      (declare (ignore length))
      (let ((output nil))
        (do-subsequence (elt sequence start end :output output :index index)
          (when (compare item (key elt))
            (unless from-end
              (return elt))
            (setf output elt)))))))

(defun find-if (predicate sequence &key from-end (start 0) end key)
  (find (coerce-fdesignator predicate) sequence
	:from-end from-end :start start :end end
	:test #'unsafe-funcall1 :key key))

(defun find-if-not (predicate sequence &key from-end (start 0) end key)
  (find (coerce-fdesignator predicate) sequence
	:from-end from-end :start start :end end
	:test-not #'unsafe-funcall1 :key key))


(defun position (item sequence &key test test-not from-end (start 0) end key)
  (with-tests (test test-not key)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (with-start-end (start end sequence)
      (let ((output nil))
        (do-subsequence (elt sequence start end
                             :output output :index index)
          (when (compare item (key elt))
            (unless from-end
              (return index))
            (setf output index)))))))

(defun position-if (predicate sequence &key from-end (start 0) end key)
  (position (coerce-fdesignator predicate) sequence
            :from-end from-end :start start :end end
            :test #'unsafe-funcall1 :key key))

(defun position-if-not (predicate sequence &key from-end (start 0) end key)
  (position (coerce-fdesignator predicate) sequence
            :from-end from-end :start start :end end
            :test-not #'unsafe-funcall1 :key key))

(defun remove-duplicates-list (sequence start end from-end test test-not key)
  (with-tests (test test-not key)
    (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
    (with-start-end (start end sequence)
      (let* ((output nil))
        (while (and sequence (plusp start))
          (setf output (cons (car (the cons sequence)) output)
                sequence (cdr (the cons sequence))
                start (1- start)
                end (1- end)))
        (let ((start sequence)
              (end (nthcdr (- end start) sequence)))
          ;; When from-end, keep the first occurrence of each duplicate
          ;; element; otherwise we keep the last one. Hence, A-I-L-P
          ;; 1) if from-end, return T only when there are no duplicates
          ;;    before current;
          ;; 2) otherwise, return T only when there are no duplicates
          ;;    after the current one.
          (flet ((already-in-list-p (start current end from-end)
                   (let ((elt (key (car (the cons current)))))
                     (if from-end
                         (loop
                            (when (eq start current)
                              (return nil))
                            (when (compare elt (key (car (the cons start))))
                              (return t))
                            (setf start (cdr (the cons start))))
                         (loop
                            (setf current (cdr (the cons current)))
                            (when (eq current end)
                              (return nil))
                            (when (compare elt (key (car (the cons current))))
                              (return t)))))))
            (loop
               (when (eq sequence end)
                 (return (nreconc output sequence)))
               (unless (already-in-list-p start sequence end from-end)
                 (push (car (the cons sequence)) output))
               (setf sequence (cdr (the cons sequence))))))))))

(defun remove-duplicates (sequence
                          &key test test-not from-end (start 0) end key)
  "Args: (sequence
       &key key (test '#'eql) test-not
            (start 0) (end (length sequence)) (from-end nil))
Returns a copy of SEQUENCE without duplicated elements."
  (cond ((listp sequence)
         (remove-duplicates-list sequence start
                                 end from-end test test-not key))
        ((vectorp sequence)
         (let* ((l (filter-duplicates-vector nil sequence
                                             start end from-end
                                             test test-not key))
                (v (make-array l :element-type
                               (array-element-type sequence))))
           (filter-duplicates-vector v sequence
                                     start end from-end
                                     test test-not key)
           v))
        ((not (vectorp sequence))
         (signal-type-error sequence 'sequence))))

(defun delete-duplicates-list (sequence start end from-end test test-not key)
  (with-tests (test test-not key)
    (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
    (with-start-end (start end sequence)
      (let* ((splice (cons nil sequence))
             (output splice))
        (while (and sequence (plusp start))
          (setf splice (cdr (the cons splice))
                sequence (cdr (the cons sequence))
                start (1- start)
                end (1- end)))
        (let ((start splice)
              (end (nthcdr (- end start) sequence)))
          (flet ((already-in-list-p (start current end from-end)
                   (let ((elt (key (car (the cons current)))))
                     (if from-end
                         (loop
                            (when (eq start current)
                              (return nil))
                            (when (compare elt (key (car (the cons start))))
                              (return t))
                            (setf start (cdr (the cons start))))
                         (loop
                            (setf current (cdr (the cons current)))
                            (when (eq current end)
                              (return nil))
                            (when (compare elt (key (car (the cons current))))
                              (return t)))))))
            (loop
               (when (eq sequence end)
                 (return (cdr (the cons output))))
               (if (already-in-list-p (cdr (the cons start))
                                      sequence end from-end)
                   (setf sequence (cdr (the cons sequence))
                         (cdr splice) sequence)
                   (setf sequence (cdr (the cons sequence))
                         splice (cdr (the cons splice)))))))))))

(defun filter-duplicates-vector (out in start end from-end test test-not key)
  (with-tests (test test-not key)
    (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
    (with-start-end (start end in length)
      (when (and out (not (eq out in)))
        (copy-subarray out 0 in 0 start))
      (flet ((already-in-vector-p (sequence start current end from-end)
               (declare (vector sequence)
                        (fixnum start current end))
               (if from-end
                   (setf end current)
                   (setf start (1+ current)))
               (let ((base (key (aref sequence current))))
                 (do-subvector (elt sequence start end :output nil)
                   (when (compare base (key elt))
                     (return t))))))
        (let ((index start)
              (jndex start))
          (declare (fixnum index jndex))
        (loop
           (when (= index end)
             (return (progn
                       (when out (copy-subarray out jndex in end length))
                       (+ jndex (- length end)))))
           (unless (already-in-vector-p in start index end from-end)
             (when out
               (setf (aref (the vector out) jndex)
                     (aref (the vector in) index)))
             (setf jndex (1+ jndex)))
           (setf index (1+ index))))))))

(defun delete-duplicates (sequence
			  &key test test-not from-end (start 0) end key)
  "Args: (sequence &key key
		     (test '#'eql) test-not
                     (start 0) (end (length sequence)) (from-end nil))
Destructive REMOVE-DUPLICATES.  SEQUENCE may be destroyed."
  (cond ((listp sequence)
         (delete-duplicates-list sequence start end from-end
                                 test test-not key))
        ((not (vectorp sequence))
         (signal-type-error sequence 'sequence))
        ((array-has-fill-pointer-p sequence)
         (let ((l (filter-duplicates-vector sequence sequence
                                            start end from-end
                                            test test-not key)))
           (setf (fill-pointer sequence) l)
           sequence))
        (t
         (let* ((l (filter-duplicates-vector nil sequence
                                             start end from-end
                                             test test-not key))
                (v (make-array l :element-type
                               (array-element-type sequence))))
           (filter-duplicates-vector v sequence
                                     start end from-end
                                     test test-not key)
           v))))       

(defun mismatch (sequence1 sequence2
		 &key from-end test test-not key
		      (start1 0) (start2 0)
		      end1 end2)
  "Args: (sequence1 sequence2
       &key key (test '#'eql) test-not
            (start1 0) (end1 (length sequence1))
            (start2 0) (end2 (length sequence2))
            (from-end nil))
Compares element-wise the specified subsequences of SEQUENCE1 and SEQUENCE2.
Returns NIL if they are of the same length and they have the same elements in
the sense of TEST.  Otherwise, returns the index of SEQUENCE1 to the first
element that does not match."
  (with-start-end (start1 end1 sequence1)
   (with-start-end (start2 end2 sequence2)
    (with-tests (test test-not key)
      (if (not from-end)
	  (do ((i1 start1 (1+ i1))
	       (i2 start2 (1+ i2)))
	      ((or (>= i1 end1) (>= i2 end2))
	       (if (and (>= i1 end1) (>= i2 end2)) nil i1))
	    (declare (fixnum i1 i2))
	    (unless (compare (key (elt sequence1 i1))
			     (key (elt sequence2 i2)))
	      (return i1)))
	  (do ((i1 (1- end1) (1- i1))
	       (i2 (1- end2)  (1- i2)))
	      ((or (< i1 start1) (< i2 start2))
	       (if (and (< i1 start1) (< i2 start2)) nil (1+ i1)))
	    (declare (fixnum i1 i2))
	    (unless (compare (key (elt sequence1 i1))
                             (key (elt sequence2 i2)))
	      (return (1+ i1)))))))))


(defun search (sequence1 sequence2
               &key from-end test test-not key
                 (start1 0) (start2 0)
                 end1 end2)
  "Args: (sequence1 sequence2
       &key key (test '#'eql) test-not
            (start1 0) (end1 (length sequence1))
            (start2 0) (end2 (length sequence2))
            (from-end nil))
Searches SEQUENCE2 for a subsequence that element-wise matches SEQUENCE1.
Returns the index to the first element of the subsequence if such a
subsequence is found.  Returns NIL otherwise."
  #+(or)
  (search-generic sequence1 start1 end1 sequence2 start2 end2
                  test test-not key from-end)
  (cond
    ((and (stringp sequence1) (stringp sequence2)
          (not from-end) (not test) (not test-not) (not key))
     (search-string sequence1 start1 end1 sequence2 start2 end2))
    ((and (vectorp sequence1) (vectorp sequence2))
     (search-vector sequence1 start1 end1 sequence2 start2 end2
                    test test-not key from-end))
    (t
     (search-generic sequence1 start1 end1 sequence2 start2 end2
                     test test-not key from-end))))

(defun search-vector (sequence1 start1 end1 sequence2 start2 end2
                      test test-not key from-end)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0))
           (vector sequence1 sequence2))
  (with-tests (test test-not key)
    (with-start-end (start1 end1 sequence1)
      (with-start-end (start2 end2 sequence2)
        (do* ((last-index -1)
              (last (let* ((l (- end1 start1))
                           (e (- end2 l)))
                      (declare (fixnum l e))
                      (unless (plusp l)
                        (return-from search-vector
                          (if from-end end2 0)))
                      (1+ e)))
              (start2 start2 (1+ start2))
              (base (key (aref sequence1 start1))))
             ((>= start2 last)
              (if (minusp last-index) nil last-index))
          (declare (fixnum start2 last-index))
          (when (compare base (key (aref sequence2 start2)))
            (let* ((i1 start1)
                   (i2 start2))
              (declare (fixnum i1 i2))
              (loop
                 (setf i1 (1+ i1)
                       i2 (1+ i2))
                 (when (or (>= i1 end1) (>= i2 end2))
                   (when from-end
                     (setf last-index start2)
                     (return))
                   (return-from search-vector start2))
                 (unless (compare (key (aref sequence1 i1))
                                  (key (aref sequence2 i2)))
                   (return))))))))))

(defun search-generic (sequence1 start1 end1 sequence2 start2 end2
                       test test-not key from-end)
  (declare (optimize (speed 3) (safety 2) (debug 0) (space 0)))
  (with-tests (test test-not key)
    (with-start-end (start1 end1 sequence1)
      (with-start-end (start2 end2 sequence2)
        (do* ((last-index -1)
              (start2 start2 (1+ start2))
              (last (let* ((l (- end1 start1))
                           (e (- end2 l)))
                      (declare (fixnum l e))
                      (unless (plusp l)
                        (return-from search-generic
                          (if from-end end2 0)))
                      (1+ e)))
              (it1 (make-seq-iterator sequence1 start1))
              (base (key (seq-iterator-ref sequence1 it1)))
              (it2 (make-seq-iterator sequence2 start2)
                   (seq-iterator-next sequence2 it2)))
             ((or (null it2) (>= start2 last))
              (if (minusp last-index) nil last-index))
          (declare (fixnum start2 last-index last))
          (when (compare base (key (seq-iterator-ref sequence2 it2)))
            (let* ((it1 it1)
                   (it2 it2)
                   (i1 start1)
                   (i2 start2))
              (declare (fixnum i1 i2))
              (loop
                 (setf it1 (seq-iterator-next sequence1 it1)
                       it2 (seq-iterator-next sequence2 it2)
                       i1 (1+ i1)
                       i2 (1+ i2))
                 (when (or (>= i1 end1) (>= i2 end2)
                           (null it1) (null it2))
                   (when from-end
                     (setf last-index start2)
                     (return))
                   (return-from search-generic start2))
                 (unless (compare (key (seq-iterator-ref sequence1 it1))
                                  (key (seq-iterator-ref sequence2 it2)))
                   (return))))))))))

#-clasp-min
(defun sort (sequence predicate &key key)
  "Args: (sequence test &key key)
Destructively sorts SEQUENCE and returns the result.  TEST should return non-
NIL if its first argument is to precede its second argument.  The order of two
elements X and Y is arbitrary if both
	(FUNCALL TEST X Y)
	(FUNCALL TEST Y X)
evaluates to NIL.  See STABLE-SORT."
  (setf key (if key (coerce-fdesignator key) #'identity)
	predicate (coerce-fdesignator predicate))
  (if (listp sequence)
      (list-merge-sort sequence predicate key)
      (quick-sort sequence 0 (the fixnum (1- (length sequence))) predicate key)))


(defun list-merge-sort (l predicate key)
  (declare (optimize (safety 0) (speed 3))
	   (function predicate key))
  (prog ((i 0) left right l0 l1 key-left key-right)
     (declare (fixnum i))
     (setq i (length l))
     (cond ((< i 2) (return l))
	   ((= i 2)
	    (setq key-left (funcall key (car l)))
	    (setq key-right (funcall key (cadr l)))
	    (cond ((funcall predicate key-left key-right) (return l))
		  ((funcall predicate key-right key-left)
		   (return (nreverse l)))
		  (t (return l)))))
     (setq i (floor i 2))
     (do ((j 1 (1+ j)) (l1 l (cdr l1)))
	 ((>= j i)
	  (setq left l)
	  (setq right (cdr l1))
	  (rplacd l1 nil))
       (declare (fixnum j)))
     (setq left (list-merge-sort left predicate key))
     (setq right (list-merge-sort right predicate key))
     (cond ((endp left) (return right))
	   ((endp right) (return left)))
     (setq l0 (cons nil nil))
     (setq l1 l0)
     (setq key-left (funcall key (car left)))
     (setq key-right (funcall key (car right)))
   loop
     (cond ((funcall predicate key-left key-right) (go left))
	   ((funcall predicate key-right key-left) (go right))
	   (t (go left)))
   left
     (rplacd l1 left)
     (setq l1 (cdr l1))
     (setq left (cdr left))
     (when (endp left)
       (rplacd l1 right)
       (return (cdr l0)))
     (setq key-left (funcall key (car left)))
     (go loop)
   right
     (rplacd l1 right)
     (setq l1 (cdr l1))
     (setq right (cdr right))
     (when (endp right)
       (rplacd l1 left)
       (return (cdr l0)))
     (setq key-right (funcall key (car right)))
     (go loop)))

#-clasp-min
(defun quick-sort (seq start end pred key)
  (declare (fixnum start end)
           (function pred key)
           (optimize (safety 0)))
  (if (< start end)
      (let* ((j (1+ end)))
        (declare (fixnum j))
        (let* ((i start)
               (l (- end start))
               (l-half (ash l -1))
               (p (+ start l-half))
               (d (elt seq p))
               (kd (funcall key d)))
          (declare (fixnum i p l l-half))
          (rotatef (elt seq p) (elt seq start))
          (block outer-loop
            (loop
               (loop 
                  (unless (> (decf j) i) (return-from outer-loop))
                  (when (funcall pred 
                                 (funcall key (elt seq j)) kd)
                    (return)))
               (loop 
              (unless (< (incf i) j) (return-from outer-loop))
                  (unless (funcall pred
                                   (funcall key (elt seq i)) kd)
                    (return)))
               (rotatef (elt seq i) (elt seq j))))
          (setf (elt seq start) (elt seq j)
                (elt seq j) d))
        (if (< (the fixnum (- j start))
               (the fixnum (- end j)))
            (progn
              (quick-sort seq start (1- j) pred key)
              (quick-sort seq (1+ j) end pred key))
            (progn
              (quick-sort seq (1+ j) end pred key)
              (quick-sort seq start (1- j) pred key))))
      seq))


(defun stable-sort-merge-vectors (source target start-1
                                  end-1 end-2 pred key)
  (let ((i start-1)
        (j end-1) ; start-2
        (target-i start-1))
    (declare (fixnum i j target-i))
    (loop
      (cond ((= i end-1)
        (loop (if (= j end-2) (return))
                  (setf (aref target target-i)
                        (aref source j))
                  (incf target-i)
                  (incf j))
            (return))
            ((= j end-2)
             (loop (if (= i end-1) (return))
                  (setf (aref target target-i)
                        (aref source i))
                  (incf target-i)
                  (incf i))
             (return))
            ((if key
                 (funcall pred (funcall key (aref source j))
                               (funcall key (aref source i)))
                 (funcall pred (aref source j) (aref source i)))
             (setf (aref target target-i)
                   (aref source j))
             (incf j))
            (t (setf (aref target target-i)
                     (aref source i))
               (incf i)))
     (incf target-i))))


(defun vector-merge-sort (vector pred key)
  (let* ((vector-len (length (the vector vector)))
         (n 1)            ; bottom-up size of contiguous runs to be merged
         (direction t)    ; t vector --> temp    nil temp --> vector
         (temp (make-array vector-len))
         (unsorted 0)   ; unsorted..vector-len are the elements that need
                                    ; to be merged for a given n
         (start-1 0))   ; one n-len subsequence to be merged with the next
    (declare (fixnum vector-len n unsorted start-1))
    (loop
       ;; for each n we start taking n-runs from the start of the vector
      (setf unsorted 0)
      (loop
        (setf start-1 unsorted)
        (let ((end-1 (+ start-1 n)))
          (declare (fixnum end-1))
          (cond ((< end-1 vector-len)
                 ;; there are enough elements for a second run
                 (let ((end-2 (+ end-1 n)))
                   (declare (fixnum end-2))
                   (if (> end-2 vector-len) (setf end-2 vector-len))
                   (setf unsorted end-2)
                   (if direction
                       (stable-sort-merge-vectors
                          vector temp start-1 end-1 end-2 pred key)
                       (stable-sort-merge-vectors
                          temp vector start-1 end-1 end-2 pred key))
                   (if (= unsorted vector-len) (return))))
                ;; if there is only one run copy those elements to the end
                (t (if direction
                       (do ((i start-1 (1+ i)))
                           ((= i vector-len))
                         (declare (fixnum i))
                         (setf (aref temp i) (aref vector i)))
                       (do ((i start-1 (1+ i)))
                           ((= i vector-len))
                         (declare (fixnum i))
                         (setf (aref vector i) (aref temp i))))
                   (return)))))
      ;; If the inner loop only executed once then there were only enough
      ;; elements for two subsequences given n so all the elements have
      ;; been merged into one list. Start-1 will have remained 0 upon exit.
      (when (zerop start-1)
        (when direction
          ;; if we just merged into the temporary copy it all back
          ;; to the given vector.
          (dotimes (i vector-len)
            (setf (aref vector i) (aref temp i))))
        (return vector))
      (setf n (ash n 1))           ; (* 2 n)
      (setf direction (not direction)))))


(defun stable-sort (sequence predicate &key key)
  "Args: (sequence test &key key)
Destructively sorts SEQUENCE and returns the result.  TEST should return non-
NIL if its first argument is to precede its second argument.  For two elements
X and Y, if both
        (FUNCALL TEST X Y)
        (FUNCALL TEST Y X)
evaluates to NIL, then the order of X and Y are the same as in the original
SEQUENCE.  See SORT."
  (setf key (if key (coerce-fdesignator key) #'identity)
        predicate (coerce-fdesignator predicate))
  (if (listp sequence)
      (list-merge-sort sequence predicate key)
      (if (or (stringp sequence) (bit-vector-p sequence))
          (sort sequence predicate :key key)
          (vector-merge-sort sequence predicate key))))


(defun merge (result-type sequence1 sequence2 predicate &key key
              &aux (l1 (length sequence1)) (l2 (length sequence2)))
  "Args: (type sequence1 sequence2 test &key key)
Merges two sequences in the way specified by TEST and returns the result as a
sequence of TYPE.  Both SEQUENCEs may be destroyed.  If both SEQUENCE1 and
SEQUENCE2 are sorted in the sense of TEST, then the result is also sorted in
the sense of TEST."
  (declare (fixnum l1 l2))
  (with-key (key)
    (with-predicate (predicate)
      (do* ((size (the fixnum (+ l1 l2)))
            (j 0 (1+ j))
            (newseq (make-sequence result-type size))
            (i1 0)
            (i2 0))
           ((= j size) newseq)
        (declare (fixnum size j i1 i2))
        (if (>= i1 l1)
            (setf (elt newseq j) (elt sequence2 i2)
                  i2 (1+ i2))
            (let ((v1 (elt sequence1 i1)))
              (if (>= i2 l2)
                  (setf (elt newseq j) v1
                        i1 (1+ i1))
                  (let* ((v2 (elt sequence2 i2))
                         (k2 (key v2))
                         (k1 (key v1)))
                    (cond ((predicate k1 k2)
                           (setf (elt newseq j) v1
                                 i1 (1+ i1)))
                          ((predicate k2 k1)
                           (setf (elt newseq j) v2
                                 i2 (1+ i2)))
                          (t
                           (setf (elt newseq j) v1
                                 i1 (1+ i1))))))))))))

(defun complement (f)
  "Args: (f)
Returns a new function which first applies F to its arguments and then negates
the output"
  #'(lambda (&rest x) (not (apply f x))))




