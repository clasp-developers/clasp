;;;; Code for the extensible sequences protocol.
;;;; System functionality is in lsp/seq{,lib,macros}.lsp.
;;;; It calls the generic functions here in the non-vector-list case.

(in-package "CLOS")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Core protocol
;;; Must be defined by extension programmers for their sequences to work.
;;;

(defgeneric sequence:elt (sequence index))

(defgeneric (setf sequence:elt) (new sequence index)
  (:argument-precedence-order sequence index new))

(defgeneric sequence:length (sequence))

(defgeneric sequence:make-sequence-like
    (sequence length &key initial-element initial-contents))

(defgeneric sequence:adjust-sequence
    (sequence length &key initial-element initial-contents))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Iterator protocol
;;; Optional.
;;; If not defined by extension programmers, the CLOS iterator protocol is used.
;;;

(defgeneric sequence:make-sequence-iterator (sequence &key from-end start end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLOS iterator protocol
;;; Optional.
;;; If not defined by extension programmers, a default implementation in terms
;;; of the core protocol will be used. This implementation will be reasonably
;;; efficient provided the sequence has efficient random access.
;;; If the full iterator protocol (i.e. make-sequence-iterator) is customized,
;;; the CLOS iterator protocol does not need to be defined. It is not used
;;; by any sequence functions directly.
;;;

(defgeneric sequence:make-simple-sequence-iterator
    (sequence &key from-end start end))

(defgeneric sequence:iterator-step (sequence iterator from-end))
(defgeneric sequence:iterator-endp (sequence iterator limit from-end))
(defgeneric sequence:iterator-element (sequence iterator))
(defgeneric (setf sequence:iterator-element) (new sequence iterator)
  (:argument-precedence-order sequence iterator new))
(defgeneric sequence:iterator-index (sequence iterator))
(defgeneric sequence:iterator-copy (sequence iterator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Definition of the iterator protocol in terms of the CLOS iterator protocol
;;;

(defmethod sequence:make-sequence-iterator
    ((sequence sequence) &key from-end start end)
  (multiple-value-bind (iterator limit from-end)
      (sequence:make-simple-sequence-iterator
       sequence :from-end from-end :start start :end end)
    (values iterator limit from-end
            #'sequence:iterator-step #'sequence:iterator-endp
            #'sequence:iterator-element #'(setf sequence:iterator-element)
            #'sequence:iterator-index #'sequence:iterator-copy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default iterator protocol
;;; Iterators and limits are indices.
;;;

(defmethod sequence:iterator-step ((sequence sequence) iterator from-end)
  (if from-end (1- iterator) (1+ iterator)))
(defmethod sequence:iterator-endp ((sequence sequence) iterator limit from-end)
  (= iterator limit))
(defmethod sequence:iterator-element ((sequence sequence) iterator)
  (sequence:elt sequence iterator))
(defmethod (setf sequence:iterator-element) (new (sequence sequence) iterator)
  (setf (sequence:elt sequence iterator) new))
(defmethod sequence:iterator-index ((sequence sequence) iterator)
  iterator)
(defmethod sequence:iterator-copy ((sequence sequence) iterator)
  iterator)

(defmethod sequence:make-simple-sequence-iterator
    ((sequence sequence) &key from-end (start 0) end)
  (let ((end (or end (length sequence))))
    (if from-end
        (values (1- end) (1- start) from-end)
        (values start end from-end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Core and iterator protocol specialization for LIST and VECTOR
;;;

(defun iep-and-icp ()
  (error "Supplied both ~s and ~s to ~s"
         :initial-element :initial-contents 'sequence:make-sequence-like))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; List
;;;

(defmethod sequence:elt ((sequence list) index) (nth index sequence))
(defmethod (setf sequence:elt) (new (sequence list) index)
  (setf (nth index sequence) new))
(defmethod sequence:length ((sequence null)) 0)
(defmethod sequence:length ((sequence cons)) (core:cons-length sequence))

(defmethod sequence:make-sequence-like
    ((sequence list) length
     &key (initial-element nil iep) (initial-contents nil icp))
  (cond ((and iep icp) (iep-and-icp))
        (iep (make-list length :initial-element initial-element))
        (icp (unless (= (length initial-contents) length)
               (error "Length mismatch in ~s" 'sequence:make-sequence-like))
             (replace (make-list length) initial-contents))
        (t (make-list length))))

(defmethod sequence:adjust-sequence
    ((s list) length &key initial-element (initial-contents nil icp))
  (if (zerop length)
      nil
      (let ((olength (length s)))
        (cond
          ((= length olength) (if icp (replace s initial-contents) s))
          ((< length olength) ; shorten the list
           (rplacd (nthcdr (1- length) s) nil)
           (if icp (replace s initial-contents) s))
          ((null s) ; make a new list
           (let ((r (make-list length :initial-element initial-element)))
             (if icp (replace r initial-contents) r)))
          (t ; lengthen list
           (rplacd (nthcdr (1- olength) s)
                   (make-list (- length olength)
                              :initial-element initial-element))
           (if icp (replace s initial-contents) s))))))

(defmethod sequence:make-sequence-iterator
    ((sequence list) &key from-end start end)
  (core::make-list-iterator sequence from-end start end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Vector
;;;

(defmethod sequence:elt ((sequence vector) index) (aref sequence index))
(defmethod (setf sequence:elt) (new (sequence vector) index)
  (setf (aref sequence index) new))
(defmethod sequence:length ((sequence vector)) (core::vector-length sequence))

(defmethod sequence:make-sequence-like
    ((sequence vector) length
     &key (initial-element nil iep) (initial-contents nil icp))
  (cond ((and iep icp) (iep-and-icp))
        (iep (make-array length :element-type (array-element-type sequence)
                                :initial-element initial-element))
        (icp (make-array length :element-type (array-element-type sequence)
                                :initial-element initial-element))
        (t (make-array length :element-type (array-element-type sequence)))))

(defmethod sequence:adjust-sequence
    ((sequence vector) length
     &rest args &key initial-element (initial-contents nil icp))
  (declare (ignore initial-element))
  (cond ((and (array-has-fill-pointer-p sequence)
              (>= (array-total-size sequence) length))
         (setf (fill-pointer sequence) length)
         (if icp (replace sequence initial-contents) sequence))
        ((= (length sequence) length)
         (if icp (replace sequence initial-contents) sequence))
        (t (apply #'adjust-array sequence length args))))

(defmethod sequence:make-sequence-iterator
    ((sequence vector) &key from-end start end)
  (core::make-vector-iterator sequence from-end start end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Derived functions
;;; Optional. Everything here works already if the core protocol is in place.
;;;
;;; IMPORTANT PROVISO: Because these functions are specified to work just like
;;; the CL ones (but with other sequences), Clasp takes the view that it is not
;;; actually required to call them at any point. That is, a call to CL:FIND with
;;; an extension sequence _may_ result in a call to SEQUENCE:FIND, but just as
;;; well may not. Since the semantics of these functions are defined entirely in
;;; terms of general iteration, no customization is actually required, and can
;;; only serve as an optimization - not an extension to the semantics.
;;;
;;; This is still in flux though. If you think this is a bad idea, contact a
;;; maintainer to talk.
;;;

(defgeneric sequence:emptyp (sequence)
  (:method ((sequence sequence)) (zerop (length sequence)))
  (:method ((sequence null)) t)
  (:method ((sequence cons)) nil))

(defgeneric sequence:count
    (item sequence &key from-end start end test test-not key)
  (:argument-precedence-order sequence item))
(defmethod sequence:count
    (item (sequence sequence) &key from-end (start 0) end test test-not key)
  (let ((counter 0))
    (core::with-tests (test test-not key)
      (core::do-general-subsequence (e sequence start end
                                       :from-end from-end :output counter)
        (when (compare item (key e)) (incf counter))))))

(defgeneric sequence:count-if (pred sequence &key from-end start end key)
  (:argument-precedence-order sequence pred))
(defmethod sequence:count-if
    (pred (sequence sequence) &key from-end (start 0) end key)
  (let ((counter 0) (pred (core:coerce-fdesignator pred)))
    (core::with-key (key)
      (core::do-general-subsequence (e sequence start end
                                       :from-end from-end :output counter)
        (when (funcall pred (key e)) (incf counter))))))

(defgeneric sequence:count-if-not (pred sequence &key from-end start end key)
  (:argument-precedence-order sequence pred))
(defmethod sequence:count-if-not
    (pred (sequence sequence) &key from-end (start 0) end key)
  (let ((counter 0) (pred (core:coerce-fdesignator pred)))
    (core::with-key (key)
      (core::do-general-subsequence (e sequence start end
                                       :from-end from-end :output counter)
        (unless (funcall pred (key e)) (incf counter))))))

(defgeneric sequence:find
    (item sequence &key from-end start end test test-not key))
(defmethod sequence:find
    (pred (sequence sequence) &key from-end (start 0) end key)
  (core::with-tests (test test-not key)
    (core::do-general-subsequence (e sequence start end :from-end from-end)
      (when (compare item (key elt)) (return elt)))))

(defgeneric sequence:find-if (pred sequence &key from-end start end key)
  (:argument-precedence-order sequence pred))
(defmethod sequence:find-if
    (pred (sequence sequence) &key from-end (start 0) end key)
  (let ((pred (core:coerce-fdesignator pred)))
    (core::with-key (key)
      (core::do-general-subsequence (e sequence start end
                                       :from-end from-end)
        (when (funcall pred (key elt)) (return elt))))))

(defgeneric sequence:find-if-not (pred sequence &key from-end start end key)
  (:argument-precedence-order sequence pred))
(defmethod sequence:find-if-not
    (pred (sequence sequence) &key from-end (start 0) end key)
  (let ((pred (core:coerce-fdesignator pred)))
    (core::with-key (key)
      (core::do-general-subsequence (e sequence start end
                                       :from-end from-end)
        (unless (funcall pred (key elt)) (return elt))))))

(defgeneric sequence:position
    (item sequence &key test test-not from-end (start 0) end key))
(defmethod sequence:position
    (item (sequence sequence) &key test test-not from-end (start 0) end key)
  (core::with-tests (test test-not key)
    (core::do-general-subsequence (elt sequence start end
                                       :from-end from-end :index index)
      (when (compare item (key elt)) (return index)))))

(defgeneric sequence:position-if
    (pred sequence &key from-end (start 0) end key))
(defmethod sequence:position-if
    (pred (sequence sequence) &key from-end (start 0) end key)
  (let ((pred (core:coerce-fdesignator pred)))
    (core::with-key (key)
      (core::do-general-subsequence (elt sequence start end
                                         :from-end from-end :index index)
        (when (funcall pred (key elt)) (return index))))))

(defgeneric sequence:position-if-not
    (pred sequence &key from-end (start 0) end key))
(defmethod sequence:position-if-not
    (pred (sequence sequence) &key from-end (start 0) end key)
  (let ((pred (core:coerce-fdesignator pred)))
    (core::with-key (key)
      (core::do-general-subsequence (elt sequence start end
                                         :from-end from-end :index index)
        (unless (funcall pred (key elt)) (return index))))))

(defgeneric sequence:subseq (sequence start &optional end))
(defmethod sequence:subseq ((sequence sequence) start &optional end)
  (let* ((end (or end (length sequence)))
         (length (- end start))
         (result (sequence:make-sequence-like sequence length)))
    (sequence:with-sequence-iterator (state limit from-end step endp elt)
        (sequence :start start :end end)
      (declare (ignore limit endp))
      (sequence:with-sequence-iterator
          (rstate rlimit rfrom-end rstep rendp relt rsetelt)
          (result)
        (declare (ignore rlimit rendp relt))
        (do ((i 0 (+ i 1)))
            ((>= i length) result)
          (funcall rsetelt (funcall elt sequence state) result rstate)
          (setq state (funcall step sequence state from-end))
          (setq rstate (funcall rstep result rstate rfrom-end)))))))

(defgeneric sequence:copy-seq (sequence))
(defmethod sequence:copy-seq ((sequence sequence))
  (sequence:subseq sequence 0))

(defgeneric sequence:fill (sequence item &key start end))
(defmethod sequence:fill ((sequence sequence) item &key (start 0) end)
  (core::do-general-subsequence (nil sequence start end
                                     :from-end from-end :setter setelt)
    (setelt item))
  sequence)

(defgeneric sequence:nsubstitute
    (new old sequence &key start end from-end test test-not count key)
  (:argument-precedence-order sequence new old))
(defmethod sequence:nsubstitute (new old (sequence sequence)
                                 &key (start 0)
                                   end from-end test test-not count key)
  (let ((c 0))
    (core::with-tests (test test-not key)
      (core::do-general-subsequence (e sequence :from-end from-end
                                                :setter setelt)
        (when (and count (>= c count)) (return))
        (when (compare old (key e)) (incf c) (setelt new)))))
  sequence)

(defgeneric sequence:nsubstitute-if
    (new pred sequence &key start end from-end count key)
  (:argument-precedence-order sequence new pred))
(defmethod sequence:nsubstitute-if (new pred (sequence sequence)
                                    &key (start 0) end from-end count key)
  (let ((c 0) (pred (core:coerce-fdesignator pred)))
    (core::with-key (key)
      (core::do-general-subsequence (e sequence :from-end from-end
                                                :setter setelt)
        (when (and count (>= c count)) (return))
        (when (funcall pred (key e)) (incf c) (setelt new)))))
  sequence)

(defgeneric sequence:nsubstitute-if=mpt
    (new pred sequence &key start end from-end count key)
  (:argument-precedence-order sequence new pred))
(defmethod sequence:nsubstitute-if-not (new pred (sequence sequence)
                                        &key (start 0) end from-end count key)
  (let ((c 0) (pred (core:coerce-fdesignator pred)))
    (core::with-key (key)
      (core::do-general-subsequence (e sequence :from-end from-end
                                                :setter setelt)
        (when (and count (>= c count)) (return))
        (unless (funcall pred (key e)) (incf c) (setelt new)))))
  sequence)

(defgeneric sequence:substitute
    (new old sequence &key start end from-end test test-not count key)
  (:argument-precedence-order sequence new old))
(defmethod sequence:substitute (new old (sequence sequence) &rest args &key
                                (start 0) end from-end test test-not count key)
  (declare (dynamic-extent args))
  (declare (ignore start end from-end test test-not count key))
  (apply #'sequence:nsubstitute new old (copy-seq sequence) args))

(defgeneric sequence:substitute-if
    (new predicate sequence &key start end from-end count key)
  (:argument-precedence-order sequence new predicate))
(defmethod sequence:substitute-if (new predicate (sequence sequence) &rest args
                                   &key (start 0) end from-end count key)
  (declare (dynamic-extent args))
  (declare (ignore start end from-end count key))
  (apply #'sequence:nsubstitute-if new predicate (copy-seq sequence) args))

(defgeneric sequence:substitute-if-not
    (new predicate sequence &key start end from-end count key)
  (:argument-precedence-order sequence new predicate))
(defmethod sequence:substitute-if-not
    (new predicate (sequence sequence)
     &rest args &key (start 0) end from-end count key)
  (declare (dynamic-extent args))
  (declare (ignore start end from-end count key))
  (apply #'sequence:nsubstitute-if-not new predicate (copy-seq sequence) args))

(defun %sequence-replace (sequence1 sequence2 start1 end1 start2 end2)
  (sequence:with-sequence-iterator
      (state1 limit1 from-end1 step1 endp1 elt1 setelt1)
      (sequence1 :start start1 :end end1)
    (declare (ignore elt1))
    (sequence:with-sequence-iterator (state2 limit2 from-end2 step2 endp2 elt2)
        (sequence2 :start start2 :end end2)
      (do ()
          ((or (funcall endp1 sequence1 state1 limit1 from-end1)
               (funcall endp2 sequence2 state2 limit2 from-end2))
           sequence1)
        (funcall setelt1 (funcall elt2 sequence2 state2) sequence1 state1)
        (setq state1 (funcall step1 sequence1 state1 from-end1))
        (setq state2 (funcall step2 sequence2 state2 from-end2))))))

(defgeneric sequence:replace
    (sequence1 sequence2 &key start1 end1 start2 end2)
  (:argument-precedence-order sequence2 sequence1))
(defmethod sequence:replace
    ((sequence1 sequence) (sequence2 sequence)
     &key (start1 0) end1 (start2 0) end2)
  (cond
    ((eq sequence1 sequence2)
     (let ((replaces (subseq sequence2 start2 end2)))
       (%sequence-replace sequence1 replaces start1 end1 0 nil)))
    (t (%sequence-replace sequence1 sequence2 start1 end1 start2 end2))))

(defgeneric sequence:nreverse (sequence))
(defmethod sequence:nreverse ((sequence sequence))
  ;; FIXME: this, in particular the :from-end iterator, will suck
  ;; mightily if the user defines a list-like structure.
  (let ((length (length sequence)))
    (sequence:with-sequence-iterator
        (state1 limit1 from-end1 step1 endp1 elt1 setelt1)
        (sequence :end (floor length 2))
      (sequence:with-sequence-iterator
          (state2 limit2 from-end2 step2 endp2 elt2 setelt2)
          (sequence :start (ceiling length 2) :from-end t)
        (declare (ignore limit2 endp2))
        (do ()
            ((funcall endp1 sequence state1 limit1 from-end1) sequence)
          (let ((x (funcall elt1 sequence state1))
                (y (funcall elt2 sequence state2)))
            (funcall setelt1 y sequence state1)
            (funcall setelt2 x sequence state2))
          (setq state1 (funcall step1 sequence state1 from-end1))
          (setq state2 (funcall step2 sequence state2 from-end2)))))))

(defgeneric sequence:reverse (sequence))
(defmethod sequence:reverse ((sequence sequence))
  (sequence:nreverse (copy-seq sequence)))

(defgeneric sequence:reduce
    (function sequence &key from-end start end initial-value)
  (:argument-precedence-order sequence function))
(defmethod sequence:reduce
    (function (sequence sequence) &key from-end (start 0) end key
                                    (initial-value nil ivp))
  (core::with-key (key)
    (sequence:with-sequence-iterator (state limit from-end step endp elt)
        (sequence :start start :end end :from-end from-end)
      (if (funcall endp sequence state limit from-end)
          (if ivp initial-value (funcall function))
          (do* ((state state (funcall step sequence state from-end))
                (value (cond
                         (ivp initial-value)
                         (t (prog1
                                (funcall key (funcall elt sequence state))
                              (setq state
                                    (funcall step sequence state from-end)))))))
               ((funcall endp sequence state limit from-end) value)
            (let ((e (key (funcall elt sequence state))))
              (if from-end
                  (setq value (funcall function e value))
                  (setq value (funcall function value e)))))))))

(defgeneric sequence:mismatch (sequence1 sequence2 &key from-end start1 end1
                               start2 end2 test test-not key))
(defmethod sequence:mismatch
    ((sequence1 sequence) (sequence2 sequence)
     &key from-end (start1 0) end1 (start2 0) end2 test test-not key)
  (core::with-tests (test test-not key)
    (sequence:with-sequence-iterator (state1 limit1 from-end1 step1 endp1 elt1)
        (sequence1 :start start1 :end end1 :from-end from-end)
      (sequence:with-sequence-iterator
          (state2 limit2 from-end2 step2 endp2 elt2)
          (sequence2 :start start2 :end end2 :from-end from-end)
        (if from-end
            (do ((result (or end1 (length sequence1)) (1- result))
                 (e1 (funcall endp1 sequence1 state1 limit1 from-end1)
                     (funcall endp1 sequence1 state1 limit1 from-end1))
                 (e2 (funcall endp2 sequence2 state2 limit2 from-end2)
                     (funcall endp2 sequence2 state2 limit2 from-end2)))
                ((or e1 e2) (if (and e1 e2) nil result))
              (let ((o1 (key (funcall elt1 sequence1 state1)))
                    (o2 (key (funcall elt2 sequence2 state2))))
                (unless (compare o1 o2) (return result))
                (setq state1 (funcall step1 sequence1 state1 from-end1))
                (setq state2 (funcall step2 sequence2 state2 from-end2))))
            (do ((result start1 (1+ result))
                 (e1 (funcall endp1 sequence1 state1 limit1 from-end1)
                     (funcall endp1 sequence1 state1 limit1 from-end1))
                 (e2 (funcall endp2 sequence2 state2 limit2 from-end2)
                     (funcall endp2 sequence2 state2 limit2 from-end2)))
                ((or e1 e2) (if (and e1 e2) nil result))
              (let ((o1 (key (funcall elt1 sequence1 state1)))
                    (o2 (key (funcall elt2 sequence2 state2))))
                (unless (compare o1 o2) (return result)))
              (setq state1 (funcall step1 sequence1 state1 from-end1))
              (setq state2 (funcall step2 sequence2 state2 from-end2))))))))

(defgeneric sequence:search (sequence1 sequence2 &key from-end start1 end1
                             start2 end2 test test-not key))
(defmethod sequence:search
    ((sequence1 sequence) (sequence2 sequence)
     &key from-end (start1 0) end1 (start2 0) end2 test test-not key)
  (core::search-generic sequence1 start1 (or end1 (length sequence1))
                        sequence2 start2 (or end2 (length sequence2))
                        test test-not key from-end))

(defgeneric sort (sequence predicate &key key))
;;; The SEQEUENCE paper says, in the section on relationships between the
;;; generic functions in this package,
;;; "the default method on SORT behaves as if it constructs a vector with the
;;;  same elements as sequence, calls SORT on that vector, then replaces the
;;;  elements of sequence with the elements of the sorted vector."
;;; I don't understand the rationale for this requirement - specifically, how
;;; could one possibly know that that's what's happening? SORT on a vector
;;; is nothing the user can specialize. If what it's supposed to mean is that
;;; the same sequence is returned, this implementation does fine there.
;;; Incidentally, it could be sped up if we could grab a sequence class's
;;;  particular ELT.
;;; Oh, and this will be terrible for a list-like sequence. Maybe the
;;; requirement is supposed to indicate that list-like sequences shouldn't get
;;; such terrible performance...?
(defmethod sequence:sort ((sequence sequence) predicate &key key)
  (core::with-key (key)
    (core::quick-sort sequence 0 (1- (length sequence))
                      (core:coerce-fdesignator predicate) key)))

(defgeneric sequence:stable-sort ((sequence sequence) predicate &key key)
  ;; FIXME: lazy. REPLACE will do checks it doesn't need to, etc.
  (core::with-key (key)
    (replace sequence (stable-sort (coerce sequence 'vector)
                                   predicate :key key))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clasp extensions
;;;

;;; This is like CL:MAKE-SEQUENCE, except the type is restricted to be
;;; either a class or a symbol naming one.
;;; This mostly only exists to make it possible to call  MAKE-SEQUENCE-LIKE
;;; from code way before class-prototype exists.
;;; Oh, and there's :initial-contents.
;;; FIXME: Add a compiler macro to fold it to make-sequence-like.

(defun sequence:make-sequence (type size
                               &rest args &key initial-element initial-contents)
  (declare (ignore initial-element initial-contents))
  (let* ((class (if (symbolp type) (find-class type) type))
         (proto (clos:class-prototype class)))
    (apply #'sequence:make-sequence-like type size args)))
