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
  (declare (ignore from-end))
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
  (:method ((sequence cons)) nil)
  (:method ((sequence t)) (core::error-not-a-sequence sequence)))

(defgeneric sequence:count
    (item sequence &key from-end start end test test-not key)
  (:argument-precedence-order sequence item)
  (:method (item (sequence t) &rest kwargs)
    (declare (ignore item kwargs))
    (core::error-not-a-sequence sequence)))
(defmethod sequence:count
    (item (sequence sequence) &key from-end (start 0) end test test-not key)
  (let ((counter 0))
    (core::with-tests (test test-not key)
      (core::do-general-subsequence (e sequence start end
                                       :from-end from-end :output counter)
        (when (core::compare item (key e)) (incf counter))))))

(defgeneric sequence:count-if (pred sequence &key from-end start end key)
  (:argument-precedence-order sequence pred)
  (:method (pred (sequence t) &rest kwargs)
    (declare (ignore pred kwargs))
    (core::error-not-a-sequence sequence)))
(defmethod sequence:count-if
    (pred (sequence sequence) &key from-end (start 0) end key)
  (let ((counter 0) (pred (core:coerce-fdesignator pred)))
    (core::with-key (key)
      (core::do-general-subsequence (e sequence start end
                                       :from-end from-end :output counter)
        (when (funcall pred (key e)) (incf counter))))))

(defgeneric sequence:count-if-not (pred sequence &key from-end start end key)
  (:argument-precedence-order sequence pred)
  (:method (pred (sequence t) &rest kwargs)
    (declare (ignore pred wargs))
    (core::error-not-a-sequence sequence)))
(defmethod sequence:count-if-not
    (pred (sequence sequence) &key from-end (start 0) end key)
  (let ((counter 0) (pred (core:coerce-fdesignator pred)))
    (core::with-key (key)
      (core::do-general-subsequence (e sequence start end
                                       :from-end from-end :output counter)
        (unless (funcall pred (key e)) (incf counter))))))

(defgeneric sequence:find
    (item sequence &key from-end start end test test-not key)
  (:argument-precedence-order sequence item)
  (:method (item (sequence t) &rest kwargs)
    (declare (ignore item kwargs))
    (core::error-not-a-sequence sequence)))
(defmethod sequence:find
    (item (sequence sequence) &key from-end (start 0) end test test-not key)
  (core::with-tests (test test-not key)
    (core::do-general-subsequence (e sequence start end :from-end from-end)
      (when (core::compare item (key e)) (return e)))))

(defgeneric sequence:find-if (pred sequence &key from-end start end key)
  (:argument-precedence-order sequence pred)
  (:method (pred (sequence t) &rest kwargs)
    (core::error-not-a-sequence sequence)))
(defmethod sequence:find-if
    (pred (sequence sequence) &key from-end (start 0) end key)
  (let ((pred (core:coerce-fdesignator pred)))
    (core::with-key (key)
      (core::do-general-subsequence (e sequence start end
                                       :from-end from-end)
        (when (funcall pred (key e)) (return e))))))

(defgeneric sequence:find-if-not (pred sequence &key from-end start end key)
  (:argument-precedence-order sequence pred)
  (:method (pred (sequence t) &rest kwargs)
    (declare (ignore pred kwargs))
    (core::error-not-a-sequence sequence)))
(defmethod sequence:find-if-not
    (pred (sequence sequence) &key from-end (start 0) end key)
  (let ((pred (core:coerce-fdesignator pred)))
    (core::with-key (key)
      (core::do-general-subsequence (e sequence start end
                                       :from-end from-end)
        (unless (funcall pred (key e)) (return e))))))

(defgeneric sequence:position
    (item sequence &key test test-not from-end (start 0) end key)
  (:argument-precedence-order sequence item)
  (:method (item (sequence t) &rest kwargs)
    (declare (ignore item kwargs))
    (core::error-not-a-sequence sequence)))
(defmethod sequence:position
    (item (sequence sequence) &key test test-not from-end (start 0) end key)
  (core::with-tests (test test-not key)
    (core::do-general-subsequence (elt sequence start end
                                       :from-end from-end :index index)
      (when (core::compare item (key elt)) (return index)))))

(defgeneric sequence:position-if
    (pred sequence &key from-end (start 0) end key)
  (:argument-precedence-order sequence pred)
  (:method (pred (sequence t) &rest kwargs)
    (declare (ignore pred kwargs))
    (core::error-not-a-sequence sequence)))
(defmethod sequence:position-if
    (pred (sequence sequence) &key from-end (start 0) end key)
  (let ((pred (core:coerce-fdesignator pred)))
    (core::with-key (key)
      (core::do-general-subsequence (elt sequence start end
                                         :from-end from-end :index index)
        (when (funcall pred (key elt)) (return index))))))

(defgeneric sequence:position-if-not
    (pred sequence &key from-end (start 0) end key)
  (:argument-precedence-order sequence pred)
  (:method (pred (sequence t) &rest kwargs)
    (declare (ignore pred kwargs))
    (core::error-not-a-sequence sequence)))
(defmethod sequence:position-if-not
    (pred (sequence sequence) &key from-end (start 0) end key)
  (let ((pred (core:coerce-fdesignator pred)))
    (core::with-key (key)
      (core::do-general-subsequence (elt sequence start end
                                         :from-end from-end :index index)
        (unless (funcall pred (key elt)) (return index))))))

(defgeneric sequence:subseq (sequence start &optional end)
  (:method ((sequence t) start &optional end)
    (declare (ignore start end))
    (core::error-not-a-sequence sequence)))
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

(defgeneric sequence:copy-seq (sequence)
  (:method ((sequence t)) (core::error-not-a-sequence sequence)))
(defmethod sequence:copy-seq ((sequence sequence))
  (sequence:subseq sequence 0))

(defgeneric sequence:fill (sequence item &key start end)
  (:method ((sequence t) item &rest kwargs)
    (declare (ignore item kwargs))
    (core::error-not-a-sequence sequence)))
(defmethod sequence:fill ((sequence sequence) item &key (start 0) end)
  (core::do-general-subsequence (nil sequence start end :setter setelt)
    (setelt item))
  sequence)

(defgeneric sequence:nsubstitute
    (new old sequence &key start end from-end test test-not count key)
  (:argument-precedence-order sequence new old)
  (:method (new old (sequence t) &rest kwargs)
    (declare (ignore new old kwargs))
    (core::error-not-a-sequence sequence)))
(defmethod sequence:nsubstitute (new old (sequence sequence)
                                 &key (start 0)
                                   end from-end test test-not count key)
  (let ((c 0))
    (core::with-tests (test test-not key)
      (core::do-general-subsequence (e sequence :from-end from-end
                                                :setter setelt)
        (when (and count (>= c count)) (return))
        (when (core::compare old (key e)) (incf c) (setelt new)))))
  sequence)

(defgeneric sequence:nsubstitute-if
    (new pred sequence &key start end from-end count key)
  (:argument-precedence-order sequence new pred)
  (:method (new pred (sequence t) &rest kwargs)
    (declare (ignore new pred kwargs))
    (core::error-not-a-sequence sequence)))
(defmethod sequence:nsubstitute-if (new pred (sequence sequence)
                                    &key (start 0) end from-end count key)
  (let ((c 0) (pred (core:coerce-fdesignator pred)))
    (core::with-key (key)
      (core::do-general-subsequence (e sequence :from-end from-end
                                                :setter setelt)
        (when (and count (>= c count)) (return))
        (when (funcall pred (key e)) (incf c) (setelt new)))))
  sequence)

(defgeneric sequence:nsubstitute-if-not
    (new pred sequence &key start end from-end count key)
  (:argument-precedence-order sequence new pred)
  (:method (new pred (sequence t) &rest kwargs)
    (declare (ignore new pred kwargs))
    (core::error-not-a-sequence sequence)))
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
  (:argument-precedence-order sequence new old)
  (:method (new old (sequence t) &rest kwargs)
    (declare (ignore new old kwargs))
    (core::error-not-a-sequence sequence)))
(defmethod sequence:substitute (new old (sequence sequence) &rest args &key
                                (start 0) end from-end test test-not count key)
  (declare (dynamic-extent args))
  (declare (ignore start end from-end test test-not count key))
  (apply #'sequence:nsubstitute new old (copy-seq sequence) args))

(defgeneric sequence:substitute-if
    (new predicate sequence &key start end from-end count key)
  (:argument-precedence-order sequence new predicate)
  (:method (new pred (sequence t) &rest kwargs)
    (declare (ignore new pred kwargs))
    (core::error-not-a-sequence sequence)))
(defmethod sequence:substitute-if (new predicate (sequence sequence) &rest args
                                   &key (start 0) end from-end count key)
  (declare (dynamic-extent args))
  (declare (ignore start end from-end count key))
  (apply #'sequence:nsubstitute-if new predicate (copy-seq sequence) args))

(defgeneric sequence:substitute-if-not
    (new predicate sequence &key start end from-end count key)
  (:argument-precedence-order sequence new predicate)
  (:method (new pred (sequence t) &rest kwargs)
    (declare (ignore new pred kwargs))
    (core::error-not-a-sequence sequence)))
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

(defgeneric sequence:nreverse (sequence)
  (:method ((sequence t)) (core::error-not-a-sequence sequence)))
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

(defgeneric sequence:reverse (sequence)
  (:method ((sequence t)) (core::error-not-a-sequence sequence)))
(defmethod sequence:reverse ((sequence sequence))
  (sequence:nreverse (copy-seq sequence)))

(defgeneric sequence:reduce
    (function sequence &key from-end start end initial-value)
  (:argument-precedence-order sequence function)
  (:method (function (sequence t) &rest kwargs)
    (declare (ignore function kwargs))
    (core::error-not-a-sequence sequence)))
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

(defgeneric sequence:mismatch (sequence1 sequence2
                               &key from-end start1 end1
                                 start2 end2 test test-not key)
  (:method ((sequence1 t) (sequence2 t) &rest kwargs)
    (declare (ignore kwargs))
    (core::error-not-a-sequence sequence1))
  (:method ((sequence1 sequence) (sequence2 t) &rest kwargs)
    (declare (ignore kwargs))
    (core::error-not-a-sequence sequence2)))
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
                (unless (core::compare o1 o2) (return result))
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
                (unless (core::compare o1 o2) (return result)))
              (setq state1 (funcall step1 sequence1 state1 from-end1))
              (setq state2 (funcall step2 sequence2 state2 from-end2))))))))

(defgeneric sequence:search (sequence1 sequence2
                             &key from-end start1 end1
                               start2 end2 test test-not key)
  (:method ((sequence1 t) (sequence2 t) &rest kwargs)
    (declare (ignore kwargs))
    (core::error-not-a-sequence sequence1))
  (:method ((sequence1 sequence) (sequence2 t) &rest kwargs)
    (declare (ignore kwargs))
    (core::error-not-a-sequence sequence2)))
(defmethod sequence:search
    ((sequence1 sequence) (sequence2 sequence)
     &key from-end (start1 0) end1 (start2 0) end2 test test-not key)
  (core::search-generic sequence1 start1 (or end1 (length sequence1))
                        sequence2 start2 (or end2 (length sequence2))
                        test test-not key from-end))

;;; shared body of delete(-if(-not)) and not delete-duplicates yet R.I.P.
(defmacro delete-macro (test-form &optional countp)
  `(let* (;; number of elements we've deleted.
          ,@(when countp '((c 0)))
          ;; length, but we only need it with from-end.
          (len (and from-end (length sequence)))
          (end (or end len))
          ;; function to step an index. NOTE: Could restrict to integers
          (stepi (if from-end #'1- #'1+)))
     (sequence:with-sequence-iterator (dst limit from-end step endp elt setelt nil copy)
         (sequence :start start :end (if from-end end nil) :from-end from-end)
       ;; We maintain two iterators, a source and a dest. They start at the start
       ;; (or for from-end, the end).
       ;; src is always at dst or ahead of (behind) it, so we only check it for endp.
       ;; Every iteration, if we haven't yet deleted the maximum number of elements,
       ;; or hit end (start), we check if the current element satisfies the test.
       ;; If it does, we just increase the count of deleted elements.
       ;; If it doesn't, or we aren't doing any more deletions, we copy it to dst
       ;; and step dst. (And src steps every iteration either way.)
       ;; For forward iteration, when we hit end, we just keep copying the entire
       ;; rest of the sequence. Once src hits the end, everything before dst is the
       ;; sequence we want and after that is trash, so we cut back with adjust-sequence.
       ;; For reverse, we stop when we hit start, then copy forward from dst to src
       ;; using replace. This means we loop over the portion of interest twice.
       (do* ((src (funcall copy sequence dst)
                  (funcall step sequence src from-end))
             (srci (if from-end end start) (funcall stepi srci))
             (dsti srci))
            ((or (funcall endp sequence src limit from-end)
                 (and from-end (> start srci)))
             (when from-end
               (replace sequence sequence :start1 srci :start2 dsti))
             (sequence:adjust-sequence
              sequence (if from-end (+ start (- len dsti)) dsti)))
         (let ((e (funcall elt sequence src)))
           (cond ((and ,@(when countp '((or (not count) (< c count))))
                       (or from-end (not end) (< srci end))
                       ,test-form)
                  ;; delete (by not copying)
                  ,@(when countp '((incf c))))
                 (t ; keep
                  (funcall setelt e sequence dst)
                  (setq dsti (funcall stepi dsti))
                  (setq dst (funcall step sequence dst from-end)))))))))

(defgeneric sequence:delete (item sequence
                             &key from-end test test-not start end count key)
  (:argument-precedence-order sequence item)
  (:method (item (sequence t) &rest kwargs)
    (declare (ignore item kwargs))
    (core::error-not-a-sequence sequence)))
(defmethod sequence:delete (item (sequence sequence)
                            &key from-end (start 0) end
                              test test-not count key)
  (core::with-tests (test test-not key)
    (delete-macro (core::compare item (key e)) t)))

(defgeneric sequence:delete-if (predicate sequence
                                &key start end from-end count key)
  (:argument-precedence-order sequence predicate)
  (:method (pred (sequence t) &rest kwargs)
    (declare (ignore pred kwargs))
    (core::error-not-a-sequence sequence)))
(defmethod sequence:delete-if (predicate (sequence sequence)
                               &key (start 0) end from-end count key)
  (core::with-key (key)
    (let ((predicate (core:coerce-fdesignator predicate)))
      (delete-macro (funcall predicate (key e)) t))))

(defgeneric sequence:delete-if-not (predicate sequence
                                    &key start end from-end count key)
  (:argument-precedence-order sequence predicate)
  (:method (pred (sequence t) &rest kwargs)
    (declare (ignore pred kwargs))
    (core::error-not-a-sequence sequence)))
(defmethod sequence:delete-if-not (predicate (sequence sequence)
                                   &key (start 0) end from-end count key)
  (core::with-key (key)
    (let ((predicate (core:coerce-fdesignator predicate)))
      (delete-macro (not (funcall predicate (key e))) t))))

(defgeneric sequence:remove
    (item sequence &key from-end test test-not start end count key)
  (:argument-precedence-order sequence item)
  (:method (item (sequence t) &rest kwargs)
    (declare (ignore item kwargs))
    (core::error-not-a-sequence sequence)))
(defmethod sequence:remove (item (sequence sequence) &rest args &key
                            from-end test test-not (start 0) end count key)
  (declare (dynamic-extent args))
  (declare (ignore from-end test test-not start end count key))
  (apply #'sequence:delete item (copy-seq sequence) args))

(defgeneric sequence:remove-if
    (predicate sequence &key from-end start end count key)
  (:argument-precedence-order sequence predicate)
  (:method (pred (sequence t) &rest kwargs)
    (declare (ignore pred kwargs))
    (core::error-not-a-sequence sequence)))
(defmethod sequence:remove-if (predicate (sequence sequence) &rest args &key
                               from-end (start 0) end count key)
  (declare (dynamic-extent args))
  (declare (ignore from-end start end count key))
  (apply #'sequence:delete-if predicate (copy-seq sequence) args))

(defgeneric sequence:remove-if-not
    (predicate sequence &key from-end start end count key)
  (:argument-precedence-order sequence predicate)
  (:method (pred (sequence t) &rest kwargs)
    (declare (ignore pred kwargs))
    (core::error-not-a-sequence sequence)))
(defmethod sequence:remove-if-not (predicate (sequence sequence) &rest args
                                   &key from-end (start 0) end count key)
  (declare (dynamic-extent args))
  (declare (ignore from-end start end count key))
  (apply #'sequence:delete-if-not predicate (copy-seq sequence) args))

(defgeneric sequence:delete-duplicates
    (sequence &key from-end test test-not start end key)
  (:method ((sequence t) &rest kwargs)
    (declare (ignore kwargs))
    (core::error-not-a-sequence sequence)))
(defmethod sequence:delete-duplicates
    ((sequence sequence) &key from-end test test-not (start 0) end key)
  (core::with-tests (test test-not key)
    (let ((c 0))
      (sequence:with-sequence-iterator
          (state1 limit from-end step endp elt setelt nil copy)
          (sequence :start start :end end :from-end from-end)
        (let ((state2 (funcall copy sequence state1)))
          (flet ((finish ()
                   (if from-end
                       (replace sequence sequence
                                :start1 start :end1 (- (length sequence) c)
                                :start2 (+ start c) :end2 (length sequence))
                       (unless (or (null end) (= end (length sequence)))
                         (replace sequence sequence
                                  :start2 end :start1 (- end c)
                                  :end1 (- (length sequence) c))))
                   (sequence:adjust-sequence sequence (- (length sequence) c))))
            (declare (dynamic-extent #'finish))
            (do ((end (or end (length sequence)))
                 (s 0 (1+ s)))
                ((funcall endp sequence state2 limit from-end) (finish))
              (let ((e (funcall elt sequence state2)))
                (loop
                  ;; Does the element exist in the previous sequence?
                  (if (position (key e) sequence
                                :test test :test-not test-not :key key
                                :start (if from-end start (+ start s 1))
                                :end (if from-end (- end s 1) end))
                      (progn
                        (incf c)
                        (incf s)
                        (setq state2 (funcall step sequence state2 from-end))
                        (when (funcall endp sequence state2 limit from-end)
                          (return-from sequence:delete-duplicates (finish)))
                        (setq e (funcall elt sequence state2)))
                      (return)))
                (funcall setelt e sequence state1))
              (setq state1 (funcall step sequence state1 from-end))
              (setq state2 (funcall step sequence state2 from-end)))))))))

(defgeneric sequence:remove-duplicates
    (sequence &key from-end test test-not start end key)
  (:method ((sequence t) &rest kwargs)
    (declare (ignore kwargs))
    (core::error-not-a-sequence sequence)))
(defmethod sequence:remove-duplicates
    ((sequence sequence) &rest args &key from-end test test-not (start 0) end key)
  (declare (dynamic-extent args))
  (declare (ignore from-end test test-not start end key))
  (apply #'sequence:delete-duplicates (copy-seq sequence) args))

(defgeneric sequence:sort (sequence predicate &key key)
  (:method ((sequence t) predicate &rest kwargs)
    (declare (ignore predicate kwargs))
    (core::error-not-a-sequence sequence)))
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

(defgeneric sequence:stable-sort (sequence predicate &key key)
  (:method ((sequence t) predicate &rest kwargs)
    (declare (ignore predicate kwargs))
    (core::error-not-a-sequence sequence)))
(defmethod sequence:stable-sort ((sequence sequence) predicate &key key)
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
    (apply #'sequence:make-sequence-like proto size args)))

;;; This is a convenience macro to define some protocol methods for sequences
;;; that can be dealt with most efficiently by iterating over them. CL:LIST is
;;; an example of this kind of sequence.
;;; The macro defines length, elt, and (setf elt).
;;; make-sequence-like, adjust-sequence, and the iteration protocol must be
;;; defined separately.
;;; The extender provides the class name, four operators, and optionally a fifth
;;; operator that defaults to CL:IDENTITY. These operators are as follows:
;;; (GET iterator) returns the element at that iterator
;;; (SET iterator value) sets the element at that iterator. Return value ignored
;;; (NEXT iterator) returns the iterator for the next iteration
;;; (ENDP iterator) returns true iff the iteration is complete
;;; (MAKE-ITERATOR sequence) returns the iteration object for the above
;;; Example usage:
;;; (sequence:define-iterative-sequence list car rplaca cdr null)

(defmacro sequence:define-iterative-sequence
    (class-name get set next endp
     &optional (make-iterator 'identity))
  `(progn
     (defmethod sequence:length ((sequence ,class-name))
       (do ((it (,make-iterator sequence) (,next it))
            (len 0 (1+ len)))
           ((,endp it) len)))
     (defmethod sequence:elt ((sequence ,class-name) index)
       (do ((it (,make-iterator sequence) (,next it))
            (i 0 (1+ i)))
           ((,endp it)
            (error "index ~d out of bounds" index))
         (when (= i index)
           (return (,get it)))))
     (defmethod (setf sequence:elt)
         (new (sequence ,class-name) index)
       (do ((it (,make-iterator sequence) (,next it))
            (i 0 (1+ i)))
           ((,endp it)
            (error "index ~d out of bounds" index))
         (when (= i index)
           (,set it new)
           (return new))))))

;;; This is a convenience macro to define some protocol methods for sequences
;;; that can efficiently be "like vectors": i.e. random-access and length are
;;; cheap, and lengths and indices are fixnums.
;;; The extender provides the name of length and access operators;
;;; #'access and #'(setf access) must be defined.
;;; The macro defines length, elt, (setf elt), and make-sequence-iterator.
;;; make-sequence-like and adjust-sequence must be defined separately.
;;; Example usage:
;;; (sequence:define-random-access-sequence vector length aref)

(defmacro sequence:define-random-access-sequence
    (class-name length elt)
  (core::with-unique-names (sequence)
    `(progn
       (defmethod sequence:length ((sequence ,class-name))
         (,length sequence))
       (defmethod sequence:elt ((sequence ,class-name) index)
         (,elt sequence index))
       (defmethod (setf sequence:elt)
           (new (sequence ,class-name) index)
         (setf (,elt sequence index) new))
       (defmethod sequence:make-sequence-iterator
           ((,sequence ,class-name) &key from-end (start 0) end)
         (let ((end (or end (,length ,sequence))))
           (sequence:make-random-access-iterator
            start end from-end #',elt #'(setf ,elt)))))))

;;; END is already normalized (i.e. a fixnum, not NIL)
(defun sequence:make-random-access-iterator
    (start end from-end elt setelt)
  (values (if from-end (1- end) start)
          (if from-end (1- start) end)
          from-end
          (if from-end
              #'core::random-access-iterator-prev
              #'core::random-access-iterator-next)
          #'core::random-access-iterator-endp
          elt setelt
          #'core::random-access-iterator-index
          #'core::random-access-iterator-copy))

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

(sequence:define-iterative-sequence list car rplaca cdr null)

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
