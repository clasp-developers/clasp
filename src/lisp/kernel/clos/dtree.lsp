(in-package "CLOS")

;;; Outcomes

(defstruct (outcome (:type vector) :named))
(defstruct (optimized-slot-reader (:type vector) (:include outcome) :named)
  index slot-name method class)
(defstruct (optimized-slot-writer (:type vector) (:include outcome) :named)
  index slot-name method class)
(defstruct (fast-method-call (:type vector) (:include outcome) :named) function)
;; see closfastgf.lsp's find-existing-emf for use of applicable-methods slot
(defstruct (effective-method-outcome (:type vector) (:include outcome) :named)
  applicable-methods (form nil) (function nil))

(defun outcome= (outcome1 outcome2)
  (or (eq outcome1 outcome2) ; covers effective-method-outcome due to closfastgf caching
      (cond ((optimized-slot-reader-p outcome1)
             (and (optimized-slot-reader-p outcome2)
                  ;; could also do class slot locations somehow,
                  ;; but it doesn't seem like a big priority.
                  (fixnump (optimized-slot-reader-index outcome1))
                  (fixnump (optimized-slot-reader-index outcome2))
                  (= (optimized-slot-reader-index outcome1)
                     (optimized-slot-reader-index outcome2))))
            ((optimized-slot-writer-p outcome1)
             (and (optimized-slot-writer-p outcome2)
                  (fixnump (optimized-slot-writer-index outcome1))
                  (fixnump (optimized-slot-writer-index outcome2))
                  (= (optimized-slot-writer-index outcome1)
                     (optimized-slot-writer-index outcome2))))
            ((fast-method-call-p outcome1)
             (and (fast-method-call-p outcome2)
                  (eq (fast-method-call-function outcome1)
                      (fast-method-call-function outcome2))))
            (t nil))))

;;; Misc

(defun eql-specializer-p (spec) (consp spec))
(defun eql-specializer-value (spec) (car spec))

(defun insert-sorted (item lst &optional (test #'<) (key #'identity))
  (if (null lst)
      (list item)
      (let* ((firstp (funcall test (funcall key item) (funcall key (car lst))))
             (sorted (if firstp
                         (cons item lst) 
                         (cons (car lst) (insert-sorted item (cdr lst) test key)))))
        sorted)))

;;; Building an abstract "basic" tree - no tag tests or anything
;;; NOTE: We could probably store this instead of the call history

(defstruct (test (:type vector) :named) (paths nil))
(defstruct (skip (:type vector) :named) next)

;; note: if we used actual specializers, this could just be eq.
(defun specializer= (s1 s2)
  (if (eql-specializer-p s1)
      (and (eql-specializer-p s2)
           (eql (eql-specializer-value s1) (eql-specializer-value s2)))
      ;; for classes:
      (eq s1 s2)))

;;; Make a new subtree with only one path, starting with the ith specializer.
(defun remaining-subtree (specializers outcome sprofile speclength i)
  (cond ((= i speclength) outcome)
        ((svref sprofile i) ; specialized
         (make-test
          :paths
          (acons (svref specializers i)
                 (remaining-subtree specializers outcome sprofile speclength (1+ i))
                 nil)))
        (t
         (make-skip
          :next (remaining-subtree specializers outcome sprofile speclength (1+ i))))))

;;; Adds a call history entry to a tree, avoiding new nodes as much as possible.
(defun add-entry (node specializers outcome sprofile speclength i)
  (unless (= i speclength)
    (cond
      ((outcome-p node)
       ;; If we're here, we don't have anything to add.
       (error "BUG in ADD-ENTRY: Redundant call history entry: ~a"
              (cons specializers outcome)))
      ((skip-p node)
       (add-entry (skip-next node) specializers outcome sprofile speclength (1+ i)))
      ((test-p node)
       (let* ((spec (svref specializers i))
              (pair (assoc spec (test-paths node)
                           :test #'specializer=)))
         (if pair
             ;; our entry is so far identical to an existing one;
             ;; continue the search.
             (add-entry (cdr pair) specializers outcome sprofile speclength (1+ i))
             ;; We have something new. Add it and we're done.
             (setf (test-paths node)
                   (acons spec
                          (remaining-subtree
                           specializers outcome sprofile speclength (1+ i))
                          (test-paths node))))))
      (t
       (error "BUG in ADD-ENTRY: Not a node: ~a" node)))))

(defun basic-tree (call-history specializer-profile)
  (assert (not (null call-history)))
  (let ((last-specialized (position nil specializer-profile :from-end t :test-not #'eq))
        (first-specialized (position t specializer-profile)))
    (when (null last-specialized)
      ;; no specialization - we go immediately to the outcome
      ;; (we could assert all outcomes are identical)
      (return-from basic-tree (cdr (first call-history))))
    ;; usual case
    (loop with result = (make-test)
          with specialized-length = (1+ last-specialized)
          for (specializers . outcome) in call-history
          do (add-entry result specializers outcome specializer-profile specialized-length
                        first-specialized)
          finally (return (loop repeat first-specialized
                                do (setf result (make-skip :next result))
                                finally (return result))))))

;;; Compiling a basic tree into concrete tests and sundry.
;;; The node/possibly DAG here basically forms a slightly weird VM defined as follows.
;;; There are two registers, ARG and STAMP. Each node performs an action and then branches.
;;; (WHERE is also an even more implicit register-
;;;  used by WHERE-BRANCH and COMPLEX-STAMP-READ.)
;;;
;;; ADVANCE: assign ARG = get next arg. unconditional jump to NEXT.
;;; TAG-TEST: check the tag of ARG. If it's one of the discriminatable tags, jump to the
;;;           tag-th entry of the tag-test's vector. Otherwise, jump to the default.
;;; WHERE-BRANCH: check where of STAMP (a header stamp). If it indicates C++ object,
;;;               jump to C++. Otherwise jump to OTHER.
;;;               N.b. this could be expanded into a multi-way branch.
;;; HEADER-STAMP-READ: assign STAMP = header stamp of ARG. unconditional jump to NEXT.
;;; COMPLEX-STAMP-READ: assign STAMP = complex stamp of ARG. unconditional jump to NEXT.
;;; <-BRANCH: If STAMP < PIVOT (a constant), jump to LEFT. Otherwise jump to RIGHT.
;;; =-CHECK: If STAMP = PIVOT (a constant), jump to NEXT. Otherwise jump to miss.
;;; RANGE-CHECK: If MIN <= STAMP <= MAX (constants), jump to NEXT, otherwise miss.
;;; EQL-SEARCH: If ARG eqls the nth object of OBJECTS, jump the nth entry of NEXTS.
;;;             If it eqls none of the OBJECTS, jump to DEFAULT.
;;; MISS: unconditional jump to dispatch-miss routine.

(defstruct (advance (:type vector) :named) next)
(defstruct (tag-test (:type vector) :named) tags default)
(defstruct (where-branch (:type vector) :named) c++ other)
(defstruct (header-stamp-read (:type vector) :named) next)
(defstruct (complex-stamp-read (:type vector) :named) next)
(defstruct (<-branch (:type vector) :named) pivot left right)
(defstruct (=-check (:type vector) :named) pivot next)
(defstruct (range-check (:type vector) :named) min max next)
(defstruct (eql-search (:type vector) :named) objects nexts default)
(defstruct (miss (:type vector) :named))

(defun compile-tree (tree)
  (cond ((outcome-p tree) tree)
        ((skip-p tree) (make-advance :next (compile-tree (skip-next tree))))
        ((test-p tree) (compile-test tree))))

(defun compile-test (test)
  (multiple-value-bind (eqls tags c++-classes other-classes)
      (differentiate-specializers (test-paths test))
    (let* (;; Convert the classes into ranges.
           (c++-ranges (classes-to-ranges c++-classes))
           (other-ranges (classes-to-ranges other-classes))
           ;; Build our tests, in reverse order so they can refer to their successors.
           (other-search
            (if other-classes
                (make-complex-stamp-read :next (compile-ranges other-ranges))
                (make-miss)))
           (c++-search (compile-ranges c++-ranges)) ; header read already
           (where-test
             (if (and (miss-p c++-search) (miss-p other-search))
                 c++-search ; don't bother with a where, miss immediately
                 (make-header-stamp-read
                  :next (make-where-branch :c++ c++-search :other other-search))))
           (tag-test
             (if (and (miss-p where-test)
                      (every #'null tags))
                 where-test ; miss immediately
                 (compile-tag-test tags where-test))))
      (make-advance
       ;; we do EQL tests before anything else. they could be moved later if we altered
       ;; when eql tests are stored in the call history, i think.
       :next (cond ((null eqls)
                    ;; we shouldn't have any empty tests - sanity check this
                    (assert (not (miss-p tag-test)))
                    tag-test)
                   (t (compile-eql-search eqls tag-test)))))))

(defun differentiate-specializers (paths)
  (loop with eqls = nil
        with tags-vector = (tags-vector)
        with c++-classes = nil
        with other-classes = nil
        for pair in paths
        for spec = (car pair)
        do (cond ((eql-specializer-p spec) (push pair eqls))
                 ((tag-spec-p spec)
                  (setf (svref tags-vector (class-tag spec)) (cdr pair)))
                 ((< (core:class-stamp-for-instances spec) cmp:+c++-stamp-max+)
                  (setf c++-classes
                        (insert-sorted pair c++-classes #'< #'path-pair-key)))
                 (t
                  (setf other-classes
                        (insert-sorted pair other-classes #'< #'path-pair-key))))
        finally (return (values eqls tags-vector c++-classes other-classes))))

(defun path-pair-key (pair) (core:class-stamp-for-instances (car pair)))

;;; tag tests

(defvar *tag-tests* (llvm-sys:tag-tests)) ; a list of (:foo-tag pseudostamp tag)
(defun tags-vector () (make-array (length *tag-tests*) :initial-element nil))

(defun tag-spec-p (class) ; is CLASS one that's manifested as a tag test?
  (member (core:class-stamp-for-instances class) *tag-tests* :key #'second))
(defun class-tag (class) ; what tag corresponds to CLASS?
  (third (find (core:class-stamp-for-instances class) *tag-tests* :key #'second)))

(defun compile-tag-test (tags where-test)
  (map-into tags (lambda (ex) (if (null ex) (make-miss) (compile-tree ex))) tags)
  (make-tag-test :tags tags :default where-test))

;;; class tests

;; return whether the two NEXT nodes can be conflated.
;; note: at the moment, non-outcomes are probably never equal
(defun next= (next1 next2)
  (if (outcome-p next1)
      (and (outcome-p next2) (outcome= next1 next2))
      (eq next1 next2)))

;; Given (class . next-node) pairs, return ((low . high) . next-node) pairs,
;; where low and high are an inclusive range of stamps.
;; Classes must be presorted by stamp.
(defun classes-to-ranges (pairs)
  (flet ((fresh (stamp next) (cons (cons stamp stamp) next)))
    (if (null pairs)
        pairs
        (loop with current = (fresh (core:class-stamp-for-instances (car (first pairs)))
                                    (cdr (first pairs)))
              with result = (list current)
              for ((class . next) . more) on (rest pairs)
              for stamp = (core:class-stamp-for-instances class)
              if (and (= (1+ (cdar current)) stamp)
                      (next= (cdr current) next))
                do (setf (cdar current) stamp)
              else do (push (setf current (fresh stamp next)) result)
              finally (return (nreverse result))))))

;; given a list of ranges, return a binary search tree.
(defun compile-ranges (ranges)
  (cond
    ((null ranges) (make-miss))
    ((null (rest ranges))
     (let* ((match (first ranges))
            (next (compile-tree (cdr match))))
       (if (= (caar match) (cdar match))
           ;; unit range
           (make-=-check :pivot (caar match) :next next)
           ;; actual range
           (make-range-check :min (caar match) :max (cdar match) :next next))))
    (t
     (let* ((len-div-2 (floor (length ranges) 2))
            (left-matches (subseq ranges 0 len-div-2))
            (right-matches (subseq ranges len-div-2))
            (right-head (first right-matches))
            (right-stamp (caar right-head)))
       (make-<-branch :pivot right-stamp
                      :left (compile-ranges left-matches)
                      :right (compile-ranges right-matches))))))

;;; eql tests

(defun compile-eql-search (eqls next)
  (make-eql-search :objects (map 'simple-vector #'caar eqls)
                   :nexts (map 'simple-vector (lambda (pair)
                                                (compile-tree (cdr pair)))
                               eqls)
                   :default next))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Linearization
;;;

(defun linearize (tree)
  (cond
    ((outcome-p tree) (linearize-outcome tree))
    ((advance-p tree) (cons 'advance (linearize (advance-next tree))))
    ((tag-test-p tree)
     (let* ((tags (tag-test-tags tree))
            (tag-labels (loop repeat (length tags) collect (gensym "TAG"))))
       (append
        '(tag-test)
        (mapcar (lambda (label) `(go ,label)) tag-labels)
        (linearize (tag-test-default tree))
        (loop for tag across tags
              for label in tag-labels
              collect `(label ,label)
              nconc (linearize tag)))))
    ((header-stamp-read-p tree)
     (cons 'header-stamp-read (linearize (header-stamp-read-next tree))))
    ((where-branch-p tree)
     (let ((c++-label (gensym "C++")))
       (append
        '(where-branch)
        `((go ,c++-label))
        (linearize (where-branch-other tree))
        `((label ,c++-label))
        (linearize (where-branch-c++ tree)))))
    ((complex-stamp-read-p tree)
     (cons 'complex-stamp-read (linearize (complex-stamp-read-next tree))))
    ((<-branch-p tree)
     (let ((left-label (gensym "LEFT")))
       (append `(<-branch ,(<-branch-pivot tree))
               `((go ,left-label))
               (linearize (<-branch-right tree))
               `((label ,left-label))
               (linearize (<-branch-left tree)))))
    ((=-check-p tree)
     (list* '=-check
            (=-check-pivot tree)
            (linearize (=-check-next tree))))
    ((range-check-p tree)
     (list* 'range-check
            (range-check-min tree)
            (range-check-max tree)
            (linearize (range-check-next tree))))
    ((eql-search-p tree)
     (let (bodies labels)
       (nconc
        (loop for object across (eql-search-objects tree)
              for next across (eql-search-nexts tree)
              for label = (gensym "EQL")
              do (push label labels)
              do (push (linearize next) bodies)
              collect 'eql
              collect object
              collect `(go ,label))
        (linearize (eql-search-default tree))
        (loop for label in labels
              for body in bodies
              collect `(label ,label)
              nconc body))))
    ((miss-p tree) (list 'miss))))

(defun linearize-outcome (outcome)
  (cond ((optimized-slot-reader-p outcome)
         (list 'optimized-slot-reader
               (optimized-slot-reader-index outcome)
               (optimized-slot-reader-slot-name outcome)
               (optimized-slot-reader-class outcome)))
        ((optimized-slot-writer-p outcome)
         (list 'optimized-slot-writer
               (optimized-slot-writer-index outcome)))
        ((fast-method-call-p outcome)
         (list 'fast-method-call
               (fast-method-call-function outcome)))
        ((effective-method-outcome-p outcome)
         (list 'effective-method-outcome
               (effective-method-outcome-function outcome)))))

(defvar *isa*
  '((miss 0) (advance 1) (tag-test 2) (header-stamp-read 3) (where-branch 4)
    (complex-stamp-read 5) (<-branch 6) (=-check 7) (range-check 8) (eql-search 9)
    (optimized-slot-reader 10) (optimized-slot-writer 11)
    (fast-method-call 12) (effective-method-outcome 13)))

(defun opcode (inst)
  ;; we sometimes have non-inst symbols and objects, such as slot names.
  (or (second (assoc inst *isa*)) inst))

(defun link (linear)
  (let* ((tags (make-hash-table :test #'eq))
         (program
           (loop with ip = 0
                 for item in linear
                 if (and (consp item) (eq (first item) 'label))
                   do (setf (gethash (second item) tags) ip)
                 else do (incf ip) and collect (opcode item))))
    (map 'vector
         (lambda (item)
           (if (and (consp item) (eq (first item) 'go))
               (or (gethash (second item) tags)
                   (error "BUG: In fastgf linker, undefined tag: ~a" (second item)))
               item))
         program)))

;;; SIMPLE ENTRY POINT

(defun compute-dispatch-program (call-history specializer-profile)
  (let* ((basic (basic-tree call-history specializer-profile))
         (compiled (compile-tree basic))
         (linear (linearize compiled))
         (final (link linear)))
    final))
