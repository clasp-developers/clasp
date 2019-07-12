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
;;;
;;; ADVANCE: assign ARG = get next arg. unconditional jump to NEXT.
;;; TAG-TEST: check the tag of ARG. If it's one of the discriminatable tags, jump to the
;;;           tag-th entry of the tag-test's vector. Otherwise, jump to the default.
;;; STAMP-READ: assign STAMP = header stamp of ARG. If STAMP indicates a C++
;;;             object, jump to C++. Otherwise, STAMP = read the complex stamp,
;;;             then jump to OTHER.
;;;             NOTE: This could be expanded into a multi-way branch
;;;             for derivables versus instances, etc.
;;; <-BRANCH: If STAMP < PIVOT (a constant), jump to LEFT. Otherwise jump to RIGHT.
;;; =-CHECK: If STAMP = PIVOT (a constant), jump to NEXT. Otherwise jump to miss.
;;; RANGE-CHECK: If MIN <= STAMP <= MAX (constants), jump to NEXT, otherwise miss.
;;; EQL-SEARCH: If ARG eqls the nth object of OBJECTS, jump the nth entry of NEXTS.
;;;             If it eqls none of the OBJECTS, jump to DEFAULT.
;;; MISS: unconditional jump to dispatch-miss routine.

(defstruct (advance (:type vector) :named) next)
(defstruct (tag-test (:type vector) :named) tags default)
(defstruct (stamp-read (:type vector) :named) c++ other)
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
    (let* (;; Build our tests, in reverse order so they can refer to their successors.
           (c++-search (compile-ranges (classes-to-ranges c++-classes)))
           (other-search (compile-ranges (classes-to-ranges other-classes)))
           (stamp
             (if (and (miss-p c++-search) (miss-p other-search))
                 c++-search ; no need to branch - miss immediately.
                 (make-stamp-read :c++ c++-search :other other-search)))
           (tag-test
             (if (and (miss-p stamp)
                      (every #'null tags))
                 stamp ; miss immediately
                 (compile-tag-test tags stamp))))
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

(defparameter *isa*
  '((miss 0) (advance 1) (tag-test 2) (stamp-read 3)
    (<-branch 4) (=-check 5) (range-check 6) (eql 7)
    (optimized-slot-reader 8) (optimized-slot-writer 9)
    (fast-method-call 10) (effective-method-outcome 11)))

(defun opcode (inst)
  (or (second (assoc inst *isa*))
      (error "BUG: In fastgf linker, symbol is not an op: ~a" inst)))

;;; Build a linear program (list of opcodes and objects) from a compiled tree.
;;; We do this in one pass to save memory (an actual problem, if this is done
;;; naively) and time.
;;; collect1 collects a new cons with some value. straightforward.
;;; wait takes a tree, puts a blank cons in the list, puts the tree in a todo
;;; list, and then returns. once a tree is finished, it does (cont). if there
;;; is a tree in the todo list, it pops one and sets the corresponding cons
;;; to have the current ip instead of nil, then continues generating from there.
(defun linearize (tree)
  (let* ((links nil) (ip 0) (head (list nil)) (tail head))
    (macrolet ((collect1 (x)
                 `(let ((new-tail (list ,x)))
                    (setf (cdr tail) new-tail)
                    (setf tail new-tail)
                    (incf ip)))
               (collect (&rest xs)
                 `(progn ,@(loop for x in xs
                                 collect `(collect1 ,x))))
               (wait (tree)
                 `(let ((new-tail (list nil)))
                    (push (cons new-tail ,tree) links)
                    (setf (cdr tail) new-tail)
                    (setf tail new-tail)
                    (incf ip)))
               (next (tree)
                 `(setf tree ,tree))
               (cont ()
                 `(if (null links)
                      ;; nothing more to do
                      (return (cdr head))
                      ;; go to the next tree
                      (destructuring-bind (patchpoint . subtree)
                          (pop links)
                        (setf (car patchpoint) ip)
                        (next subtree)))))
      (loop (cond ((advance-p tree)
                   (collect (opcode 'advance))
                   (next (advance-next tree)))
                  ((tag-test-p tree)
                   (collect (opcode 'tag-test))
                   (loop for tag across (tag-test-tags tree)
                         do (wait tag))
                   (next (tag-test-default tree)))
                  ((stamp-read-p tree)
                   (collect (opcode 'stamp-read))
                   (wait (stamp-read-c++ tree))
                   (next (stamp-read-other tree)))
                  ((<-branch-p tree)
                   (collect (opcode '<-branch)
                            (<-branch-pivot tree))
                   (wait (<-branch-left tree))
                   (next (<-branch-right tree)))
                  ((=-check-p tree)
                   (collect (opcode '=-check)
                            (=-check-pivot tree))
                   (next (=-check-next tree)))
                  ((range-check-p tree)
                   (collect (opcode 'range-check)
                            (range-check-min tree)
                            (range-check-max tree))
                   (next (range-check-next tree)))
                  ((eql-search-p tree)
                   (loop for object across (eql-search-objects tree)
                         for next across (eql-search-nexts tree)
                         do (collect (opcode 'eql)
                                     object)
                            (wait next))
                   (next (eql-search-default tree)))
                  ((miss-p tree)
                   (collect (opcode 'miss))
                   (cont))
                  ((optimized-slot-reader-p tree)
                   (collect (opcode 'optimized-slot-reader)
                            (optimized-slot-reader-index tree)
                            (optimized-slot-reader-slot-name tree)
                            (optimized-slot-reader-class tree))
                   (cont))
                  ((optimized-slot-writer-p tree)
                   (collect (opcode 'optimized-slot-writer)
                            (optimized-slot-writer-index tree))
                   (cont))
                  ((fast-method-call-p tree)
                   (collect (opcode 'fast-method-call)
                            (fast-method-call-function tree))
                   (cont))
                  ((effective-method-outcome-p tree)
                   (collect (opcode 'effective-method-outcome)
                            (effective-method-outcome-function tree))
                   (cont))
                  (t (error "Unknown dtree: ~a" tree)))))))

;;; used in cmpfastgf
;;; T if any outcomes are effective method outcomes
;;; dtree must be a compiled tree

(defun dtree-requires-vaslist-p (dtree)
  (cond ((effective-method-outcome-p dtree) t)
        ((outcome-p dtree) nil) ; other outcomes are fine
        ((advance-p dtree)
         (dtree-requires-vaslist-p (advance-next dtree)))
        ((tag-test-p dtree)
         (or (dtree-requires-vaslist-p (tag-test-default dtree))
             (some #'dtree-requires-vaslist-p (tag-test-tags dtree))))
        ((stamp-read-p dtree)
         (or (dtree-requires-vaslist-p (stamp-read-c++ dtree))
             (dtree-requires-vaslist-p (stamp-read-other dtree))))
        ((<-branch-p dtree)
         (or (dtree-requires-vaslist-p (<-branch-left dtree))
             (dtree-requires-vaslist-p (<-branch-right dtree))))
        ((=-check-p dtree)
         (dtree-requires-vaslist-p (=-check-next dtree)))
        ((range-check-p dtree)
         (dtree-requires-vaslist-p (range-check-next dtree)))
        ((eql-search-p dtree)
         (or (dtree-requires-vaslist-p (eql-search-default dtree))
             (some #'dtree-requires-vaslist-p (eql-search-nexts dtree))))
        ((miss-p dtree) nil)
        (t (error "Unknown dtree: ~a" dtree))))

;;; SIMPLE ENTRY POINTS

(defun calculate-dtree (call-history specializer-profile)
  (compile-tree (basic-tree call-history specializer-profile)))

(defun compute-dispatch-program (call-history specializer-profile)
  (let* ((basic (basic-tree call-history specializer-profile))
         (compiled (compile-tree basic))
         (linear (linearize compiled))
         (final (coerce linear 'vector)))
    final))
