(in-package "CLOS")

;;; Misc

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

(defstruct (test (:type vector) :named) index (paths nil))
(defstruct (skip (:type vector) :named) next)

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
              (pair (assoc spec (test-paths node))))
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
        (first-specialized (position-if #'identity specializer-profile)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;

;;; Make a new subtree with only one path, starting with the ith specializer.
(defun bc-remaining-subtree (specializers outcome specializer-indices)
  (if (null specializer-indices)
      outcome
      (make-test :index (car specializer-indices)
                 :paths
                 (acons (svref specializers (car specializer-indices))
                        (bc-remaining-subtree specializers outcome (cdr specializer-indices))
                        nil))))

;;; Adds a call history entry to a tree, avoiding new nodes as much as possible.
(defun bc-add-entry (node specializers outcome specializer-indices)
  (when specializer-indices
    (let ((specializer-index (car specializer-indices)))
      (cond
        ((outcome-p node)
         ;; If we're here, we don't have anything to add.
         (error "BUG in BC-ADD-ENTRY: Redundant call history entry: ~a"
                (cons specializers outcome)))
        ((test-p node)
         (let* ((spec (svref specializers specializer-index))
                (pair (assoc spec (test-paths node))))
           (if pair
               ;; our entry is so far identical to an existing one;
               ;; continue the search.
               (bc-add-entry (cdr pair) specializers outcome (cdr specializer-indices))
               ;; We have something new. Add it and we're done.
               (setf (test-paths node)
                     (acons spec
                            (bc-remaining-subtree 
                             specializers outcome (cdr specializer-indices))
                            (test-paths node))))))
        (t
         (error "BUG in BC-ADD-ENTRY: Not a node: ~a" node))))))

(defun bc-basic-tree (call-history specializer-profile)
  (assert (not (null call-history)))
  (let ((last-specialized (position nil specializer-profile :from-end t :test-not #'eq))
        (first-specialized (position-if #'identity specializer-profile)))
    (let ((specializer-indices (when (and (integerp first-specialized) (integerp last-specialized))
                                 (loop for index from first-specialized to last-specialized
                                     when (elt specializer-profile index)
                                       collect index))))
      (when (null last-specialized)
        ;; no specialization - we go immediately to the outcome
        ;; (we could assert all outcomes are identical)
        (return-from bc-basic-tree (values (cdr (first call-history)) 0)))
      ;; usual case
      (loop with result = (make-test :index (car specializer-indices))
            with specialized-length = (1+ last-specialized)
            for (specializers . outcome) in call-history
            do (bc-add-entry result specializers outcome specializer-indices)
            finally (return (values result specialized-length))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 
;;; Compiling a basic tree into concrete tests and sundry.
;;; The node/possibly DAG here basically forms a slightly weird VM defined as follows.
;;; There are two registers, ARG and STAMP. Each node performs an action and then branches.
;;;
;;; ARGUMENT: assign ARG = get next arg if COUNT==NIL then unconditional jump to NEXT.
;;;                        = COUNT (if COUNT is fixnum) then unconditional jump to NEXT
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

(defstruct (argument (:type vector) :named) count next)
(defstruct (register (:type vector) :named) index next)
(defstruct (tag-test (:type vector) :named) tags default)
(defstruct (stamp-read (:type vector) :named) c++ other)
(defstruct (<-branch (:type vector) :named) pivot left right)
(defstruct (=-check (:type vector) :named) pivot next)
(defstruct (range-check (:type vector) :named) min max next)
(defstruct (eql-search (:type vector) :named) objects nexts default)
(defstruct (miss (:type vector) :named))

(defun compile-tree-top (tree)
    (compile-tree tree))

(defun compile-tree (tree)
  (cond ((outcome-p tree) tree)
        ((skip-p tree) (make-argument :next (compile-tree (skip-next tree))))
        ((test-p tree) (compile-test tree))))

(defun compile-test (test)
  (multiple-value-bind (eqls tags c++-classes other-classes)
      (differentiate-specializers (test-paths test))
    (let* (;; Build our tests, in reverse order so they can refer to their successors.
           (c++-search (compile-ranges (classes-to-ranges c++-classes)))
           (other-search (compile-ranges (classes-to-ranges other-classes)))
           (stamp
             (if (and (miss-p c++-search) (miss-p other-search))
                 c++-search    ; no need to branch - miss immediately.
                 (make-stamp-read :c++ c++-search :other other-search)))
           (tag-test
             (if (and (miss-p stamp)
                      (every #'null tags))
                 stamp                  ; miss immediately
                 (compile-tag-test tags stamp))))
      (make-argument
       :count (test-index test)
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
        do (cond ((safe-eql-specializer-p spec) (push pair eqls))
                 ((tag-spec-p spec)
                  (setf (svref tags-vector (class-tag spec)) (cdr pair)))
                 (t
                  (if (core::header-stamp-case (core:class-stamp-for-instances spec)
                                               t t t nil)
                      (setf other-classes
                            (insert-sorted pair other-classes #'< #'path-pair-key))
                      (setf c++-classes
                            (insert-sorted pair c++-classes #'< #'path-pair-key)))))
        finally (return (values eqls tags-vector c++-classes other-classes))))

(defun path-pair-key (pair) (core:class-stamp-for-instances (car pair)))

;;; tag tests

(defun tags-vector () (make-array (length *tag-tests*) :initial-element nil))

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
              if (and (core:stamps-adjacent-p (cdar current) stamp)
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
  (make-eql-search :objects (map 'simple-vector (lambda (pair)
                                                  (eql-specializer-object
                                                   (car pair)))
                                 eqls)
                   :nexts (map 'simple-vector (lambda (pair)
                                                (compile-tree (cdr pair)))
                               eqls)
                   :default next))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Linearization
;;;

(defun opcode (inst)
  (or (second (assoc inst *isa*))
      (error "BUG: In fastgf linker, symbol is not an op: ~a" inst)))

(defstruct (bc-constant-arg (:type vector) :named)
  value)

(defstruct (bc-constant-ref (:type vector) :named)
  ref)

(defstruct (bc-label-arg (:type vector) :named)
  lip index delta)

(defstruct (bc-register-arg (:type vector) :named)
  index)

(defstruct (bc-instruction (:type vector) :named)
  name lip index byte-index code final-op)

(defstruct (bc-long-instruction (:type vector) (:include bc-instruction) :named)
  code-add)

(defun longify-instruction (short-instruction num-ops instr)
  (declare (ignorable instr))
  (let ((longer (make-bc-long-instruction :name (bc-instruction-name short-instruction)
                                          :code (bc-instruction-code short-instruction)
                                          :code-add num-ops
                                          :lip (bc-instruction-lip short-instruction)
                                          :index (bc-instruction-index short-instruction)
                                          :byte-index (bc-instruction-byte-index short-instruction))))
;;;    (format t "longify-instruction ~s~%  from ~s~%" longer instr)
    longer))


(defun annotated-opcode (inst)
  (let* ((code-cell (assoc inst *isa*))
         (code (second code-cell)))
    (make-bc-instruction :name inst :code code)))

(defconstant +longify-trigger+ 255)

;; Move constants into the literals vector and replace them with
;; indices in the program.  Also accumulate patchpoints for labels
(defun literalify-arguments (instr dtree-op literals coallesce-indexes)
  (loop named literalify
        with long-arg = nil
        for cur = (cdr instr) then (cdr cur)
        for annotated-arg = (car cur)
        for arg-type in (dtree-op-arguments dtree-op)
        collect (cond
                  ((bc-constant-arg-p annotated-arg)
                   (let* ((arg (bc-constant-arg-value annotated-arg))
                          (seen-index-value (gethash arg coallesce-indexes)))
                     (if (null seen-index-value)
                         (let* ((index (vector-push-extend arg literals))
                               (index-value (make-bc-constant-ref :ref index)))
                           (setf (gethash arg coallesce-indexes) index-value)
                           (when (> index +longify-trigger+)
                             (setf long-arg t))
                           index-value)
                         seen-index-value)))
                  ((bc-label-arg-p annotated-arg) annotated-arg)
                  ((bc-register-arg-p annotated-arg) annotated-arg)
                  (t (error "Illegal arg ~a" annotated-arg)))
          into new-args
        when (null (cdr cur))
          do (return-from literalify (values new-args long-arg))))

;;; Move constants into a literal vector and replace them with references
;;; Return a vector of nil/T, one for each instruction if the instruction is long
(defun reference-literals (compiled)
  (let* ((coallesce-indexes (make-hash-table :test 'eql))
         (literals (make-array (length compiled) :fill-pointer 0
                                                 :adjustable t
                                                 :initial-element nil))
         (longs (make-array (length compiled) :initial-element nil))
         (new-program (loop for instr in compiled
                            for index from 0
                            for annotated-op = (first instr)
                            for op = (bc-instruction-code annotated-op)
                            for dop = (elt *dtree-ops* op)
                            collect (multiple-value-bind (new-args long-arg)
                                        (literalify-arguments instr dop literals coallesce-indexes)
                                      (if long-arg
                                          (progn
                                            (format t "Dealing with long instruction ~s~%" instr)
                                            (setf (elt longs index) long-arg)
                                            (longify-instruction annotated-op (length *dtree-ops*) instr))
                                          (list* annotated-op new-args))))))
    (values new-program literals longs)))

;;; Calculate a map of labels to byte-positions taking
;;; into account the longs
(defun generate-label-map (instructions longs)
  (let ((map (make-array (length instructions))))
    (loop for instr in instructions
          for index from 0
          with ip = 0
          for annotated-op = (first instr)
          for op = (bc-instruction-code annotated-op)
          for dop = (elt *dtree-ops* op)
          for long = (elt longs index)
          for byte-length = (dtree-op-byte-length dop long)
          do (setf (elt map index) ip)
          do (incf ip byte-length))
    map))

(defun longify-instruction-p (ip instruction dtree-op labels)
  (let ((need-longify nil))
    (loop named longify
          for idx below (length (dtree-op-label-argument-indices dtree-op))
          for label-index = (elt (dtree-op-label-argument-indices dtree-op) idx)
          for annotated-jump-label = (elt (cdr instruction) label-index)
          for jump-label = (bc-label-arg-index annotated-jump-label)
          for start-ip = ip
          for end-ip = (elt labels jump-label)
          for delta = (- end-ip start-ip)
          do (setf (bc-label-arg-delta annotated-jump-label) delta) ; save the delta in the arg
          when (> delta +longify-trigger+)
            do (setf need-longify t))
    need-longify))

;;; Iterate through the instructions and determine if any of them
;;; need to be made long - if they do then update the longs vector
;;; and return new-longify=t
(defun maybe-longify-instructions (instructions labels longs)
  (let ((new-long nil))
    (loop for instr in instructions
          for index from 0
          for ip = (elt labels index)
          for annotated-op = (first instr)
          for op = (bc-instruction-code annotated-op)
          for dop = (elt *dtree-ops* op)
          for long = (elt longs index)
          do (when (not long)
               (when (longify-instruction-p ip instr dop labels)
                 (setf (first instr) (longify-instruction annotated-op (length *dtree-ops*) instr)
                       new-long t
                       (elt longs index) t))))
    (values longs new-long)))

(defun update-label-deltas (instructions labels longs)
  (loop for instr in instructions
        for index from 0
        for ip = (elt labels index)
        for annotated-op = (first instr)
        for op = (bc-instruction-code annotated-op)
        for dop = (elt *dtree-ops* op)
        for long = (elt longs index)
        for longify = (longify-instruction-p ip instr dop labels)
        when (and longify (not long))
          do (error "A new longify was calculated ~a" instr)))

(defun byteify-args (args long bytecode)
  (labels ((two-byte (val bytecode)
             (let ((low (logand val #xff))
                   (high (logand (ash val -8) #xff))
                   (higher (ash val -16)))
               (when (> higher 0) (error "A value ~a larger than 65535 cannot be coded in two bytes - you need a bigger vm" val))
               (vector-push-extend low bytecode)
               (vector-push-extend high bytecode))))
    (loop for arg in args
          do (cond
               ((bc-constant-ref-p arg)
                (let ((arg-val (bc-constant-ref-ref arg)))
                  (if long
                      (two-byte arg-val bytecode)
                      (if (> arg-val +longify-trigger+)
                          (error "This value should be long ~a" arg-val)
                          (vector-push-extend arg-val bytecode)))))
               ((bc-label-arg-p arg)
                (let ((arg-val (bc-label-arg-delta arg)))
                  (if long
                      (two-byte arg-val bytecode)
                      (if (> arg-val +longify-trigger+)
                          (warn "This value should be long ~a" arg)
                          (vector-push-extend arg-val bytecode)))))
               ((bc-register-arg-p arg)
                (let ((arg-val (bc-register-arg-index arg)))
                  (if long
                      (two-byte arg-val bytecode)
                      (if (> arg-val +longify-trigger+)
                          (warn "This value should be long ~a" arg)
                          (vector-push-extend arg-val bytecode)))))
               (t (error "Illegal arg type ~a" arg))))))

(defun bytecodeify (instructions longs labels bytecode)
  (let* ((ip (length bytecode))
         (saw-long nil)
         (new-instructions (loop for instr in instructions
                                 for index from 0
                                 for long across longs
                                 for annotated-op = (car instr)
                                 for op = (bc-instruction-code annotated-op)
                                 for final-op = (if long
                                                    (if (bc-long-instruction-p annotated-op)
                                                        (progn
                                                          (setf saw-long t)
                                                          (+ op (bc-long-instruction-code-add annotated-op)))
                                                        (error "instruction is not long ~a" annotated-op))
                                                    op)
                                 for args = (cdr instr)
                                 for byte-ip = (elt labels index)
                                 do (vector-push-extend final-op bytecode)
                                 do (byteify-args args long bytecode)
                                 do (setf (bc-instruction-byte-index annotated-op) byte-ip
                                          (bc-instruction-final-op annotated-op) final-op)
                                 collect (list* annotated-op (cdr instr)))))
    (values ip new-instructions saw-long)))

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
      (loop (cond ((argument-p tree)
                   (cond
                     ((null (argument-count tree))
                      (collect (opcode 'advance)))
                     ((= (argument-count tree) 0)
                      (collect (opcode 'farg0)))
                     ((= (argument-count tree) 1)
                      (collect (opcode 'farg1)))
                     ((= (argument-count tree) 2)
                      (collect (opcode 'farg2)))
                     ((= (argument-count tree) 3)
                      (collect (opcode 'farg3)))
                     ((= (argument-count tree) 4)
                      (collect (opcode 'farg4)))
                     (t (collect(opcode 'argn) (argument-count tree))))
                   (next (argument-next tree)))
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
                   (collect (opcode 'lt-branch)
                            (<-branch-pivot tree))
                   (wait (<-branch-left tree))
                   (next (<-branch-right tree)))
                  ((=-check-p tree)
                   (collect (opcode 'eq-check)
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
                   (collect
                    (if (core:fixnump (optimized-slot-reader-index tree))
                        (opcode 'optimized-slot-reader) ; instance
                        (opcode 'car))                  ; class
                    (optimized-slot-reader-index tree)
                    (optimized-slot-reader-slot-name tree))
                   (cont))
                  ((optimized-slot-writer-p tree)
                   (collect
                    (if (core:fixnump (optimized-slot-writer-index tree))
                        (opcode 'optimized-slot-writer) ; instance
                        (opcode 'rplaca))               ; class
                    (optimized-slot-writer-index tree))
                   (cont))
                  ((effective-method-outcome-p tree)
                   (collect (opcode 'effective-method-outcome)
                            (effective-method-outcome-function tree))
                   (cont))
                  (t (error "BUG: Unknown dtree: ~a" tree)))))))

(defun group-instruction (linear ip-place index)
  (symbol-macrolet ((ip (car ip-place))
                    (ip++ (prog1 (car ip-place) (incf (car ip-place)))))
    (let* ((start-ip ip)
           (op (elt linear ip++))
           (dtree-op (elt *dtree-ops* op))
           (arguments (loop for arg in (dtree-op-arguments dtree-op)
                            collect (cond
                                      ((eq (car arg) 'constant-arg)
                                       (make-bc-constant-arg :value (elt linear ip++)))
                                      ((eq (car arg) 'label-arg)
                                       (make-bc-label-arg :lip (elt linear ip++)))
                                      ((eq (car arg) 'register-arg)
                                       (make-bc-register-arg :index (elt linear ip++)))
                                      (t (error "Illegal arg type ~a" arg))))))
      (list* (make-bc-instruction :name (dtree-op-name dtree-op) :lip start-ip :code op :index index) arguments))))

(defun group-instructions (linear)
  (let ((ip-place (list 0)))
    (loop for index from 0
          collect (group-instruction linear ip-place index) into instructions
          do (when (>= (car ip-place) (length linear))
               (return-from group-instructions instructions)))))

(defun index-instructions (instructions)
  (let ((lip-to-index (make-hash-table :test 'eql)))
    (loop for instr in instructions
          for op = (first instr)
          do (setf (gethash (bc-instruction-lip op) lip-to-index) (bc-instruction-index op)))
    (loop for instr in instructions
          for op = (first instr)
          for args = (rest instr)
          do (loop for arg in args
                   when (bc-label-arg-p arg)
                     do (setf (bc-label-arg-index arg) (gethash (bc-label-arg-lip arg) lip-to-index))))))

;;; Bytecode approach

(defun dtree-compile (generic-function)
  (multiple-value-bind (basic specialized-length)
      (bc-basic-tree
       (safe-gf-call-history generic-function)
       (safe-gf-specializer-profile generic-function))
    (values (compile-tree-top basic) specialized-length)))

;;; Called by GFBytecodeSimpleFun/make
(defun bytecode-dtree-compile (generic-function)
  (multiple-value-bind (compiled specialized-length)
      (dtree-compile generic-function)
    (let* ((linear (linearize compiled))
           (grouped (group-instructions linear)))
      (index-instructions grouped)
      (multiple-value-bind (instructions literals longs)
          (reference-literals grouped)
        (loop named longify
              with new-long
              with long-changes
              do (let ((labels (generate-label-map instructions longs)))
                   (multiple-value-setq (longs long-changes)
                     (maybe-longify-instructions instructions labels longs))
                   (when (null long-changes) (return-from longify labels))))
        (let ((labels (generate-label-map instructions longs))
              (bytecode (make-array 16 :element-type 'ext:byte8
                                       :adjustable t
                                       :fill-pointer 0))
              (entry-points (make-array 16 :adjustable t :fill-pointer 0)))
          (update-label-deltas instructions labels longs)
          (multiple-value-bind (entry-ip new-instructions saw-long)
              (bytecodeify instructions longs labels bytecode)
            (declare (ignorable saw-long))
            (vector-push-extend entry-ip entry-points)
            (values (copy-seq bytecode) (copy-seq entry-points) (copy-seq literals) specialized-length
                    #| Remaining return values are for debugging |#
                    instructions new-instructions labels grouped compiled)))))))

(defun bytecode-interpreted-discriminator (generic-function)
  (let ((program (sys:gfbytecode-simple-fun/make generic-function)))
    program))

(export 'bytecode-dtree-compile :clos)

;;; Return a list of instruction specs and labels,
;;; where each instruction spec is of the form (operator ...),
;;; and labels are integers.
;;; (Because discriminator bytecode never jumps backwards,
;;;  we can do this in one pass.)
(defun %disassemble-discriminator (bytecode literals)
  (declare (type (simple-array (unsigned-byte 8) (*)) bytecode)
           (type simple-vector literals))
  (flet ((code (ip) (aref bytecode ip))
         (literal (index) (aref literals index))
         (arg (ip offset len)
           (ecase len
             ((1) (aref bytecode (+ ip offset)))
             ((2) (+ (ash (aref bytecode (+ ip offset 1)) 8)
                          (aref bytecode (+ ip offset)))))))
    (loop with ops = *dtree-ops*
          with long-code-add = (length *dtree-ops*)
          with labels = nil
          with len = (length bytecode)
          with ip = 0
          for opcode = (code ip)
          for longp = (> opcode long-code-add)
          for real-opcode = (if longp (- opcode long-code-add) opcode)
          for op = (or (find real-opcode ops :key #'dtree-op-code)
                       (error "Unknown opcode ~d" opcode))
          for sym = (dtree-op-sym op)
          for argspecs = (if longp
                             (dtree-op-long-arguments op)
                             (dtree-op-arguments op))
          when (member ip labels)
            collect ip
          collect (list*
                   sym
                   (loop for offset = 1 then (+ offset len)
                         for (type len) in argspecs
                         collect (ecase type
                                   ((label-arg)
                                    (let ((label (+ ip (arg ip offset len))))
                                      (push label labels)
                                      label))
                                   ((register-arg) (arg ip offset len))
                                   ((constant-arg)
                                    (literal (arg ip offset len))))))
          do (incf ip (+ 1 (reduce #'+ argspecs :key #'second)))
          until (>= ip len))))

(defun disassemble-discriminator (function)
  (format t "~&Disassembly (discriminator):~%")
  (loop with bytecode = (core:gfbytecode-simple-fun/bytecode function)
        with literals = (core:gfbytecode-simple-fun/literals function)
        for item in (%disassemble-discriminator bytecode literals)
        do (etypecase item
             (unsigned-byte
              (format t " ~d:~%" item))
             (cons
              (format t "~(~a~)~{ ~s~}~%"
                      (symbol-name (car item)) (cdr item))))))
