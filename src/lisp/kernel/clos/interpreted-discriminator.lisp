(in-package "CLOS")

;;; ISA
;;; FIXME: Read from bytecode-machines

(defclass dtree-op ()
  ;; FIXME ho-ly crap do we really need all these fields
  ((%sym :initarg :sym :reader dtree-op-sym)
   (%name :initarg :name :reader dtree-op-name)
   (%code :initarg :code :reader dtree-op-code)
   (%macro-name :initarg :macro-name :reader dtree-op-macro-name)
   (%macros :initarg :macros :reader dtree-op-macros)
   (%arguments :initarg :arguments :reader dtree-op-arguments)
   (%long-arguments :initarg :long-arguments :reader dtree-op-long-arguments)
   (%label-argument-indices :initarg :label-argument-indices
                            :reader dtree-op-label-argument-indices)))

(defun dtree-op-byte-length (dtree-op long)
  (1+ (if long
          (let ((sum-bytes 0))
            (dolist (arg (dtree-op-long-arguments dtree-op))
              (let ((bytes (second arg)))
                (incf sum-bytes bytes)))
            sum-bytes)
          (let ((sum-bytes 0))
            (dolist (arg (dtree-op-arguments dtree-op))
              (incf sum-bytes (second arg)))
            sum-bytes))))

(defclass dtree-macro () ; FIXME what even is this
  ((%name :initarg :name :reader dtree-macro-name)
   (%value :initarg :value :reader dtree-macro-value)))

(macrolet ((defops (&rest ops)
             (let ((new-dtree-ops (make-list (length ops)))
                   new-isa)
               (dolist (op ops)
                 (destructuring-bind (name code macro-name &optional argument-info)
                     op
                   (let* ((sym (intern (string-upcase name)))
                          ;; KLUDGE: make-array does not work at compile time
                          ;; because the element type T is resolved to a cmp class
                          #+(or)
                          (label-argument-indices (make-array 4 :adjustable t :fill-pointer 0))
                          rev-label-argument-indices
                          rev-arguments
                          rev-long-arguments
                          rev-macros)
                     (push `(early-make-instance dtree-macro
                                                 :name ',macro-name :value ,code)
                           rev-macros)
                     (dotimes (index (length argument-info))
                       (let ((arg (elt argument-info index)))
                         (destructuring-bind (arg-type arg-name)
                             arg
                           (push `(early-make-instance dtree-macro
                                                       :name ',arg-name
                                                       :value ,(1+ index))
                                 rev-macros)
                           (cond
                             ((eq arg-type 'constant-arg)
                              (push `(constant-arg 1) rev-arguments)
                              (push `(constant-arg 2) rev-long-arguments))
                             ((eq arg-type 'label-arg)
                              (push index rev-label-argument-indices)
                              (push `(label-arg 1) rev-arguments)
                              (push `(label-arg 2) rev-long-arguments))
                             ((eq arg-type 'register-arg)
                              (push `(register-arg 1) rev-arguments)
                              (push `(register-arg 2) rev-long-arguments))
                             ((eq (car arg) 'offset))
                             (t (error "Illegal argument type ~s" arg))))))
                     (setf (elt new-dtree-ops code)
                           `(early-make-instance
                             dtree-op
                             :sym ',sym :name ',name :code ',code
                             :macro-name ',macro-name
                             :macros ',(nreverse rev-macros)
                             :arguments ',(nreverse rev-arguments)
                             :long-arguments ',(nreverse rev-long-arguments)
                             :label-argument-indices ',(apply #'vector (nreverse rev-label-argument-indices))))
                     (push (list sym code) new-isa))))
               `(progn
                  (defparameter *dtree-ops* (vector ,@new-dtree-ops))
                  (defparameter *isa* ',new-isa)))))
  (defops
    ("miss" 0 "DTREE_OP_MISS")
    ("advance" 1 "DTREE_OP_ADVANCE")
    ("tag-test" 2 "DTREE_OP_TAG_TEST" ((label-arg "DTREE_FIXNUM_TAG_OFFSET")
                                       (label-arg "DTREE_SINGLE_FLOAT_TAG_OFFSET")
                                       (label-arg "DTREE_CHARACTER_TAG_OFFSET")
                                       (label-arg "DTREE_CONS_TAG_OFFSET")
                                       (offset "DTREE_GENERAL_TAG_OFFSET")))
    ("stamp-read" 3 "DTREE_OP_STAMP_READ" ((label-arg "DTREE_READ_HEADER_OFFSET")
                                           (offset "DTREE_READ_OTHER_OFFSET")))
    ("lt-branch" 4 "DTREE_OP_LT_BRANCH" ((constant-arg "DTREE_LT_PIVOT_OFFSET")
                                         (label-arg "DTREE_LT_LEFT_OFFSET")
                                         (offset "DTREE_LT_RIGHT_OFFSET")))
    ("eq-check" 5 "DTREE_OP_EQ_CHECK" ((constant-arg "DTREE_EQ_PIVOT_OFFSET")
                                       (offset "DTREE_EQ_NEXT_OFFSET")))
    ("range-check" 6 "DTREE_OP_RANGE_CHECK" ((constant-arg "DTREE_RANGE_MIN_OFFSET")
                                             (constant-arg "DTREE_RANGE_MAX_OFFSET")
                                             (offset "DTREE_RANGE_NEXT_OFFSET")))
    ("eql" 7 "DTREE_OP_EQL" ((constant-arg "DTREE_EQL_OBJECT_OFFSET")
                             (label-arg "DTREE_EQL_BRANCH_OFFSET")
                             (offset "DTREE_EQL_NEXT_OFFSET")))
    ("optimized-slot-reader" 8 "DTREE_OP_SLOT_READ" ((constant-arg "DTREE_SLOT_READER_INDEX_OFFSET")
                                                     (constant-arg "DTREE_SLOT_READER_SLOT_NAME_OFFSET")))
    ("optimized-slot-writer" 9 "DTREE_OP_SLOT_WRITE" ((constant-arg "DTREE_SLOT_WRITER_INDEX_OFFSET")))
    ("car" 10 "DTREE_OP_CAR" ((constant-arg "DTREE_CAR_READER_INDEX_OFFSET")
                              (constant-arg "DTREE_CAR_READER_CAR_NAME_OFFSET")))
    ("rplaca" 11 "DTREE_OP_RPLACA" ((constant-arg "DTREE_RPLACA_WRITER_INDEX_OFFSET")))
    ("effective-method-outcome" 12 "DTREE_OP_EFFECTIVE_METHOD" ((constant-arg "DTREE_EFFECTIVE_METHOD_OFFSET")))
    ("farg0" 13 "DTREE_OP_FARG0")
    ("farg1" 14 "DTREE_OP_FARG1")
    ("farg2" 15 "DTREE_OP_FARG2")
    ("farg3" 16 "DTREE_OP_FARG3")
    ("farg4" 17 "DTREE_OP_FARG4")
    ("argn" 18 "DTREE_OP_ARGN" ((register-arg "DTREE_ARGN_OFFSET")
                                (offset "DTREE_ARGN_NEXT_OFFSET")))
    ("sd-eq-branch" 19 "DTREE_OP_SD_EQ_BRANCH" ((constant-arg "DTREE_SD_STAMP_OFFSET")
                                                (label-arg "DTREE_SD_FAIL_OFFSET")
                                                (offset "DTREE_SD_NEXT_OFFSET")))
    ("single-dispatch-miss" 20 "DTREE_OP_SINGLE_DISPATCH_MISS")
    ))

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

(defmacro define-type-predicate (function-name class-name)
  `(progn
     (defgeneric ,function-name (object))
     (defmethod ,function-name ((object ,class-name)) t)
     (defmethod ,function-name ((object t)) nil)))

(defclass dtree-test ()
  ((%index :initarg :index :reader dtree-index)
   (%paths :initarg :paths :initform nil :accessor test-paths)))
(defun make-test (&key index paths)
  (early-make-instance dtree-test :index index :paths paths))

(defclass dtree-skip ()
  ((%next :initarg :next :reader dtree-next)))
(defun make-skip (&key next)
  (early-make-instance dtree-skip :next next))

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
    (typecase node
      (outcome
       ;; If we're here, we don't have anything to add.
       (error "BUG in ADD-ENTRY: Redundant call history entry: ~a"
              (cons specializers outcome)))
      (dtree-skip
       (add-entry (dtree-next node) specializers outcome sprofile speclength (1+ i)))
      (dtree-test
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
      (typecase node
        (outcome
         ;; If we're here, we don't have anything to add.
         (error "BUG in BC-ADD-ENTRY: Redundant call history entry: ~a"
                (cons specializers outcome)))
        (dtree-test
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

(defclass dtree-argument ()
  ((%count :initarg :count :reader argument-count)
   (%next :initarg :next :reader dtree-next)))
(defun make-argument (&key count next)
  (early-make-instance dtree-argument :count count :next next))

(defclass dtree-tag-test ()
  ((%tags :initarg :tags :reader tag-test-tags)
   (%default :initarg :default :reader tag-test-default)))
(defun make-tag-test (&key tags default)
  (early-make-instance dtree-tag-test :tags tags :default default))

(defclass dtree-stamp-read ()
  ((%c++ :initarg :c++ :reader stamp-read-c++)
   (%other :initarg :other :reader stamp-read-other)))
(defun make-stamp-read (&key c++ other)
  (early-make-instance dtree-stamp-read :c++ c++ :other other))

(defclass dtree-<-branch ()
  ((%pivot :initarg :pivot :reader pivot)
   (%left :initarg :left :reader <-branch-left)
   (%right :initarg :right :reader <-branch-right)))
(defun make-<-branch (&key pivot left right)
  (early-make-instance dtree-<-branch :pivot pivot :left left :right right))

(defclass dtree-=-check ()
  ((%pivot :initarg :pivot :reader pivot)
   (%next :initarg :next :reader dtree-next)))
(defun make-=-check (&key pivot next)
  (early-make-instance dtree-=-check :pivot pivot :next next))

(defclass dtree-range-check ()
  ((%min :initarg :min :reader range-check-min)
   (%max :initarg :max :reader range-check-max)
   (%next :initarg :next :reader dtree-next)))
(defun make-range-check (&key min max next)
  (early-make-instance dtree-range-check :min min :max max :next next))

(defclass dtree-eql-search ()
  ((%objects :initarg :objects :reader eql-search-objects)
   (%nexts :initarg :nexts :reader eql-search-nexts)
   (%default :initarg :default :reader eql-search-default)))
(defun make-eql-search (&key objects nexts default)
  (early-make-instance dtree-eql-search
                       :objects objects :nexts nexts :default default))

(defclass dtree-miss () ())
(defun make-miss () (early-make-instance dtree-miss))

(defun compile-tree-top (tree)
    (compile-tree tree))

(defun compile-tree (tree)
  (etypecase tree
    (outcome tree)
    (dtree-skip (make-argument :next (compile-tree (dtree-next tree))))
    (dtree-test (compile-test tree))))

(defun compile-test (test)
  (multiple-value-bind (eqls tags c++-classes other-classes)
      (differentiate-specializers (test-paths test))
    (let* (;; Build our tests, in reverse order so they can refer to their successors.
           (c++-search (compile-ranges (classes-to-ranges c++-classes)))
           (other-search (compile-ranges (classes-to-ranges other-classes)))
           (stamp
             (if (and (typep c++-search 'dtree-miss)
                   (typep other-search 'dtree-miss))
                 c++-search    ; no need to branch - miss immediately.
                 (make-stamp-read :c++ c++-search :other other-search)))
           (tag-test
             (if (and (typep stamp 'dtree-miss)
                      (every #'null tags))
                 stamp                  ; miss immediately
                 (compile-tag-test tags stamp))))
      (make-argument
       :count (dtree-index test)
       ;; we do EQL tests before anything else. they could be moved later if we altered
       ;; when eql tests are stored in the call history, i think.
       :next (cond ((null eqls)
                    ;; we shouldn't have any empty tests - sanity check this
                    ;;(assert (not (miss-p tag-test)))
                    tag-test)
                   (t (compile-eql-search eqls tag-test)))))))

(defvar *tag-tests* (llvm-sys:tag-tests))

(defun tag-spec-p (class) ; is CLASS one that's manifested as a tag test?
  (member (stamp-for-instances class) *tag-tests* :key #'second))

;;; FIXME: wrapped
(defgeneric c++-class-p (class))
(defmethod c++-class-p ((class built-in-class)) t)
(defmethod c++-class-p ((class standard-class)) nil)
(defmethod c++-class-p ((class funcallable-standard-class)) nil)
(defmethod c++-class-p ((class structure-class)) nil)
;; These are not "C++ classes" in the sense of having low, unchanging stamps.
(defmethod c++-class-p ((class core:derivable-cxx-class)) nil)
(defmethod c++-class-p ((class core:clbind-cxx-class)) nil)

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
                 ((c++-class-p spec)
                  (setf c++-classes
                        (insert-sorted pair c++-classes #'< #'path-pair-key)))
                 (t
                  (setf other-classes
                        (insert-sorted pair other-classes #'< #'path-pair-key))))
        finally (return (values eqls tags-vector c++-classes other-classes))))

(defun path-pair-key (pair) (stamp-for-instances (car pair)))

;;; tag tests

(defun tags-vector () (make-array (length *tag-tests*) :initial-element nil))

(defun class-tag (class) ; what tag corresponds to CLASS?
  (third (find (stamp-for-instances class) *tag-tests* :key #'second)))

(defun compile-tag-test (tags where-test)
  (map-into tags (lambda (ex) (if (null ex) (make-miss) (compile-tree ex))) tags)
  (make-tag-test :tags tags :default where-test))

;;; class tests

;; return whether the two NEXT nodes can be conflated.
;; note: at the moment, non-outcomes are probably never equal
(defun next= (next1 next2)
  (if (typep next1 'outcome)
      (and (typep next2 'outcome) (outcome= next1 next2))
      (eq next1 next2)))

;; Given (class . next-node) pairs, return ((low . high) . next-node) pairs,
;; where low and high are an inclusive range of stamps.
;; Classes must be presorted by stamp.
(defun classes-to-ranges (pairs)
  (flet ((fresh (stamp next) (cons (cons stamp stamp) next)))
    (if (null pairs)
        pairs
        (loop with current = (fresh (stamp-for-instances (car (first pairs)))
                                    (cdr (first pairs)))
              with result = (list current)
              for (class . next) in (rest pairs)
              for stamp = (stamp-for-instances class)
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

(defclass bc-constant-arg ()
  ((%value :initarg :value :reader bc-constant-arg-value)))
(defun make-bc-constant-arg (&key value)
  (early-make-instance bc-constant-arg :value value))

(defclass bc-constant-ref ()
  ((%ref :initarg :ref :reader bc-constant-ref-ref)))
(defun make-bc-constant-ref (&key ref)
  (early-make-instance bc-constant-ref :ref ref))

(defclass bc-label-arg ()
  ((%lip :initarg :lip :reader bc-lip)
   (%index :initarg :index :accessor dtree-index)
   (%delta :initarg :delta :accessor bc-label-arg-delta)))
(defun make-bc-label-arg (&key lip index delta)
  (early-make-instance bc-label-arg :lip lip :index index :delta delta))

(defclass bc-register-arg ()
  ((%index :initarg :index :reader dtree-index)))
(defun make-bc-register-arg (&key index)
  (early-make-instance bc-register-arg :index index))

(defclass bc-instruction ()
  ((%name :initarg :name :reader bc-instruction-name)
   (%lip :initarg :lip :reader bc-lip)
   (%index :initarg :index :reader dtree-index)
   (%code :initarg :code :reader bc-instruction-code)))
(defun make-bc-instruction (&key name lip index code)
  (early-make-instance bc-instruction :name name :lip lip :index index :code code))

(defun longify-instruction (short-instruction instr)
  (declare (ignorable instr))
  (make-bc-instruction :name (bc-instruction-name short-instruction)
                       :code (bc-instruction-code short-instruction)
                       :lip (bc-lip short-instruction)
                       :index (dtree-index short-instruction)))

(defun annotated-opcode (inst)
  (let* ((code-cell (assoc inst *isa*))
         (code (second code-cell)))
    (make-bc-instruction :name inst :code code)))

(defconstant +longify-trigger+ 255)

;; Move constants into the literals vector and replace them with
;; indices in the program.  Also accumulate patchpoints for labels
(defun literalify-arguments (instr literals coallesce-indexes)
  (loop named literalify
        with long-arg = nil
        for annotated-arg in (cdr instr)
        collect (typecase annotated-arg
                  (bc-constant-arg
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
                  (bc-label-arg annotated-arg)
                  (bc-register-arg annotated-arg)
                  (t (error "Illegal arg ~a" annotated-arg)))
          into new-args
        finally (return-from literalify (values new-args long-arg))))

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
                            collect (multiple-value-bind (new-args long-arg)
                                        (literalify-arguments instr literals coallesce-indexes)
                                      (if long-arg
                                          (progn
                                            (format t "Dealing with long instruction ~s~%" instr)
                                            (setf (elt longs index) long-arg)
                                            (longify-instruction annotated-op instr))
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
          for jump-label = (dtree-index annotated-jump-label)
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
                 (setf (first instr) (longify-instruction annotated-op instr)
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
                   (high (logand (ash val -8) #xff)))
               (when (> val 65535)
                 (error "A value ~a larger than 65535 cannot be coded in two bytes - you need a bigger vm" val))
               (vector-push-extend low bytecode)
               (vector-push-extend high bytecode)))
           (arg (arg-val)
             (cond (long (two-byte arg-val bytecode))
                   ((> arg-val +longify-trigger+)
                    (error "This value should be long ~a" arg-val))
                   (t (vector-push-extend arg-val bytecode)))))
    (loop for arg in args
          do (typecase arg
               (bc-constant-ref (arg (bc-constant-ref-ref arg)))
               (bc-label-arg (arg (bc-label-arg-delta arg)))
               (bc-register-arg (arg (dtree-index arg)))
               (t (error "Illegal arg type ~a" arg))))))

(defun bytecodeify (instructions longs bytecode)
  (let* ((ip (length bytecode))
         (saw-long nil)
         (new-instructions (loop for instr in instructions
                                 for long across longs
                                 for annotated-op = (car instr)
                                 for op = (bc-instruction-code annotated-op)
                                 for final-op = (if long
                                                    (progn
                                                      (setf saw-long t)
                                                      (+ op (length *dtree-ops*)))
                                                    op)
                                 for args = (cdr instr)
                                 do (vector-push-extend final-op bytecode)
                                 do (byteify-args args long bytecode)
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
      (loop (typecase tree
              (dtree-argument
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
                 (t (collect (opcode 'argn) (argument-count tree))))
               (next (dtree-next tree)))
              (dtree-tag-test
               (collect (opcode 'tag-test))
               (loop for tag across (tag-test-tags tree)
                     do (wait tag))
               (next (tag-test-default tree)))
              (dtree-stamp-read
               (collect (opcode 'stamp-read))
               (wait (stamp-read-c++ tree))
               (next (stamp-read-other tree)))
              (dtree-<-branch
               (collect (opcode 'lt-branch) (pivot tree))
               (wait (<-branch-left tree))
               (next (<-branch-right tree)))
              (dtree-=-check
               (collect (opcode 'eq-check) (pivot tree))
               (next (dtree-next tree)))
              (dtree-range-check
               (collect (opcode 'range-check)
                 (range-check-min tree)
                 (range-check-max tree))
               (next (dtree-next tree)))
              (dtree-eql-search
               (loop for object across (eql-search-objects tree)
                     for next across (eql-search-nexts tree)
                     do (collect (opcode 'eql)
                          object)
                        (wait next))
               (next (eql-search-default tree)))
              (dtree-miss
               (collect (opcode 'miss))
               (cont))
              (optimized-slot-reader
               (collect
                   (if (core:fixnump (optimized-slot-accessor-index tree))
                       (opcode 'optimized-slot-reader) ; instance
                       (opcode 'car))                  ; class
                 (optimized-slot-accessor-index tree)
                 (optimized-slot-accessor-slot-name tree))
               (cont))
              (optimized-slot-writer
               (collect
                   (if (core:fixnump (optimized-slot-accessor-index tree))
                       (opcode 'optimized-slot-writer) ; instance
                       (opcode 'rplaca))               ; class
                 (optimized-slot-accessor-index tree))
               (cont))
              (effective-method-outcome
               (collect (opcode 'effective-method-outcome)
                 (effective-method-outcome-function tree))
               (cont))
              (t (error "BUG: Unknown dtree: ~a" tree)))))))

;;; Group an instruction into a list. The first element of the list is
;;; a bc-instruction and the remainder of the list are argument objects.
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
          do (setf (gethash (bc-lip op) lip-to-index) (dtree-index op)))
    (loop for instr in instructions
          for args = (rest instr)
          do (loop for arg in args
                   when (typep arg 'bc-label-arg)
                     do (setf (dtree-index arg) (gethash (bc-lip arg) lip-to-index))))))

;;; Bytecode approach

(defun dtree-compile (generic-function)
  (let ((call-history (generic-function-call-history generic-function)))
    (if (null call-history)
        (values (make-miss) 0)
        (multiple-value-bind (basic specialized-length)
            (bc-basic-tree
             (generic-function-call-history generic-function)
             (generic-function-specializer-profile generic-function))
          (values (compile-tree-top basic) specialized-length)))))

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
              (bytecodeify instructions longs bytecode)
            (declare (ignorable saw-long))
            (vector-push-extend entry-ip entry-points)
            (values (copy-seq bytecode) (copy-seq entry-points) (copy-seq literals) specialized-length
                    #| Remaining return values are for debugging |#
                    instructions new-instructions labels grouped compiled)))))))

(defun bytecode-interpreted-discriminator (generic-function)
  (let ((program (sys:gfbytecode-simple-fun/make generic-function)))
    program))

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
