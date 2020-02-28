(in-package #:cc-hir-to-mir)


(defmethod cleavir-hir-to-mir:specialize ((instr cleavir-ir:instruction)
                                          (impl clasp-cleavir:clasp) proc os)
  ;; By default just return the current instruction
  instr)


(defmethod cleavir-hir-to-mir:specialize ((instr cleavir-ir:car-instruction)
                                          (impl clasp-cleavir:clasp) proc os)
  (change-class instr 'cleavir-ir:memref2-instruction
                :inputs (list (first (cleavir-ir:inputs instr)))
                :offset (- cmp:+cons-car-offset+ cmp:+cons-tag+)
                :outputs (cleavir-ir:outputs instr)))

(defmethod cleavir-hir-to-mir:specialize ((instr cleavir-ir:cdr-instruction)
                                          (impl clasp-cleavir:clasp) proc os)
  (change-class instr 'cleavir-ir:memref2-instruction
                :inputs (list (first (cleavir-ir:inputs instr)))
                :offset (- cmp:+cons-cdr-offset+ cmp:+cons-tag+)
                :outputs (cleavir-ir:outputs instr)))


(defmethod cleavir-hir-to-mir:specialize ((instr cleavir-ir:rplaca-instruction)
                                          (impl clasp-cleavir:clasp) proc os)
  (change-class instr 'cleavir-ir:memset2-instruction
                :inputs (list (first (cleavir-ir:inputs instr))
                              (second (cleavir-ir:inputs instr)))
                :offset (- cmp:+cons-car-offset+ cmp:+cons-tag+)
                :outputs nil))

(defmethod cleavir-hir-to-mir:specialize ((instr cleavir-ir:rplacd-instruction)
                                          (impl clasp-cleavir:clasp) proc os)
  (change-class instr 'cleavir-ir:memset2-instruction
                :inputs (list (first (cleavir-ir:inputs instr))
                              (second (cleavir-ir:inputs instr)))
                :offset (- cmp:+cons-cdr-offset+ cmp:+cons-tag+)
                :outputs nil))


(defmethod cleavir-hir-to-mir:specialize ((instr clasp-cleavir-hir:cas-car-instruction)
                                          (impl clasp-cleavir:clasp) proc os)
  (declare (ignore proc os))
  (change-class instr 'cc-mir:memcas2-instruction
                :inputs (cleavir-ir:inputs instr)
                :offset (- cmp:+cons-car-offset+ cmp:+cons-tag+)
                :outputs (cleavir-ir:outputs instr)))

(defmethod cleavir-hir-to-mir:specialize ((instr clasp-cleavir-hir:cas-cdr-instruction)
                                          (impl clasp-cleavir:clasp) proc os)
  (declare (ignore proc os))
  (change-class instr 'cc-mir:memcas2-instruction
                :inputs (cleavir-ir:inputs instr)
                :offset (- cmp:+cons-cdr-offset+ cmp:+cons-tag+)
                :outputs (cleavir-ir:outputs instr)))


(defmethod cleavir-hir-to-mir:specialize ((instr cleavir-ir:fetch-instruction)
                                          (impl clasp-cleavir:clasp) proc os)
  (let ((env (first (cleavir-ir:inputs instr)))
        (idx (cleavir-ir:value (second (cleavir-ir:inputs instr)))))
    (change-class instr 'cleavir-ir:memref2-instruction
                  :inputs (list env)
                  :offset (cmp:%closure-with-slots%.offset-of[n]/t* idx)
                  :outputs (cleavir-ir:outputs instr))))

(defmethod cleavir-hir-to-mir:specialize ((instr cleavir-ir:read-cell-instruction)
                                          (impl clasp-cleavir:clasp) proc os)
  (change-class instr 'cleavir-ir:memref2-instruction
                :inputs (list (first (cleavir-ir:inputs instr)))
                :offset (- cmp:+cons-car-offset+ cmp:+cons-tag+)
                :outputs (cleavir-ir:outputs instr)))

(defmethod cleavir-hir-to-mir:specialize ((instr cleavir-ir:write-cell-instruction)
                                          (impl clasp-cleavir:clasp) proc os)
  (change-class instr 'cleavir-ir:memset2-instruction
                :inputs (list (first (cleavir-ir:inputs instr))
                              (second (cleavir-ir:inputs instr)))
                :offset (- cmp:+cons-car-offset+ cmp:+cons-tag+)
                :outputs (cleavir-ir:outputs instr)))

(defun gen-sv-call (fname args result succ)
  (let ((fdef (cleavir-ir:new-temporary))
        (vals (cleavir-ir:make-values-location)))
    (cleavir-ir:make-fdefinition-instruction
     (cleavir-ir:make-constant-input fname) fdef
     (cleavir-ir:make-funcall-instruction
      (list* fdef args)
      (list vals)
      (cleavir-ir:make-multiple-to-fixed-instruction
       vals
       (list result)
       succ)))))

(defun gen-branch-call (fname args pro con)
  (let ((fdef (cleavir-ir:new-temporary))
        (vals (cleavir-ir:make-values-location))
        (bool (cleavir-ir:new-temporary)))
    (cleavir-ir:make-fdefinition-instruction
     (cleavir-ir:make-constant-input fname) fdef
     (cleavir-ir:make-funcall-instruction
      (list* fdef args)
      (list vals)
      (cleavir-ir:make-multiple-to-fixed-instruction
       vals
       (list bool)
       (cleavir-ir:make-eq-instruction
        (list bool (cleavir-ir:make-constant-input 'nil))
        (list con pro)))))))

;;; clasp doesn't like it when a funcall receives immediate arguments.
;;; we make do.
(defmacro with-constant ((var value) form)
  (let ((const (gensym "CONST")))
    `(let ((,const (cleavir-ir:make-constant-input ,value))
           (,var (cleavir-ir:new-temporary)))
       (cleavir-ir:make-assignment-instruction
        ,const
        ,var
        ,form))))

(defun gen-class-check (object class-name pro con)
  (let ((low (cleavir-ir:new-temporary))
        (high (cleavir-ir:new-temporary)))
    (gen-sv-call 'class-of (list object) low
                 (with-constant (cn class-name)
                   (gen-sv-call 'find-class (list cn) high
                                (gen-branch-call 'clos::subclassp (list low high) pro con))))))

(defun gen-typep-check (object type pro con)
  (when (symbolp type)
    ;; We can sometimes call a predicate instead.
    (let ((f (core::simple-type-predicate type)))
      (when f
        (return-from gen-typep-check
          (gen-branch-call f (list object) pro con))))
    ;; Or if it's a class, we can call clos:subclassp
    ;; (Classes, like types, have to stay at least kind of constant between compile and load,
    ;;  according to the semantic constraints.)
    (let ((c (find-class type nil)))
      (when c
        (return-from gen-typep-check
          (gen-class-check object type pro con)))))
  (with-constant (ty type)
    (gen-branch-call 'typep (list object ty) pro con)))

(defun gen-eql-check (object1 literal pro con)
  (with-constant (object2 literal)
    (if (typep literal '(and number (not fixnum) (not single-float))) ; non-eq-comparable
        (gen-branch-call 'eql (list object1 object2) pro con)
        (cleavir-ir:make-eq-instruction
         (list object1 object2)
         (list pro con)))))

(defun gen-dimension-check (object dim spec pro con)
  (if (eq spec '*)
      pro ; don't need a nop as this will not be returned from gen-array-type-check
      (let ((arrayd (cleavir-ir:new-temporary)))
        (with-constant (d dim)
          (gen-sv-call 'array-dimension
                       (list object d)
                       arrayd
                       (gen-eql-check arrayd spec pro con))))))

(defun gen-rank-check (object rank pro con)
  (let ((arrayr (cleavir-ir:new-temporary)))
    (gen-sv-call 'array-rank
                 (list object)
                 arrayr
                 (gen-eql-check arrayr rank pro con))))

;;; The way some of the complicated ones are built is a little backwards.
;;; We repeatedly set one of the branches, so that the next test produces branches
;;; to that test.
;;; For instance, to test (array * 2), we might first setf pro to a rank check,
;;; then set con to check that it's an array, then return con.
;;; The check that it's an array will use the 'pro' leading to a rank check, so that
;;; the test when run will first check that it's an array, and if it is, go to the
;;; rank check.
;;; Basically this means we generate the tests in reverse order to how they're run.
;;; We don't do this for arrays any more though.
;;; This should all be source-level but we are unprepared.

(defun gen-array-type-check (object element-type dimensions simple-only-p pro con)
  (let* ((dimensions (if (integerp dimensions) (make-list dimensions :initial-element '*) dimensions))
         (rank (if (eq dimensions '*) '* (length dimensions)))
         (simple-vector-type (core::simple-vector-type element-type))
         (complex-vector-type (core::complex-vector-type element-type))
         (simple-mdarray-type (core::simple-mdarray-type element-type))
         (mdarray-type (core::complex-mdarray-type element-type)))
    (let ((pro (if (or (eq dimensions '*) (null dimensions))
                   pro
                   (loop for dim in dimensions
                         for i from 0
                         for test = (gen-dimension-check object i dim pro con)
                           then (gen-dimension-check object i dim test con)
                         finally (return test)))))
      (cond (simple-only-p
             (cond ((eql rank 1)
                    (maybe-gen-primitive-type-check
                     object simple-vector-type pro con))
                   ((eq rank '*)
                    (maybe-gen-primitive-type-check
                     object simple-vector-type pro
                     (maybe-gen-primitive-type-check
                      object simple-mdarray-type pro con)))
                   (t
                    (maybe-gen-primitive-type-check
                     object simple-mdarray-type
                     (gen-rank-check object rank pro con)
                     con))))
            (t
             (cond ((eql rank 1)
                    ;; check for vector types only
                    ;; (and so we don't need a rank check)
                    (maybe-gen-primitive-type-check
                     object simple-vector-type pro
                     (maybe-gen-primitive-type-check
                      object complex-vector-type pro con)))
                   ((eq rank '*)
                    ;; check for both vector and mdarray types
                    ;; (and no rank check because it wasn't specified)
                    (maybe-gen-primitive-type-check
                     object simple-vector-type pro
                     (maybe-gen-primitive-type-check
                      object complex-vector-type pro
                      (maybe-gen-primitive-type-check
                       object simple-mdarray-type pro
                       (maybe-gen-primitive-type-check
                        object mdarray-type pro con)))))
                   (t
                    ;; check for mdarray types only,
                    ;; and check the rank.
                    (maybe-gen-primitive-type-check
                     object simple-mdarray-type pro
                     (maybe-gen-primitive-type-check
                      object mdarray-type
                      (gen-rank-check object rank pro con) con)))))))))

(defun gen-interval-type-check (object head low high pro con)
  (let ((prims
          (ecase head
            ;; We can primitive check multiple types at once, sometimes.
            ;; but this doesn't work if one is fixnum or single-float, because
            ;; those aren't Generals. So we just split it up.
            ((integer)
             ;; special case fixnum
             (if (and (or (eql low most-negative-fixnum)
                          (and (listp low) (eql (car low) (1- most-negative-fixnum))))
                      (or (eql high most-positive-fixnum)
                          (and (listp high) (eql (car high) (1+ most-positive-fixnum)))))
                 (progn (setf low '* high '*) '(fixnum))
                 '(fixnum bignum)))
            ((rational) '(fixnum bignum ratio))
            ;; singles and doubles always exist.
            ;; if shorts don't exist, they're singles. same with long and double.
            #+short-float ((short-float) '(short-float))
            ((#-short-float short-float single-float) '(single-float))
            #+long-float ((long-float) '(long-float))
            ((#-long-float long-float double-float) '(double-float))
            ((float) '(#+short-float short-float single-float
                       double-float #+long-float long-float))
            ((real) '(fixnum bignum ratio #+short-float short-float
                      single-float double-float #+long-float long-float))
            (otherwise (error "BUG: Unknown type head ~a passed to gen-interval-type-check"
                              head)))))
    (unless (eq high '*)
      (setf pro
            (if (listp high)
                (with-constant (hi (first high)) (gen-branch-call '< (list object hi) pro con))
                (with-constant (hi high) (gen-branch-call '<= (list object hi) pro con)))))
    (unless (eq low '*)
      (setf pro
            (if (listp low)
                (with-constant (lo (first low)) (gen-branch-call '> (list object lo) pro con))
                (with-constant (lo low) (gen-branch-call '>= (list object lo) pro con)))))
    (loop for prim in prims
          do (setf con (maybe-gen-primitive-type-check object prim pro con)))
    con))

(defun maybe-gen-primitive-type-check (object primitive-type pro con)
  (case primitive-type
    ((fixnum) (cleavir-ir:make-fixnump-instruction object (list pro con)))
    ((cons) (cleavir-ir:make-consp-instruction object (list pro con)))
    ((character) (cc-mir:make-characterp-instruction object (list pro con)))
    ((single-float) (cc-mir:make-single-float-p-instruction object (list pro con)))
    ((core:general) (cc-mir:make-generalp-instruction object (list pro con)))
    (t (let ((header-info (gethash primitive-type core:+type-header-value-map+)))
         (cond (header-info
                (check-type header-info (or integer cons)) ; sanity check
                (cc-mir:make-headerq-instruction header-info object (list pro con)))
               (t (gen-typep-check object primitive-type pro con)))))))

(defun gen-type-check (object type pro con)
  (multiple-value-bind (head args) (core::normalize-type type)
    (case head
      ((t) (cleavir-ir:make-nop-instruction (list pro)))
      ((nil) (cleavir-ir:make-nop-instruction (list con)))
      ((and) (loop with pro = (cleavir-ir:make-nop-instruction (list pro))
                   for type in args
                   do (setf pro (gen-type-check object type pro con))
                   finally (return pro)))
      ((or) (loop with con = (cleavir-ir:make-nop-instruction (list con))
                  for type in args
                  do (setf con (gen-type-check object type pro con))
                  finally (return con)))
      ((not) (gen-type-check object (first args) con pro))
      ((eql) (gen-eql-check object (first args) pro con))
      ((member) (loop with con = (cleavir-ir:make-nop-instruction (list con))
                      for literal in args
                      do (setf con (gen-eql-check object literal pro con))
                      finally (return con)))
      ((cons)
       (destructuring-bind (&optional (cart '*) (cdrt '*)) args
         (maybe-gen-primitive-type-check
          object 'cons
          (let* ((cdr-branch
                   (if (eq cdrt '*)
                       pro
                       (let ((cdro (cleavir-ir:new-temporary)))
                         (cleavir-ir:make-cdr-instruction
                          object cdro
                          (gen-type-check cdro cdrt pro con))))))
            (if (eq cart '*)
                cdr-branch
                (let ((caro (cleavir-ir:new-temporary)))
                  (cleavir-ir:make-car-instruction
                   object caro
                   (gen-type-check caro cart cdr-branch con)))))
          con)))
      ((simple-array array)
       (destructuring-bind (&optional (et '*) (dims '*)) args
         (gen-array-type-check
          object (if (eq et '*) '* (upgraded-array-element-type et))
          dims (eq head 'simple-array)
          pro con)))
      ((#+short-float short-float single-float
        double-float #+long-float long-float
        float integer rational real)
       (destructuring-bind (&optional (low '*) (high '*)) args
         (gen-interval-type-check object head low high pro con)))
      ((complex) ; we don't have multiple complex types
       (maybe-gen-primitive-type-check object 'complex pro con))
      ((number)
       ;;; have to special case cos of fixnum and single-float.
       (gen-interval-type-check
        object 'real '* '* pro
        (maybe-gen-primitive-type-check object 'complex pro con)))
      ((function)
       (if args ; runtime error. we should warn.
           (gen-typep-check object type pro con)
           (maybe-gen-primitive-type-check object 'function pro con)))
      ((stream core:string-input-stream synonym-stream file-stream
               concatenated-stream echo-stream core:string-output-stream
               two-way-stream string-stream core:iostream-stream
               core:iofile-stream ext:ansi-stream broadcast-stream)
       ;; Can't use primitive typeq due to gray-streams, i.e. user subclassing.
       (gen-typep-check object type pro con))
      ((standard-object)
       ;; Header check doesn't work. Don't know why not.
       (gen-typep-check object type pro con))
      ((values) ; runtime error. we should warn.
       (gen-typep-check object type pro con))
      (t (if args
             (gen-typep-check object type pro con) ; unknown compound type
             (maybe-gen-primitive-type-check object head pro con))))))

(defun replace-typeq (typeq-instruction)
  (let ((object (first (cleavir-ir:inputs typeq-instruction)))
        (type (cleavir-ir:value-type typeq-instruction))
        (pro (first (cleavir-ir:successors typeq-instruction)))
        (con (second (cleavir-ir:successors typeq-instruction)))
        (preds (cleavir-ir:predecessors typeq-instruction))
        (cleavir-ir:*policy* (cleavir-ir:policy typeq-instruction))
        (cleavir-ir:*origin* (cleavir-ir:origin typeq-instruction))
        (cleavir-ir:*dynamic-environment*
          (cleavir-ir:dynamic-environment typeq-instruction)))
    (let ((new (gen-type-check object type pro con)))
      (dolist (pred preds)
        (setf (cleavir-ir:successors pred)
              (substitute new typeq-instruction (cleavir-ir:successors pred)))))))

(defun reduce-typeqs (initial-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (i)
     (when (typep i 'cleavir-ir:typeq-instruction)
       (replace-typeq i)))
   initial-instruction)
  (cleavir-ir:set-predecessors initial-instruction))


(defmethod cleavir-hir-transformations::maybe-eliminate :around ((instruction cleavir-ir:typeq-instruction))
  "This is HIR to MIR translation done by eliminate-typeq"
  (let ((type (cleavir-ir:value-type instruction)))
    (cond ((and (subtypep type 'character) (subtypep 'character type))
           (change-class instruction 'cc-mir:characterp-instruction))
          ((and (subtypep type 'single-float) (subtypep 'single-float type))
           (change-class instruction 'cc-mir:single-float-p-instruction))
          (t (call-next-method)))))

(defun convert-to-tll (lexical-location type)
  (unless (typep lexical-location 'cc-mir:typed-lexical-location)
    (change-class lexical-location 'cc-mir:typed-lexical-location :type type)))

(defun convert-tll-list (list type)
  (mapc (lambda (ll) (convert-to-tll ll type)) list))

(defmethod cleavir-hir-to-mir:specialize ((instruction cleavir-ir:box-instruction)
                                  (impl clasp-cleavir:clasp) proc os)
  (convert-tll-list (cleavir-ir:inputs instruction)
                    (cmp::element-type->llvm-type (cleavir-ir:element-type instruction)))
  instruction)

(defmethod cleavir-hir-to-mir:specialize ((instruction cleavir-ir:unbox-instruction)
                                  (impl clasp-cleavir:clasp) proc os)
  (convert-tll-list (cleavir-ir:outputs instruction)
                    (cmp::element-type->llvm-type (cleavir-ir:element-type instruction)))
  instruction)

(defmethod cleavir-hir-to-mir:specialize ((instruction clasp-cleavir-hir:save-values-instruction)
                                          (impl clasp-cleavir:clasp) proc os)
  (let ((outputs (cleavir-ir:outputs instruction)))
    (convert-to-tll (first outputs) cmp:%size_t%)
    (convert-to-tll (second outputs) cmp:%t**%)
    instruction))

(defmethod cleavir-hir-to-mir:specialize ((instruction clasp-cleavir-hir:load-values-instruction)
                                          (impl clasp-cleavir:clasp) proc os)
  (let ((inputs (cleavir-ir:inputs instruction)))
    (convert-to-tll (first inputs) cmp:%size_t%)
    (convert-to-tll (second inputs) cmp:%t**%)
    instruction))

(defmacro define-float-specializer (instruction-class-name)
  `(defmethod cleavir-hir-to-mir:specialize ((instruction ,instruction-class-name)
                                     (impl clasp-cleavir:clasp) proc os)
     (declare (ignore proc os))
     ;; Could probably work out the llvm types ahead of time, but this shouldn't be a big deal.
     (convert-tll-list (cleavir-ir:inputs instruction)
                       (cmp::element-type->llvm-type (cleavir-ir:subtype instruction)))
     (convert-tll-list (cleavir-ir:outputs instruction)
                       (cmp::element-type->llvm-type (cleavir-ir:subtype instruction)))
     instruction))

(define-float-specializer cleavir-ir:float-add-instruction)
(define-float-specializer cleavir-ir:float-sub-instruction)
(define-float-specializer cleavir-ir:float-mul-instruction)
(define-float-specializer cleavir-ir:float-div-instruction)

(define-float-specializer cleavir-ir:float-less-instruction)
(define-float-specializer cleavir-ir:float-not-greater-instruction)
(define-float-specializer cleavir-ir:float-equal-instruction)
