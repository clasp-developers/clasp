(in-package #:cc-hir-to-mir)


(defmethod cleavir-ir:specialize ((instr cleavir-ir:instruction)
				  (impl clasp-cleavir:clasp) proc os)
  ;; By default just return the current instruction
  instr)


#+(or)(defmethod cleavir-ir:specialize ((instr cleavir-ir:enter-instruction)
				  (impl clasp-cleavir:clasp) proc os)
  (change-class instr 'cc-mir:enter-instruction))



(defmethod cleavir-ir:specialize ((instr cleavir-ir:car-instruction)
                                  (impl clasp-cleavir:clasp) proc os)
  (change-class instr 'cleavir-ir:memref-instruction
                :offset (- cmp:+cons-car-offset+ cmp:+cons-tag+)))

(defmethod cleavir-ir:specialize ((instr cleavir-ir:cdr-instruction)
                                  (impl clasp-cleavir:clasp) proc os)
  (change-class instr 'cleavir-ir:memref-instruction
                :offset (- cmp:+cons-cdr-offset+ cmp:+cons-tag+)))


(defmethod cleavir-ir:specialize ((instr cleavir-ir:rplaca-instruction)
                                  (impl clasp-cleavir:clasp) proc os)
  (change-class instr 'cleavir-ir:memset-instruction
                :inputs (list (second (cleavir-ir:inputs instr))
                              (first (cleavir-ir:inputs instr)))
                :offset (- cmp:+cons-car-offset+ cmp:+cons-tag+)))

(defmethod cleavir-ir:specialize ((instr cleavir-ir:rplacd-instruction)
                                  (impl clasp-cleavir:clasp) proc os)
  (change-class instr 'cleavir-ir:memset-instruction
                :inputs (list (second (cleavir-ir:inputs instr))
                              (first (cleavir-ir:inputs instr)))
                :offset (- cmp:+cons-car-offset+ cmp:+cons-tag+)))


(defmethod cleavir-ir:specialize ((instr cleavir-ir:aref-instruction)
                                  (impl clasp-cleavir:clasp) proc os)
  (unless (and (cleavir-ir:simple-p instr) (eq (cleavir-ir:element-type instr) t))
    (error "BUG: Aref instruction we don't know how to deal with generated ~s" instr))
  (change-class instr 'cleavir-ir:memref-instruction
                :offset (- cmp:+simple-vector._length-offset+ cmp:+general-tag+)
                :scale (list 1 cmp::+t-size+)))

(defmethod cleavir-ir:specialize ((instr cleavir-ir:aset-instruction)
                                  (impl clasp-cleavir:clasp) proc os)
  (unless (and (cleavir-ir:simple-p instr) (eq (cleavir-ir:element-type instr) t))
    (error "BUG: Aref instruction we don't know how to deal with generated ~s" instr))
  (destructuring-bind (array index value) (cleavir-ir:inputs instr)
    (change-class instr 'cleavir-ir:memref-instruction
                  :inputs (list value array index)
                  :offset (- cmp:+simple-vector._length-offset+ cmp:+general-tag+)
                  :scale (list 1 cmp::+t-size+))))

(defmethod cleavir-hir-transformations::maybe-eliminate :around ((instruction cleavir-ir:typeq-instruction))
  "This is HIR to MIR translation done by eliminate-typeq"
  (let ((type (cleavir-ir:value-type instruction)))
    (cond ((and (subtypep type 'character) (subtypep 'character type))
           (change-class instruction 'cc-mir:characterp-instruction))
          ((and (subtypep type 'single-float) (subtypep 'single-float type))
           (change-class instruction 'cc-mir:single-float-p-instruction))
          (t (call-next-method)))))

