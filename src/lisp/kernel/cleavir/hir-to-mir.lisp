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
  (change-class instr 'cleavir-ir:memref2-instruction
                :inputs (list (first (cleavir-ir:inputs instr))
                              (cleavir-ir:make-immediate-input (- cmp:+cons-car-offset+ cmp:+cons-tag+)))
                :outputs (cleavir-ir:outputs instr)))

(defmethod cleavir-ir:specialize ((instr cleavir-ir:cdr-instruction)
                                  (impl clasp-cleavir:clasp) proc os)
  (change-class instr 'cleavir-ir:memref2-instruction
                :inputs (list (first (cleavir-ir:inputs instr))
                              (cleavir-ir:make-immediate-input (- cmp:+cons-cdr-offset+ cmp:+cons-tag+)))
                :outputs (cleavir-ir:outputs instr)))


(defmethod cleavir-ir:specialize ((instr cleavir-ir:rplaca-instruction)
                                  (impl clasp-cleavir:clasp) proc os)
  #+(or)(cleavir-ir:insert-instruction-after
         (cleavir-ir:make-assignment-instruction
          (first (cleavir-ir:inputs instr))
          (first (cleavir-ir:outputs instr)))
         instr)
  (change-class instr 'cleavir-ir:memset2-instruction
                :inputs (list (first (cleavir-ir:inputs instr))
                              (cleavir-ir:make-immediate-input (- cmp:+cons-car-offset+ cmp:+cons-tag+))
                              (second (cleavir-ir:inputs instr)))
                :outputs nil))


(defmethod cleavir-ir:specialize ((instr cleavir-ir:rplacd-instruction)
                                  (impl clasp-cleavir:clasp) proc os)
  #+(or)(cleavir-ir:insert-instruction-after
         (cleavir-ir:make-assignment-instruction
          (first (cleavir-ir:inputs instr))
          (first (cleavir-ir:outputs instr)))
         instr)
  (change-class instr 'cleavir-ir:memset2-instruction
                :inputs (list (first (cleavir-ir:inputs instr))
                              (cleavir-ir:make-immediate-input (- cmp:+cons-cdr-offset+ cmp:+cons-tag+))
                              (second (cleavir-ir:inputs instr)))
                :outputs nil))


