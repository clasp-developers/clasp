(in-package #:cc-hir-to-mir)


(defmethod cleavir-ir:specialize ((instr cleavir-ir:instruction)
				  (impl clasp-cleavir:clasp) proc os)
  ;; By default just return the current instruction
  instr)


(defmethod cleavir-ir:specialize ((instr cleavir-ir:enter-instruction)
				  (impl clasp-cleavir:clasp) proc os)
  (change-class instr 'cc-mir:enter-instruction))



#+(or)(defmethod cleavir-ir:specialize ((instr cleavir-ir:fdefinition-instruction)
				  (impl clasp-cleavir:clasp) proc os)
  (let ((output (first (cleavir-ir:outputs instr))))
    (change-class output 'cc-mir:closure-pointer-dynamic-lexical-location)
    ;;(format t "specializing the fdefinition-instruction new output: ~a~%" output)
    ;; By default just return the current instruction
    instr))


