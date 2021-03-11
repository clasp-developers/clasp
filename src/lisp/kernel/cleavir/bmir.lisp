(in-package #:cc-bmir)

(defclass fixnump (cleavir-bir:conditional-test) ())
(defclass consp (cleavir-bir:conditional-test) ())
(defclass characterp (cleavir-bir:conditional-test) ())
(defclass single-float-p (cleavir-bir:conditional-test) ())
(defclass generalp (cleavir-bir:conditional-test) ())
(defclass headerq (cleavir-bir:conditional-test)
  ((%info :initarg :info :reader info)))

(defclass memref2 (cleavir-bir:one-input cleavir-bir:one-output
                   cleavir-bir:instruction)
  ((%offset :initarg :offset :reader offset :type integer)))

(defclass load (cc-bir:atomic cleavir-bir:one-input cleavir-bir:one-output
                cleavir-bir:instruction)
  ())

(defclass store (cc-bir:atomic cleavir-bir:no-output cleavir-bir:instruction)
  ())

(defclass cas (cc-bir:atomic cleavir-bir:one-output cleavir-bir:instruction)
  ())
