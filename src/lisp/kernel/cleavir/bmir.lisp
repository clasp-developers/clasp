(in-package #:cc-bmir)

(defclass type-branch (cleavir-bir::one-input cleavir-bir::no-output
                       cleavir-bir:terminator)
  ())

(defclass fixnump (type-branch) ())
(defclass consp (type-branch) ())
(defclass characterp (type-branch) ())
(defclass single-float-p (type-branch) ())
(defclass generalp (type-branch) ())
(defclass headerq (type-branch)
  ((%info :initarg :info :reader info)))

(defclass memref2 (cleavir-bir::one-input cleavir-bir::one-output
                   cleavir-bir:instruction)
  ((%offset :initarg :offset :reader offset :type integer)))

(defclass load (cc-bir:atomic cleavir-bir::one-input cleavir-bir::one-output
                cleavir-bir:instruction)
  ())

(defclass store (cc-bir:atomic cleavir-bir::no-output cleavir-bir:instruction)
  ())

(defclass cas (cc-bir:atomic cleavir-bir::one-output cleavir-bir:instruction)
  ())
