(in-package #:cc-bmir)

(defclass type-branch (cleavir-bir::one-input cleavir-bir:terminator
                       cleavir-bir:operation)
  ())

(defclass fixnump (type-branch) ())
(defclass consp (type-branch) ())
(defclass characterp (type-branch) ())
(defclass single-float-p (type-branch) ())
(defclass generalp (type-branch) ())
(defclass headerq (type-branch)
  ((%info :initarg :info :reader info)))

(defclass memref2 (cleavir-bir::one-input cleavir-bir:computation)
  ((%offset :initarg :offset :reader offset :type integer)))
(defmethod cleavir-bir:rtype ((mr memref2)) :address)

(defclass load (cc-bir:atomic cleavir-bir::one-input cleavir-bir:computation)
  ())
(defmethod cleavir-bir:rtype ((l load)) :object)

(defclass store (cc-bir:atomic cleavir-bir::no-output cleavir-bir:operation) ())

(defclass cas (cc-bir:atomic cleavir-bir:computation) ())
(defmethod cleavir-bir:rtype ((c cas)) :object)
