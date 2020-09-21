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
