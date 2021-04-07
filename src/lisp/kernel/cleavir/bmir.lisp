(in-package #:cc-bmir)

(defclass fixnump (bir:conditional-test) ())
(defclass consp (bir:conditional-test) ())
(defclass characterp (bir:conditional-test) ())
(defclass single-float-p (bir:conditional-test) ())
(defclass generalp (bir:conditional-test) ())
(defclass headerq (bir:conditional-test)
  ((%info :initarg :info :reader info)))

(defclass memref2 (bir:one-input bir:one-output bir:instruction)
  ((%offset :initarg :offset :reader offset :type integer)))

(defclass load (cc-bir:atomic bir:one-input bir:one-output bir:instruction)
  ())

(defclass store (cc-bir:atomic bir:no-output bir:instruction)
  ())

(defclass cas (cc-bir:atomic bir:one-output bir:instruction)
  ())
