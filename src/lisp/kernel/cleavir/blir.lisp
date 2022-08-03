(in-package #:cc-blir)

;;; Low-level instructions, working directly on memory, etc.

(defclass memref2 (bir:one-input bir:one-output bir:instruction)
  ((%offset :initarg :offset :reader offset :type integer)))

(defclass load (cc-bir:atomic bir:one-input bir:one-output bir:instruction)
  ())

(defclass store (cc-bir:atomic bir:no-output bir:instruction)
  ())

(defclass cas (cc-bir:atomic bir:one-output bir:instruction)
  ())
