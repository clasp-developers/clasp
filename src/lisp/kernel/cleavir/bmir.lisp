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

(defclass datum (bir:datum)
  ((%rtype :initarg :rtype :accessor rtype)))

(defclass output (datum bir:output) ())
(defclass phi (datum bir:phi) ())

(defmethod rtype ((datum bir:variable)) '(:object))
(defmethod rtype ((datum bir:argument)) '(:object))
(defmethod rtype ((datum bir:load-time-value)) '(:object))
(defmethod rtype ((datum bir:constant)) '(:object))
