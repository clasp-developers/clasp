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

;;;

(defgeneric rtype (datum))
(defmethod rtype ((datum bir:variable)) :object)

;; Given a user (instruction) and a datum, determine the rtype required.
(defgeneric use-rtype (instruction datum))
(defmethod use-rtype ((inst bir:instruction) (datum bir:datum))
  ;; Having this as a default is mildly dicey but should work: instructions
  ;; that need multiple value inputs are a definite minority.
  :object)
(defmethod use-rtype ((inst bir:mv-call) (datum bir:datum))
  (if (member datum (rest (bir:inputs inst)))
      :multiple-values :object))
(defmethod use-rtype ((inst bir:mv-local-call) (datum bir:datum))
  (if (member datum (rest (bir:inputs inst)))
      :multiple-values :object))
(defmethod use-rtype ((inst bir:returni) (datum bir:datum)) :multiple-values)
(defmethod use-rtype ((inst bir:values-save) (datum bir:datum))
  :multiple-values)
(defmethod use-rtype ((inst bir:values-collect) (datum bir:datum))
  :multiple-values)
(defmethod use-rtype ((inst bir:unwind) (datum bir:datum))
  ;; FIXME: This is essentially a Clasp implementation limitation. It should be
  ;; possible to return single values in many cases.
  :multiple-values)
(defmethod use-rtype ((inst bir:jump) (datum bir:datum))
  (error "BUG: transitive-rtype should make this impossible!"))
;; FIXME: multiple-to-fixed will be removed
(defmethod use-rtype ((inst bir:multiple-to-fixed) (datum bir:datum))
  :multiple-values)
(defmethod use-rtype ((inst bir:thei) (datum bir:datum))
  ;; actual type tests, which need multiple values, should have been turned
  ;; into mv calls by this point. but out of an abundance of caution,
  (if (symbolp (bir:type-check-function inst))
      (rtype (first (bir:outputs inst)))
      :multiple-values))
             
;; Determine the rtype of a datum by chasing transitive use.
(defun transitive-rtype (datum)
  (loop (let ((use (bir:use datum)))
          (etypecase use
            (null (return :object)) ; unused, doesn't matter
            (bir:jump
             (setf datum (nth (position datum (bir:inputs use))
                              (bir:outputs use))))
            (bir:instruction (return (use-rtype use datum)))))))
(defmethod rtype ((datum bir:phi)) (transitive-rtype datum))
(defmethod rtype ((datum bir:output)) (transitive-rtype datum))
(defmethod rtype ((datum bir:argument)) (transitive-rtype datum))
(defmethod rtype ((datum bir:load-time-value)) :object)
(defmethod rtype ((datum bir:constant)) :object)
