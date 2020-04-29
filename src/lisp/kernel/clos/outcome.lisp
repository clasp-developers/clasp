(in-package "CLOS")

;;; An "outcome" is a potential "outcome" of a generic function call.
;;; Basically, an outcome represents an effective method function,
;;; only it's simpler in many cases.

;;; Outcomes

(defstruct (outcome (:type vector) :named))
(defstruct (optimized-slot-reader (:type vector) (:include outcome) :named)
  index slot-name method class)
(defstruct (optimized-slot-writer (:type vector) (:include outcome) :named)
  index slot-name method class)
(defstruct (fast-method-call (:type vector) (:include outcome) :named) function)
;; see closfastgf.lsp's find-existing-emf for use of applicable-methods slot
(defstruct (effective-method-outcome (:type vector) (:include outcome) :named)
  applicable-methods (form nil) (function nil))

(defun outcome= (outcome1 outcome2)
  (or (eq outcome1 outcome2) ; covers effective-method-outcome due to closfastgf caching
      (cond ((optimized-slot-reader-p outcome1)
             (and (optimized-slot-reader-p outcome2)
                  ;; could also do class slot locations somehow,
                  ;; but it doesn't seem like a big priority.
                  (fixnump (optimized-slot-reader-index outcome1))
                  (fixnump (optimized-slot-reader-index outcome2))
                  (= (optimized-slot-reader-index outcome1)
                     (optimized-slot-reader-index outcome2))))
            ((optimized-slot-writer-p outcome1)
             (and (optimized-slot-writer-p outcome2)
                  (fixnump (optimized-slot-writer-index outcome1))
                  (fixnump (optimized-slot-writer-index outcome2))
                  (= (optimized-slot-writer-index outcome1)
                     (optimized-slot-writer-index outcome2))))
            ((fast-method-call-p outcome1)
             (and (fast-method-call-p outcome2)
                  (eq (fast-method-call-function outcome1)
                      (fast-method-call-function outcome2))))
            (t nil))))
