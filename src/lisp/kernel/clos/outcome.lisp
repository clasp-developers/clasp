(in-package "CLOS")

;;; An "outcome" is a potential "outcome" of a generic function call.
;;; Basically, an outcome represents an effective method function,
;;; only it's simpler in many cases.

;;; Outcomes

(defstruct (outcome (:type vector) :named) methods)
(defstruct (optimized-slot-reader (:type vector) (:include outcome) :named)
  index slot-name class)
(defstruct (optimized-slot-writer (:type vector) (:include outcome) :named)
  index slot-name class)
(defstruct (effective-method-outcome (:type vector) (:include outcome) :named)
 (form nil) (function nil))

(defun outcome= (outcome1 outcome2)
  (eq outcome1 outcome2)) ; thanks, caching! (in find-existing-outcome)
