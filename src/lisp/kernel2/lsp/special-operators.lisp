(in-package #:core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Set the fdefinition for all special operators to something more reasonable than T
;;; For known operators, put in a function with correct lambda list for the sake of
;;; documentation.
;;;
(macrolet
    ((def-special-operator-function (name lambda-list &optional (vars lambda-list))
       `(unless (fboundp ',name)
          (setf (fdefinition ',name)
                (lambda ,lambda-list
                  (declare (ignore ,@vars))
                  (error 'do-not-funcall-special-operator :operator ',name))))))
  (def-special-operator-function progn (&rest forms) (forms))
  (def-special-operator-function block (name &rest forms) (name forms))
  (def-special-operator-function catch (tag &rest forms) (tag forms))
  (def-special-operator-function eval-when (situations &rest forms) (situations forms))
  (def-special-operator-function flet (bindings &rest body) (bindings body))
  (def-special-operator-function function (thing))
  (def-special-operator-function the (values-type form))
  (def-special-operator-function go (tag))
  (def-special-operator-function if (condition then else))
  (def-special-operator-function labels (bindings &rest body) (bindings body))
  (def-special-operator-function let (bindings &rest body) (bindings body))
  (def-special-operator-function let* (bindings &rest body) (bindings body))
  (def-special-operator-function locally (&rest body) (body))
  (def-special-operator-function macrolet (bindings &rest body) (bindings body))
  (def-special-operator-function multiple-value-prog1 (values-form &rest forms) (values-form forms))
  (def-special-operator-function multiple-value-call (function &rest args) (function args))
  (def-special-operator-function progv (symbols values &rest forms) (symbols values forms))
  (def-special-operator-function quote (object))
  (def-special-operator-function return-from (name &optional value) (name value))
  (def-special-operator-function setq (&rest pairs) (pairs))
  (def-special-operator-function tagbody (&rest statements) (statements))
  (def-special-operator-function throw (tag result-form))
  (def-special-operator-function unwind-protect (protected &rest cleanup) (protected cleanup))
  (def-special-operator-function symbol-macrolet (bindings &rest body) (bindings body))
  (def-special-operator-function load-time-value (form &optional read-only-p) (form read-only-p)))
