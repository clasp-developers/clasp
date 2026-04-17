(in-package #:clasp-tests)

;;; We use these instead of CL or other symbols so that we can know
;;; exactly what declarations are on them, if any.
(defvar *global-special*)
(define-symbol-macro global-symbol-macro 0)

;;; TODO: Put in tests for reading declarations, once we can actually
;;; do that.

(macrolet ((var (var &environment e)
             `(values
               ,@(multiple-value-bind (kind localp decls)
                     (clasp-cltl2:variable-information var e)
                   (declare (ignore decls))
                   (list kind (not (not localp)))))))
  (test cltl2.unbound-variable
    (var x)
    (nil nil))
  (test cltl2.lexical-variable
    (let ((x 0)) x (var x)) ; stray x to make variable unignored
    (:lexical t))
  (test cltl2.global-special-variable
    (var *global-special*)
    (:special nil))
  (test cltl2.local-special-variable
    (let ((x 0)) (declare (special x)) (var x))
    (:special t))
  (test cltl2.global-symbol-macro
    (var global-symbol-macro)
    (:symbol-macro nil))
  (test cltl2.local-symbol-macro
    (symbol-macrolet ((x 0)) (var x))
    (:symbol-macro t)))

(defmacro global-macro ())
(defun global-function ())

(macrolet ((fun (fun &environment e)
             `(values
               ,@(multiple-value-bind (kind localp decls)
                     (clasp-cltl2:function-information fun e)
                   (declare (ignore decls))
                   (list kind (not (not localp)))))))
  (test cltl2.unbound-function
    (fun x)
    (nil nil))
  (test cltl2.special-operator
    (fun catch)
    (:special-form nil))
  (test cltl2.global-macro
    (fun global-macro)
    (:macro nil))
  (test cltl2.local-macro
    (macrolet ((f ())) (fun f))
    (:macro t))
  ;; Because global-function was defined by defun above, when this file
  ;; is compiled, global-function should be known by the compiler but
  ;; not bound. So if cltl2:function-information only checks for bound
  ;; functions, it will miss global-function.
  ;; We force a test with eval to see how it works with an actual bound
  ;; function, but we want to check that global-function can just be
  ;; known, as well.
  (test cltl2.global-function-toplevel
    (fun global-function)
    (:function nil))
  (test cltl2.global-function
    (eval '(macrolet ((fun (fun &environment e)
                       `(values
                         ,@(multiple-value-bind (kind localp decls)
                               (clasp-cltl2:function-information fun e)
                             (declare (ignore decls))
                             (list kind (not (not localp)))))))
            (fun global-function)))
    (:function nil))
  (test cltl2.local-function
    (flet ((f ())) (fun f))
    (:function t)))

;;; TODO: test OPTIMIZE/declaration-information
;;; TODO: augment-environment more extensively,
;;;       test with -information, etc

(defmacro cltl2.test-macro (x) (declare (ignore x)) :wrong)
(define-symbol-macro cltl2.test-symbol-macro :wrong)
(test cltl2.enclose
  (flet ((expander (form env)
           (declare (ignore env))
           (destructuring-bind (arg) (rest form) arg)))
    (funcall (clasp-cltl2:enclose
              '(lambda (y) (values cltl2.test-symbol-macro
                            (cltl2.test-macro y)))
              (clasp-cltl2:augment-environment
               nil :macro (list (list 'cltl2.test-macro #'expander))
                   :symbol-macro (list
                                  (list 'cltl2.test-symbol-macro :right))))
             :right))
  (:right :right))
