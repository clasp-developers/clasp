;;; Optimization of cons functions
;;; No further relation to ECL file of similar name

(in-package #:cmp)

(defconstant +nthcdr-inline-limit+ 8) ; totally arbitrary

(define-compiler-macro nthcdr (&whole whole index list &environment env)
  (if (constantp index env)
      (let ((i (ext:constant-form-value index env)))
        (if (and (integerp i)
                 (<= 0 i)
                 (< i +nthcdr-inline-limit+))
            (loop for result = list then `(cdr ,result)
                  repeat i
                  finally (return result))
            ;; could do optimization notes
            whole))
      whole))

;;; This is of course basically an inline definition, but we do it at this level
;;; so that the nthcdr can have its compiler macro used (which it wouldn't be, with ASTs)
(define-compiler-macro nth (index list)
  `(car (nthcdr ,index ,list)))
