(in-package #:cmp)

(define-compiler-macro not (&whole form objectf)
  ;; Take care of (not (not x)), which code generates sometimes.
  (if (and (consp objectf)
           (eq (car objectf) 'not)
           (consp (cdr objectf))
           (null (cddr objectf)))
      (second objectf)
      form))

;;; every, etc. defined in opt-sequence
