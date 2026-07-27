(in-package #:cmp)

;;; skip optional processing
;;; gethash is common enough that this is probably worthwhile, but ideally
;;; it could perhaps be handled automatically by a compiler?
(define-compiler-macro gethash (key hash-table &optional (default nil))
  `(core:gethash3 ,key ,hash-table ,default))

(define-compiler-macro (setf gethash) (new key hash-table
                                       &optional (default nil defaultp))
  (if defaultp
      ;; have to ensure the default is evaluated, and in the right order.
      `(core:puthash ,new ,key (prog1 ,hash-table ,default))
      `(core:puthash ,new ,key ,hash-table)))
