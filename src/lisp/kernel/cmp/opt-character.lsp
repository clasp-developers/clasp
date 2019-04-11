(in-package #:cmp)

;; In clasp, chars are eq iff they are char=.
;; We'll need more work for char< and char-equal and so on.

;; Note: On low optimize safety, in cclasp, there will be no type checks here.

(define-compiler-macro char= (&whole form &rest args)
  (core:expand-compare form 'eq (mapcar (lambda (arg) `(the character ,arg)) args)))

(define-compiler-macro char/= (&whole form &rest args)
  (core:expand-uncompare form 'eq (mapcar (lambda (arg) `(the character ,arg)) args)))
