(in-package #:cmp)

;; In clasp, chars are eq iff they are char=.
;; We'll need more work for char< and char-equal and so on.

;; Note: On low optimize safety, in cclasp, there will be no type checks here.

(define-compiler-macro char= (&whole form &rest args)
  (core:expand-compare form 'eq args 'character))

(define-compiler-macro char/= (&whole form &rest args)
  (core:expand-uncompare form 'eq args 'character))

(define-compiler-macro char-equal (&whole form &rest chars)
  (core:expand-compare form 'core:two-arg-char-equal chars 'character))

(define-compiler-macro char-not-equal (&whole form &rest chars)
  (core:expand-uncompare form 'core:two-arg-char-equal chars 'character))
