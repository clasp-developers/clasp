(in-package #:cmp)

;; In clasp, chars are eq iff they are char=.
;; We'll need more work for char< and char-equal and so on.

;; Note: On low optimize safety, in cclasp, there will be no type checks here.

(define-compiler-macro char/= (&whole form &rest args)
  (core:expand-uncompare form 'eq args 'character))

(define-compiler-macro char-not-equal (&whole form &rest chars)
  (core:expand-uncompare form 'core:two-arg-char-equal chars 'character))

(macrolet ((def (fname two-arg-name)
             `(define-compiler-macro ,fname (&whole form &rest chars)
                (core:expand-compare form ',two-arg-name chars 'character))))
  (def char= eq)
  (def char-equal core:two-arg-char-equal)
  (def char< core:two-arg-char<)
  (def char<= core:two-arg-char<=)
  (def char> core:two-arg-char>)
  (def char>= core:two-arg-char>=)
  (def char-lessp core:two-arg-char-lessp)
  (def char-not-greaterp core:two-arg-char-not-greaterp)
  (def char-greaterp core:two-arg-char-greaterp)
  (def char-not-lessp core:two-arg-char-not-lessp))
