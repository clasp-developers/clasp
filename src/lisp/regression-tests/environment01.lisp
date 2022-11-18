(in-package #:clasp-tests)

(TEST-EXPECT-ERROR sleep-1 (sleep nil) :type type-error)

(TEST-EXPECT-ERROR sleep-2 (sleep #\space) :type type-error)

(TEST-EXPECT-ERROR sleep-3 (sleep -1) :type type-error)

(defun %%test%% (n) (* n 2))

(define-compiler-macro %%test%% (&whole form arg)
  (if (constantp arg)
      (* arg 2)
      form))

(test DOCUMENTATION.LIST.COMPILER-MACRO.2
      (progn (setf (documentation '%%test%% 'compiler-macro) "Buh")
             (documentation '%%test%% 'compiler-macro))
      ("Buh"))

(test funcall-compiler-macro
      (funcall (compiler-macro-function '%%test%%)
               '(funcall #'%%test%% 2) nil)
      (4))


