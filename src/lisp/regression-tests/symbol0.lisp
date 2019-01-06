(in-package #:clasp-tests)

(TEST-EXPECT-ERROR TEST-MAKE-SYMBOL-ERROR-0 (MAKE-SYMBOL 23) :TYPE TYPE-ERROR)
(TEST-EXPECT-ERROR TEST-MAKE-SYMBOL-ERROR-1 (MAKE-SYMBOL 'DEFUN) :TYPE
 TYPE-ERROR)
(TEST-EXPECT-ERROR TEST-MAKE-SYMBOL-ERROR-2 (MAKE-SYMBOL #\A) :TYPE TYPE-ERROR)
(TEST TEST-MAKE-SYMBOL-0
 (SYMBOLP (HANDLER-CASE (MAKE-SYMBOL "ABCCC") (ERROR (E) E))))
(TEST TEST-MAKE-SYMBOL-1
 (SYMBOLP
  (HANDLER-CASE
   (MAKE-SYMBOL
    (MAKE-ARRAY '(6)
                :INITIAL-CONTENTS
                '(#\A #\B #\C #\D #\E #\F)
                :ELEMENT-TYPE
                'CHARACTER
                :FILL-POINTER
                4))
    (ERROR (E) E))))

(test-expect-error makunbound-1
                   (let ((foo 23))
                     (makunbound 23))
                   :type type-error)
