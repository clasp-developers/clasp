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
                   
(test-expect-error gensym-2
                   (gensym -1)
                   :type type-error)

(test-expect-error gensym-3
                   (gensym (1- most-negative-fixnum))
                   :type type-error)

(test-expect-error gensym-4
                   (gensym 'defun)
                   :type type-error)

(test gensym-5 (gensym (+ most-positive-fixnum  most-positive-fixnum)))

(test gensym-6
      (let ((bignum 12345678901234567890123456789012345678901234567890))
        (= (1+ bignum)
           (LET ((*GENSYM-COUNTER* bignum))
             (GENSYM)
             *GENSYM-COUNTER*))))

(test gensym-7
      (= (1+ most-positive-fixnum)
         (let ((*gensym-counter* most-positive-fixnum))
           (gensym)
           *gensym-counter*)))

(test-expect-error gensym-8
                   (let ((*gensym-counter* -1))
                     (gensym)
                     *gensym-counter*))

(test-expect-error gensym-9
                   (let ((*gensym-counter* (1- most-negative-fixnum)))
                     (gensym)
                     *gensym-counter*))
      

(test-expect-error gentemp-1
                   (gentemp nil)
                   :type type-error)

