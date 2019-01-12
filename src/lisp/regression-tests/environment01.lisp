(in-package #:clasp-tests)

(TEST-EXPECT-ERROR
 sleep-1
 (sleep nil)
 :type type-error)

(TEST-EXPECT-ERROR
 sleep-2
 (sleep #\space)
 :type type-error)

(TEST-EXPECT-ERROR
 sleep-3
 (sleep -1)
  :type type-error)
