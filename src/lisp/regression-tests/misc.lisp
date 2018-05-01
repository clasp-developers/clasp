(in-package #:clasp-tests)

(test-expect-error
 misc-1
 (disassemble 0)
 :type type-error)

(test misc-2
      (> (length (with-output-to-string (*standard-output*)
		   (room))) 0))
