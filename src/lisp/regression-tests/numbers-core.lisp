(in-package #:clasp-tests)

(test single-float-bit-positive-cases
      (let ((args
             (list most-negative-single-float -3.0 0.0 3.0 most-positive-single-float)))
        (dolist (arg args t)
          (let ((bits (ext:single-float-to-bits arg)))
            (unless (= arg (ext:bits-to-single-float bits))
              (return nil))))))

(test-expect-error single-float-bit-negative-1 (ext:single-float-to-bits 0))
(test-expect-error single-float-bit-negative-2 (ext:single-float-to-bits 3))
(test-expect-error single-float-bit-negative-3 (ext:single-float-to-bits 3.d0))
(test-expect-error single-float-bit-negative-4 (ext:single-float-to-bits (1+ most-positive-fixnum)))
(test-expect-error single-float-bit-negative-5 (ext:single-float-to-bits (1- most-negative-fixnum)))

(test double-float-bit-positive-cases
      (let ((args
             (list most-negative-double-float -3.0d0 0.0d0 3.0d0 most-positive-double-float)))
        (dolist (arg args t)
          (let ((bits (ext:double-float-to-bits arg)))
            (unless
                (= arg (ext:bits-to-double-float bits))
              (return nil))))))

(test-expect-error double-float-bit-negative-1 (ext:double-float-to-bits 0))
(test-expect-error double-float-bit-negative-2 (ext:double-float-to-bits 3))
(test-expect-error double-float-bit-negative-3 (ext:double-float-to-bits 3.0))
(test-expect-error double-float-bit-negative-4 (ext:double-float-to-bits (1+ most-positive-fixnum)))
(test-expect-error double-float-bit-negative-5 (ext:double-float-to-bits (1- most-negative-fixnum)))



