(in-package #:clasp-tests)

(test single-float-bit-positive-cases
      (let ((args
             (list most-negative-single-float -3.0 0.0 3.0 most-positive-single-float)))
        (dolist (arg args t)
          (let ((bits (core::single-float-bits arg)))
            (unless
                (= arg (core::single-float-from-unsigned-byte-32 bits))
              (return nil))))))

(test-expect-error single-float-bit-negative-1 (core::single-float-bits 0))
(test-expect-error single-float-bit-negative-2 (core::single-float-bits 3))
(test-expect-error single-float-bit-negative-3 (core::single-float-bits 3.d0))
(test-expect-error single-float-bit-negative-4 (core::single-float-bits (1+ most-positive-fixnum)))
(test-expect-error single-float-bit-negative-5 (core::single-float-bits (1- most-negative-fixnum)))

(test double-float-bit-positive-cases
      (let ((args
             (list most-negative-double-float -3.0d0 0.0d0 3.0d0 most-positive-double-float)))
        (dolist (arg args t)
          (let ((bits (core::double-float-bits arg)))
            (unless
                (= arg (core::double-float-from-bits bits))
              (return nil))))))

(test-expect-error double-float-bit-negative-1 (core::double-float-bits 0))
(test-expect-error double-float-bit-negative-2 (core::double-float-bits 3))
(test-expect-error double-float-bit-negative-3 (core::double-float-bits 3.0))
(test-expect-error double-float-bit-negative-4 (core::double-float-bits (1+ most-positive-fixnum)))
(test-expect-error double-float-bit-negative-5 (core::double-float-bits (1- most-negative-fixnum)))



