(in-package #:clasp-tests)

(defun %shm-name ()
  (format nil "/clasp-test-shm-~D" (clasp-posix:getpid)))

(test shmem-roundtrip
      (let ((name (%shm-name))
            (size 4096))
        (unwind-protect
             (let ((m (clasp-posix:open-shared-memory name size :create t)))
               (clasp-ffi:%mem-set (clasp-posix:mapping-pointer m) :uint32 #x12345678 0)
               (clasp-posix:msync m)
               (clasp-posix:munmap m)
               (let ((m2 (clasp-posix:open-shared-memory name size
                                                         :direction :input :create nil)))
                 (prog1
                     (clasp-ffi:%mem-ref (clasp-posix:mapping-pointer m2) :uint32 0)
                   (clasp-posix:close-shared-memory m2))))
          (ignore-errors (clasp-posix:shm-unlink name))))
      (#x12345678))

(test shmem-shared-backing
      (let ((name (%shm-name))
            (size 4096))
        (unwind-protect
             (let ((fd (clasp-posix:shm-open name '(:rdwr :create) #o600)))
               (clasp-posix:ftruncate fd size)
               (let ((a (clasp-posix:mmap nil size '(:read :write) '(:shared) fd 0))
                     (b (clasp-posix:mmap nil size '(:read :write) '(:shared) fd 0)))
                 (clasp-ffi:%mem-set (clasp-posix:mapping-pointer a) :uint32 12345 0)
                 (prog1
                     (clasp-ffi:%mem-ref (clasp-posix:mapping-pointer b) :uint32 0)
                   (clasp-posix:munmap a)
                   (clasp-posix:munmap b)
                   (clasp-posix:close-fd fd))))
          (ignore-errors (clasp-posix:shm-unlink name))))
      (12345))

(test-expect-error shmem-open-missing
                   (clasp-posix:shm-open "/clasp-no-such-xyz" '(:rdwr))
                   :type clasp-posix:syscall-error)

(test shmem-munmap-idempotent
      (let ((name (%shm-name))
            (size 4096))
        (unwind-protect
             (let ((m (clasp-posix:open-shared-memory name size :create t)))
               (clasp-posix:munmap m)
               (list (clasp-posix:munmap m)
                     (handler-case
                         (progn (clasp-posix:mapping-pointer m) :no-error)
                       (error () :errored))))
          (ignore-errors (clasp-posix:shm-unlink name))))
      ((t :errored)))

(test-true shmem-pagesize
           (let ((p (clasp-posix:getpagesize)))
             (and (integerp p) (plusp p) (zerop (logand p (1- p))))))

;; a POSIX shm object may be sized only by its creator; ftruncate on an existing one is EINVAL
(test shmem-reopen-with-default-create
      (let ((name (%shm-name))
            (size 4096))
        (unwind-protect
             (let ((a (clasp-posix:open-shared-memory name size)))
               (clasp-ffi:%mem-set (clasp-posix:mapping-pointer a) :uint32 4242 0)
               (let ((b (clasp-posix:open-shared-memory name size)))
                 (prog1 (clasp-ffi:%mem-ref (clasp-posix:mapping-pointer b) :uint32 0)
                   (clasp-posix:munmap b)
                   (clasp-posix:close-shared-memory a))))
          (ignore-errors (clasp-posix:shm-unlink name))))
      (4242))

(test shmem-input-direction-with-default-create
      (let ((name (%shm-name))
            (size 4096))
        (unwind-protect
             (let ((a (clasp-posix:open-shared-memory name size)))
               (clasp-ffi:%mem-set (clasp-posix:mapping-pointer a) :uint32 99 0)
               (let ((b (clasp-posix:open-shared-memory name size :direction :input)))
                 (prog1 (clasp-ffi:%mem-ref (clasp-posix:mapping-pointer b) :uint32 0)
                   (clasp-posix:munmap b)
                   (clasp-posix:close-shared-memory a))))
          (ignore-errors (clasp-posix:shm-unlink name))))
      (99))

;; MAPPING-POINTER hands out a pointer that does not keep the MAPPING alive
(test-true shmem-no-gc-finalizer
           (let ((name (%shm-name)))
             (unwind-protect
                  (let ((m (clasp-posix:open-shared-memory name 4096)))
                    (prog1 (null (gctools:finalizers m))
                      (clasp-posix:close-shared-memory m)))
               (ignore-errors (clasp-posix:shm-unlink name)))))

(test shmem-pointer-survives-gc
      (let ((name (%shm-name)))
        (unwind-protect
             (let ((ptr (let ((m (clasp-posix:open-shared-memory name 4096)))
                          (clasp-ffi:%mem-set (clasp-posix:mapping-pointer m) :uint32 31337 0)
                          (clasp-posix:mapping-pointer m))))
               (gctools:garbage-collect)
               (gctools:garbage-collect)
               (clasp-ffi:%mem-ref ptr :uint32 0))
          (ignore-errors (clasp-posix:shm-unlink name))))
      (31337))

(test shmem-fork-ipc
      (let ((name (%shm-name))
            (size 4096)
            (sentinel 7654321))
        (unwind-protect
             (let* ((m (clasp-posix:open-shared-memory name size :create t))
                    (ptr (clasp-posix:mapping-pointer m)))
               (clasp-ffi:%mem-set ptr :uint32 0 0)
               (finish-output)
               (finish-output *error-output*)
               (multiple-value-bind (stream pid) (clasp-posix:fork nil)
                 (declare (ignore stream))
                 (cond
                   ((zerop pid)
                    (clasp-ffi:%mem-set ptr :uint32 sentinel 0)
                    (core:cexit 0))
                   (t
                    (clasp-posix:wait)
                    (prog1
                        (clasp-ffi:%mem-ref ptr :uint32 0)
                      (clasp-posix:close-shared-memory m))))))
          (ignore-errors (clasp-posix:shm-unlink name))))
      (7654321))
