(in-package #:core)

;;; POSIX named shared memory. Public API re-exported through CLASP-POSIX.
;;; Raw syscalls are CORE sys-* primitives from src/core/shmem.cc, each
;;; returning (values result errno) with errno 0 on success.
;;; FTRUNCATE is defined on the CLASP-POSIX symbol directly: the plain name
;;; would otherwise clobber CL:FTRUNCATE in the CORE package.

(defparameter *shm-flags*
  (let ((h (make-hash-table :test #'eq)))
    (loop for (k . v) in (sys-shm-constants) do (setf (gethash k h) v))
    h))

(defun %shm-flag (keyword)
  (or (gethash keyword *shm-flags*)
      (error "Unknown POSIX shared-memory flag ~S" keyword)))

(defun %shm-flags (spec)
  (cond ((null spec) 0)
        ((integerp spec) spec)
        ((keywordp spec) (%shm-flag spec))
        ((listp spec) (reduce #'logior spec :key #'%shm-flag :initial-value 0))
        (t (error "Bad POSIX flag spec ~S" spec))))

(define-condition syscall-error (error)
  ((errno :initarg :errno :reader syscall-errno)
   (name :initarg :name :reader syscall-name))
  (:report (lambda (c s)
             (format s "POSIX ~A failed: ~A (errno ~D)"
                     (syscall-name c) (sys-strerror (syscall-errno c))
                     (syscall-errno c)))))

(deftype posix-error () 'syscall-error)
(defun posix-error-errno (condition) (syscall-errno condition))

(defun %signal-syscall-error (name errno)
  (error 'syscall-error :name name :errno errno))

(defmacro %checked (name form)
  (let ((r (gensym)) (e (gensym)))
    `(multiple-value-bind (,r ,e) ,form
       (if (zerop ,e) ,r (%signal-syscall-error ,name ,e)))))

;; NOT `mapping': CORE::MAPPING is already an exported C++ class (hashTable.h Mapping_O)
(defstruct (shm-mapping (:conc-name mapping-)
                        (:constructor %make-mapping)
                        (:predicate mappingp))
  (address 0 :type integer)
  (size 0 :type integer)
  (fd -1 :type integer)
  (name nil)
  (prot 0 :type integer)
  (unmapped nil :type boolean))

(defun mapping-pointer (mapping)
  "Return a CFFI/clasp-ffi foreign pointer to the mapped region."
  (when (mapping-unmapped mapping)
    (error "Shared-memory mapping ~S is already unmapped" mapping))
  (clasp-ffi:%make-pointer (mapping-address mapping)))

(defun %addr->int (addr)
  (cond ((null addr) 0)
        ((integerp addr) addr)
        (t (clasp-ffi:%foreign-data-address addr))))

(defun shm-open (name oflags &optional (mode #o600))
  "Open/create POSIX shared-memory object NAME; return a file descriptor."
  (%checked "shm_open" (sys-shm-open name (%shm-flags oflags) mode)))

(defun shm-unlink (name)
  "Remove the POSIX shared-memory object named NAME."
  (%checked "shm_unlink" (sys-shm-unlink name))
  t)

(defun clasp-posix::ftruncate (fd length)
  "Set the size of the object behind FD to LENGTH bytes."
  (%checked "ftruncate" (sys-ftruncate fd length))
  t)

(defun getpagesize ()
  "Return the system page size in bytes."
  (sys-getpagesize))

(defun mmap (addr length prot flags fd offset)
  "Map LENGTH bytes of FD; return a MAPPING. ADDR is usually NIL. Unmap it explicitly."
  ;; deliberately no GC finalizer: MAPPING-POINTER hands out a pointer that does not keep
  ;; the MAPPING alive, so a finalizing munmap could unmap a region still in use
  (let* ((prot-bits (%shm-flags prot))
         (address (%checked "mmap"
                    (sys-mmap (%addr->int addr) length
                              prot-bits (%shm-flags flags) fd offset))))
    (%make-mapping :address address :size length :fd fd :prot prot-bits)))

(defun munmap (mapping)
  "Unmap MAPPING; idempotent."
  (unless (mapping-unmapped mapping)
    (%checked "munmap" (sys-munmap (mapping-address mapping) (mapping-size mapping)))
    (setf (mapping-unmapped mapping) t))
  t)

(defun mprotect (mapping prot)
  "Change the memory protection of MAPPING to PROT."
  (%checked "mprotect"
    (sys-mprotect (mapping-address mapping) (mapping-size mapping) (%shm-flags prot)))
  t)

(defun msync (mapping &optional (flags '(:sync)))
  "Flush MAPPING to its backing store."
  (%checked "msync"
    (sys-msync (mapping-address mapping) (mapping-size mapping) (%shm-flags flags)))
  t)

(defun mlock (mapping)
  "Lock MAPPING into physical RAM."
  (%checked "mlock" (sys-mlock (mapping-address mapping) (mapping-size mapping)))
  t)

(defun munlock (mapping)
  "Unlock MAPPING from physical RAM."
  (%checked "munlock" (sys-munlock (mapping-address mapping) (mapping-size mapping)))
  t)

(defun %direction-flags (direction)
  (ecase direction
    (:input  (values '(:rdonly) '(:read)))
    (:output (values '(:rdwr)   '(:read :write)))
    (:io     (values '(:rdwr)   '(:read :write)))))

(defun %shm-open-or-create (name oflags mode size)
  "Open NAME, sizing it only when we are its creator; return a file descriptor."
  ;; only the creator may ftruncate a POSIX shm object -- on an existing one that is EINVAL
  (multiple-value-bind (fd errno)
      (sys-shm-open name (%shm-flags '(:create :exclusive :rdwr)) mode)
    (cond ((zerop errno)
           (clasp-posix::ftruncate fd size)
           fd)
          ((eql errno (%shm-flag :eexist))
           (shm-open name oflags mode))
          (t (%signal-syscall-error "shm_open" errno)))))

(defun open-shared-memory (name size &key (direction :io) (create t) (mode #o600))
  "Open/create shared-memory NAME of SIZE bytes and map it; return a MAPPING."
  (multiple-value-bind (oflags prot) (%direction-flags direction)
    (let* ((fd (if create
                   (%shm-open-or-create name oflags mode size)
                   (shm-open name oflags mode)))
           (m (mmap nil size prot '(:shared) fd 0)))
      (setf (mapping-name m) name)
      m)))

(defun close-shared-memory (mapping &key unlink)
  "Unmap MAPPING, close its fd, and optionally shm-unlink its name."
  (let ((fd (mapping-fd mapping))
        (name (mapping-name mapping)))
    (munmap mapping)
    (when (>= fd 0) (close-fd fd))
    (when (and unlink name) (shm-unlink name))
    t))

(defmacro with-shared-memory ((var name size &key (direction :io) (create t)
                                              (mode #o600) (unlink-on-exit nil))
                              &body body)
  "Bind VAR to a shared-memory MAPPING for the dynamic extent of BODY."
  `(let ((,var (open-shared-memory ,name ,size :direction ,direction
                                   :create ,create :mode ,mode)))
     (unwind-protect (progn ,@body)
       (close-shared-memory ,var :unlink ,unlink-on-exit))))

;;; re-export through CLASP-POSIX; FTRUNCATE is already a CLASP-POSIX symbol
(let ((syms '("SHM-OPEN" "SHM-UNLINK" "GETPAGESIZE"
              "MMAP" "MUNMAP" "MPROTECT" "MSYNC" "MLOCK" "MUNLOCK"
              "SHM-MAPPING" "MAPPINGP" "MAPPING-POINTER" "MAPPING-ADDRESS"
              "MAPPING-SIZE" "MAPPING-FD" "MAPPING-NAME"
              "OPEN-SHARED-MEMORY" "CLOSE-SHARED-MEMORY" "WITH-SHARED-MEMORY"
              "SYSCALL-ERROR" "SYSCALL-ERRNO" "SYSCALL-NAME"
              "POSIX-ERROR" "POSIX-ERROR-ERRNO")))
  (dolist (n syms)
    (let ((s (find-symbol n "CORE")))
      (import s "CLASP-POSIX")
      (export s "CLASP-POSIX"))))

(export (intern "FTRUNCATE" "CLASP-POSIX") "CLASP-POSIX")
