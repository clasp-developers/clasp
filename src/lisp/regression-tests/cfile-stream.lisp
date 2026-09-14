(in-package #:clasp-tests)

;;; Linux-only, using Clasp's built-in FFI. No Quicklisp or network services.
;;; Compile a private fixture without rebuilding the Clasp executable.
(let ((library (core:mkstemp "/tmp/clasp-cfile-stream-")))
  (unwind-protect
       (progn
         (multiple-value-bind (stream code)
             (ext:run-program
              "cc"
              (list "-std=c11" "-shared" "-fPIC" "-pthread" "-o" (namestring library)
                    (namestring
                     (translate-logical-pathname
                      #P"sys:src;lisp;regression-tests;cfile-stream-helper.c")))
              :input nil :output t :error t :wait t)
           (declare (ignore stream))
           (unless (eql code 0)
             (error "Could not compile CFileStream fixture (exit ~s)" code)))
         (clasp-ffi:%load-foreign-library "cfile-stream-test" (namestring library)))
    (when (probe-file library) (delete-file library))))

(defun check-cfile-interrupted-read (prefix)
  (let ((fd (clasp-ffi:%foreign-funcall "clasp_test_interrupted_input" :int prefix :int))
        (stream nil)
        (buffer (make-array 8 :element-type '(unsigned-byte 8) :initial-element 255)))
    (when (minusp fd) (error "Could not create interrupted-read fixture"))
    (unwind-protect
         (progn
           ;; MAKE-STREAM-FROM-FD does not infer a NIL external format from
           ;; a compound byte type, unlike OPEN. Request binary mode explicitly.
           (setf stream (ext:make-stream-from-fd fd :input :buffering t
                                               :element-type '(unsigned-byte 8)
                                               :external-format nil))
           ;; Request eight bytes, supply six, and check the untouched tail.
           (let ((count (read-sequence buffer stream)))
             (list count (coerce buffer 'list) (read-byte stream nil :eof))))
      ;; Join before closing, including on failure, to restore the handler
      ;; and avoid closing a descriptor still used by the fixture.
      (let ((signals (clasp-ffi:%foreign-funcall "clasp_test_finish_interrupted_input" :int)))
        (if stream
            (close stream :abort t)
            (clasp-ffi:%foreign-funcall "close" :int fd :int))
        (unless (plusp signals) (error "Interrupted-read fixture failed: ~d" signals))))))

(test cfile-read-eintr-before-data
      (check-cfile-interrupted-read 0)
      ((6 (97 98 99 100 101 102 255 255) :eof)))

(test cfile-read-eintr-after-partial-data
      (check-cfile-interrupted-read 3)
      ((6 (97 98 99 100 101 102 255 255) :eof)))

(defun check-cfile-buffered-close (abort &optional broken-peer)
  (let ((fd (clasp-ffi:%foreign-funcall "clasp_test_output_pair" :int))
        (stream nil)
        (peer-open t))
    (when (minusp fd) (error "Could not create output socket pair"))
    (unwind-protect
         (progn
           (setf stream (ext:make-stream-from-fd fd :output :buffering t
                                               :element-type '(unsigned-byte 8)
                                               :external-format nil))
           (write-byte 42 stream)
           ;; Establish that the byte is buffered before testing close.
           (unless (= -2 (clasp-ffi:%foreign-funcall "clasp_test_peer_byte" :int))
             (error "Test output was not buffered"))
           (when broken-peer
             (clasp-ffi:%foreign-funcall "clasp_test_close_peer" :int)
             (setf peer-open nil))
           (close stream :abort abort)
           (if broken-peer
               :closed
               (clasp-ffi:%foreign-funcall "clasp_test_peer_byte" :int)))
      ;; Keep the peer open during cleanup if CLOSE signalled an error.
      (unwind-protect
           (if stream
               (when (open-stream-p stream) (ignore-errors (close stream :abort t)))
               (clasp-ffi:%foreign-funcall "close" :int fd :int))
        (when peer-open (clasp-ffi:%foreign-funcall "clasp_test_close_peer" :int))))))

(test cfile-normal-close-flushes-buffer (check-cfile-buffered-close nil) (42))
(test cfile-abort-close-discards-buffer (check-cfile-buffered-close t) (-1))
(test cfile-abort-close-broken-peer (check-cfile-buffered-close t t) (:closed))
