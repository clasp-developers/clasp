(in-package #:clasp-tests)

;;; Content of backtraces is weird to test, so we're pretty basic here

;;; Test that backtraces exist at all
(test-type backtrace-1
    (with-output-to-string (s)
      (clasp-debug:print-backtrace :stream s))
    string)

;;; ...that functions do actually show up in the backtrace
;;; Note that if this test fails, most of the rest will have meaningless results
(declaim (notinline function-to-show-up-in-backtrace))
(defun function-to-show-up-in-backtrace (f x)
  ;; Lambda list and docstring are repeated below - if editing this, edit that too
  "Dummy function for use in tests."
  (funcall f)
  x)

(test-true backtrace-2
      (block nil
        (function-to-show-up-in-backtrace
         (lambda ()
          (clasp-debug:with-stack (stack)
            (clasp-debug:map-stack
             (lambda (frame)
               (when (eq (clasp-debug:frame-function-name frame)
                         'function-to-show-up-in-backtrace)
                 (return t)))
             stack)))
         nil)))

;;; ...that without :count, all frames are taken
;;; or at least some.
(defun nest-ftsuib (f n)
  ;; Call function-to-show-up-in-backtrace n times nested. Return NIL.
  (if (zerop n)
      (funcall f)
      (nest-ftsuib
       (lambda ()
         (function-to-show-up-in-backtrace f nil))
       (1- n))))

(test backtrace-3
      (block nil
        (nest-ftsuib
         (lambda ()
           (let ((count 0))
             (clasp-debug:with-stack (stack)
               (clasp-debug:map-stack
                (lambda (frame)
                  (when (eq (clasp-debug:frame-function-name frame)
                            'function-to-show-up-in-backtrace)
                    (incf count)))
                stack))
             (return count)))
         23))
      (23))

;;; ...that with count, only so many frames are taken
(test backtrace-4
      (block nil
        (nest-ftsuib
         (lambda ()
           (let ((count 0))
             (clasp-debug:with-stack (stack)
               (clasp-debug:map-stack
                (lambda (frame)
                  (declare (ignore frame))
                  (incf count))
                stack
                :count 7))
             (return count)))
         23))
      (7))

;;; ...that arguments are retrievable
;;; NOTE about the FRAME-foo tests: they will probably have to be reduced or eliminated
;;; in the future, because that info is only sometimes available. For this one specifically,
;;; maybe we'll need a high DEBUG to capture locals.
(test frame-locals
  (block nil
    (defun get-frame-locals ()
      (clasp-debug:map-backtrace
       (lambda (frame)
         (when (eq (clasp-debug:frame-function-name frame)
                   'function-to-show-up-in-backtrace)
           (return (clasp-debug:frame-locals frame))))))
    (function-to-show-up-in-backtrace 'get-frame-locals 357))
  (((f . get-frame-locals) (x . 357))))

;;; ...that functions are retrievable (see NOTE above)
(test-true frame-function
      (eq #'function-to-show-up-in-backtrace
          (block nil
            (function-to-show-up-in-backtrace
             (lambda ()
              (clasp-debug:with-stack (stack)
                (clasp-debug:map-stack
                 (lambda (frame)
                   (when (eq (clasp-debug:frame-function-name frame)
                             'function-to-show-up-in-backtrace)
                     (return (clasp-debug:frame-function frame))))
                 stack)))
             nil))))

;;; ...that lambda lists are retrievable (see NOTE above)
(test frame-function-lambda-list
      (block nil
        (function-to-show-up-in-backtrace
         (lambda ()
          (clasp-debug:with-stack (stack)
            (clasp-debug:map-stack
             (lambda (frame)
               (when (eq (clasp-debug:frame-function-name frame)
                         'function-to-show-up-in-backtrace)
                 (return (clasp-debug:frame-function-lambda-list frame))))
             stack)))
         nil))
      ((f x) t))

;;; ...that docstrings are retrievable (see NOTE above)
(test frame-function-documentation
      (block nil
        (function-to-show-up-in-backtrace
         (lambda ()
          (clasp-debug:with-stack (stack)
            (clasp-debug:map-stack
             (lambda (frame)
               (when (eq (clasp-debug:frame-function-name frame)
                         'function-to-show-up-in-backtrace)
                 (return (clasp-debug:frame-function-documentation frame))))
             stack)))
         nil))
  ("Dummy function for use in tests."))

;;; And now for bytecode frames.
(test-true bytecode-frame-function-name
  (progn
    (funcall (cmp:bytecompile
              '(lambda ()
                (defun bc2 ()
                  (let ((frames nil))
                    (clasp-debug:map-backtrace
                     (lambda (frame)
                       (when (eq (clasp-debug:frame-language frame)
                                 :bytecode)
                         (push (clasp-debug:frame-function-name frame)
                               frames))))
                    frames))
                (defun bc1 () (bc2)))))
    ;; We use (fdefinition 'bc1) to avoid a compiler warning hopefully.
    (search '(bc1 bc2) (funcall (fdefinition 'bc1)))))

(test bytecode-frame-locals
  (progn
    (funcall (cmp:bytecompile
              '(lambda ()
                (defun bcl (x)
                  (clasp-debug:map-backtrace
                   (lambda (frame)
                     (when (eq (clasp-debug:frame-function-name frame) 'bcl)
                       (return-from bcl (clasp-debug:frame-locals frame)))))))))
    (bcl 137))
  (((x . 137))))

;;; ...that print errors don't escape print-backtrace
;;; Note that this result is meaningless if frame-arguments fails.
(defclass unprintable-object () ())

(defmethod print-object ((o unprintable-object) s)
  (declare (ignore s))
  (error "PRINT-OBJECT signaled an error and it wasn't handled"))

(test-true print-backtrace-handles-errors
      (progn
       (with-output-to-string (s)
         (function-to-show-up-in-backtrace
          (lambda ()
           (clasp-debug:print-backtrace :stream s))
          (make-instance 'unprintable-object)))
       t))

;;; ...that with-truncated-stack works
(test-true truncate-stack
      (block nil
        (clasp-debug:with-truncated-stack ()
          (function-to-show-up-in-backtrace
           (lambda ()
            (clasp-debug:with-stack (stack)
              (clasp-debug:map-stack
               (lambda (frame)
                 (when (eq (clasp-debug:frame-function-name frame)
                           'function-to-show-up-in-backtrace)
                   (return nil)))
               stack)))
           t))))

;;; ...that with-capped-stack works
(test-true cap-stack
      (block nil
        (function-to-show-up-in-backtrace
         (lambda ()
          (clasp-debug:with-capped-stack ()
            (clasp-debug:with-stack (stack)
              (clasp-debug:map-stack
               (lambda (frame)
                 (when (eq (clasp-debug:frame-function-name frame)
                           'function-to-show-up-in-backtrace)
                   (return nil)))
               stack))))
         t)))

;;; ...that :delimited nil ignores with-truncated-stack
(test-true undelimited-stack-1
      (block nil
        (clasp-debug:with-truncated-stack ()
          (function-to-show-up-in-backtrace
           (lambda ()
            (clasp-debug:with-stack (stack :delimited nil)
              (clasp-debug:map-stack
               (lambda (frame)
                 (when (eq (clasp-debug:frame-function-name frame)
                           'function-to-show-up-in-backtrace)
                   (return t)))
               stack)))
           nil))))

;;; ...that :delimited nil ignores with-capped-stack
(test-true undelimited-stack-2
      (block nil
        (function-to-show-up-in-backtrace
         (lambda ()
          (clasp-debug:with-capped-stack ()
            (clasp-debug:with-stack (stack :delimited nil)
              (clasp-debug:map-stack
               (lambda (frame)
                 (when (eq (clasp-debug:frame-function-name frame)
                           'function-to-show-up-in-backtrace)
                   (return t)))
               stack))))
         nil)))

;;; TODO: Visibility tests?

;;; STEP tests

;;; we can compile code with breakstep in special positions - this has
;;; sometimes been a problem, since the compiler inlines them specially.
(test breakstep-compile
      (values-list
       (cdr
       (multiple-value-list
        (compile nil '(lambda (f)
                       (clasp-debug:set-breakstep)
                       (unwind-protect
                            (funcall f)
                         (clasp-debug:unset-breakstep)))))))
      (nil nil))

;;; stepping means a break condition is actually signaled.
(test-true step
           (block nil
             (let ((ext:*invoke-debugger-hook*
                     (lambda (condition old-hook)
                       (declare (ignore old-hook))
                       (return (typep condition 'clasp-debug:step-condition)))))
               (step (print 4)))))

(test breakstepping-p
  (let ((ext:*invoke-debugger-hook* ; don't step during the test!
          (lambda (condition old-hook)
            (declare (ignore condition old-hook))
            (invoke-restart 'clasp-debug:step-over))))
    (values (progn (clasp-debug:set-breakstep)
                   (clasp-debug:breakstepping-p))
            (progn (clasp-debug:unset-breakstep)
                   (clasp-debug:breakstepping-p))))
  (t nil))

;;; breakstep can also be used to enable the stepper, without STEP itself.
(test-true breakstep
           (block nil
             (let ((ext:*invoke-debugger-hook*
                     (lambda (condition old-hook)
                       (declare (ignore old-hook))
                       (return (typep condition 'clasp-debug:step-condition)))))
               (clasp-debug:set-breakstep)
               (unwind-protect
                    (locally
                        ;; FIXME: Export declaration? DEBUG 3 instead?
                        ;; We are of course assuming cclasp is being used
                        ;; to compile the tests.
                        (declare (optimize core::insert-step-conditions))
                      (print 4))
                 (clasp-debug:unset-breakstep)))))
