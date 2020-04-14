(in-package #:clasp-tests)

;;; Content of backtraces is weird to test, so we're pretty basic here

;;; Test that backtraces exist at all
(test backtrace-1
      (stringp
       (with-output-to-string (s)
         (clasp-debug:print-backtrace :stream s))))

;;; ...that functions do actually show up in the backtrace
;;; Note that if this test fails, most of the rest will have meaningless results
(declaim (notinline function-to-show-up-in-backtrace))
(defun function-to-show-up-in-backtrace (f x)
  ;; Lambda list and docstring are repeated below - if editing this, edit that too
  "Dummy function for use in tests."
  (funcall f)
  x)

(test backtrace-2
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
      (= 23
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
            23))))

;;; ...that with count, only so many frames are taken
(test backtrace-4
      (= 7
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
            23))))

;;; ...that arguments are retrievable
;;; NOTE about the FRAME-foo tests: they will probably have to be reduced or eliminated
;;; in the future, because that info is only sometimes available. For this one specifically,
;;; maybe we'll need a high DEBUG to capture arguments, or more importantly, the locals
;;; will be better to use than the arguments.
(test frame-arguments
      (block nil
        (labels ((f ()
                   (clasp-debug:with-stack (stack)
                     (clasp-debug:map-stack
                      (lambda (frame)
                        (when (eq (clasp-debug:frame-function-name frame)
                                'function-to-show-up-in-backtrace)
                          (return
                            (equal (clasp-debug:frame-arguments frame)
                                   (list #'f nil)))))
                      stack))))
          (function-to-show-up-in-backtrace #'f nil))))

;;; ...that functions are retrievable (see NOTE above)
(test frame-function
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

;;; ...that lisp frames are marked as such
(test frame-language
      (eq :lisp
          (block nil
            (function-to-show-up-in-backtrace
             (lambda ()
              (clasp-debug:with-stack (stack)
                (clasp-debug:map-stack
                 (lambda (frame)
                   (when (eq (clasp-debug:frame-function-name frame)
                             'function-to-show-up-in-backtrace)
                     (return (clasp-debug:frame-language frame))))
                 stack)))
             nil))))

;;; ...that lambda lists are retrievable (see NOTE above)
(test frame-function-lambda-list
      (multiple-value-bind (lambda-list retrievedp)
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
        (and retrievedp (equal lambda-list '(f x)))))

;;; ...that docstrings are retrievable (see NOTE above)
(test frame-function-documentation
      (string=
       "Dummy function for use in tests."
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
          nil))))

;;; ...that print errors don't escape print-backtrace
;;; Note that this result is meaningless if frame-arguments fails.
(defclass unprintable-object () ())

(defmethod print-object ((o unprintable-object) s)
  (declare (ignore s))
  (error "PRINT-OBJECT signaled an error and it wasn't handled"))

(test print-backtrace-handles-errors
      (progn
       (with-output-to-string (s)
         (function-to-show-up-in-backtrace
          (lambda ()
           (clasp-debug:print-backtrace :stream s))
          (make-instance 'unprintable-object)))
       t))

;;; ...that with-truncated-stack works
(test truncate-stack
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
(test cap-stack
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
(test undelimited-stack-1
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
(test undelimited-stack-2
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
