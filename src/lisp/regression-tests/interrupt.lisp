(in-package #:clasp-tests)

;;; These tests are all a little arcane in order to manage something that will
;;; succeed or fail without hanging. (That perhaps needs some work.)

;;; Create a process NAME that waits until BODY has executed before executing,
;;; and wait for it to end.
(defmacro with-delayed-process ((name &body tbody) &body body)
  (let ((lock (gensym "LOCK")))
    `(let ((,lock (mp:make-lock))
           ,name)
       (mp:with-lock (,lock)
         (setf ,name
               (mp:process-run-function
                ',name
                (lambda ()
                  (mp:with-lock (,lock)
                    (core:check-pending-interrupts)
                    ,@tbody))))
         ,@body)
       ;; ignore-errors in case the thread was cancelled.
       (ignore-errors (mp:process-join ,name)))))

(test cancellation-interrupt
      ;; Check that the thread is canceled before it can exit normally.
      (let ((cell t))
        (with-delayed-process (cancellation-interrupt-test
                               (setf cell nil))
          (mp:process-cancel cancellation-interrupt-test))
        cell)
      (t))

(test handle-interrupt
      (let* ((cell nil)
             (proc (mp:process-run-function
                    'handle-interrupt-test
                    (lambda ()
                      (handler-case
                          (loop (core:check-pending-interrupts))
                        (mp:cancellation-interrupt ()
                          (setf cell t)))))))
        (mp:process-cancel proc)
        (ignore-errors (mp:process-join proc))
        cell)
      (t))

(test call-interrupt
      (let ((cell (list nil)))
        (with-delayed-process (call-interrupt-test)
          (mp:interrupt-process call-interrupt-test
                                (lambda () (setf (mp:atomic (car cell)) t))))
        cell)
      ((t)))

;;; Necessarily a little imprecise
(test sleep-interruptible
      (let ((cell (list nil))
            (thread (mp:process-run-function 'sleep-interruptible-test
                                             (lambda () (sleep 3)))))
        (mp:interrupt-process thread
                              (lambda () (setf (mp:atomic (car cell)) t)))
        (mp:process-join thread)
        cell)
      ((t)))

(test lock-interruptible
      (let ((lock (mp:make-lock))
            (cell (list t))
            proc)
        (mp:with-lock (lock)
          (setf proc (mp:process-run-function
                      'lock-interruptible-test
                      (lambda ()
                        (mp:with-lock (lock)
                          (setf (mp:atomic (car cell)) nil)))))
          (mp:process-cancel proc))
        (ignore-errors (mp:process-join proc))
        cell)
      ((t)))

(test input-interruptible
      (let* ((cell t)
             (proc (mp:process-run-function
                    'input-interruptible-test
                    (lambda () (read) (setf cell nil)))))
        (mp:process-cancel proc)
        (ignore-errors (mp:process-join proc))
        cell)
      (t))

;;; Check unwind-protect semantics: Interrupts are disabled while unwinding,
;;; but restored afterwards.
(defun call-with-uwp (function cleanup)
  ;; separate to try to avoid compiler optimizations.
  (unwind-protect (funcall function) (funcall cleanup)))

(test unwind-protect.interrupt.1
      (mp:with-interrupts
        (let ((outer-interruptiblep (mp:interruptiblep))
              unwinding-interruptiblep)
          (call-with-uwp (lambda ()) (lambda () (setf unwinding-interruptiblep
                                                      (mp:interruptiblep))))
          (values unwinding-interruptiblep (eq outer-interruptiblep
                                               (mp:interruptiblep)))))
      (nil t))

(test unwind-protect.interrupt.2
      (mp:with-interrupts
        (let ((outer-interruptiblep (mp:interruptiblep)))
          (values (block nil
                    (call-with-uwp (lambda ())
                                   (lambda () (return (mp:interruptiblep)))))
                  (eq outer-interruptiblep (mp:interruptiblep)))))
      (nil t))
