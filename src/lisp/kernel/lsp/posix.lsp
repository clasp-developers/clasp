(in-package :ext)

(defun enable-interrupt (signal mode &optional lisp-handler)
  "Enable/disable signal, 
   If mode = :ignore, ignore the signal
   If mode = :default, set the default handler
   If mode = :lisp, define lisp-handler"
  (flet ((handle-lisp-handler (signal-as-int)
             (core::note-signal-handler signal-as-int lisp-handler)
             2)
         (handle-non-lisp-handler (signal-as-int)
             (core::forget-signal-handler signal-as-int)))
      (let ((int-signal (core::external-to-int-signal signal)))
        (cond (int-signal
               (core:enable-disable-signals
                int-signal
                (ecase mode
                  (:ignore  (handle-non-lisp-handler int-signal) 0)
                  (:default (handle-non-lisp-handler int-signal) 1)
                  (:lisp (handle-lisp-handler int-signal))))
               :done)
              (t :not-found)))))

(defun default-interrupt (signal)
  "Set the default handler for signal"
  (enable-interrupt signal :default))

(defun ignore-interrupt (signal)
  "Ignore the interrupt for signal"
  (enable-interrupt signal :ignore))

(defun get-signal-handler (signal)
  "Get a lisp handler handler for signal, or nil if none defined"
  (let ((internal-signal (core::external-to-int-signal signal)))
    (if internal-signal
        (core::get-signal-handler internal-signal)
        nil)))

(defun set-signal-handler (signal handler)
  "Set a lisp handler handler for signal"
  (enable-interrupt signal :lisp handler))

#|
(defun karsten-handler(signal)(format t "~&Processed Signal ~a~%" signal))
(ext:enable-interrupt :sigpipe :lisp #'karsten-handler)
kill -s PIPE <clasp-pid>
COMMON-LISP-USER> Processed Signal 13
(ext:get-signal-handler :sigpipe)
(ext:ignore-interrupt :sigpipe)
(ext:default-interrupt :sigpipe)
|#

(in-package :core)

(defparameter *signal-to-function* nil)

(defun external-to-int-signal (signal)
  (let* ((signal-alist
          '((:SIGHUP  1       :term "hangup ")
            (:SIGINT  2       :term "interrupt ")
            (:SIGQUIT 3       :core "quit ")
            (:SIGILL  4       :core "illegal instruction (not reset when caught) ")
            (:SIGTRAP 5       :unknown "trace trap (not reset when caught) ")
            (:SIGABRT 6       :core "abort() ")
            (:SIGPOLL 7       :unknown"pollable event ([XSR] generated, not supported) ")
            (:SIGIOT  6       :core "compatibility ")
            (:SIGEMT  7       :unknown "EMT instruction ")
            (:SIGFPE  8       :core "floating point exception ")
            (:SIGKILL 9       :term "kill (cannot be caught or ignored) ")
            (:SIGBUS  10      :term "bus error ")
            (:SIGSEGV 11      :core "segmentation violation ")
            (:SIGSYS  12      :unknown "bad argument to system call ")
            (:SIGPIPE 13      :term "write on a pipe with no one to read it ")
            (:SIGALRM 14      :term "alarm clock ")
            (:SIGTERM 15      :term "software termination signal from kill ")
            (:SIGURG  16      :term "urgent condition on IO channel ")
            (:SIGSTOP 17      :term "sendable stop signal not from tty ")
            (:SIGTSTP 18      "stop signal from tty ")
            (:SIGCONT 19      "continue a stopped process ")
            (:SIGCHLD 20      "to parent on child stop or exit ")
            (:SIGTTIN 21      "to readers pgrp upon background tty read ")
            (:SIGTTOU 22      "like TTIN for output if (tp->t_local&LTOSTOP) ")
            (:SIGIO   23      "input/output possible signal ")
            (:SIGXCPU 24      "exceeded CPU time limit ")
            (:SIGXFSZ 25      "exceeded file size limit ")
            (:SIGVTALRM 26    "virtual time alarm ")
            (:SIGPROF 27      "profiling time alarm ")
            (:SIGWINCH 28     "window size changes ")
            (:SIGINFO 29      "information request ")
            (:SIGUSR1 30      "user defined signal 1 ")
            (:SIGUSR2 31      "user defined signal 2 ")))
         (found (Assoc signal signal-alist)))
    (if found
        (second found)
        nil)))
        
(defun note-signal-handler (signal function)
  (setf (getf *signal-to-function* signal) function))

(defun forget-signal-handler (signal)
  (when *signal-to-function*
    (remf *signal-to-function* signal)))

(defun get-signal-handler (signal)
  (getf *signal-to-function* signal))

(defun call-lisp-symbol-handler (signal-as-fixnum)
  (let ((function (get-signal-handler signal-as-fixnum)))
    (if function
        (funcall function signal-as-fixnum)
        (error 'ext:unix-signal-received :code signal-as-fixnum))))


