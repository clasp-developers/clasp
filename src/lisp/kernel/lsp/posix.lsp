(in-package :ext)

(defun enable-interrupt (signal mode &optional lisp-handler)
  "Enable/disable signal, 
   If mode = :ignore, sets a handler that ignores the signal
   If mode = :default, it sets the handler to the default handler.
   If mode = :lisp, define lisp-handler for signal.
   lisp-handler must only be provided, if mode = :lisp.
   lisp-handler should be a function of one argument (the signal)
   Returns :done if successfull,
           signals an error, if signal is not recognized or cannot be set"
  (flet ((handle-lisp-handler (signal-as-int)
           (core::note-signal-handler signal-as-int lisp-handler)
           2)
         (handle-non-lisp-handler (signal-as-int)
           (core::forget-signal-handler signal-as-int)))
    (let ((int-signal (core::external-to-int-signal signal))
          (result nil))
      (cond (int-signal
             (setq result (core:enable-disable-signals
                           int-signal
                           (ecase mode
                             (:ignore  (handle-non-lisp-handler int-signal) 0)
                             (:default (handle-non-lisp-handler int-signal) 1)
                             (:lisp (handle-lisp-handler int-signal)))))
             (if (zerop result)
                 :done
                 (error "Setting signal ~s in mode ~s errored " signal mode)))
            (t (error "Signal ~s not recognized, Handler cannot be set" signal))))))

(defun default-interrupt (signal)
  "Sets the handler to the default handler for signal"
  (enable-interrupt signal :default))

(defun ignore-interrupt (signal)
  "Sets a handler that ignores the signal"
  (enable-interrupt signal :ignore))

(defun get-signal-handler (signal)
  "Get a lisp handler handler for signal, or signals an error, if signal is not recognized"
  (let ((internal-signal (core::external-to-int-signal signal)))
    (if internal-signal
        (core::get-signal-handler internal-signal)
        (error "Signal ~s not recognized, Handler cannot be read" signal))))

(defun set-signal-handler (signal handler)
  "Set a lisp handler handler for signal"
  (enable-interrupt signal :lisp handler))

(in-package :core)

(defparameter *signal-to-function* nil)


;;; See https://pubs.opengroup.org/onlinepubs/009695399/basedefs/signal.h.html
;;; Only include posix signals
;;; Codes taken from https://gitlab.common-lisp.net/cmucl/cmucl/-/blob/master/src/code/signal.lisp
;;; #+linux is only valid for linux on x86/ARM (http://man7.org/linux/man-pages/man7/signal.7.html)
;;; bsd signal mapping verified with https://www.freebsd.org/cgi/man.cgi?query=signal&sektion=3&manpath=freebsd-release-ports
(defun external-to-int-signal (signal)
  (let* ((signal-alist
          '((:SIGHUP  1       :term "hangup ")
            (:SIGINT  2       :term "interrupt ")
            (:SIGQUIT 3       :core "quit ")
            (:SIGILL  4       :core "illegal instruction (not reset when caught) ")
            (:SIGTRAP 5       :unknown "trace trap (not reset when caught) ")
            (:SIGABRT 6       :core "abort() ")
            #+linux (:SIGPOLL 29       :unknown"pollable event ([XSR] generated, not supported) ")
            #+(or)(:SIGIOT  6       :core "compatibility ")
            #+(or) (:SIGEMT  7       :unknown "EMT instruction ")
            (:SIGFPE  8       :core "floating point exception ")
            (:SIGKILL 9       :term "kill (cannot be caught or ignored) ")
            (:SIGBUS  #-linux 10 #+linux 7     :term "bus error ")
            (:SIGSEGV 11      :core "segmentation violation ")
            (:SIGSYS  #-linux 12 #+linux 31     :unknown "bad argument to system call ")
            (:SIGPIPE 13      :term "write on a pipe with no one to read it ")
            (:SIGALRM 14      :term "alarm clock ")
            (:SIGTERM 15      :term "software termination signal from kill ")
            (:SIGURG  #-linux 16 #+linux 23      :term "urgent condition on IO channel ")
            (:SIGSTOP #-linux 17 #+linux 19      :term "sendable stop signal not from tty ")
            (:SIGTSTP #-linux 18 #+linux 20     "stop signal from tty ")
            (:SIGCONT #-linux 19 #+linux 18     "continue a stopped process ")
            (:SIGCHLD #-linux 20 #+linux 17     "to parent on child stop or exit ")
            (:SIGTTIN 21      "to readers pgrp upon background tty read ")
            (:SIGTTOU 22      "like TTIN for output if (tp->t_local&LTOSTOP) ")
            #+(or) (:SIGIO   #-linux 23 #+linux 29     "input/output possible signal ")
            (:SIGXCPU 24      "exceeded CPU time limit ")
            (:SIGXFSZ 25      "exceeded file size limit ")
            (:SIGVTALRM 26    "virtual time alarm ")
            (:SIGPROF 27      "profiling time alarm ")
            #+(or) (:SIGWINCH 28     "window size changes ")
            #+(or) (:SIGINFO 29      "information request ")
            (:SIGUSR1 #-linux 30 #+linux 10     "user defined signal 1 ")
            (:SIGUSR2 #-linux 31 #+linux 12     "user defined signal 2 ")))
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


