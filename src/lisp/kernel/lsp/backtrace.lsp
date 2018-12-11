;;;
;;; Christian Schafmeister Sept 2017
;;;
;;; Generate backtraces using a combination of libc backtrace, backtrace_symbols, jit symbols and the shadow stack.
;;; It pulls as much information out of these sources as it can and correlates them to produce as rich a backtrace as possible.
;;; If you turn on (declare (debug 3)) in a function then call arguments will be saved at run-time and made available through the stack.
;;; Jitted symbols are correlated to return addresses from 'backtrace' and attached to call frames.
;;; 

(in-package :core)

(defstruct (backtrace-frame (:type vector) :named)
  type                                  ; 1 ( :lisp | :c++ | :unknown)
  return-address                        ; 2
  raw-name                              ; 3
  function-name                         ; 4
  print-name                            ; 5
  frame-size                            ; 6
  frame-offset                          ; 7
  function-start-address                ; 8
  function-end-address                  ; 9
  base-pointer                          ; 10
  arguments                             ; 11
  closure                               ; 12
  function-description                  ; 13
  )


(defun dump-jit-symbol-info ()
  (maphash (lambda (key value)
             (let ((func-size (first value))
                   (func-start (second value)))
               (bformat t "%s -> %s %s%N" key func-start func-size)))
           cmp::*jit-saved-symbol-info*)
  (values))


;;; Use a return address to identify the JITted function that contains it
(defun locate-jit-symbol-info (address)
  (maphash (lambda (key value)
             (let ((func-size (first value))
                   (func-start (second value)))
               (if (core:pointer-in-pointer-range address func-start func-size)
                   (return-from locate-jit-symbol-info (values key func-start func-size)))))
           cmp::*jit-saved-symbol-info*)
  (values))

(defun ensure-function-name (name)
  "Return a symbol or cons that can be used as a function name in a backtrace.
For Lisp it's easy - it must be a symbol or (setf symbol) - return that.
For C/C++ frames - return (list 'c-function name)."
  (cond
    ((symbolp name) name)
    ((consp name) name)
    ((stringp name) (list :c-function name))
    (t (error "Illegal name for function ~a" name))))

(defconstant-equal +interpreted-closure-entry-point+ "core::interpretedClosureEntryPoint")
(defconstant +interpreted-closure-entry-point-length+ (length +interpreted-closure-entry-point+))
;;; Interpreted closures have their shadow stack frames merged in a different way
(defun add-interpreter-frames (frames)
  (let (new-frames)
    (dolist (frame frames)
      (when (backtrace-frame-shadow-frame frame)
        (when (and (stringp (backtrace-frame-function-name frame))
                   (string= +interpreted-closure-entry-point+ (backtrace-frame-function-name frame)
                            :start2 0 :end2 +interpreted-closure-entry-point-length+))
          (let* ((shadow-frame (backtrace-frame-shadow-frame frame))
                 (interpreted-frame (make-backtrace-frame :type :lisp
                                                          :return-address nil 
                                                          :raw-name (shadow-backtrace-frame-function-name shadow-frame)
                                                          :function-name (ensure-function-name (shadow-backtrace-frame-function-name shadow-frame))
                                                          :print-name (shadow-backtrace-frame-function-name shadow-frame)
                                                          :arguments (shadow-backtrace-frame-arguments shadow-frame))))
            (push interpreted-frame new-frames))))
      (push frame new-frames))
    new-frames))

;;; This is messy - backtrace_symbols for macOS and Linux return different strings
;;; Extracting the name and the return address from the strings handled differently
;;; depending on the operating system.
(defun extract-backtrace-frame-name (line)
  ;; On OS X this is how we get the name part
  ;; The format seems to be as follows:
  ;; "framenumber    processname        address      functionname"
  ;; with variable numbers of spaces. processname is always at character 4 so we start there.
  #+target-os-darwin
  (let* ((pos1 (position-if (lambda (c) (char/= c #\space)) line :start 4)) ; skip whitespace
         (pos1e (position-if (lambda (c) (char= c #\space)) line :start pos1)) ; skip chars
         (pos2 (position-if (lambda (c) (char/= c #\space)) line :start pos1e))
         (pos2e (position-if (lambda (c) (char= c #\space)) line :start pos2)) ; skip chars
         (pos3 (position-if (lambda (c) (char/= c #\space)) line :start pos2e)))
    (subseq line pos3 (length line)))
  #+target-os-freebsd ; fixme cracauer, confirm that this works
  (let* ((pos1 (position-if (lambda (c) (char/= c #\space)) line :start 4)) ; skip whitespace
         (pos1e (position-if (lambda (c) (char= c #\space)) line :start pos1)) ; skip chars
         (pos2 (position-if (lambda (c) (char/= c #\space)) line :start pos1e))
         (pos2e (position-if (lambda (c) (char= c #\space)) line :start pos2)) ; skip chars
         (pos3 (position-if (lambda (c) (char/= c #\space)) line :start pos2e)))
    (subseq line pos3 (length line)))
  #+target-os-linux
  (let ((pos-open (position-if (lambda (c) (char= c #\()) line)))
    (if pos-open
        (let* ((pos-name-start (1+ pos-open))
               (pos-close (position-if (lambda (c) (char= c #\))) line :start pos-name-start :end nil :from-end t))
               (pos-name-end (position #\+ line :start pos-name-start :end pos-close :from-end t))
               (name (cond
                      ((null pos-name-end) :unknown-lisp-function)
                      ((= pos-name-start pos-name-end) :unknown-lisp-function)
                      (t (subseq line pos-name-start pos-name-end)))))
          name)
      (let* ((square-open (position #\[ line))
             (square-close (position #\] line)))
        (if (and square-open square-close)
            (subseq line (1+ square-open) square-close)
          :unknown-lisp-function)))))

;;; Return the backtrace as a list of backtrace-frame
(defun backtrace-as-list (clib-backtrace &optional verbose)
  (dolist (frame clib-backtrace)
    (let ((common-lisp-name (backtrace-frame-raw-name frame)))
      (if (and common-lisp-name (search "^^" common-lisp-name))
          (let* ((name-with-parts #+target-os-linux common-lisp-name
                                  ;; fixme cracauer.  Find out what FreeBSD actually needs here
                                  #+target-os-freebsd common-lisp-name
                                  #+target-os-darwin (if (char= (char common-lisp-name 0) #\_)
                                                         (subseq common-lisp-name 1 (length common-lisp-name)) ; strip preceeding "_"
                                                         common-lisp-name))
                 (parts (cmp:unescape-and-split-jit-name name-with-parts))
                 (symbol-name (first parts))
                 (package-name (second parts))
                 (maybe-fn-or-specializers (third parts))
                 (maybe-method (fourth parts))
                 (name (intern symbol-name (or package-name :keyword)))
                 (print-name (cond
                               ((string= maybe-method "METHOD")
                                (format nil "(METHOD ~a ~a)" name maybe-fn-or-specializers))
                               (t name))))
            (setf (backtrace-frame-function-name frame) name
                  (backtrace-frame-print-name frame) print-name))
          (setf (backtrace-frame-function-name frame) common-lisp-name
                (backtrace-frame-print-name frame) common-lisp-name))))
  clib-backtrace)


(defun gather-frames (frames start-trigger &key verbose)
  (let ((state (if start-trigger
                   :skip-first-frames
                   :gather)) ; If the start-trigger is NIL start gathering right away
        new-state result)
    (dolist (frame frames)
      (let ((func-name (backtrace-frame-function-name frame)))
        ;; State machine
        (setq new-state (cond
                          ((and (eq state :skip-first-frames)
                                (funcall start-trigger frame))
                           :gather)
                          (t state)))
        (when verbose
          (bformat t "-----state -> %s   new-state -> %s%N" state new-state)
          (bformat t "     frame: %s%N" frame))
        (setq state new-state)
        (when (and (eq (backtrace-frame-type frame) :lisp)
                   (eq state :gather))
          (when verbose (bformat t "     PUSHED!%N"))
          (push frame result))))
    (reverse result)))

;;; Extract just the Common Lisp backtrace frames
;;; starting from a frame that satisfies gather-start-trigger
(defun common-lisp-backtrace-frames (backtrace &key verbose (focus t)
                                       (gather-start-trigger nil)
                                       gather-all-frames)
  "Extract the common lisp backtrace frames.  Provide a gather-start-trigger function
that takes one argument (the backtrace-frame-function-name) and returns T it should trigger when to start
recording backtrace frames for Common Lisp.   Looking for the 'UNIVERSAL-ERROR-HANDLER' string
is one way to eliminate frames that aren't interesting to the user.
Set gather-all-frames to T and you can gather C++ and Common Lisp frames"
  (let ((frames (backtrace-as-list backtrace)))
    (cond
      ((gather-frames frames gather-start-trigger :verbose verbose)) ; Start gathering on a specific frame
      ((gather-frames frames (lambda (frame) (eq :lisp (backtrace-frame-type frame))) :verbose verbose)) ; return all :LISP frames
      (t frames)))) ; return ALL frames


(defun btcl (&key all (args t) (stream *standard-output*))
  "Print backtrace of just common lisp frames.  Set args to nil if you don't want arguments printed"
  (core:call-with-backtrace
   (lambda (raw-backtrace)
     (let ((frames (common-lisp-backtrace-frames raw-backtrace))
           (index 0))
       (dolist (e frames)
         (let ((name (backtrace-frame-print-name e))
               (arguments (backtrace-frame-arguments e)))
           (if arguments
               (progn
                 (prin1 (prog1 index (incf index)) stream)
                 (write-string ": (" stream)
                 (princ name stream)
                 (if (> (length arguments) 0)
                     (progn
                       (if args 
                           (dotimes (i (length arguments))
                             (princ #\space stream)
                             (prin1 (aref arguments i) stream))
                           (prin1 " -args-suppressed-" stream))))
                 (princ ")" stream))
               (progn
                 (prin1 (prog1 index (incf index)) stream)
                 (write-string ": " stream)
                 (princ name stream)))
           (terpri stream)))))))

(export '(btcl common-lisp-backtrace-frames
          backtrace-frame-function-name backtrace-frame-arguments))

(defmacro with-dtrace-trigger (&body body)
  `(unwind-protect
        (progn
          (core:trigger-dtrace-start
           (lambda ()
             ,@body)))
     (core:trigger-dtrace-stop)))

