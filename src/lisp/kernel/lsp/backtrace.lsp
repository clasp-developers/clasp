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
  type ; :lisp :c++ :unknown
  return-address
  raw-name
  function-name
  print-name
  function-start-address
  function-length-bytes
  base-pointer
  next-base-pointer
  arguments
  shadow-frame)


;;; Common Lisp functions maintain a shadow stack of arguments
;;; including closure arguments.  When generating a backtrace
;;; shadow-backtrace-frame is used to represent each shadow stack frame
(defstruct (shadow-backtrace-frame (:type vector) :named)
  index
  frame-address
  function-name
  function
  arguments
  environment)

(defun dump-jit-symbol-info ()
  (maphash (lambda (key value)
             (let ((func-size (first value))
                   (func-start (second value)))
               (bformat t "%s -> %s %s\n" key func-start func-size)))
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

(defun parse-frame (return-address backtrace-name base-pointer next-base-pointer verbose maybe-shadow-frame)
  ;; Get the name
  (let (pos)
    (if verbose (bformat *debug-io* "backtrace-name: %s\n" backtrace-name))
    (cond
      ;; On linux we don't get Common Lisp function names from backtrace_symbols
      ;; I don't know why not.  So set the name to :unknown-lisp-function and when
      ;; we merge the backtrace with the shadow backtrace we will use the names of
      ;; the Common Lisp functions
      (maybe-shadow-frame
       (make-backtrace-frame :type :lisp
                             :return-address return-address
                             :function-name (ensure-function-name (shadow-backtrace-frame-function-name maybe-shadow-frame))
                             :print-name (shadow-backtrace-frame-function-name maybe-shadow-frame)
                             :raw-name (shadow-backtrace-frame-function-name maybe-shadow-frame)
                             :arguments (shadow-backtrace-frame-arguments maybe-shadow-frame)
                             :shadow-frame maybe-shadow-frame
                             :base-pointer base-pointer
                             :next-base-pointer next-base-pointer))
     ((eq backtrace-name :unknown-lisp-function)
      (make-backtrace-frame :type :lisp
                            :return-address return-address
                            :raw-name :unknown-lisp-function
                            :function-name :unknown-lisp-function
                            :print-name :unknown-lisp-function
                            :base-pointer base-pointer
                            :next-base-pointer next-base-pointer))
     ;; If the backtrace-name starts with 0x then it is a return address for a JITted function
     ;; lookup the JITted function and create a backtrace frame for it.
     ((string= (subseq backtrace-name 0 (min 2 (length backtrace-name))) "0x")
      (let* ((symbol-info (llvm-sys:lookup-jit-symbol-info return-address))
;;;              (_ (core:bformat t "symbol-info address: %s -> %s\n" return-address symbol-info))
             (jit-name (first symbol-info))
             (function-bytes (second symbol-info))
             (function-start-address (third symbol-info)))
        ;; strip leading #\_
        (if (and jit-name (search "^^" jit-name))
            (let* ((name-with-parts #+target-os-linux jit-name
                                    #+target-os-freebsd (subseq jit-name 1 (length jit-name))
                                    #+target-os-darwin (subseq jit-name 1 (length jit-name)))
                   (parts (cmp:unescape-and-split-jit-name name-with-parts))
                   (symbol-name (first parts))
                   (package-name (second parts))
                   (name (intern symbol-name (or package-name :keyword))))
              (if verbose (bformat *debug-io* "-->JITted CL frame\n"))
              (make-backtrace-frame :type :lisp
                                    :return-address return-address
                                    :raw-name jit-name
                                    :function-name (ensure-function-name name)
                                    :print-name (cmp:print-name-from-unescaped-split-name jit-name parts name)
                                    :function-start-address function-start-address
                                    :function-length-bytes function-bytes
                                    :base-pointer base-pointer
                                    :next-base-pointer next-base-pointer))
          (progn
            (if verbose (bformat *debug-io* "-->JITted unknown frame\n"))
            (make-backtrace-frame :type :unknown
                                  :return-address return-address
                                  :function-name (ensure-function-name jit-name)
                                  :print-name (or jit-name (core:bformat nil "%s" return-address))
                                  :raw-name backtrace-name
                                  :base-pointer base-pointer
                                  :next-base-pointer next-base-pointer)))))
     ;; For some reason only darwin backtrace_symbols gives us Common Lisp function names
     #+target-os-darwin
     ((setq pos (search "^^" backtrace-name :from-end t))
      (if verbose (bformat *debug-io* "-->CL frame\n"))
      (let* ((parts (cmp:unescape-and-split-jit-name (subseq backtrace-name 0 pos)))
             (symbol-name (first parts))
             (package-name (second parts))
             (name (intern symbol-name (or package-name :keyword))))
        (make-backtrace-frame :type :lisp
                              :return-address return-address
                              :raw-name backtrace-name
                              :print-name (cmp:print-name-from-unescaped-split-name backtrace-name parts name)
                              :function-name (ensure-function-name name)
                              :base-pointer base-pointer
                              :next-base-pointer next-base-pointer)))
     ;; It's a C++ function with a mangled name
     (t (let* ((first-space (position-if (lambda (c) (char= c #\space)) backtrace-name))
               (just-name (if first-space
                              (subseq backtrace-name 0 first-space)
                            backtrace-name))
               (unmangled (core::maybe-demangle just-name)))
          (if verbose (bformat *debug-io* "-->C++ frame\n"))
          (make-backtrace-frame :type :c
                                :return-address return-address
                                :raw-name backtrace-name
                                :print-name (or unmangled just-name)
                                :function-name (if unmangled
                                                   (ensure-function-name unmangled)
                                                   (ensure-function-name just-name))
                                :base-pointer base-pointer
                                :next-base-pointer next-base-pointer))))))

;;; Search for a backtrace frame that matches the shadow stack frame.
;;; The shadow stack frames are stored in the threads stack - so the
;;; matching compares the address of the shadow stack frame to the
;;; frame pointers of the thread stack frames and their following frames.
(defun search-for-matching-frame (frames entry)
  (let ((saved-frames frames)
        (frame-address (shadow-backtrace-frame-frame-address entry)))
    (do* ((cur frames (cdr cur))
          (frame (car cur) (car cur)))
         ((null cur) (values (or cur saved-frames) nil))
      (let ((bp (backtrace-frame-base-pointer frame))
            (next-bp (backtrace-frame-next-base-pointer frame)))
        (when (and bp next-bp
                   (pointer-in-pointer-range frame-address bp next-bp))
          (return-from search-for-matching-frame (values cur t)))))))

(defun search-for-matching-shadow-frame (base-pointer next-base-pointer shadow-backtrace)
  (when shadow-backtrace
    (do* ((cur shadow-backtrace (cdr cur))
          (shadow-frame (car cur) (car cur)))
         ((or (null cur) (and next-base-pointer
                              base-pointer
                              (pointer-in-pointer-range (shadow-backtrace-frame-frame-address shadow-frame) base-pointer next-base-pointer))) shadow-frame))))

;;; Attach the shadow backtrace frames to the matching thread backtrace frames.
(defun attach-shadow-backtrace (orig-frames shadow-backtrace)
  (let ((frames orig-frames))
    (dolist (shadow-entry shadow-backtrace)
      (multiple-value-bind (frame-cur found)
          (search-for-matching-frame frames shadow-entry)
        (if found
            (let ((frame (car frame-cur)))
              (setf (backtrace-frame-arguments frame) (shadow-backtrace-frame-arguments shadow-entry))
              (setf frames frame-cur)
              (setf (backtrace-frame-shadow-frame frame) shadow-entry)
              (when (eq (backtrace-frame-raw-name frame) :unknown-lisp-function)
                (setf (backtrace-frame-raw-name frame) (shadow-backtrace-frame-function-name shadow-entry))
                (setf (backtrace-frame-print-name frame) (shadow-backtrace-frame-function-name shadow-entry))
                (setf (backtrace-frame-function-name frame) (format nil "~a" (shadow-backtrace-frame-function-name shadow-entry)))))
            (bformat t "attach-shadow-backtrace could not find stack frame for address: %s\n" (shadow-backtrace-frame-address shadow-entry))))))
  (let ((new-frames (add-interpreter-frames orig-frames)))
    (nreverse new-frames)))

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
  #+target-os-freebsd
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
(defun backtrace-as-list (&optional verbose)
  (let ((clib-backtrace (core:clib-backtrace-as-list))
        (shadow-backtrace (core:shadow-backtrace-as-list))
        result
        prev-base-pointer)
    (do* ((cur clib-backtrace (cdr cur))
          (clib-frame (car cur) (car cur))
          (next-clib-frame (cadr cur) (cadr cur)))
         ((null next-clib-frame) nil)
      (let ((base-pointer (third clib-frame)))
        (when prev-base-pointer 
          (let* ((address (first clib-frame))
                 (line (second clib-frame))
                 (name (extract-backtrace-frame-name line))
                 (maybe-shadow-frame (search-for-matching-shadow-frame prev-base-pointer base-pointer shadow-backtrace))
                 (entry (parse-frame address name prev-base-pointer base-pointer verbose maybe-shadow-frame)))
            (push entry result)))
        (setf prev-base-pointer base-pointer)))
    (nreverse result)))

;;; Get the backtrace and the shadow-backtrace and merge them
(defun backtrace-with-arguments ()
  (backtrace-as-list)
  #+(or)(let ((ordered (backtrace-as-list))
        (shadow-backtrace (core:shadow-backtrace-as-list)))
    (attach-shadow-backtrace ordered shadow-backtrace)))


;;; Extract just the Common Lisp backtrace frames
;;; starting from a frame that satisfies gather-start-trigger
(defun common-lisp-backtrace-frames (&key verbose (focus t)
                                       (gather-start-trigger nil)
                                       gather-all-frames)
  "Extract the common lisp backtrace frames.  Provide a gather-start-trigger function
that takes one argument (the backtrace-frame-function-name) and returns T it should trigger when to start
recording backtrace frames for Common Lisp.   Looking for the 'UNIVERSAL-ERROR-HANDLER' string
is one way to eliminate frames that aren't interesting to the user.
Set gather-all-frames to T and you can gather C++ and Common Lisp frames"
  (let ((frames (backtrace-with-arguments)))
    (flet ((gather-frames (start-trigger)
             (let (result
                   (state (if gather-start-trigger :skip-first-frames :gather))
                   new-state)
               (dolist (frame frames)
                 (let ((func-name (backtrace-frame-function-name frame)))
                   ;; State machine
                   (setq new-state (cond
                                     ((and (eq state :skip-first-frames)
                                           start-trigger
                                           (funcall start-trigger frame))
                                      :gather)
                                     (t state)))
                   (when verbose
                     (bformat t "-----state -> %s   new-state -> %s\n" state new-state)
                     (bformat t "     frame: %s\n" frame))
                   (setq state new-state)
                   (when (and (eq state :gather) (or gather-all-frames (eq (backtrace-frame-type frame) :lisp)))
                     (when verbose (bformat t "     PUSHED!\n"))
                     (push frame result))))
               (nreverse result))))
      (let ((frames (gather-frames gather-start-trigger)))
        (if frames
            frames
            (gather-frames nil))        ; fallback and gather all
        ))))

;;; A quick and dirty way to work with btcl frames
(defvar *current-btcl-frames* nil)
(defvar *current-btcl-index* 0)
(defun cbtcl-frame (index)
  (elt *current-btcl-frames* index))

(defun cbtcl-func (index)
  (backtrace-frame-print-name (cbtcl-frame index)))

(defun cbtcl-arguments (index)
  (backtrace-frame-arguments (cbtcl-frame index)))

(defun btcl-frames (&key all)
  (setq *current-btcl-index* 0)
  (let ((frames (if all
                    (common-lisp-backtrace-frames)
                    (common-lisp-backtrace-frames
                     :gather-start-trigger
                     (lambda (x)
                       (eq 'core:universal-error-handler (backtrace-frame-function-name x)))))))
    (unless frames
      (setf frames (common-lisp-backtrace-frames)))
    (setq *current-btcl-frames* frames)
    frames))

(defun btcl (&key all (args t) (stream *standard-output*))
  "Print backtrace of just common lisp frames.  Set args to nil if you don't want arguments printed"
  (let ((l (btcl-frames :all all))
        (index 0))
    (dolist (e l)
      (let ((name (backtrace-frame-print-name e))
            (arguments (backtrace-frame-arguments e)))
        (if arguments
            (progn
              (prin1 (prog1 index (incf index)) stream)
              (princ ": (" stream)
              (princ name stream)
              (if (> (length arguments) 0)
                  (progn
                    (if args 
                        (dotimes (i (length arguments))
                          (princ #\space stream)
                          (prin1 (aref arguments i) stream))
                      (prin1 " -args-suppressed-" stream))))
              (princ ")") stream)
          (progn
            (prin1 (prog1 index (incf index)) stream)
            (princ #\space stream)
            (princ name stream)))
        (terpri stream)))))

(defun bt ()
  (let ((l (backtrace-as-list)))
    (bformat t "There are %s frames\n" (length l))
    (dolist (e l)
          (bformat t "%s\n" (backtrace-frame-function-name e)))))

(defun btargs ()
  (let ((l (backtrace-with-arguments)))
    (bformat t "There are %s frames\n" (length l))
    (dolist (e l)
      (let ((name (backtrace-frame-print-name e))
            (arguments (backtrace-frame-arguments e)))
        (if arguments
            (progn
              (princ name)
              (dotimes (i (length arguments))
                (princ #\space)
                (prin1 (aref arguments i))))
            (progn
              (princ name)))
        (terpri)))))

(export '(btcl bt btargs common-lisp-backtrace-frames
          backtrace-frame-function-name backtrace-frame-arguments))
