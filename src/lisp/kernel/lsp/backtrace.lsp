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
  shadow-frame
  )


(defstruct (shadow-backtrace-frame (:type vector) :named)
  index
  frame-address
  function-name
  function
  arguments
  environment
  )

(defun dump-jit-symbol-info ()
  (maphash (lambda (key value)
             (let ((func-size (first value))
                   (func-start (second value)))
               (bformat t "%s -> %s %s\n" key func-start func-size)))
           cmp::*jit-saved-symbol-info*)
  (values))


(defun locate-jit-symbol-info (address)
  (maphash (lambda (key value)
             (let ((func-size (first value))
                   (func-start (second value)))
               (if (core:pointer-in-pointer-range address func-start func-size)
                   (return-from locate-jit-symbol-info (values key func-start func-size)))))
           cmp::*jit-saved-symbol-info*)
  (values))




(defun parse-frame (return-address backtrace-name base-pointer next-base-pointer verbose)
  ;; Get the name
  (let (pos)
    (if verbose (bformat *debug-io* "backtrace-name: %s\n" backtrace-name))
    (cond
     ;; If there is no backtrace_symbol info - it's probably jitted
     ((eq backtrace-name :unknown-lisp-function)
      (make-backtrace-frame :type :lisp
                            :return-address return-address
                            :raw-name :unknown-lisp-function
                            :function-name :unknown-lisp-function
                            :print-name :unknown-lisp-function
                            :base-pointer base-pointer
                            :next-base-pointer next-base-pointer))
     ((string= (subseq backtrace-name 0 (min 2 (length backtrace-name))) "0x")
      (let* ((symbol-info (llvm-sys:lookup-jit-symbol-info return-address))
;;;              (_ (core:bformat t "symbol-info address: %s -> %s\n" return-address symbol-info))
             (jit-name (first symbol-info))
             (function-bytes (second symbol-info))
             (function-start-address (third symbol-info)))
        ;; strip leading #\_
        (if (and jit-name (search "^^" jit-name))
            (let* ((parts (cmp:unescape-and-split-jit-name (subseq jit-name 1 (length jit-name))))
                   (symbol-name (first parts))
                   (package-name (second parts))
                   (name (intern symbol-name (or package-name :keyword))))
              (if verbose (bformat *debug-io* "-->JITted CL frame\n"))
              (make-backtrace-frame :type :lisp
                                    :return-address return-address
                                    :raw-name jit-name
                                    :function-name name
                                    :print-name (cmp:print-name-from-unescaped-split-name jit-name parts name)
                                    :function-start-address function-start-address
                                    :function-length-bytes function-bytes
                                    :base-pointer base-pointer
                                    :next-base-pointer next-base-pointer))
          (progn
            (if verbose (bformat *debug-io* "-->JITted unknown frame\n"))
            (make-backtrace-frame :type :unknown
                                  :return-address return-address
                                  :function-name (or jit-name (core:bformat nil "%s" return-address))
                                  :print-name (or jit-name (core:bformat nil "%s" return-address))
                                  :raw-name backtrace-name
                                  :base-pointer base-pointer
                                  :next-base-pointer next-base-pointer)))))
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
                              :function-name name
                              :base-pointer base-pointer
                              :next-base-pointer next-base-pointer)))
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
                                :function-name (or unmangled just-name)
                                :base-pointer base-pointer
                                :next-base-pointer next-base-pointer))))))


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

(defun merge-shadow-backtrace (orig-frames shadow-backtrace)
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
                (setf (backtrace-frame-raw-name frame) (string (shadow-backtrace-frame-function-name shadow-entry)))
                (setf (backtrace-frame-print-name frame) (string (shadow-backtrace-frame-function-name shadow-entry)))
                (setf (backtrace-frame-function-name frame) (shadow-backtrace-frame-function-name shadow-entry))))
            (bformat t "merge-shadow-backtrace could not find stack frame for address: %s\n" (frame-iterator-frame-address shadow-entry))))))
  (let ((new-frames (add-interpreter-frames orig-frames)))
    (nreverse new-frames)))

(defconstant-equal +interpreted-closure-entry-point+ "core::interpretedClosureEntryPoint")
(defconstant +interpreted-closure-entry-point-length+ (length +interpreted-closure-entry-point+))
(defun add-interpreter-frames (frames)
  (let (new-frames)
    (dolist (frame frames)
      (when (backtrace-frame-shadow-frame frame)
        (when (string= +interpreted-closure-entry-point+ (backtrace-frame-function-name frame)
                       :start2 0 :end2 +interpreted-closure-entry-point-length+)
          (let* ((shadow-frame (backtrace-frame-shadow-frame frame))
                 (interpreted-frame (make-backtrace-frame :type :lisp
                                                          :return-address nil 
                                                          :raw-name (core:frame-iterator-function-name shadow-frame)
                                                          :function-name (core:frame-iterator-function-name shadow-frame)
                                                          :print-name (core:frame-iterator-function-name shadow-frame)
                                                          :arguments (core:frame-iterator-arguments shadow-frame))))
            (push interpreted-frame new-frames))))
      (push frame new-frames))
    new-frames))

    
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

(defun backtrace-as-list (&optional verbose)
  (let ((clib-backtrace (core:clib-backtrace-as-list))
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
                 (entry (parse-frame address name prev-base-pointer base-pointer verbose)))
            (push entry result)))
        (setf prev-base-pointer base-pointer)))
    (nreverse result)))

(defun backtrace-with-arguments ()
  (let ((ordered (backtrace-as-list))
        (shadow-backtrace (core:shadow-backtrace-as-list)))
    (merge-shadow-backtrace ordered shadow-backtrace)))

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

(defun btcl (&key all (args t))
  "Print backtrace of just common lisp frames.  Set args to nil if you don't want arguments printed"
  (let ((l (btcl-frames :all all))
        (index 0))
    (dolist (e l)
      (let ((name (backtrace-frame-print-name e))
            (arguments (backtrace-frame-arguments e)))
        (if arguments
            (progn
              (prin1 (prog1 index (incf index)))
              (princ " (")
              (princ name)
              (if (> (length arguments) 0)
                  (progn
                    (if args 
                        (dotimes (i (length arguments))
                          (princ #\space)
                          (prin1 (aref arguments i)))
                        (prin1 " -args-suppressed-"))
                    (princ ")"))))
            (progn
              (prin1 (prog1 index (incf index)))
              (princ #\space )
              (princ name)))
        (terpri)))))

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

(export '(btcl bt btargs common-lisp-backtrace-frames))
