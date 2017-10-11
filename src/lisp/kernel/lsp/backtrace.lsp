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




(defun parse-frame (return-address backtrace-string base-pointer next-base-pointer verbose)
  ;; Get the name
  (let (pos)
    (if verbose (bformat *debug-io* "backtrace-string: %s\n" backtrace-string))
    (cond
      ;; If there is no backtrace_symbol info - it's probably jitted
      ((string= (subseq backtrace-string 0 (min 3 (length backtrace-string))) "0x0")
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
                                     :raw-name backtrace-string
                                     :base-pointer base-pointer
                                     :next-base-pointer next-base-pointer)))))
      ((setq pos (search "^^" backtrace-string :from-end t))
       (if verbose (bformat *debug-io* "-->CL frame\n"))
       (let* ((parts (cmp:unescape-and-split-jit-name (subseq backtrace-string 0 pos)))
              (symbol-name (first parts))
              (package-name (second parts))
              (name (intern symbol-name (or package-name :keyword))))
         (make-backtrace-frame :type :lisp
                               :return-address return-address
                               :raw-name backtrace-string
                               :print-name (cmp:print-name-from-unescaped-split-name backtrace-string parts name)
                               :function-name name
                               :base-pointer base-pointer
                               :next-base-pointer next-base-pointer)))
      (t (let* ((first-space (position-if (lambda (c) (char= c #\space)) backtrace-string))
                (just-name (if first-space
                               (subseq backtrace-string 0 first-space)
                               backtrace-string))
                (unmangled (core::maybe-demangle just-name)))
           (if verbose (bformat *debug-io* "-->C++ frame\n"))
           (make-backtrace-frame :type :c
                                 :return-address return-address
                                 :raw-name backtrace-string
                                 :print-name (or unmangled just-name)
                                 :function-name (or unmangled just-name)
                                 :base-pointer base-pointer
                                 :next-base-pointer next-base-pointer))))))


(defun search-for-matching-frame (frames entry)
  (let ((saved-frames frames)
        (frame-address (frame-iterator-frame-address entry)))
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
              (setf (backtrace-frame-arguments frame) (frame-iterator-arguments shadow-entry))
              (setf frames frame-cur)
              (setf (backtrace-frame-shadow-frame frame) shadow-entry))
            (bformat t "Could not find stack frame for address: %s\n" (frame-iterator-frame-address shadow-entry))))))
  (let ((new-frames (add-interpreter-frames orig-frames)))
    (nreverse new-frames)))

(defconstant +interpreted-closure-entry-point+ "core::interpretedClosureEntryPoint")
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
  (let* ((pos0 (position-if (lambda (c) (char/= c #\space)) line)) ; skip initial whitespace
         (pos0e (position-if (lambda (c) (char= c #\space)) line :start pos0)) ; skip chars
         (pos1 (position-if (lambda (c) (char/= c #\space)) line :start pos0e)) ; skip whitespace
         (pos1e (position-if (lambda (c) (char= c #\space)) line :start pos1)) ; skip chars
         (pos2 (position-if (lambda (c) (char/= c #\space)) line :start pos1e))
         (pos2e (position-if (lambda (c) (char= c #\space)) line :start pos2)) ; skip chars
         (pos3 (position-if (lambda (c) (char/= c #\space)) line :start pos2e)))
    (subseq line pos3 (length line))))

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
                                       (gather-start-trigger nil))
  "Extract the common lisp backtrace frames.  Provide a gather-start-trigger function
that takes one argument (the backtrace-frame-function-name) and returns T it should trigger when to start
recording backtrace frames for Common Lisp.   Looking for the 'universal_error_handler' string
is one way to eliminate frames that aren't interesting to the user."
  (let ((frames (backtrace-with-arguments))
        result
        (state (if gather-start-trigger :skip-first-frames :gather))
        new-state)
    (dolist (frame frames)
      (let ((func-name (backtrace-frame-function-name frame)))
        ;; State machine
        (setq new-state (cond
                          ((and (eq state :skip-first-frames)
                                gather-start-trigger
                                (funcall gather-start-trigger frame))
                           :gather)
                          (t state)))
        (when verbose
          (bformat t "-----state -> %s   new-state -> %s\n" state new-state)
          (bformat t "     frame: %s\n" frame))
        (setq state new-state)
        (when (and (eq state :gather) (eq (backtrace-frame-type frame) :lisp))
          (when verbose (bformat t "     PUSHED!\n"))
          (push frame result))))
    (nreverse result)))
      
(defun btcl ()
  "Print backtrace of just common lisp frames"
  (let ((l (common-lisp-backtrace-frames)))
    (dolist (e l)
      (let ((name (backtrace-frame-print-name e))
            (arguments (backtrace-frame-arguments e)))
        (if arguments
            (progn
              (princ "(")
              (princ name)
              (dotimes (i (length arguments))
                (princ #\space)
                (prin1 (aref arguments i)))
              (princ ")"))
            (progn
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
