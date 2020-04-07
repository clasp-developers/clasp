;;;
;;; Christian Schafmeister Sept 2017
;;;
;;; Generate backtraces using a combination of libc backtrace, backtrace_symbols, jit symbols and the shadow stack.
;;; It pulls as much information out of these sources as it can and correlates them to produce as rich a backtrace as possible.
;;; If you turn on (declare (debug 3)) in a function then call arguments will be saved at run-time and made available through the stack.
;;; Jitted symbols are correlated to return addresses from 'backtrace' and attached to call frames.
;;; 

(in-package :core)

;;;
;;; backtrace-frame struct is now defined in C++ in debugger.h and debugger.cc
;;;


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
(defun backtrace-frame-fix-names (frame)
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
                             ((backtrace-frame-closure frame)
                              (function-name
                               (backtrace-frame-closure frame)))
                             ((string= maybe-method "METHOD")
                              (format nil "(METHOD ~a ~a)" name maybe-fn-or-specializers))
                             (t name))))
          (setf (backtrace-frame-function-name frame) name
                (backtrace-frame-print-name frame) print-name))
        (setf (backtrace-frame-function-name frame) common-lisp-name
              (backtrace-frame-print-name frame) common-lisp-name))))

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
                                                 (gather-start-trigger
                                                  (lambda (frame)
                                                    (and (eq :lisp (backtrace-frame-type frame))
                                                         (eq 'cl:error (backtrace-frame-function-name frame)))))
                                       gather-all-frames)
  "Extract the common lisp backtrace frames.  Provide a gather-start-trigger function
that takes one argument (the backtrace-frame-function-name) and returns T it should trigger when to start
recording backtrace frames for Common Lisp.   Looking for the 'UNIVERSAL-ERROR-HANDLER' string
is one way to eliminate frames that aren't interesting to the user.
Set gather-all-frames to T and you can gather C++ and Common Lisp frames"
  (let ((frames backtrace))
    (cond
      ((gather-frames frames gather-start-trigger :verbose verbose)) ; Start gathering on a specific frame
      ((gather-frames frames (lambda (frame) (eq :lisp (backtrace-frame-type frame))) :verbose verbose)) ; return all :LISP frames
      (t frames)))) ; return ALL frames

(defun bt-argument (frame-index argument-index)
  "Get argument argument-index (or all if argument-index is not a number) from frame frame-index in current backtrace"
  (core:call-with-backtrace
   #'(lambda (raw-backtrace)
       (let ((frames (common-lisp-backtrace-frames raw-backtrace))
             (index 0))
         (dolist (frame frames)
           (when (= index frame-index)
             (let ((arguments-vector (backtrace-frame-arguments frame)))
               (return-from bt-argument
                 (if (numberp argument-index)
                     (if (> (length arguments-vector) argument-index)
                         (aref arguments-vector argument-index)
                         :invalid-argument-index)
                     arguments-vector))))
           (incf index)))
       (return-from bt-argument :invalid-frame-index))))

(export '(common-lisp-backtrace-frames
          backtrace-frame-function-name backtrace-frame-arguments bt-argument))

(defmacro with-dtrace-trigger (&body body)
  `(unwind-protect
        (progn
          (core:trigger-dtrace-start
           (lambda ()
             ,@body)))
     (core:trigger-dtrace-stop)))

(defun ext:information-interrupt (&rest args)
  (core:safe-backtrace))

(in-package :ext)

(defstruct (code-source-line (:type vector) :named)
  source-pathname line-number column)

(export '(code-source-line code-source-line-source-pathname code-source-line-line-number))

(defun parse-llvm-dwarfdump (stream)
  (let (info)
    (tagbody
     top
       (let ((line (read-line stream nil :eof)))
         (when (eq line :eof) (go done))
         (cond
           ((search "DW_AT_comp_dir" line)
            (let* ((open-paren-pos (position #\( line))
                   (close-paren-pos (position #\) line :from-end t))
                   (data (read-from-string line nil :eof :start (1+ open-paren-pos) :end close-paren-pos)))
              (push (cons :comp-dir data) info)))
           ((search "DW_AT_name" line)
            (let* ((open-paren-pos (position #\( line))
                   (close-paren-pos (position #\) line :from-end t))
                   (data (read-from-string line nil :eof :start (1+ open-paren-pos) :end close-paren-pos)))
              (push (cons :name data) info)))
           ((search "DW_TAG_inlined_subroutine" line)
            (push (cons :inlined-subroutine t) info))
           ((search "DW_AT_call_file" line)
            (let* ((open-paren-pos (position #\( line))
                   (close-paren-pos (position #\) line :from-end t))
                   (call-file (read-from-string line nil :eof :start (1+ open-paren-pos) :end close-paren-pos)))
              (push (cons :call-file call-file) info)))
           ((search "DW_AT_call_line" line)
            (let* ((open-paren-pos (position #\( line))
                   (close-paren-pos (position #\) line :from-end t))
                   (call-line (parse-integer line :start (1+ open-paren-pos) :end close-paren-pos)))
              (push (cons :call-line call-line) info)))
           ((search "Line info:" line)
            ;; Parse lines like: "Line info: file '-unknown-file-', line 231, column 12, start line 226"
            (let* ((line-start (search ", line " line))
                   (line-pos (+ line-start (length ", line ")))
                   (column-start (search ", column " line))
                   (column-pos (+ column-start (length ", column ")))
                   (start-line-start (search ", start line" line))
                   (line-no (parse-integer line :start line-pos :end column-start))
                   (column (parse-integer line :start column-pos :end start-line-start)))
              (push (cons :line-info-line line-no) info)
              (go done)))))
       (go top)
     done)
    (close stream)
    (let ((info (reverse info)))
      (cond
        ((assoc :inlined-subroutine info)
         (let ((file-name (cdr (assoc :call-file info)))
               (line-no (cdr (assoc :call-line info)))
               (column 0))
           (values file-name line-no column)))
        (t
         (let ((file-name (concatenate 'string (cdr (assoc :comp-dir info)) "/" (cdr (assoc :name info))))
               (line-no (cdr (assoc :line-info-line info)))
               (column (cdr (assoc :line-info-column info))))
           (values file-name line-no column)))))))

(defun run-llvm-dwarfdump (address file)
  (let ((pathname #+target-os-darwin (pathname (format nil "~a.dwarf" file))
                  #+target-os-linux (pathname file)))
    (if (null (probe-file pathname))
        (values)
        (let ((llvm-dwarfdump (namestring (make-pathname :name "llvm-dwarfdump" :type nil :defaults core:*clang-bin*))))
          (let ((cmd (list llvm-dwarfdump
                           "-lookup"
                           (format nil "0x~x" address)
                           (namestring pathname))))
            (multiple-value-bind (err error-msg stream)
                (ext:vfork-execvp cmd t)
              (parse-llvm-dwarfdump stream)))))))

(defun object-file-address-information (addr &key verbose)
  (multiple-value-bind (sectioned-address object-file)
      (llvm-sys:object-file-for-instruction-pointer addr verbose)
    (unless (null sectioned-address)
      (llvm-sys:get-line-info-for-address
       (llvm-sys:create-dwarf-context object-file)
       sectioned-address))))

(defparameter *debug-code-source-position* nil)
(defun code-source-position (return-address &key verbose)
  (let ((address (core:pointer-increment return-address -1)))
    ;; First, if it's an object file try that.
    (multiple-value-bind (source-path function-name source line column start-line discriminator)
        (object-file-address-information address :verbose verbose)
      (declare (ignore function-name source start-line discriminator))
      (when source-path ; success, it was in an object file
        (return-from code-source-position
          (make-code-source-line
           :source-pathname source-path
           :line-number line :column column))))
    ;; OK no good, so try it as an address from a library.
    (multiple-value-bind (symbol start end type library-name library-origin offset-from-start-of-library)
        (core:lookup-address address)
      (multiple-value-bind (source-pathname line-number column)
          (run-llvm-dwarfdump offset-from-start-of-library library-name)
        (make-code-source-line
         :source-pathname source-pathname
         :line-number line-number
         :column column)))))

(export 'code-source-position)
