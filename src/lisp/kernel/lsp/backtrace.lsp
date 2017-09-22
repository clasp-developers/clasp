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
  function-start-address
  function-length-bytes
  base-pointer
  next-base-pointer
  arguments
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




(defun parse-frame (return-address backtrace-string base-pointer next-base-pointer)
  ;; Get the name
  (cond
    ;; If there is no backtrace_symbol info - it's probably jitted
    ((string= (subseq backtrace-string 0 (min 3 (length backtrace-string))) "0x0")
     (multiple-value-bind (jit-name function-start-address function-bytes)
         (locate-jit-symbol-info return-address)
       ;; strip leading #\_
       (if (and jit-name (search "^^" jit-name))
           (let* ((parts (cmp:unescape-and-split-jit-name (subseq jit-name 1 (length jit-name))))
                  (symbol-name (first parts))
                  (package-name (second parts))
                  (name (intern symbol-name (or package-name :keyword))))
             (make-backtrace-frame :type :lisp
                                   :return-address return-address
                                   :raw-name jit-name
                                   :function-name name
                                   :function-start-address function-start-address
                                   :function-length-bytes function-bytes
                                   :base-pointer base-pointer
                                   :next-base-pointer next-base-pointer))
           (make-backtrace-frame :type :unknown
                                 :return-address return-address
                                 :function-name (or jit-name (core:bformat nil "%s" return-address))
                                 :raw-name backtrace-string
                                 :base-pointer base-pointer
                                 :next-base-pointer next-base-pointer))))
    ((let ((pos (search "^^" backtrace-string :from-end t)))
       (when pos
         (let* ((parts (cmp:unescape-and-split-jit-name (subseq backtrace-string 0 pos)))
                (symbol-name (first parts))
                (package-name (second parts))
                (name (intern symbol-name (or package-name :keyword))))
           (make-backtrace-frame :type :lisp
                                 :return-address return-address
                                 :raw-name backtrace-string
                                 :function-name name
                                 :base-pointer base-pointer
                                 :next-base-pointer next-base-pointer)))))
    (t (let ((unmangled (core::maybe-demangle backtrace-string)))
         (make-backtrace-frame :type :c
                               :return-address return-address
                               :raw-name backtrace-string
                               :function-name (if unmangled unmangled backtrace-string)
                               :base-pointer base-pointer
                               :next-base-pointer next-base-pointer)))))


(defun search-for-matching-frame (frames entry)
  (let ((saved-frames frames)
        (frame-address (frame-iterator-frame-address entry)))
    (do* ((cur frames (cdr cur))
          (frame (car cur) (car cur)))
         ((null cur) (or cur saved-frames))
      (let ((bp (backtrace-frame-base-pointer frame))
            (next-bp (backtrace-frame-next-base-pointer frame)))
        (when (and bp next-bp
                   (pointer-in-pointer-range frame-address bp next-bp))
          (return-from search-for-matching-frame cur))))))

(defun add-call-arguments (frames shadow-backtrace)
  (dolist (shadow-entry shadow-backtrace)
    (let ((frame-cur (search-for-matching-frame frames shadow-entry)))
      (if frame-cur
          (let ((frame (car frame-cur)))
            (setf (backtrace-frame-arguments frame) (frame-iterator-arguments shadow-entry))
            (setf frames frame-cur))
          (bformat t "Could not find stack frame for address: %s\n" (frame-iterator-frame-address shadow-entry))))))

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

(defun backtrace-as-list ()
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
                 (entry (parse-frame address name prev-base-pointer base-pointer)))
            (push entry result)))
        (setf prev-base-pointer base-pointer)))
    (nreverse result)))

(defun backtrace-with-arguments ()
  (let ((ordered (backtrace-as-list))
        (shadow-backtrace (core:shadow-backtrace-as-list)))
    (add-call-arguments ordered shadow-backtrace)
    ordered))

(defun common-lisp-backtrace-frames (&key verbose (focus t))
  "Extract the common lisp backtrace frames"
  (let ((frames (backtrace-with-arguments))
        result
        (state :skip-first-frames)
        newstate)
    (dolist (frame frames)
      (let ((func-name (backtrace-frame-function-name frame)))
        ;; State machine
        (setq new-state (cond
                          ((and (eq state :skip-first-frames)
                                (search "universal_error_handler" (string (backtrace-frame-function-name frame))))
                           :gather)
                          ((and (eq state :skip-first-frames)
                                (eq (backtrace-frame-function-name frame) 'core:universal-error-handler))
                           :skip-universal-error-handler)
                          ((eq state :skip-universal-error-handler)
                           :gather)
                          ((and focus
                                (eq state :gather)
                                (or (eq func-name 'clos::combine-method-functions3.lambda)
                                    (eq func-name 'cmp::from-bclasp-implicit-compile-repl-form)))
                           (return))
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
  (let ((l (common-lisp-backtrace-frames)))
    (dolist (e l)
      (let ((name (backtrace-frame-function-name e))
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

(export '(btcl bt common-lisp-backtrace-frames))
