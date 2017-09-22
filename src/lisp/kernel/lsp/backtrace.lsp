;;
;; Christian Schafmeister Sept 2017
;;
;; Generate backtraces using a combination of 

(in-package :core)


(defstruct (backtrace-frame (:type vector) :named)
  common-lisp-frame
  raw-name
  function-name
  function-start-address
  function-length-bytes
  base-pointer
  next-base-pointer)


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
    ((string= backtrace-string "0x0")
     (multiple-value-bind (jit-name function-start-address function-bytes)
         (locate-jit-symbol-info return-address)
       ;; strip leading #\_
       (let* ((parts (unescape-and-split-jit-name (subseq jit-name 1 (length jit-name))))
              (symbol-name (first parts))
              (package-name (second parts))
              (name (intern symbol-name package-name)))
         (make-backtrace-frame :common-lisp-frame t
                               :raw-name jit-name
                               :function-name name
                               :function-start-address function-start-address
                               :function-length-bytes function-bytes
                               :base-pointer base-pointer
                               :next-base-pointer next-base-pointer))))
    ((let ((pos (search "^^" backtrace-string :from-end t)))
       (when pos
         (let* ((parts (unescape-and-split-jit-name (subseq backtrace-string 0 pos)))
                (symbol-name (first parts))
                (package-name (second parts))
                (name (intern symbol-name package-name)))
           (make-backtrace-frame :common-lisp-frame t
                                 :raw-name backtrace-string
                                 :function-name name
                                 :base-pointer base-pointer
                                 :next-base-pointer next-base-pointer)))))
    (t (let ((unmangled (core::maybe-demangle backtrace-string)))
         (make-backtrace-frame :common-lisp-frame nil
                               :raw-name backtrace-string
                               :function-name (if unmangled unmangled backtrace-string)
                               :base-pointer base-pointer
                               :next-base-pointer next-base-pointer)))))

(defun backtrace-as-list ()
  (let ((clib-backtrace (core:clib-backtrace-as-list))
        (shadow-backtrace (core:shadow-backtrace-as-list))
        result prev-entry)
    (do* ((cur clib-backtrace (cdr cur))
          (clib-frame (car cur) (car cur))
          (next-clib-frame (cadr cur) (cadr cur)))
         ((null next-clib-frame) (nreverse result))
      (let* ((address (first clib-frame))
             (line (second clib-frame))
             (base-pointer (third clib-frame))
             (next-base-pointer (third next-clib-frame))
             (split (core:split line " "))
             (name (string (fourth split)))
             (entry (parse-frame address name base-pointer next-base-pointer)))
        (push entry result)))))
        
(defun btcl ()
  (let ((l (backtrace-as-list)))
    (dolist (e l)
      (let ((name (backtrace-frame-function-name e)))
        (if (and (backtrace-frame-common-lisp-frame e) (not (eq name 'lambda)))
            (bformat t "%s\n" name))))))

(defun bt ()
  (let ((l (backtrace-as-list)))
    (dolist (e l)
          (bformat t "%s\n" (backtrace-frame-function-name e)))))
