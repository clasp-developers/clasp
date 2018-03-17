(in-package :ext)
;;; Written by Shinmera 2015
;;; adapted to clasp by Christian Schafmeister 2015

(defun llvm-short-version ()
  (ext:llvm-version))

;; don't include this if you already have a splitting function (you probably do)
#+(or)
(defun split (string split-char)
  (loop with out = (make-string-output-stream)
     with parts = ()
     for char across string
     do (if (char= char split-char)
            (let ((part (get-output-stream-string out)))
              (when (string/= "" part)
                (push part parts)))
            (write-char char out))
     finally (return (nreverse parts))))

(defun ensure-directory-namestring (namestring)
  (if (char= (char namestring (1- (length namestring)))
             #+windows #\\ #-windows #\/ )
      namestring
      (format NIL "~a/" namestring)))

(defun external-path-paths (&optional (var (ext:getenv "PATH")))
  (mapcar
   (lambda (namestring)
     (pathname (ensure-directory-namestring namestring)))
   (core:split var ":")))

(defvar *clang-names* (list
                       (core:bformat NIL "clang-%s" (llvm-short-version))
                       (core:bformat NIL "clang++-%s" (llvm-short-version))
                       "clang" "clang++"))

(defun discover-clang (&key debug (search-paths (external-path-paths)) (names *clang-names*))
  (or (ext:getenv "CLASP_CLANG_PATH")
      (and ext:*clasp-clang-path* (stringp ext:*clasp-clang-path*) (probe-file (pathname ext:*clasp-clang-path*)))
      (dolist (path search-paths)
        (dolist (n names)
          (let ((cp (make-pathname :name n :type nil :defaults path)))
            (when debug
              (format t "Searching for clang at ~a~%" cp))
            (let ((file (probe-file cp)))
              (when file (return-from discover-clang file))))))))

(defparameter core:*clang-bin* (discover-clang))

 ;; This would only work after kernel/clos/conditions
#+(or)
(define-condition clang-not-found-error (error)
  ((tried-path :initarg :path :initform NIL :accessor tried-path))
  (:report (lambda (c s) (format s "Could not find clang~@[ on path ~s~]."
                                 (tried-path c)))))

(defun run-clang (args &key (clang core:*clang-bin*) output-file-name)
  "Run the discovered clang compiler on the arguments. This replaces a simpler version of run-clang."
  (unless (probe-file clang)
    (error "Could not find clang at ~a" clang))
  ;; This would only work after kernel/clos/conditions
  #+(or)
  (labels ((read-path ()
             (list (pathname (read *query-io*))))
           (clang-usable-p ()
             (restart-case
                 (if (and clang (probe-file clang))
                     T
                     (error 'clang-not-found-error :path clang))
               (use-value (value)
                 :interactive read-path
                 :report "Use an alternative path for clang."
                 :test (lambda (c) (typep c 'clang-not-found-error))
                 (setf clang value)
                 NIL)
               (store-value (value)
                 :interactive read-path
                 :report "Store a new path for *CLANG-BIN*."
                 :test (lambda (c) (typep c 'clang-not-found-error))
                 (setf *clang-bin* value clang value)
                 NIL))))
    (loop until (clang-usable-p)))
  (when (member :debug-run-clang *features*)
    (let ((cmd (with-output-to-string (sout)
                 (core:bformat sout "%s" (namestring clang))
                 (dolist (arg args)
                   (core:bformat sout " %s" arg)))))
      (core:bformat t "run-clang:  %s\n" cmd)))
  (let ((start-time (get-internal-run-time)))
    (cmp::safe-system (list* (namestring clang) args) :output-file-name output-file-name)
    (setf llvm-sys:*accumulated-clang-link-time* (+ llvm-sys:*accumulated-clang-link-time* (* (/ 1.0 internal-time-units-per-second) (- (get-internal-run-time) start-time))))
    (incf llvm-sys:*number-of-clang-links*)))

(export 'run-clang)
