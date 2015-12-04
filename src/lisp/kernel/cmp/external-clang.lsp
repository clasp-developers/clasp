(in-package :ext)
;;; Written by Shinmera 2015
;;; adapted to clasp by Christian Schafmeister 2015

(defun llvm-short-version ()
  (subseq (ext:llvm-version) 0 (position #\. (ext:llvm-version) :from-end t)))

;; don't include this if you already have a splitting function (you probably do)
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
   (split var #\:)))

(defvar *clang-names* (list
                       (format NIL "clang-~a" (llvm-short-version))
                       (format NIL "clang++-~a" (llvm-short-version))
                       "clang" "clang++"))

(defun discover-clang (&key debug (search-paths (external-path-paths)) (names *clang-names*))
  (or (ext:getenv "CLASP_CLANG_PATH")
      (dolist (path search-paths)
        (dolist (n names)
          (let ((cp (make-pathname :name n :type nil :defaults path)))
            (when debug
              (format t "Searching for clang at ~a~%" cp))
            (let ((file (probe-file cp)))
              (when file (return-from discover-clang file))))))))

(defparameter *clang-bin* (discover-clang))

(define-condition clang-not-found-error (error)
  ((tried-path :initarg :path :initform NIL :accessor tried-path))
  (:report (lambda (c s) (format s "Could not find clang~@[ on path ~s~]."
                                 (tried-path c)))))

(defun run-clang (args &key (clang core:*clang-bin*))
  "Run the discovered clang compiler on the arguments. This replaces a simpler version of run-clang."
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
  (cmp:safe-system (list* (namestring clang) args)))
