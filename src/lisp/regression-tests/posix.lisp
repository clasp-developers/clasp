(in-package #:clasp-tests)

(test-true stat-all
      (ext:stat "sys:src;lisp;regression-tests;run-all.lisp"))

(test-type stat-size
    (let ((file "sys:src;lisp;regression-tests;run-all.lisp"))
      (ext:stat file))
    number)

(test-type stat-mtime
    (let ((file "sys:src;lisp;regression-tests;run-all.lisp"))
      (nth-value 1 (ext:stat file)))
    number)

(test-true stat-size-mtime-no-logical-pathname
      (let ((file-no-lp (translate-logical-pathname "sys:src;lisp;regression-tests;run-all.lisp"))
            (file-lp "sys:src;lisp;regression-tests;run-all.lisp"))
        (= (nth-value 1 (ext:stat file-no-lp))(nth-value 1 (ext:stat file-lp)))))

(test-true stat-size-mode
      (nth-value 2 (ext:stat "sys:src;lisp;regression-tests;run-all.lisp")))

(test-true fstat-all
      (let ((file "sys:src;lisp;regression-tests;run-all.lisp"))
        (with-open-file (stream file :direction :input)
          (let ((fd (ext:file-stream-file-descriptor stream)))
            (ext:fstat fd)))))
 
(test-true fstat-size
      (let ((file "sys:src;lisp;regression-tests;run-all.lisp"))
        (with-open-file (stream file :direction :input)
          (let ((fd (ext:file-stream-file-descriptor stream)))
            (= (nth-value 0 (ext:stat file))
               (nth-value 0 (ext:fstat fd)))))))

(test-true fstat-mtime
      (let ((file "sys:src;lisp;regression-tests;run-all.lisp"))
        (with-open-file (stream file :direction :input)
          (let ((fd (ext:file-stream-file-descriptor stream)))
            (= (nth-value 1 (ext:stat file))
               (nth-value 1 (ext:fstat fd)))))))

(test-true fstat-mode
      (let ((file "sys:src;lisp;regression-tests;run-all.lisp"))
        (with-open-file (stream file :direction :input)
          (let ((fd (ext:file-stream-file-descriptor stream)))
            (= (nth-value 2 (ext:stat file))
               (nth-value 2 (ext:fstat fd)))))))

(test-expect-error file-stream-file-descriptor-wrong-type
                   (ext:file-stream-file-descriptor 23)
                   :type type-error)

(test-true
 FileStream_O__repr__
 (multiple-value-bind
       (errno pid-or-error-message stream)
     (ext:vfork-execvp (list "llvm-config" "--ldflags" "--libdir" "--libs") t)
   (declare (ignore errno pid-or-error-message))
   (if stream
       (write stream)
       nil)))

