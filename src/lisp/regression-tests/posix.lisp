(in-package #:clasp-tests)

;;; Cannot really change a handler here, so just check read operations
(test get-signal-handler
      (let ((handler (ext:get-signal-handler :sigpipe)))
        (or handler t)))

(test-expect-error get-signal-handler-wrong-signal
                   (ext:get-signal-handler :sig-non-existant)
                   :type simple-error)
(test stat-all
      (ext:stat "sys:regression-tests;run-all.lisp"))

(test stat-size
      (let ((file "sys:regression-tests;run-all.lisp"))
        (numberp (nth-value 0 (ext:stat file)))))

(test stat-mtime
      (let ((file "sys:regression-tests;run-all.lisp"))
        (numberp (nth-value 1 (ext:stat file)))))

(test stat-size-mtime-no-logical-pathname
      (let ((file-no-lp (translate-logical-pathname "sys:regression-tests;run-all.lisp"))
            (file-lp "sys:regression-tests;run-all.lisp"))
        (= (nth-value 1 (ext:stat file-no-lp))(nth-value 1 (ext:stat file-lp)))))

(test stat-size-mode
      (nth-value 2 (ext:stat "sys:regression-tests;run-all.lisp")))

(test fstat-all
      (let ((file "sys:regression-tests;run-all.lisp"))
        (with-open-file (stream file :direction :input)
          (let ((fd (ext:file-stream-file-descriptor stream)))
            (ext:fstat fd)))))
 
(test fstat-size
      (let ((file "sys:regression-tests;run-all.lisp"))
        (with-open-file (stream file :direction :input)
          (let ((fd (ext:file-stream-file-descriptor stream)))
            (= (nth-value 0 (ext:stat file))
               (nth-value 0 (ext:fstat fd)))))))

(test fstat-mtime
      (let ((file "sys:regression-tests;run-all.lisp"))
        (with-open-file (stream file :direction :input)
          (let ((fd (ext:file-stream-file-descriptor stream)))
            (= (nth-value 1 (ext:stat file))
               (nth-value 1 (ext:fstat fd)))))))

(test fstat-mode
      (let ((file "sys:regression-tests;run-all.lisp"))
        (with-open-file (stream file :direction :input)
          (let ((fd (ext:file-stream-file-descriptor stream)))
            (= (nth-value 2 (ext:stat file))
               (nth-value 2 (ext:fstat fd)))))))

(test-expect-error file-stream-file-descriptor-wrong-type
                   (ext:file-stream-file-descriptor 23)
                   :type type-error)


