(in-package #:clasp-tests)

;;; Cannot really change a handler here, so just check read operations
(test get-signal-handler
      (let ((handler (ext:get-signal-handler :sigpipe)))
        (or handler t)))

(test-expect-error get-signal-handler-wrong-signal
                   (ext:get-signal-handler :sig-non-existant)
                   :type simple-error)
(test stat-size
      (ext:stat "sys:regression-tests;run-all.lisp"))

(test stat-size-2
      (let ((file "sys:regression-tests;run-all.lisp"))
        (= (ext:stat file)
           (ext:stat file :size))))

(test stat-size-mtime
      (ext:stat "sys:regression-tests;run-all.lisp" :mtime))

(test stat-size-mtime-no-logical-pathname
      (ext:stat (translate-logical-pathname "sys:regression-tests;run-all.lisp") :mtime))

(test stat-size-mode
      (ext:stat "sys:regression-tests;run-all.lisp" :mode))

(test fstat-size
      (let ((file "sys:regression-tests;run-all.lisp"))
        (with-open-file (stream file :direction :input)
          (let ((fd (ext:stream-file-descriptor stream)))
            (= (ext:stat file)
               (ext:stat file :size)
               (ext:fstat fd)
               (ext:fstat fd :size))))))

(test fstat-mtime
      (let ((file "sys:regression-tests;run-all.lisp"))
        (with-open-file (stream file :direction :input)
          (let ((fd (ext:stream-file-descriptor stream)))
            (= (ext:stat file :mtime)
               (ext:fstat fd :mtime))))))

(test fstat-mode
      (let ((file "sys:regression-tests;run-all.lisp"))
        (with-open-file (stream file :direction :input)
          (let ((fd (ext:stream-file-descriptor stream)))
            (= (ext:stat file :mode)
               (ext:fstat fd :mode))))))


