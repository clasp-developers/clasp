(in-package #:clasp-tests)

(test-true clip-failure
           (ext::source-location-p (first (ext:source-location '(setf find-class) :function))))

(test-true source-location-function
           (ext::source-location-p (first (ext:source-location 'find-class :function))))

(test-true source-location-class-symbol
           (ext::source-location-p (first (ext:source-location 'number :class))))

(test-true source-location-class-object
           (ext::source-location-p (first (ext:source-location (find-class 'number) t))))

(test-true source-location-macro
           (ext::source-location-p (first (ext:source-location 'defun :function))))

;;; TODO: Currently disabled as the actual special form compilation is in C++
;;; (the bytecode compiler)
;;; If we used Maclina instead we could source that. Or we could source the methods in
;;; b2b/compile-bytecode.lisp, but the mapping isn't perfectly clear (e.g. for LET).
#+(or)
(test-true source-location-special-from
           (ext::source-location-p (first (ext:source-location 'progn :function))))

(test-true source-location-generic-function
           (ext::source-location-p (first (ext:source-location #'initialize-instance t))))

(test-true source-location-type
           (ext::source-location-p (first (ext:source-location 'fixnum :type))))

(test-true source-location-variable
           (ext::source-location-p (first (ext:source-location 'ext:*source-location-kinds* :variable))))

(test-true
 run-program-hello-world
 (let* ((stream (ext:run-program "/bin/sh" (list "-c" "echo hello world")))
        (output (read-line stream)))
   (close stream)
   (string= output "hello world")))

(test source-tracking.1
      (with-input-from-string (s "hello")
        (ext:with-source-tracking (s :pathname "test.lisp"
                                     :lineno 7 :offset 13)
          (read-char s) (read-char s) ; move column
          (let ((source (ext:stream-source-location s)))
            (values (ext:source-location-pathname source)
                    (ext:source-location-lineno source)
                    (ext:source-location-column source)))))
      ;; line numbering starts on 1, apparently.
      (#p"test.lisp" 8 2))

(test source-tracking.2
      (let* ((fun (with-input-from-string (s "(lambda (x) x)")
                    (ext:with-source-tracking (s :pathname "lambda.lisp")
                      (multiple-value-call #'ext:compile-source
                        (ext:read-source s)))))
             (sources (ext:source-location fun t)))
        (if (find #p"lambda.lisp" sources
                  :test #'equal :key #'ext:source-location-pathname)
            t
            sources))
      (t))

(test source-tracking.3
      (with-input-from-string (s "(let ((x . 4)) x)")
        (ext:with-source-tracking (s :pathname "test.lisp"
                                     :lineno 7 :offset 13)
          (multiple-value-bind (form source)
              (ext:read-source s)
            (handler-case
                (ext:eval-source form source)
              (program-error (e)
                (let ((origin (cmp:compiler-condition-origin e)))
                  (if origin
                      (values (ext:source-location-pathname origin)
                              (ext:source-location-lineno origin)
                              (if (<= 0 (ext:source-location-column origin) 17) ; length of the string
                                  t
                                  (ext:source-location-column origin)))
                      :no-origin)))
              (condition (c)
                (values :wrong-condition c))
              (:no-error (&rest v)
                (declare (ignore v))
                :no-error)))))
      (#p"test.lisp" 8 t))

(test-true source-tracking.4
           (with-input-from-string (s "(+ x y)")
             (ext:with-source-tracking (s :pathname "augment.lisp")
               (multiple-value-bind (form source)
                   (ext:read-source s)
                 (let* ((expr `(lambda (x y) ,form))
                        (aug (ext:augment-source
                              expr source
                              (ext:stream-source-location s)))
                        (f (ext:compile-source expr aug)))
                   (ext:source-location f t))))))
