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
