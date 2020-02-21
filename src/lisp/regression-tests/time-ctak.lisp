(defun ctak-aux (x y z)
  (declare (fixnum x y z))
  (cond ((not (< y x))
         (throw 'ctak z))
        (t (ctak-aux
             (catch 'ctak
               (ctak-aux (the fixnum (1- x)) y z))
             (catch 'ctak
               (ctak-aux (the fixnum (1- y)) z x))
             (catch 'ctak
               (ctak-aux (the fixnum (1- z)) x y))))))

(defun ctak (x y z)
  (declare (fixnum x y z))
  (catch 'ctak (ctak-aux x y z)))

(defun tak (x y z)
  (if (< y x)
      (tak (tak (1- x) y z)
           (tak (1- y) z x)
           (tak (1- z) x y))
      z))

(defun run-ctak ()
  (dotimes (i 2) (ctak 22 12 8)))

(defun run-tak ()
  (dotimes (i 10000) (tak 22 12 8)))

(defun time-ctak (n)
  (loop repeat n
        collect (mp:process-run-function 'test #'run-ctak) into threads
        finally (time (mapc #'mp:process-join threads))))

(defun time-tak (n)
  (loop repeat n
        collect (mp:process-run-function 'test #'run-tak) into threads
        finally (time (mapc #'mp:process-join threads))))

(defun time-ctak-c++ (nthreads &optional allocate (num 0))
  (loop repeat nthreads
        collect (mp:process-run-function 'test (lambda () (core:ctak 22 12 8 :times num :allocate allocate))) into threads
        finally (time (mapc #'mp:process-join threads))))

(defun time-tak-c++ (nthreads &optional allocate (num 0))
  (loop repeat nthreads
        collect (mp:process-run-function 'test (lambda () (core:tak 22 12 8 :times num :allocate allocate))) into threads
        finally (time (mapc #'mp:process-join threads))))

(defun run-all ()
  (dotimes (i 8)
    (format t "Running tak with ~a threads~%" (1+ i))
    (time-tak (1+ i)))
  (dotimes (i 8)
    (format t "Running ctak with ~a threads~%" (1+ i))
    (time-ctak (1+ i))))

(defun run-all-c++ ()
  (dotimes (i 8)
    (format t "Running tak with ~a threads~%" (1+ i))
    (time-tak-c++ (1+ i) nil 10000))
  (dotimes (i 8)
    (format t "Running ctak with ~a threads~%" (1+ i))
    (time-ctak-c++ (1+ i) nil 10))
  (dotimes (i 8)
    (format t "Running tak with ~a threads and allocation~%" (1+ i))
    (time-tak-c++ (1+ i) t 100))
  (dotimes (i 8)
    (format t "Running ctak with ~a threads and allocation~%" (1+ i))
    (time-ctak-c++ (1+ i) t 10)))

