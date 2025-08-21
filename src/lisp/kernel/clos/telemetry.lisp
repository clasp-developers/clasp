(in-package #:clos)

(defun %start-profiling/record (gf)
  (check-type gf standard-generic-function)
  (setf (generic-function-tracy gf) (list :profile-record 0.0)))

(defun %start-profiling/ongoing (gf)
  (check-type gf standard-generic-function)
  (setf (generic-function-tracy gf) (list :profile-ongoing 0.0)))

(defun start-profiling (generic-function &key (report :ongoing))
  "Start profiling GENERIC-FUNCTION.
If REPORT is :ONGOING (default), events will be printed to *TRACE-OUTPUT* as they occur. Otherwise, they will just be silently recorded.

Experimental."
  (check-type report (member :ongoing nil))
  (if (eq report :ongoing)
      (%start-profiling/ongoing generic-function)
      (%start-profiling/record generic-function)))

(defun stop-profiling (generic-function)
  "Stop profiling GENERIC-FUNCTION. Profiling data is erased.

See REPORT-PROFILING
See PROFILING-DATA

Experimental."
  (setf (generic-function-tracy generic-function) nil))

(defun report-profiling (generic-function)
  "Print a representation of the current profile of GENERIC-FUNCTION to *TRACE-OUTPUT*.

Experimental."
  (let ((tracy (generic-function-tracy generic-function)))
    (when (null tracy)
      (warn "~a is not being profiled" generic-function)
      (return-from report-profiling))
    (destructuring-bind (time &rest calls) (cdr tracy)
      (format *trace-output*
              "~&;  ~s Misses: ~d Overhead: ~fs~%"
              (generic-function-name generic-function)
              (length calls) time))))

(defun profiling-data (generic-function)
  "Return current profiling information for GENERIC-FUNCTION. Currently this consists of two values: the number of dispatch misses that have occured, and approximately how much overhead was incurred by these misses in seconds.

Experimental."
  (let ((tracy (generic-function-tracy generic-function)))
    (when (null tracy)
      (warn "~a is not being profiled" generic-function)
      (return-from profiling-data (values 0 0.0)))
    (destructuring-bind (time &rest calls) (cdr tracy)
      (values (length calls) time))))

(defmacro with-profiling ((&rest fnames) (&key (report :after))
                          &body body)
  "Start CLOS profiling of some generic functions.
REPORT may be:
 :ONGOING - print dispatch miss info as it happens (this increases overhead!), and also a summary report
 :AFTER - just print a summary report when the WITH-PROFILING exits
 NIL - print nothing. Useful in connection with PROFILING-DATA.

Experimental."
  (check-type report (member :ongoing :after nil))
  `(unwind-protect
        (progn ,@(loop with start = (if (eq report :ongoing)
                                        '%start-profiling/ongoing
                                        '%start-profiling/record)
                       for fname in fnames
                       collecting `(,start #',fname))
               ,@body)
     ,@(when report
         `((format *trace-output* "~&; CLOS profiling report:~%")
           ,@(loop for fname in fnames
                   collecting `(report-profiling #',fname))
           (format *trace-output* "~&; end of CLOS profiling report~%")))
     ,@(loop for fname in fnames collecting `(stop-profiling #',fname))))
