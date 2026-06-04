(in-package #:ext)

(defmacro with-flame-profile ((path &key (rate 97) (title "")) &body body)
  "Profile BODY with the sampling profiler and write a flame graph SVG to PATH.

Example:
  (ext:with-flame-profile (\"/tmp/my-profile.svg\" :rate 197)
    (my-expensive-computation))

RATE is the sampling frequency in Hz (default 97, a prime to avoid
aliasing with periodic work). TITLE is an optional string for the SVG header.

Returns the values of BODY. Signals an error if the profiler is already
running. The profiler is guaranteed to be stopped and reset on any exit
(normal return, throw, or condition)."
  (let ((path-var (gensym "PATH"))
        (rate-var (gensym "RATE"))
        (title-var (gensym "TITLE"))
        (vals-var (gensym "VALS")))
    `(let ((,path-var ,path)
           (,rate-var ,rate)
           (,title-var ,title))
       (when (ext:profile-running-p)
         (error "Sampling profiler is already running"))
       (unless (ext:profile-start :rate ,rate-var)
         (error "Failed to start sampling profiler"))
       (let (,vals-var)
         (unwind-protect
              (setf ,vals-var (multiple-value-list (progn ,@body)))
           (ext:profile-stop)
           (let ((samples (ext:profile-symbolicated-samples)))
             (when samples
               (with-open-file (out ,path-var
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
                 (flamegraph:flamegraph :data samples
                                        :output out
                                        :title (if (string= ,title-var "")
                                                   (format nil "clasp ~A" (core:getpid))
                                                   ,title-var)))))
           (ext:profile-reset))
         (values-list ,vals-var)))))
