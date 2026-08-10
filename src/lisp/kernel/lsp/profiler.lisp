(in-package #:ext)

(defun flame-profile-annotation (start-universal-time elapsed-seconds)
  "One-line summary of when a flame profile started and how much wall-clock
time it covered.  Used both as the SVG subtitle (rendered under the title)
and as the NOTES comment in the SVG file header."
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time start-universal-time)
    (format nil "started ~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d | elapsed ~,2f s"
            year month date hour min sec elapsed-seconds)))

(defmacro with-flame-profile ((&key (path (format nil "~~/public_html/flame-~d.svg"
                                                  (core:getpid)))
                                 (rate 97) (title "")
                                 (buffer-bytes 0)) &body body)
  "Profile BODY with the sampling profiler and write a flame graph SVG to PATH.

Example:
  (ext:with-flame-profile (:path \"/tmp/my-profile.svg\" :rate 197)
    (my-expensive-computation))

RATE is the sampling frequency in Hz (default 97, a prime to avoid
aliasing with periodic work). TITLE is an optional string for the SVG header.

The SVG records when the profile started and how many seconds of wall-clock
time it covered — as a subtitle under the title, and as a NOTES comment in
the file header.  The measured window is profile-start to profile-stop, so
it excludes symbolication and SVG rendering.

Returns the values of BODY. Signals an error if the profiler is already
running. The profiler is guaranteed to be stopped and reset on any exit
(normal return, throw, or condition)."
  (let ((path-var (gensym "PATH"))
        (rate-var (gensym "RATE"))
        (buffer-bytes-var (gensym "BUFFER-BYTES"))
        (title-var (gensym "TITLE"))
        (start-ut-var (gensym "START-UT"))
        (start-real-var (gensym "START-REAL"))
        (annotation-var (gensym "ANNOTATION"))
        (vals-var (gensym "VALS")))
    `(let ((,path-var ,path)
           (,rate-var ,rate)
           (,buffer-bytes-var ,buffer-bytes)
           (,title-var ,title))
       (when (ext:profile-running-p)
         (error "Sampling profiler is already running"))
       ;; Stamp the wall clock immediately before the profiler starts, so the
       ;; recorded window matches what the samples actually cover.
       (let ((,start-ut-var (get-universal-time))
             (,start-real-var (get-internal-real-time))
             (,annotation-var "")
             ,vals-var)
         (unless (ext:profile-start :rate ,rate-var :buffer-bytes ,buffer-bytes-var)
           (error "Failed to start sampling profiler"))
         (unwind-protect
              (setf ,vals-var (multiple-value-list (progn ,@body)))
           (ext:profile-stop)
           ;; Close the window here, before the (potentially slow)
           ;; symbolication and rendering below.
           (setf ,annotation-var
                 (flame-profile-annotation
                  ,start-ut-var
                  (/ (float (- (get-internal-real-time) ,start-real-var) 1d0)
                     internal-time-units-per-second)))
           (let ((used     (ext:profile-bytes-used))
                 (avail    (ext:profile-bytes-available))
                 (recorded (ext:profile-samples-recorded))
                 (dropped  (ext:profile-samples-dropped)))
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
                                                     ,title-var)
                                          :subtitle ,annotation-var
                                          :notes ,annotation-var))))
             (ext:profile-reset)
             (format t "Profiling buffer: ~:d / ~:d bytes used (~,1f%), ~:d samples~@[, ~:d   DROPPED (buffer full)~]~%"
                     used avail
                     (if (plusp avail) (/ (* 100.0 used) avail) 0.0)
                     recorded
                     (when (plusp dropped) dropped)))
           )
         (format t "Wrote flame graph to ~s~%" ,path-var)
         (values-list ,vals-var)))))