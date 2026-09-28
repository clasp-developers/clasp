(in-package #:ext)

(defun flame-profile-annotation (start-universal-time elapsed-seconds)
  "One-line summary of when a flame profile started and how much wall-clock
time it covered.  Used both as the SVG subtitle (rendered under the title)
and as the NOTES comment in the SVG file header."
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time start-universal-time)
    (format nil "started ~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d | elapsed ~,2f s"
            year month date hour min sec elapsed-seconds)))

(defun profile-executable-range-annotation ()
  "Summarize the shared dynamic executable-range cache since its last rebuild."
  (multiple-value-bind (filled capacity rejected)
      (ext:profile-executable-range-stats)
    (format nil "dynamic executable ranges ~:d/~:d; rejected registrations ~:d"
            filled capacity rejected)))

(defun write-profile-walk-stop-report (svg-path &key allocation (write-report t))
  "Summarize walk stops and optionally save detailed records beside SVG-PATH.
Return two values: the compact reason-count annotation (NIL when the only
reason is :NONADVANCING-FRAME), and the report pathname (NIL when WRITE-REPORT
is NIL). The detailed report retains all counts even when the annotation is
suppressed. Summary-only mode does not construct or symbolicate raw records.
The selected profiler must already be stopped; call this before resetting
its raw buffer.
Groups use reason, last PC, and candidate return PC, never frame addresses.
Allocation groups retain both sample counts and attributed-byte weights."
  (let* ((records (ext:profile-walk-stops :allocation allocation
                                        :summary-only (not write-report)))
         (report-path (when write-report
                        (make-pathname :type "stops.sexp"
                                       :defaults (pathname svg-path))))
         (reasons (make-hash-table :test 'eq))
         (groups (when write-report (make-hash-table :test 'equal)))
         (sample-count 0)
         (attributed-bytes (when allocation 0)))
    (labels ((add-weight (entry count bytes)
               (incf (getf entry :samples) count)
               (when allocation
                 (incf (getf entry :attributed-bytes) bytes))))
      (loop for record across records
            for reason = (getf record :reason)
            for count = (getf record :samples)
            for bytes = (getf record :attributed-bytes)
            for key = (when write-report
                        (list reason (getf record :last-pc)
                              (getf record :return-pc)))
            do (incf sample-count count)
               (when allocation (incf attributed-bytes bytes))
               (add-weight
                (or (gethash reason reasons)
                    (setf (gethash reason reasons)
                          (list :reason reason :samples 0
                                :attributed-bytes (when allocation 0))))
                count bytes)
               (when write-report
                 (add-weight
                  (or (gethash key groups)
                      (setf (gethash key groups)
                            (list :reason reason
                                  :last-pc (getf record :last-pc)
                                  :last-function (getf record :last-function)
                                  :return-pc (getf record :return-pc)
                                  :return-function (getf record :return-function)
                                  :samples 0
                                  :attributed-bytes (when allocation 0))))
                  count bytes))))
    (let* ((reason-labels '((:null-frame-pointer . "null-FP")
                            (:null-return-address . "null-PC")
                            (:unaligned-frame-pointer . "unaligned")
                            (:frame-outside-stack . "outside")
                            (:unrecognized-return-address . "unknown-PC")
                            (:nonadvancing-frame . "nonadvance")
                            (:depth-limit . "depth")
                            (:no-stack-bounds . "no-bounds")
                            (:unknown . "unknown")))
           (reason-counts
             (loop for (reason . label) in reason-labels
                   for entry = (gethash reason reasons)
                   when entry collect entry))
           (summary
             (with-output-to-string (out)
               (write-string "Walk stops: " out)
               (if (zerop sample-count)
                   (write-string "0 samples" out)
                   (loop with separator = ""
                         for (reason . label) in reason-labels
                         for entry = (gethash reason reasons)
                         when entry
                           do (format out "~a~a=~d" separator label
                                      (getf entry :samples))
                              (setf separator ", ")))))
           (group-list
             (when write-report
               (sort (loop for entry being the hash-values of groups collect entry)
                     #'> :key (lambda (entry)
                                (getf entry (if allocation :attributed-bytes
                                                :samples)))))))
      (when write-report
        (with-open-file (out report-path :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
          ;; Override interactive printer limits: every raw address and record
          ;; must survive a READ of this file, including when there are no samples.
          (with-standard-io-syntax
            (write (list :format :clasp-profile-walk-stops :version 1
                         :kind (if allocation :allocation :cpu)
                         :summary summary :samples sample-count
                         :attributed-bytes attributed-bytes
                         :reason-counts reason-counts
                         :groups (coerce group-list 'vector)
                         :records records)
                   :stream out :readably t :pretty nil)
            (terpri out))))
      ;; Nonadvancing links commonly terminate the native startup frame.
      ;; Suppress that lone annotation without claiming every such link is
      ;; proof of reaching the top; keep all reasons in the detailed report.
      (values (unless (and (= (hash-table-count reasons) 1)
                           (gethash :nonadvancing-frame reasons))
                summary)
              report-path))))

(defmacro with-flame-profile ((&key (path (format nil "~~/public_html/flame-~d.svg"
                                                  (core:getpid)))
                                 (rate 97) (title "")
                                 (buffer-bytes 0)
                                 (walk-stop-report nil)) &body body)
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

The SVG header and terminal filename message also report the dynamic
executable-range cache's filled slots, capacity, and rejected registrations.
Walk-stop reason counts appear in the SVG and console unless the only
reason is a nonadvancing frame. Set
:WALK-STOP-REPORT T to also save raw per-sample stops and grouped counts
beside the SVG in a .stops.sexp companion file (default NIL).

Returns the values of BODY. Signals an error if the profiler is already
running. The profiler is guaranteed to be stopped and reset on any exit
(normal return, throw, or condition)."
  (let ((path-var (gensym "PATH"))
        (rate-var (gensym "RATE"))
        (buffer-bytes-var (gensym "BUFFER-BYTES"))
        (title-var (gensym "TITLE"))
        (walk-stop-report-var (gensym "WALK-STOP-REPORT"))
        (start-ut-var (gensym "START-UT"))
        (start-real-var (gensym "START-REAL"))
        (annotation-var (gensym "ANNOTATION"))
        (range-annotation-var (gensym "RANGE-ANNOTATION"))
        (stop-annotation-var (gensym "STOP-ANNOTATION"))
        (stop-path-var (gensym "STOP-PATH"))
        (vals-var (gensym "VALS")))
    `(let ((,path-var ,path)
           (,rate-var ,rate)
           (,buffer-bytes-var ,buffer-bytes)
           (,title-var ,title)
           (,walk-stop-report-var ,walk-stop-report))
       (when (ext:profile-running-p)
         (error "Sampling profiler is already running"))
       ;; Stamp the wall clock immediately before the profiler starts, so the
       ;; recorded window matches what the samples actually cover.
       (let ((,start-ut-var (get-universal-time))
             (,start-real-var (get-internal-real-time))
             (,annotation-var "")
             (,range-annotation-var "")
             ,vals-var)
         (unless (ext:profile-start :rate ,rate-var :buffer-bytes ,buffer-bytes-var)
           (error "Failed to start sampling profiler"))
         (unwind-protect
              (setf ,vals-var (multiple-value-list (progn ,@body)))
           (ext:profile-stop)
           (unwind-protect
                (progn
                  ;; Close the window before symbolication and rendering.
                  (setf ,annotation-var
                        (flame-profile-annotation
                         ,start-ut-var
                         (/ (float (- (get-internal-real-time) ,start-real-var) 1d0)
                            internal-time-units-per-second)))
                  (setf ,range-annotation-var
                        (profile-executable-range-annotation))
                  (multiple-value-bind (,stop-annotation-var ,stop-path-var)
                      (write-profile-walk-stop-report
                       ,path-var :write-report ,walk-stop-report-var)
                    (setf ,annotation-var
                          (format nil "~a~%~a~@[~%~a~]" ,annotation-var
                                  ,range-annotation-var ,stop-annotation-var))
                    (cond (,stop-path-var
                           (format t "Wrote walk-stop report to ~s~@[ | ~a~]~%"
                                   ,stop-path-var ,stop-annotation-var))
                          (,stop-annotation-var
                           (format t "~a~%" ,stop-annotation-var))))
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
                          (flamegraph:flamegraph
                           :data samples :output out
                           :title (if (string= ,title-var "")
                                      (format nil "clasp ~A" (core:getpid))
                                      ,title-var)
                           :subtitle ,annotation-var :notes ,annotation-var))))
                    (format t "Profiling buffer: ~:d / ~:d bytes used (~,1f%), ~:d samples~@[, ~:d   DROPPED (buffer full)~]~%"
                            used avail
                            (if (plusp avail) (/ (* 100.0 used) avail) 0.0)
                            recorded
                            (when (plusp dropped) dropped))))
             (ext:profile-reset)))
         (format t "Wrote flame graph to ~s | ~a~%"
                 ,path-var ,range-annotation-var)
         (values-list ,vals-var)))))

(defmacro with-cpu-profile (&rest args)
  `(with-flame-profile ,@args))

(defmacro with-allocation-profile
    ((&key
       (path (format nil "~~/public_html/allocation-~d.svg"
                     (core:getpid)))
       (bytes-per-sample (* 1024 1024))
       (max-depth 4096)
       (title "")
       (buffer-bytes 0)
       (walk-stop-report nil))
     &body body)
  "Profile managed allocations performed by BODY and write an SVG flame graph.

Example:
  (ext:with-allocation-profile
      (:path \"/tmp/allocation.svg\"
       :bytes-per-sample (* 1024 1024))
    (my-expensive-computation))

BYTES-PER-SAMPLE controls the allocation sampling interval. Values below
1 MiB are clamped to 1 MiB. MAX-DEPTH controls the native stack depth and
is clamped to [1,4096]. BUFFER-BYTES zero selects the 64 MiB default ring.

The flame graph is weighted by attributed bytes rather than record count,
and each stack ends in an allocation-type frame. The measured window
excludes symbolication and SVG rendering.

The SVG subtitle and NOTES include attributed GiB and dropped bytes alongside
the start time and elapsed time. These are cumulative sampled allocations,
not live memory or RSS. RSS before and after profiling is reported separately
in GiB in both the SVG and console output. The RSS readings are taken outside
the measured window, with the final reading before symbolication and rendering.
The SVG header and terminal filename message also report the dynamic
executable-range cache's filled slots, capacity, and rejected registrations.
Walk-stop reason counts appear in the SVG and console unless the only
reason is a nonadvancing frame. Set
:WALK-STOP-REPORT T to also write a .stops.sexp companion file (default NIL).
That file preserves raw per-sample records and groups by reason and PCs,
with both sample counts and attributed-byte weights.

Returns no values. Signals an error if allocation profiling is already
active. The profiler is stopped and reset during every exit, including
nonlocal exits."
  (let ((path-var (gensym "PATH"))
        (bytes-per-sample-var (gensym "BYTES-PER-SAMPLE"))
        (max-depth-var (gensym "MAX-DEPTH"))
        (buffer-bytes-var (gensym "BUFFER-BYTES"))
        (title-var (gensym "TITLE"))
        (walk-stop-report-var (gensym "WALK-STOP-REPORT"))
        (rss-before-var (gensym "RSS-BEFORE"))
        (rss-after-var (gensym "RSS-AFTER"))
        (start-ut-var (gensym "START-UT"))
        (start-real-var (gensym "START-REAL"))
        (annotation-var (gensym "ANNOTATION"))
        (range-annotation-var (gensym "RANGE-ANNOTATION"))
        (stop-annotation-var (gensym "STOP-ANNOTATION"))
        (stop-path-var (gensym "STOP-PATH"))
        (wrote-var (gensym "WROTE")))
    `(let ((,path-var ,path)
           (,bytes-per-sample-var ,bytes-per-sample)
           (,max-depth-var ,max-depth)
           (,buffer-bytes-var ,buffer-bytes)
           (,title-var ,title)
           (,walk-stop-report-var ,walk-stop-report))
       (when (ext:allocation-profile-running-p)
         (error "Allocation profiler is already running"))
       (let* ((,rss-before-var (ext:current-rss-bytes))
              (,start-ut-var (get-universal-time))
              (,start-real-var (get-internal-real-time))
              (,annotation-var "")
              (,range-annotation-var "")
              (,wrote-var nil))
         (unless
             (ext:allocation-profile-start
              :bytes-per-sample ,bytes-per-sample-var
              :max-depth ,max-depth-var
              :buffer-bytes ,buffer-bytes-var)
           (error "Failed to start allocation profiler"))
         (unwind-protect
              (progn ,@body (values))
           (ext:allocation-profile-stop)
           (unwind-protect
                (progn
                  (setf ,annotation-var
                        (flame-profile-annotation
                         ,start-ut-var
                         (/ (float
                             (- (get-internal-real-time)
                                ,start-real-var)
                             1d0)
                            internal-time-units-per-second)))
                  (setf ,range-annotation-var
                        (profile-executable-range-annotation))
                  (let ((,rss-after-var (ext:current-rss-bytes))
                        (used
                          (ext:allocation-profile-bytes-used))
                        (available
                          (ext:allocation-profile-bytes-available))
                        (recorded
                          (ext:allocation-profile-samples-recorded))
                        (dropped
                          (ext:allocation-profile-samples-dropped))
                        (attributed
                          (ext:allocation-profile-bytes-attributed))
                        (dropped-bytes
                          (ext:allocation-profile-bytes-dropped)))
                    (multiple-value-bind (,stop-annotation-var ,stop-path-var)
                        (write-profile-walk-stop-report
                         ,path-var :allocation t :write-report ,walk-stop-report-var)
                      (setf ,annotation-var
                            (format nil
                                    "~a | attributed ~,3f GiB | dropped ~:d bytes | RSS before ~,3f GiB, after ~,3f GiB~%~a~@[~%~a~]"
                                    ,annotation-var
                                    (/ attributed (expt 1024d0 3))
                                    dropped-bytes
                                    (/ ,rss-before-var (expt 1024d0 3))
                                    (/ ,rss-after-var (expt 1024d0 3))
                                    ,range-annotation-var ,stop-annotation-var))
                      (cond (,stop-path-var
                             (format t "Wrote walk-stop report to ~s~@[ | ~a~]~%"
                                     ,stop-path-var ,stop-annotation-var))
                            (,stop-annotation-var
                             (format t "~a~%" ,stop-annotation-var))))
                    (let ((samples
                            (ext:allocation-profile-symbolicated-samples)))
                      (if (plusp (length samples))
                          (progn
                            (with-open-file
                                (out ,path-var
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
                              (flamegraph:flamegraph
                               :data samples
                               :output out
                               :title
                               (if (string= ,title-var "")
                                   (format nil
                                           "clasp allocations ~A"
                                           (core:getpid))
                                   ,title-var)
                               :subtitle ,annotation-var
                               :notes ,annotation-var
                               :colors "mem"
                               :name-type "Allocation:"
                               :count-name "bytes"))
                            (setf ,wrote-var t))
                          (format t
                                  "No allocation samples captured; no flame graph written.~%")))
                    (format t
                            "Allocation profiling buffer: ~:d / ~:d bytes used (~,1f%), ~:d records, ~:d attributed bytes~%"
                            used available
                            (if (plusp available)
                                (/ (* 100.0 used) available)
                                0.0)
                            recorded attributed)
                    (format t "RSS before profiling: ~,3f GiB; after: ~,3f GiB~%"
                            (/ ,rss-before-var (expt 1024d0 3))
                            (/ ,rss-after-var (expt 1024d0 3)))
                    (when (plusp dropped)
                      (format t
                              "Allocation profiler dropped ~:d records representing ~:d bytes (buffer full).~%"
                              dropped dropped-bytes))))
             (ext:allocation-profile-reset)))
         (when ,wrote-var
           (format t "Wrote allocation flame graph to ~s | ~a~%"
                   ,path-var ,range-annotation-var))
         (values)))))
