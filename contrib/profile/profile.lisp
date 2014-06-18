;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(eval-when (:compile-toplevel :load-toplevel)
  (defpackage "PROFILE"
    (:nicknames "PROF" "SB-PROFILE")
    (:export "PROFILE" "REPORT" "RESET" "UNPROFILE" "UNPROFILE-ALL"))
)

(in-package "PROFILE")

;;;; reading internal run time with high resolution and low overhead

(defconstant +ticks-per-second+ internal-time-units-per-second)

#+threads
(defvar *profile-lock* (mp:make-lock))

(eval-when (:compile-toplevel :execute)
  (defmacro get-internal-ticks () '(get-internal-run-time))

  (defmacro gethash-locked (&rest args)
    `(mp:with-lock (*profile-lock*) (gethash ,@args)))

  (defmacro remhash-locked (&rest args)
    `(mp:with-lock (*profile-lock*) (remhash ,@args)))

  (defmacro remhash-locked (&rest args)
    `(mp:with-lock (*profile-lock*) (remhash ,@args)))

  (defmacro sethash-locked (hash key value)
    `(mp:with-lock (*profile-lock*) (setf (gethash ,hash ,key) ,value)))

  (defmacro dohash (((key value) hash &key locked) &body body)
    (let* ((it (gensym))
           (entry (gensym))
           (body
            `(with-hash-table-iterator (,it ,hash)
               (loop (multiple-value-bind (,entry ,key ,value)
                         (,it)
                       (unless ,entry (return))
                       (let ()
                         ,@body))))))
      (if locked
          `(mp:with-lock (*profile-lock*) ,body)
          body)))

  (defmacro without-package-locks (&rest body)
    `(progn ,@body))
)

(ffi:clines "
extern ECL_API size_t GC_get_total_bytes();
")

(let () ; This prevents compile-time evaluation of the following
  (defconstant +wrap+ (ffi:c-inline () () :object
				    "ecl_make_unsigned_integer(~((size_t)0))"
				    :one-liner t)))

(defun get-bytes-consed (orig)
  (let ((bytes (ffi:c-inline () () :object "ecl_make_unsigned_integer(GC_get_total_bytes())"
			     :one-liner t)))
    (if (< bytes orig)
	(+ (- +wrap+ orig) bytes)
	(- bytes orig))))

(deftype counter () '(integer 0 *))


;;;; implementation-dependent interfaces

#|
;;; To avoid unnecessary consing in the "encapsulation" code, we want
;;; find out the number of required arguments, and use &REST to
;;; capture only non-required arguments. This function returns (VALUES
;;; MIN-ARGS OPTIONALS-P), where MIN-ARGS is the number of required
;;; arguments and OPTIONALS-P is true iff there are any non-required
;;; arguments (such as &OPTIONAL, &REST, or &KEY).
(declaim (ftype (function ((or symbol cons)) (values fixnum t)) fun-signature))
(defun fun-signature (name)
  (let ((type (info :function :type name)))
    (cond ((not (fun-type-p type))
           (values 0 t))
          (t
           (values (length (fun-type-required type))
                   (or (fun-type-optional type)
                       (fun-type-keyp type)
                       (fun-type-rest type)))))))
|#

;;;; global data structures

;;; We associate a PROFILE-INFO structure with each profiled function
;;; name. This holds the functions that we call to manipulate the
;;; closure which implements the encapsulation.
(defvar *profiled-fun-name->info*
  (make-hash-table
   ;; EQL testing isn't good enough for generalized function names
   ;; like (SETF FOO).
   :test 'equal))
(defstruct (profile-info (:copier nil))
  (name              (missing-arg) :read-only t)
  (encapsulated-fun  (missing-arg) :type function :read-only t)
  (encapsulation-fun (missing-arg) :type function :read-only t)
  (read-stats-fun    (missing-arg) :type function :read-only t)
  (clear-stats-fun   (missing-arg) :type function :read-only t))

;;; These variables are used to subtract out the time and consing for
;;; recursive and other dynamically nested profiled calls. The total
;;; resource consumed for each nested call is added into the
;;; appropriate variable. When the outer function returns, these
;;; amounts are subtracted from the total.
(defvar *enclosed-ticks* 0)
(defvar *enclosed-consing* 0)
(declaim (type counter *enclosed-ticks* *enclosed-consing*))

;;; This variable is also used to subtract out time for nested
;;; profiled calls. The time inside the profile wrapper call --
;;; between its two calls to GET-INTERNAL-TICKS -- is accounted
;;; for by the *ENCLOSED-TIME* variable. However, there's also extra
;;; overhead involved, before we get to the first call to
;;; GET-INTERNAL-TICKS, and after we get to the second call. By
;;; keeping track of the count of enclosed profiled calls, we can try
;;; to compensate for that.
(defvar *enclosed-profiles* 0)
(declaim (type counter *enclosed-profiles*))

;;; the encapsulated function we're currently computing profiling data
;;; for, recorded so that we can detect the problem of
;;; PROFILE-computing machinery calling a function which has itself
;;; been PROFILEd
(defvar *computing-profiling-data-for*)

;;; the components of profiling overhead
(defstruct (overhead (:copier nil))
  ;; the number of ticks a bare function call takes. This is
  ;; factored into the other overheads, but not used for itself.
  (call (missing-arg) :type single-float :read-only t)
  ;; the number of ticks that will be charged to a profiled
  ;; function due to the profiling code
  (internal (missing-arg) :type single-float :read-only t)
  ;; the number of ticks of overhead for profiling that a single
  ;; profiled call adds to the total runtime for the program
  (total (missing-arg) :type single-float :read-only t))
(defvar *overhead*)
(declaim (type overhead *overhead*))
(makunbound '*overhead*) ; in case we reload this file when tweaking

;;;; profile encapsulations

;;; Trade off space for time by handling the usual all-FIXNUM cases inline.
(eval-when (:compile-toplevel)
  (defmacro fastbig- (x y)
    `(- ,x ,y))
  (defmacro fastbig-1+ (x)
    `(1+ ,x)))

;;; Return a collection of closures over the same lexical context,
;;;   (VALUES ENCAPSULATION-FUN READ-STATS-FUN CLEAR-STATS-FUN).
;;;
;;; ENCAPSULATION-FUN is a plug-in replacement for ENCAPSULATED-FUN,
;;; which updates statistics whenever it's called.
;;;
;;; READ-STATS-FUN returns the statistics:
;;;   (VALUES COUNT TIME CONSING PROFILE).
;;; COUNT is the count of calls to ENCAPSULATION-FUN. TICKS is
;;; the total number of ticks spent in ENCAPSULATED-FUN.
;;; CONSING is the total consing of ENCAPSULATION-FUN. PROFILE is the
;;; number of calls to the profiled function, stored for the purposes
;;; of trying to estimate that part of profiling overhead which occurs
;;; outside the interval between the profile wrapper function's timer
;;; calls.
;;;
;;; CLEAR-STATS-FUN clears the statistics.
;;;
;;; (The reason for implementing this as coupled closures, with the
;;; counts built into the lexical environment, is that we hope this
;;; will minimize profiling overhead.)
(defun profile-encapsulation-lambdas (encapsulated-fun)
  (declare (type function encapsulated-fun))
  (let* ((count 0)
         (ticks 0)
         (consing 0)
         (profiles 0))
    (declare (type counter count ticks consing profiles))
    (values
     ;; ENCAPSULATION-FUN
     (lambda (&rest args)
       (declare (optimize speed safety))
       ;; Make sure that we're not recursing infinitely.
       (when (boundp '*computing-profiling-data-for*)
         (unprofile-all) ; to avoid further recursion
         (error "~@<When computing profiling data for ~S, the profiled function ~S was called. To get out of this infinite recursion, all functions have been unprofiled. (Since the profiling system evidently uses ~S in its computations, it looks as though it's a bad idea to profile it.)~:@>"
                *computing-profiling-data-for*
                encapsulated-fun
                encapsulated-fun))
       ;; FIXME: Probably when this is stable, we should optimize (SAFETY 0).
       (incf count 1)
       (let ((dticks 0)
             (dconsing 0)
             (inner-enclosed-profiles 0)
	     (old-enclosed-ticks *enclosed-ticks*)
	     (old-enclosed-consing *enclosed-consing*)
	     (old-enclosed-profiles *enclosed-profiles*)
	     (start-ticks (get-internal-ticks))
	     (start-consed (get-bytes-consed 0)))
         (unwind-protect
	      (progn
		(setf *enclosed-ticks* 0
		      *enclosed-profiles* 0
		      *enclosed-consing* 0)
		(apply encapsulated-fun args))
	   (setf dticks (- (get-internal-ticks) start-ticks))
	   (setf dconsing (get-bytes-consed start-consed))
	   (setf inner-enclosed-profiles *enclosed-profiles*)
	   (let ((net-dticks (- dticks *enclosed-ticks*)))
	     (incf ticks net-dticks))
	   (let ((net-dconsing (- dconsing *enclosed-consing*)))
	     (incf consing net-dconsing))
	   (incf profiles inner-enclosed-profiles)
           (setf *enclosed-ticks* (+ old-enclosed-ticks dticks)
		 *enclosed-consing* (+ old-enclosed-consing dconsing)
		 *enclosed-profiles* (+ old-enclosed-profiles inner-enclosed-profiles 1)))))
     ;; READ-STATS-FUN
     (lambda ()
       (values count ticks consing profiles))
     ;; CLEAR-STATS-FUN
     (lambda ()
       (setf count 0
             ticks 0
             consing 0
             profiles 0)))))

;;;; interfaces

;;; A symbol or (SETF FOO) list names a function, a string names all
;;; the functions named by symbols in the named package.
(defun mapc-on-named-funs (function names)
  (dolist (name names)
    (etypecase name
      (symbol (funcall function name))
      (list
       (legal-fun-name-or-type-error name)
       ;; Then we map onto it.
       (funcall function name))
      (string (let ((package (si:coerce-to-package name)))
                (do-symbols (symbol package)
                  (when (eq (symbol-package symbol) package)
                    (when (and (fboundp symbol)
                               (not (macro-function symbol))
                               (not (special-operator-p symbol)))
                      (funcall function symbol))
                    (let ((setf-name `(setf ,symbol)))
                      (when (fboundp setf-name)
                        (funcall function setf-name)))))))))
  (values))

;;; Profile the named function, which should exist and not be profiled
;;; already.
(defun profile-1-unprofiled-fun (name)
  (let ((encapsulated-fun (fdefinition name)))
    (multiple-value-bind (encapsulation-fun read-stats-fun clear-stats-fun)
        (profile-encapsulation-lambdas encapsulated-fun)
      (without-package-locks
       (setf (fdefinition name)
             encapsulation-fun))
      (sethash-locked name *profiled-fun-name->info*
                      (make-profile-info :name name
                                         :encapsulated-fun encapsulated-fun
                                         :encapsulation-fun encapsulation-fun
                                         :read-stats-fun read-stats-fun
                                         :clear-stats-fun clear-stats-fun))
      (values))))

;;; Profile the named function. If already profiled, unprofile first.
(defun profile-1-fun (name)
  (cond ((fboundp name)
         (when (gethash-locked name *profiled-fun-name->info*)
           (warn "~S is already profiled, so unprofiling it first." name)
           (unprofile-1-fun name))
         (profile-1-unprofiled-fun name))
        (t
         (warn "ignoring undefined function ~S" name)))
  (values))

;;; Unprofile the named function, if it is profiled.
(defun unprofile-1-fun (name)
  (let ((pinfo (gethash-locked name *profiled-fun-name->info*)))
    (cond (pinfo
           (remhash-locked name *profiled-fun-name->info*)
           (if (eq (fdefinition name) (profile-info-encapsulation-fun pinfo))
               (without-package-locks
                (setf (fdefinition name) (profile-info-encapsulated-fun pinfo)))
               (warn "preserving current definition of redefined function ~S"
                     name)))
          (t
           (warn "~S is not a profiled function." name))))
  (values))

(defmacro profile (&rest names)
  "PROFILE Name*

   If no names are supplied, return the list of profiled functions.

   If names are supplied, wrap profiling code around the named functions.
   As in TRACE, the names are not evaluated. A symbol names a function.
   A string names all the functions named by symbols in the named
   package. If a function is already profiled, then unprofile and
   reprofile (useful to notice function redefinition.)  If a name is
   undefined, then we give a warning and ignore it. See also
   UNPROFILE, REPORT and RESET."
  (if (null names)
      `(loop for k being each hash-key in *profiled-fun-name->info*
             collecting k)
      `(mapc-on-named-funs #'profile-1-fun ',names)))

(defmacro unprofile (&rest names)
  "Unwrap any profiling code around the named functions, or if no names
  are given, unprofile all profiled functions. A symbol names
  a function. A string names all the functions named by symbols in the
  named package. NAMES defaults to the list of names of all currently
  profiled functions."
  (if names
      `(mapc-on-named-funs #'unprofile-1-fun ',names)
      `(unprofile-all)))

(defun unprofile-all ()
  (dohash ((name profile-info) *profiled-fun-name->info*
           :locked t)
    (declare (ignore profile-info))
    (unprofile-1-fun name)))

(defun reset ()
  "Reset the counters for all profiled functions."
  (dohash ((name profile-info) *profiled-fun-name->info* :locked t)
    (declare (ignore name))
    (funcall (profile-info-clear-stats-fun profile-info))))

;;;; reporting results

(defstruct (time-info (:copier nil))
  name
  calls
  seconds
  consing)

;;; Return our best guess for the run time in a function, subtracting
;;; out factors for profiling overhead. We subtract out the internal
;;; overhead for each call to this function, since the internal
;;; overhead is the part of the profiling overhead for a function that
;;; is charged to that function.
;;;
;;; We also subtract out a factor for each call to a profiled function
;;; within this profiled function. This factor is the total profiling
;;; overhead *minus the internal overhead*. We don't subtract out the
;;; internal overhead, since it was already subtracted when the nested
;;; profiled functions subtracted their running time from the time for
;;; the enclosing function.
(defun compensate-time (calls ticks profile)
  (let ((raw-compensated
         (- (/ (float ticks) (float +ticks-per-second+))
            (* (overhead-internal *overhead*) (float calls))
            (* (- (overhead-total *overhead*)
                  (overhead-internal *overhead*))
               (float profile)))))
    (max raw-compensated 0.0)))

(defun report ()
  "Report results from profiling. The results are approximately adjusted
for profiling overhead. The compensation may be rather inaccurate when
bignums are involved in runtime calculation, as in a very-long-running
Lisp process."
  (unless (boundp '*overhead*)
    (setf *overhead*
          (compute-overhead)))
  (let ((time-info-list ())
        (no-call-name-list ()))
    (dohash ((name pinfo) *profiled-fun-name->info* :locked t)
      (unless (eq (fdefinition name)
                  (profile-info-encapsulation-fun pinfo))
        (warn "Function ~S has been redefined, so times may be inaccurate.~@
               PROFILE it again to record calls to the new definition."
              name))
      (multiple-value-bind (calls ticks consing profile)
          (funcall (profile-info-read-stats-fun pinfo))
        (if (zerop calls)
            (push name no-call-name-list)
            (push (make-time-info :name name
                                  :calls calls
                                  :seconds (compensate-time calls
                                                            ticks
                                                            profile)
                                  :consing consing)
                  time-info-list))))

    (setf time-info-list
          (sort time-info-list
                #'>=
                :key #'time-info-seconds))
    (print-profile-table time-info-list)

    (when no-call-name-list
      (format *trace-output*
              "~%These functions were not called:~%~{~<~%~:; ~S~>~}~%"
              (sort no-call-name-list #'string<
                    :key (lambda (name)
                           (symbol-name
                            (if (consp name)
                                (cadr name)
                                name))))))

    (values)))


(defun print-profile-table (time-info-list)
  (let ((total-seconds 0.0)
        (total-consed 0)
        (total-calls 0)
        (seconds-width (length "seconds"))
        (consed-width (length "consed"))
        (calls-width (length "calls"))
        (sec/call-width 10)
        (name-width 6))
    (dolist (time-info time-info-list)
      (incf total-seconds (time-info-seconds time-info))
      (incf total-consed (time-info-consing time-info))
      (incf total-calls (time-info-calls time-info)))
    (setf seconds-width (max (length (format nil "~10,3F" total-seconds))
                             seconds-width)
          calls-width (max (length (format nil "~:D" total-calls))
                           calls-width)
          consed-width (max (length (format nil "~:D" total-consed))
                            consed-width))

    (flet ((dashes ()
             (dotimes (i (+ seconds-width consed-width calls-width
                            sec/call-width name-width
                            (* 5 3)))
               (write-char #\- *trace-output*))
             (terpri *trace-output*)))
      (format *trace-output* "~&~@{ ~v:@<~A~>~^|~}~%"
              seconds-width "seconds"
              (1+ consed-width) "consed"
              (1+ calls-width) "calls"
              (1+ sec/call-width) "sec/call"
              (1+ name-width) "name")

      (dashes)

      (dolist (time-info time-info-list)
        (format *trace-output* "~v,3F | ~v:D | ~v:D | ~10,6F | ~S~%"
                seconds-width (time-info-seconds time-info)
                consed-width (time-info-consing time-info)
                calls-width (time-info-calls time-info)
                (/ (time-info-seconds time-info)
                   (float (time-info-calls time-info)))
                (time-info-name time-info)))

      (dashes)

      (format *trace-output* "~v,3F | ~v:D | ~v:D |            | Total~%"
                seconds-width total-seconds
                consed-width total-consed
                calls-width total-calls)

      (format *trace-output*
              "~%estimated total profiling overhead: ~4,2F seconds~%"
              (* (overhead-total *overhead*) (float total-calls)))
      (format *trace-output*
              "~&overhead estimation parameters:~%  ~Ss/call, ~Ss total profiling, ~Ss internal profiling~%"
              (overhead-call *overhead*)
              (overhead-total *overhead*)
              (overhead-internal *overhead*)))))


;;;; overhead estimation

;;; We average the timing overhead over this many iterations.
;;;
;;; (This is a variable, not a constant, so that it can be set in
;;; .sbclrc if desired. Right now, that's an unsupported extension
;;; that I (WHN) use for my own experimentation, but it might
;;; become supported someday. Comments?)
(declaim (type unsigned-byte *timer-overhead-iterations*))
(defparameter *timer-overhead-iterations*
  500000)

;;; a dummy function that we profile to find profiling overhead
(declaim (notinline compute-overhead-aux))
(defun compute-overhead-aux (x)
  (declare (ignore x)))

;;; Return a newly computed OVERHEAD object.
(defun compute-overhead ()
  (format *debug-io* "~&measuring PROFILE overhead..")
  (flet ((frob ()
           (let ((start (get-internal-ticks))
                 (fun (symbol-function 'compute-overhead-aux)))
             (declare (type function fun))
             (dotimes (i *timer-overhead-iterations*)
               (funcall fun fun))
             (/ (float (- (get-internal-ticks) start))
                (float +ticks-per-second+)
                (float *timer-overhead-iterations*)))))
    (let (;; Measure unprofiled calls to estimate call overhead.
          (call-overhead (frob))
          total-overhead
          internal-overhead)
      ;; Measure profiled calls to estimate profiling overhead.
      (unwind-protect
          (progn
            (profile compute-overhead-aux)
            (setf total-overhead
                  (- (frob) call-overhead)))
        (let* ((pinfo (gethash-locked 'compute-overhead-aux
                                      *profiled-fun-name->info*))
               (read-stats-fun (profile-info-read-stats-fun pinfo))
               (time (nth-value 1 (funcall read-stats-fun))))
          (setf internal-overhead
                (/ (float time)
                   (float +ticks-per-second+)
                   (float *timer-overhead-iterations*))))
        (unprofile compute-overhead-aux))
      (prog1
          (make-overhead :call call-overhead
                         :total total-overhead
                         :internal internal-overhead)
        (format *debug-io* "done~%")))))

;;; It would be bad to compute *OVERHEAD*, save it into a .core file,
;;; then load the old *OVERHEAD* value from the .core file into a
;;; different machine running at a different speed. We avoid this by
;;; erasing *CALL-OVERHEAD* whenever we save a .core file.
(defun profile-deinit ()
  (without-package-locks
    (makunbound '*overhead*)))
