;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "SYSTEM")

;;; This could be improved, e.g. getting the lambda expression of
;;; interpreted functions, but there are better introspection designs.
(defun function-lambda-expression (function)
  (values nil
          (and (typep function 'core:closure)
               (not (zerop (core:closure-length function))))
          (core:function-name function)))

(defun   logical-pathname-translations (p)
  (or (si:pathname-translations p)
      (error 'simple-type-error
             :datum p
             :expected-type 'logical-pathname
             :format-control "logical host not yet defined: ~S"
             :format-arguments (list p))))
(defsetf logical-pathname-translations si:pathname-translations)

(defun load-logical-pathname-translations (host)
  "Search for a logical pathname named host, if not already defined. If already
defined no attempt to find or load a definition is attempted and NIL is
returned. If host is not already defined, but definition is found and loaded
successfully, T is returned, else error."
  (declare (type string host)
           (ext:check-arguments-type))
  (let ((*autoload-translations* nil))
    (unless (or (string-equal host "sys")
                (si::pathname-translations host))
      (with-open-file (in-str (make-pathname :defaults "sys:translations;"
                                             :name (string-downcase host)
                                             :type "translations"))
        (if *load-verbose*
            (format *error-output*
                    ";; Loading pathname translations from ~A~%"
                    (namestring (truename in-str))))
        (setf (logical-pathname-translations host) (read in-str)))
      t)))

(defvar *do-time-level* -1)

(defun do-time (closure)
  (let* ((real-start (get-internal-real-time))
	 (run-start (get-internal-run-time))
	 (llvm-finalization-time-start llvm-sys:*accumulated-llvm-finalization-time*)
	 (llvm-finalization-number-start llvm-sys:*number-of-llvm-finalizations*)
	 llvm-finalization-time-end
	 llvm-finalization-number-end
	 (clang-link-time-start llvm-sys:*accumulated-clang-link-time*)
	 (clang-link-number-start llvm-sys:*number-of-clang-links*)
         #+(and debug-track-unwinds) (start-unwinds (gctools:unwind-counter))
         #+(and debug-track-unwinds) end-unwinds
         #+(and debug-track-unwinds) (start-return-from (gctools:return-from-counter))
         #+(and debug-track-unwinds) end-return-from
         #+(and debug-track-unwinds) (start-dynamic-go (gctools:dynamic-go-counter))
         #+(and debug-track-unwinds) end-dynamic-go
         #+(and debug-track-unwinds) (start-catch-throw (gctools:catch-throw-counter))
         #+(and debug-track-unwinds) end-catch-throw
	 clang-link-time-end
	 clang-link-number-end
	 gc-start
         clasp-bytes-start clasp-bytes-end
	 real-end
	 run-end
         interpreted-calls-start interpreted-calls-end
         llh-calls-begin llh-calls-end
	 gc-end)
    ;; Garbage collection forces counters to be updated
    (multiple-value-setq (clasp-bytes-start)
      (gctools:bytes-allocated))
    (setq interpreted-calls-start (core:interpreted-closure-calls))
    (multiple-value-prog1
	(funcall closure)
      (multiple-value-setq (clasp-bytes-end)
        (gctools:bytes-allocated))
      (setq run-end (get-internal-run-time)
	    real-end (get-internal-real-time)
            interpreted-calls-end (core:interpreted-closure-calls)
	    llvm-finalization-time-end llvm-sys:*accumulated-llvm-finalization-time*
	    llvm-finalization-number-end llvm-sys:*number-of-llvm-finalizations*
	    clang-link-time-end llvm-sys:*accumulated-clang-link-time*
	    clang-link-number-end llvm-sys:*number-of-clang-links*
            )
      #+(and debug-track-unwinds) (setf end-unwinds (gctools:unwind-counter))
      #+(and debug-track-unwinds) (setf end-return-from (gctools:return-from-counter))
      #+(and debug-track-unwinds) (setf end-dynamic-go (gctools:dynamic-go-counter))
      #+(and debug-track-unwinds) (setf end-catch-throw (gctools:catch-throw-counter))
      (format *trace-output*
              "Real time           : ~,3F secs~%~
              Run time            : ~,3F secs~%~
              Bytes consed        : ~a bytes~%~
              LLVM time           : ~,3F secs~%~
              LLVM compiles       : ~A~%~
              clang link time     : ~,3F secs~%~
              clang links         : ~A~%~
              Interpreted closures: ~A~%"
              (float (/ (- real-end real-start) internal-time-units-per-second))
              (float (/ (- run-end run-start) internal-time-units-per-second))
              (- clasp-bytes-end clasp-bytes-start)
              (- llvm-finalization-time-end llvm-finalization-time-start)
              (- llvm-finalization-number-end llvm-finalization-number-start)
              (- clang-link-time-end clang-link-time-start)
              (- clang-link-number-end clang-link-number-start)
              (- interpreted-calls-end interpreted-calls-start))
      #+(and debug-track-unwinds)
      (format *trace-output*
              "Unwinds             : ~A~%~
              ReturnFrom unwinds  : ~A~%~
              DynamicGo unwinds   : ~A~%~
              CatchThrow unwinds  : ~A~%"
              (- end-unwinds start-unwinds)
              (- end-return-from start-return-from)
              (- end-dynamic-go start-dynamic-go)
              (- end-catch-throw start-catch-throw))))
  #+boehm-gc
  (let* ((*do-time-level* (1+ *do-time-level*))
         real-start
	 run-start
	 consed-start
	 gc-no-start
	 real-end
	 run-end
	 consed-end
	 gc-no-end)
    ;; Garbage collection forces the value of counters to be updated
    (si::gc t)
    ;; If there are no nested calls, we just reset the counters
    (when (zerop *do-time-level*) (si::gc-stats 0))
    ;; but in general we copy the previous values.
    (multiple-value-setq (consed-start gc-no-start) (gc-stats t))
    (setq real-start (get-internal-real-time)
	  run-start (get-internal-run-time))
    (multiple-value-prog1
	(funcall closure)
      (setq run-end (get-internal-run-time)
	    real-end (get-internal-real-time))
      ;; Garbage collection forces the value of counters to be updated
      (si::gc t)
      (multiple-value-setq (consed-end gc-no-end) (gc-stats nil))
      (fresh-line *trace-output*)
      (format *trace-output*
	      "real time : ~,3F secs~%~
              run time  : ~,3F secs~%~
              gc count  : ~D times~%~
              consed    : ~D bytes~%"
	      (/ (- real-end real-start) internal-time-units-per-second)
	      (/ (- run-end run-start) internal-time-units-per-second)
	      (- gc-no-end gc-no-start)
	      (- consed-end consed-start)))))

(defmacro time (form)
  "Syntax: (time form)
Evaluates FORM, outputs the realtime and runtime used for the evaluation to
*TRACE-OUTPUT*, and then returns all values of FORM."
  `(do-time #'(lambda () ,form)))

(defun leap-year-p (y)
  (and (zerop (mod y 4))
       (or (not (zerop (mod y 100))) (zerop (mod y 400)))))

(defun number-of-days-from-1900 (y)
  (let ((y1 (1- y)))
    (+ (* (- y 1900) 365)
       (floor y1 4) (- (floor y1 100)) (floor y1 400)
       -460)))

(defconstant-eqx month-startdays #(0 31 59 90 120 151 181 212 243 273 304 334 365) equalp)


#-clasp-min
(defun get-local-time-zone ()
  "Returns the number of hours West of Greenwich for the local time zone."
  (core:unix-get-local-time-zone))

(defun recode-universal-time (sec min hour day month year tz dst)
  (let ((days (+ (if (and (leap-year-p year) (> month 2)) 1 0)
		 (1- day)
		 (svref month-startdays (1- month))
		 (number-of-days-from-1900 year))))
    (+ sec (* 60 (+ min (* 60 (+ tz dst hour (* 24 days))))))))

#-clasp-min
(defun decode-universal-time (orig-ut &optional (tz nil tz-p) &aux (dstp nil))
  "Args: (integer &optional (timezone (si::get-local-time-zone)))
Returns as nine values the day-and-time represented by INTEGER.  See GET-
DECODED-TIME."
(loop
  (let* ((ut orig-ut) sec min hour day month year dow days)
    (unless tz
      (setq tz (get-local-time-zone)))
    (decf ut (round (* (+ tz (if dstp -1 0)) 3600)))
    (multiple-value-setq (ut sec) (floor ut 60))
    (multiple-value-setq (ut min) (floor ut 60))
    (multiple-value-setq (days hour) (floor ut 24))
    (setq dow (mod days 7))
    (setq year (+ 1900 (floor days 366))) ; Guess!
    (do ((x))
        ((< (setq x (- days (number-of-days-from-1900 year)))
            (if (leap-year-p year) 366 365))
         (setq day (1+ x)))
      (incf year))
    (when (leap-year-p year)
      (cond ((= day 60) (setf month 2 day 29))
	    ((> day 60) (decf day))))
    (unless month
      (setq month (position day month-startdays :test #'<=)
	    day (- day (svref month-startdays (1- month)))))
    (if (and (not tz-p) (daylight-saving-time-p orig-ut year))
	(setf tz-p t dstp t)
	(return (values sec min hour day month year dow dstp tz))))))

(defun encode-universal-time (sec min hour day month year &optional tz)
  "Args: (second minute hour date month year
       &optional (timezone (si::get-local-time-zone)))
Returns an integer that represents the given day-and-time.  See
GET-DECODED-TIME."
  (when (<= 0 year 99)
    ;; adjust to year in the century within 50 years of this year
    (multiple-value-bind (sec min hour day month this-year dow dstp tz)
	(get-decoded-time)
      (declare (ignore sec min hour day month dow dstp tz))
      (incf year (* 100 (ceiling (- this-year year 50) 100)))))
  (let ((dst 0))
    (unless tz
      (setq tz (rational (get-local-time-zone)))
      (when (daylight-saving-time-p (recode-universal-time sec min hour day month year tz -1) year)
	;; assume DST applies, and check if at corresponging UT it applies.
	;; There is an ambiguity between midnight and 1 o'clock on the day
	;; when time reverts from DST to solar:
	;; 12:01 on that day could be either 11:01 UT (before the switch) or
	;; 12:01 UT (after the switch). We opt for the former.
	(setf dst -1)))
    (recode-universal-time sec min hour day month year tz dst)))

(defun daylight-saving-time-p (universal-time year)
  "Returns T if Daylight Saving Time applies to the local time zone at
Universal Time UT, which defaults to the current time."
  ;; Some systems cannot deal with dates before 1-1-1970 and no POSIX
  ;; system will be able to handle dates beyond 2038. We must
  ;; therefore restrict the time to the interval that can handled by
  ;; the timezone database.
  (let* ((utc-1-1-1970 2208988800)
	 (unix-time (- universal-time utc-1-1-1970)))
    (cond ((minusp unix-time)
	   ;; For dates before 1970 we shift to 1980/81 to guess the daylight
	   ;; saving times.
	   (setf unix-time
		 (+ (if (leap-year-p year)
			#.(encode-universal-time 0 0 0 1 1 1980 0)
			#.(encode-universal-time 0 0 0 1 1 1981 0))
		    (- universal-time (encode-universal-time 0 0 0 1 1 year 0) utc-1-1-1970))))
	  ((not (fixnump unix-time))
	   ;; Same if date is too big: we shift to year 2035/36, like SBCL does.
	   (setf unix-time
		 (+ (if (leap-year-p year)
			#.(encode-universal-time 0 0 0 1 1 2032 0)
			#.(encode-universal-time 0 0 0 1 1 2033 0))
		    (- universal-time (encode-universal-time 0 0 0 1 1 year 0) utc-1-1-1970)))))
    #-clasp-min
    (core:unix-daylight-saving-time unix-time)))

(defun get-decoded-time ()
  "Args: ()
Returns the current day-and-time as nine values:
	second (0 - 59)
	minute (0 - 59)
	hour (0 - 23)
	date (1 - 31)
	month (1 - 12)
	year (Christian, not Japanese long-live-Emperor)
	day of week (0 for Mon, .. 6 for Sun)
	summer time or not (T or NIL)
	time zone (-9 in Japan)
Sunday is the *last* day of the week!!"
  (decode-universal-time (get-universal-time)))

(defun ensure-directories-exist (pathname &key verbose (mode #o777))
"Args: (ensure-directories pathname &key :verbose)
Creates tree of directories specified by the given pathname. Outputs
	(VALUES pathname created)
where CREATED is true only if we succeeded on creating all directories."
  (let* ((created nil)
	 (full-pathname (merge-pathnames pathname))
	 d)
    (when (typep full-pathname 'logical-pathname)
      (setf full-pathname (translate-logical-pathname full-pathname)))
    (when (or (wild-pathname-p full-pathname :directory)
	      (wild-pathname-p full-pathname :host)
	      (wild-pathname-p full-pathname :device))
      (error 'file-error :pathname pathname))
    ;; Here we have already a full pathname. We set our own
    ;; *default-pathname-defaults* to avoid that the user's value,
    ;; which may contain names or types, clobbers our computations.
    (let ((*default-pathname-defaults*
	   (make-pathname :name nil :type nil :directory nil
			  :defaults full-pathname)))
      (dolist (item (pathname-directory full-pathname))
	(setf d (nconc d (list item)))
	(let* ((p (make-pathname :directory d :defaults *default-pathname-defaults*)))
	  (unless (or (symbolp item) (si::file-kind p nil))
	    (setf created t)
	    (let ((ps (namestring p)))
	      (when verbose
		(format t "~%;;; Making directory ~A" ps))
	      (si::mkdir ps mode)))))
      (values pathname created))))

(defmacro with-hash-table-iterator ((iterator package) &body body)
"Syntax: (with-hash-table-iterator (iterator package) &body body)
Loop over the elements of a hash table. ITERATOR is a lexically bound function
that outputs three values
	(VALUES entry-p key value)
ENTRY-P is true only if KEY and VALUE denote a pair of key and value of the
hash table; otherwise it signals that we have reached the end of the hash table."
  `(let ((,iterator (hash-table-iterator ,package)))
    (macrolet ((,iterator () (list 'funcall ',iterator)))
      ,@body)))

(defun sharp-!-reader (stream subchar arg)
  (declare (ignore arg subchar))
  (read-line stream)
  (values))

(set-dispatch-macro-character #\# #\! 'sharp-!-reader)

(defun si::simple-program-error (message &rest datum)
  (signal-simple-error 'simple-program-error nil message datum))

#+debug-count-allocations
(defun do-allocations (closure)
  (let ((start-memory (gctools:allocation-counts)))
    (multiple-value-prog1
        (funcall closure)
      (let ((end-memory (gctools:allocation-counts)))
        (let* ((number-of-stamps (gctools:next-stamp-value))
               (stamp-names (make-array number-of-stamps))
               (stamp-name-alist (gctools:get-stamp-name-map))
               (allocs-stamp-name nil))
          (dolist (name-stamp stamp-name-alist)
            (let ((name (car name-stamp))
                  (stamp (cdr name-stamp)))
              (setf (aref stamp-names stamp) name)))
          (dotimes (i (length end-memory))
            (let* ((start-count (if (>= i (length start-memory)) 0 (svref start-memory i)))
                   (end-count (if (>= i (length end-memory)) 0 (svref end-memory i)))
                   (allocs (- end-count start-count)))
              (when (> allocs 0)
                (push (list allocs (svref stamp-names i) i) allocs-stamp-name))))
          (setf allocs-stamp-name (sort allocs-stamp-name #'< :key #'car))
          (terpri *trace-output*)
          (format *trace-output* "Allocations  Stamp-name/Stamp~%")
          (dolist (part allocs-stamp-name)
            (core:bformat *trace-output* "%10d %s/%d%N"
                          (first part)
                          (second part)
                          (third part))))))))

#+debug-count-allocations
(defmacro allocations (form)
  "Syntax: (allocations form)
Evaluates FORM, outputs the allocations that took place for the evaluation to
*TRACE-OUTPUT*, and then returns all values of FORM."
  `(do-allocations #'(lambda () ,form)))

              
#+debug-count-allocations
(defun do-collect-backtraces-for-allocations-by-stamp (backtrace-filename stamp closure)
  (unless (and (>= stamp 0) (< stamp (gctools:next-stamp-value)))
    (error "Stamp value ~d must be less than maximum stamp value ~d" stamp (gctools:next-stamp-value)))
  (unwind-protect
       (progn
         (gctools:start-collecting-backtraces-for-allocations-by-stamp backtrace-filename stamp)
         (funcall closure))
    (gctools:stop-collecting-backtraces-for-allocations-by-stamp)))


#+debug-count-allocations
(defmacro collect-backtraces-for-allocations-by-stamp (backtrace-filename stamp form)
  "Syntax: (allocations form)
Evaluates FORM, outputs the allocations that took place for the evaluation to
*TRACE-OUTPUT*, and then returns all values of FORM."
  `(do-collect-backtraces-for-allocations-by-stamp ,backtrace-filename ,stamp #'(lambda () ,form)))


#+debug-count-allocations
(export '(allocations collect-backtraces-for-allocations-by-stamp))
