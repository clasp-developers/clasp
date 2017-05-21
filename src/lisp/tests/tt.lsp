(in-package :core)

(defun recode-universal-time (sec min hour day month year tz dst)
  (declare (si::c-local))
  (let ((days (+ (if (and (leap-year-p year) (> month 2)) 1 0)
		 (1- day)
		 (svref month-startdays (1- month))
		 (number-of-days-from-1900 year))))
    (+ sec (* 60 (+ min (* 60 (+ tz dst hour (* 24 days))))))))

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
  (declare (si::c-local))
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
    #-(or ecl-min clasp-min)
    (ffi::c-inline core:unix-daylight-saving-time (unix-time) (:unsigned-long) :bool "
{
	time_t when = (#0);
	struct tm *ltm = localtime(&when);
	@(return) = ltm->tm_isdst;
}"
		   :one-liner nil)))
