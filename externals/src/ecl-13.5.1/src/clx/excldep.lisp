;;; -*- Mode: common-lisp; Package: xlib; Base: 10; Lowercase: Yes -*-
;;;
;;; CLX -- excldep.cl
;;;
;;; Copyright (c) 1987, 1988, 1989 Franz Inc, Berkeley, Ca.
;;;
;;; Permission is granted to any individual or institution to use, copy,
;;; modify, and distribute this software, provided that this complete
;;; copyright and permission notice is maintained, intact, in all copies and
;;; supporting documentation.
;;;
;;; Franz Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

(in-package :xlib)

(eval-when (compile load eval)
  (require :foreign)
  (require :process)			; Needed even if scheduler is not
					; running.  (Must be able to make
					; a process-lock.)
  )

(eval-when (load)
  (provide :clx))


#-(or little-endian big-endian)
(eval-when (eval compile load)
  (let ((x '#(1)))
    (if (not (eq 0 (sys::memref x
				#.(sys::mdparam 'comp::md-lvector-data0-norm)
				0 :unsigned-byte)))
	(pushnew :little-endian *features*)
      (pushnew :big-endian *features*))))


(defmacro correct-case (string)
  ;; This macro converts the given string to the 
  ;; current preferred case, or leaves it alone in a case-sensitive mode.
  (let ((str (gensym)))
    `(let ((,str ,string))
       (case excl::*current-case-mode*
	 (:case-insensitive-lower
	  (string-downcase ,str))
	 (:case-insensitive-upper
	  (string-upcase ,str))
	 ((:case-sensitive-lower :case-sensitive-upper)
	  ,str)))))


(defconstant type-pred-alist
    '(#-(version>= 4 1 devel 16)
      (card8  . card8p)
      #-(version>= 4 1 devel 16)
      (card16 . card16p)
      #-(version>= 4 1 devel 16)
      (card29 . card29p)
      #-(version>= 4 1 devel 16)
      (card32 . card32p)
      #-(version>= 4 1 devel 16)
      (int8   . int8p)
      #-(version>= 4 1 devel 16)
      (int16  . int16p)
      #-(version>= 4 1 devel 16)
      (int32  . int32p)
      #-(version>= 4 1 devel 16)
      (mask16 . card16p)
      #-(version>= 4 1 devel 16)
      (mask32 . card32p)
      #-(version>= 4 1 devel 16)
      (pixel  . card32p)
      #-(version>= 4 1 devel 16)
      (resource-id . card29p)
      #-(version>= 4 1 devel 16)
      (keysym . card32p)
      (angle  . anglep)
      (color  . color-p)
      (bitmap-format . bitmap-format-p)
      (pixmap-format . pixmap-format-p)
      (display  . display-p)
      (drawable . drawable-p)
      (window   . window-p)
      (pixmap   . pixmap-p)
      (visual-info . visual-info-p)
      (colormap . colormap-p)
      (cursor . cursor-p)
      (gcontext .  gcontext-p)
      (screen . screen-p)
      (font . font-p)
      (image-x . image-x-p)
      (image-xy . image-xy-p)
      (image-z . image-z-p)
      (wm-hints . wm-hints-p)
      (wm-size-hints . wm-size-hints-p)
      ))

;; This (if (and ...) t nil) stuff has a purpose -- it lets the old 
;; sun4 compiler opencode the `and'.

#-(version>= 4 1 devel 16)
(defun card8p (x)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum x))
  (if (and (excl:fixnump x) (> #.(expt 2 8) x) (>= x 0))
      t
    nil))

#-(version>= 4 1 devel 16)
(defun card16p (x)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum x))
  (if (and (excl:fixnump x) (> #.(expt 2 16) x) (>= x 0))
      t
    nil))

#-(version>= 4 1 devel 16)
(defun card29p (x)
  (declare (optimize (speed 3) (safety 0)))
  (if (or (and (excl:fixnump x) (>= (the fixnum x) 0))
	  (and (excl:bignump x) (> #.(expt 2 29) (the bignum x))
	       (>= (the bignum x) 0)))
      t
    nil))

#-(version>= 4 1 devel 16)
(defun card32p (x)
  (declare (optimize (speed 3) (safety 0)))
  (if (or (and (excl:fixnump x) (>= (the fixnum x) 0))
	  (and (excl:bignump x) (> #.(expt 2 32) (the bignum x))
	       (>= (the bignum x) 0)))
      t
    nil))

#-(version>= 4 1 devel 16)
(defun int8p (x)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum x))
  (if (and (excl:fixnump x) (> #.(expt 2 7) x) (>= x #.(expt -2 7)))
      t
    nil))

#-(version>= 4 1 devel 16)
(defun int16p (x)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum x))
  (if (and (excl:fixnump x) (> #.(expt 2 15) x) (>= x #.(expt -2 15)))
      t
    nil))

#-(version>= 4 1 devel 16)
(defun int32p (x)
  (declare (optimize (speed 3) (safety 0)))
  (if (or (excl:fixnump x)
	  (and (excl:bignump x) (> #.(expt 2 31) (the bignum x))
	       (>= (the bignum x) #.(expt -2 31))))
      t
    nil))

;; This one can be handled better by knowing a little about what we're
;; testing for.  Plus this version can handle (single-float pi), which
;; is otherwise larger than pi!
(defun anglep (x)
  (declare (optimize (speed 3) (safety 0)))
  (if (or (and (excl::fixnump x) (>= (the fixnum x) #.(truncate (* -2 pi)))
	       (<= (the fixnum x) #.(truncate (* 2 pi))))
	  (and (excl::single-float-p x)
	       (>= (the single-float x) #.(float (* -2 pi) 0.0s0))
	       (<= (the single-float x) #.(float (* 2 pi) 0.0s0)))
	  (and (excl::double-float-p x)
	       (>= (the double-float x) #.(float (* -2 pi) 0.0d0))
	       (<= (the double-float x) #.(float (* 2 pi) 0.0d0))))
      t
    nil))

(eval-when (load eval)
  #+(version>= 4 1 devel 16)
  (mapcar #'(lambda (elt) (excl:add-typep-transformer (car elt) (cdr elt)))
	  type-pred-alist)
  #-(version>= 4 1 devel 16)
  (nconc excl::type-pred-alist type-pred-alist))


;; Return t if there is a character available for reading or on error,
;; otherwise return nil.
#-(version>= 6 0)
(progn

#-(or (version>= 4 2) mswindows)
(defun fd-char-avail-p (fd)
  (multiple-value-bind (available-p errcode)
      (comp::.primcall-sargs 'sys::filesys excl::fs-char-avail fd)
    (excl:if* errcode
       then t
       else available-p)))

#+(and (version>= 4 2) (not mswindows))
(defun fd-char-avail-p (fd)
  (excl::filesys-character-available-p fd))

#+mswindows
(defun fd-char-avail-p (socket-stream)
  (listen socket-stream))
)

#+(version>= 6 0)
(defun fd-char-avail-p (socket-stream)
  (excl::read-no-hang-p socket-stream))

(defmacro with-interrupt-checking-on (&body body)
  `(locally (declare (optimize (safety 1)))
     ,@body))

;; Read from the given fd into 'vector', which has element type card8.
;; Start storing at index 'start-index' and read exactly 'length' bytes.
;; Return t if an error or eof occurred, nil otherwise.
(defun fd-read-bytes (fd vector start-index length)
  ;; Read from the given stream fd into 'vector', which has element type card8.
  ;; Start storing at index 'start-index' and read exactly 'length' bytes.
  ;; Return t if an error or eof occurred, nil otherwise.
  (declare (fixnum next-index start-index length))
  (with-interrupt-checking-on
      (let ((end-index (+ start-index length)))
	(loop
	  (let ((next-index (excl:read-vector vector fd 
					      :start start-index
					      :end end-index)))
	    (excl:if* (eq next-index start-index)
	       then			; end of file before was all filled up
		    (return t)
	     elseif (eq next-index end-index)
	       then			; we're all done
		    (return nil)
	       else (setq start-index next-index)))))))


;; special patch for CLX (various process fixes)
;; patch1000.2

(eval-when (compile load eval)
  (unless (find-package :patch)
    (make-package :patch :use '(:lisp :excl))))

(in-package :patch)

(defvar *patches* nil)

#+allegro
(eval-when (compile eval load)
  (when (and (= excl::cl-major-version-number 3)
	     (or (= excl::cl-minor-version-number 0)
		 (and (= excl::cl-minor-version-number 1)
		      excl::cl-generation-number
		      (< excl::cl-generation-number 9))))
    (push :clx-r4-process-patches *features*)))

#+clx-r4-process-patches
(push (cons 1000.2 "special patch for CLX (various process fixes)")
      *patches*)


(in-package :mp)

#+clx-r4-process-patches
(export 'wait-for-input-available)


#+clx-r4-process-patches
(defun with-timeout-event (seconds fnc args)
  (unless *scheduler-stack-group* (start-scheduler)) ;[spr670]
  (let ((clock-event (make-clock-event)))
    (when (<= seconds 0) (setq seconds 0))
    (multiple-value-bind (secs msecs) (truncate seconds)
      ;; secs is now a nonegative integer, and msecs is either fixnum zero
      ;; or else something interesting.
      (unless (eq 0 msecs)
	(setq msecs (truncate (* 1000.0 msecs))))
      ;; Now msecs is also a nonnegative fixnum.
      (multiple-value-bind (now mnow) (excl::cl-internal-real-time)
	(incf secs now)
	(incf msecs mnow)
	(when (>= msecs 1000)
	  (decf msecs 1000)
	  (incf secs))
	(unless (excl:fixnump secs) (setq secs most-positive-fixnum))
	(setf (clock-event-secs clock-event) secs
	      (clock-event-msecs clock-event) msecs
	      (clock-event-function clock-event) fnc
	      (clock-event-args clock-event) args)))
    clock-event))


#+clx-r4-process-patches
(defmacro with-timeout ((seconds &body timeout-body) &body body)
  `(let* ((clock-event (with-timeout-event ,seconds
					   #'process-interrupt
					   (cons *current-process*
						 '(with-timeout-internal))))
	  (excl::*without-interrupts* t)
	  ret)
     (unwind-protect
	 ;; Warning: Branch tensioner better not reorder this code!
	 (setq ret (catch 'with-timeout-internal
		     (add-to-clock-queue clock-event)
		     (let ((excl::*without-interrupts* nil))
		       (multiple-value-list (progn ,@body)))))
       (excl:if* (eq ret 'with-timeout-internal)
	  then (let ((excl::*without-interrupts* nil))
		 (setq ret (multiple-value-list (progn ,@timeout-body))))
	  else (remove-from-clock-queue clock-event)))
     (values-list ret)))


#+clx-r4-process-patches
(defun process-lock (lock &optional (lock-value *current-process*)
				    (whostate "Lock") timeout)
  (declare (optimize (speed 3)))
  (unless (process-lock-p lock)
    (error "First argument to PROCESS-LOCK must be a process-lock: ~s" lock))
  (without-interrupts
   (excl:if* (null (process-lock-locker lock))
      then (setf (process-lock-locker lock) lock-value)
      else (excl:if* timeout
	      then (excl:if* (or (eq 0 timeout) ;for speed
				 (zerop timeout))
		      then nil
		      else (with-timeout (timeout)
			     (process-lock-1 lock lock-value whostate)))
	      else (process-lock-1 lock lock-value whostate)))))


#+clx-r4-process-patches
(defun process-lock-1 (lock lock-value whostate)
  (declare (type process-lock lock)
	   (optimize (speed 3)))
  (let ((process *current-process*))
    (declare (type process process))
    (unless process
      (error
       "PROCESS-LOCK may not be called on the scheduler's stack group."))
    (loop (unless (process-lock-locker lock)
	    (return (setf (process-lock-locker lock) lock-value)))
      (push process (process-lock-waiting lock))
      (let ((saved-whostate (process-whostate process)))
	(unwind-protect
	    (progn (setf (process-whostate process) whostate)
		   (process-add-arrest-reason process lock))
	  (setf (process-whostate process) saved-whostate))))))


#+clx-r4-process-patches
(defun process-wait (whostate function &rest args)
  (declare (optimize (speed 3)))
  ;; Run the wait function once here both for efficiency and as a
  ;; first line check for errors in the function.
  (unless (apply function args)
    (process-wait-1 whostate function args)))


#+clx-r4-process-patches
(defun process-wait-1 (whostate function args)
  (declare (optimize (speed 3)))
  (let ((process *current-process*))
    (declare (type process process))
    (unless process
      (error
       "Process-wait may not be called within the scheduler's stack group."))
    (let ((saved-whostate (process-whostate process)))
      (unwind-protect
	  (without-scheduling-internal
	   (without-interrupts
	    (setf (process-whostate process) whostate
		  (process-wait-function process) function
		  (process-wait-args process) args)
	    (chain-rem-q process)
	    (chain-ins-q process *waiting-processes*))
	   (process-resume-scheduler nil))
	(setf (process-whostate process) saved-whostate
	      (process-wait-function process) nil
	      (process-wait-args process) nil)))))


#+clx-r4-process-patches
(defun process-wait-with-timeout (whostate seconds function &rest args)
  ;; Now returns T upon completion, NIL upon timeout. -- 6Jun89 smh
  ;; [spr1135] [rfe939] Timeout won't throw out of interrupt level code.
  ;;  -- 28Feb90 smh
  ;; Run the wait function once here both for efficiency and as a
  ;; first line check for errors in the function.
  (excl:if* (apply function args)
     then t
     else (let ((ret (list nil)))
            (without-interrupts
             (let ((clock-event
                    (with-timeout-event seconds #'identity '(nil))))
               (add-to-clock-queue clock-event)
               (process-wait-1 whostate
                               #'(lambda (clock-event function args ret)
                                   (or (null (chain-next clock-event))
                                       (and (apply function args)
                                            (setf (car ret) 't))))
                               (list clock-event function args ret))))
            (car ret))))


;;
;; Returns nil on timeout, otherwise t.
;;
#+clx-r4-process-patches
(defun wait-for-input-available
    (stream-or-fd &key (wait-function #'listen)
		       (whostate "waiting for input")
		       timeout)
  (let ((fd (excl:if* (excl:fixnump stream-or-fd) then stream-or-fd
	     elseif (streamp stream-or-fd)
	       then (excl::stream-input-fn stream-or-fd)
	       else (error "wait-for-input-available expects a stream or file descriptor: ~s" stream-or-fd))))
    ;; At this point fd could be nil, since stream-input-fn returns nil for
    ;; streams that are output only, or for certain special purpose streams.
    (if fd
	(unwind-protect
	    (progn
	      (mp::mpwatchfor fd)
	      (excl:if* timeout
		 then (mp::process-wait-with-timeout
		       whostate timeout wait-function stream-or-fd)
		 else (mp::process-wait whostate wait-function stream-or-fd)
		      t))
	  (mp::mpunwatchfor fd))
      (excl:if* timeout
	 then (mp::process-wait-with-timeout
	       whostate timeout wait-function stream-or-fd)
	 else (mp::process-wait whostate wait-function stream-or-fd)
	      t))))
