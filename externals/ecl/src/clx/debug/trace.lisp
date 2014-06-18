;;; -*- Mode: LISP; Syntax: Common-lisp; Package: XLIB; Base: 10; Lowercase: Yes -*-

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 2909
;;;			       AUSTIN, TEXAS 78769
;;;
;;; Copyright (C) 1987 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

;; Trace works by substituting trace functions for the display-write/input functions.
;; The trace functions maintain a database of requests sent to the server in the
;; trace-history display property.  This is an alist of (id . byte-vector) where
;; id is the request number for writes, :reply for replies, :event for events and
;; :error for errors.  The alist is kept in reverse order (most recent first)

;; In a multiprocessing system is it very helpful to know what process wrote or
;; read certain requests.  Thus I have modified the format of the trace-history
;; list.  It is now an alist of: ((id . more-info) . byte-vector).
;; (more-info is a list returned by the trace-more-info function).
;; Also added the ability to suspend and resume tracing without destroying the
;; trace history.  Renamed 'display-trace' to 'show-trace' to avoid confusion.
;; 7feb91 -- jdi

;;; Created 09/14/87 by LaMott G. OREN

(in-package :xlib)

(eval-when (load eval)
  (export '(trace-display
	    suspend-display-tracing
	    resume-display-tracing
	    untrace-display
	    show-trace
	    display-trace		; for backwards compatibility
	    describe-request
	    describe-event
	    describe-reply
	    describe-error
	    describe-trace)))

(defun trace-display (display)
  "Start a trace on DISPLAY.
 If display is already being traced, this discards previous history.
 See show-trace and describe-trace."  
  (declare (type display display))
  (unless (getf (display-plist display) 'write-function)
    (bind-io-hooks display))
  (setf (display-trace-history display) nil)
  t)

(defun suspend-display-tracing (display)
  "Tracing is suspended, but history is not cleared."
  (if (getf (display-plist display) 'suspend-display-tracing)
      (warn "Tracing is already suspend for ~s" display)
    (progn
      (unbind-io-hooks display)
      (setf (getf (display-plist display) 'suspend-display-tracing) t))))

(defun resume-display-tracing (display)
  "Used to resume tracing after suspending"
  (if (getf (display-plist display) 'suspend-display-tracing)
      (progn
	(bind-io-hooks display)
	(remf (display-plist display) 'suspend-display-tracing))
    (warn "Tracing was not suspended for ~s" display)))
  
(defun untrace-display (display)
  "Stop tracing DISPLAY."
  (declare (type display display))
  (if (not (getf (display-plist display) 'suspend-display-tracing))
      (unbind-io-hooks display)
    (remf (display-plist display) 'suspend-display-tracing))
  (setf (display-trace-history display) nil))

;; Assumes tracing is not already on.
(defun bind-io-hooks (display)
  (let ((write-function (display-write-function display))
	(input-function (display-input-function display)))
    ;; Save origional write/input functions so we can untrace
    (setf (getf (display-plist display) 'write-function) write-function)
    (setf (getf (display-plist display) 'input-function) input-function)
    ;; Set new write/input functions that will record what's sent to the server
    (setf (display-write-function display)
      #'(lambda (vector display start end)
	  (trace-write-hook vector display start end)
	  (funcall write-function vector display start end)))
    (setf (display-input-function display)
      #'(lambda (display vector start end timeout)
	  (let ((result (funcall input-function
				 display vector start end timeout)))
	    (unless result
	      (trace-read-hook display vector start end))
	    result)))))

(defun unbind-io-hooks (display)
  (let ((write-function (getf (display-plist display) 'write-function))
	(input-function (getf (display-plist display) 'input-function)))
    (when write-function
      (setf (display-write-function display) write-function))
    (when input-function
      (setf (display-input-function display) input-function))
    (remf (display-plist display) 'write-function)
    (remf (display-plist display) 'input-function)))
  

(defun byte-ref16 (vector index)
  #+clx-little-endian
  (logior (the card16
	    (ash (the card8 (aref vector (index+ index 1))) 8))
	  (the card8
	    (aref vector index)))
  #-clx-little-endian
  (logior (the card16
	    (ash (the card8 (aref vector index)) 8))
	  (the card8
	    (aref vector (index+ index 1)))))

(defun byte-ref32 (a i)
  (declare (type buffer-bytes a)
	   (type array-index i))
  (declare (values card32))
  (declare-buffun)
  #+clx-little-endian
  (the card32
       (logior (the card32
		    (ash (the card8 (aref a (index+ i 3))) 24))
	       (the card29
		    (ash (the card8 (aref a (index+ i 2))) 16))
	       (the card16
		    (ash (the card8 (aref a (index+ i 1))) 8))
	       (the card8
		    (aref a i))))
  #-clx-little-endian
  (the card32
       (logior (the card32
		    (ash (the card8 (aref a i)) 24))
	       (the card29
		    (ash (the card8 (aref a (index+ i 1))) 16))
	       (the card16
		    (ash (the card8 (aref a (index+ i 2))) 8))
	       (the card8
		    (aref a (index+ i 3))))))

(defun trace-write-hook (vector display start end)
  ;; Called only by buffer-flush.  Start should always be 0
  (unless (zerop start)
    (format *debug-io* "write-called with non-zero start: ~d" start))
  (let* ((history (display-trace-history display))
	 (request-number (display-request-number display))
	 (last-history (car history)))
    ;; There may be several requests in the buffer, and the last one may be
    ;; incomplete.  The first one may be the completion of a previous request.
    ;; We can detect incomplete requests by comparing the expected length of
    ;; the last request with the actual length.
    (when (and last-history (numberp (caar last-history)))
      (let* ((last-length (index* 4 (byte-ref16 (cdr last-history) 2)))
	     (append-length (min (- last-length (length (cdr last-history)))
				 (- end start))))
	(when (plusp append-length)
	  ;; Last history incomplete - append to last
	  (setf (cdr last-history)
	    (concatenate '(vector card8) (cdr last-history)
			 (subseq vector start (+ start append-length))))
	  (index-incf start append-length))))
    ;; Copy new requests into the history
    (do* ((new-history nil)
	  (i start (+ i length))
	  request
	  length)
	 ((>= i end)
	  ;; add in sequence numbers
	  (dolist (entry new-history)
	    (setf (caar entry) request-number)
	    (decf request-number))
	  (setf (display-trace-history display)
		(nconc new-history history)))
      (setq request (aref vector i))
      (setq length (index* 4 (byte-ref16 vector (+ i 2))))
      (when (zerop length)
	(warn "Zero length in buffer")
	(return nil))
      (push (cons (cons 0 (trace-more-info display request vector
					   i (min (+ i length) end)))
		  (subseq vector i (min (+ i length) end))) new-history)
      (when (zerop request)
	(warn "Zero length in buffer")
	(return nil)))))

(defun trace-read-hook (display vector start end)
  ;; Reading is done with an initial length of 32 (with start = 0)
  ;; This may be followed by several other reads for long replies.
  (let* ((history (display-trace-history display))
	 (last-history (car history))
	 (length (- end start)))
    (when (and history (eq (caar last-history) :reply))
      (let* ((last-length (index+ 32 (index* 4 (byte-ref32 (cdr last-history) 4))))
	     (append-length (min (- last-length (length (cdr last-history)))
				 (- end start))))
	(when (plusp append-length)
	  (setf (cdr last-history)
	    (concatenate '(vector card8) (cdr last-history)
			 (subseq vector start (+ start append-length))))
	  (index-incf start append-length)
	  (index-decf length append-length))))
    
    ;; Copy new requests into the history
    (when (plusp length)
      (let ((reply-type (case (aref vector start) (0 :error) (1 :reply)
			      (otherwise :event))))
	(push (cons (cons reply-type
			  (trace-more-info display reply-type vector start
					   (+ start length)))
		    (subseq vector start (+ start length)))
	    (display-trace-history display))))))

(defun trace-more-info (display request-id vector start end)
  ;; Currently only returns current process.
  #+allegro
  (list mp::*current-process*))


(defun show-trace (display &key length show-process)
  "Display the trace history for DISPLAY.
 The default is to show ALL history entries.
 When the LENGTH parameter is used, only the last LENGTH entries are
 displayed."
  (declare (type display display))
  (dolist (hist (reverse (subseq (display-trace-history display)
				 0 length)))
    (let* ((id (caar hist))
	   (more-info (cdar hist))
	   (vector (cdr hist))
	   (length (length vector))
	   (request (aref vector 0)))
      (format t "~%~5d " id)
      (case id
	(:error
	 (trace-error-print display more-info vector))
	(:event
	 (format t "~a (~d) Sequence ~d"
		 (if (< request (length *event-key-vector*))
		     (aref *event-key-vector* request)
		   "Unknown")
		 request
		 (byte-ref16 vector 2))
	 (when show-process
	   #+allegro
	   (format t ", Proc ~a" (mp::process-name (car more-info)))))
	(:reply
	 (format t "To ~d length ~d"
		 (byte-ref16 vector 2) length)
	 (let ((actual-length (index+ 32 (index* 4 (byte-ref32 vector 4)))))
	   (unless (= length actual-length)
	     (format t " Should be ~d **************" actual-length)))
	 (when show-process
	   #+allegro
	   (format t ", Proc ~a" (mp::process-name (car more-info)))))
	(otherwise
	 (format t "~a (~d) length ~d"
		 (request-name request) request length)
	 (when show-process
	   #+allegro
	   (format t ", Proc ~a" (mp::process-name (car more-info)))))))))

;; For backwards compatibility
(defun display-trace (&rest args)
  (apply 'show-trace args))

(defun find-trace (display type sequence &optional (number 0))
  (dolist (history (display-trace-history display))
    (when (and (symbolp (caar history))
	       (= (logandc2 (aref (cdr history) 0) 128) type)
	       (= (byte-ref16 (cdr history) 2) sequence)
	       (minusp (decf number)))
      (return (cdr history)))))

(defun describe-error (display sequence)
  "Describe the error associated with request SEQUENCE."
  (let ((vector (find-trace display 0 sequence)))
    (if vector
	(progn
	  (terpri)
	  (trace-error-print display nil vector))
      (format t "Error with sequence ~d not found." sequence))))

(defun trace-error-print (display more-info vector
			  &optional (stream *standard-output*))
  (let ((event (allocate-event)))
    ;; Copy into event from reply buffer
    (buffer-replace (reply-ibuf8 event)
		    vector
		    0
		    *replysize*)
    (reading-event (event)
      (let* ((type (read-card8 0))
	     (error-code (read-card8 1))
	     (sequence (read-card16 2))
	     (resource-id (read-card32 4))
	     (minor-code (read-card16 8))
	     (major-code (read-card8 10))
	     (current-sequence (ldb (byte 16 0) (buffer-request-number display)))
	     (error-key
	       (if (< error-code (length *xerror-vector*))
		   (aref *xerror-vector* error-code)
		 'unknown-error))
	     (params
	       (case error-key
		 ((colormap-error cursor-error drawable-error font-error gcontext-error
				  id-choice-error pixmap-error window-error)
		  (list :resource-id resource-id))
		 (atom-error 
		  (list :atom-id resource-id))
		 (value-error
		  (list :value resource-id))
		 (unknown-error
		  ;; Prevent errors when handler is a sequence
		  (setq error-code 0)
		  (list :error-code error-code)))))
	type
	(let ((condition 
		(apply #+lispm #'si:make-condition
		       #+allegro #'make-condition
		       #-(or lispm allegro) #'make-condition
		       error-key
		       :error-key error-key
		       :display display
		       :major major-code
		       :minor minor-code
		       :sequence sequence
		       :current-sequence current-sequence
		       params)))
	  (princ condition stream)
	  (deallocate-event event)
	  condition)))))

(defun describe-request (display sequence)
  "Describe the request with sequence number SEQUENCE"
  #+ti (si:load-if "clx:debug;describe")
  (let ((request (assoc sequence (display-trace-history display)
		       :test #'(lambda (item key)
				 (eql item (car key))))))
    (if (null request)
	(format t "~%Request number ~d not found in trace history" sequence)
      (let* ((vector (cdr request))
	     (len (length vector))
	     (hist (make-reply-buffer len)))
	(buffer-replace (reply-ibuf8 hist) vector 0 len)
	(print-history-description hist)))))

(defun describe-reply (display sequence)
  "Print the reply to request SEQUENCE.
 (The current implementation doesn't print very pretty)"
  (let ((vector (find-trace display 1 sequence))
	(*print-array* t))
    (if vector
	(print vector)
      (format t "~%Reply not found"))))

(defun event-number (name)
  (if (integerp name)
      (let ((name (logandc2 name 128)))
	(if (typep name '(integer 0 63))
	    (aref *event-key-vector* name))
	name)
    (position (string name) *event-key-vector* :test #'equalp :key #'string)))

(defun describe-event (display name sequence &optional (number 0))
  "Describe the event with event-name NAME and sequence number SEQUENCE.
If there is more than one event, return NUMBER in the sequence."
  (declare (type display display)
	   (type (or stringable (integer 0 63)) name)
	   (integer sequence))
  (let* ((event (event-number name))
	 (vector (and event (find-trace display event sequence number))))
    (if (not event)
	(format t "~%~s isn't an event name" name)
      (if (not vector)
	  (if (and (plusp number) (setq vector (find-trace display event sequence 0)))
	      (do ((i 1 (1+ i))
		   (last-vector))
		  (nil)
		(if (setq vector (find-trace display event sequence i))
		    (setq last-vector vector)
		  (progn 
		    (format t "~%Event number ~d not found, last event was ~d"
			    number (1- i))
		    (return (trace-event-print display last-vector)))))
	    (format t "~%Event ~s not found"
		    (aref *event-key-vector* event)))
	(trace-event-print display vector)))))

(defun trace-event-print (display vector)
  (let* ((event (allocate-event))
	 (event-code (ldb (byte 7 0) (aref vector 0)))
	 (event-decoder (aref *event-handler-vector* event-code)))
    ;; Copy into event from reply buffer
    (setf (event-code event) event-code)
    (buffer-replace (reply-ibuf8 event)
		    vector
		    0
		    *replysize*)
    (prog1 (funcall event-decoder display event
		    #'(lambda (&rest args &key send-event-p &allow-other-keys)
			(setq args (copy-list args))
			(remf args :display)
			(remf args :event-code)
			(unless send-event-p (remf args :send-event-p))
			args))
	   (deallocate-event event))))

(defun describe-trace (display &optional length)
  "Display the trace history for DISPLAY.
 The default is to show ALL history entries.
 When the LENGTH parameter is used, only the last LENGTH entries are
 displayed."
  (declare (type display display))
  #+ti (si:load-if "clx:debug;describe")
  (dolist (hist (reverse (subseq (display-trace-history display)
				 0 length)))
    (let* ((id (car hist))
	   (vector (cdr hist))
	   (length (length vector)))
      (format t "~%~5d " id)
      (case id
	(:error
	 (trace-error-print display nil vector))
	(:event
	 (let ((event (trace-event-print display vector)))
	   (when event (format t "from ~d ~{ ~s~}"
			       (byte-ref16 vector 2) event))))
	(:reply
	 (format t "To ~d length ~d"
		 (byte-ref16 vector 2) length)
	 (let ((actual-length (index+ 32 (index* 4 (byte-ref32 vector 4)))))
	   (unless (= length actual-length)
	     (format t " Should be ~d **************" actual-length)))
	 (let ((*print-array* t)
	       (*print-base* 16.))
	   (princ " ")
	   (princ vector)))
	(otherwise
	  (let* ((len (length vector))
		 (hist (make-reply-buffer len)))
	    (buffer-replace (reply-ibuf8 hist) vector 0 len)
	    (print-history-description hist)))))))

;; End of file
