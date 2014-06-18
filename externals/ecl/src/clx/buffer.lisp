;;; -*- Mode: LISP; Syntax: Common-lisp; Package: XLIB; Base: 10; Lowercase: Yes -*-

;;; This file contains definitions for the BUFFER object for Common-Lisp X
;;; windows version 11

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

;; A few notes:
;;
;;  1. The BUFFER implements a two-way buffered byte / half-word
;;     / word stream.  Hooks are left for implementing this with a
;;     shared memory buffer, or with effenciency hooks to the network
;;     code.
;;
;;  2. The BUFFER object uses overlapping displaced arrays for
;;     inserting and removing bytes half-words and words.
;;
;;  3. The BYTE component of these arrays is written to a STREAM
;;     associated with the BUFFER.  The stream has its own buffer.
;;     This may be made more efficient by using the Zetalisp
;;     :Send-Output-Buffer operation.
;;
;;  4. The BUFFER object is INCLUDED in the DISPLAY object.
;;     This was done to reduce access time when sending requests,
;;     while maintaing some code modularity.
;;     Several buffer functions are duplicated (with-buffer,
;;     buffer-force-output, close-buffer) to keep the naming
;;     conventions consistent.
;;
;;  5. A nother layer of software is built on top of this for generating
;;     both client and server interface routines, given a specification
;;     of the protocol. (see the INTERFACE file)
;;
;;  6. Care is taken to leave the buffer pointer (buffer-bbuf) set to
;;     a point after a complete request.  This is to ensure that a partial
;;     request won't be left after aborts (e.g. control-abort on a lispm).

(in-package :xlib)

(defconstant +requestsize+ 160) ;; Max request size (excluding variable length requests)

;;; This is here instead of in bufmac so that with-display can be
;;; compiled without macros and bufmac being loaded.

(defmacro with-buffer ((buffer &key timeout inline)
		       &body body &environment env)
  ;; This macro is for use in a multi-process environment.  It provides
  ;; exclusive access to the local buffer object for request generation and
  ;; reply processing.
  `(macrolet ((with-buffer ((buffer &key timeout) &body body)
		;; Speedup hack for lexically nested with-buffers
		`(progn
		   (progn ,buffer ,@(and timeout `(,timeout)) nil)
		   ,@body)))
     ,(if (and (null inline) (macroexpand '(use-closures) env))
	  `(flet ((.with-buffer-body. () ,@body))
	     #+clx-ansi-common-lisp
	     (declare (dynamic-extent #'.with-buffer-body.))
	     (with-buffer-function ,buffer ,timeout #'.with-buffer-body.))
	(let ((buf (if (or (symbolp buffer) (constantp buffer))
		       buffer
		     '.buffer.)))
	  `(let (,@(unless (eq buf buffer) `((,buf ,buffer))))
	     ,@(unless (eq buf buffer) `((declare (type buffer ,buf))))
	     ,(declare-bufmac)
	     (when (buffer-dead ,buf)
	       (x-error 'closed-display :display ,buf))
	     (holding-lock ((buffer-lock ,buf) ,buf "CLX Display Lock"
			    ,@(and timeout `(:timeout ,timeout)))
	       ,@body))))))

(defun with-buffer-function (buffer timeout function)
  (declare (type display buffer)
	   (type (or null number) timeout)
	   (type function function)
	   #+clx-ansi-common-lisp
	   (dynamic-extent function)
	   ;; FIXME: This is probably more a bug in SBCL (logged as
	   ;; bug #243)
	   (ignorable timeout)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg function))
  (with-buffer (buffer :timeout timeout :inline t)
    (funcall function)))

;;; The following are here instead of in bufmac so that event-case can
;;; be compiled without macros and bufmac being loaded.

(defmacro read-card8 (byte-index)
  `(aref-card8 buffer-bbuf (index+ buffer-boffset ,byte-index)))

(defmacro read-int8 (byte-index)
  `(aref-int8 buffer-bbuf (index+ buffer-boffset ,byte-index)))

(defmacro read-card16 (byte-index)
  #+clx-overlapping-arrays
  `(aref-card16 buffer-wbuf (index+ buffer-woffset (index-ash ,byte-index -1)))
  #-clx-overlapping-arrays
  `(aref-card16 buffer-bbuf (index+ buffer-boffset ,byte-index)))

(defmacro read-int16 (byte-index)
  #+clx-overlapping-arrays
  `(aref-int16 buffer-wbuf (index+ buffer-woffset (index-ash ,byte-index -1)))
  #-clx-overlapping-arrays
  `(aref-int16 buffer-bbuf (index+ buffer-boffset ,byte-index)))

(defmacro read-card32 (byte-index)
  #+clx-overlapping-arrays
  `(aref-card32 buffer-lbuf (index+ buffer-loffset (index-ash ,byte-index -2)))
  #-clx-overlapping-arrays
  `(aref-card32 buffer-bbuf (index+ buffer-boffset ,byte-index)))

(defmacro read-int32 (byte-index)
  #+clx-overlapping-arrays
  `(aref-int32 buffer-lbuf (index+ buffer-loffset (index-ash ,byte-index -2)))
  #-clx-overlapping-arrays
  `(aref-int32 buffer-bbuf (index+ buffer-boffset ,byte-index)))

(defmacro read-card29 (byte-index)
  #+clx-overlapping-arrays
  `(aref-card29 buffer-lbuf (index+ buffer-loffset (index-ash ,byte-index -2)))
  #-clx-overlapping-arrays
  `(aref-card29 buffer-bbuf (index+ buffer-boffset ,byte-index)))

(defmacro event-code (reply-buffer)
  ;; The reply-buffer structure is used for events.
  ;; The size slot is used for the event code.
  `(reply-size ,reply-buffer))

(defmacro reading-event ((event &rest options) &body body)
  (declare (arglist (buffer &key sizes) &body body))
  ;; BODY may contain calls to (READ32 &optional index) etc.
  ;; These calls will read from the input buffer at byte
  ;; offset INDEX.  If INDEX is not supplied, then the next
  ;; word, half-word or byte is returned.
  `(with-buffer-input (,event ,@options) ,@body))

(defmacro with-buffer-input ((reply-buffer &key display (sizes '(8 16 32)) index)
			     &body body)
  (unless (listp sizes) (setq sizes (list sizes)))
  ;; 160 is a special hack for client-message-events
  (when (set-difference sizes '(0 8 16 32 160 256))
    (error "Illegal sizes in ~a" sizes))
  `(let ((%reply-buffer ,reply-buffer)
	 ,@(and display `((%buffer ,display))))
     (declare (type reply-buffer %reply-buffer)
	      ,@(and display '((type display %buffer))))
     ,(declare-bufmac)
     ,@(and display '(%buffer))
     (let* ((buffer-boffset (the array-index ,(or index 0)))
	    #-clx-overlapping-arrays
	    (buffer-bbuf (reply-ibuf8 %reply-buffer))
	    #+clx-overlapping-arrays
	    ,@(append
		(when (member 8 sizes)
		  `((buffer-bbuf (reply-ibuf8 %reply-buffer))))
		(when (or (member 16 sizes) (member 160 sizes))
		  `((buffer-woffset (index-ash buffer-boffset -1))
		    (buffer-wbuf (reply-ibuf16 %reply-buffer))))
		(when (member 32 sizes)
		  `((buffer-loffset (index-ash buffer-boffset -2))
		    (buffer-lbuf (reply-ibuf32 %reply-buffer))))))
       (declare (type array-index buffer-boffset))
       #-clx-overlapping-arrays
       (declare (type buffer-bytes buffer-bbuf))
       #+clx-overlapping-arrays
       ,@(append
	   (when (member 8 sizes)
	     '((declare (type buffer-bytes buffer-bbuf))))
	   (when (member 16 sizes)
	     '((declare (type array-index buffer-woffset))
	       (declare (type buffer-words buffer-wbuf))))
	   (when (member 32 sizes)
	     '((declare (type array-index buffer-loffset))
	       (declare (type buffer-longs buffer-lbuf)))))
       buffer-boffset
       #-clx-overlapping-arrays
       buffer-bbuf
       #+clx-overlapping-arrays
       ,@(append
	   (when (member 8  sizes) '(buffer-bbuf))
	   (when (member 16 sizes) '(buffer-woffset buffer-wbuf))
	   (when (member 32 sizes) '(buffer-loffset buffer-lbuf)))
       #+clx-overlapping-arrays
       (macrolet ((%buffer-sizes () ',sizes))
	 ,@body)
       #-clx-overlapping-arrays
       ,@body)))

(defun make-buffer (output-size constructor &rest options)
  (declare (dynamic-extent options))
  ;; Output-Size is the output-buffer size in bytes.
  (let ((byte-output (make-array output-size :element-type 'card8
				 :initial-element 0)))
    (apply constructor
	   :size output-size
	   :obuf8 byte-output
	   #+clx-overlapping-arrays
	   :obuf16
	   #+clx-overlapping-arrays
	   (make-array (index-ash output-size -1)
		       :element-type 'overlap16
		       :displaced-to byte-output)
	   #+clx-overlapping-arrays
	   :obuf32
	   #+clx-overlapping-arrays
	   (make-array (index-ash output-size -2)
		       :element-type 'overlap32
		       :displaced-to byte-output)
	   options))) 

(defun make-reply-buffer (size)
  ;; Size is the buffer size in bytes
  (let ((byte-input (make-array size :element-type 'card8
				:initial-element 0)))
    (make-reply-buffer-internal
      :size size
      :ibuf8 byte-input
      #+clx-overlapping-arrays
      :ibuf16
      #+clx-overlapping-arrays
      (make-array (index-ash size -1)
		  :element-type 'overlap16
		  :displaced-to byte-input)
      #+clx-overlapping-arrays
      :ibuf32
      #+clx-overlapping-arrays
      (make-array (index-ash size -2)
		  :element-type 'overlap32
		  :displaced-to byte-input))))

(defun buffer-ensure-size (buffer size)
  (declare (type buffer buffer)
	   (type array-index size))
  (when (index> size (buffer-size buffer))
    (with-buffer (buffer)
      (buffer-flush buffer)
      (let* ((new-buffer-size (index-ash 1 (integer-length (index1- size))))
	     (new-buffer (make-array new-buffer-size :element-type 'card8
				     :initial-element 0)))
	(setf (buffer-obuf8 buffer) new-buffer)
	#+clx-overlapping-arrays
	(setf (buffer-obuf16 buffer)
	      (make-array (index-ash new-buffer-size -1)
			  :element-type 'overlap16
			  :displaced-to new-buffer)
	      (buffer-obuf32 buffer)
	      (make-array (index-ash new-buffer-size -2)
			  :element-type 'overlap32
			  :displaced-to new-buffer))))))

(defun buffer-pad-request (buffer pad)
  (declare (type buffer buffer)
	   (type array-index pad))
  (unless (index-zerop pad)
    (when (index> (index+ (buffer-boffset buffer) pad)
		  (buffer-size buffer))
      (buffer-flush buffer))
    (incf (buffer-boffset buffer) pad)
    (unless (index-zerop (index-mod (buffer-boffset buffer) 4))
      (buffer-flush buffer))))

(declaim (inline buffer-new-request-number))

(defun buffer-new-request-number (buffer)
  (declare (type buffer buffer))
  (setf (buffer-request-number buffer)
	(ldb (byte 16 0) (1+ (buffer-request-number buffer)))))

(defun with-buffer-request-function (display gc-force request-function)
  (declare (type display display)
	   (type (or null gcontext) gc-force))
  (declare (type function request-function)
	   #+clx-ansi-common-lisp
	   (dynamic-extent request-function)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg request-function))
  (with-buffer (display :inline t)
    (multiple-value-prog1
      (progn
	(when gc-force (force-gcontext-changes-internal gc-force))
	(without-aborts (funcall request-function display)))
      (display-invoke-after-function display))))

(defun with-buffer-request-function-nolock (display gc-force request-function)
  (declare (type display display)
	   (type (or null gcontext) gc-force))
  (declare (type function request-function)
	   #+clx-ansi-common-lisp
	   (dynamic-extent request-function)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg request-function))
  (multiple-value-prog1
    (progn
      (when gc-force (force-gcontext-changes-internal gc-force))
      (without-aborts (funcall request-function display)))
    (display-invoke-after-function display)))

(defstruct (pending-command (:copier nil) (:predicate nil))
  (sequence 0 :type card16)
  (reply-buffer nil :type (or null reply-buffer))
  (process nil)
  (next nil #-explorer :type #-explorer (or null pending-command)))

(defun with-buffer-request-and-reply-function
       (display multiple-reply request-function reply-function)
  (declare (type display display)
	   (type generalized-boolean multiple-reply))
  (declare (type function request-function reply-function)
	   #+clx-ansi-common-lisp
	   (dynamic-extent request-function reply-function)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg request-function reply-function))
  (let ((pending-command nil)
	(reply-buffer nil))
    (declare (type (or null pending-command) pending-command)
	     (type (or null reply-buffer) reply-buffer))
    (unwind-protect
	(progn 
	  (with-buffer (display :inline t)
	    (setq pending-command (start-pending-command display))
	    (without-aborts (funcall request-function display))
	    (buffer-force-output display)
	    (display-invoke-after-function display))
	  (cond (multiple-reply
		 (loop
		   (setq reply-buffer (read-reply display pending-command))
		   (when (funcall reply-function display reply-buffer) (return nil))
		   (deallocate-reply-buffer (shiftf reply-buffer nil))))
		(t
		 (setq reply-buffer (read-reply display pending-command))
		 (funcall reply-function display reply-buffer))))
      (when reply-buffer (deallocate-reply-buffer reply-buffer))
      (when pending-command (stop-pending-command display pending-command)))))

;;
;; Buffer stream operations
;;

(defun buffer-write (vector buffer start end)
  ;; Write out VECTOR from START to END into BUFFER
  ;; Internal function, MUST BE CALLED FROM WITHIN WITH-BUFFER
  (declare (type buffer buffer)
	   (type array-index start end))
  (when (buffer-dead buffer)
    (x-error 'closed-display :display buffer))
  (wrap-buf-output (buffer)
    (funcall (buffer-write-function buffer) vector buffer start end))
  nil)

(defun buffer-flush (buffer)
  ;; Write the buffer contents to the server stream - doesn't force-output the stream
  ;; Internal function, MUST BE CALLED FROM WITHIN WITH-BUFFER
  (declare (type buffer buffer))
  (unless (buffer-flush-inhibit buffer)
    (let ((boffset (buffer-boffset buffer)))
      (declare (type array-index boffset))
      (when (index-plusp boffset)
	(buffer-write (buffer-obuf8 buffer) buffer 0 boffset)
	(setf (buffer-boffset buffer) 0)
	(setf (buffer-last-request buffer) nil))))
  nil)

(defmacro with-buffer-flush-inhibited ((buffer) &body body)
  (let ((buf (if (or (symbolp buffer) (constantp buffer)) buffer '.buffer.)))
    `(let* (,@(and (not (eq buf buffer)) `((,buf ,buffer)))
	    (.saved-buffer-flush-inhibit. (buffer-flush-inhibit ,buf)))
       (unwind-protect
	   (progn
	     (setf (buffer-flush-inhibit ,buf) t)
	     ,@body)
	 (setf (buffer-flush-inhibit ,buf) .saved-buffer-flush-inhibit.)))))

(defun buffer-force-output (buffer)
  ;; Output is normally buffered, this forces any buffered output to the server.
  (declare (type buffer buffer))
  (when (buffer-dead buffer)
    (x-error 'closed-display :display buffer))
  (buffer-flush buffer)
  (wrap-buf-output (buffer)
    (without-aborts
      (funcall (buffer-force-output-function buffer) buffer)))
  nil)

(defun close-buffer (buffer &key abort)
  ;; Close the host connection in BUFFER
  (declare (type buffer buffer))
  (unless (null (buffer-output-stream buffer))
    (wrap-buf-output (buffer)
      (funcall (buffer-close-function buffer) buffer :abort abort))
    (setf (buffer-dead buffer) t)
    ;; Zap pointers to the streams, to ensure they're GC'd
    (setf (buffer-output-stream buffer) nil)
    (setf (buffer-input-stream buffer) nil)
    )
  nil)

(defun buffer-input  (buffer vector start end &optional timeout)
  ;; Read into VECTOR from the buffer stream
  ;; Timeout, when non-nil, is in seconds
  ;; Returns non-nil if EOF encountered
  ;; Returns :TIMEOUT when timeout exceeded
  (declare (type buffer buffer)
	   (type vector vector)
	   (type array-index start end)
	   (type (or null number) timeout))
  (declare (clx-values eof-p))
  (when (buffer-dead buffer)
    (x-error 'closed-display :display buffer))
  (unless (= start end)
    (let ((result
	    (wrap-buf-input (buffer)
	      (funcall (buffer-input-function buffer)
		       buffer vector start end timeout))))
      (unless (or (null result) (eq result :timeout))
	(close-buffer buffer))
      result)))

(defun buffer-input-wait  (buffer timeout)
  ;; Timeout, when non-nil, is in seconds
  ;; Returns non-nil if EOF encountered
  ;; Returns :TIMEOUT when timeout exceeded
  (declare (type buffer buffer)
	   (type (or null number) timeout))
  (declare (clx-values timeout))
  (when (buffer-dead buffer)
    (x-error 'closed-display :display buffer))
  (let ((result
	  (wrap-buf-input (buffer)
	    (funcall (buffer-input-wait-function buffer)
		     buffer timeout))))
    (unless (or (null result) (eq result :timeout))
      (close-buffer buffer))
    result))

(defun buffer-listen (buffer)
  ;; Returns T if there is input available for the buffer. This should never
  ;; block, so it can be called from the scheduler.
  (declare (type buffer buffer))
  (declare (clx-values input-available))
  (or (not (null (buffer-dead buffer)))
      (wrap-buf-input (buffer)
	(funcall (buffer-listen-function buffer) buffer))))

;;; Reading sequences of strings

;;; a list of pascal-strings with card8 lengths, no padding in between
;;; can't use read-sequence-char
(defun read-sequence-string (buffer-bbuf length nitems result-type
			     &optional (buffer-boffset 0))
  (declare (type buffer-bytes buffer-bbuf)
	   (type array-index length nitems buffer-boffset))
  length
  (with-vector (buffer-bbuf buffer-bytes)
    (let ((result (make-sequence result-type nitems)))
      (do* ((index 0 (index+ index 1 string-length))
	    (count 0 (index1+ count))
	    (string-length 0)
	    (string ""))
	   ((index>= count nitems)
	    result)
	(declare (type array-index index count string-length)
		 (type string string))
	(setq string-length (read-card8 index)
	      string (make-sequence 'string string-length))
	(do ((i (index1+ index) (index1+ i))
	     (j 0 (index1+ j)))
	    ((index>= j string-length)
	     (setf (elt result count) string))
	  (declare (type array-index i j))
	  (setf (aref string j) (card8->char (read-card8 i))))))))

;;; Reading sequences of chars

(defmacro define-transformed-sequence-reader (name totype transformer reader)
  (let ((ntrans (gensym)))
    `(defun ,name (reply-buffer result-type nitems &optional transform data (start 0) (index 0))
      (declare 
       (type reply-buffer reply-buffer)
       (type t result-type)
       (type array-index nitems start index)
       (type (or null sequence) data)
       (type (or null (function (,totype) t)) transform)
       #+clx-ansi-common-lisp (dynamic-extent transform)
       #+(and lispm (not clx-ansi-common-lisp)) (sys:downward-funarg transform))
      (if transform
	  (flet ((,ntrans (v) (funcall transform (,transformer v))))
	    #+clx-ansi-common-lisp (declare (dynamic-extent #',ntrans))
	    (,reader reply-buffer result-type nitems #',ntrans data start index))
	  (,reader reply-buffer result-type nitems #',transformer data start index)))))

(define-transformed-sequence-reader read-sequence-char character 
  card8->char read-sequence-card8)

;;; Reading sequences of card8's

(defmacro define-list-readers ((name tname) type size step reader)
  `(progn 
    (defun ,name (reply-buffer nitems data start index)
      (declare (type reply-buffer reply-buffer)
	       (type array-index nitems start index)
	       (type list data))
      (with-buffer-input (reply-buffer :sizes (,size) :index index)
	(do* ((j nitems (index- j 1))
	      (list (nthcdr start data) (cdr list))
	      (index 0 (index+ index ,step)))
	     ((index-zerop j))
	  (declare (type array-index index j) (type list list))
	  (setf (car list) (,reader index)))))
    (defun ,tname (reply-buffer nitems data transform start index)
      (declare (type reply-buffer reply-buffer)
	       (type array-index nitems start index)
	       (type list data)
	       (type (function (,type) t) transform)
	       #+clx-ansi-common-lisp (dynamic-extent transform)
	       #+(and lispm (not clx-ansi-common-lisp)) (sys:downward-funarg transform))
      (with-buffer-input (reply-buffer :sizes (,size) :index index)
	(do* ((j nitems (index- j 1))
	      (list (nthcdr start data) (cdr list))
	      (index 0 (index+ index ,step)))
	     ((index-zerop j))
	  (declare (type array-index index j) (type list list))
	  (setf (car list) (funcall transform (,reader index))))))))

(define-list-readers (read-list-card8 read-list-card8-with-transform) card8
  8 1 read-card8)

#-lispm
(defun read-simple-array-card8 (reply-buffer nitems data start index)
  (declare (type reply-buffer reply-buffer)
	   (type array-index nitems start index)
	   (type (simple-array card8 (*)) data))
  (with-vector (data (simple-array card8 (*)))
    (with-buffer-input (reply-buffer :sizes (8))
      (buffer-replace data buffer-bbuf start (index+ start nitems) index))))

#-lispm
(defun read-simple-array-card8-with-transform (reply-buffer nitems data transform start index)
  (declare (type reply-buffer reply-buffer)
	   (type array-index nitems start index)
	   (type (simple-array card8 (*)) data))
  (declare (type (function (card8) card8) transform)
	   #+clx-ansi-common-lisp
	   (dynamic-extent transform)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg transform))
  (with-vector (data (simple-array card8 (*)))
    (with-buffer-input (reply-buffer :sizes (8) :index index)
      (do* ((j start (index+ j 1))
	    (end (index+ start nitems))
	    (index 0 (index+ index 1)))
	   ((index>= j end))
	(declare (type array-index j end index))
	(setf (aref data j) (the card8 (funcall transform (read-card8 index))))))))

(defun read-vector-card8 (reply-buffer nitems data start index)
  (declare (type reply-buffer reply-buffer)
	   (type array-index nitems start index)
	   (type vector data)
	   (optimize #+cmu(ext:inhibit-warnings 3)))
  (with-vector (data vector)
    (with-buffer-input (reply-buffer :sizes (8) :index index)
      (do* ((j start (index+ j 1))
	    (end (index+ start nitems))
	    (index 0 (index+ index 1)))
	   ((index>= j end))
	(declare (type array-index j end index))
	(setf (aref data j) (read-card8 index))))))

(defun read-vector-card8-with-transform (reply-buffer nitems data transform start index)
  (declare (type reply-buffer reply-buffer)
	   (type array-index nitems start index)
	   (type vector data)
	   (optimize #+cmu(ext:inhibit-warnings 3)))
  (declare (type (function (card8) t) transform)
	   #+clx-ansi-common-lisp
	   (dynamic-extent transform)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg transform))
  (with-vector (data vector)
    (with-buffer-input (reply-buffer :sizes (8) :index index)
      (do* ((j start (index+ j 1))
	    (end (index+ start nitems))
	    (index 0 (index+ index 1)))
	   ((index>= j end))
	(declare (type array-index j end index))
	(setf (aref data j) (funcall transform (read-card8 index)))))))

(defmacro define-sequence-reader (name type (list tlist) (sa tsa) (vec tvec))
  `(defun ,name (reply-buffer result-type nitems &optional transform data (start 0) (index 0))
    (declare
     (type reply-buffer reply-buffer)
     (type t result-type)
     (type array-index nitems start index)
     (type (or null sequence) data)
     (type (or null (function (,type) t)) transform)
     #+clx-ansi-common-lisp (dynamic-extent transform)
     #+(and lispm (not clx-ansi-common-lisp)) (sys:downward-funarg transform))
    (let ((result (or data (make-sequence result-type nitems))))
      (typecase result
	(list
	 (if transform
	     (,tlist reply-buffer nitems result transform start index)
	     (,list reply-buffer nitems result start index)))
	#-lispm
	((simple-array ,type (*))
	 (if transform
	     (,tsa reply-buffer nitems result transform start index)
	     (,sa reply-buffer nitems result start index)))
	;; FIXME: general sequences
	(t 
	 (if transform
	     (,tvec reply-buffer nitems result transform start index)
	     (,vec reply-buffer nitems result start index))))
      result)))

(define-sequence-reader read-sequence-card8 card8 
  (read-list-card8 read-list-card8-with-transform)
  (read-simple-array-card8 read-simple-array-card8-with-transform)
  (read-vector-card8 read-vector-card8-with-transform))

(define-transformed-sequence-reader read-sequence-int8 int8
  card8->int8 read-sequence-card8)

;;; Reading sequences of card16's

(define-list-readers (read-list-card16 read-list-card16-with-transform) card16
  16 2 read-card16)

#-lispm
(defun read-simple-array-card16 (reply-buffer nitems data start index)
  (declare (type reply-buffer reply-buffer)
	   (type array-index nitems start index)
	   (type (simple-array card16 (*)) data))
  (with-vector (data (simple-array card16 (*)))
    (with-buffer-input (reply-buffer :sizes (16) :index index)
      #-clx-overlapping-arrays
      (do* ((j start (index+ j 1))
	    (end (index+ start nitems))
	    (index 0 (index+ index 2)))
	   ((index>= j end))
	(declare (type array-index j end index))
	(setf (aref data j) (the card16 (read-card16 index))))
      #+clx-overlapping-arrays
      (buffer-replace data buffer-wbuf start (index+ start nitems) (index-floor index 2)))))

#-lispm
(defun read-simple-array-card16-with-transform (reply-buffer nitems data transform start index)
  (declare (type reply-buffer reply-buffer)
	   (type array-index nitems start index)
	   (type (simple-array card16 (*)) data))
  (declare (type (function (card16) card16) transform)
	   #+clx-ansi-common-lisp
	   (dynamic-extent transform)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg transform))
  (with-vector (data (simple-array card16 (*)))
    (with-buffer-input (reply-buffer :sizes (16) :index index)
      (do* ((j start (index+ j 1))
	    (end (index+ start nitems))
	    (index 0 (index+ index 2)))
	   ((index>= j end))
	(declare (type array-index j end index))
	(setf (aref data j) (the card16 (funcall transform (read-card16 index))))))))

(defun read-vector-card16 (reply-buffer nitems data start index)
  (declare (type reply-buffer reply-buffer)
	   (type array-index nitems start index)
	   (type vector data)
	   (optimize #+cmu(ext:inhibit-warnings 3)))
  (with-vector (data vector)
    (with-buffer-input (reply-buffer :sizes (16) :index index)
      #-clx-overlapping-arrays
      (do* ((j start (index+ j 1))
	    (end (index+ start nitems))
	    (index 0 (index+ index 2)))
	   ((index>= j end))
	(declare (type array-index j end index))
	(setf (aref data j) (read-card16 index)))
      #+clx-overlapping-arrays
      (buffer-replace data buffer-wbuf start (index+ start nitems) (index-floor index 2)))))

(defun read-vector-card16-with-transform (reply-buffer nitems data transform start index)
  (declare (type reply-buffer reply-buffer)
	   (type array-index nitems start index)
	   (type vector data)
	   (optimize #+cmu(ext:inhibit-warnings 3)))
  (declare (type (function (card16) t) transform)
	   #+clx-ansi-common-lisp
	   (dynamic-extent transform)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg transform))
  (with-vector (data vector)
    (with-buffer-input (reply-buffer :sizes (16) :index index)
      (do* ((j start (index+ j 1))
	    (end (index+ start nitems))
	    (index 0 (index+ index 2)))
	   ((index>= j end))
	(declare (type array-index j end index))
	(setf (aref data j) (funcall transform (read-card16 index)))))))

(define-sequence-reader read-sequence-card16 card16
  (read-list-card16 read-list-card16-with-transform)
  (read-simple-array-card16 read-simple-array-card16-with-transform)
  (read-vector-card16 read-vector-card16-with-transform))

(define-transformed-sequence-reader read-sequence-int16 int16
  card16->int16 read-sequence-card16)

;;; Reading sequences of card32's

(define-list-readers (read-list-card32 read-list-card32-with-transform) card32
  32 4 read-card32)

#-lispm
(defun read-simple-array-card32 (reply-buffer nitems data start index)
  (declare (type reply-buffer reply-buffer)
	   (type array-index nitems start index)
	   (type (simple-array card32 (*)) data))
  (with-vector (data (simple-array card32 (*)))
    (with-buffer-input (reply-buffer :sizes (32) :index index)
      #-clx-overlapping-arrays
      (do* ((j start (index+ j 1))
	    (end (index+ start nitems))
	    (index 0 (index+ index 4)))
	   ((index>= j end))
	(declare (type array-index j end index))
	(setf (aref data j) (the card32 (read-card32 index))))
      #+clx-overlapping-arrays
      (buffer-replace data buffer-lbuf start (index+ start nitems) (index-floor index 4)))))

#-lispm
(defun read-simple-array-card32-with-transform (reply-buffer nitems data transform start index)
  (declare (type reply-buffer reply-buffer)
	   (type array-index nitems start index)
	   (type (simple-array card32 (*)) data))
  (declare (type (function (card32) card32) transform)
	   #+clx-ansi-common-lisp
	   (dynamic-extent transform)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg transform))
  (with-vector (data (simple-array card32 (*)))
    (with-buffer-input (reply-buffer :sizes (32) :index index)
      (do* ((j start (index+ j 1))
	    (end (index+ start nitems))
	    (index 0 (index+ index 4)))
	   ((index>= j end))
	(declare (type array-index j end index))
	(setf (aref data j) (the card32 (funcall transform (read-card32 index))))))))

(defun read-vector-card32 (reply-buffer nitems data start index)
  (declare (type reply-buffer reply-buffer)
	   (type array-index nitems start index)
	   (type vector data)
	   (optimize #+cmu(ext:inhibit-warnings 3)))
  (with-vector (data vector)
    (with-buffer-input (reply-buffer :sizes (32) :index index)
      #-clx-overlapping-arrays
      (do* ((j start (index+ j 1))
	    (end (index+ start nitems))
	    (index 0 (index+ index 4)))
	   ((index>= j end))
	(declare (type array-index j end index))
	(setf (aref data j) (read-card32 index)))
      #+clx-overlapping-arrays
      (buffer-replace data buffer-lbuf start (index+ start nitems) (index-floor index 4)))))

(defun read-vector-card32-with-transform (reply-buffer nitems data transform start index)
  (declare (type reply-buffer reply-buffer)
	   (type array-index nitems start index)
	   (type vector data)
	   (optimize #+cmu(ext:inhibit-warnings 3)))
  (declare (type (function (card32) t) transform)
	   #+clx-ansi-common-lisp
	   (dynamic-extent transform)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg transform))
  (with-vector (data vector)
    (with-buffer-input (reply-buffer :sizes (32) :index index)
      (do* ((j start (index+ j 1))
	    (end (index+ start nitems))
	    (index 0 (index+ index 4)))
	   ((index>= j end))
	(declare (type array-index j end index))
	(setf (aref data j) (funcall transform (read-card32 index)))))))

(define-sequence-reader read-sequence-card32 card32
  (read-list-card32 read-list-card32-with-transform)
  (read-simple-array-card32 read-simple-array-card32-with-transform)
  (read-vector-card32 read-vector-card32-with-transform))

(define-transformed-sequence-reader read-sequence-int32 int32 
  card32->int32 read-sequence-card32)

;;; Writing sequences of chars

(defmacro define-transformed-sequence-writer (name fromtype transformer writer)
  (let ((ntrans (gensym)))
    `(defun ,name (buffer boffset data &optional (start 0) (end (length data)) transform)
      (declare 
       (type buffer buffer)
       (type sequence data)
       (type array-index boffset start end)
       (type (or null (function (t) ,fromtype)) transform)
       #+clx-ansi-common-lisp (dynamic-extent transform)
       #+(and lispm (not clx-ansi-common-lisp)) (sys:downward-funarg transform))
      (if transform
	  (flet ((,ntrans (x) (,transformer (the ,fromtype (funcall transform x)))))
	    #+clx-ansi-common-lisp (declare (dynamic-extent #',ntrans))
	    (,writer buffer boffset data start end #',ntrans))
	  (,writer buffer boffset data start end #',transformer)))))

(define-transformed-sequence-writer write-sequence-char character 
  char->card8 write-sequence-card8)

;;; Writing sequences of card8's

(defmacro define-list-writers ((name tname) type step writer)
  `(progn
    (defun ,name (buffer boffset data start end)
      (declare 
       (type buffer buffer)
       (type list data)
       (type array-index boffset start end))
      (writing-buffer-chunks ,type
	  ((list (nthcdr start data)))
	  ((type list list))
	(do ((j 0 (index+ j ,step)))
	    ((index>= j chunk))
	  (declare (type array-index j))
	  (,writer j (pop list)))))
    (defun ,tname (buffer boffset data start end transform)
      (declare 
       (type buffer buffer)
       (type list data)
       (type array-index boffset start end)
       (type (function (t) ,type) transform)
       #+clx-ansi-common-lisp (dynamic-extent transform)
       #+(and lispm (not clx-ansi-common-lisp)) (sys:downward-funarg transform))
      (writing-buffer-chunks ,type
	  ((list (nthcdr start data)))
	  ((type list list))
	(do ((j 0 (index+ j ,step)))
	    ((index>= j chunk))
	  (declare (type array-index j))
	  (,writer j (funcall transform (pop list))))))))

;;; original CLX comment: "TI Compiler bug", in WRITE-LIST-CARD8
#+ti
(progn
  (defun write-list-card8 (buffer boffset data start end)
    (writing-buffer-chunks card8
	((list (nthcdr start data)))
	((type list list))
      (dotimes (j chunk)
	(setf (aref buffer-bbuf (index+ buffer-boffset j)) (pop list)))))
  (defun write-list-card8-with-transform (buffer boffset data start end transform)
    (writing-buffer-chunks card8
	((list (nthcdr start data)))
	((type list lst))
      (dotimes (j chunk)
	(declare (type array-index j))
	(write-card8 j (funcall transform (pop lst)))))))

#-ti
(define-list-writers (write-list-card8 write-list-card8-with-transform) card8
  1 write-card8)

;;; Should really write directly from data, instead of into the buffer first
#-lispm
(defun write-simple-array-card8 (buffer boffset data start end)
  (declare (type buffer buffer)
	   (type (simple-array card8 (*)) data)
	   (type array-index boffset start end))
  (with-vector (data (simple-array card8 (*)))
    (writing-buffer-chunks card8
			   ((index start (index+ index chunk)))
			   ((type array-index index))
      (buffer-replace buffer-bbuf data
		      buffer-boffset
		      (index+ buffer-boffset chunk)
		      index)))
  nil)

#-lispm
(defun write-simple-array-card8-with-transform (buffer boffset data start end transform)
  (declare (type buffer buffer)
	   (type (simple-array card8 (*)) data)
	   (type array-index boffset start end))
  (declare (type (function (card8) card8) transform)
	   #+clx-ansi-common-lisp
	   (dynamic-extent transform)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg transform))
  (with-vector (data (simple-array card8 (*)))
    (writing-buffer-chunks card8
			   ((index start))
			   ((type array-index index))
      (dotimes (j chunk)
	(declare (type array-index j))
	(write-card8 j (funcall transform (aref data index)))
	(setq index (index+ index 1)))))
  nil)

(defun write-vector-card8 (buffer boffset data start end)
  (declare (type buffer buffer)
	   (type vector data)
	   (type array-index boffset start end)
	   (optimize #+cmu(ext:inhibit-warnings 3)))
  (with-vector (data vector)
    (writing-buffer-chunks card8
			   ((index start))
			   ((type array-index index))
      (dotimes (j chunk)
	(declare (type array-index j))
	(write-card8 j (aref data index))
	(setq index (index+ index 1)))))
  nil)

(defun write-vector-card8-with-transform (buffer boffset data start end transform)
  (declare (type buffer buffer)
	   (type vector data)
	   (type array-index boffset start end))
  (declare (type (function (t) card8) transform)
	   #+clx-ansi-common-lisp
	   (dynamic-extent transform)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg transform))
  (with-vector (data vector)
    (writing-buffer-chunks card8
			   ((index start))
			   ((type array-index index))
      (dotimes (j chunk)
	(declare (type array-index j))
	(write-card8 j (funcall transform (aref data index)))
	(setq index (index+ index 1)))))
  nil)

(defmacro define-sequence-writer (name type (list tlist) (sa tsa) (vec tvec))
  `(defun ,name (buffer boffset data &optional (start 0) (end (length data)) transform)
    (declare
     (type buffer buffer)
     (type sequence data)
     (type array-index boffset start end)
     (type (or null (function (t) ,type)) transform)
     #+clx-ansi-common-lisp (dynamic-extent transform)
     #+(and lispm (not clx-ansi-common-lisp)) (sys:downward-funarg transform))
    (typecase data
      (list
       (if transform
	   (,tlist buffer boffset data start end transform)
	   (,list buffer boffset data start end)))
      #-lispm
      ((simple-array ,type (*))
       (if transform
	   (,tsa buffer boffset data start end transform)
	   (,sa buffer boffset data start end)))
      (t ; FIXME: general sequences
       (if transform
	   (,tvec buffer boffset data start end transform)
	   (,vec buffer boffset data start end))))))

(define-sequence-writer write-sequence-card8 card8
  (write-list-card8 write-list-card8-with-transform)
  (write-simple-array-card8 write-simple-array-card8-with-transform)
  (write-vector-card8 write-vector-card8-with-transform))

(define-transformed-sequence-writer write-sequence-int8 int8 
  int8->card8 write-sequence-card8)

;;; Writing sequences of card16's

(define-list-writers (write-list-card16 write-list-card16-with-transform) card16
  2 write-card16)

#-lispm
(defun write-simple-array-card16 (buffer boffset data start end)
  (declare (type buffer buffer)
	   (type (simple-array card16 (*)) data)
	   (type array-index boffset start end))
  (with-vector (data (simple-array card16 (*)))
    (writing-buffer-chunks card16
			   ((index start))
			   ((type array-index index))
      ;; Depends upon the chunks being an even multiple of card16's big
      (do ((j 0 (index+ j 2)))
	  ((index>= j chunk))
	(declare (type array-index j))
	(write-card16 j (aref data index))
	(setq index (index+ index 1)))
      ;; overlapping case
      (let ((length (floor chunk 2)))
	(buffer-replace buffer-wbuf data
			buffer-woffset
			(index+ buffer-woffset length)
			index)
	(setq index (index+ index length)))))
  nil)

#-lispm
(defun write-simple-array-card16-with-transform (buffer boffset data start end transform)
  (declare (type buffer buffer)
	   (type (simple-array card16 (*)) data)
	   (type array-index boffset start end))
  (declare (type (function (card16) card16) transform)
	   #+clx-ansi-common-lisp
	   (dynamic-extent transform)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg transform))
  (with-vector (data (simple-array card16 (*)))
    (writing-buffer-chunks card16
			   ((index start))
			   ((type array-index index))
      ;; Depends upon the chunks being an even multiple of card16's big
      (do ((j 0 (index+ j 2)))
	  ((index>= j chunk))
	(declare (type array-index j))
	(write-card16 j (funcall transform (aref data index)))
	(setq index (index+ index 1)))))
  nil)

(defun write-vector-card16 (buffer boffset data start end)
  (declare (type buffer buffer)
	   (type vector data)
	   (type array-index boffset start end)
	   (optimize #+cmu(ext:inhibit-warnings 3)))
  (with-vector (data vector)
    (writing-buffer-chunks card16
			   ((index start))
			   ((type array-index index))
      ;; Depends upon the chunks being an even multiple of card16's big
      (do ((j 0 (index+ j 2)))
	  ((index>= j chunk))
	(declare (type array-index j))
	(write-card16 j (aref data index))
	(setq index (index+ index 1)))
      ;; overlapping case
      (let ((length (floor chunk 2)))
	(buffer-replace buffer-wbuf data
			buffer-woffset
			(index+ buffer-woffset length)
			index)
	(setq index (index+ index length)))))
  nil)

(defun write-vector-card16-with-transform (buffer boffset data start end transform)
  (declare (type buffer buffer)
	   (type vector data)
	   (type array-index boffset start end)
	   (optimize #+cmu(ext:inhibit-warnings 3)))
  (declare (type (function (t) card16) transform)
	   #+clx-ansi-common-lisp
	   (dynamic-extent transform)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg transform))
  (with-vector (data vector)
    (writing-buffer-chunks card16
			   ((index start))
			   ((type array-index index))
      ;; Depends upon the chunks being an even multiple of card16's big
      (do ((j 0 (index+ j 2)))
	  ((index>= j chunk))
	(declare (type array-index j))
	(write-card16 j (funcall transform (aref data index)))
	(setq index (index+ index 1)))))
  nil)

(define-sequence-writer write-sequence-card16 card16
  (write-list-card16 write-list-card16-with-transform)
  (write-simple-array-card16 write-simple-array-card16-with-transform)
  (write-vector-card16 write-vector-card16-with-transform))

;;; Writing sequences of int16's

(define-list-writers (write-list-int16 write-list-int16-with-transform) int16
  2 write-int16)

#-lispm
(defun write-simple-array-int16 (buffer boffset data start end)
  (declare (type buffer buffer)
	   (type (simple-array int16 (*)) data)
	   (type array-index boffset start end))
  (with-vector (data (simple-array int16 (*)))
    (writing-buffer-chunks int16
			   ((index start))
			   ((type array-index index))
      ;; Depends upon the chunks being an even multiple of int16's big
      (do ((j 0 (index+ j 2)))
	  ((index>= j chunk))
	(declare (type array-index j))
	(write-int16 j (aref data index))
	(setq index (index+ index 1)))
      ;; overlapping case
      (let ((length (floor chunk 2)))
	(buffer-replace buffer-wbuf data
			buffer-woffset
			(index+ buffer-woffset length)
			index)
	(setq index (index+ index length)))))
  nil)

#-lispm
(defun write-simple-array-int16-with-transform (buffer boffset data start end transform)
  (declare (type buffer buffer)
	   (type (simple-array int16 (*)) data)
	   (type array-index boffset start end))
  (declare (type (function (int16) int16) transform)
	   #+clx-ansi-common-lisp
	   (dynamic-extent transform)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg transform))
  (with-vector (data (simple-array int16 (*)))
    (writing-buffer-chunks int16
			   ((index start))
			   ((type array-index index))
      ;; Depends upon the chunks being an even multiple of int16's big
      (do ((j 0 (index+ j 2)))
	  ((index>= j chunk))
	(declare (type array-index j))
	(write-int16 j (funcall transform (aref data index)))
	(setq index (index+ index 1)))))
  nil)

(defun write-vector-int16 (buffer boffset data start end)
  (declare (type buffer buffer)
	   (type vector data)
	   (type array-index boffset start end)
	   (optimize #+cmu(ext:inhibit-warnings 3)))
  (with-vector (data vector)
    (writing-buffer-chunks int16
			   ((index start))
			   ((type array-index index))
      ;; Depends upon the chunks being an even multiple of int16's big
      (do ((j 0 (index+ j 2)))
	  ((index>= j chunk))
	(declare (type array-index j))
	(write-int16 j (aref data index))
	(setq index (index+ index 1)))
      ;; overlapping case
      (let ((length (floor chunk 2)))
	(buffer-replace buffer-wbuf data
			buffer-woffset
			(index+ buffer-woffset length)
			index)
	(setq index (index+ index length)))))
  nil)

(defun write-vector-int16-with-transform (buffer boffset data start end transform)
  (declare (type buffer buffer)
	   (type vector data)
	   (type array-index boffset start end)
	   (optimize #+cmu(ext:inhibit-warnings 3)))
  (declare (type (function (t) int16) transform)
	   #+clx-ansi-common-lisp
	   (dynamic-extent transform)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg transform))
  (with-vector (data vector)
    (writing-buffer-chunks int16
			   ((index start))
			   ((type array-index index))
      ;; Depends upon the chunks being an even multiple of int16's big
      (do ((j 0 (index+ j 2)))
	  ((index>= j chunk))
	(declare (type array-index j))
	(write-int16 j (funcall transform (aref data index)))
	(setq index (index+ index 1)))))
  nil)

(define-sequence-writer write-sequence-int16 int16
  (write-list-int16 write-list-int16-with-transform)
  (write-simple-array-int16 write-simple-array-int16-with-transform)
  (write-vector-int16 write-vector-int16-with-transform))

;;; Writing sequences of card32's

(define-list-writers (write-list-card32 write-list-card32-with-transform) card32
  4 write-card32)

#-lispm
(defun write-simple-array-card32 (buffer boffset data start end)
  (declare (type buffer buffer)
	   (type (simple-array card32 (*)) data)
	   (type array-index boffset start end))
  (with-vector (data (simple-array card32 (*)))
    (writing-buffer-chunks card32
			   ((index start))
			   ((type array-index index))
      ;; Depends upon the chunks being an even multiple of card32's big
      (do ((j 0 (index+ j 4)))
	  ((index>= j chunk))
	(declare (type array-index j))
	(write-card32 j (aref data index))
	(setq index (index+ index 1)))
      ;; overlapping case
      (let ((length (floor chunk 4)))
	(buffer-replace buffer-lbuf data
			buffer-loffset
			(index+ buffer-loffset length)
			index)
	(setq index (index+ index length)))))
  nil)

#-lispm
(defun write-simple-array-card32-with-transform (buffer boffset data start end transform)
  (declare (type buffer buffer)
	   (type (simple-array card32 (*)) data)
	   (type array-index boffset start end))
  (declare (type (function (card32) card32) transform)
	   #+clx-ansi-common-lisp
	   (dynamic-extent transform)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg transform))
  (with-vector (data (simple-array card32 (*)))
    (writing-buffer-chunks card32
			   ((index start))
			   ((type array-index index))
      ;; Depends upon the chunks being an even multiple of card32's big
      (do ((j 0 (index+ j 4)))
	  ((index>= j chunk))
	(declare (type array-index j))
	(write-card32 j (funcall transform (aref data index)))
	(setq index (index+ index 1)))))
  nil)

(defun write-vector-card32 (buffer boffset data start end)
  (declare (type buffer buffer)
	   (type vector data)
	   (type array-index boffset start end)
	   (optimize #+cmu(ext:inhibit-warnings 3)))
  (with-vector (data vector)
    (writing-buffer-chunks card32
			   ((index start))
			   ((type array-index index))
      ;; Depends upon the chunks being an even multiple of card32's big
      (do ((j 0 (index+ j 4)))
	  ((index>= j chunk))
	(declare (type array-index j))
	(write-card32 j (aref data index))
	(setq index (index+ index 1)))
      ;; overlapping case
      (let ((length (floor chunk 4)))
	(buffer-replace buffer-lbuf data
			buffer-loffset
			(index+ buffer-loffset length)
			index)
	(setq index (index+ index length)))))
  nil)

(defun write-vector-card32-with-transform (buffer boffset data start end transform)
  (declare (type buffer buffer)
	   (type vector data)
	   (type array-index boffset start end)
	   (optimize #+cmu(ext:inhibit-warnings 3)))
  (declare (type (function (t) card32) transform)
	   #+clx-ansi-common-lisp
	   (dynamic-extent transform)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg transform))
  (with-vector (data vector)
    (writing-buffer-chunks card32
			   ((index start))
			   ((type array-index index))
      ;; Depends upon the chunks being an even multiple of card32's big
      (do ((j 0 (index+ j 4)))
	  ((index>= j chunk))
	(declare (type array-index j))
	(write-card32 j (funcall transform (aref data index)))
	(setq index (index+ index 1)))))
  nil)

(define-sequence-writer write-sequence-card32 card32
  (write-list-card32 write-list-card32-with-transform)
  (write-simple-array-card32 write-simple-array-card32-with-transform)
  (write-vector-card32 write-vector-card32-with-transform))

(define-transformed-sequence-writer write-sequence-int32 int32 
  int32->card32 write-sequence-card32)

(defun read-bitvector256 (buffer-bbuf boffset data)
  (declare (type buffer-bytes buffer-bbuf)
	   (type array-index boffset)
	   (type (or null (simple-bit-vector 256)) data))
  (let ((result (or data (make-array 256 :element-type 'bit :initial-element 0))))
    (declare (type (simple-bit-vector 256) result))
    (do ((i (index+ boffset 1) (index+ i 1)) ;; Skip first byte
	 (j 8 (index+ j 8)))
	((index>= j 256))
      (declare (type array-index i j))
      (do ((byte (aref-card8 buffer-bbuf i) (index-ash byte -1))
	   (k j (index+ k 1)))
	  ((zerop byte)
	   (when data ;; Clear uninitialized bits in data
	     (do ((end (index+ j 8)))
		 ((index= k end))
	       (declare (type array-index end))
	       (setf (aref result k) 0)
	       (index-incf k))))
	(declare (type array-index k)
		 (type card8 byte))
	(setf (aref result k) (the bit (logand byte 1)))))
    result))

(defun write-bitvector256 (buffer boffset map)
  (declare (type buffer buffer)
	   (type array-index boffset)
	   (type (simple-array bit (*)) map))
  (with-buffer-output (buffer :index boffset :sizes 8)
    (do* ((i (index+ buffer-boffset 1) (index+ i 1))	; Skip first byte
	  (j 8 (index+ j 8)))		
	 ((index>= j 256))
      (declare (type array-index i j))
      (do ((byte 0)
	   (bit (index+ j 7) (index- bit 1)))
	  ((index< bit j)
	   (aset-card8 byte buffer-bbuf i))
	(declare (type array-index bit)
		 (type card8 byte))
	(setq byte (the card8 (logior (the card8 (ash byte 1)) (aref map bit))))))))

;;; Writing sequences of char2b's

(define-list-writers (write-list-char2b write-list-char2b-with-transform) card16
  2 write-char2b)

#-lispm
(defun write-simple-array-char2b (buffer boffset data start end)
  (declare (type buffer buffer)
	   (type (simple-array card16 (*)) data)
	   (type array-index boffset start end))
  (with-vector (data (simple-array card16 (*)))
    (writing-buffer-chunks card16
			   ((index start))
			   ((type array-index index))
      (do ((j 0 (index+ j 2)))
	  ((index>= j (1- chunk)) (setf chunk j))
	(declare (type array-index j))
	(write-char2b j (aref data index))
	(setq index (index+ index 1)))))
  nil)

#-lispm
(defun write-simple-array-char2b-with-transform (buffer boffset data start end transform)
  (declare (type buffer buffer)
	   (type (simple-array card16 (*)) data)
	   (type array-index boffset start end))
  (declare (type (function (card16) card16) transform)
	   #+clx-ansi-common-lisp
	   (dynamic-extent transform)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg transform))
  (with-vector (data (simple-array card16 (*)))
    (writing-buffer-chunks card16
			   ((index start))
			   ((type array-index index))
      (do ((j 0 (index+ j 2)))
	  ((index>= j (1- chunk)) (setf chunk j))
	(declare (type array-index j))
	(write-char2b j (funcall transform (aref data index)))
	(setq index (index+ index 1)))))
  nil)

(defun write-vector-char2b (buffer boffset data start end)
  (declare (type buffer buffer)
	   (type vector data)
	   (type array-index boffset start end)
	   (optimize #+cmu(ext:inhibit-warnings 3)))
  (with-vector (data vector)
    (writing-buffer-chunks card16
			   ((index start))
			   ((type array-index index))
      (do ((j 0 (index+ j 2)))
	  ((index>= j (1- chunk)) (setf chunk j))
	(declare (type array-index j))
	(write-char2b j (aref data index))
	(setq index (index+ index 1)))))
  nil)

(defun write-vector-char2b-with-transform (buffer boffset data start end transform)
  (declare (type buffer buffer)
	   (type vector data)
	   (type array-index boffset start end)
	   (optimize #+cmu(ext:inhibit-warnings 3)))
  (declare (type (function (t) card16) transform)
	   #+clx-ansi-common-lisp
	   (dynamic-extent transform)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg transform))
  (with-vector (data vector)
    (writing-buffer-chunks card16
			   ((index start))
			   ((type array-index index))
      (do ((j 0 (index+ j 2)))
	  ((index>= j (1- chunk)) (setf chunk j))
	(declare (type array-index j))
	(write-char2b j (funcall transform (aref data index)))
	(setq index (index+ index 1)))))
  nil)

(define-sequence-writer write-sequence-char2b card16
  (write-list-char2b write-list-char2b-with-transform)
  (write-simple-array-char2b write-simple-array-char2b-with-transform)
  (write-vector-char2b write-vector-char2b-with-transform))
