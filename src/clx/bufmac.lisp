;;; -*- Mode: LISP; Syntax: Common-lisp; Package: XLIB; Base: 10; Lowercase: Yes -*-

;;; This file contains macro definitions for the BUFFER object for Common-Lisp
;;; X windows version 11

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

(in-package :xlib)

;;; The read- macros are in buffer.lisp, because event-case depends on (most of) them.

(defmacro write-card8 (byte-index item)
  `(aset-card8 (the card8 ,item) buffer-bbuf (index+ buffer-boffset ,byte-index)))

(defmacro write-int8 (byte-index item)
  `(aset-int8 (the int8 ,item) buffer-bbuf (index+ buffer-boffset ,byte-index)))

(defmacro write-card16 (byte-index item)
  #+clx-overlapping-arrays
  `(aset-card16 (the card16 ,item) buffer-wbuf
		(index+ buffer-woffset (index-ash ,byte-index -1)))
  #-clx-overlapping-arrays
  `(aset-card16 (the card16 ,item) buffer-bbuf
		(index+ buffer-boffset ,byte-index)))

(defmacro write-int16 (byte-index item)
  #+clx-overlapping-arrays
  `(aset-int16 (the int16 ,item) buffer-wbuf
	       (index+ buffer-woffset (index-ash ,byte-index -1)))
  #-clx-overlapping-arrays
  `(aset-int16 (the int16 ,item) buffer-bbuf
	       (index+ buffer-boffset ,byte-index)))

(defmacro write-card32 (byte-index item)
  #+clx-overlapping-arrays
  `(aset-card32 (the card32 ,item) buffer-lbuf
		(index+ buffer-loffset (index-ash ,byte-index -2)))
  #-clx-overlapping-arrays
  `(aset-card32 (the card32 ,item) buffer-bbuf
		(index+ buffer-boffset ,byte-index)))

(defmacro write-int32 (byte-index item)
  #+clx-overlapping-arrays
  `(aset-int32 (the int32 ,item) buffer-lbuf
	       (index+ buffer-loffset (index-ash ,byte-index -2)))
  #-clx-overlapping-arrays
  `(aset-int32 (the int32 ,item) buffer-bbuf
	       (index+ buffer-boffset ,byte-index)))

(defmacro write-card29 (byte-index item)
  #+clx-overlapping-arrays
  `(aset-card29 (the card29 ,item) buffer-lbuf
		(index+ buffer-loffset (index-ash ,byte-index -2)))
  #-clx-overlapping-arrays
  `(aset-card29 (the card29 ,item) buffer-bbuf
		(index+ buffer-boffset ,byte-index)))

;; This is used for 2-byte characters, which may not be aligned on 2-byte boundaries
;; and always are written high-order byte first.
(defmacro write-char2b (byte-index item)
  ;; It is impossible to do an overlapping write, so only nonoverlapping here.
  `(let ((%item ,item)
	 (%byte-index (index+ buffer-boffset ,byte-index)))
     (declare (type card16 %item)
	      (type array-index %byte-index))
     (aset-card8 (the card8 (ldb (byte 8 8) %item)) buffer-bbuf %byte-index)
     (aset-card8 (the card8 (ldb (byte 8 0) %item)) buffer-bbuf (index+ %byte-index 1))))

(defmacro set-buffer-offset (value &environment env)
  env
  `(let ((.boffset. ,value))
     (declare (type array-index .boffset.))
     (setq buffer-boffset .boffset.)
     #+clx-overlapping-arrays
     ,@(when (member 16 (macroexpand '(%buffer-sizes) env))
	 `((setq buffer-woffset (index-ash .boffset. -1))))
     #+clx-overlapping-arrays
     ,@(when (member 32 (macroexpand '(%buffer-sizes) env))
	 `((setq buffer-loffset (index-ash .boffset. -2))))
     #+clx-overlapping-arrays
     .boffset.))

(defmacro advance-buffer-offset (value)
  `(set-buffer-offset (index+ buffer-boffset ,value)))

(defmacro with-buffer-output ((buffer &key (sizes '(8 16 32)) length index) &body body)
  (unless (listp sizes) (setq sizes (list sizes)))
  `(let ((%buffer ,buffer))
     (declare (type display %buffer))
     ,(declare-bufmac)
     ,(when length
	`(when (index>= (index+ (buffer-boffset %buffer) ,length) (buffer-size %buffer))
	   (buffer-flush %buffer)))
     (let* ((buffer-boffset (the array-index ,(or index `(buffer-boffset %buffer))))
	    #-clx-overlapping-arrays
	    (buffer-bbuf (buffer-obuf8 %buffer))
	    #+clx-overlapping-arrays
	    ,@(append
		(when (member 8 sizes)
		  `((buffer-bbuf (buffer-obuf8 %buffer))))
		(when (or (member 16 sizes) (member 160 sizes))
		  `((buffer-woffset (index-ash buffer-boffset -1))
		    (buffer-wbuf (buffer-obuf16 %buffer))))
		(when (member 32 sizes)
		  `((buffer-loffset (index-ash buffer-boffset -2))
		    (buffer-lbuf (buffer-obuf32 %buffer))))))
       (declare (type array-index buffer-boffset))
       #-clx-overlapping-arrays
       (declare (type buffer-bytes buffer-bbuf))
       #+clx-overlapping-arrays
       ,@(append
	   (when (member 8  sizes)
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

;;; This macro is just used internally in buffer

(defmacro writing-buffer-chunks (type args decls &body body)
  (when (> (length body) 2)
    (error "writing-buffer-chunks called with too many forms"))
  (let* ((size (* 8 (index-increment type)))
	 (form #-clx-overlapping-arrays
	       (first body)
	       #+clx-overlapping-arrays		; XXX type dependencies
	       (or (second body)
		   (first body))))
    `(with-buffer-output (buffer :index boffset :sizes ,(reverse (adjoin size '(8))))
       ;; Loop filling the buffer
       (do* (,@args
	     ;; Number of bytes needed to output
	     (len ,(if (= size 8)
		       `(index- end start)
		       `(index-ash (index- end start) ,(truncate size 16)))
		  (index- len chunk))
	     ;; Number of bytes available in buffer
	     (chunk (index-min len (index- (buffer-size buffer) buffer-boffset))
		    (index-min len (index- (buffer-size buffer) buffer-boffset))))
	    ((not (index-plusp len)))
	 (declare ,@decls
		  (type array-index len chunk))
	 ,form
	 (index-incf buffer-boffset chunk)
	 ;; Flush the buffer
	 (when (and (index-plusp len) (index>= buffer-boffset (buffer-size buffer)))
	   (setf (buffer-boffset buffer) buffer-boffset)
	   (buffer-flush buffer)
	   (setq buffer-boffset (buffer-boffset buffer))
	   #+clx-overlapping-arrays
	   ,(case size
	      (16 '(setq buffer-woffset (index-ash buffer-boffset -1)))
	      (32 '(setq buffer-loffset (index-ash buffer-boffset -2))))))
       (setf (buffer-boffset buffer) (lround buffer-boffset))))) 
