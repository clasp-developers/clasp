;;; -*- Mode: common-lisp; Package: xlib; Base: 10; Lowercase: Yes -*-
;;;
;;; CLX -- exclcmac.cl
;;;           This file provides for inline expansion of some functions.
;;;
;;; Copyright (c) 1989 Franz Inc, Berkeley, Ca.
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

;;
;; Type predicates
;;
(excl:defcmacro card8p (x)
  (let ((xx (gensym)))
    `(let ((,xx ,x))
       (declare (optimize (speed 3) (safety 0))
		(fixnum ,xx))
       (and (excl:fixnump ,xx) (> #.(expt 2 8) ,xx) (>= ,xx 0)))))

(excl:defcmacro card16p (x)
  (let ((xx (gensym)))
    `(let ((,xx ,x))
       (declare (optimize (speed 3) (safety 0))
		(fixnum ,xx))
       (and (excl:fixnump ,xx) (> #.(expt 2 16) ,xx) (>= ,xx 0)))))

(excl:defcmacro int8p (x)
  (let ((xx (gensym)))
    `(let ((,xx ,x))
       (declare (optimize (speed 3) (safety 0))
		(fixnum ,xx))
       (and (excl:fixnump ,xx) (> #.(expt 2 7) ,xx) (>= ,xx #.(expt -2 7))))))

(excl:defcmacro int16p (x)
  (let ((xx (gensym)))
    `(let ((,xx ,x))
       (declare (optimize (speed 3) (safety 0))
		(fixnum ,xx))
       (and (excl:fixnump ,xx) (> #.(expt 2 15) ,xx) (>= ,xx #.(expt -2 15))))))

;; Card29p, card32p, int32p are too large to expand inline


;;
;; Type transformers
;;
(excl:defcmacro card8->int8 (x)
  (let ((xx (gensym)))
    `(let ((,xx ,x))
       ,(declare-bufmac)
       (declare (type card8 ,xx))
       (the int8 (if (logbitp 7 ,xx)
		     (the int8 (- ,xx #x100))
		   ,xx)))))
(excl:defcmacro int8->card8 (x)
  `(locally ,(declare-bufmac)
     (the card8 (ldb (byte 8 0) (the int8 ,x)))))

(excl:defcmacro card16->int16 (x)
  (let ((xx (gensym)))
    `(let ((,xx ,x))
       ,(declare-bufmac)
       (declare (type card16 ,xx))
       (the int16 (if (logbitp 15 ,xx)
		      (the int16 (- ,xx #x10000))
		    ,xx)))))

(excl:defcmacro int16->card16 (x)
  `(locally ,(declare-bufmac)
     (the card16 (ldb (byte 16 0) (the int16 ,x)))))

(excl:defcmacro card32->int32 (x)
  (let ((xx (gensym)))
    `(let ((,xx ,x))
       ,(declare-bufmac)
       (declare (type card32 ,xx))
       (the int32 (if (logbitp 31 ,xx)
		      (the int32 (- ,xx #x100000000))
		    ,xx)))))

(excl:defcmacro int32->card32 (x)
  `(locally ,(declare-bufmac)
     (the card32 (ldb (byte 32 0) (the int32 ,x)))))

(excl:defcmacro char->card8 (char)
  `(locally ,(declare-bufmac)
     (the card8 (char-code (the string-char ,char)))))

(excl:defcmacro card8->char (card8)
  `(locally ,(declare-bufmac)
     (the string-char (code-char (the card8 ,card8)))))


;;
;; Array accessors and setters
;;
(excl:defcmacro aref-card8 (a i)
  `(locally ,(declare-bufmac)
     (the card8 (sys:memref (the buffer-bytes ,a)
			    #.(comp::mdparam 'comp::md-svector-data0-adj)
			    (the array-index ,i)
			    :unsigned-byte))))
  
(excl:defcmacro aset-card8 (v a i)
  `(locally ,(declare-bufmac)
     (setf (sys:memref (the buffer-bytes ,a)
		       #.(comp::mdparam 'comp::md-svector-data0-adj)
		       (the array-index ,i)
		       :unsigned-byte)
	   (the card8 ,v))))
  
(excl:defcmacro aref-int8 (a i)
  `(locally ,(declare-bufmac)
     (the int8 (sys:memref (the buffer-bytes ,a)
			   #.(comp::mdparam 'comp::md-svector-data0-adj)
			   (the array-index ,i)
			   :signed-byte))))
  
(excl:defcmacro aset-int8 (v a i)
  `(locally ,(declare-bufmac)
     (setf (sys:memref (the buffer-bytes ,a)
		       #.(comp::mdparam 'comp::md-svector-data0-adj)
		       (the array-index ,i)
		       :signed-byte)
       (the int8 ,v))))

(excl:defcmacro aref-card16 (a i)
  `(locally ,(declare-bufmac)
     (the card16 (sys:memref (the buffer-bytes ,a)
			     #.(comp::mdparam 'comp::md-svector-data0-adj)
			     (the array-index ,i)
			     :unsigned-word))))
  
(excl:defcmacro aset-card16 (v a i)
  `(locally ,(declare-bufmac)
     (setf (sys:memref (the buffer-bytes ,a)
		       #.(comp::mdparam 'comp::md-svector-data0-adj)
		       (the array-index ,i)
		       :unsigned-word)
	   (the card16 ,v))))
  
(excl:defcmacro aref-int16 (a i)
  `(locally ,(declare-bufmac)
     (the int16 (sys:memref (the buffer-bytes ,a)
			    #.(comp::mdparam 'comp::md-svector-data0-adj)
			    (the array-index ,i)
			    :signed-word))))
  
(excl:defcmacro aset-int16 (v a i)
  `(locally ,(declare-bufmac)
     (setf (sys:memref (the buffer-bytes ,a)
		       #.(comp::mdparam 'comp::md-svector-data0-adj)
		       (the array-index ,i)
		       :signed-word)
       (the int16 ,v))))
  
(excl:defcmacro aref-card32 (a i)
  `(locally ,(declare-bufmac)
     (the card32 (sys:memref (the buffer-bytes ,a)
			     #.(comp::mdparam 'comp::md-svector-data0-adj)
			     (the array-index ,i)
			     :unsigned-long))))
    
(excl:defcmacro aset-card32 (v a i)
  `(locally ,(declare-bufmac)
     (setf (sys:memref (the buffer-bytes ,a)
		       #.(comp::mdparam 'comp::md-svector-data0-adj)
		       (the array-index ,i)
		       :unsigned-long)
       (the card32 ,v))))

(excl:defcmacro aref-int32 (a i)
  `(locally ,(declare-bufmac)
     (the int32 (sys:memref (the buffer-bytes ,a)
			    #.(comp::mdparam 'comp::md-svector-data0-adj)
			    (the array-index ,i)
			    :signed-long))))
    
(excl:defcmacro aset-int32 (v a i)
  `(locally ,(declare-bufmac)
     (setf (sys:memref (the buffer-bytes ,a)
		       #.(comp::mdparam 'comp::md-svector-data0-adj)
		       (the array-index ,i)
		       :signed-long)
       (the int32 ,v))))

(excl:defcmacro aref-card29 (a i)
  ;; Don't need to mask bits here since X protocol guarantees top bits zero
  `(locally ,(declare-bufmac)
     (the card29 (sys:memref (the buffer-bytes ,a)
			     #.(comp::mdparam 'comp::md-svector-data0-adj)
			     (the array-index ,i)
			     :unsigned-long))))

(excl:defcmacro aset-card29 (v a i)
  ;; I also assume here Lisp is passing a number that fits in 29 bits.
  `(locally ,(declare-bufmac)
     (setf (sys:memref (the buffer-bytes ,a)
		       #.(comp::mdparam 'comp::md-svector-data0-adj)
		       (the array-index ,i)
		       :unsigned-long)
       (the card29 ,v))))

;;
;; Font accessors
;;
(excl:defcmacro font-id (font)
  ;; Get font-id, opening font if needed
  (let ((f (gensym)))
    `(let ((,f ,font))
       (or (font-id-internal ,f)
	   (open-font-internal ,f)))))

(excl:defcmacro font-font-info (font)
  (let ((f (gensym)))
    `(let ((,f ,font))
       (or (font-font-info-internal ,f)
	   (query-font ,f)))))

(excl:defcmacro font-char-infos (font)
  (let ((f (gensym)))
    `(let ((,f ,font))
       (or (font-char-infos-internal ,f)
	   (progn (query-font ,f)
		  (font-char-infos-internal ,f))))))


;;
;; Miscellaneous
;;
(excl:defcmacro current-process ()
  `(the (or mp::process null) (and mp::*scheduler-stack-group*
				  mp::*current-process*)))

(excl:defcmacro process-wakeup (process)
  (let ((proc (gensym)))
    `(let ((.pw-curproc. mp::*current-process*)
	   (,proc ,process))
       (when (and .pw-curproc. ,proc)
	 (if (> (mp::process-priority ,proc)
		(mp::process-priority .pw-curproc.))
	     (mp::process-allow-schedule ,proc))))))

(excl:defcmacro buffer-new-request-number (buffer)
  (let ((buf (gensym)))
    `(let ((,buf ,buffer))
       (declare (type buffer ,buf))
       (setf (buffer-request-number ,buf)
	 (ldb (byte 16 0) (1+ (buffer-request-number ,buf)))))))


