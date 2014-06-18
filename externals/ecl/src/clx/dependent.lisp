;;; -*- Mode: Lisp; Package: Xlib; Log: clx.log -*-

;; This file contains some of the system dependent code for CLX

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

(proclaim '(declaration array-register))

#+cmu
(setf (getf ext:*herald-items* :xlib)
      `("    CLX X Library " ,*version*))


;;; The size of the output buffer.  Must be a multiple of 4.
(defparameter *output-buffer-size* 8192)

#+explorer
(zwei:define-indentation event-case (1 1))

;;; Number of seconds to wait for a reply to a server request
(defparameter *reply-timeout* nil) 

#-(or clx-overlapping-arrays (not clx-little-endian))
(progn
  (defconstant +word-0+ 0)
  (defconstant +word-1+ 1)

  (defconstant +long-0+ 0)
  (defconstant +long-1+ 1)
  (defconstant +long-2+ 2)
  (defconstant +long-3+ 3))

#-(or clx-overlapping-arrays clx-little-endian)
(progn
  (defconstant +word-0+ 1)
  (defconstant +word-1+ 0)

  (defconstant +long-0+ 3)
  (defconstant +long-1+ 2)
  (defconstant +long-2+ 1)
  (defconstant +long-3+ 0))

;;; Set some compiler-options for often used code

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +buffer-speed+ #+clx-debugging 1 #-clx-debugging 3
    "Speed compiler option for buffer code.")
  (defconstant +buffer-safety+ #+clx-debugging 3 #-clx-debugging 0
    "Safety compiler option for buffer code.")
  (defconstant +buffer-debug+ #+clx-debugging 2 #-clx-debugging 1
    "Debug compiler option for buffer code>")
  (defun declare-bufmac ()
    `(declare (optimize
	       (speed ,+buffer-speed+)
	       (safety ,+buffer-safety+)
	       (debug ,+buffer-debug+))))
  ;; It's my impression that in lucid there's some way to make a
  ;; declaration called fast-entry or something that causes a function
  ;; to not do some checking on args. Sadly, we have no lucid manuals
  ;; here.  If such a declaration is available, it would be a good
  ;; idea to make it here when +buffer-speed+ is 3 and +buffer-safety+
  ;; is 0.
  (defun declare-buffun ()
    `(declare (optimize
	       (speed ,+buffer-speed+)
	       (safety ,+buffer-safety+)
	       (debug ,+buffer-debug+)))))

(declaim (inline card8->int8 int8->card8
		 card16->int16 int16->card16
		 card32->int32 int32->card32))

#-Genera
(progn

(defun card8->int8 (x)
  (declare (type card8 x))
  (declare (clx-values int8))
  #.(declare-buffun)
  (the int8 (if (logbitp 7 x)
		(the int8 (- x #x100))
	      x)))

(defun int8->card8 (x)
  (declare (type int8 x))
  (declare (clx-values card8))
  #.(declare-buffun)
  (the card8 (ldb (byte 8 0) x)))

(defun card16->int16 (x)
  (declare (type card16 x))
  (declare (clx-values int16))
  #.(declare-buffun)
  (the int16 (if (logbitp 15 x)
		 (the int16 (- x #x10000))
		 x)))

(defun int16->card16 (x)
  (declare (type int16 x))
  (declare (clx-values card16))
  #.(declare-buffun)
  (the card16 (ldb (byte 16 0) x)))

(defun card32->int32 (x)
  (declare (type card32 x))
  (declare (clx-values int32))
  #.(declare-buffun)
  (the int32 (if (logbitp 31 x)
		 (the int32 (- x #x100000000))
		 x)))

(defun int32->card32 (x)
  (declare (type int32 x))
  (declare (clx-values card32))
  #.(declare-buffun)
  (the card32 (ldb (byte 32 0) x)))

)

#+Genera
(progn

(defun card8->int8 (x)
  (declare lt:(side-effects simple reducible))
  (if (logbitp 7 x) (- x #x100) x))

(defun int8->card8 (x)
  (declare lt:(side-effects simple reducible))
  (ldb (byte 8 0) x))

(defun card16->int16 (x)
  (declare lt:(side-effects simple reducible))
  (if (logbitp 15 x) (- x #x10000) x))

(defun int16->card16 (x)
  (declare lt:(side-effects simple reducible))
  (ldb (byte 16 0) x))

(defun card32->int32 (x)
  (declare lt:(side-effects simple reducible))
  (sys:%logldb (byte 32 0) x))

(defun int32->card32 (x)
  (declare lt:(side-effects simple reducible))
  (ldb (byte 32 0) x))

)

(declaim (inline aref-card8 aset-card8 aref-int8 aset-int8))

#-(or Genera lcl3.0 excl)
(progn

(defun aref-card8 (a i)
  (declare (type buffer-bytes a)
	   (type array-index i))
  (declare (clx-values card8))
  #.(declare-buffun)
  (the card8 (aref a i)))

(defun aset-card8 (v a i)
  (declare (type card8 v)
	   (type buffer-bytes a)
	   (type array-index i))
  #.(declare-buffun)
  (setf (aref a i) v))

(defun aref-int8 (a i)
  (declare (type buffer-bytes a)
	   (type array-index i))
  (declare (clx-values int8))
  #.(declare-buffun)
  (card8->int8 (aref a i)))

(defun aset-int8 (v a i)
  (declare (type int8 v)
	   (type buffer-bytes a)
	   (type array-index i))
  #.(declare-buffun)
  (setf (aref a i) (int8->card8 v)))

)

#+Genera
(progn

(defun aref-card8 (a i)
  (aref a i))

(defun aset-card8 (v a i)
  (zl:aset v a i))

(defun aref-int8 (a i)
  (card8->int8 (aref a i)))

(defun aset-int8 (v a i)
  (zl:aset (int8->card8 v) a i))

)

#+(or excl lcl3.0 clx-overlapping-arrays)
(declaim (inline aref-card16 aref-int16 aref-card32 aref-int32 aref-card29
		 aset-card16 aset-int16 aset-card32 aset-int32 aset-card29))

#+(and clx-overlapping-arrays Genera)
(progn

(defun aref-card16 (a i)
  (aref a i))

(defun aset-card16 (v a i)
  (zl:aset v a i))

(defun aref-int16 (a i)
  (card16->int16 (aref a i)))

(defun aset-int16 (v a i)
  (zl:aset (int16->card16 v) a i)
  v)

(defun aref-card32 (a i)
  (int32->card32 (aref a i)))

(defun aset-card32 (v a i)
  (zl:aset (card32->int32 v) a i))

(defun aref-int32 (a i) (aref a i))

(defun aset-int32 (v a i)
  (zl:aset v a i))

(defun aref-card29 (a i)
  (aref a i))

(defun aset-card29 (v a i)
  (zl:aset v a i))

)

#+(and clx-overlapping-arrays (not Genera))
(progn

(defun aref-card16 (a i)
  (aref a i))

(defun aset-card16 (v a i)
  (setf (aref a i) v))

(defun aref-int16 (a i)
  (card16->int16 (aref a i)))

(defun aset-int16 (v a i)
  (setf (aref a i) (int16->card16 v))
  v)

(defun aref-card32 (a i)
  (aref a i))

(defun aset-card32 (v a i)
  (setf (aref a i) v))

(defun aref-int32 (a i)
  (card32->int32 (aref a i)))

(defun aset-int32 (v a i)
  (setf (aref a i) (int32->card32 v))
  v)

(defun aref-card29 (a i)
  (aref a i))

(defun aset-card29 (v a i)
  (setf (aref a i) v))

)

#+excl
(progn
  
(defun aref-card8 (a i)
  (declare (type buffer-bytes a)
	   (type array-index i))
  (declare (clx-values card8))
  #.(declare-buffun)
  (the card8 (sys:memref a #.(comp::mdparam 'comp::md-svector-data0-adj) i
			 :unsigned-byte)))

(defun aset-card8 (v a i)
  (declare (type card8 v)
	   (type buffer-bytes a)
	   (type array-index i))
  #.(declare-buffun)
  (setf (sys:memref a #.(comp::mdparam 'comp::md-svector-data0-adj) i
		    :unsigned-byte) v))

(defun aref-int8 (a i)
  (declare (type buffer-bytes a)
	   (type array-index i))
  (declare (clx-values int8))
  #.(declare-buffun)
  (the int8 (sys:memref a #.(comp::mdparam 'comp::md-svector-data0-adj) i
			:signed-byte)))

(defun aset-int8 (v a i)
  (declare (type int8 v)
	   (type buffer-bytes a)
	   (type array-index i))
  #.(declare-buffun)
  (setf (sys:memref a #.(comp::mdparam 'comp::md-svector-data0-adj) i
		    :signed-byte) v))

(defun aref-card16 (a i)
  (declare (type buffer-bytes a)
	   (type array-index i))
  (declare (clx-values card16))
  #.(declare-buffun)
  (the card16 (sys:memref a #.(comp::mdparam 'comp::md-svector-data0-adj) i
			  :unsigned-word)))
  
(defun aset-card16 (v a i)
  (declare (type card16 v)
	   (type buffer-bytes a)
	   (type array-index i))
  #.(declare-buffun)
  (setf (sys:memref a #.(comp::mdparam 'comp::md-svector-data0-adj) i
		    :unsigned-word) v))
  
(defun aref-int16 (a i)
  (declare (type buffer-bytes a)
	   (type array-index i))
  (declare (clx-values int16))
  #.(declare-buffun)
  (the int16 (sys:memref a #.(comp::mdparam 'comp::md-svector-data0-adj) i
			 :signed-word)))
  
(defun aset-int16 (v a i)
  (declare (type int16 v)
	   (type buffer-bytes a)
	   (type array-index i))
  #.(declare-buffun)
  (setf (sys:memref a #.(comp::mdparam 'comp::md-svector-data0-adj) i
		    :signed-word) v))
  
(defun aref-card32 (a i)
  (declare (type buffer-bytes a)
	   (type array-index i))
  (declare (clx-values card32))
  #.(declare-buffun)
  (the card32 (sys:memref a #.(comp::mdparam 'comp::md-svector-data0-adj) i
			  :unsigned-long)))
    
(defun aset-card32 (v a i)
  (declare (type card32 v)
	   (type buffer-bytes a)
	   (type array-index i))
  #.(declare-buffun)
  (setf (sys:memref a #.(comp::mdparam 'comp::md-svector-data0-adj) i
		    :unsigned-long) v))

(defun aref-int32 (a i)
  (declare (type buffer-bytes a)
	   (type array-index i))
  (declare (clx-values int32))
  #.(declare-buffun)
  (the int32 (sys:memref a #.(comp::mdparam 'comp::md-svector-data0-adj) i
			 :signed-long)))
    
(defun aset-int32 (v a i)
  (declare (type int32 v)
	   (type buffer-bytes a)
	   (type array-index i))
  #.(declare-buffun)
  (setf (sys:memref a #.(comp::mdparam 'comp::md-svector-data0-adj) i
		    :signed-long) v))

(defun aref-card29 (a i)
  (declare (type buffer-bytes a)
	   (type array-index i))
  (declare (clx-values card29))
  #.(declare-buffun)
  (the card29 (sys:memref a #.(comp::mdparam 'comp::md-svector-data0-adj) i
			  :unsigned-long)))

(defun aset-card29 (v a i)
  (declare (type card29 v)
	   (type buffer-bytes a)
	   (type array-index i))
  #.(declare-buffun)
  (setf (sys:memref a #.(comp::mdparam 'comp::md-svector-data0-adj) i
		    :unsigned-long) v))
  
)

#+lcl3.0
(progn

(defun aref-card8 (a i)
  (declare (type buffer-bytes a)
	   (type array-index i)
	   (clx-values card8))
  #.(declare-buffun)
  (the card8 (lucid::%svref-8bit a i)))

(defun aset-card8 (v a i)
  (declare (type card8 v)
	   (type buffer-bytes a)
	   (type array-index i))
  #.(declare-buffun)
  (setf (lucid::%svref-8bit a i) v))

(defun aref-int8 (a i)
  (declare (type buffer-bytes a)
	   (type array-index i)
	   (clx-values int8))
  #.(declare-buffun)
  (the int8 (lucid::%svref-signed-8bit a i)))

(defun aset-int8 (v a i)
  (declare (type int8 v)
	   (type buffer-bytes a)
	   (type array-index i))
  #.(declare-buffun)
  (setf (lucid::%svref-signed-8bit a i) v))

(defun aref-card16 (a i)
  (declare (type buffer-bytes a)
	   (type array-index i)
	   (clx-values card16))
  #.(declare-buffun)
  (the card16 (lucid::%svref-16bit a (index-ash i -1))))
  
(defun aset-card16 (v a i)
  (declare (type card16 v)
	   (type buffer-bytes a)
	   (type array-index i))
  #.(declare-buffun)
  (setf (lucid::%svref-16bit a (index-ash i -1)) v))
  
(defun aref-int16 (a i)
  (declare (type buffer-bytes a)
	   (type array-index i)
	   (clx-values int16))
  #.(declare-buffun)
  (the int16 (lucid::%svref-signed-16bit a (index-ash i -1))))
  
(defun aset-int16 (v a i)
  (declare (type int16 v)
	   (type buffer-bytes a)
	   (type array-index i))
  #.(declare-buffun)
  (setf (lucid::%svref-signed-16bit a (index-ash i -1)) v))

(defun aref-card32 (a i)
  (declare (type buffer-bytes a)
	   (type array-index i)
	   (clx-values card32))
  #.(declare-buffun)
  (the card32 (lucid::%svref-32bit a (index-ash i -2))))
    
(defun aset-card32 (v a i)
  (declare (type card32 v)
	   (type buffer-bytes a)
	   (type array-index i))
  #.(declare-buffun)
  (setf (lucid::%svref-32bit a (index-ash i -2)) v))

(defun aref-int32 (a i)
  (declare (type buffer-bytes a)
	   (type array-index i)
	   (clx-values int32))
  #.(declare-buffun)
  (the int32 (lucid::%svref-signed-32bit a (index-ash i -2))))
    
(defun aset-int32 (v a i)
  (declare (type int32 v)
	   (type buffer-bytes a)
	   (type array-index i))
  #.(declare-buffun)
  (setf (lucid::%svref-signed-32bit a (index-ash i -2)) v))

(defun aref-card29 (a i)
  (declare (type buffer-bytes a)
	   (type array-index i)
	   (clx-values card29))
  #.(declare-buffun)
  (the card29 (lucid::%svref-32bit a (index-ash i -2))))

(defun aset-card29 (v a i)
  (declare (type card29 v)
	   (type buffer-bytes a)
	   (type array-index i))
  #.(declare-buffun)
  (setf (lucid::%svref-32bit a (index-ash i -2)) v))

)



#-(or excl lcl3.0 clx-overlapping-arrays)
(progn

(defun aref-card16 (a i)
  (declare (type buffer-bytes a)
	   (type array-index i))
  (declare (clx-values card16))
  #.(declare-buffun)
  (the card16
       (logior (the card16
		    (ash (the card8 (aref a (index+ i +word-1+))) 8))
	       (the card8
		    (aref a (index+ i +word-0+))))))

(defun aset-card16 (v a i)
  (declare (type card16 v)
	   (type buffer-bytes a)
	   (type array-index i))
  #.(declare-buffun)
  (setf (aref a (index+ i +word-1+)) (the card8 (ldb (byte 8 8) v))
	(aref a (index+ i +word-0+)) (the card8 (ldb (byte 8 0) v)))
  v)

(defun aref-int16 (a i)
  (declare (type buffer-bytes a)
	   (type array-index i))
  (declare (clx-values int16))
  #.(declare-buffun)
  (the int16
       (logior (the int16
		    (ash (the int8 (aref-int8 a (index+ i +word-1+))) 8))
	       (the card8
		    (aref a (index+ i +word-0+))))))

(defun aset-int16 (v a i)
  (declare (type int16 v)
	   (type buffer-bytes a)
	   (type array-index i))
  #.(declare-buffun)
  (setf (aref a (index+ i +word-1+)) (the card8 (ldb (byte 8 8) v))
	(aref a (index+ i +word-0+)) (the card8 (ldb (byte 8 0) v)))
  v)

(defun aref-card32 (a i)
  (declare (type buffer-bytes a)
	   (type array-index i))
  (declare (clx-values card32))
  #.(declare-buffun)
  (the card32
       (logior (the card32
		    (ash (the card8 (aref a (index+ i +long-3+))) 24))
	       (the card29
		    (ash (the card8 (aref a (index+ i +long-2+))) 16))
	       (the card16
		    (ash (the card8 (aref a (index+ i +long-1+))) 8))
	       (the card8
		    (aref a (index+ i +long-0+))))))

(defun aset-card32 (v a i)
  (declare (type card32 v)
	   (type buffer-bytes a)
	   (type array-index i))
  #.(declare-buffun)
  (setf (aref a (index+ i +long-3+)) (the card8 (ldb (byte 8 24) v))
	(aref a (index+ i +long-2+)) (the card8 (ldb (byte 8 16) v))
	(aref a (index+ i +long-1+)) (the card8 (ldb (byte 8 8) v))
	(aref a (index+ i +long-0+)) (the card8 (ldb (byte 8 0) v)))
  v)

(defun aref-int32 (a i)
  (declare (type buffer-bytes a)
	   (type array-index i))
  (declare (clx-values int32))
  #.(declare-buffun)
  (the int32
       (logior (the int32
		    (ash (the int8 (aref-int8 a (index+ i +long-3+))) 24))
	       (the card29
		    (ash (the card8 (aref a (index+ i +long-2+))) 16))
	       (the card16
		    (ash (the card8 (aref a (index+ i +long-1+))) 8))
	       (the card8
		    (aref a (index+ i +long-0+))))))

(defun aset-int32 (v a i)
  (declare (type int32 v)
	   (type buffer-bytes a)
	   (type array-index i))
  #.(declare-buffun)
  (setf (aref a (index+ i +long-3+)) (the card8 (ldb (byte 8 24) v))
	(aref a (index+ i +long-2+)) (the card8 (ldb (byte 8 16) v))
	(aref a (index+ i +long-1+)) (the card8 (ldb (byte 8 8) v))
	(aref a (index+ i +long-0+)) (the card8 (ldb (byte 8 0) v)))
  v)

(defun aref-card29 (a i)
  (declare (type buffer-bytes a)
	   (type array-index i))
  (declare (clx-values card29))
  #.(declare-buffun)
  (the card29
       (logior (the card29
		    (ash (the card8 (aref a (index+ i +long-3+))) 24))
	       (the card29
		    (ash (the card8 (aref a (index+ i +long-2+))) 16))
	       (the card16
		    (ash (the card8 (aref a (index+ i +long-1+))) 8))
	       (the card8
		    (aref a (index+ i +long-0+))))))

(defun aset-card29 (v a i)
  (declare (type card29 v)
	   (type buffer-bytes a)
	   (type array-index i))
  #.(declare-buffun)
  (setf (aref a (index+ i +long-3+)) (the card8 (ldb (byte 8 24) v))
	(aref a (index+ i +long-2+)) (the card8 (ldb (byte 8 16) v))
	(aref a (index+ i +long-1+)) (the card8 (ldb (byte 8 8) v))
	(aref a (index+ i +long-0+)) (the card8 (ldb (byte 8 0) v)))
  v)

)

(defsetf aref-card8 (a i) (v)
  `(aset-card8 ,v ,a ,i))

(defsetf aref-int8 (a i) (v)
  `(aset-int8 ,v ,a ,i))

(defsetf aref-card16 (a i) (v)
  `(aset-card16 ,v ,a ,i))

(defsetf aref-int16 (a i) (v)
  `(aset-int16 ,v ,a ,i))

(defsetf aref-card32 (a i) (v)
  `(aset-card32 ,v ,a ,i))

(defsetf aref-int32 (a i) (v)
  `(aset-int32 ,v ,a ,i))

(defsetf aref-card29 (a i) (v)
  `(aset-card29 ,v ,a ,i))

;;; Other random conversions

(defun rgb-val->card16 (value)
  ;; Short floats are good enough
  (declare (type rgb-val value))
  (declare (clx-values card16))
  #.(declare-buffun)
  ;; Convert VALUE from float to card16
  (the card16 (values (round (the rgb-val value) #.(/ 1.0s0 #xffff)))))

(defun card16->rgb-val (value) 
  ;; Short floats are good enough
  (declare (type card16 value))
  (declare (clx-values short-float))
  #.(declare-buffun)
  ;; Convert VALUE from card16 to float
  (the short-float (* (the card16 value) #.(/ 1.0s0 #xffff))))

(defun radians->int16 (value)
  ;; Short floats are good enough
  (declare (type angle value))
  (declare (clx-values int16))
  #.(declare-buffun)
  (the int16 (values (round (the angle value) #.(float (/ pi 180.0s0 64.0s0) 0.0s0)))))

(defun int16->radians (value)
  ;; Short floats are good enough
  (declare (type int16 value))
  (declare (clx-values short-float))
  #.(declare-buffun)
  (the short-float (* (the int16 value) #.(coerce (/ pi 180.0 64.0) 'short-float))))


#+(or cmu sbcl clisp ecl) (progn

;;; This overrides the (probably incorrect) definition in clx.lisp.  Since PI
;;; is irrational, there can't be a precise rational representation.  In
;;; particular, the different float approximations will always be /=.  This
;;; causes problems with type checking, because people might compute an
;;; argument in any precision.  What we do is discard all the excess precision
;;; in the value, and see if the protocol encoding falls in the desired range
;;; (64'ths of a degree.)
;;;
(deftype angle () '(satisfies anglep))

(defun anglep (x)
  (and (typep x 'real)
       (<= (* -360 64) (radians->int16 x) (* 360 64))))

)


;;-----------------------------------------------------------------------------
;; Character transformation
;;-----------------------------------------------------------------------------


;;; This stuff transforms chars to ascii codes in card8's and back.
;;; You might have to hack it a little to get it to work for your machine.

(declaim (inline char->card8 card8->char))

(macrolet ((char-translators ()
	     (let ((alist
		     `(#-lispm
		       ;; The normal ascii codes for the control characters.
		       ,@`((#\Return . 13)
			   (#\Linefeed . 10)
			   (#\Rubout . 127)
			   (#\Page . 12)
			   (#\Tab . 9)
			   (#\Backspace . 8)
			   (#\Newline . 10)
			   (#\Space . 32))
		       ;; One the lispm, #\Newline is #\Return, but we'd really like
		       ;; #\Newline to translate to ascii code 10, so we swap the
		       ;; Ascii codes for #\Return and #\Linefeed. We also provide
		       ;; mappings from the counterparts of these control characters
		       ;; so that the character mapping from the lisp machine
		       ;; character set to ascii is invertible.
		       #+lispm
		       ,@`((#\Return . 10)   (,(code-char  10) . ,(char-code #\Return))
			   (#\Linefeed . 13) (,(code-char  13) . ,(char-code #\Linefeed))
			   (#\Rubout . 127)  (,(code-char 127) . ,(char-code #\Rubout))
			   (#\Page . 12)     (,(code-char  12) . ,(char-code #\Page))
			   (#\Tab . 9)       (,(code-char   9) . ,(char-code #\Tab))
			   (#\Backspace . 8) (,(code-char   8) . ,(char-code #\Backspace))
			   (#\Newline . 10)  (,(code-char  10) . ,(char-code #\Newline))
			   (#\Space . 32)    (,(code-char  32) . ,(char-code #\Space)))
		       ;; The rest of the common lisp charater set with the normal
		       ;; ascii codes for them.
		       (#\! . 33) (#\" . 34) (#\# . 35) (#\$ . 36)
		       (#\% . 37) (#\& . 38) (#\' . 39) (#\( . 40)
		       (#\) . 41) (#\* . 42) (#\+ . 43) (#\, . 44)
		       (#\- . 45) (#\. . 46) (#\/ . 47) (#\0 . 48)
		       (#\1 . 49) (#\2 . 50) (#\3 . 51) (#\4 . 52)
		       (#\5 . 53) (#\6 . 54) (#\7 . 55) (#\8 . 56)
		       (#\9 . 57) (#\: . 58) (#\; . 59) (#\< . 60)
		       (#\= . 61) (#\> . 62) (#\? . 63) (#\@ . 64)
		       (#\A . 65) (#\B . 66) (#\C . 67) (#\D . 68)
		       (#\E . 69) (#\F . 70) (#\G . 71) (#\H . 72)
		       (#\I . 73) (#\J . 74) (#\K . 75) (#\L . 76)
		       (#\M . 77) (#\N . 78) (#\O . 79) (#\P . 80)
		       (#\Q . 81) (#\R . 82) (#\S . 83) (#\T . 84)
		       (#\U . 85) (#\V . 86) (#\W . 87) (#\X . 88)
		       (#\Y . 89) (#\Z . 90) (#\[ . 91) (#\\ . 92)
		       (#\] . 93) (#\^ . 94) (#\_ . 95) (#\` . 96)
		       (#\a . 97) (#\b . 98) (#\c . 99) (#\d . 100)
		       (#\e . 101) (#\f . 102) (#\g . 103) (#\h . 104)
		       (#\i . 105) (#\j . 106) (#\k . 107) (#\l . 108)
		       (#\m . 109) (#\n . 110) (#\o . 111) (#\p . 112)
		       (#\q . 113) (#\r . 114) (#\s . 115) (#\t . 116)
		       (#\u . 117) (#\v . 118) (#\w . 119) (#\x . 120)
		       (#\y . 121) (#\z . 122) (#\{ . 123) (#\| . 124)
		       (#\} . 125) (#\~ . 126))))
	       (cond ((dolist (pair alist nil)
			(when (not (= (char-code (car pair)) (cdr pair)))
			  (return t)))
		      `(progn
			 (defconstant *char-to-card8-translation-table*
				      ',(let ((array (make-array
						       (let ((max-char-code 255))
							 (dolist (pair alist)
							   (setq max-char-code
								 (max max-char-code
								      (char-code (car pair)))))
							 (1+ max-char-code))
						       :element-type 'card8)))
					  (dotimes (i (length array))
					    (setf (aref array i) (mod i 256)))
					  (dolist (pair alist)
					    (setf (aref array (char-code (car pair)))
						  (cdr pair)))
					  array))
			 (defconstant *card8-to-char-translation-table*
				      ',(let ((array (make-array 256)))
					  (dotimes (i (length array))
					    (setf (aref array i) (code-char i)))
					  (dolist (pair alist)
					    (setf (aref array (cdr pair)) (car pair)))
					  array))
			 #-Genera
			 (progn
  			   (defun char->card8 (char)
			     (declare (type base-char char))
			     #.(declare-buffun)
			     (the card8 (aref (the (simple-array card8 (*))
						   *char-to-card8-translation-table*)
					      (the array-index (char-code char)))))
			   (defun card8->char (card8)
			     (declare (type card8 card8))
			     #.(declare-buffun)
			     (the base-char
				  (or (aref (the simple-vector *card8-to-char-translation-table*)
					    card8)
				      (error "Invalid CHAR code ~D." card8))))
			   )
			 #+Genera
			 (progn
			   (defun char->card8 (char)
			     (declare lt:(side-effects reader reducible))
			     (aref *char-to-card8-translation-table* (char-code char)))
			   (defun card8->char (card8)
			     (declare lt:(side-effects reader reducible))
			     (aref *card8-to-char-translation-table* card8))
			   )
			 #-Minima
			 (dotimes (i 256)
			   (unless (= i (char->card8 (card8->char i)))
			     (warn "The card8->char mapping is not invertible through char->card8.  Info:~%~S"
				   (list i
					 (card8->char i)
					 (char->card8 (card8->char i))))
			     (return nil)))
			 #-Minima
			 (dotimes (i (length *char-to-card8-translation-table*))
			   (let ((char (code-char i)))
			     (unless (eql char (card8->char (char->card8 char)))
			       (warn "The char->card8 mapping is not invertible through card8->char.  Info:~%~S"
				     (list char
					   (char->card8 char)
					   (card8->char (char->card8 char))))
			       (return nil))))))
		     (t
		      `(progn
			 (defun char->card8 (char)
			   (declare (type base-char char))
			   #.(declare-buffun)
			   (the card8 (char-code char)))
			 (defun card8->char (card8)
			   (declare (type card8 card8))
			   #.(declare-buffun)
			   (the base-char (code-char card8)))
			 ))))))
  (char-translators))

;;-----------------------------------------------------------------------------
;; Process Locking
;;
;;	Common-Lisp doesn't provide process locking primitives, so we define
;;	our own here, based on Zetalisp primitives.  Holding-Lock is very
;;	similar to with-lock on The TI Explorer, and a little more efficient
;;	than with-process-lock on a Symbolics.
;;-----------------------------------------------------------------------------

;;; MAKE-PROCESS-LOCK: Creating a process lock.

#-(or LispM excl Minima sbcl (and cmu mp) (and ecl threads))
(defun make-process-lock (name)
  (declare (ignore name))
  nil)

#+excl
(defun make-process-lock (name)
  (mp:make-process-lock :name name))

#+(and LispM (not Genera))
(defun make-process-lock (name)
  (vector nil name))

#+Genera
(defun make-process-lock (name)
  (process:make-lock name :flavor 'clx-lock))

#+Minima
(defun make-process-lock (name)
  (minima:make-lock name :recursive t))

#+(and cmu mp)
(defun make-process-lock (name)
  (mp:make-lock name))

#+sbcl
(defun make-process-lock (name)
  (sb-thread:make-mutex :name name))

#+(and ecl threads)
(defun make-process-lock (name)
  (mp:make-lock :name name :recursive t))

;;; HOLDING-LOCK: Execute a body of code with a lock held.

;;; The holding-lock macro takes a timeout keyword argument.  EVENT-LISTEN
;;; passes its timeout to the holding-lock macro, so any timeout you want to
;;; work for event-listen you should do for holding-lock.

;; If you're not sharing DISPLAY objects within a multi-processing
;; shared-memory environment, this is sufficient
#-(or lispm excl lcl3.0 Minima sbcl (and CMU mp) (and ecl threads))
(defmacro holding-lock ((locator display &optional whostate &key timeout) &body body)
  (declare (ignore locator display whostate timeout))
  `(progn ,@body))

;;; HOLDING-LOCK for CMU Common Lisp.
;;;
;;; We are not multi-processing, but we use this macro to try to protect
;;; against re-entering request functions.  This can happen if an interrupt
;;; occurs and the handler attempts to use X over the same display connection.
;;; This can happen if the GC hooks are used to notify the user over the same
;;; display connection.  We inhibit GC notifications since display of them
;;; could cause recursive entry into CLX.
;;;
#+(and CMU (not mp))
(defmacro holding-lock ((locator display &optional whostate &key timeout)
			&body body)
  `(let #+cmu((ext:*gc-verbose* nil)
	      (ext:*gc-inhibit-hook* nil)
	      (ext:*before-gc-hooks* nil)
	      (ext:*after-gc-hooks* nil))
	#+sbcl()
     ,locator ,display ,whostate ,timeout
     (system:without-interrupts (progn ,@body))))

;;; HOLDING-LOCK for CMU Common Lisp with multi-processes.
;;;
#+(and cmu mp)
(defmacro holding-lock ((lock display &optional (whostate "CLX wait")
			      &key timeout)
			&body body)
  (declare (ignore display))
  `(mp:with-lock-held (,lock ,whostate ,@(and timeout `(:timeout ,timeout)))
    ,@body))

#+clisp
(defmacro holding-lock ((lock display &optional (whostate "CLX wait")
                              &key timeout)
                        &body body)
  (declare (ignore lock display whostate timeout))
  `(progn
     ,@body))

#+(and ecl threads)
(defmacro holding-lock ((lock display &optional (whostate "CLX wait")
			      &key timeout)
			&body body)
  (declare (ignore display))
  `(mp::with-lock (,lock)
      ,@body))

#+sbcl
(defmacro holding-lock ((lock display &optional (whostate "CLX wait")
			      &key timeout)
			&body body)
  ;; This macro is used by WITH-DISPLAY, which claims to be callable
  ;; recursively.  So, had better use a recursive lock.
  ;;
  ;; FIXME: This is hideously ugly.  If WITH-TIMEOUT handled NIL
  ;; timeouts...
  (declare (ignore display whostate))
  (if timeout
      `(if ,timeout
	   (handler-case
	       (sb-ext:with-timeout ,timeout
		 (sb-thread:with-recursive-lock (,lock)
		   ,@body))
	     (sb-ext:timeout () nil))
	   (sb-thread:with-recursive-lock (,lock)
	     ,@body))
      `(sb-thread:with-recursive-lock (,lock)
	 ,@body)))

#+Genera
(defmacro holding-lock ((locator display &optional whostate &key timeout)
			&body body)
  (declare (ignore whostate))
  `(process:with-lock (,locator :timeout ,timeout)
     (let ((.debug-io. (buffer-debug-io ,display)))
       (scl:let-if .debug-io. ((*debug-io* .debug-io.))
	 ,@body))))

#+(and lispm (not Genera))
(defmacro holding-lock ((locator display &optional whostate &key timeout)
			&body body)
  (declare (ignore display))
  ;; This macro is for use in a multi-process environment.
  (let ((lock (gensym))
	(have-lock (gensym))
	(timeo (gensym)))
    `(let* ((,lock (zl:locf (svref ,locator 0)))
	    (,have-lock (eq (car ,lock) sys:current-process))
	    (,timeo ,timeout))
       (unwind-protect 
	   (when (cond (,have-lock)
		       ((#+explorer si:%store-conditional
			 #-explorer sys:store-conditional
			 ,lock nil sys:current-process))
		       ((null ,timeo)
			(sys:process-lock ,lock nil ,(or whostate "CLX Lock")))
		       ((sys:process-wait-with-timeout
			    ,(or whostate "CLX Lock") (round (* ,timeo 60.))
			  #'(lambda (lock process)
			      (#+explorer si:%store-conditional
			       #-explorer sys:store-conditional
			       lock nil process))
			  ,lock sys:current-process)))
	     ,@body)
	 (unless ,have-lock
	   (#+explorer si:%store-conditional
	    #-explorer sys:store-conditional
	    ,lock sys:current-process nil))))))

;; Lucid has a process locking mechanism as well under release 3.0
#+lcl3.0
(defmacro holding-lock ((locator display &optional whostate &key timeout)
			&body body)
  (declare (ignore display))
  (if timeout
      ;; Hair to support timeout.
      `(let ((.have-lock. (eq ,locator lcl:*current-process*))
	     (.timeout. ,timeout))
	 (unwind-protect
	     (when (cond (.have-lock.)
			 ((conditional-store ,locator nil lcl:*current-process*))
			 ((null .timeout.)
			  (lcl:process-lock ,locator)
			  t)
			 ((lcl:process-wait-with-timeout ,whostate .timeout.
			    #'(lambda ()
				(conditional-store ,locator nil lcl:*current-process*))))
			 ;; abort the PROCESS-UNLOCK if actually timing out
			 (t
			  (setf .have-lock. :abort)
			  nil))
	       ,@body)
	   (unless .have-lock. 
	     (lcl:process-unlock ,locator))))
    `(lcl:with-process-lock (,locator)
       ,@body)))


#+excl
(defmacro holding-lock ((locator display &optional whostate &key timeout)
			&body body)
  (declare (ignore display))
  `(let (.hl-lock. .hl-obtained-lock. .hl-curproc.)
     (unwind-protect
	 (block .hl-doit.
	   (when mp::*scheduler-stack-group* ; fast test for scheduler running
	     (setq .hl-lock. ,locator
		   .hl-curproc. mp::*current-process*)
	     (when (and .hl-curproc.	; nil if in process-wait fun
			(not (eq (mp::process-lock-locker .hl-lock.)
				 .hl-curproc.)))
	       ;; Then we need to grab the lock.
	       ,(if timeout
		    `(if (not (mp::process-lock .hl-lock. .hl-curproc.
						,whostate ,timeout))
			 (return-from .hl-doit. nil))
		  `(mp::process-lock .hl-lock. .hl-curproc.
				     ,@(when whostate `(,whostate))))
	       ;; There is an apparent race condition here.  However, there is
	       ;; no actual race condition -- our implementation of mp:process-
	       ;; lock guarantees that the lock will still be held when it
	       ;; returns, and no interrupt can happen between that and the
	       ;; execution of the next form.  -- jdi 2/27/91
	       (setq .hl-obtained-lock. t)))
	   ,@body)
       (if (and .hl-obtained-lock.
		;; Note -- next form added to allow error handler inside
		;; body to unlock the lock prematurely if it knows that
		;; the current process cannot possibly continue but will
		;; throw out (or is it throw up?).
		(eq (mp::process-lock-locker .hl-lock.) .hl-curproc.))
	   (mp::process-unlock .hl-lock. .hl-curproc.)))))

#+Minima
(defmacro holding-lock ((locator display &optional whostate &key timeout) &body body)
  `(holding-lock-1 #'(lambda () ,@body) ,locator ,display
		   ,@(and whostate `(:whostate ,whostate))
		   ,@(and timeout `(:timeout ,timeout))))

#+Minima
(defun holding-lock-1 (continuation lock display &key (whostate "Lock") timeout)
  (declare (dynamic-extent continuation))
  (declare (ignore display whostate timeout))
  (minima:with-lock (lock)
    (funcall continuation)))

;;; WITHOUT-ABORTS

;;; If you can inhibit asynchronous keyboard aborts inside the body of this
;;; macro, then it is a good idea to do this.  This macro is wrapped around
;;; request writing and reply reading to ensure that requests are atomically
;;; written and replies are atomically read from the stream.

#-(or Genera excl lcl3.0)
(defmacro without-aborts (&body body)
  `(progn ,@body))

#+Genera
(defmacro without-aborts (&body body)
  `(sys:without-aborts (clx "CLX is in the middle of an operation that should be atomic.")
     ,@body))

#+excl
(defmacro without-aborts (&body body)
  `(without-interrupts ,@body))
    
#+lcl3.0
(defmacro without-aborts (&body body)
  `(lcl:with-interruptions-inhibited ,@body))

;;; PROCESS-BLOCK: Wait until a given predicate returns a non-NIL value.
;;; Caller guarantees that PROCESS-WAKEUP will be called after the predicate's
;;; value changes.

#-(or lispm excl lcl3.0 Minima (and sb-thread sbcl) (and cmu mp) (and ecl threads))
(defun process-block (whostate predicate &rest predicate-args)
  (declare (ignore whostate))
  (or (apply predicate predicate-args)
      (error "Program tried to wait with no scheduler.")))

#+Genera
(defun process-block (whostate predicate &rest predicate-args)
  (declare (type function predicate)
	   #+clx-ansi-common-lisp
	   (dynamic-extent predicate)
	   #-clx-ansi-common-lisp
	   (sys:downward-funarg predicate))
  (apply #'process:block-process whostate predicate predicate-args))

#+(and lispm (not Genera))
(defun process-block (whostate predicate &rest predicate-args)
  (declare (type function predicate)
	   #+clx-ansi-common-lisp
	   (dynamic-extent predicate)
	   #-clx-ansi-common-lisp 
	   (sys:downward-funarg predicate))
  (apply #'global:process-wait whostate predicate predicate-args))

#+excl
(defun process-block (whostate predicate &rest predicate-args)
  (if mp::*scheduler-stack-group*
      (apply #'mp::process-wait whostate predicate predicate-args)
      (or (apply predicate predicate-args)
	  (error "Program tried to wait with no scheduler."))))

#+lcl3.0
(defun process-block (whostate predicate &rest predicate-args)
  (declare (dynamic-extent predicate-args))
  (apply #'lcl:process-wait whostate predicate predicate-args))

#+Minima
(defun process-block (whostate predicate &rest predicate-args)
  (declare (type function predicate)
	   (dynamic-extent predicate))
  (apply #'minima:process-wait whostate predicate predicate-args))

#+(and cmu mp)
(defun process-block (whostate predicate &rest predicate-args)
  (declare (type function predicate))
  (mp:process-wait whostate #'(lambda ()
				(apply predicate predicate-args))))

#+(and sbcl sb-thread)
(progn
  (declaim (inline yield))
  (defun yield ()
    (declare (optimize speed (safety 0)))
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "sched_yield" (function sb-alien:int)))
    (values)))

#+(and sbcl sb-thread)
(defun process-block (whostate predicate &rest predicate-args)
  (declare (ignore whostate))
  (declare (type function predicate))
  (loop
   (when (apply predicate predicate-args)
     (return))
   (yield)))

#+(and ecl threads)
(defun process-block (whostate predicate &rest predicate-args)
  (declare (ignore whostate))
  (declare (type function predicate))
  (loop
   (when (apply predicate predicate-args)
     (return))
   (mp:process-yield)))

;;; FIXME: the below implementation for threaded PROCESS-BLOCK using
;;; queues and condition variables might seem better, but in fact it
;;; turns out to make performance extremely suboptimal, at least as
;;; measured by McCLIM on linux 2.4 kernels.  -- CSR, 2003-11-10
#+(or)
(defvar *process-conditions* (make-hash-table))

#+(or)
(defun process-block (whostate predicate &rest predicate-args)
  (declare (ignore whostate))
  (declare (type function predicate))
  (let* ((pid (sb-thread:current-thread-id))
	 (last (gethash  pid *process-conditions*))
	 (lock
	  (or (car last)
	      (sb-thread:make-mutex :name (format nil "lock ~A" pid))))
	 (queue
	  (or (cdr last)
	      (sb-thread:make-waitqueue :name (format nil "queue ~A" pid)))))
    (unless last
      (setf (gethash pid *process-conditions*) (cons lock queue)))
    (sb-thread:with-mutex (lock)
      (loop
       (when (apply predicate predicate-args) (return))
       (handler-case
	   (sb-ext:with-timeout .5
	     (sb-thread:condition-wait queue lock))
	 (sb-ext:timeout ()
	   (format *trace-output* "thread ~A, process-block timed out~%"
		   (sb-thread:current-thread-id) )))))))

;;; PROCESS-WAKEUP: Check some other process' wait function.

(declaim (inline process-wakeup))

#-(or excl Genera Minima (and sbcl sb-thread) (and cmu mp) (and ecl threads))
(defun process-wakeup (process)
  (declare (ignore process))
  nil)

#+excl
(defun process-wakeup (process)
  (let ((curproc mp::*current-process*))
    (when (and curproc process)
      (unless (mp::process-p curproc)
	(error "~s is not a process" curproc))
      (unless (mp::process-p process)
	(error "~s is not a process" process))
      (if (> (mp::process-priority process) (mp::process-priority curproc))
	  (mp::process-allow-schedule process)))))

#+Genera
(defun process-wakeup (process)
  (process:wakeup process))

#+Minima
(defun process-wakeup (process)
  (when process
    (minima:process-wakeup process)))

#+(and cmu mp)
(defun process-wakeup (process)
  (declare (ignore process))
  (mp:process-yield))

#+(and sb-thread sbcl)
(defun process-wakeup (process)
  (declare (ignore process))
  (yield))

#+(and ecl threads)
(defun process-wakeup (process)
  (declare (ignore process))
  (mp:process-yield))

#+(or)
(defun process-wakeup (process)
  (declare (ignore process))
  (destructuring-bind (lock . queue)
      (gethash (sb-thread:current-thread-id) *process-conditions*
	       (cons nil nil))
    (declare (ignore lock))
    (when queue
      (sb-thread:condition-notify queue))))


;;; CURRENT-PROCESS: Return the current process object for input locking and
;;; for calling PROCESS-WAKEUP.

(declaim (inline current-process))

;;; Default return NIL, which is acceptable even if there is a scheduler.

#-(or lispm excl lcl3.0 sbcl Minima (and cmu mp) (and ecl threads))
(defun current-process ()
  nil)

#+lispm
(defun current-process ()
  sys:current-process)

#+excl
(defun current-process ()
  (and mp::*scheduler-stack-group*
       mp::*current-process*))

#+lcl3.0
(defun current-process ()
  lcl:*current-process*)

#+Minima
(defun current-process ()
  (minima:current-process))

#+(or (and cmu mp) (and ecl threads))
(defun current-process ()
  mp:*current-process*)

#+sbcl
(defun current-process ()
  sb-thread:*current-thread*)

;;; WITHOUT-INTERRUPTS -- provide for atomic operations.

#-(or lispm excl lcl3.0 Minima cmu)
(defmacro without-interrupts (&body body)
  `(progn ,@body))

#+(and lispm (not Genera))
(defmacro without-interrupts (&body body)
  `(sys:without-interrupts ,@body))

#+Genera
(defmacro without-interrupts (&body body)
  `(process:with-no-other-processes ,@body))

#+LCL3.0
(defmacro without-interrupts (&body body)
  `(lcl:with-scheduling-inhibited ,@body))

#+Minima
(defmacro without-interrupts (&body body)
  `(minima:with-no-other-processes ,@body))

#+cmu
(defmacro without-interrupts (&body body)
  `(system:without-interrupts ,@body))

#+ecl
(defmacro without-interrupts (&body body)
  `(mp:without-interrupts ,@body))

#+sbcl
(defvar *without-interrupts-sic-lock*
  (sb-thread:make-mutex :name "lock simulating *without-interrupts*"))
#+sbcl
(defmacro without-interrupts (&body body)
  `(sb-thread:with-recursive-lock (*without-interrupts-sic-lock*)
    ,@body))

;;; CONDITIONAL-STORE:

;; This should use GET-SETF-METHOD to avoid evaluating subforms multiple times.
;; It doesn't because CLtL doesn't pass the environment to GET-SETF-METHOD.
#-sbcl
(defmacro conditional-store (place old-value new-value)
  `(without-interrupts
     (cond ((eq ,place ,old-value)
	    (setf ,place ,new-value)
	    t))))

#+sbcl
(progn
  (defvar *conditional-store-lock*
    (sb-thread:make-mutex :name "conditional store"))
  (defmacro conditional-store (place old-value new-value)
    `(sb-thread:with-mutex (*conditional-store-lock*)
       (cond ((eq ,place ,old-value)
	      (setf ,place ,new-value)
	      t)))))

;;;----------------------------------------------------------------------------
;;; IO Error Recovery
;;;	All I/O operations are done within a WRAP-BUF-OUTPUT macro.
;;;	It prevents multiple mindless errors when the network craters.
;;;
;;;----------------------------------------------------------------------------

#-Genera
(defmacro wrap-buf-output ((buffer) &body body)
  ;; Error recovery wrapper
  `(unless (buffer-dead ,buffer)
     ,@body))

#+Genera
(defmacro wrap-buf-output ((buffer) &body body)
  ;; Error recovery wrapper
  `(let ((.buffer. ,buffer))
     (unless (buffer-dead .buffer.)
       (scl:condition-bind
	 (((sys:network-error)
	   #'(lambda (error)
	       (scl:condition-case () 
		    (funcall (buffer-close-function .buffer.) .buffer. :abort t)
		  (sys:network-error))
	       (setf (buffer-dead .buffer.) error)
	       (setf (buffer-output-stream .buffer.) nil)
	       (setf (buffer-input-stream .buffer.) nil)
	       nil)))
	 ,@body))))

#-Genera
(defmacro wrap-buf-input ((buffer) &body body)
  (declare (ignore buffer))
  ;; Error recovery wrapper
  `(progn ,@body))

#+Genera
(defmacro wrap-buf-input ((buffer) &body body)
  ;; Error recovery wrapper
  `(let ((.buffer. ,buffer))
     (scl:condition-bind
       (((sys:network-error)
	 #'(lambda (error)
	     (scl:condition-case () 
		  (funcall (buffer-close-function .buffer.) .buffer. :abort t)
		(sys:network-error))
	     (setf (buffer-dead .buffer.) error)
	     (setf (buffer-output-stream .buffer.) nil)
	     (setf (buffer-input-stream .buffer.) nil)
	     nil)))
       ,@body)))


;;;----------------------------------------------------------------------------
;;; System dependent IO primitives
;;;	Functions for opening, reading writing forcing-output and closing 
;;;	the stream to the server.
;;;----------------------------------------------------------------------------

;;; OPEN-X-STREAM - create a stream for communicating to the appropriate X
;;; server

#-(or explorer Genera lucid kcl ibcl excl Minima CMU sbcl ecl clisp)
(defun open-x-stream (host display protocol)
  host display protocol ;; unused
  (error "OPEN-X-STREAM not implemented yet."))

#+clisp
(defun open-x-stream (host display protocol)
  (declare (ignore protocol)
           (type (integer 0) display))
  (let ((socket
         ;; are we dealing with a localhost?
         (when (or (string= host "")
                   (string= host "unix"))
           ;; ok, try to connect to a AF_UNIX domain socket
           (sys::make-socket-stream "" display))))
    (if socket
        socket
        ;; try to connect by hand
        (let ((host (host-address host)))
          (when host
            ;; Fixme: get a descent ip standard in CLX: a vector!
            (let ((ip (format nil
                              "~{~D~^.~}"
                              (rest host))))
              (socket:socket-connect (+ 6000 display) ip
                                     :element-type '(unsigned-byte 8))))))))


;;; Genera:

;;; TCP and DNA are both layered products, so try to work with either one.

#+Genera
(when (fboundp 'tcp:add-tcp-port-for-protocol)
  (tcp:add-tcp-port-for-protocol :x-window-system 6000))

#+Genera
(when (fboundp 'dna:add-dna-contact-id-for-protocol)
  (dna:add-dna-contact-id-for-protocol :x-window-system "X$X0"))

#+Genera
(net:define-protocol :x-window-system (:x-window-system :byte-stream)
  (:invoke-with-stream ((stream :characters nil :ascii-translation nil))
    stream))

#+Genera
(eval-when (compile)
  (compiler:function-defined 'tcp:open-tcp-stream)
  (compiler:function-defined 'dna:open-dna-bidirectional-stream))

#+Genera
(defun open-x-stream (host display protocol)
  (let ((host (net:parse-host host)))
    (if (or protocol (plusp display))
	;; The protocol was specified or the display isn't 0, so we
	;; can't use the Generic Network System.  If the protocol was
	;; specified, then use that protocol, otherwise, blindly use
	;; TCP.
	(ccase protocol
	  ((:tcp nil)
	   (tcp:open-tcp-stream
	     host (+ *x-tcp-port* display) nil
	     :direction :io
	     :characters nil
	     :ascii-translation nil))
	  ((:dna)
	   (dna:open-dna-bidirectional-stream
	     host (format nil "X$X~D" display)
	     :characters nil
	     :ascii-translation nil)))
      (let ((neti:*invoke-service-automatic-retry* t))
	(net:invoke-service-on-host :x-window-system host)))))

#+explorer
(defun open-x-stream (host display protocol)
  (declare (ignore protocol))
  (net:open-connection-on-medium
    (net:parse-host host)			;Host
    :byte-stream				;Medium
    "X11"					;Logical contact name
    :stream-type :character-stream
    :direction :bidirectional
    :timeout-after-open nil
    :remote-port (+ *x-tcp-port* display)))

#+explorer
(net:define-logical-contact-name
  "X11"
  `((:local "X11")
    (:chaos "X11")
    (:nsp-stream "X11")
    (:tcp ,*x-tcp-port*)))

#+lucid
(defun open-x-stream (host display protocol)
  protocol ;; unused
  (let ((fd (connect-to-server host display)))
    (when (minusp fd)
      (error "Failed to connect to server: ~A ~D" host display))
    (user::make-lisp-stream :input-handle fd
			    :output-handle fd
			    :element-type 'unsigned-byte
			    #-lcl3.0 :stream-type #-lcl3.0 :ephemeral)))

#+(or kcl ibcl)
(defun open-x-stream (host display protocol)
  protocol ;; unused
  (let ((stream (open-socket-stream host display)))
    (if (streamp stream)
	stream
      (error "Cannot connect to server: ~A:~D" host display))))

#+excl
;;
;; Note that since we don't use the CL i/o facilities to do i/o, the display
;; input and output "stream" is really a file descriptor (fixnum).
;;
(defun open-x-stream (host display protocol)
  (declare (ignore protocol));; unused
  (let ((fd (connect-to-server (string host) display)))
    (when (minusp fd)
      (error "Failed to connect to server: ~A ~D" host display))
    fd))

#+Minima
(defun open-x-stream (host display protocol)
  (declare (ignore protocol));; unused
  (minima:open-tcp-stream :foreign-address (apply #'minima:make-ip-address
						  (cdr (host-address host)))
			  :foreign-port (+ *x-tcp-port* display)))

#+(or sbcl ecl)
(defun open-x-stream (host display protocol)  
  (declare (ignore protocol)
           (type (integer 0) display))
  (let ((local-socket-path (unix-socket-path-from-host host display)))
  (socket-make-stream 
     (if local-socket-path
       (let ((s (make-instance 'local-socket :type :stream)))
	   (socket-connect s local-socket-path)
	 s)
       (let ((host (car (host-ent-addresses (get-host-by-name host)))))
	 (when host
	   (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
	     (socket-connect s host (+ 6000 display))
	     s))))
   :element-type '(unsigned-byte 8)
     :input t :output t :buffering :none)))

;;; BUFFER-READ-DEFAULT - read data from the X stream

#+(or Genera explorer)
(defun buffer-read-default (display vector start end timeout)
  ;; returns non-NIL if EOF encountered
  ;; Returns :TIMEOUT when timeout exceeded
  (declare (type display display)
	   (type buffer-bytes vector)
	   (type array-index start end)
	   (type (or null (real 0 *)) timeout))
  #.(declare-buffun)
  (let ((stream (display-input-stream display)))
    (or (cond ((null stream))
	      ((funcall stream :listen) nil)
	      ((and timeout (= timeout 0)) :timeout)
	      ((buffer-input-wait-default display timeout)))
	(multiple-value-bind (ignore eofp)
	    (funcall stream :string-in nil vector start end)
	  eofp))))


#+excl
;;
;; Rewritten 10/89 to not use foreign function interface to do I/O.
;;
(defun buffer-read-default (display vector start end timeout)
  (declare (type display display)
	   (type buffer-bytes vector)
	   (type array-index start end)
	   (type (or null (real 0 *)) timeout))
  #.(declare-buffun)
    
  (let* ((howmany (- end start))
	 (fd (display-input-stream display)))
    (declare (type array-index howmany)
	     (fixnum fd))
    (or (cond ((fd-char-avail-p fd) nil)
	      ((and timeout (= timeout 0)) :timeout)
	      ((buffer-input-wait-default display timeout)))
	(fd-read-bytes fd vector start howmany))))


#+lcl3.0
(defmacro with-underlying-stream ((variable stream display direction) &body body)
  `(let ((,variable
	  (or (getf (display-plist ,display) ',direction)
	      (setf (getf (display-plist ,display) ',direction)
		    (lucid::underlying-stream
		      ,stream ,(if (eq direction 'input) :input :output))))))
     ,@body))

#+lcl3.0
(defun buffer-read-default (display vector start end timeout)
  ;;Note that LISTEN must still be done on "slow stream" or the I/O system
  ;;gets confused.  But reading should be done from "fast stream" for speed.
  ;;We used to inhibit scheduling because there were races in Lucid's
  ;;multitasking system.  Empirical evidence suggests they may be gone now.
  ;;Should you decide you need to inhibit scheduling, do it around the
  ;;lcl:read-array.
  (declare (type display display)
	   (type buffer-bytes vector)
	   (type array-index start end)
	   (type (or null (real 0 *)) timeout))
  #.(declare-buffun)
  (let ((stream (display-input-stream display)))
    (declare (type (or null stream) stream))
    (or (cond ((null stream))
	      ((listen stream) nil)
	      ((and timeout (= timeout 0)) :timeout)
	      ((buffer-input-wait-default display timeout)))
	(with-underlying-stream (stream stream display input)
	  (eq (lcl:read-array stream vector start end nil :eof) :eof)))))

#+Minima
(defun buffer-read-default (display vector start end timeout)
  ;; returns non-NIL if EOF encountered
  ;; Returns :TIMEOUT when timeout exceeded
  (declare (type display display)
	   (type buffer-bytes vector)
	   (type array-index start end)
	   (type (or null (real 0 *)) timeout))
  #.(declare-buffun)
  (let ((stream (display-input-stream display)))
    (or (cond ((null stream))
	      ((listen stream) nil)
	      ((and timeout (= timeout 0)) :timeout)
	      ((buffer-input-wait-default display timeout)))
	(eq :eof (minima:read-vector vector stream nil start end)))))

;;; BUFFER-READ-DEFAULT for CMU Common Lisp.
;;;
;;;    If timeout is 0, then we call LISTEN to see if there is any input.
;;; Timeout 0 is the only case where READ-INPUT dives into BUFFER-READ without
;;; first calling BUFFER-INPUT-WAIT-DEFAULT.
;;;
#+(or CMU sbcl)
(defun buffer-read-default (display vector start end timeout)
  (declare (type display display)
	   (type buffer-bytes vector)
	   (type array-index start end)
	   (type (or null fixnum) timeout))
  #.(declare-buffun)
  (cond ((and (eql timeout 0)
	      (not (listen (display-input-stream display))))
	 :timeout)
	(t
	 (#+cmu system:read-n-bytes
	  #+sbcl sb-sys:read-n-bytes
	  (display-input-stream display)
	  vector start (- end start))
	 nil)))

#+(or ecl clisp)
(defun buffer-read-default (display vector start end timeout)
  (declare (type display display)
	   (type buffer-bytes vector)
	   (type array-index start end)
	   (type (or null fixnum) timeout))
  #.(declare-buffun)
  (cond ((and (eql timeout 0)
	      (not (listen (display-input-stream display))))
	 :timeout)
	(t
	 (read-sequence vector
			(display-input-stream display)
                        :start start
                        :end end)
	 nil)))

;;; WARNING:
;;;	CLX performance will suffer if your lisp uses read-byte for
;;;	receiving all data from the X Window System server.
;;;	You are encouraged to write a specialized version of
;;;	buffer-read-default that does block transfers.
#-(or Genera explorer excl lcl3.0 Minima CMU sbcl ecl clisp)
(defun buffer-read-default (display vector start end timeout)
  (declare (type display display)
	   (type buffer-bytes vector)
	   (type array-index start end)
	   (type (or null (real 0 *)) timeout))
  #.(declare-buffun)
  (let ((stream (display-input-stream display)))
    (declare (type (or null stream) stream))
    (or (cond ((null stream))
	      ((listen stream) nil)
	      ((and timeout (= timeout 0)) :timeout)
	      ((buffer-input-wait-default display timeout)))
	(do* ((index start (index1+ index)))
	     ((index>= index end) nil)
	  (declare (type array-index index))
	  (let ((c (read-byte stream nil nil)))
	    (declare (type (or null card8) c))
	    (if (null c)
		(return t)
	      (setf (aref vector index) (the card8 c))))))))

;;; BUFFER-WRITE-DEFAULT - write data to the X stream

#+(or Genera explorer)
(defun buffer-write-default (vector display start end)
  ;; The default buffer write function for use with common-lisp streams
  (declare (type buffer-bytes vector)
	   (type display display)
	   (type array-index start end))
  #.(declare-buffun)
  (let ((stream (display-output-stream display)))
    (declare (type (or null stream) stream))
    (unless (null stream) 
      (write-string vector stream :start start :end end))))

#+excl
(defun buffer-write-default (vector display start end)
  (declare (type buffer-bytes vector)
	   (type display display)
	   (type array-index start end))
  #.(declare-buffun)
  (excl::filesys-write-bytes (display-output-stream display) vector start
			     (- end start)))
  
#+lcl3.0
(defun buffer-write-default (vector display start end)
  ;;We used to inhibit scheduling because there were races in Lucid's
  ;;multitasking system.  Empirical evidence suggests they may be gone now.
  ;;Should you decide you need to inhibit scheduling, do it around the
  ;;lcl:write-array.
  (declare (type display display)
	   (type buffer-bytes vector)
	   (type array-index start end))
  #.(declare-buffun)
  (let ((stream (display-output-stream display)))
    (declare (type (or null stream) stream))
    (unless (null stream) 
      (with-underlying-stream (stream stream display output)
	(lcl:write-array stream vector start end)))))

#+Minima
(defun buffer-write-default (vector display start end)
  ;; The default buffer write function for use with common-lisp streams
  (declare (type buffer-bytes vector)
	   (type display display)
	   (type array-index start end))
  #.(declare-buffun)
  (let ((stream (display-output-stream display)))
    (declare (type (or null stream) stream))
    (unless (null stream) 
      (minima:write-vector vector stream start end))))

#+CMU
(defun buffer-write-default (vector display start end)
  (declare (type buffer-bytes vector)
	   (type display display)
	   (type array-index start end))
  #.(declare-buffun)
  (system:output-raw-bytes (display-output-stream display) vector start end)
  nil)

#+(or sbcl ecl clisp)
(defun buffer-write-default (vector display start end)
  (declare (type buffer-bytes vector)
	   (type display display)
	   (type array-index start end))
  #.(declare-buffun)
  (write-sequence vector (display-output-stream display) :start start :end end)
  nil)

;;; WARNING:
;;;	CLX performance will be severely degraded if your lisp uses
;;;	write-byte to send all data to the X Window System server.
;;;	You are STRONGLY encouraged to write a specialized version
;;;	of buffer-write-default that does block transfers.

#-(or Genera explorer excl lcl3.0 Minima CMU sbcl clisp ecl)
(defun buffer-write-default (vector display start end)
  ;; The default buffer write function for use with common-lisp streams
  (declare (type buffer-bytes vector)
	   (type display display)
	   (type array-index start end))
  #.(declare-buffun)
  (let ((stream (display-output-stream display)))
    (declare (type (or null stream) stream))
    (unless (null stream)
      (with-vector (vector buffer-bytes)
	(do ((index start (index1+ index)))
	    ((index>= index end))
	  (declare (type array-index index))
	  (write-byte (aref vector index) stream))))))

;;; buffer-force-output-default - force output to the X stream

#+excl
(defun buffer-force-output-default (display)
  ;; buffer-write-default does the actual writing.
  (declare (ignore display)))

#-(or excl)
(defun buffer-force-output-default (display)
  ;; The default buffer force-output function for use with common-lisp streams
  (declare (type display display))
  (let ((stream (display-output-stream display)))
    (declare (type (or null stream) stream))
    (unless (null stream)
      (force-output stream))))

;;; BUFFER-CLOSE-DEFAULT - close the X stream

#+excl
(defun buffer-close-default (display &key abort)
  ;; The default buffer close function for use with common-lisp streams
  (declare (type display display)
	   (ignore abort))
  #.(declare-buffun)
  (excl::filesys-checking-close (display-output-stream display)))

#-(or excl)
(defun buffer-close-default (display &key abort)
  ;; The default buffer close function for use with common-lisp streams
  (declare (type display display))
  #.(declare-buffun)
  (let ((stream (display-output-stream display)))
    (declare (type (or null stream) stream))
    (unless (null stream)
      (close stream :abort abort))))

;;; BUFFER-INPUT-WAIT-DEFAULT - wait for for input to be available for the
;;; buffer.  This is called in read-input between requests, so that a process
;;; waiting for input is abortable when between requests.  Should return
;;; :TIMEOUT if it times out, NIL otherwise.

;;; The default implementation

;; Poll for input every *buffer-read-polling-time* SECONDS.
#-(or Genera explorer excl lcl3.0 CMU sbcl)
(defparameter *buffer-read-polling-time* 0.5)

#-(or Genera explorer excl lcl3.0 CMU sbcl clisp)
(defun buffer-input-wait-default (display timeout)
  (declare (type display display)
	   (type (or null (real 0 *)) timeout))
  (declare (clx-values timeout))
  
  (let ((stream (display-input-stream display)))
    (declare (type (or null stream) stream))
    (cond ((null stream))
	  ((listen stream) nil)
	  ((and timeout (= timeout 0)) :timeout)
	  ((not (null timeout))
	   (multiple-value-bind (npoll fraction)
	       (truncate timeout *buffer-read-polling-time*)
	     (dotimes (i npoll)			; Sleep for a time, then listen again
	       (sleep *buffer-read-polling-time*)
	       (when (listen stream)
		 (return-from buffer-input-wait-default nil)))
	     (when (plusp fraction)
	       (sleep fraction)			; Sleep a fraction of a second
	       (when (listen stream)		; and listen one last time
		 (return-from buffer-input-wait-default nil)))
	     :timeout)))))

#+(or CMU sbcl clisp)
(defun buffer-input-wait-default (display timeout)
  (declare (type display display)
	   (type (or null number) timeout))
  (let ((stream (display-input-stream display)))
    (declare (type (or null stream) stream))
    (cond ((null stream))
	  ((listen stream) nil)
	  ((eql timeout 0) :timeout)
	  (t
	   (if #+sbcl (sb-sys:wait-until-fd-usable (sb-sys:fd-stream-fd stream)
						   :input timeout)
	       #+mp (mp:process-wait-until-fd-usable
		     (system:fd-stream-fd stream) :input timeout)
               #+clisp (multiple-value-bind (sec usec) (floor (or timeout 0))
                         (ext:socket-status stream (and timeout sec)
                                            (round usec 1d-6)))
	       #-(or sbcl mp clisp) (system:wait-until-fd-usable
			       (system:fd-stream-fd stream) :input timeout)
	       nil
	       :timeout)))))

#+Genera
(defun buffer-input-wait-default (display timeout)
  (declare (type display display)
	   (type (or null (real 0 *)) timeout))
  (declare (clx-values timeout))
  (let ((stream (display-input-stream display)))
    (declare (type (or null stream) stream))
    (cond ((null stream))
	  ((scl:send stream :listen) nil)
	  ((and timeout (= timeout 0)) :timeout)
	  ((null timeout) (si:stream-input-block stream "CLX Input"))
	  (t
	   (scl:condition-bind ((neti:protocol-timeout
				  #'(lambda (error)
				      (when (eq stream (scl:send error :stream))
					(return-from buffer-input-wait-default :timeout)))))
	     (neti:with-stream-timeout (stream :input timeout)
	       (si:stream-input-block stream "CLX Input")))))
    nil))

#+explorer
(defun buffer-input-wait-default (display timeout)
  (declare (type display display)
	   (type (or null (real 0 *)) timeout))
  (declare (clx-values timeout))
  (let ((stream (display-input-stream display)))
    (declare (type (or null stream) stream))
    (cond ((null stream))
	  ((zl:send stream :listen) nil)
	  ((and timeout (= timeout 0)) :timeout)
	  ((null timeout)
	   (si:process-wait "CLX Input" stream :listen))
	  (t
	   (unless (si:process-wait-with-timeout
		       "CLX Input" (round (* timeout 60.)) stream :listen)
	     (return-from buffer-input-wait-default :timeout))))
    nil))

#+excl
;;
;; This is used so an 'eq' test may be used to find out whether or not we can
;; safely throw this process out of the CLX read loop.
;;
(defparameter *read-whostate* "waiting for input from X server")

;;
;; Note that this function returns nil on error if the scheduler is running,
;; t on error if not.  This is ok since buffer-read will detect the error.
;;
#+excl
(defun buffer-input-wait-default (display timeout)
  (declare (type display display)
	   (type (or null (real 0 *)) timeout))
  (declare (clx-values timeout))
  (let ((fd (display-input-stream display)))
    (declare (fixnum fd))
    (when (>= fd 0)
      (cond ((fd-char-avail-p fd)
	     nil)
	    
	    ;; Otherwise no bytes were available on the socket
	    ((and timeout (= timeout 0))
	     ;; If there aren't enough and timeout == 0, timeout.
	     :timeout)
	  
	    ;; If the scheduler is running let it do timeouts.
	    (mp::*scheduler-stack-group*
	     #+allegro
	     (if (not
		  (mp:wait-for-input-available fd :whostate *read-whostate*
					       :wait-function #'fd-char-avail-p
					       :timeout timeout))
		 (return-from buffer-input-wait-default :timeout))
	     #-allegro
	     (mp::wait-for-input-available fd :whostate *read-whostate*
					   :wait-function #'fd-char-avail-p))
	    
	    ;; Otherwise we have to handle timeouts by hand, and call select()
	    ;; to block until input is available.  Note we don't really handle
	    ;; the interaction of interrupts and (numberp timeout) here.  XX
	    (t
	     (let ((res 0))
	       (declare (fixnum res))
	       (with-interrupt-checking-on
		(loop
		  (setq res (fd-wait-for-input fd (if (null timeout) 0
						    (truncate timeout))))
		  (cond ((plusp res)	; success
			 (return nil))
			((eq res 0)	; timeout
			 (return :timeout))
			((eq res -1)	; error
			 (return t))
			;; Otherwise we got an interrupt -- go around again.
			)))))))))

	   
#+lcl3.0
(defun buffer-input-wait-default (display timeout)
  (declare (type display display)
	   (type (or null (real 0 *)) timeout)
	   (clx-values timeout))
  #.(declare-buffun)
  (let ((stream (display-input-stream display)))
    (declare (type (or null stream) stream))
    (cond ((null stream))
	  ((listen stream) nil)
	  ((and timeout (= timeout 0)) :timeout)
	  ((with-underlying-stream (stream stream display input)
	     (lucid::waiting-for-input-from-stream stream
               (lucid::with-io-unlocked
		 (if (null timeout)
		     (lcl:process-wait "CLX Input" #'listen stream)
		   (lcl:process-wait-with-timeout
		     "CLX Input" timeout #'listen stream)))))
	   nil)
	  (:timeout))))


;;; BUFFER-LISTEN-DEFAULT - returns T if there is input available for the
;;; buffer. This should never block, so it can be called from the scheduler.

;;; The default implementation is to just use listen.
#-(or excl)
(defun buffer-listen-default (display)
  (declare (type display display))
  (let ((stream (display-input-stream display)))
    (declare (type (or null stream) stream))
    (if (null stream)
	t
      (listen stream))))

#+excl 
(defun buffer-listen-default (display)
  (declare (type display display))
  (let ((fd (display-input-stream display)))
    (declare (type fixnum fd))
    (if (= fd -1)
	t
      (fd-char-avail-p fd))))


;;;----------------------------------------------------------------------------
;;; System dependent speed hacks
;;;----------------------------------------------------------------------------

;;
;; WITH-STACK-LIST is used by WITH-STATE as a memory saving feature.
;; If your lisp doesn't have stack-lists, and you're worried about
;; consing garbage, you may want to re-write this to allocate and
;; initialize lists from a resource.
;;
#-lispm
(defmacro with-stack-list ((var &rest elements) &body body)
  ;; SYNTAX: (WITH-STACK-LIST (var exp1 ... expN) body)
  ;; Equivalent to (LET ((var (MAPCAR #'EVAL '(exp1 ... expN)))) body)
  ;; except that the list produced by MAPCAR resides on the stack and
  ;; therefore DISAPPEARS when WITH-STACK-LIST is exited.
  `(let ((,var (list ,@elements)))
     (declare (type cons ,var)
	      #+clx-ansi-common-lisp (dynamic-extent ,var))
     ,@body))

#-lispm
(defmacro with-stack-list* ((var &rest elements) &body body)
  ;; SYNTAX: (WITH-STACK-LIST* (var exp1 ... expN) body)
  ;; Equivalent to (LET ((var (APPLY #'LIST* (MAPCAR #'EVAL '(exp1 ... expN))))) body)
  ;; except that the list produced by MAPCAR resides on the stack and
  ;; therefore DISAPPEARS when WITH-STACK-LIST is exited.
  `(let ((,var (list* ,@elements)))
     (declare (type cons ,var)
	      #+clx-ansi-common-lisp (dynamic-extent ,var))
     ,@body))

(declaim (inline buffer-replace))

#+lispm
(defun buffer-replace (buf1 buf2 start1 end1 &optional (start2 0))
  (declare (type vector buf1 buf2)
	   (type array-index start1 end1 start2))
  (sys:copy-array-portion buf2 start2 (length buf2) buf1 start1 end1))

#+excl
(defun buffer-replace (target-sequence source-sequence target-start
				       target-end &optional (source-start 0))
  (declare (type buffer-bytes target-sequence source-sequence)
	   (type array-index target-start target-end source-start)
	   (optimize (speed 3) (safety 0)))
  
  (let ((source-end (length source-sequence)))
    (declare (type array-index source-end))
    
    (excl:if* (and (eq target-sequence source-sequence)
		   (> target-start source-start))
       then (let ((nelts (min (- target-end target-start)
			      (- source-end source-start))))
	      (do ((target-index (+ target-start nelts -1) (1- target-index))
		   (source-index (+ source-start nelts -1) (1- source-index)))
		  ((= target-index (1- target-start)) target-sequence)
		(declare (type array-index target-index source-index))
		
		(setf (aref target-sequence target-index)
		  (aref source-sequence source-index))))
       else (do ((target-index target-start (1+ target-index))
		 (source-index source-start (1+ source-index)))
		((or (= target-index target-end) (= source-index source-end))
		 target-sequence)
	      (declare (type array-index target-index source-index))

	      (setf (aref target-sequence target-index)
		(aref source-sequence source-index))))))

#+cmu 
(defun buffer-replace (buf1 buf2 start1 end1 &optional (start2 0))
  (declare (type buffer-bytes buf1 buf2)
	   (type array-index start1 end1 start2))
  #.(declare-buffun)
  (kernel:bit-bash-copy
   buf2 (+ (* start2 #+cmu vm:byte-bits #+sbcl sb-vm:n-byte-bits)
	   (* vm:vector-data-offset #+cmu vm:word-bits #+sbcl sb-vm:n-word-bits))
   buf1 (+ (* start1 #+cmu vm:byte-bits #+sbcl sb-vm:n-byte-bits)
	   (* vm:vector-data-offset #+cmu vm:word-bits #+sbcl sb-vm:n-word-bits))
   (* (- end1 start1) #+cmu vm:byte-bits #+sbcl sb-vm:n-byte-bits)))

#+lucid
;;;The compiler is *supposed* to optimize calls to replace, but in actual
;;;fact it does not.
(defun buffer-replace (buf1 buf2 start1 end1 &optional (start2 0))
  (declare (type buffer-bytes buf1 buf2)
	   (type array-index start1 end1 start2))
  #.(declare-buffun)
  (let ((end2 (lucid::%simple-8bit-vector-length buf2)))
    (declare (type array-index end2))
    (lucid::simple-8bit-vector-replace-internal
      buf1 buf2 start1 end1 start2 end2)))

#+(and clx-overlapping-arrays (not (or lispm excl)))
(defun buffer-replace (buf1 buf2 start1 end1 &optional (start2 0))
  (declare (type vector buf1 buf2)
	   (type array-index start1 end1 start2))
  (replace buf1 buf2 :start1 start1 :end1 end1 :start2 start2))

#-(or lispm lucid excl CMU clx-overlapping-arrays)
(defun buffer-replace (buf1 buf2 start1 end1 &optional (start2 0))
  (declare (type buffer-bytes buf1 buf2)
	   (type array-index start1 end1 start2))
  (replace buf1 buf2 :start1 start1 :end1 end1 :start2 start2))

#+ti
(defun with-location-bindings (sys:&quote bindings &rest body)
  (do ((bindings bindings (cdr bindings)))
      ((null bindings)
       (sys:eval-body-as-progn body))
    (sys:bind (sys:*eval `(sys:locf ,(caar bindings)))
	      (sys:*eval (cadar bindings)))))

#+ti
(compiler:defoptimizer with-location-bindings with-l-b-compiler nil (form)
  (let ((bindings (cadr form))
	(body (cddr form)))
    `(let ()
       ,@(loop for (accessor value) in bindings
	       collect `(si:bind (si:locf ,accessor) ,value))
       ,@body)))

#+ti
(defun (:property with-location-bindings compiler::cw-handler) (exp)
  (let* ((bindlist (mapcar #'compiler::cw-clause (second exp)))
	 (body (compiler::cw-clause (cddr exp))))
    (and compiler::cw-return-expansion-flag
	 (list* (first exp) bindlist body))))

#+(and lispm (not ti))
(defmacro with-location-bindings (bindings &body body)
  `(sys:letf* ,bindings ,@body))

#+lispm
(defmacro with-gcontext-bindings ((gc saved-state indexes ts-index temp-mask temp-gc)
				  &body body)
  ;; don't use svref on LHS because Symbolics didn't define locf for it
  (let* ((local-state (gensym))
	 (bindings `(((aref ,local-state ,ts-index) 0))))	; will become zero anyway
    (dolist (index indexes)
      (push `((aref ,local-state ,index) (svref ,saved-state ,index))
	    bindings))
    `(let ((,local-state (gcontext-local-state ,gc)))
       (declare (type gcontext-state ,local-state))
       (unwind-protect
	   (with-location-bindings ,bindings
	     ,@body)
	 (setf (svref ,local-state ,ts-index) 0)
	 (when ,temp-gc
	   (restore-gcontext-temp-state ,gc ,temp-mask ,temp-gc))
	 (deallocate-gcontext-state ,saved-state)))))

#-lispm
(defmacro with-gcontext-bindings ((gc saved-state indexes ts-index temp-mask temp-gc)
				  &body body)
  (let ((local-state (gensym))
	(resets nil))
    (dolist (index indexes)
      (push `(setf (svref ,local-state ,index) (svref ,saved-state ,index))
	    resets))
    `(unwind-protect
	 (progn
	   ,@body)
       (let ((,local-state (gcontext-local-state ,gc)))
	 (declare (type gcontext-state ,local-state))
	 ,@resets
	 (setf (svref ,local-state ,ts-index) 0))
       (when ,temp-gc
	 (restore-gcontext-temp-state ,gc ,temp-mask ,temp-gc))
       (deallocate-gcontext-state ,saved-state))))

;;;----------------------------------------------------------------------------
;;; How much error detection should CLX do?
;;; Several levels are possible:
;;;
;;; 1. Do the equivalent of check-type on every argument.
;;; 
;;; 2. Simply report TYPE-ERROR.  This eliminates overhead of all the format
;;;    strings generated by check-type.
;;; 
;;; 3. Do error checking only on arguments that are likely to have errors
;;;    (like keyword names)
;;; 
;;; 4. Do error checking only where not doing so may dammage the envirnment
;;;    on a non-tagged machine (i.e. when storing into a structure that has
;;;    been passed in)
;;; 
;;; 5. No extra error detection code.  On lispm's, ASET may barf trying to
;;;    store a non-integer into a number array. 
;;; 
;;; How extensive should the error checking be?  For example, if the server
;;; expects a CARD16, is is sufficient for CLX to check for integer, or
;;; should it also check for non-negative and less than 65536?
;;;----------------------------------------------------------------------------
 
;; The +TYPE-CHECK?+ constant controls how much error checking is done.
;; Possible values are:
;;    NIL      - Don't do any error checking
;;    t        - Do the equivalent of checktype on every argument
;;    :minimal - Do error checking only where errors are likely

;;; This controls macro expansion, and isn't changable at run-time You will
;;; probably want to set this to nil if you want good performance at
;;; production time.
(defconstant +type-check?+
  #+(or Genera Minima CMU sbcl) nil
  #-(or Genera Minima CMU sbcl) t)

;; TYPE? is used to allow the code to do error checking at a different level from
;; the declarations.  It also does some optimizations for systems that don't have
;; good compiler support for TYPEP.  The definitions for CARD32, CARD16, INT16, etc.
;; include range checks.  You can modify TYPE? to do less extensive checking
;; for these types if you desire.

;;
;; ### This comment is a lie!  TYPE? is really also used for run-time type
;; dispatching, not just type checking.  -- Ram.

(defmacro type? (object type)
  #+(or cmu sbcl clisp)
  `(typep ,object ,type)
  #-(or cmu sbcl clisp)
  (if (not (constantp type))
      `(typep ,object ,type)
    (progn
      (setq type (eval type))
      #+(or Genera explorer Minima)
      (if +type-check?+
	  `(locally (declare (optimize safety)) (typep ,object ',type))
	`(typep ,object ',type))
      #-(or Genera explorer Minima)
      (let ((predicate (assoc type
			      '((drawable drawable-p) (window window-p)
				(pixmap pixmap-p) (cursor cursor-p)
				(font font-p) (gcontext gcontext-p)
				(colormap colormap-p) (null null)
				(integer integerp)))))
	(cond (predicate
	       `(,(second predicate) ,object))
	      ((eq type 'generalized-boolean)
	       't)			; Everything is a generalized-boolean.
	      (+type-check?+
	       `(locally (declare (optimize safety)) (typep ,object ',type)))
	      (t
	       `(typep ,object ',type)))))))

;; X-TYPE-ERROR is the function called for type errors.
;; If you want lots of checking, but are concerned about code size,
;; this can be made into a macro that ignores some parameters.

(defun x-type-error (object type &optional error-string)
  (x-error 'x-type-error
	   :datum object
	   :expected-type type
	   :type-string error-string))


;;-----------------------------------------------------------------------------
;; Error handlers
;;    Hack up KMP error signaling using zetalisp until the real thing comes 
;;    along
;;-----------------------------------------------------------------------------

(defun default-error-handler (display error-key &rest key-vals
			      &key asynchronous &allow-other-keys)
  (declare (type generalized-boolean asynchronous)
	   (dynamic-extent key-vals))
  ;; The default display-error-handler.
  ;; It signals the conditions listed in the DISPLAY file.
  (if asynchronous
      (apply #'x-cerror "Ignore" error-key :display display :error-key error-key key-vals)
      (apply #'x-error error-key :display display :error-key error-key key-vals)))

#+(and lispm (not Genera) (not clx-ansi-common-lisp))
(defun x-error (condition &rest keyargs)
  (apply #'sys:signal condition keyargs))

#+(and lispm (not Genera) (not clx-ansi-common-lisp))
(defun x-cerror (proceed-format-string condition &rest keyargs)
  (sys:signal (apply #'zl:make-condition condition keyargs)
	      :proceed-types proceed-format-string))

#+(and Genera (not clx-ansi-common-lisp))
(defun x-error (condition &rest keyargs)
  (declare (dbg:error-reporter))
  (apply #'sys:signal condition keyargs))

#+(and Genera (not clx-ansi-common-lisp))
(defun x-cerror (proceed-format-string condition &rest keyargs)
  (declare (dbg:error-reporter))
  (apply #'sys:signal condition :continue-format-string proceed-format-string keyargs))

#+(or clx-ansi-common-lisp excl lcl3.0 clisp (and CMU mp))
(defun x-error (condition &rest keyargs)
  (declare (dynamic-extent keyargs))
  (apply #'error condition keyargs))

#+(or clx-ansi-common-lisp excl lcl3.0 CMU clisp)
(defun x-cerror (proceed-format-string condition &rest keyargs)
  (declare (dynamic-extent keyargs))
  (apply #'cerror proceed-format-string condition keyargs))

;;; X-ERROR for CMU Common Lisp
;;;
;;; We detect a couple condition types for which we disable event handling in
;;; our system.  This prevents going into the debugger or returning to a
;;; command prompt with CLX repeatedly seeing the same condition.  This occurs
;;; because CMU Common Lisp provides for all events (that is, X, input on file
;;; descriptors, Mach messages, etc.) to come through one routine anyone can
;;; use to wait for input.
;;;
#+(and CMU (not mp)) 
(defun x-error (condition &rest keyargs)
  (let ((condx (apply #'make-condition condition keyargs)))
    (when (eq condition 'closed-display)
      (let ((disp (closed-display-display condx)))
	(warn "Disabled event handling on ~S." disp)
	(ext::disable-clx-event-handling disp)))
    (error condx)))

#-(or lispm ansi-common-lisp excl lcl3.0 CMU sbcl clisp)
(defun x-error (condition &rest keyargs)
  (error "X-Error: ~a"
	 (princ-to-string (apply #'make-condition condition keyargs))))

#-(or lispm clx-ansi-common-lisp excl lcl3.0 CMU sbcl clisp)
(defun x-cerror (proceed-format-string condition &rest keyargs)
  (cerror proceed-format-string "X-Error: ~a"
	 (princ-to-string (apply #'make-condition condition keyargs))))

;; version 15 of Pitman error handling defines the syntax for define-condition to be:
;; DEFINE-CONDITION name (parent-type) [({slot}*) {option}*]
;; Where option is one of: (:documentation doc-string) (:conc-name symbol-or-string)
;; or (:report exp)

#+lcl3.0 
(defmacro define-condition (name parent-types &optional slots &rest args)
  `(lcl:define-condition
     ,name (,(first parent-types))
     ,(mapcar #'(lambda (slot) (if (consp slot) (car slot) slot))
	      slots)
     ,@args))

#+(and excl (not clx-ansi-common-lisp))
(defmacro define-condition (name parent-types &optional slots &rest args)
  `(excl::define-condition
     ,name (,(first parent-types))
     ,(mapcar #'(lambda (slot) (if (consp slot) (car slot) slot))
	      slots)
     ,@args))

#+(and CMU (not clx-ansi-common-lisp))
(defmacro define-condition (name parent-types &optional slots &rest args)
  `(common-lisp:define-condition
     ,name (,(first parent-types))
     ,(mapcar #'(lambda (slot) (if (consp slot) (car slot) slot))
	      slots)
     ,@args))

#+(and lispm (not clx-ansi-common-lisp))
(defmacro define-condition (name parent-types &body options)
  (let ((slot-names
	  (mapcar #'(lambda (slot) (if (consp slot) (car slot) slot))
		  (pop options)))
	(documentation nil)
	(conc-name (concatenate 'string (string name) "-"))	       
	(reporter nil))
    (dolist (item options)
      (ecase (first item)
	(:documentation (setq documentation (second item)))
	(:conc-name (setq conc-name (string (second item))))
	(:report (setq reporter (second item)))))
    `(within-definition (,name define-condition)
       (zl:defflavor ,name ,slot-names ,parent-types
	 :initable-instance-variables
	 #-Genera
	 (:accessor-prefix ,conc-name)
	 #+Genera
	 (:conc-name ,conc-name)
	 #-Genera
	 (:outside-accessible-instance-variables ,@slot-names)
	 #+Genera
	 (:readable-instance-variables ,@slot-names))
       ,(when reporter ;; when no reporter, parent's is inherited
	  `(zl:defmethod #-Genera (,name :report)
	                 #+Genera (dbg:report ,name) (stream)
	      ,(if (stringp reporter)
		   `(write-string ,reporter stream)
		 `(,reporter global:self stream))
	      global:self))
       (zl:compile-flavor-methods ,name)
       ,(when documentation
	  `(setf (documentation name 'type) ,documentation))
       ',name)))

#+(and lispm (not Genera) (not clx-ansi-common-lisp))
(zl:defflavor x-error () (global:error))

#+(and Genera (not clx-ansi-common-lisp))
(scl:defflavor x-error
	((dbg:proceed-types '(:continue))	;
	 continue-format-string)
	(sys:error)
  (:initable-instance-variables continue-format-string))

#+(and Genera (not clx-ansi-common-lisp))
(scl:defmethod (scl:make-instance x-error) (&rest ignore)
  (when (not (sys:variable-boundp continue-format-string))
    (setf dbg:proceed-types (remove :continue dbg:proceed-types))))

#+(and Genera (not clx-ansi-common-lisp))
(scl:defmethod (dbg:proceed x-error :continue) ()
  :continue)

#+(and Genera (not clx-ansi-common-lisp))
(sys:defmethod (dbg:document-proceed-type x-error :continue) (stream)
  (format stream continue-format-string))

#+(or clx-ansi-common-lisp excl lcl3.0 CMU sbcl clisp)
(define-condition x-error (error) ())

#-(or lispm clx-ansi-common-lisp excl lcl3.0 CMU sbcl)
(defstruct x-error
  report-function)

#-(or lispm clx-ansi-common-lisp excl lcl3.0 CMU sbcl)
(defmacro define-condition (name parent-types &body options)
  ;; Define a structure that when printed displays an error message
  (flet ((reporter-for-condition (name)
	   (xintern "." name '-reporter.)))
    (let ((slot-names
	    (mapcar #'(lambda (slot) (if (consp slot) (car slot) slot))
		    (pop options)))
	  (documentation nil)
	  (conc-name (concatenate 'string (string name) "-"))	       
	  (reporter nil)
	  (condition (gensym))
	  (stream (gensym))
	  (report-function (reporter-for-condition name)))
      (dolist (item options)
	(ecase (first item)
	  (:documentation (setq documentation (second item)))
	  (:conc-name (setq conc-name (string (second item))))
	  (:report (setq reporter (second item)))))
      (unless reporter
	(setq report-function (reporter-for-condition (first parent-types))))
      `(within-definition (,name define-condition)
	 (defstruct (,name (:conc-name ,(intern conc-name))
		     (:print-function condition-print)
		     (:include ,(first parent-types)
		      (report-function ',report-function)))
	   ,@slot-names)
	 ,(when documentation
	    `(setf (documentation name 'type) ,documentation))
	 ,(when reporter
	    `(defun ,report-function (,condition ,stream)
	       ,(if (stringp reporter)
		    `(write-string ,reporter ,stream)
		  `(,reporter ,condition ,stream))
	       ,condition))
	 ',name))))

#-(or lispm clx-ansi-common-lisp excl lcl3.0 CMU sbcl clisp)
(defun condition-print (condition stream depth)
  (declare (type x-error condition)
	   (type stream stream)
	   (ignore depth))
  (if *print-escape*
      (print-unreadable-object (condition stream :type t))
    (funcall (x-error-report-function condition) condition stream))
  condition)
  
#-(or lispm clx-ansi-common-lisp excl lcl3.0 CMU sbcl clisp)
(defun make-condition (type &rest slot-initializations)
  (declare (dynamic-extent slot-initializations))
  (let ((make-function (intern (concatenate 'string (string 'make-) (string type))
			       (symbol-package type))))
    (apply make-function slot-initializations)))

#-(or clx-ansi-common-lisp excl lcl3.0 CMU sbcl clisp)
(define-condition type-error (x-error)
  ((datum :reader type-error-datum :initarg :datum)
   (expected-type :reader type-error-expected-type :initarg :expected-type))
  (:report
    (lambda (condition stream)
      (format stream "~s isn't a ~a"
	      (type-error-datum condition)
	      (type-error-expected-type condition)))))


;;-----------------------------------------------------------------------------
;;  HOST hacking
;;-----------------------------------------------------------------------------

#-(or explorer Genera Minima Allegro CMU sbcl ecl clisp)
(defun host-address (host &optional (family :internet))
  ;; Return a list whose car is the family keyword (:internet :DECnet :Chaos)
  ;; and cdr is a list of network address bytes.
  (declare (type stringable host)
	   (type (or null (member :internet :decnet :chaos) card8) family))
  (declare (clx-values list))
  host family
  (error "HOST-ADDRESS not implemented yet."))

#+clisp
(defun host-address (host &optional (family :internet))
  "Return a list whose car is the family keyword (:internet :DECnet :Chaos)
  and cdr is a list of network address bytes."
  (declare (type stringable host)
           (type (or null (member :internet :decnet :chaos) card8) family))
  (declare (clx-values list))
  (labels ((no-host-error ()
             (error "Unknown host ~S" host))
           (no-address-error ()
             (error "Host ~S has no ~S address" host family)))

    (let ((hostent (posix::resolve-host-ipaddr (string host))))
      (when (not (posix::hostent-addr-list hostent))
        (no-host-error))
      (ecase family
        ((:internet nil 0)
         (unless (= (posix::hostent-addrtype hostent) 2)
           (no-address-error))
         (let ((addr (first (posix::hostent-addr-list hostent))))
           (etypecase addr
             (integer
              (list :internet
                    (ldb (byte 8 24) addr)
                    (ldb (byte 8 16) addr)
                    (ldb (byte 8  8) addr)
                    (ldb (byte 8  0) addr)))
             (string
              (let ((parts (read-from-string
                            (nsubstitute #\Space #\. (ext:string-concat
                                                      "(" addr ")")))))
                (check-type parts (cons (unsigned-byte 8)
                                        (cons (unsigned-byte 8)
                                              (cons (unsigned-byte 8)
                                                    (cons (unsigned-byte 8)
                                                          NULL)))))
                (cons :internet parts))))))))))


#+explorer
(defun host-address (host &optional (family :internet))
  ;; Return a list whose car is the family keyword (:internet :DECnet :Chaos)
  ;; and cdr is a list of network address bytes.
  (declare (type stringable host)
	   (type (or null (member :internet :decnet :chaos) card8) family))
  (declare (clx-values list))
  (ecase family
    ((:internet nil 0)
     (let ((addr (ip:get-ip-address host)))
       (unless addr (error "~s isn't an internet host name" host))
       (list :internet
	     (ldb (byte 8 24) addr)
	     (ldb (byte 8 16) addr)
	     (ldb (byte 8 8) addr)
	     (ldb (byte 8 0) addr))))
    ((:chaos 2)
     (let ((addr (first (chaos:chaos-addresses host))))
       (unless addr (error "~s isn't a chaos host name" host))
       (list :chaos
	     (ldb (byte 8 0) addr)
	     (ldb (byte 8 8) addr))))))

#+Genera
(defun host-address (host &optional (family :internet))
  ;; Return a list whose car is the family keyword (:internet :DECnet :Chaos)
  ;; and cdr is a list of network address bytes.
  (declare (type stringable host)
	   (type (or null (member :internet :decnet :chaos) card8) family))
  (declare (clx-values list))
  (setf host (string host))
  (let ((net-type (ecase family
		    ((:internet nil 0) :internet)
		    ((:DECnet 1) :dna)
		    ((:chaos 2) :chaos))))
    (dolist (addr
	      (sys:send (net:parse-host host) :network-addresses)
	      (error "~S isn't a valid ~(~A~) host name" host family))
      (let ((network (car addr))
	    (address (cadr addr)))
	(when (sys:send network :network-typep net-type)
	  (return (ecase family
		    ((:internet nil 0)
		     (multiple-value-bind (a b c d) (tcp:explode-internet-address address)
		       (list :internet a b c d)))
		    ((:DECnet 1)
		     (list :DECnet (ldb (byte 8 0) address) (ldb (byte 8 8) address)))
		    ((:chaos 2)
		     (list :chaos (ldb (byte 8 0) address) (ldb (byte 8 8) address))))))))))

#+Minima
(defun host-address (host &optional (family :internet))
  ;; Return a list whose car is the family keyword (:internet :DECnet :Chaos)
  ;; and cdr is a list of network address bytes.
  (declare (type stringable host)
	   (type (or null (member :internet :decnet :chaos) card8) family))
  (declare (clx-values list))
  (etypecase family
    ((:internet nil 0)
      (list* :internet
	     (multiple-value-list
	       (minima:ip-address-components (minima:parse-ip-address (string host))))))))

#+Allegro
(defun host-address (host &optional (family :internet))
  ;; Return a list whose car is the family keyword (:internet :DECnet :Chaos)
  ;; and cdr is a list of network address bytes.
  (declare (type stringable host)
	   (type (or null (member :internet :decnet :chaos) card8) family))
  (declare (clx-values list))
  (labels ((no-host-error ()
	     (error "Unknown host ~S" host))
	   (no-address-error ()
	     (error "Host ~S has no ~S address" host family)))
    (let ((hostent 0))
      (unwind-protect
	   (progn
	     (setf hostent (ipc::gethostbyname (string host)))
	     (when (zerop hostent)
	       (no-host-error))
	     (ecase family
	       ((:internet nil 0)
		(unless (= (ipc::hostent-addrtype hostent) 2)
		  (no-address-error))
		(assert (= (ipc::hostent-length hostent) 4))
		(let ((addr (ipc::hostent-addr hostent)))
		   (when (or (member comp::.target.
				     '(:hp :sgi4d :sony :dec3100)
				     :test #'eq)
			     (probe-file "/lib/ld.so"))
		     ;; BSD 4.3 based systems require an extra indirection
		     (setq addr (si:memref-int addr 0 0 :unsigned-long)))
		  (list :internet
			(si:memref-int addr 0 0 :unsigned-byte)
			(si:memref-int addr 1 0 :unsigned-byte)
			(si:memref-int addr 2 0 :unsigned-byte)
			(si:memref-int addr 3 0 :unsigned-byte))))))
	(ff:free-cstruct hostent)))))

;#+sbcl
;(require :sockets)

#+CMU
(defun host-address (host &optional (family :internet))
  ;; Return a list whose car is the family keyword (:internet :DECnet :Chaos)
  ;; and cdr is a list of network address bytes.
  (declare (type stringable host)
	   (type (or null (member :internet :decnet :chaos) card8) family))
  (declare (clx-values list))
  (labels ((no-host-error ()
	     (error "Unknown host ~S" host))
	   (no-address-error ()
	     (error "Host ~S has no ~S address" host family)))
    (let ((hostent #+rwi-sockets(ext:lookup-host-entry (string host))
		   #+mna-sockets(net.sbcl.sockets:look-up-host-entry
				 (string host)) 
		   #+db-sockets(sockets:get-host-by-name (string host))))
      (when (not hostent)
	(no-host-error))
      (ecase family
	((:internet nil 0)
	 #+rwi-sockets(unless (= (ext::host-entry-addr-type hostent) 2)
			(no-address-error))
	 #+mna-sockets(unless (= (net.sbcl.sockets::host-entry-addr-type hostent) 2)
			(no-address-error))
	 ;; the following form is for use with SBCL and Daniel
	 ;; Barlow's socket package
	 #+db-sockets(unless (sockets:host-ent-address hostent)
	   (no-address-error))
	 (append (list :internet)
		 #+rwi-sockets
		 (let ((addr (first (ext::host-entry-addr-list hostent))))
			(list (ldb (byte 8 24) addr)
			      (ldb (byte 8 16) addr)
			      (ldb (byte 8  8) addr)
			      (ldb (byte 8  0) addr)))
		 #+mna-sockets
		 (let ((addr (first (net.sbcl.sockets::host-entry-addr-list hostent))))
				(list (ldb (byte 8 24) addr)
				      (ldb (byte 8 16) addr)
				      (ldb (byte 8  8) addr)
				      (ldb (byte 8  0) addr)))
		 ;; the following form is for use with SBCL and Daniel
		 ;; Barlow's socket package
		 #+db-sockets(coerce (sockets:host-ent-address hostent)
				     'list)))))))

#+sbcl
(defun host-address (host &optional (family :internet))
  ;; Return a list whose car is the family keyword (:internet :DECnet :Chaos)
  ;; and cdr is a list of network address bytes.
  (declare (type stringable host)
	   (type (or null (member :internet :decnet :chaos) card8) family))
  (declare (clx-values list))
  (let ((hostent (get-host-by-name (string host))))
    (ecase family
      ((:internet nil 0)
       (cons :internet (coerce (host-ent-address hostent) 'list))))))

#+ecl
(defun host-address (host &optional (family :internet))
  ;; Return a list whose car is the family keyword (:internet :DECnet :Chaos)
  ;; and cdr is a list of network address bytes.
  (declare (type stringable host)
	   (type (or null (member :internet :decnet :chaos) card8) family))
  (declare (clx-values list))
  (labels ((no-host-error ()
	     (error "Unknown host ~S" host)))
    (let ((addr (first (nth-value 3 (si::lookup-host-entry (string host))))))
      (unless addr
	(no-host-error))
      (list :internet
	    (ldb (byte 8 24) addr)
	    (ldb (byte 8 16) addr)
	    (ldb (byte 8  8) addr)
	    (ldb (byte 8  0) addr)))))

#+explorer ;; This isn't required, but it helps make sense of the results from access-hosts
(defun get-host (host-object)
  ;; host-object is a list whose car is the family keyword (:internet :DECnet :Chaos)
  ;; and cdr is a list of network address bytes.
  (declare (type list host-object))
  (declare (clx-values string family))
  (let* ((family (first host-object))
	 (address (ecase family
		    (:internet
		     (dpb (second host-object)
			  (byte 8 24)
			  (dpb (third host-object)
			       (byte 8 16)
			       (dpb (fourth host-object)
				    (byte 8 8)
				    (fifth host-object)))))
		    (:chaos
		     (dpb (third host-object) (byte 8 8) (second host-object))))))
    (when (eq family :internet) (setq family :ip))
    (let ((host (si:get-host-from-address address family)))
      (values (and host (funcall host :name)) family))))

;;; This isn't required, but it helps make sense of the results from access-hosts
#+Genera
(defun get-host (host-object)
  ;; host-object is a list whose car is the family keyword (:internet :DECnet :Chaos)
  ;; and cdr is a list of network address bytes.
  (declare (type list host-object))
  (declare (clx-values string family))
  (let ((family (first host-object)))
    (values (sys:send (net:get-host-from-address 
			(ecase family
			  (:internet
			    (apply #'tcp:build-internet-address (rest host-object)))
			  ((:chaos :DECnet)
			   (dpb (third host-object) (byte 8 8) (second host-object))))
			(net:local-network-of-type (if (eq family :DECnet)
						       :DNA
						       family)))
		      :name)
	    family)))

;;; This isn't required, but it helps make sense of the results from access-hosts
#+Minima
(defun get-host (host-object)
  ;; host-object is a list whose car is the family keyword (:internet :DECnet :Chaos)
  ;; and cdr is a list of network address bytes.
  (declare (type list host-object))
  (declare (clx-values string family))
  (let ((family (first host-object)))
    (values (ecase family
	      (:internet
		(minima:ip-address-string
		  (apply #'minima:make-ip-address (rest host-object)))))
	    family)))


;;-----------------------------------------------------------------------------
;; Whether to use closures for requests or not.
;;-----------------------------------------------------------------------------

;;; If this macro expands to non-NIL, then request and locking code is
;;; compiled in a much more compact format, as the common code is shared, and
;;; the specific code is built into a closure that is funcalled by the shared
;;; code.  If your compiler makes efficient use of closures then you probably
;;; want to make this expand to T, as it makes the code more compact.

(defmacro use-closures ()
  #+(or lispm Minima) t
  #-(or lispm Minima) nil)

#+(or Genera Minima)
(defun clx-macroexpand (form env)
  (declare (ignore env))
  form)

#-(or Genera Minima)
(defun clx-macroexpand (form env)
  (macroexpand form env))


;;-----------------------------------------------------------------------------
;; Resource stuff
;;-----------------------------------------------------------------------------


;;; Utilities 

(defun getenv (name)
  #+excl (sys:getenv name)
  #+lcl3.0 (lcl:environment-variable name)
  #+CMU (cdr (assoc name ext:*environment-list* :test #'string=))
  #+sbcl (sb-ext:posix-getenv name)
  #+ecl (si:getenv name)
  #+clisp (ext:getenv name)
  #-(or sbcl excl lcl3.0 CMU ecl clisp) (progn name nil))

(defun get-host-name ()
  "Return the same hostname as gethostname(3) would"
  ;; machine-instance probably works on a lot of lisps, but clisp is not
  ;; one of them
  #+(or cmu sbcl ecl) (machine-instance)
  ;; resources-pathname was using short-site-name for this purpose
  #+excl (short-site-name)
  #+clisp (let ((s (machine-instance))) (subseq s 0 (position #\Space s)))
  #-(or excl cmu sbcl ecl clisp) (error "get-host-name not implemented"))

(defun homedir-file-pathname (name)
  (and #-(or unix mach) (search "Unix" (software-type) :test #'char-equal)
       (merge-pathnames (user-homedir-pathname) (pathname name))))

;;; DEFAULT-RESOURCES-PATHNAME - The pathname of the resources file to load if
;;; a resource manager isn't running.

(defun default-resources-pathname ()
  (homedir-file-pathname ".Xdefaults"))

;;; RESOURCES-PATHNAME - The pathname of the resources file to load after the
;;; defaults have been loaded.

(defun resources-pathname ()
  (or (let ((string (getenv "XENVIRONMENT")))
	(and string
	     (pathname string)))
      (homedir-file-pathname
       (concatenate 'string ".Xdefaults-" (get-host-name)))))

;;; AUTHORITY-PATHNAME - The pathname of the authority file.

(defun authority-pathname ()
  (or (let ((xauthority (getenv "XAUTHORITY")))
	(and xauthority
	     (pathname xauthority)))
      (homedir-file-pathname ".Xauthority")))

#+ecl
(eval-when (:load-toplevel :execute :compile-toplevel)
  (pushnew :unix *features*))

;;; this particular defaulting behaviour is typical to most Unices, I think
#+unix
(defun get-default-display (&optional display-name)
  "Parse the argument DISPLAY-NAME, or the environment variable $DISPLAY
if it is NIL.  Display names have the format

  [protocol/] [hostname] : [:] displaynumber [.screennumber]

There are two special cases in parsing, to match that done in the Xlib
C language bindings

 - If the hostname is ``unix'' or the empty string, any supplied
   protocol is ignored and a connection is made using the :local
   transport.

 - If a double colon separates hostname from displaynumber, the
   protocol is assumed to be decnet.

Returns a list of (host display-number screen protocol)."
  (let* ((name (or display-name
		   (getenv "DISPLAY")
		   (error "DISPLAY environment variable is not set")))
	 (slash-i (or (position #\/ name) -1))
	 (colon-i (position #\: name :start (1+ slash-i)))
	 (decnet-colon-p (eql (elt name (1+ colon-i)) #\:))
	 (host (subseq name (1+ slash-i) colon-i))
	 (dot-i (and colon-i (position #\. name :start colon-i)))
	 (display (when colon-i
		    (parse-integer name
				   :start (if decnet-colon-p
					      (+ colon-i 2)
					      (1+ colon-i))
				   :end dot-i)))
	 (screen (when dot-i
		   (parse-integer name :start (1+ dot-i))))
	 (protocol
	  (cond ((or (string= host "") (string-equal host "unix")) :local)
		(decnet-colon-p :decnet)
		((> slash-i -1) (intern
				 (string-upcase (subseq name 0 slash-i))
				 :keyword))
		(t :internet))))
    (list host (or display 0) (or screen 0) protocol)))


;;-----------------------------------------------------------------------------
;; GC stuff
;;-----------------------------------------------------------------------------

(defun gc-cleanup ()
  (declare (special *event-free-list*
		    *pending-command-free-list*
		    *reply-buffer-free-lists*
		    *gcontext-local-state-cache*
		    *temp-gcontext-cache*))
  (setq *event-free-list* nil)
  (setq *pending-command-free-list* nil)
  (when (boundp '*reply-buffer-free-lists*)
    (fill *reply-buffer-free-lists* nil))
  (setq *gcontext-local-state-cache* nil)
  (setq *temp-gcontext-cache* nil)
  nil)

#+Genera
(si:define-gc-cleanup clx-cleanup ("CLX Cleanup")
  (gc-cleanup))


;;-----------------------------------------------------------------------------
;; WITH-STANDARD-IO-SYNTAX equivalent, used in (SETF WM-COMMAND)
;;-----------------------------------------------------------------------------

#-(or clx-ansi-common-lisp Genera CMU sbcl ecl)
(defun with-standard-io-syntax-function (function)
  (declare #+lispm
	   (sys:downward-funarg function))
  (let ((*package* (find-package :user))
	(*print-array* t)
	(*print-base* 10)
	(*print-case* :upcase)
	(*print-circle* nil)
	(*print-escape* t)
	(*print-gensym* t)
	(*print-length* nil)
	(*print-level* nil)
	(*print-pretty* nil)
	(*print-radix* nil)
	(*read-base* 10)
	(*read-default-float-format* 'single-float)
	(*read-suppress* nil)
	#+ticl (ticl:*print-structure* t)
	#+lucid (lucid::*print-structure* t))
    (funcall function)))

#-(or clx-ansi-common-lisp Genera CMU sbcl ecl)
(defmacro with-standard-io-syntax (&body body)
  `(flet ((.with-standard-io-syntax-body. () ,@body))
     (with-standard-io-syntax-function #'.with-standard-io-syntax-body.)))


;;-----------------------------------------------------------------------------
;; DEFAULT-KEYSYM-TRANSLATE
;;-----------------------------------------------------------------------------

;;; If object is a character, char-bits are set from state.
;;;
;;; [the following isn't implemented (should it be?)]
;;; If object is a list, it is an alist with entries:
;;; (base-char [modifiers] [mask-modifiers])
;;; When MODIFIERS are specified, this character translation
;;; will only take effect when the specified modifiers are pressed.
;;; MASK-MODIFIERS can be used to specify a set of modifiers to ignore.
;;; When MASK-MODIFIERS is missing, all other modifiers are ignored.
;;; In ambiguous cases, the most specific translation is used.

#-(or (and clx-ansi-common-lisp (not lispm) (not allegro)) CMU sbcl)
(defun default-keysym-translate (display state object)
  (declare (type display display)
	   (type card16 state)
	   (type t object)
	   (clx-values t)
	   (special left-meta-keysym right-meta-keysym
		    left-super-keysym right-super-keysym
		    left-hyper-keysym right-hyper-keysym))
  (when (characterp object)
    (when (logbitp (position :control +state-mask-vector+) state)
      (setf (char-bit object :control) 1))
    (when (or (state-keysymp display state left-meta-keysym)
	      (state-keysymp display state right-meta-keysym))
      (setf (char-bit object :meta) 1))
    (when (or (state-keysymp display state left-super-keysym)
	      (state-keysymp display state right-super-keysym))
      (setf (char-bit object :super) 1))
    (when (or (state-keysymp display state left-hyper-keysym)
	      (state-keysymp display state right-hyper-keysym))
      (setf (char-bit object :hyper) 1)))
  object)

#+(or (and clx-ansi-common-lisp (not lispm) (not allegro)) CMU sbcl clisp)
(defun default-keysym-translate (display state object)
  (declare (type display display)
	   (type card16 state)
	   (type t object)
	   (ignore display state)
	   (clx-values t))
  object)


;;-----------------------------------------------------------------------------
;; Image stuff
;;-----------------------------------------------------------------------------

;;; Types

(deftype pixarray-1-element-type ()
  'bit)

(deftype pixarray-4-element-type ()
  '(unsigned-byte 4))

(deftype pixarray-8-element-type ()
  '(unsigned-byte 8))

(deftype pixarray-16-element-type ()
  '(unsigned-byte 16))

(deftype pixarray-24-element-type ()
  '(unsigned-byte 24))

(deftype pixarray-32-element-type ()
  #-(or Genera Minima) '(unsigned-byte 32)
  #+(or Genera Minima) 'fixnum)

(deftype pixarray-1  ()
  '(#+(or cmu sbcl) simple-array
    #-(or cmu sbcl) array pixarray-1-element-type (* *)))

(deftype pixarray-4  ()
  '(#+(or cmu sbcl) simple-array
    #-(or cmu sbcl) array pixarray-4-element-type (* *)))

(deftype pixarray-8  ()
  '(#+(or cmu sbcl) simple-array
    #-(or cmu sbcl) array pixarray-8-element-type (* *)))

(deftype pixarray-16 ()
  '(#+(or cmu sbcl) simple-array
    #-(or cmu sbcl) array pixarray-16-element-type (* *)))

(deftype pixarray-24 ()
  '(#+(or cmu sbcl) simple-array
    #-(or cmu sbcl) array pixarray-24-element-type (* *)))

(deftype pixarray-32 ()
  '(#+(or cmu sbcl) simple-array #-(or cmu sbcl) array pixarray-32-element-type (* *)))

(deftype pixarray ()
  '(or pixarray-1 pixarray-4 pixarray-8 pixarray-16 pixarray-24 pixarray-32))

(deftype bitmap ()
  'pixarray-1)

;;; WITH-UNDERLYING-SIMPLE-VECTOR 

#+Genera
(defmacro with-underlying-simple-vector
	  ((variable element-type pixarray) &body body)
  (let ((bits-per-element
	  (sys:array-bits-per-element
	    (symbol-value (sys:type-array-element-type element-type)))))
    `(scl:stack-let ((,variable
		      (make-array
			(index-ceiling
			  (index* (array-total-size ,pixarray)
				  (sys:array-element-size ,pixarray))
			  ,bits-per-element)
			:element-type ',element-type
			:displaced-to ,pixarray)))
       (declare (type (vector ,element-type) ,variable))
       ,@body)))

#+lcl3.0
(defmacro with-underlying-simple-vector
	  ((variable element-type pixarray) &body body)
  `(let ((,variable (sys:underlying-simple-vector ,pixarray)))
     (declare (type (simple-array ,element-type (*)) ,variable))
     ,@body))

#+excl
(defmacro with-underlying-simple-vector
	  ((variable element-type pixarray) &body body)
  `(let ((,variable (cdr (excl::ah_data ,pixarray))))
     (declare (type (simple-array ,element-type (*)) ,variable))
     ,@body))

#+(or CMU sbcl)
;;; We do *NOT* support viewing an array as having a different element type.
;;; Element-type is ignored.
;;;
(defmacro with-underlying-simple-vector 
    ((variable element-type pixarray) &body body)
  (declare (ignore element-type))
  `(#+cmu kernel::with-array-data #+sbcl sb-kernel:with-array-data
    ((,variable ,pixarray) (start) (end))
    (declare (ignore start end))
    ,@body))

;;; These are used to read and write pixels from and to CARD8s.

;;; READ-IMAGE-LOAD-BYTE is used to extract 1 and 4 bit pixels from CARD8s.

(defmacro read-image-load-byte (size position integer)
  (unless +image-bit-lsb-first-p+ (setq position (- 7 position)))
  `(the (unsigned-byte ,size)
	(#-Genera ldb #+Genera sys:%logldb
	 (byte ,size ,position)
	 (the card8 ,integer))))

;;; READ-IMAGE-ASSEMBLE-BYTES is used to build 16, 24 and 32 bit pixels from
;;; the appropriate number of CARD8s.

(defmacro read-image-assemble-bytes (&rest bytes)
  (unless +image-byte-lsb-first-p+ (setq bytes (reverse bytes)))
  (let ((it (first bytes))
	(count 0))
    (dolist (byte (rest bytes))
      (setq it
	    `(#-Genera dpb #+Genera sys:%logdpb 
	      (the card8 ,byte)
	      (byte 8 ,(incf count 8))
	      (the (unsigned-byte ,count) ,it))))
    #-Genera `(the (unsigned-byte ,(* (length bytes) 8)) ,it)
    #+Genera it))

;;; WRITE-IMAGE-LOAD-BYTE is used to extract a CARD8 from a 16, 24 or 32 bit
;;; pixel.

(defmacro write-image-load-byte (position integer integer-size)
  integer-size
  (unless +image-byte-lsb-first-p+ (setq position (- integer-size 8 position)))
  `(the card8
	(#-Genera ldb #+Genera sys:%logldb
	 (byte 8 ,position)
	 #-Genera (the (unsigned-byte ,integer-size) ,integer)
	 #+Genera ,integer
	 )))

;;; WRITE-IMAGE-ASSEMBLE-BYTES is used to build a CARD8 from 1 or 4 bit
;;; pixels.

(defmacro write-image-assemble-bytes (&rest bytes)
  (unless +image-bit-lsb-first-p+ (setq bytes (reverse bytes)))
  (let ((size (floor 8 (length bytes)))
	(it (first bytes))
	(count 0))
    (dolist (byte (rest bytes))
      (setq it `(#-Genera dpb #+Genera sys:%logdpb
		 (the (unsigned-byte ,size) ,byte)
		 (byte ,size ,(incf count size))
		 (the (unsigned-byte ,count) ,it))))
    `(the card8 ,it)))

#+(or Genera lcl3.0 excl)
(defvar *computed-image-byte-lsb-first-p* +image-byte-lsb-first-p+)

#+(or Genera lcl3.0 excl)
(defvar *computed-image-bit-lsb-first-p* +image-bit-lsb-first-p+)

;;; The following table gives the bit ordering within bytes (when accessed
;;; sequentially) for a scanline containing 32 bits, with bits numbered 0 to
;;; 31, where bit 0 should be leftmost on the display.  For a given byte
;;; labelled A-B, A is for the most significant bit of the byte, and B is
;;; for the least significant bit.
;;; 
;;; legend:
;;; 	1   scanline-unit = 8
;;; 	2   scanline-unit = 16
;;; 	4   scanline-unit = 32
;;; 	M   byte-order = MostSignificant
;;; 	L   byte-order = LeastSignificant
;;; 	m   bit-order = MostSignificant
;;; 	l   bit-order = LeastSignificant
;;; 
;;; 
;;; format	ordering
;;; 
;;; 1Mm	00-07 08-15 16-23 24-31
;;; 2Mm	00-07 08-15 16-23 24-31
;;; 4Mm	00-07 08-15 16-23 24-31
;;; 1Ml	07-00 15-08 23-16 31-24
;;; 2Ml	15-08 07-00 31-24 23-16
;;; 4Ml	31-24 23-16 15-08 07-00
;;; 1Lm	00-07 08-15 16-23 24-31
;;; 2Lm	08-15 00-07 24-31 16-23
;;; 4Lm	24-31 16-23 08-15 00-07
;;; 1Ll	07-00 15-08 23-16 31-24
;;; 2Ll	07-00 15-08 23-16 31-24
;;; 4Ll	07-00 15-08 23-16 31-24

#+(or Genera lcl3.0 excl) 
(defconstant
  *image-bit-ordering-table*
  '(((1 (00 07) (08 15) (16 23) (24 31)) (nil nil))
    ((2 (00 07) (08 15) (16 23) (24 31)) (nil nil))
    ((4 (00 07) (08 15) (16 23) (24 31)) (nil nil))
    ((1 (07 00) (15 08) (23 16) (31 24)) (nil t))
    ((2 (15 08) (07 00) (31 24) (23 16)) (nil t))
    ((4 (31 24) (23 16) (15 08) (07 00)) (nil t))
    ((1 (00 07) (08 15) (16 23) (24 31)) (t   nil))
    ((2 (08 15) (00 07) (24 31) (16 23)) (t   nil))
    ((4 (24 31) (16 23) (08 15) (00 07)) (t   nil))
    ((1 (07 00) (15 08) (23 16) (31 24)) (t   t))
    ((2 (07 00) (15 08) (23 16) (31 24)) (t   t))
    ((4 (07 00) (15 08) (23 16) (31 24)) (t   t))))
  
#+(or Genera lcl3.0 excl) 
(defun compute-image-byte-and-bit-ordering ()
  (declare (clx-values image-byte-lsb-first-p image-bit-lsb-first-p))
  ;; First compute the ordering 
  (let ((ordering nil)
	(a (make-array '(1 32) :element-type 'bit :initial-element 0)))
    (dotimes (i 4)
      (push (flet ((bitpos (a i n)
		     (declare (optimize (speed 3) (safety 0) (space 0)))
		     (declare (type (simple-array bit (* *)) a)
			      (type fixnum i n))
		     (with-underlying-simple-vector (v (unsigned-byte 8) a)
		       (prog2
			 (setf (aref v i) n)
			 (dotimes (i 32)
			   (unless (zerop (aref a 0 i))
			     (return i)))
			 (setf (aref v i) 0)))))
	      (list (bitpos a i #b10000000)
		    (bitpos a i #b00000001)))
	    ordering))
    (setq ordering (cons (floor +image-unit+ 8) (nreverse ordering)))
    ;; Now from the ordering, compute byte-lsb-first-p and bit-lsb-first-p
    (let ((byte-and-bit-ordering
	    (second (assoc ordering *image-bit-ordering-table*
			   :test #'equal))))
      (unless byte-and-bit-ordering
	(error "Couldn't determine image byte and bit ordering~@
                measured image ordering = ~A"
	       ordering))
      (values-list byte-and-bit-ordering))))

#+(or Genera lcl3.0 excl) 
(multiple-value-setq
  (*computed-image-byte-lsb-first-p* *computed-image-bit-lsb-first-p*)
  (compute-image-byte-and-bit-ordering))

;;; If you can write fast routines that can read and write pixarrays out of a
;;; buffer-bytes, do it!  It makes the image code a lot faster.  The
;;; FAST-READ-PIXARRAY, FAST-WRITE-PIXARRAY and FAST-COPY-PIXARRAY routines
;;; return T if they can do it, NIL if they can't.

;;; FIXME: though we have some #+sbcl -conditionalized routines in
;;; here, they would appear not to work, and so are commented out in
;;; the the FAST-xxx-PIXARRAY routines themseleves.  Investigate
;;; whether the unoptimized routines are often used, and also whether
;;; speeding them up while maintaining correctness is possible.

;;; FAST-READ-PIXARRAY - fill part of a pixarray from a buffer of card8s

#+(or lcl3.0 excl)
(defun fast-read-pixarray-1 (buffer-bbuf index array x y width height  
			     padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type pixarray-1 array)
	   (type card16 x y width height)
	   (type array-index index padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (with-underlying-simple-vector (vector pixarray-1-element-type array)
      (do* ((start (index+ index
			   (index* y padded-bytes-per-line)
			   (index-ceiling x 8))
		   (index+ start padded-bytes-per-line))
	    (y 0 (index1+ y))
	    (left-bits (the array-index (mod (the fixnum (- x)) 8)))
	    (right-bits (index-mod (index- width left-bits) 8))
	    (middle-bits (the fixnum (- (the fixnum (- width left-bits))
					right-bits)))
	    (middle-bytes (index-floor middle-bits 8)))
	   ((index>= y height))
	(declare (type array-index start y
		       left-bits right-bits middle-bytes)
		 (fixnum middle-bits))
	(cond ((< middle-bits 0)
	       (let ((byte (aref buffer-bbuf (index1- start)))
		     (x (array-row-major-index array y left-bits)))
		 (declare (type card8 byte)
			  (type array-index x))
		 (when (index> right-bits 6)
		   (setf (aref vector (index- x 1))
			 (read-image-load-byte 1 7 byte)))
		 (when (and (index> left-bits 1)
			    (index> right-bits 5))
		   (setf (aref vector (index- x 2))
			 (read-image-load-byte 1 6 byte)))
		 (when (and (index> left-bits 2)
			    (index> right-bits 4))
		   (setf (aref vector (index- x 3))
			 (read-image-load-byte 1 5 byte)))
		 (when (and (index> left-bits 3)
			    (index> right-bits 3))
		   (setf (aref vector (index- x 4))
			 (read-image-load-byte 1 4 byte)))
		 (when (and (index> left-bits 4)
			    (index> right-bits 2))
		   (setf (aref vector (index- x 5))
			 (read-image-load-byte 1 3 byte)))
		 (when (and (index> left-bits 5)
			    (index> right-bits 1))
		   (setf (aref vector (index- x 6))
			 (read-image-load-byte 1 2 byte)))
		 (when (index> left-bits 6)
		   (setf (aref vector (index- x 7))
			 (read-image-load-byte 1 1 byte)))))
	      (t
	       (unless (index-zerop left-bits)
		 (let ((byte (aref buffer-bbuf (index1- start)))
		       (x (array-row-major-index array y left-bits)))
		   (declare (type card8 byte)
			    (type array-index x))
		   (setf (aref vector (index- x 1))
			 (read-image-load-byte 1 7 byte))
		   (when (index> left-bits 1)
		     (setf (aref vector (index- x 2))
			   (read-image-load-byte 1 6 byte))
		     (when (index> left-bits 2)
		       (setf (aref vector (index- x 3))
			     (read-image-load-byte 1 5 byte))
		       (when (index> left-bits 3)
			 (setf (aref vector (index- x 4))
			       (read-image-load-byte 1 4 byte))
			 (when (index> left-bits 4)
			   (setf (aref vector (index- x 5))
				 (read-image-load-byte 1 3 byte))
			   (when (index> left-bits 5)
			     (setf (aref vector (index- x 6))
				   (read-image-load-byte 1 2 byte))
			     (when (index> left-bits 6)
			       (setf (aref vector (index- x 7))
				     (read-image-load-byte 1 1 byte))
			       ))))))))
	       (do* ((end (index+ start middle-bytes))
		     (i start (index1+ i))
		     (x (array-row-major-index array y left-bits) (index+ x 8)))
		    ((index>= i end)
		     (unless (index-zerop right-bits)
		       (let ((byte (aref buffer-bbuf end))
			     (x (array-row-major-index
				 array y (index+ left-bits middle-bits))))
			 (declare (type card8 byte)
				  (type array-index x))
			 (setf (aref vector (index+ x 0))
			       (read-image-load-byte 1 0 byte))
			 (when (index> right-bits 1)
			   (setf (aref vector (index+ x 1))
				 (read-image-load-byte 1 1 byte))
			   (when (index> right-bits 2)
			     (setf (aref vector (index+ x 2))
				   (read-image-load-byte 1 2 byte))
			     (when (index> right-bits 3)
			       (setf (aref vector (index+ x 3))
				     (read-image-load-byte 1 3 byte))
			       (when (index> right-bits 4)
				 (setf (aref vector (index+ x 4))
				       (read-image-load-byte 1 4 byte))
				 (when (index> right-bits 5)
				   (setf (aref vector (index+ x 5))
					 (read-image-load-byte 1 5 byte))
				   (when (index> right-bits 6)
				     (setf (aref vector (index+ x 6))
					   (read-image-load-byte 1 6 byte))
				     )))))))))
		 (declare (type array-index end i x))
		 (let ((byte (aref buffer-bbuf i)))
		   (declare (type card8 byte))
		   (setf (aref vector (index+ x 0))
			 (read-image-load-byte 1 0 byte))
		   (setf (aref vector (index+ x 1))
			 (read-image-load-byte 1 1 byte))
		   (setf (aref vector (index+ x 2))
			 (read-image-load-byte 1 2 byte))
		   (setf (aref vector (index+ x 3))
			 (read-image-load-byte 1 3 byte))
		   (setf (aref vector (index+ x 4))
			 (read-image-load-byte 1 4 byte))
		   (setf (aref vector (index+ x 5))
			 (read-image-load-byte 1 5 byte))
		   (setf (aref vector (index+ x 6))
			 (read-image-load-byte 1 6 byte))
		   (setf (aref vector (index+ x 7))
			 (read-image-load-byte 1 7 byte))))
	       )))))
    t)

#+(or lcl3.0 excl)
(defun fast-read-pixarray-4 (buffer-bbuf index array x y width height 
			     padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type pixarray-4 array)
	   (type card16 x y width height)
	   (type array-index index padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (with-underlying-simple-vector (vector pixarray-4-element-type array)
      (do* ((start (index+ index
			   (index* y padded-bytes-per-line)
			   (index-ceiling x 2))
		   (index+ start padded-bytes-per-line))
	    (y 0 (index1+ y))
	    (left-nibbles (the array-index (mod (the fixnum (- (the fixnum x)))
						2)))
	    (right-nibbles (index-mod (index- width left-nibbles) 2))
	    (middle-nibbles (index- width left-nibbles right-nibbles))
	    (middle-bytes (index-floor middle-nibbles 2)))
	   ((index>= y height))
	(declare (type array-index start y
		       left-nibbles right-nibbles middle-nibbles middle-bytes))
	(unless (index-zerop left-nibbles)
	  (setf (aref array y 0)
		(read-image-load-byte
		  4 4 (aref buffer-bbuf (index1- start)))))
	(do* ((end (index+ start middle-bytes))
	      (i start (index1+ i))
	      (x (array-row-major-index array y left-nibbles) (index+ x 2)))
	     ((index>= i end)
	      (unless (index-zerop right-nibbles)
		(setf (aref array y (index+ left-nibbles middle-nibbles))
		      (read-image-load-byte 4 0 (aref buffer-bbuf end)))))
	  (declare (type array-index end i x))
	  (let ((byte (aref buffer-bbuf i)))
	    (declare (type card8 byte))
	    (setf (aref vector (index+ x 0))
		  (read-image-load-byte 4 0 byte))
	    (setf (aref vector (index+ x 1))
		  (read-image-load-byte 4 4 byte))))
	)))
  t)

#+(or Genera lcl3.0 excl CMU sbcl)
(defun fast-read-pixarray-24 (buffer-bbuf index array x y width height 
			      padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type pixarray-24 array)
	   (type card16 width height)
	   (type array-index index padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (with-underlying-simple-vector (vector pixarray-24-element-type array)
      (do* ((start (index+ index
			   (index* y padded-bytes-per-line)
			   (index* x 3))
		   (index+ start padded-bytes-per-line))
	    (y 0 (index1+ y)))
	   ((index>= y height))
	(declare (type array-index start y))
	(do* ((end (index+ start (index* width 3)))
	      (i start (index+ i 3))
	      (x (array-row-major-index array y 0) (index1+ x)))
	     ((index>= i end))
	  (declare (type array-index end i x))
	  (setf (aref vector x)
		(read-image-assemble-bytes
		  (aref buffer-bbuf (index+ i 0))
		  (aref buffer-bbuf (index+ i 1))
		  (aref buffer-bbuf (index+ i 2))))))))
  t)

#+lispm
(defun fast-read-pixarray-using-bitblt
       (bbuf boffset pixarray x y width height padded-bytes-per-line
	bits-per-pixel)
  (#+Genera sys:stack-let* #-Genera let*
   ((dimensions (list (+ y height)
		      (floor (* padded-bytes-per-line 8) bits-per-pixel)))
    (a (make-array
	 dimensions
	 :element-type (array-element-type pixarray)
	 :displaced-to bbuf
	 :displaced-index-offset (floor (* boffset 8) bits-per-pixel))))
   (sys:bitblt boole-1 width height a x y pixarray 0 0))
  t)

#+(or CMU sbcl)
(defun pixarray-element-size (pixarray)
  (let ((eltype (array-element-type pixarray)))
    (cond ((eq eltype 'bit) 1)
	  ((and (consp eltype) (eq (first eltype) 'unsigned-byte))
	   (second eltype))
	  (t
	   (error "Invalid pixarray: ~S." pixarray)))))

#+CMU
;;; COPY-BIT-RECT  --  Internal
;;;
;;;    This is the classic BITBLT operation, copying a rectangular subarray
;;; from one array to another (but source and destination must not overlap.)
;;; Widths are specified in bits.  Neither array can have a non-zero
;;; displacement.  We allow extra random bit-offset to be thrown into the X.
;;;
(defun copy-bit-rect (source source-width sx sy dest dest-width dx dy
			     height width)
  (declare (type array-index source-width sx sy dest-width dx dy height width))
  #.(declare-buffun)
  (kernel::with-array-data ((sdata source)
				 (sstart)
				 (send))
    (declare (ignore send))
    (kernel::with-array-data ((ddata dest)
				   (dstart)
				   (dend))
      (declare (ignore dend))
      (assert (and (zerop sstart) (zerop dstart)))
      (do ((src-idx (index+ (* vm:vector-data-offset #+cmu vm:word-bits #+sbcl sb-vm:n-word-bits)
			    sx (index* sy source-width))
		    (index+ src-idx source-width))
	   (dest-idx (index+ (* vm:vector-data-offset #+cmu vm:word-bits #+sbcl sb-vm:n-word-bits)
			     dx (index* dy dest-width))
		     (index+ dest-idx dest-width))
	   (count height (1- count)))
	  ((zerop count))
	(declare (type array-index src-idx dest-idx count))
	(kernel:bit-bash-copy sdata src-idx ddata dest-idx width)))))


#+sbcl
(defun copy-bit-rect (source source-width sx sy dest dest-width dx dy
			     height width)
  (declare (type array-index source-width sx sy dest-width dx dy height width))
  #.(declare-buffun)
  (sb-kernel:with-array-data ((sdata source) (sstart) (send))
    (declare (ignore send))
    (sb-kernel:with-array-data ((ddata dest) (dstart) (dend))
      (declare (ignore dend))
      (assert (and (zerop sstart) (zerop dstart)))
      (do ((src-idx (index+ (* sb-vm:vector-data-offset sb-vm:n-word-bits)
			    sx (index* sy source-width))
		    (index+ src-idx source-width))
	   (dest-idx (index+ (* sb-vm:vector-data-offset sb-vm:n-word-bits)
			     dx (index* dy dest-width))
		     (index+ dest-idx dest-width))
	   (count height (1- count)))
	  ((zerop count))
	(declare (type array-index src-idx dest-idx count))
	(sb-kernel:ub1-bash-copy sdata src-idx ddata dest-idx width)))))

#+(or CMU sbcl)
(defun fast-read-pixarray-using-bitblt
       (bbuf boffset pixarray x y width height padded-bytes-per-line
	bits-per-pixel)
  (declare (type (array * 2) pixarray))
  #.(declare-buffun)
  (copy-bit-rect bbuf
		 (index* padded-bytes-per-line #+cmu vm:byte-bits #+sbcl sb-vm:n-byte-bits)
		 (index* boffset #+cmu vm:byte-bits #+sbcl sb-vm:n-byte-bits) 0
		 pixarray
		 (index* (array-dimension pixarray 1) bits-per-pixel)
		 x y
		 height
		 (index* width bits-per-pixel))
  t)

#+(or Genera lcl3.0 excl)
(defun fast-read-pixarray-with-swap
       (bbuf boffset pixarray x y width height padded-bytes-per-line
	bits-per-pixel unit byte-lsb-first-p bit-lsb-first-p)
  (declare (type buffer-bytes bbuf)
	   (type array-index boffset
		 padded-bytes-per-line)
	   (type pixarray pixarray)
	   (type card16 x y width height)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (type (member 8 16 32) unit)
	   (type generalized-boolean byte-lsb-first-p bit-lsb-first-p))
  (unless (index= bits-per-pixel 24)
    (let ((pixarray-padded-bits-per-line
	    (if (index= height 1) 0
	      (index* (index- (array-row-major-index pixarray 1 0)
			      (array-row-major-index pixarray 0 0))
		      bits-per-pixel)))
	  (x-bits (index* x bits-per-pixel)))
      (declare (type array-index pixarray-padded-bits-per-line x-bits))
      (when (if (eq *computed-image-byte-lsb-first-p* *computed-image-bit-lsb-first-p*)
		(and (index-zerop (index-mod pixarray-padded-bits-per-line 8))
		     (index-zerop (index-mod x-bits 8)))
	      (and (index-zerop (index-mod pixarray-padded-bits-per-line +image-unit+))
		   (index-zerop (index-mod x-bits +image-unit+))))
	(multiple-value-bind (image-swap-function image-swap-lsb-first-p)
	    (image-swap-function
	      bits-per-pixel 
	      unit byte-lsb-first-p bit-lsb-first-p
	      +image-unit+ *computed-image-byte-lsb-first-p*
	      *computed-image-bit-lsb-first-p*)
	  (declare (type symbol image-swap-function)
		   (type generalized-boolean image-swap-lsb-first-p))
	  (with-underlying-simple-vector (dst card8 pixarray)
	    (funcall
	      (symbol-function image-swap-function) bbuf dst
	      (index+ boffset
		      (index* y padded-bytes-per-line)
		      (index-floor x-bits 8))
	      0 (index-ceiling (index* width bits-per-pixel) 8)
	      padded-bytes-per-line
	      (index-floor pixarray-padded-bits-per-line 8)
	      height image-swap-lsb-first-p)))
	t))))

(defun fast-read-pixarray (bbuf boffset pixarray
			   x y width height padded-bytes-per-line
			   bits-per-pixel
			   unit byte-lsb-first-p bit-lsb-first-p)
  (declare (type buffer-bytes bbuf)
	   (type array-index boffset
		 padded-bytes-per-line)
	   (type pixarray pixarray)
	   (type card16 x y width height)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (type (member 8 16 32) unit)
	   (type generalized-boolean byte-lsb-first-p bit-lsb-first-p))
  (progn bbuf boffset pixarray x y width height padded-bytes-per-line
	 bits-per-pixel unit byte-lsb-first-p bit-lsb-first-p)
  (or
    #+(or Genera lcl3.0 excl)
    (fast-read-pixarray-with-swap
      bbuf boffset pixarray x y width height padded-bytes-per-line
      bits-per-pixel unit byte-lsb-first-p bit-lsb-first-p)
    (let ((function
	    (or #+lispm
		(and (= (sys:array-element-size pixarray) bits-per-pixel)
		     (zerop (index-mod padded-bytes-per-line 4))
		     (zerop (index-mod
			      (* #+Genera (sys:array-row-span pixarray)
				 #-Genera (array-dimension pixarray 1)
				 bits-per-pixel)
			      32))
		     #'fast-read-pixarray-using-bitblt)
		#+(or CMU)
		(and (index= (pixarray-element-size pixarray) bits-per-pixel)
		     #'fast-read-pixarray-using-bitblt)
		#+(or lcl3.0 excl)
		(and (index= bits-per-pixel 1)
		     #'fast-read-pixarray-1)
		#+(or lcl3.0 excl)
		(and (index= bits-per-pixel 4)
		     #'fast-read-pixarray-4)
		#+(or Genera lcl3.0 excl CMU)
		(and (index= bits-per-pixel 24)
		     #'fast-read-pixarray-24))))
      (when function
	(read-pixarray-internal
	  bbuf boffset pixarray x y width height padded-bytes-per-line
	  bits-per-pixel function
	  unit byte-lsb-first-p bit-lsb-first-p
	  +image-unit+ +image-byte-lsb-first-p+ +image-bit-lsb-first-p+)))))

;;; FAST-WRITE-PIXARRAY - copy part of a pixarray into an array of CARD8s

#+(or lcl3.0 excl)
(defun fast-write-pixarray-1 (buffer-bbuf index array x y width height
			      padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type pixarray-1 array)
	   (type card16 x y width height)
	   (type array-index index padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (with-underlying-simple-vector (vector pixarray-1-element-type array)
      (do* ((h 0 (index1+ h))
	    (y y (index1+ y))
	    (right-bits (index-mod width 8))
	    (middle-bits (index- width right-bits))
	    (middle-bytes (index-ceiling middle-bits 8))
	    (start index (index+ start padded-bytes-per-line)))
	   ((index>= h height))
	(declare (type array-index h y right-bits middle-bits
		       middle-bytes start))
	(do* ((end (index+ start middle-bytes))
	      (i start (index1+ i))
	      (start-x x)
	      (x (array-row-major-index array y start-x) (index+ x 8)))
	     ((index>= i end)
	      (unless (index-zerop right-bits)
		(let ((x (array-row-major-index
			   array y (index+ start-x middle-bits))))
		  (declare (type array-index x))
		  (setf (aref buffer-bbuf end)
			(write-image-assemble-bytes
			  (aref vector (index+ x 0))
			  (if (index> right-bits 1)
			      (aref vector (index+ x 1))
			    0)
			  (if (index> right-bits 2)
			      (aref vector (index+ x 2))
			    0)
			  (if (index> right-bits 3)
			      (aref vector (index+ x 3))
			    0)
			  (if (index> right-bits 4)
			      (aref vector (index+ x 4))
			    0)
			  (if (index> right-bits 5)
			      (aref vector (index+ x 5))
			    0)
			  (if (index> right-bits 6)
			      (aref vector (index+ x 6))
			    0)
			  0)))))
	  (declare (type array-index end i start-x x))
	  (setf (aref buffer-bbuf i)
		(write-image-assemble-bytes
		  (aref vector (index+ x 0))
		  (aref vector (index+ x 1))
		  (aref vector (index+ x 2))
		  (aref vector (index+ x 3))
		  (aref vector (index+ x 4))
		  (aref vector (index+ x 5))
		  (aref vector (index+ x 6))
		  (aref vector (index+ x 7))))))))
  t)

#+(or lcl3.0 excl)
(defun fast-write-pixarray-4 (buffer-bbuf index array x y width height
			      padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type pixarray-4 array)
	   (type int16 x y)
	   (type card16 width height)
	   (type array-index index padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (with-underlying-simple-vector (vector pixarray-4-element-type array)
      (do* ((h 0 (index1+ h))
	    (y y (index1+ y))
	    (right-nibbles (index-mod width 2))
	    (middle-nibbles (index- width right-nibbles))
	    (middle-bytes (index-ceiling middle-nibbles 2))
	    (start index (index+ start padded-bytes-per-line)))
	   ((index>= h height))
	(declare (type array-index h y right-nibbles middle-nibbles
		       middle-bytes start))
	(do* ((end (index+ start middle-bytes))
	      (i start (index1+ i))
	      (start-x x)
	      (x (array-row-major-index array y start-x) (index+ x 2)))
	     ((index>= i end)
	      (unless (index-zerop right-nibbles)
		(setf (aref buffer-bbuf end)
		      (write-image-assemble-bytes
			(aref array y (index+ start-x middle-nibbles))
			0))))
	  (declare (type array-index end i start-x x))
	  (setf (aref buffer-bbuf i)
		(write-image-assemble-bytes
		  (aref vector (index+ x 0))
		  (aref vector (index+ x 1))))))))
  t)

#+(or Genera lcl3.0 excl CMU sbcl)
(defun fast-write-pixarray-24 (buffer-bbuf index array x y width height
			       padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type pixarray-24 array)
	   (type int16 x y)
	   (type card16 width height)
	   (type array-index index padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (with-underlying-simple-vector (vector pixarray-24-element-type array)
      (do* ((h 0 (index1+ h))
	    (y y (index1+ y))
	    (start index (index+ start padded-bytes-per-line)))
	   ((index>= h height))
	(declare (type array-index y start))
	(do* ((end (index+ start (index* width 3)))
	      (i start (index+ i 3))
	      (x (array-row-major-index array y x) (index1+ x)))
	     ((index>= i end))
	  (declare (type array-index end i x))
	  (let ((pixel (aref vector x)))
	    (declare (type pixarray-24-element-type pixel))
	    (setf (aref buffer-bbuf (index+ i 0))
		  (write-image-load-byte 0 pixel 24))
	    (setf (aref buffer-bbuf (index+ i 1))
		  (write-image-load-byte 8 pixel 24))
	    (setf (aref buffer-bbuf (index+ i 2))
		  (write-image-load-byte 16 pixel 24)))))))
  t)

#+lispm
(defun fast-write-pixarray-using-bitblt
       (bbuf boffset pixarray x y width height padded-bytes-per-line
	bits-per-pixel)
  (#+Genera sys:stack-let* #-Genera let*
   ((dimensions (list (+ y height)
		      (floor (* padded-bytes-per-line 8) bits-per-pixel)))
    (a (make-array
	 dimensions
	 :element-type (array-element-type pixarray)
	 :displaced-to bbuf
	 :displaced-index-offset (floor (* boffset 8) bits-per-pixel))))
   (sys:bitblt boole-1 width height pixarray x y a 0 0))
  t)

#+(or CMU sbcl)
(defun fast-write-pixarray-using-bitblt
       (bbuf boffset pixarray x y width height padded-bytes-per-line
	bits-per-pixel)
  #.(declare-buffun)
  (copy-bit-rect pixarray
		 (index* (array-dimension pixarray 1) bits-per-pixel)
		 x y
		 bbuf
		 (index* padded-bytes-per-line #+cmu vm:byte-bits #+sbcl sb-vm:n-byte-bits)
		 (index* boffset #+cmu vm:byte-bits #+sbcl sb-vm:n-byte-bits) 0
		 height
		 (index* width bits-per-pixel))
  t)

#+(or Genera lcl3.0 excl)
(defun fast-write-pixarray-with-swap
       (bbuf boffset pixarray x y width height padded-bytes-per-line
	bits-per-pixel unit byte-lsb-first-p bit-lsb-first-p)
  (declare (type buffer-bytes bbuf)
	   (type pixarray pixarray)
	   (type card16 x y width height)
	   (type array-index boffset padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (type (member 8 16 32) unit)
	   (type generalized-boolean byte-lsb-first-p bit-lsb-first-p))
  (unless (index= bits-per-pixel 24)
    (let ((pixarray-padded-bits-per-line
	    (if (index= height 1) 0
	      (index* (index- (array-row-major-index pixarray 1 0)
			      (array-row-major-index pixarray 0 0))
		      bits-per-pixel)))
	  (pixarray-start-bit-offset
	    (index* (array-row-major-index pixarray y x)
		    bits-per-pixel)))
      (declare (type array-index pixarray-padded-bits-per-line
		     pixarray-start-bit-offset))
      (when (if (eq *computed-image-byte-lsb-first-p* *computed-image-bit-lsb-first-p*)
		(and (index-zerop (index-mod pixarray-padded-bits-per-line 8))
		     (index-zerop (index-mod pixarray-start-bit-offset 8)))
	      (and (index-zerop (index-mod pixarray-padded-bits-per-line +image-unit+))
		   (index-zerop (index-mod pixarray-start-bit-offset +image-unit+))))
	(multiple-value-bind (image-swap-function image-swap-lsb-first-p)
	    (image-swap-function
	      bits-per-pixel
	      +image-unit+ *computed-image-byte-lsb-first-p*
	      *computed-image-bit-lsb-first-p*
	      unit byte-lsb-first-p bit-lsb-first-p)
	  (declare (type symbol image-swap-function)
		   (type generalized-boolean image-swap-lsb-first-p))
	  (with-underlying-simple-vector (src card8 pixarray)
	    (funcall
	      (symbol-function image-swap-function)
	      src bbuf (index-floor pixarray-start-bit-offset 8) boffset
	      (index-ceiling (index* width bits-per-pixel) 8)
	      (index-floor pixarray-padded-bits-per-line 8)
	      padded-bytes-per-line height image-swap-lsb-first-p))
	  t)))))

(defun fast-write-pixarray (bbuf boffset pixarray x y width height
			    padded-bytes-per-line bits-per-pixel
			    unit byte-lsb-first-p bit-lsb-first-p)
  (declare (type buffer-bytes bbuf)
	   (type pixarray pixarray)
	   (type card16 x y width height)
	   (type array-index boffset padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (type (member 8 16 32) unit)
	   (type generalized-boolean byte-lsb-first-p bit-lsb-first-p))
  (progn bbuf boffset pixarray x y width height padded-bytes-per-line
	 bits-per-pixel unit byte-lsb-first-p bit-lsb-first-p)
  (or
    #+(or Genera lcl3.0 excl)
    (fast-write-pixarray-with-swap
      bbuf boffset pixarray x y width height padded-bytes-per-line
      bits-per-pixel unit byte-lsb-first-p bit-lsb-first-p)
    (let ((function
	    (or #+lispm
		(and (= (sys:array-element-size pixarray) bits-per-pixel)
		     (zerop (index-mod padded-bytes-per-line 4))
		     (zerop (index-mod
			      (* #+Genera (sys:array-row-span pixarray)
				 #-Genera (array-dimension pixarray 1)
				 bits-per-pixel)
			      32))
		     #'fast-write-pixarray-using-bitblt)
		#+(or CMU)
		(and (index= (pixarray-element-size pixarray) bits-per-pixel)
		     #'fast-write-pixarray-using-bitblt)
		#+(or lcl3.0 excl)
		(and (index= bits-per-pixel 1)
		     #'fast-write-pixarray-1)
		#+(or lcl3.0 excl)
		(and (index= bits-per-pixel 4)
		     #'fast-write-pixarray-4)
		#+(or Genera lcl3.0 excl CMU)
		(and (index= bits-per-pixel 24)
		     #'fast-write-pixarray-24))))
      (when function
	(write-pixarray-internal
	  bbuf boffset pixarray x y width height padded-bytes-per-line
	  bits-per-pixel function
	  +image-unit+ +image-byte-lsb-first-p+ +image-bit-lsb-first-p+
	  unit byte-lsb-first-p bit-lsb-first-p)))))

;;; FAST-COPY-PIXARRAY - copy part of a pixarray into another

(defun fast-copy-pixarray (pixarray copy x y width height bits-per-pixel)
  (declare (type pixarray pixarray copy)
	   (type card16 x y width height)
	   (type (member 1 4 8 16 24 32) bits-per-pixel))
  (progn pixarray copy x y width height bits-per-pixel nil)
  (or
    #+(or lispm CMU)
    (let* ((pixarray-padded-pixels-per-line
	     #+Genera (sys:array-row-span pixarray)
	     #-Genera (array-dimension pixarray 1))
	   (pixarray-padded-bits-per-line
	     (* pixarray-padded-pixels-per-line bits-per-pixel))
	   (copy-padded-pixels-per-line
	     #+Genera (sys:array-row-span copy)
	     #-Genera (array-dimension copy 1))
	   (copy-padded-bits-per-line
	     (* copy-padded-pixels-per-line bits-per-pixel)))
      #-(or CMU)
      (when (and (= (sys:array-element-size pixarray) bits-per-pixel)
		 (zerop (index-mod pixarray-padded-bits-per-line 32))
		 (zerop (index-mod copy-padded-bits-per-line 32)))
	(sys:bitblt boole-1 width height pixarray x y copy 0 0)
	t)
      #+(or CMU)
      (when (index= (pixarray-element-size pixarray)
		    (pixarray-element-size copy)
		    bits-per-pixel)
	(copy-bit-rect pixarray pixarray-padded-bits-per-line x y
		       copy copy-padded-bits-per-line 0 0
		       height
		       (index* width bits-per-pixel))
	t))
	
    #+(or lcl3.0 excl)
    (unless (index= bits-per-pixel 24)
      (let ((pixarray-padded-bits-per-line
	      (if (index= height 1) 0
		(index* (index- (array-row-major-index pixarray 1 0)
				(array-row-major-index pixarray 0 0))
			bits-per-pixel)))
	    (copy-padded-bits-per-line
	      (if (index= height 1) 0
		(index* (index- (array-row-major-index copy 1 0)
				(array-row-major-index copy 0 0))
			bits-per-pixel)))
	    (pixarray-start-bit-offset
	      (index* (array-row-major-index pixarray y x)
		      bits-per-pixel)))
	(declare (type array-index pixarray-padded-bits-per-line
		       copy-padded-bits-per-line pixarray-start-bit-offset))
	(when (if (eq *computed-image-byte-lsb-first-p* *computed-image-bit-lsb-first-p*)
		  (and (index-zerop (index-mod pixarray-padded-bits-per-line 8))
		       (index-zerop (index-mod copy-padded-bits-per-line 8))
		       (index-zerop (index-mod pixarray-start-bit-offset 8)))
		(and (index-zerop (index-mod pixarray-padded-bits-per-line +image-unit+))
		     (index-zerop (index-mod copy-padded-bits-per-line +image-unit+))
		     (index-zerop (index-mod pixarray-start-bit-offset +image-unit+))))
	  (with-underlying-simple-vector (src card8 pixarray)
	    (with-underlying-simple-vector (dst card8 copy)
	      (image-noswap
		src dst
		(index-floor pixarray-start-bit-offset 8) 0
		(index-ceiling (index* width bits-per-pixel) 8)
		(index-floor pixarray-padded-bits-per-line 8)
		(index-floor copy-padded-bits-per-line 8)
		height nil)))
	  t)))
    #+(or lcl3.0 excl)
    (macrolet
      ((copy (type element-type)
	 `(let ((pixarray pixarray)
		(copy copy))
	    (declare (type ,type pixarray copy))
	    #.(declare-buffun)
	    (with-underlying-simple-vector (src ,element-type pixarray)
	      (with-underlying-simple-vector (dst ,element-type copy)
		(do* ((dst-y 0 (index1+ dst-y))
		      (src-y y (index1+ src-y)))
		     ((index>= dst-y height))
		  (declare (type card16 dst-y src-y))
		  (do* ((dst-idx (array-row-major-index copy dst-y 0)
				 (index1+ dst-idx))
			(dst-end (index+ dst-idx width))
			(src-idx (array-row-major-index pixarray src-y x)
				 (index1+ src-idx)))
		       ((index>= dst-idx dst-end))
		    (declare (type array-index dst-idx src-idx dst-end))
		    (setf (aref dst dst-idx)
			  (the ,element-type (aref src src-idx))))))))))
      (ecase bits-per-pixel
	(1  (copy pixarray-1  pixarray-1-element-type))
	(4  (copy pixarray-4  pixarray-4-element-type))
	(8  (copy pixarray-8  pixarray-8-element-type))
	(16 (copy pixarray-16 pixarray-16-element-type))
	(24 (copy pixarray-24 pixarray-24-element-type))
	(32 (copy pixarray-32 pixarray-32-element-type)))
      t)))
