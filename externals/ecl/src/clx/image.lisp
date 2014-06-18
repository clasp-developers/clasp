;;; -*- Mode:Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;; CLX Image functions

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

(defmacro with-image-data-buffer ((buffer size) &body body)
  (declare (indentation 0 4 1 1))
  `(let ((.reply-buffer. (allocate-reply-buffer ,size)))
     (declare (type reply-buffer .reply-buffer.))
     (unwind-protect
	 (let ((,buffer (reply-ibuf8 .reply-buffer.)))
	   (declare (type buffer-bytes ,buffer))
	   (with-vector (,buffer buffer-bytes)
	     ,@body))
       (deallocate-reply-buffer .reply-buffer.))))

(def-clx-class (image (:constructor nil) (:copier nil) (:predicate nil))
  ;; Public structure
  (width 0 :type card16 :read-only t)
  (height 0 :type card16 :read-only t)
  (depth 1 :type card8 :read-only t)
  (plist nil :type list))

;; Image-Plist accessors:
(defmacro image-name (image) `(getf (image-plist ,image) :name))
(defmacro image-x-hot (image) `(getf (image-plist ,image) :x-hot))
(defmacro image-y-hot (image) `(getf (image-plist ,image) :y-hot))
(defmacro image-red-mask (image) `(getf (image-plist ,image) :red-mask))
(defmacro image-blue-mask (image) `(getf (image-plist ,image) :blue-mask))
(defmacro image-green-mask (image) `(getf (image-plist ,image) :green-mask))

(defun print-image (image stream depth)
  (declare (type image image)
	   (ignore depth))
  (print-unreadable-object (image stream :type t)
    (when (image-name image)
      (write-string (string (image-name image)) stream)
      (write-string " " stream))
    (prin1 (image-width image) stream)
    (write-string "x" stream)
    (prin1 (image-height image) stream)
    (write-string "x" stream)
    (prin1 (image-depth image) stream)))

(defconstant +empty-data-x+ '#.(make-sequence '(array card8 (*)) 0))

(defconstant +empty-data-z+
  '#.(make-array '(0 0) :element-type 'pixarray-1-element-type))

(def-clx-class (image-x (:include image) (:copier nil)
			(:print-function print-image))
  ;; Use this format for shoveling image data
  ;; Private structure. Accessors for these NOT exported.
  (format :z-pixmap :type (member :bitmap :xy-pixmap :z-pixmap))
  (bytes-per-line 0 :type card16)
  (bits-per-pixel 1 :type (member 1 4 8 16 24 32))
  (bit-lsb-first-p +image-bit-lsb-first-p+ :type generalized-boolean)	; Bit order
  (byte-lsb-first-p +image-byte-lsb-first-p+ :type generalized-boolean)	; Byte order
  (data +empty-data-x+ :type (array card8 (*)))			; row-major
  (unit +image-unit+ :type (member 8 16 32))			; Bitmap unit
  (pad +image-pad+ :type (member 8 16 32))			; Scanline pad
  (left-pad 0 :type card8))					; Left pad

(def-clx-class (image-xy (:include image) (:copier nil)
			 (:print-function print-image))
  ;; Public structure
  ;; Use this format for image processing
  (bitmap-list nil :type list)) ;; list of bitmaps

(def-clx-class (image-z (:include image) (:copier nil)
			(:print-function print-image))
  ;; Public structure
  ;; Use this format for image processing
  (bits-per-pixel 1 :type (member 1 4 8 16 24 32))
  (pixarray +empty-data-z+ :type pixarray))

(defun create-image (&key width height depth
		     (data (required-arg data))
		     plist name x-hot y-hot
		     red-mask blue-mask green-mask
		     bits-per-pixel format bytes-per-line
		     (byte-lsb-first-p 
		       #+clx-little-endian t
		       #-clx-little-endian nil)
		     (bit-lsb-first-p
		       #+clx-little-endian t
		       #-clx-little-endian nil)
		     unit pad left-pad)
  ;; Returns an image-x image-xy or image-z structure, depending on the
  ;; type of the :DATA parameter.
  (declare
    (type (or null card16) width height)	; Required
    (type (or null card8) depth)		; Defualts to 1
    (type (or buffer-bytes			; Returns image-x
	      list				; Returns image-xy
	      pixarray) data)			; Returns image-z
    (type list plist)
    (type (or null stringable) name)
    (type (or null card16) x-hot y-hot)
    (type (or null pixel) red-mask blue-mask green-mask)
    (type (or null (member 1 4 8 16 24 32)) bits-per-pixel)
    
    ;; The following parameters are ignored for image-xy and image-z:
    (type (or null (member :bitmap :xy-pixmap :z-pixmap))
	  format)				; defaults to :z-pixmap
    (type (or null card16) bytes-per-line)
    (type generalized-boolean byte-lsb-first-p bit-lsb-first-p)
    (type (or null (member 8 16 32)) unit pad)
    (type (or null card8) left-pad))
  (declare (clx-values image))
  (let ((image
	  (etypecase data
	    (buffer-bytes			; image-x
	      (let ((data data))
		(declare (type buffer-bytes data))
		(unless depth (setq depth (or bits-per-pixel 1)))
		(unless format
		  (setq format (if (= depth 1) :xy-pixmap :z-pixmap)))
		(unless bits-per-pixel
		  (setq bits-per-pixel
			(cond ((eq format :xy-pixmap) 1)
			      ((index> depth 24) 32)
			      ((index> depth 16) 24)
			      ((index> depth 8)  16)
			      ((index> depth 4)   8)
			      ((index> depth 1)   4)
			      (t                  1))))
		(unless width (required-arg width))
		(unless height (required-arg height))
		(unless bytes-per-line
		  (let* ((pad (or pad 8))
			 (bits-per-line (index* width bits-per-pixel))
			 (padded-bits-per-line
			   (index* (index-ceiling bits-per-line pad) pad)))
		    (declare (type array-index pad bits-per-line
				   padded-bits-per-line))
		    (setq bytes-per-line (index-ceiling padded-bits-per-line 8))))
		(unless unit (setq unit +image-unit+))
		(unless pad
		  (setq pad
			(dolist (pad '(32 16 8))
			  (when (and (index<= pad +image-pad+)
				     (zerop
				       (index-mod
					 (index* bytes-per-line 8) pad)))
			    (return pad)))))
		(unless left-pad (setq left-pad 0))
		(make-image-x
		  :width width :height height :depth depth :plist plist
		  :format format :data data
		  :bits-per-pixel bits-per-pixel 
		  :bytes-per-line bytes-per-line
		  :byte-lsb-first-p byte-lsb-first-p
		  :bit-lsb-first-p bit-lsb-first-p
		  :unit unit :pad pad :left-pad left-pad)))
	    (list				; image-xy
	      (let ((data data))
		(declare (type list data))
		(unless depth (setq depth (length data)))
		(when data
		  (unless width (setq width (array-dimension (car data) 1)))
		  (unless height (setq height (array-dimension (car data) 0))))
		(make-image-xy
		  :width width :height height :plist plist :depth depth
		  :bitmap-list data)))
	    (pixarray				; image-z
	      (let ((data data))
		(declare (type pixarray data))
		(unless width (setq width (array-dimension data 1)))
		(unless height (setq height (array-dimension data 0)))
		(unless bits-per-pixel
		  (setq bits-per-pixel
			(etypecase data
			  (pixarray-32 32)
			  (pixarray-24 24)
			  (pixarray-16 16)
			  (pixarray-8   8)
			  (pixarray-4   4)
			  (pixarray-1   1)))))
	      (unless depth (setq depth bits-per-pixel))
	      (make-image-z
		:width width :height height :depth depth :plist plist
		:bits-per-pixel bits-per-pixel :pixarray data)))))
    (declare (type image image))
    (when name (setf (image-name image) name))
    (when x-hot (setf (image-x-hot image) x-hot))
    (when y-hot (setf (image-y-hot image) y-hot))
    (when red-mask (setf (image-red-mask image) red-mask))
    (when blue-mask (setf (image-blue-mask image) blue-mask))
    (when green-mask (setf (image-green-mask image) green-mask))
    image))

;;;-----------------------------------------------------------------------------
;;; Swapping stuff

(defun image-noswap
       (src dest srcoff destoff srclen srcinc destinc height lsb-first-p)
  (declare (type buffer-bytes src dest)
	   (type array-index srcoff destoff srclen srcinc destinc)
	   (type card16 height)
	   (type generalized-boolean lsb-first-p)
	   (ignore lsb-first-p))
  #.(declare-buffun)
  (if (index= srcinc destinc)
      (buffer-replace
	dest src destoff
	(index+ destoff (index* srcinc (index1- height)) srclen)
	srcoff)
    (do* ((h height (index1- h))
	  (srcstart srcoff (index+ srcstart srcinc))
	  (deststart destoff (index+ deststart destinc))
	  (destend (index+ deststart srclen) (index+ deststart srclen)))
	 ((index-zerop h))
      (declare (type array-index srcstart deststart destend)
	       (type card16 h))
      (buffer-replace dest src deststart destend srcstart))))

(defun image-swap-two-bytes
       (src dest srcoff destoff srclen srcinc destinc height lsb-first-p)
  (declare (type buffer-bytes src dest)
	   (type array-index srcoff destoff srclen srcinc destinc)
	   (type card16 height)
	   (type generalized-boolean lsb-first-p))
  #.(declare-buffun)
  (with-vector (src buffer-bytes)
    (with-vector (dest buffer-bytes)
      (do ((length (index* (index-ceiling srclen 2) 2))
	   (h height (index1- h))
	   (srcstart srcoff (index+ srcstart srcinc))
	   (deststart destoff (index+ deststart destinc)))
	  ((index-zerop h))
	(declare (type array-index length srcstart deststart)
		 (type card16 h))
	(when (and (index= h 1) (not (index= srclen length)))
	  (index-decf length 2)
	  (if lsb-first-p
	      (setf (aref dest (index1+ (index+ deststart length)))
		    (the card8 (aref src (index+ srcstart length))))
	    (setf (aref dest (index+ deststart length))
		  (the card8 (aref src (index1+ (index+ srcstart length)))))))
	(do ((i length (index- i 2))
	     (srcidx srcstart (index+ srcidx 2))
	     (destidx deststart (index+ destidx 2)))
	    ((index-zerop i))
	  (declare (type array-index i srcidx destidx))
	  (setf (aref dest destidx)
		(the card8 (aref src (index1+ srcidx))))
	  (setf (aref dest (index1+ destidx))
		(the card8 (aref src srcidx))))))))

(defun image-swap-three-bytes
       (src dest srcoff destoff srclen srcinc destinc height lsb-first-p)
  (declare (type buffer-bytes src dest)
	   (type array-index srcoff destoff srclen srcinc destinc)
	   (type card16 height)
	   (type generalized-boolean lsb-first-p))
  #.(declare-buffun)
  (with-vector (src buffer-bytes)
    (with-vector (dest buffer-bytes)
      (do ((length (index* (index-ceiling srclen 3) 3))
	   (h height (index1- h))
	   (srcstart srcoff (index+ srcstart srcinc))
	   (deststart destoff (index+ deststart destinc)))
	  ((index-zerop h))
	(declare (type array-index length srcstart deststart)
		 (type card16 h))
	(when (and (index= h 1) (not (index= srclen length)))
	  (index-decf length 3)
	  (when (index= (index- srclen length) 2)
	    (setf (aref dest (index+ deststart length 1))
		  (the card8 (aref src (index+ srcstart length 1)))))
	  (if lsb-first-p
	      (setf (aref dest (index+ deststart length 2))
		    (the card8 (aref src (index+ srcstart length))))
	    (setf (aref dest (index+ deststart length))
		  (the card8 (aref src (index+ srcstart length 2))))))
	(do ((i length (index- i 3))
	     (srcidx srcstart (index+ srcidx 3))
	     (destidx deststart (index+ destidx 3)))
	    ((index-zerop i))
	  (declare (type array-index i srcidx destidx))
	  (setf (aref dest destidx)
		(the card8 (aref src (index+ srcidx 2))))
	  (setf (aref dest (index1+ destidx))
		(the card8 (aref src (index1+ srcidx))))
	  (setf (aref dest (index+ destidx 2))
		(the card8 (aref src srcidx))))))))

(defun image-swap-four-bytes
       (src dest srcoff destoff srclen srcinc destinc height lsb-first-p)
  (declare (type buffer-bytes src dest)
	   (type array-index srcoff destoff srclen srcinc destinc)
	   (type card16 height)
	   (type generalized-boolean lsb-first-p))
  #.(declare-buffun)
  (with-vector (src buffer-bytes)
    (with-vector (dest buffer-bytes)
      (do ((length (index* (index-ceiling srclen 4) 4))
	   (h height (index1- h))
	   (srcstart srcoff (index+ srcstart srcinc))
	   (deststart destoff (index+ deststart destinc)))
	  ((index-zerop h))
	(declare (type array-index length srcstart deststart)
		 (type card16 h))
	(when (and (index= h 1) (not (index= srclen length)))
	  (index-decf length 4)
	  (unless lsb-first-p
	    (setf (aref dest (index+ deststart length))
		  (the card8 (aref src (index+ srcstart length 3)))))
	  (when (if lsb-first-p
		    (index= (index- srclen length) 3)
		  (not (index-zerop (index-logand srclen 2))))
	    (setf (aref dest (index+ deststart length 1))
		  (the card8 (aref src (index+ srcstart length 2)))))
	  (when (if (null lsb-first-p)
		    (index= (index- srclen length) 3)
		  (not (index-zerop (index-logand srclen 2))))
	    (setf (aref dest (index+ deststart length 2))
		  (the card8 (aref src (index+ srcstart length 1)))))
	  (when lsb-first-p
	    (setf (aref dest (index+ deststart length 3))
		  (the card8 (aref src (index+ srcstart length))))))
	(do ((i length (index- i 4))
	     (srcidx srcstart (index+ srcidx 4))
	     (destidx deststart (index+ destidx 4)))
	    ((index-zerop i))
	  (declare (type array-index i srcidx destidx))
	  (setf (aref dest destidx)
		(the card8 (aref src (index+ srcidx 3))))
	  (setf (aref dest (index1+ destidx))
		(the card8 (aref src (index+ srcidx 2))))
	  (setf (aref dest (index+ destidx 2))
		(the card8 (aref src (index1+ srcidx))))
	  (setf (aref dest (index+ destidx 3))
		(the card8 (aref src srcidx))))))))

(defun image-swap-words
       (src dest srcoff destoff srclen srcinc destinc height lsb-first-p)
  (declare (type buffer-bytes src dest)
	   (type array-index srcoff destoff srclen srcinc destinc)
	   (type card16 height)
	   (type generalized-boolean lsb-first-p))
  #.(declare-buffun)
  (with-vector (src buffer-bytes)
    (with-vector (dest buffer-bytes)
      (do ((length (index* (index-ceiling srclen 4) 4))
	   (h height (index1- h))
	   (srcstart srcoff (index+ srcstart srcinc))
	   (deststart destoff (index+ deststart destinc)))
	  ((index-zerop h))
	(declare (type array-index length srcstart deststart)
		 (type card16 h))
	(when (and (index= h 1) (not (index= srclen length)))
	  (index-decf length 4)
	  (unless lsb-first-p
	    (setf (aref dest (index+ deststart length 1))
		  (the card8 (aref src (index+ srcstart length 3)))))
	  (when (if lsb-first-p
		    (index= (index- srclen length) 3)
		  (not (index-zerop (index-logand srclen 2))))
	    (setf (aref dest (index+ deststart length))
		  (the card8 (aref src (index+ srcstart length 2)))))
	  (when (if (null lsb-first-p)
		    (index= (index- srclen length) 3)
		  (not (index-zerop (index-logand srclen 2))))
	    (setf (aref dest (index+ deststart length 3))
		  (the card8 (aref src (index+ srcstart length 1)))))
	  (when lsb-first-p
	    (setf (aref dest (index+ deststart length 2))
		  (the card8 (aref src (index+ srcstart length))))))
	(do ((i length (index- i 4))
	     (srcidx srcstart (index+ srcidx 4))
	     (destidx deststart (index+ destidx 4)))
	    ((index-zerop i))
	  (declare (type array-index i srcidx destidx))
	  (setf (aref dest destidx)
		(the card8 (aref src (index+ srcidx 2))))
	  (setf (aref dest (index1+ destidx))
		(the card8 (aref src (index+ srcidx 3))))
	  (setf (aref dest (index+ destidx 2))
		(the card8 (aref src srcidx)))
	  (setf (aref dest (index+ destidx 3))
		(the card8 (aref src (index1+ srcidx)))))))))

(defun image-swap-nibbles
       (src dest srcoff destoff srclen srcinc destinc height lsb-first-p)
  (declare (type buffer-bytes src dest)
	   (type array-index srcoff destoff srclen srcinc destinc)
	   (type card16 height)
	   (type generalized-boolean lsb-first-p)
	   (ignore lsb-first-p))
  #.(declare-buffun)
  (with-vector (src buffer-bytes)
    (with-vector (dest buffer-bytes)
      (do ((h height (index1- h))
	   (srcstart srcoff (index+ srcstart srcinc))
	   (deststart destoff (index+ deststart destinc)))
	  ((index-zerop h))
	(declare (type array-index srcstart deststart)
		 (type card16 h))
	(do ((i srclen (index1- i))
	     (srcidx srcstart (index1+ srcidx))
	     (destidx deststart (index1+ destidx)))
	    ((index-zerop i))
	  (declare (type array-index i srcidx destidx))
	  (setf (aref dest destidx)
		(the card8
		     (let ((byte (aref src srcidx)))
		       (declare (type card8 byte))
		       (dpb (the card4 (ldb (byte 4 0) byte))
			    (byte 4 4)
			    (the card4 (ldb (byte 4 4) byte)))))))))))

(defun image-swap-nibbles-left
       (src dest srcoff destoff srclen srcinc destinc height lsb-first-p)
  (declare (type buffer-bytes src dest)
	   (type array-index srcoff destoff srclen srcinc destinc)
	   (type card16 height)
	   (type generalized-boolean lsb-first-p)
	   (ignore lsb-first-p))
  #.(declare-buffun)
  (with-vector (src buffer-bytes)
    (with-vector (dest buffer-bytes)
      (do ((h height (index1- h))
	   (srcstart srcoff (index+ srcstart srcinc))
	   (deststart destoff (index+ deststart destinc)))
	  ((index-zerop h))
	(declare (type array-index srcstart deststart)
		 (type card16 h))
	(do ((i srclen (index1- i))
	     (srcidx srcstart (index1+ srcidx))
	     (destidx deststart (index1+ destidx)))
	    ((index= i 1)
	     (setf (aref dest destidx)
		   (the card8
			(let ((byte1 (aref src srcidx)))
			  (declare (type card8 byte1))
			  (dpb (the card4 (ldb (byte 4 0) byte1))
			       (byte 4 4)
			       0)))))
	  (declare (type array-index i srcidx destidx))
	  (setf (aref dest destidx)
		(the card8
		     (let ((byte1 (aref src srcidx))
			   (byte2 (aref src (index1+ srcidx))))
		       (declare (type card8 byte1 byte2))
		       (dpb (the card4 (ldb (byte 4 0) byte1))
			    (byte 4 4)
			    (the card4 (ldb (byte 4 4) byte2)))))))))))

(defconstant +image-byte-reverse+
 '#.(coerce
     '#(
	0 128 64 192 32 160 96 224 16 144 80 208 48 176 112 240
	8 136 72 200 40 168 104 232 24 152 88 216 56 184 120 248
	4 132 68 196 36 164 100 228 20 148 84 212 52 180 116 244
	12 140 76 204 44 172 108 236 28 156 92 220 60 188 124 252
	2 130 66 194 34 162 98 226 18 146 82 210 50 178 114 242
	10 138 74 202 42 170 106 234 26 154 90 218 58 186 122 250
	6 134 70 198 38 166 102 230 22 150 86 214 54 182 118 246
	14 142 78 206 46 174 110 238 30 158 94 222 62 190 126 254
	1 129 65 193 33 161 97 225 17 145 81 209 49 177 113 241
	9 137 73 201 41 169 105 233 25 153 89 217 57 185 121 249
	5 133 69 197 37 165 101 229 21 149 85 213 53 181 117 245
	13 141 77 205 45 173 109 237 29 157 93 221 61 189 125 253
	3 131 67 195 35 163 99 227 19 147 83 211 51 179 115 243
	11 139 75 203 43 171 107 235 27 155 91 219 59 187 123 251
	7 135 71 199 39 167 103 231 23 151 87 215 55 183 119 247
	15 143 79 207 47 175 111 239 31 159 95 223 63 191 127 255)
     '(vector card8)))

(defun image-swap-bits
       (src dest srcoff destoff srclen srcinc destinc height lsb-first-p)
  (declare (type buffer-bytes src dest)
	   (type array-index srcoff destoff srclen srcinc destinc)
	   (type card16 height)
	   (type generalized-boolean lsb-first-p)
	   (ignore lsb-first-p))
  #.(declare-buffun)
  (with-vector (src buffer-bytes)
    (with-vector (dest buffer-bytes)
      (let ((byte-reverse +image-byte-reverse+))
	(with-vector (byte-reverse (simple-array card8 (256)))
	  (macrolet ((br (byte)
		       `(the card8 (aref byte-reverse (the card8 ,byte)))))
	    (do ((h height (index1- h))
		 (srcstart srcoff (index+ srcstart srcinc))
		 (deststart destoff (index+ deststart destinc)))
		((index-zerop h))
	      (declare (type array-index srcstart deststart)
		       (type card16 h))
	      (do ((i srclen (index1- i))
		   (srcidx srcstart (index1+ srcidx))
		   (destidx deststart (index1+ destidx)))
		  ((index-zerop i))
		(declare (type array-index i srcidx destidx))
		(setf (aref dest destidx) (br (aref src srcidx)))))))))))

(defun image-swap-bits-and-two-bytes
       (src dest srcoff destoff srclen srcinc destinc height lsb-first-p)
  (declare (type buffer-bytes src dest)
	   (type array-index srcoff destoff srclen srcinc destinc)
	   (type card16 height)
	   (type generalized-boolean lsb-first-p))
  #.(declare-buffun)
  (with-vector (src buffer-bytes)
    (with-vector (dest buffer-bytes)
      (let ((byte-reverse +image-byte-reverse+))
	(with-vector (byte-reverse (simple-array card8 (256)))
	  (macrolet ((br (byte)
		       `(the card8 (aref byte-reverse (the card8 ,byte)))))
	    (do ((length (index* (index-ceiling srclen 2) 2))
		 (h height (index1- h))
		 (srcstart srcoff (index+ srcstart srcinc))
		 (deststart destoff (index+ deststart destinc)))
		((index-zerop h))
	      (declare (type array-index length srcstart deststart)
		       (type card16 h))
	      (when (and (index= h 1) (not (index= srclen length)))
		(index-decf length 2)
		(if lsb-first-p
		    (setf (aref dest (index1+ (index+ deststart length)))
			  (br (aref src (index+ srcstart length))))
		  (setf (aref dest (index+ deststart length))
			(br (aref src (index1+ (index+ srcstart length)))))))
	      (do ((i length (index- i 2))
		   (srcidx srcstart (index+ srcidx 2))
		   (destidx deststart (index+ destidx 2)))
		  ((index-zerop i))
		(declare (type array-index i srcidx destidx))
		(setf (aref dest destidx)
		      (br (aref src (index1+ srcidx))))
		(setf (aref dest (index1+ destidx))
		      (br (aref src srcidx)))))))))))

(defun image-swap-bits-and-four-bytes
       (src dest srcoff destoff srclen srcinc destinc height lsb-first-p)
  (declare (type buffer-bytes src dest)
	   (type array-index srcoff destoff srclen srcinc destinc)
	   (type card16 height)
	   (type generalized-boolean lsb-first-p))
  #.(declare-buffun)
  (with-vector (src buffer-bytes)
    (with-vector (dest buffer-bytes)
      (let ((byte-reverse +image-byte-reverse+))
	(with-vector (byte-reverse (simple-array card8 (256)))
	  (macrolet ((br (byte)
		       `(the card8 (aref byte-reverse (the card8 ,byte)))))
	    (do ((length (index* (index-ceiling srclen 4) 4))
		 (h height (index1- h))
		 (srcstart srcoff (index+ srcstart srcinc))
		 (deststart destoff (index+ deststart destinc)))
		((index-zerop h))
	      (declare (type array-index length srcstart deststart)
		       (type card16 h))
	      (when (and (index= h 1) (not (index= srclen length)))
		(index-decf length 4)
		(unless lsb-first-p
		  (setf (aref dest (index+ deststart length))
			(br (aref src (index+ srcstart length 3)))))
		(when (if lsb-first-p
			  (index= (index- srclen length) 3)
			(not (index-zerop (index-logand srclen 2))))
		  (setf (aref dest (index+ deststart length 1))
			(br (aref src (index+ srcstart length 2)))))
		(when (if (null lsb-first-p)
			  (index= (index- srclen length) 3)
			(not (index-zerop (index-logand srclen 2))))
		  (setf (aref dest (index+ deststart length 2))
			(br (aref src (index+ srcstart length 1)))))
		(when lsb-first-p
		  (setf (aref dest (index+ deststart length 3))
			(br (aref src (index+ srcstart length))))))
	      (do ((i length (index- i 4))
		   (srcidx srcstart (index+ srcidx 4))
		   (destidx deststart (index+ destidx 4)))
		  ((index-zerop i))
		(declare (type array-index i srcidx destidx))
		(setf (aref dest destidx)
		      (br (aref src (index+ srcidx 3))))
		(setf (aref dest (index1+ destidx))
		      (br (aref src (index+ srcidx 2))))
		(setf (aref dest (index+ destidx 2))
		      (br (aref src (index1+ srcidx))))
		(setf (aref dest (index+ destidx 3))
		      (br (aref src srcidx)))))))))))

(defun image-swap-bits-and-words
       (src dest srcoff destoff srclen srcinc destinc height lsb-first-p)
  (declare (type buffer-bytes src dest)
	   (type array-index srcoff destoff srclen srcinc destinc)
	   (type card16 height)
	   (type generalized-boolean lsb-first-p))
  #.(declare-buffun)
  (with-vector (src buffer-bytes)
    (with-vector (dest buffer-bytes)
      (let ((byte-reverse +image-byte-reverse+))
	(with-vector (byte-reverse (simple-array card8 (256)))
	  (macrolet ((br (byte)
		       `(the card8 (aref byte-reverse (the card8 ,byte)))))
	    (do ((length (index* (index-ceiling srclen 4) 4))
		 (h height (index1- h))
		 (srcstart srcoff (index+ srcstart srcinc))
		 (deststart destoff (index+ deststart destinc)))
		((index-zerop h))
	      (declare (type array-index length srcstart deststart)
		       (type card16 h))
	      (when (and (index= h 1) (not (index= srclen length)))
		(index-decf length 4)
		(unless lsb-first-p
		  (setf (aref dest (index+ deststart length 1))
			(br (aref src (index+ srcstart length 3)))))
		(when (if lsb-first-p
			  (index= (index- srclen length) 3)
			(not (index-zerop (index-logand srclen 2))))
		  (setf (aref dest (index+ deststart length))
			(br (aref src (index+ srcstart length 2)))))
		(when (if (null lsb-first-p)
			  (index= (index- srclen length) 3)
			(not (index-zerop (index-logand srclen 2))))
		  (setf (aref dest (index+ deststart length 3))
			(br (aref src (index+ srcstart length 1)))))
		(when lsb-first-p
		  (setf (aref dest (index+ deststart length 2))
			(br (aref src (index+ srcstart length))))))
	      (do ((i length (index- i 4))
		   (srcidx srcstart (index+ srcidx 4))
		   (destidx deststart (index+ destidx 4)))
		  ((index-zerop i))
		(declare (type array-index i srcidx destidx))
		(setf (aref dest destidx)
		      (br (aref src (index+ srcidx 2))))
		(setf (aref dest (index1+ destidx))
		      (br (aref src (index+ srcidx 3))))
		(setf (aref dest (index+ destidx 2))
		      (br (aref src srcidx)))
		(setf (aref dest (index+ destidx 3))
		      (br (aref src (index1+ srcidx))))))))))))

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
;;; 
;;; 
;;; The following table gives the required conversion between any two
;;; formats.  It is based strictly on the table above.  If you believe one,
;;; you should believe the other.
;;; 
;;; legend:
;;; 	n   no changes
;;; 	s   reverse 8-bit units within 16-bit units
;;; 	l   reverse 8-bit units within 32-bit units
;;; 	w   reverse 16-bit units within 32-bit units
;;; 	r   reverse bits within 8-bit units
;;; 	sr  s+R
;;; 	lr  l+R
;;; 	wr  w+R

(defconstant +image-swap-function+
 '#.(make-array
     '(12 12) :initial-contents
     (let ((n  'image-noswap)
	   (s  'image-swap-two-bytes)
	   (l  'image-swap-four-bytes)
	   (w  'image-swap-words)
	   (r  'image-swap-bits)
	   (sr 'image-swap-bits-and-two-bytes)
	   (lr 'image-swap-bits-and-four-bytes)
	   (wr 'image-swap-bits-and-words))
       (list  #|       1Mm 2Mm 4Mm 1Ml 2Ml 4Ml 1Lm 2Lm 4Lm 1Ll 2Ll 4Ll  |#
	(list #| 1Mm |# n   n   n   r   sr  lr  n   s   l   r   r   r )
	(list #| 2Mm |# n   n   n   r   sr  lr  n   s   l   r   r   r )
	(list #| 4Mm |# n   n   n   r   sr  lr  n   s   l   r   r   r )
	(list #| 1Ml |# r   r   r   n   s   l   r   sr  lr  n   n   n )
	(list #| 2Ml |# sr  sr  sr  s   n   w   sr  r   wr  s   s   s )
	(list #| 4Ml |# lr  lr  lr  l   w   n   lr  wr  r   l   l   l )
	(list #| 1Lm |# n   n   n   r   sr  lr  n   s   l   r   r   r )
	(list #| 2Lm |# s   s   s   sr  r   wr  s   n   w   sr  sr  sr)
	(list #| 4Lm |# l   l   l   lr  wr  r   l   w   n   lr  lr  lr)
	(list #| 1Ll |# r   r   r   n   s   l   r   sr  lr  n   n   n )
	(list #| 2Ll |# r   r   r   n   s   l   r   sr  lr  n   n   n )
	(list #| 4Ll |# r   r   r   n   s   l   r   sr  lr  n   n   n )))))

;;; Of course, the table above is a lie.  We also need to factor in the
;;; order of the source data to cope with swapping half of a unit at the
;;; end of a scanline, since we are trying to avoid de-ref'ing off the
;;; end of the source.
;;;
;;; Defines whether the first half of a unit has the first half of the data

(defconstant +image-swap-lsb-first-p+
 '#.(make-array
     12 :initial-contents
     (list t   #| 1mm |#
	   t   #| 2mm |#
	   t   #| 4mm |#
	   t   #| 1ml |#
	   nil #| 2ml |#
	   nil #| 4ml |#
	   t   #| 1lm |#
	   nil #| 2lm |#
	   nil #| 4lm |#
	   t   #| 1ll |#
	   t   #| 2ll |#
	   t   #| 4ll |#
	   )))

(defun image-swap-function
       (bits-per-pixel
	from-bitmap-unit from-byte-lsb-first-p from-bit-lsb-first-p
	to-bitmap-unit to-byte-lsb-first-p to-bit-lsb-first-p)
  (declare (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (type (member 8 16 32) from-bitmap-unit to-bitmap-unit)
	   (type generalized-boolean from-byte-lsb-first-p from-bit-lsb-first-p
		 to-byte-lsb-first-p to-bit-lsb-first-p)
	   (clx-values function lsb-first-p))
  (cond ((index= bits-per-pixel 1)
	 (let ((from-index
		 (index+
		   (ecase from-bitmap-unit (32 2) (16 1) (8 0))
		   (if from-bit-lsb-first-p 3 0)
		   (if from-byte-lsb-first-p 6 0))))
	   (values
	     (aref +image-swap-function+ from-index
		   (index+
		     (ecase to-bitmap-unit (32 2) (16 1) (8 0))
		     (if to-bit-lsb-first-p 3 0)
		     (if to-byte-lsb-first-p 6 0)))
	     (aref +image-swap-lsb-first-p+ from-index))))
	(t
	 (values 
	   (if (if (index= bits-per-pixel 4)
		   (eq from-bit-lsb-first-p to-bit-lsb-first-p)
		 (eq from-byte-lsb-first-p to-byte-lsb-first-p))
	       'image-noswap
	     (ecase bits-per-pixel
	       (4  'image-swap-nibbles)
	       (8  'image-noswap)
	       (16 'image-swap-two-bytes)
	       (24 'image-swap-three-bytes)
	       (32 'image-swap-four-bytes)))
	   from-byte-lsb-first-p))))


;;;-----------------------------------------------------------------------------
;;; GET-IMAGE

(defun read-pixarray-1 (buffer-bbuf index array x y width height  
			padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type pixarray-1 array)
	   (type card16 x y width height)
	   (type array-index index padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (do* ((start (index+ index
			 (index* y padded-bytes-per-line)
			 (index-ceiling x 8))
		 (index+ start padded-bytes-per-line))
	  (y 0 (index1+ y))
	  (left-bits (the array-index
			  (mod (the (integer #x-FFFF 0) (- x))
			       8)))
	  (right-bits (index-mod (index- width left-bits) 8))
	  (middle-bits (- width left-bits right-bits))
	  (middle-bytes (floor middle-bits 8)))
	 ((index>= y height))
      (declare (type array-index start y left-bits right-bits))
      (declare (fixnum middle-bits middle-bytes))
      (cond ((< middle-bits 0)
	     (let ((byte (aref buffer-bbuf (index1- start)))
		   (x left-bits))
	       (declare (type card8 byte)
			(type array-index x))
	       (when (index> right-bits 6)
		 (setf (aref array y (index- x 1))
		       (read-image-load-byte 1 7 byte)))
	       (when (and (index> left-bits 1)
			  (index> right-bits 5))
		 (setf (aref array y (index- x 2))
		       (read-image-load-byte 1 6 byte)))
	       (when (and (index> left-bits 2)
			  (index> right-bits 4))
		 (setf (aref array y (index- x 3))
		       (read-image-load-byte 1 5 byte)))
	       (when (and (index> left-bits 3)
			  (index> right-bits 3))
		 (setf (aref array y (index- x 4))
		       (read-image-load-byte 1 4 byte)))
	       (when (and (index> left-bits 4)
			  (index> right-bits 2))
		 (setf (aref array y (index- x 5))
		       (read-image-load-byte 1 3 byte)))
	       (when (and (index> left-bits 5)
			  (index> right-bits 1))
		 (setf (aref array y (index- x 6))
		       (read-image-load-byte 1 2 byte)))
	       (when (index> left-bits 6)
		 (setf (aref array y (index- x 7))
		       (read-image-load-byte 1 1 byte)))))
	    (t
	     (unless (index-zerop left-bits)
	       (let ((byte (aref buffer-bbuf (index1- start)))
		     (x left-bits))
		 (declare (type card8 byte)
			  (type array-index x))
		 (setf (aref array y (index- x 1))
		       (read-image-load-byte 1 7 byte))
		 (when (index> left-bits 1)
		   (setf (aref array y (index- x 2))
			 (read-image-load-byte 1 6 byte))
		   (when (index> left-bits 2)
		     (setf (aref array y (index- x 3))
			   (read-image-load-byte 1 5 byte))
		     (when (index> left-bits 3)
		       (setf (aref array y (index- x 4))
			     (read-image-load-byte 1 4 byte))
		       (when (index> left-bits 4)
			 (setf (aref array y (index- x 5))
			       (read-image-load-byte 1 3 byte))
			 (when (index> left-bits 5)
			   (setf (aref array y (index- x 6))
				 (read-image-load-byte 1 2 byte))
			   (when (index> left-bits 6)
			     (setf (aref array y (index- x 7))
				   (read-image-load-byte 1 1 byte))
			     ))))))))
	     (do* ((end (index+ start middle-bytes))
		   (i start (index1+ i))
		   (x left-bits (index+ x 8)))
		  ((index>= i end)
		   (unless (index-zerop right-bits)
		     (let ((byte (aref buffer-bbuf end))
			   (x (index+ left-bits middle-bits)))
		       (declare (type card8 byte)
				(type array-index x))
		       (setf (aref array y (index+ x 0))
			     (read-image-load-byte 1 0 byte))
		       (when (index> right-bits 1)
			 (setf (aref array y (index+ x 1))
			       (read-image-load-byte 1 1 byte))
			 (when (index> right-bits 2)
			   (setf (aref array y (index+ x 2))
				 (read-image-load-byte 1 2 byte))
			   (when (index> right-bits 3)
			     (setf (aref array y (index+ x 3))
				   (read-image-load-byte 1 3 byte))
			     (when (index> right-bits 4)
			       (setf (aref array y (index+ x 4))
				     (read-image-load-byte 1 4 byte))
			       (when (index> right-bits 5)
				 (setf (aref array y (index+ x 5))
				       (read-image-load-byte 1 5 byte))
				 (when (index> right-bits 6)
				   (setf (aref array y (index+ x 6))
					 (read-image-load-byte 1 6 byte))
				   )))))))))
	       (declare (type array-index end i x))
	       (let ((byte (aref buffer-bbuf i)))
		 (declare (type card8 byte))
		 (setf (aref array y (index+ x 0))
		       (read-image-load-byte 1 0 byte))
		 (setf (aref array y (index+ x 1))
		       (read-image-load-byte 1 1 byte))
		 (setf (aref array y (index+ x 2))
		       (read-image-load-byte 1 2 byte))
		 (setf (aref array y (index+ x 3))
		       (read-image-load-byte 1 3 byte))
		 (setf (aref array y (index+ x 4))
		       (read-image-load-byte 1 4 byte))
		 (setf (aref array y (index+ x 5))
		       (read-image-load-byte 1 5 byte))
		 (setf (aref array y (index+ x 6))
		       (read-image-load-byte 1 6 byte))
		 (setf (aref array y (index+ x 7))
		       (read-image-load-byte 1 7 byte))))
	     )))))

(defun read-pixarray-4 (buffer-bbuf index array x y width height 
			padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type pixarray-4 array)
	   (type card16 x y width height)
	   (type array-index index padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (do* ((start (index+ index
			 (index* y padded-bytes-per-line)
			 (index-ceiling x 2))
		 (index+ start padded-bytes-per-line))
	  (y 0 (index1+ y))
	  (left-nibbles (mod (the fixnum (- x)) 2))
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
	    (x left-nibbles (index+ x 2)))
	   ((index>= i end)
	    (unless (index-zerop right-nibbles)
	      (setf (aref array y (index+ left-nibbles middle-nibbles))
		    (read-image-load-byte 4 0 (aref buffer-bbuf end)))))
	(declare (type array-index end i x))
	(let ((byte (aref buffer-bbuf i)))
	  (declare (type card8 byte))
	  (setf (aref array y (index+ x 0))
		(read-image-load-byte 4 0 byte))
	  (setf (aref array y (index+ x 1))
		(read-image-load-byte 4 4 byte))))
      )))

(defun read-pixarray-8 (buffer-bbuf index array x y width height 
			padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type pixarray-8 array)
	   (type card16 x y width height)
	   (type array-index index padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (do* ((start (index+ index
			 (index* y padded-bytes-per-line)
			 x)
		 (index+ start padded-bytes-per-line))
	  (y 0 (index1+ y)))
	 ((index>= y height))
      (declare (type array-index start y))
      (do* ((end (index+ start width))
	    (i start (index1+ i))
	    (x 0 (index1+ x)))
	   ((index>= i end))
	(declare (type array-index end i x))
	(setf (aref array y x)
	      (the card8 (aref buffer-bbuf i)))))))

(defun read-pixarray-16 (buffer-bbuf index array x y width height 
			 padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type pixarray-16 array)
	   (type card16 width height)
	   (type array-index index padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (do* ((start (index+ index
			 (index* y padded-bytes-per-line)
			 (index* x 2))
		 (index+ start padded-bytes-per-line))
	  (y 0 (index1+ y)))
	 ((index>= y height))
      (declare (type array-index start y))
      (do* ((end (index+ start (index* width 2)))
	    (i start (index+ i 2))
	    (x 0 (index1+ x)))
	   ((index>= i end))
	(declare (type array-index end i x))
	(setf (aref array y x)
	      (read-image-assemble-bytes
		(aref buffer-bbuf (index+ i 0))
		(aref buffer-bbuf (index+ i 1))))))))

(defun read-pixarray-24 (buffer-bbuf index array x y width height 
			 padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type pixarray-24 array)
	   (type card16 width height)
	   (type array-index index padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (do* ((start (index+ index
			 (index* y padded-bytes-per-line)
			 (index* x 3))
		 (index+ start padded-bytes-per-line))
	  (y 0 (index1+ y)))
	 ((index>= y height))
      (declare (type array-index start y))
      (do* ((end (index+ start (index* width 3)))
	    (i start (index+ i 3))
	    (x 0 (index1+ x)))
	   ((index>= i end))
	(declare (type array-index end i x))
	(setf (aref array y x)
	      (read-image-assemble-bytes
		(aref buffer-bbuf (index+ i 0))
		(aref buffer-bbuf (index+ i 1))
		(aref buffer-bbuf (index+ i 2))))))))

(defun read-pixarray-32 (buffer-bbuf index array x y width height 
			 padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type pixarray-32 array)
	   (type card16 width height)
	   (type array-index index padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (do* ((start (index+ index
			 (index* y padded-bytes-per-line)
			 (index* x 4))
		 (index+ start padded-bytes-per-line))
	  (y 0 (index1+ y)))
	 ((index>= y height))
      (declare (type array-index start y))
      (do* ((end (index+ start (index* width 4)))
	    (i start (index+ i 4))
	    (x 0 (index1+ x)))
	   ((index>= i end))
	(declare (type array-index end i x))
	(setf (aref array y x)
	      (read-image-assemble-bytes
		(aref buffer-bbuf (index+ i 0))
		(aref buffer-bbuf (index+ i 1))
		(aref buffer-bbuf (index+ i 2))
		(aref buffer-bbuf (index+ i 3))))))))

(defun read-pixarray-internal
       (bbuf boffset pixarray x y width height padded-bytes-per-line
	bits-per-pixel read-pixarray-function
	from-unit from-byte-lsb-first-p from-bit-lsb-first-p
	to-unit to-byte-lsb-first-p to-bit-lsb-first-p)
  (declare (type buffer-bytes bbuf)
	   (type array-index boffset padded-bytes-per-line)
	   (type pixarray pixarray)
	   (type card16 x y width height)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (type function read-pixarray-function)
	   (type (member 8 16 32) from-unit to-unit)
	   (type generalized-boolean from-byte-lsb-first-p from-bit-lsb-first-p
		 to-byte-lsb-first-p to-bit-lsb-first-p))
  (multiple-value-bind (image-swap-function image-swap-lsb-first-p)
      (image-swap-function
	bits-per-pixel
	from-unit from-byte-lsb-first-p from-bit-lsb-first-p
	to-unit to-byte-lsb-first-p to-bit-lsb-first-p)
    (if (eq image-swap-function 'image-noswap)
	(funcall
	  read-pixarray-function
	  bbuf boffset pixarray x y width height padded-bytes-per-line
	  bits-per-pixel)
      (with-image-data-buffer (buf (index* height padded-bytes-per-line))
	(funcall
	  (symbol-function image-swap-function) bbuf buf
	  (index+ boffset (index* y padded-bytes-per-line)) 0
	  (index-ceiling (index* (index+ x width) bits-per-pixel) 8)
	  padded-bytes-per-line padded-bytes-per-line height
	  image-swap-lsb-first-p)
	(funcall
	  read-pixarray-function 
	  buf 0 pixarray x 0 width height padded-bytes-per-line
	  bits-per-pixel)))))

(defun read-pixarray
       (bbuf boffset pixarray x y width height padded-bytes-per-line
	bits-per-pixel unit byte-lsb-first-p bit-lsb-first-p)
  (declare (type buffer-bytes bbuf)
	   (type array-index boffset padded-bytes-per-line)
	   (type pixarray pixarray)
	   (type card16 x y width height)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (type (member 8 16 32) unit)
	   (type generalized-boolean byte-lsb-first-p bit-lsb-first-p))
  (unless (fast-read-pixarray
	    bbuf boffset pixarray x y width height padded-bytes-per-line
	    bits-per-pixel unit byte-lsb-first-p bit-lsb-first-p)
    (read-pixarray-internal
      bbuf boffset pixarray x y width height padded-bytes-per-line
      bits-per-pixel 
      (ecase bits-per-pixel
	( 1 #'read-pixarray-1 )
	( 4 #'read-pixarray-4 )
	( 8 #'read-pixarray-8 )
	(16 #'read-pixarray-16)
	(24 #'read-pixarray-24)
	(32 #'read-pixarray-32))
      unit byte-lsb-first-p bit-lsb-first-p
      +image-unit+ +image-byte-lsb-first-p+ +image-bit-lsb-first-p+)))

(defun read-xy-format-image-x
       (buffer-bbuf index length data width height depth
	padded-bytes-per-line padded-bytes-per-plane
	unit byte-lsb-first-p bit-lsb-first-p pad)
  (declare (type buffer-bytes buffer-bbuf)
	   (type card16 width height)
	   (type array-index index length padded-bytes-per-line
		 padded-bytes-per-plane)
	   (type image-depth depth)
	   (type (member 8 16 32) unit pad)
	   (type generalized-boolean byte-lsb-first-p bit-lsb-first-p)
	   (clx-values image-x))
  (assert (index<= (index* depth padded-bytes-per-plane) length))
  (let* ((bytes-per-line (index-ceiling width 8))
	 (data-length (index* padded-bytes-per-plane depth)))
    (declare (type array-index bytes-per-line data-length))
    (cond (data
	   (check-type data buffer-bytes)
	   (assert (index>= (length data) data-length)))
	  (t
	   (setq data (make-array data-length :element-type 'card8))))
    (do ((plane 0 (index1+ plane)))
	((index>= plane depth))
      (declare (type image-depth plane))
      (image-noswap
	buffer-bbuf data
	(index+ index (index* plane padded-bytes-per-plane))
	(index* plane padded-bytes-per-plane)
	bytes-per-line padded-bytes-per-line padded-bytes-per-line
	height byte-lsb-first-p))
    (create-image 
      :width width :height height :depth depth :data data
      :bits-per-pixel 1 :format :xy-pixmap
      :bytes-per-line padded-bytes-per-line
      :unit unit :pad pad
      :byte-lsb-first-p byte-lsb-first-p :bit-lsb-first-p bit-lsb-first-p)))

(defun read-z-format-image-x
       (buffer-bbuf index length data width height depth
	padded-bytes-per-line 
	unit byte-lsb-first-p bit-lsb-first-p pad bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type card16 width height)
	   (type array-index index length padded-bytes-per-line)
	   (type image-depth depth)
	   (type (member 8 16 32) unit pad)
	   (type generalized-boolean byte-lsb-first-p bit-lsb-first-p)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (clx-values image-x))
  (assert (index<= (index* height padded-bytes-per-line) length))
  (let ((bytes-per-line (index-ceiling (index* width bits-per-pixel) 8))
	(data-length (index* padded-bytes-per-line height)))
    (declare (type array-index bytes-per-line data-length))
    (cond (data
	   (check-type data buffer-bytes)
	   (assert (index>= (length data) data-length)))
	  (t
	   (setq data (make-array data-length :element-type 'card8))))
    (image-noswap
      buffer-bbuf data index 0 bytes-per-line padded-bytes-per-line
      padded-bytes-per-line height byte-lsb-first-p)
    (create-image 
      :width width :height height :depth depth :data data
      :bits-per-pixel bits-per-pixel :format :z-pixmap
      :bytes-per-line padded-bytes-per-line
      :unit unit :pad pad
      :byte-lsb-first-p byte-lsb-first-p :bit-lsb-first-p bit-lsb-first-p)))

(defun read-image-xy (bbuf index length data x y width height depth
		      padded-bytes-per-line padded-bytes-per-plane
		      unit byte-lsb-first-p bit-lsb-first-p)
  (declare (type buffer-bytes bbuf)
	   (type card16 x y width height)
	   (type array-index index length padded-bytes-per-line
		 padded-bytes-per-plane)
	   (type image-depth depth)
	   (type (member 8 16 32) unit)
	   (type generalized-boolean byte-lsb-first-p bit-lsb-first-p)
	   (clx-values image-xy))
  (check-type data list)
  (multiple-value-bind (dimensions element-type)
      (if data
	  (values (array-dimensions (first data))
		  (array-element-type (first data)))
	(values (list height
		      (index* (index-ceiling width +image-pad+) +image-pad+))
		'pixarray-1-element-type))
    (do* ((arrays data)
	  (result nil)
	  (limit (index+ length index))
	  (plane 0 (1+ plane))
	  (index index (index+ index padded-bytes-per-plane)))
	 ((or (>= plane depth)
	      (index> (index+ index padded-bytes-per-plane) limit))
	  (setq data (nreverse result) depth (length data)))
      (declare (type array-index limit index)
	       (type image-depth plane)
	       (type list arrays result))
      (let ((array (or (pop arrays)
		       (make-array dimensions :element-type element-type))))
	(declare (type pixarray-1 array))
	(push array result)
	(read-pixarray
	  bbuf index array x y width height padded-bytes-per-line 1
	  unit byte-lsb-first-p bit-lsb-first-p)))
    (create-image 
      :width width :height height :depth depth :data data)))

(defun read-image-z (bbuf index length data x y width height depth
		     padded-bytes-per-line bits-per-pixel
		     unit byte-lsb-first-p bit-lsb-first-p)
  (declare (type buffer-bytes bbuf)
	   (type card16 x y width height)
	   (type array-index index length padded-bytes-per-line)
	   (type image-depth depth)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (type (member 8 16 32) unit)
	   (type generalized-boolean byte-lsb-first-p bit-lsb-first-p)
	   (clx-values image-z))
  (assert (index<= (index* (index+ y height) padded-bytes-per-line) length))
  (let* ((image-bits-per-line (index* width bits-per-pixel))
	 (image-pixels-per-line
	   (index-ceiling
	     (index* (index-ceiling image-bits-per-line +image-pad+)
		     +image-pad+)
	     bits-per-pixel)))
    (declare (type array-index image-bits-per-line image-pixels-per-line))
    (unless data
      (setq data
	    (make-array
	      (list height image-pixels-per-line)
	      :element-type (ecase bits-per-pixel
			      (1  'pixarray-1-element-type)
			      (4  'pixarray-4-element-type)
			      (8  'pixarray-8-element-type)
			      (16 'pixarray-16-element-type)
			      (24 'pixarray-24-element-type)
			      (32 'pixarray-32-element-type)))))
    (read-pixarray
      bbuf index data x y width height padded-bytes-per-line bits-per-pixel
      unit byte-lsb-first-p bit-lsb-first-p)
    (create-image 
      :width width :height height :depth depth :data data
      :bits-per-pixel bits-per-pixel)))

(defun get-image (drawable &key
		  data
		  (x (required-arg x))
		  (y (required-arg y))
		  (width (required-arg width))
		  (height (required-arg height))
		  plane-mask format result-type)
  (declare (type drawable drawable)
	   (type (or buffer-bytes list pixarray) data)
	   (type int16 x y) ;; required
	   (type card16 width height) ;; required
	   (type (or null pixel) plane-mask)
	   (type (or null (member :xy-pixmap :z-pixmap)) format)
	   (type (or null (member image-xy image-x image-z)) result-type)
	   (clx-values image visual-info))
  (unless result-type
    (setq result-type (ecase format
			(:xy-pixmap 'image-xy)
			(:z-pixmap 'image-z)
			((nil) 'image-x))))
  (unless format
    (setq format (case result-type
		   (image-xy :xy-pixmap)
		   ((image-z image-x) :z-pixmap))))
  (unless (ecase result-type
	    (image-xy (eq format :xy-pixmap))
	    (image-z (eq format :z-pixmap))
	    (image-x t))
    (error "Result-type ~s is incompatible with format ~s"
	   result-type format))
  (unless plane-mask (setq plane-mask #xffffffff))
  (let ((display (drawable-display drawable)))
    (with-buffer-request-and-reply (display +x-getimage+ nil :sizes (8 32))
	 (((data (member error :xy-pixmap :z-pixmap)) format)
	  (drawable drawable)
	  (int16 x y)
	  (card16 width height)
	  (card32 plane-mask))
      (let* ((depth (card8-get 1))
	     (length (index* 4 (card32-get 4)))
	     (visual-info (visual-info display (resource-id-get 8)))
	     (bitmap-format (display-bitmap-format display))
	     (unit (bitmap-format-unit bitmap-format))
	     (byte-lsb-first-p (display-image-lsb-first-p display))
	     (bit-lsb-first-p  (bitmap-format-lsb-first-p bitmap-format)))
	(declare (type image-depth depth)
		 (type array-index length)
		 (type (or null visual-info) visual-info)
		 (type bitmap-format bitmap-format)
		 (type (member 8 16 32) unit)
		 (type generalized-boolean byte-lsb-first-p bit-lsb-first-p))
	(multiple-value-bind (pad bits-per-pixel)
	    (ecase format
	      (:xy-pixmap
		(values (bitmap-format-pad bitmap-format) 1))
	      (:z-pixmap
		(if (= depth 1)
		    (values (bitmap-format-pad bitmap-format) 1)
		  (let ((pixmap-format
			  (find depth (display-pixmap-formats display)
				:key #'pixmap-format-depth)))
		    (declare (type pixmap-format pixmap-format))
		    (values (pixmap-format-scanline-pad pixmap-format)
			    (pixmap-format-bits-per-pixel pixmap-format))))))
	  (declare (type (member 8 16 32) pad)
		   (type (member 1 4 8 16 24 32) bits-per-pixel))
	  (let* ((bits-per-line (index* bits-per-pixel width))
		 (padded-bits-per-line
		   (index* (index-ceiling bits-per-line pad) pad))
		 (padded-bytes-per-line
		   (index-ceiling padded-bits-per-line 8))
		 (padded-bytes-per-plane
		   (index* padded-bytes-per-line height))
		 (image
		   (ecase result-type
		     (image-x
		       (ecase format
			 (:xy-pixmap
			   (read-xy-format-image-x
			     buffer-bbuf +replysize+ length data
			     width height depth
			     padded-bytes-per-line padded-bytes-per-plane
			     unit byte-lsb-first-p bit-lsb-first-p
			     pad))
			 (:z-pixmap
			   (read-z-format-image-x
			     buffer-bbuf +replysize+ length data
			     width height depth
			     padded-bytes-per-line
			     unit byte-lsb-first-p bit-lsb-first-p
			     pad bits-per-pixel))))
		     (image-xy
		       (read-image-xy
			 buffer-bbuf +replysize+ length data
			 0 0 width height depth
			 padded-bytes-per-line padded-bytes-per-plane
			 unit byte-lsb-first-p bit-lsb-first-p))
		     (image-z
		       (read-image-z
			 buffer-bbuf +replysize+ length data
			 0 0 width height depth padded-bytes-per-line
			 bits-per-pixel 
			 unit byte-lsb-first-p bit-lsb-first-p)))))
	    (declare (type image image)
		     (type array-index bits-per-line 
			   padded-bits-per-line padded-bytes-per-line))
	    (when visual-info
	      (unless (zerop (visual-info-red-mask visual-info))
		(setf (image-red-mask image)
		      (visual-info-red-mask visual-info)))
	      (unless (zerop (visual-info-green-mask visual-info))
		(setf (image-green-mask image)
		      (visual-info-green-mask visual-info)))
	      (unless (zerop (visual-info-blue-mask visual-info))
		(setf (image-blue-mask image)
		      (visual-info-blue-mask visual-info))))
	    (values image visual-info)))))))


;;;-----------------------------------------------------------------------------
;;; PUT-IMAGE

(defun write-pixarray-1 (buffer-bbuf index array x y width height
			 padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type pixarray-1 array)
	   (type card16 x y width height)
	   (type array-index index padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
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
	    (x start-x (index+ x 8)))
	   ((index>= i end)
	    (unless (index-zerop right-bits)
	      (let ((x (index+ start-x middle-bits)))
		(declare (type array-index x))
		(setf (aref buffer-bbuf end)
		      (write-image-assemble-bytes
			(aref array y (index+ x 0))
			(if (index> right-bits 1)
			    (aref array y (index+ x 1))
			  0)
			(if (index> right-bits 2)
			    (aref array y (index+ x 2))
			  0)
			(if (index> right-bits 3)
			    (aref array y (index+ x 3))
			  0)
			(if (index> right-bits 4)
			    (aref array y (index+ x 4))
			  0)
			(if (index> right-bits 5)
			    (aref array y (index+ x 5))
			  0)
			(if (index> right-bits 6)
			    (aref array y (index+ x 6))
			  0)
			0)))))
	(declare (type array-index end i start-x x))
	(setf (aref buffer-bbuf i)
	      (write-image-assemble-bytes
		(aref array y (index+ x 0))
		(aref array y (index+ x 1))
		(aref array y (index+ x 2))
		(aref array y (index+ x 3))
		(aref array y (index+ x 4))
		(aref array y (index+ x 5))
		(aref array y (index+ x 6))
		(aref array y (index+ x 7))))))))

(defun write-pixarray-4 (buffer-bbuf index array x y width height
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
	    (x start-x (index+ x 2)))
	   ((index>= i end)
	    (unless (index-zerop right-nibbles)
	      (setf (aref buffer-bbuf end)
		    (write-image-assemble-bytes
		      (aref array y (index+ start-x middle-nibbles))
		      0))))
	(declare (type array-index end i start-x x))
	(setf (aref buffer-bbuf i)
	      (write-image-assemble-bytes
		(aref array y (index+ x 0))
		(aref array y (index+ x 1))))))))

(defun write-pixarray-8 (buffer-bbuf index array x y width height
			 padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type pixarray-8 array)
	   (type int16 x y)
	   (type card16 width height)
	   (type array-index index padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (do* ((h 0 (index1+ h))
	  (y y (index1+ y))
	  (start index (index+ start padded-bytes-per-line)))
	 ((index>= h height))
      (declare (type array-index h y start))
      (do* ((end (index+ start width))
	    (i start (index1+ i))
	    (x x (index1+ x)))
	   ((index>= i end))
	(declare (type array-index end i x))
	(setf (aref buffer-bbuf i) (the card8 (aref array y x)))))))

(defun write-pixarray-16 (buffer-bbuf index array x y width height
			  padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type pixarray-16 array)
	   (type int16 x y)
	   (type card16 width height)
	   (type array-index index padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (do* ((h 0 (index1+ h))
	  (y y (index1+ y))
	  (start index (index+ start padded-bytes-per-line)))
	 ((index>= h height))
      (declare (type array-index h y start))
      (do* ((end (index+ start (index* width 2)))
	    (i start (index+ i 2))
	    (x x (index1+ x)))
	   ((index>= i end))
	(declare (type array-index end i x))
	(let ((pixel (aref array y x)))
	  (declare (type pixarray-16-element-type pixel))
	  (setf (aref buffer-bbuf (index+ i 0))
		(write-image-load-byte 0 pixel 16))
	  (setf (aref buffer-bbuf (index+ i 1))
		(write-image-load-byte 8 pixel 16)))))))

(defun write-pixarray-24 (buffer-bbuf index array x y width height
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
    (do* ((h 0 (index1+ h))
	  (y y (index1+ y))
	  (start index (index+ start padded-bytes-per-line)))
	 ((index>= h height))
      (declare (type array-index y start))
      (do* ((end (index+ start (index* width 3)))
	    (i start (index+ i 3))
	    (x x (index1+ x)))
	   ((index>= i end))
	(declare (type array-index end i x))
	(let ((pixel (aref array y x)))
	  (declare (type pixarray-24-element-type pixel))
	  (setf (aref buffer-bbuf (index+ i 0))
		(write-image-load-byte 0 pixel 24))
	  (setf (aref buffer-bbuf (index+ i 1))
		(write-image-load-byte 8 pixel 24))
	  (setf (aref buffer-bbuf (index+ i 2))
		(write-image-load-byte 16 pixel 24)))))))

(defun write-pixarray-32 (buffer-bbuf index array x y width height
			  padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type pixarray-32 array)
	   (type int16 x y)
	   (type card16 width height)
	   (type array-index index padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (do* ((h 0 (index1+ h))
	  (y y (index1+ y))
	  (start index (index+ start padded-bytes-per-line)))
	 ((index>= h height))
      (declare (type array-index h y start))
      (do* ((end (index+ start (index* width 4)))
	    (i start (index+ i 4))
	    (x x (index1+ x)))
	   ((index>= i end))
	(declare (type array-index end i x))
	(let ((pixel (aref array y x)))
	  (declare (type pixarray-32-element-type pixel))
	  (setf (aref buffer-bbuf (index+ i 0))
		(write-image-load-byte 0 pixel 32))
	  (setf (aref buffer-bbuf (index+ i 1))
		(write-image-load-byte 8 pixel 32))
	  (setf (aref buffer-bbuf (index+ i 2))
		(write-image-load-byte 16 pixel 32))
	  (setf (aref buffer-bbuf (index+ i 3))
		(write-image-load-byte 24 pixel 32)))))))

(defun write-pixarray-internal
       (bbuf boffset pixarray x y width height padded-bytes-per-line
	bits-per-pixel write-pixarray-function
	from-unit from-byte-lsb-first-p from-bit-lsb-first-p
	to-unit to-byte-lsb-first-p to-bit-lsb-first-p)
  (declare (type buffer-bytes bbuf)
	   (type pixarray pixarray)
	   (type card16 x y width height)
	   (type array-index boffset padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (type function write-pixarray-function)
	   (type (member 8 16 32) from-unit to-unit)
	   (type generalized-boolean from-byte-lsb-first-p from-bit-lsb-first-p
		 to-byte-lsb-first-p to-bit-lsb-first-p))
  (multiple-value-bind (image-swap-function image-swap-lsb-first-p)
      (image-swap-function
	bits-per-pixel
	from-unit from-byte-lsb-first-p from-bit-lsb-first-p
	to-unit to-byte-lsb-first-p to-bit-lsb-first-p)
    (declare (type symbol image-swap-function)
	     (type generalized-boolean image-swap-lsb-first-p))
    (if (eq image-swap-function 'image-noswap)
	(funcall
	  write-pixarray-function
	  bbuf boffset pixarray x y width height padded-bytes-per-line
	  bits-per-pixel)
      (with-image-data-buffer (buf (index* height padded-bytes-per-line))
	(funcall
	  write-pixarray-function 
	  buf 0 pixarray x y width height padded-bytes-per-line
	  bits-per-pixel)
	(funcall
	  (symbol-function image-swap-function) buf bbuf 0 boffset
	  (index-ceiling (index* width bits-per-pixel) 8)
	  padded-bytes-per-line padded-bytes-per-line height
	  image-swap-lsb-first-p)))))

(defun write-pixarray
       (bbuf boffset pixarray x y width height padded-bytes-per-line
	bits-per-pixel unit byte-lsb-first-p bit-lsb-first-p)
  (declare (type buffer-bytes bbuf)
	   (type pixarray pixarray)
	   (type card16 x y width height)
	   (type array-index boffset padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (type (member 8 16 32) unit)
	   (type generalized-boolean byte-lsb-first-p bit-lsb-first-p))
  (unless (fast-write-pixarray
	    bbuf boffset pixarray x y width height padded-bytes-per-line
	    bits-per-pixel unit byte-lsb-first-p bit-lsb-first-p)
    (write-pixarray-internal
      bbuf boffset pixarray x y width height padded-bytes-per-line
      bits-per-pixel
      (ecase bits-per-pixel
	( 1 #'write-pixarray-1 )
	( 4 #'write-pixarray-4 )
	( 8 #'write-pixarray-8 )
	(16 #'write-pixarray-16)
	(24 #'write-pixarray-24)
	(32 #'write-pixarray-32))
      +image-unit+ +image-byte-lsb-first-p+ +image-bit-lsb-first-p+
      unit byte-lsb-first-p bit-lsb-first-p)))

(defun write-xy-format-image-x-data
       (data obuf data-start obuf-start x y width height
	from-padded-bytes-per-line to-padded-bytes-per-line
	from-bitmap-unit from-byte-lsb-first-p from-bit-lsb-first-p
	to-bitmap-unit to-byte-lsb-first-p to-bit-lsb-first-p)
  (declare (type buffer-bytes data obuf)
	   (type array-index data-start obuf-start
		 from-padded-bytes-per-line to-padded-bytes-per-line)
	   (type card16 x y width height)
	   (type (member 8 16 32) from-bitmap-unit to-bitmap-unit)
	   (type generalized-boolean from-byte-lsb-first-p from-bit-lsb-first-p
		 to-byte-lsb-first-p to-bit-lsb-first-p))
  (assert (index-zerop (index-mod x 8)))
  (multiple-value-bind (image-swap-function image-swap-lsb-first-p)
      (image-swap-function
	1
	from-bitmap-unit from-byte-lsb-first-p from-bit-lsb-first-p
	to-bitmap-unit to-byte-lsb-first-p to-bit-lsb-first-p)
    (declare (type symbol image-swap-function)
	     (type generalized-boolean image-swap-lsb-first-p))
    (let ((x-mod-unit (index-mod x from-bitmap-unit)))
      (declare (type card16 x-mod-unit))
      (if (and (index-plusp x-mod-unit)
	       (not (eq from-byte-lsb-first-p from-bit-lsb-first-p)))
	  (let* ((temp-width (index+ width x-mod-unit))
		 (temp-bytes-per-line (index-ceiling temp-width 8))
		 (temp-padded-bits-per-line
		   (index* (index-ceiling temp-width from-bitmap-unit)
			   from-bitmap-unit))
		 (temp-padded-bytes-per-line
		   (index-ceiling temp-padded-bits-per-line 8)))
	    (declare (type card16 temp-width temp-bytes-per-line
			   temp-padded-bits-per-line temp-padded-bytes-per-line))
	    (with-image-data-buffer
		 (buf (index* height temp-padded-bytes-per-line))
	      (funcall
		(symbol-function image-swap-function) data buf
		(index+ data-start
			(index* y from-padded-bytes-per-line)
			(index-floor (index- x x-mod-unit) 8))
		0 temp-bytes-per-line from-padded-bytes-per-line
		temp-padded-bytes-per-line height image-swap-lsb-first-p)
	      (write-xy-format-image-x-data
		buf obuf 0 obuf-start x-mod-unit 0 width height
		temp-padded-bytes-per-line to-padded-bytes-per-line
		from-bitmap-unit to-byte-lsb-first-p to-byte-lsb-first-p
		to-bitmap-unit to-byte-lsb-first-p to-bit-lsb-first-p)))
	(funcall
	  (symbol-function image-swap-function) data obuf 
	  (index+ data-start
		  (index* y from-padded-bytes-per-line)
		  (index-floor x 8))
	  obuf-start (index-ceiling width 8) from-padded-bytes-per-line
	  to-padded-bytes-per-line height image-swap-lsb-first-p)))))

(defun write-xy-format-image-x
       (display image src-x src-y width height
	padded-bytes-per-line
	unit byte-lsb-first-p bit-lsb-first-p)
  (declare (type display display)
	   (type image-x image)
	   (type int16 src-x src-y)
	   (type card16 width height)
	   (type array-index padded-bytes-per-line)
	   (type (member 8 16 32) unit)
	   (type generalized-boolean byte-lsb-first-p bit-lsb-first-p))
  (dotimes (plane (image-depth image))
    (let ((data-start
	    (index* (index* plane (image-height image))
		    (image-x-bytes-per-line image)))
	  (src-y src-y)
	  (height height))
      (declare (type int16 src-y)
	       (type card16 height))
      (loop 
	(when (index-zerop height) (return))
	(let ((nlines
		(index-min (index-floor (index- (buffer-size display)
						(buffer-boffset display))
					padded-bytes-per-line)
			   height)))
	  (declare (type array-index nlines))
	  (when (index-plusp nlines)
	    (write-xy-format-image-x-data
	      (image-x-data image) (buffer-obuf8 display)
	      data-start (buffer-boffset display)
	      src-x src-y width nlines 
	      (image-x-bytes-per-line image) padded-bytes-per-line
	      (image-x-unit image) (image-x-byte-lsb-first-p image)
	      (image-x-bit-lsb-first-p image)
	      unit byte-lsb-first-p bit-lsb-first-p)
	    (index-incf (buffer-boffset display)
			(index* nlines padded-bytes-per-line))
	    (index-incf src-y nlines)
	    (when (index-zerop (index-decf height nlines)) (return))))
	(buffer-flush display)))))

(defun write-z-format-image-x-data
       (data obuf data-start obuf-start x y width height
	from-padded-bytes-per-line to-padded-bytes-per-line
	bits-per-pixel
	from-bitmap-unit from-byte-lsb-first-p from-bit-lsb-first-p
	to-bitmap-unit to-byte-lsb-first-p to-bit-lsb-first-p)
  (declare (type buffer-bytes data obuf)
	   (type array-index data-start obuf-start
		 from-padded-bytes-per-line to-padded-bytes-per-line)
	   (type card16 x y width height)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (type (member 8 16 32) from-bitmap-unit to-bitmap-unit)
	   (type generalized-boolean from-byte-lsb-first-p from-bit-lsb-first-p
		 to-byte-lsb-first-p to-bit-lsb-first-p))
  (if (index= bits-per-pixel 1)
      (write-xy-format-image-x-data
	data obuf data-start obuf-start x y width height
	from-padded-bytes-per-line to-padded-bytes-per-line
	from-bitmap-unit from-byte-lsb-first-p from-bit-lsb-first-p
	to-bitmap-unit to-byte-lsb-first-p to-bit-lsb-first-p)
    (let ((srcoff
	    (index+ data-start
		    (index* y from-padded-bytes-per-line)
		    (index-floor (index* x bits-per-pixel) 8)))
	  (srclen (index-ceiling (index* width bits-per-pixel) 8)))
      (declare (type array-index srcoff srclen))
      (if (and (index= bits-per-pixel 4) (index-oddp x))
	  (with-image-data-buffer (buf (index* height to-padded-bytes-per-line))
	    (image-swap-nibbles-left
	      data buf srcoff 0 srclen
	      from-padded-bytes-per-line to-padded-bytes-per-line height nil)
	    (write-z-format-image-x-data
	      buf obuf 0 obuf-start 0 0 width height
	      to-padded-bytes-per-line to-padded-bytes-per-line
	      bits-per-pixel
	      from-bitmap-unit from-byte-lsb-first-p from-bit-lsb-first-p
	      to-bitmap-unit to-byte-lsb-first-p to-bit-lsb-first-p))
	(multiple-value-bind (image-swap-function image-swap-lsb-first-p)
	    (image-swap-function
	      bits-per-pixel
	      from-bitmap-unit from-byte-lsb-first-p from-bit-lsb-first-p
	      to-bitmap-unit to-byte-lsb-first-p to-bit-lsb-first-p)
	  (declare (type symbol image-swap-function)
		   (type generalized-boolean image-swap-lsb-first-p))
	  (funcall
	    (symbol-function image-swap-function) data obuf srcoff obuf-start
	    srclen from-padded-bytes-per-line to-padded-bytes-per-line height
	    image-swap-lsb-first-p))))))

(defun write-z-format-image-x (display image src-x src-y width height
			       padded-bytes-per-line
			       unit byte-lsb-first-p bit-lsb-first-p)
  (declare (type display display)
	   (type image-x image)
	   (type int16 src-x src-y)
	   (type card16 width height)
	   (type array-index padded-bytes-per-line)
	   (type generalized-boolean byte-lsb-first-p bit-lsb-first-p))
  (loop 
    (when (index-zerop height) (return))
    (let ((nlines
	    (index-min (index-floor (index- (buffer-size display)
					    (buffer-boffset display))
				    padded-bytes-per-line)
		       height)))
      (declare (type array-index nlines))
      (when (index-plusp nlines)
	(write-z-format-image-x-data 
	  (image-x-data image) (buffer-obuf8 display) 0 (buffer-boffset display)
	  src-x src-y width nlines
	  (image-x-bytes-per-line image) padded-bytes-per-line
	  (image-x-bits-per-pixel image)
	  (image-x-unit image) (image-x-byte-lsb-first-p image)
	  (image-x-bit-lsb-first-p image)
	  unit byte-lsb-first-p bit-lsb-first-p)
	(index-incf (buffer-boffset display)
		    (index* nlines padded-bytes-per-line))
	(index-incf src-y nlines)
	(when (index-zerop (index-decf height nlines)) (return))))
    (buffer-flush display)))

(defun write-image-xy (display image src-x src-y width height
		       padded-bytes-per-line
		       unit byte-lsb-first-p bit-lsb-first-p)
  (declare (type display display)
	   (type image-xy image)
	   (type array-index padded-bytes-per-line)
	   (type int16 src-x src-y)
	   (type card16 width height)
	   (type (member 8 16 32) unit)
	   (type generalized-boolean byte-lsb-first-p bit-lsb-first-p))
  (dolist (bitmap (image-xy-bitmap-list image))
    (declare (type pixarray-1 bitmap))
    (let ((src-y src-y)
	  (height height))
      (declare (type int16 src-y)
	       (type card16 height))
      (loop 
	(let ((nlines
		(index-min (index-floor (index- (buffer-size display)
						(buffer-boffset display))
					padded-bytes-per-line)
			   height)))
	  (declare (type array-index nlines))
	  (when (index-plusp nlines)
	    (write-pixarray 
	      (buffer-obuf8 display) (buffer-boffset display)
	      bitmap src-x src-y width nlines
	      padded-bytes-per-line 1
	      unit byte-lsb-first-p bit-lsb-first-p)
	    (index-incf (buffer-boffset display)
			(index* nlines padded-bytes-per-line))
	    (index-incf src-y nlines)
	    (when (index-zerop (index-decf height nlines)) (return))))
	(buffer-flush display)))))

(defun write-image-z (display image src-x src-y width height
		      padded-bytes-per-line
		      unit byte-lsb-first-p bit-lsb-first-p)
  (declare (type display display)
	   (type image-z image)
	   (type array-index padded-bytes-per-line)
	   (type int16 src-x src-y)
	   (type card16 width height)
	   (type (member 8 16 32) unit)
	   (type generalized-boolean byte-lsb-first-p bit-lsb-first-p))
  (loop 
    (let ((bits-per-pixel (image-z-bits-per-pixel image))
	  (nlines
	    (index-min (index-floor (index- (buffer-size display)
					    (buffer-boffset display))
				    padded-bytes-per-line)
		       height)))
      (declare (type (member 1 4 8 16 24 32) bits-per-pixel)
	       (type array-index nlines))
      (when (index-plusp nlines)
	(write-pixarray
	  (buffer-obuf8 display) (buffer-boffset display)
	  (image-z-pixarray image) src-x src-y width nlines
	  padded-bytes-per-line bits-per-pixel
	  unit byte-lsb-first-p bit-lsb-first-p)
	(index-incf (buffer-boffset display)
		    (index* nlines padded-bytes-per-line))
	(index-incf src-y nlines)
	(when (index-zerop (index-decf height nlines)) (return))))
    (buffer-flush display)))

;;; Note:	The only difference between a format of :bitmap and :xy-pixmap
;;;		of depth 1 is that when sending a :bitmap format the foreground 
;;;		and background in the gcontext are used.

(defun put-image (drawable gcontext image &key
		  (src-x 0) (src-y 0)		;Position within image
		  (x (required-arg x))		;Position within drawable
		  (y (required-arg y))
		  width height
		  bitmap-p)
  ;; Copy an image into a drawable.
  ;; WIDTH and HEIGHT default from IMAGE.
  ;; When BITMAP-P, force format to be :bitmap when depth=1.
  ;; This causes gcontext to supply foreground & background pixels.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type image image)
	   (type int16 x y) ;; required
	   (type int16 src-x src-y)
	   (type (or null card16) width height)
	   (type generalized-boolean bitmap-p))
  (let* ((format
	   (etypecase image
	     (image-x (image-x-format (the image-x image)))
	     (image-xy :xy-pixmap)
	     (image-z :z-pixmap)))
	 (src-x
	   (if (image-x-p image)
	       (index+ src-x (image-x-left-pad (the image-x image)))
	     src-x))
	 (image-width (image-width image))
	 (image-height (image-height image))
	 (width (min (or width image-width) (index- image-width src-x)))
	 (height (min (or height image-height) (index- image-height src-y)))
	 (depth (image-depth image))
	 (display (drawable-display drawable))
	 (bitmap-format (display-bitmap-format display))
	 (unit (bitmap-format-unit bitmap-format))
	 (byte-lsb-first-p (display-image-lsb-first-p display))
	 (bit-lsb-first-p  (bitmap-format-lsb-first-p bitmap-format)))
    (declare (type (member :bitmap :xy-pixmap :z-pixmap) format)
	     (type fixnum src-x image-width image-height width height)
	     (type image-depth depth)
	     (type display display)
	     (type bitmap-format bitmap-format)
	     (type (member 8 16 32) unit)
	     (type generalized-boolean byte-lsb-first-p bit-lsb-first-p))
    (when (and bitmap-p (not (index= depth 1)))
      (error "Bitmaps must have depth 1"))
    (unless (<= 0 src-x (index1- (image-width image)))
      (error "src-x not inside image"))
    (unless (<= 0 src-y (index1- (image-height image)))
      (error "src-y not inside image"))
    (when (and (index> width 0) (index> height 0))
      (multiple-value-bind (pad bits-per-pixel)
	  (ecase format
	    ((:bitmap :xy-pixmap)
	      (values (bitmap-format-pad bitmap-format) 1))
	    (:z-pixmap
	      (if (= depth 1) 
		  (values (bitmap-format-pad bitmap-format) 1)
		(let ((pixmap-format
			(find depth (display-pixmap-formats display)
			      :key #'pixmap-format-depth)))
		  (declare (type (or null pixmap-format) pixmap-format))
		  (if (null pixmap-format)
		      (error "The depth of the image ~s does not match any server pixmap format." image))
		  (if (not (= (etypecase image
				(image-z (image-z-bits-per-pixel image))
				(image-x (image-x-bits-per-pixel image)))
			      (pixmap-format-bits-per-pixel pixmap-format)))
		      ;; We could try to use the "/* XXX slow, but works */"
		      ;; code in XPutImage from X11R4 here.  However, that
		      ;; would require considerable support code
		      ;; (see XImUtil.c, etc).
		      (error "The bits-per-pixel of the image ~s does not match any server pixmap format." image))
		  (values (pixmap-format-scanline-pad pixmap-format)
			  (pixmap-format-bits-per-pixel pixmap-format))))))
	(declare (type (member 8 16 32) pad)
		 (type (member 1 4 8 16 24 32) bits-per-pixel))
	(let* ((left-pad
		 (if (or (eq format :xy-pixmap) (= depth 1))
		     (index-mod src-x (index-min pad +image-pad+))
		   0))
	       (left-padded-src-x (index- src-x left-pad))
	       (left-padded-width (index+ width left-pad))
	       (bits-per-line (index* left-padded-width bits-per-pixel))
	       (padded-bits-per-line
		 (index* (index-ceiling bits-per-line pad) pad))
	       (padded-bytes-per-line (index-ceiling padded-bits-per-line 8))
	       (request-bytes-per-line
		 (ecase format
		   ((:bitmap :xy-pixmap) (index* padded-bytes-per-line depth))
		   (:z-pixmap padded-bytes-per-line)))
	       (max-bytes-per-request
		 (index* (index- (display-max-request-length display) 6) 4))
	       (max-request-height
		 (floor max-bytes-per-request request-bytes-per-line)))
	  (declare (type card8 left-pad)
		   (type int16 left-padded-src-x)
		   (type card16 left-padded-width)
		   (type array-index bits-per-line padded-bits-per-line
			 padded-bytes-per-line request-bytes-per-line
			 max-bytes-per-request max-request-height))
	  ;; Be sure that a scanline can fit in a request
	  (when (index-zerop max-request-height)
	    (error "Can't even fit one image scanline in a request"))
	  ;; Be sure a scanline can fit in a buffer
	  (buffer-ensure-size display padded-bytes-per-line)
	  ;; Send the image in multiple requests to avoid exceeding the
	  ;; request limit
	  (do* ((request-src-y src-y (index+ request-src-y request-height))
		(request-y y (index+ request-y request-height))
		(height-remaining
 		  height (the fixnum (- height-remaining request-height)))
		(request-height
		  (index-min height-remaining max-request-height)
		  (index-min height-remaining max-request-height)))
	       ((<= height-remaining 0))
	    (declare (type array-index request-src-y request-height)
		     (fixnum height-remaining))
	    (let* ((request-bytes (index* request-bytes-per-line request-height))
		   (request-words (index-ceiling request-bytes 4))
		   (request-length (index+ request-words 6)))
	      (declare (type array-index request-bytes)
		       (type card16 request-words request-length))
	      (with-buffer-request (display +x-putimage+ :gc-force gcontext)
		((data (member :bitmap :xy-pixmap :z-pixmap))
		 (cond ((or (eq format :bitmap) bitmap-p) :bitmap)
		       ((plusp left-pad) :xy-pixmap)
		       (t format)))
		(drawable drawable)
		(gcontext gcontext)
		(card16 width request-height)
		(int16 x request-y)
		(card8 left-pad depth)
		(pad16 nil)
		(progn 
		  (length-put 2 request-length)
		  (setf (buffer-boffset display) (advance-buffer-offset 24))
		  (etypecase image
		    (image-x
		      (ecase (image-x-format (the image-x image))
			((:bitmap :xy-pixmap)
			  (write-xy-format-image-x
			    display image left-padded-src-x request-src-y
			    left-padded-width request-height
			    padded-bytes-per-line
			    unit byte-lsb-first-p bit-lsb-first-p))
			(:z-pixmap
			  (write-z-format-image-x
			    display image left-padded-src-x request-src-y
			    left-padded-width request-height
			    padded-bytes-per-line
			    unit byte-lsb-first-p bit-lsb-first-p))))
		    (image-xy
		      (write-image-xy
			display image left-padded-src-x request-src-y
			left-padded-width request-height
			padded-bytes-per-line
			unit byte-lsb-first-p bit-lsb-first-p))
		    (image-z
		      (write-image-z
			display image left-padded-src-x request-src-y
			left-padded-width request-height
			padded-bytes-per-line
			unit byte-lsb-first-p bit-lsb-first-p)))
		  ;; Be sure the request is padded to a multiple of 4 bytes
		  (buffer-pad-request display (index- (index* request-words 4) request-bytes))
		  )))))))))

;;;-----------------------------------------------------------------------------
;;; COPY-IMAGE

(defun xy-format-image-x->image-x (image x y width height)
  (declare (type image-x image)
	   (type card16 x y width height)
	   (clx-values image-x))
  (let* ((padded-x (index+ x (image-x-left-pad image)))
	 (left-pad (index-mod padded-x 8))
	 (x (index- padded-x left-pad))
	 (unit (image-x-unit image))
	 (byte-lsb-first-p (image-x-byte-lsb-first-p image))
	 (bit-lsb-first-p (image-x-bit-lsb-first-p image))
	 (pad (image-x-pad image))
	 (padded-width
	   (index* (index-ceiling (index+ width left-pad) pad) pad))
	 (padded-bytes-per-line (index-ceiling padded-width 8))
	 (padded-bytes-per-plane (index* padded-bytes-per-line height))
	 (length (index* padded-bytes-per-plane (image-depth image)))
	 (obuf (make-array length :element-type 'card8)))
    (declare (type card16 x)
	     (type card8 left-pad)
	     (type (member 8 16 32) unit pad)
	     (type array-index padded-width padded-bytes-per-line
		   padded-bytes-per-plane length)
	     (type buffer-bytes obuf))
    (dotimes (plane (image-depth image))
      (let ((data-start
	      (index* (image-x-bytes-per-line image)
		      (image-height image)
		      plane))
	    (obuf-start
	      (index* padded-bytes-per-plane
		      plane)))
	(declare (type array-index data-start obuf-start))
	(write-xy-format-image-x-data
	  (image-x-data image) obuf data-start obuf-start
	  x y width height 
	  (image-x-bytes-per-line image) padded-bytes-per-line
	  unit byte-lsb-first-p bit-lsb-first-p
	  unit byte-lsb-first-p bit-lsb-first-p)))
    (create-image
      :width width :height height :depth (image-depth image)
      :data obuf :format (image-x-format image) :bits-per-pixel 1
      :bytes-per-line padded-bytes-per-line
      :unit unit :pad pad :left-pad left-pad
      :byte-lsb-first-p byte-lsb-first-p :bit-lsb-first-p bit-lsb-first-p)))

(defun z-format-image-x->image-x (image x y width height)
  (declare (type image-x image)
	   (type card16 x y width height)
	   (clx-values image-x))
  (let* ((padded-x (index+ x (image-x-left-pad image)))
	 (left-pad
	   (if (index= (image-depth image) 1)
	       (index-mod padded-x 8)
	     0))
	 (x (index- padded-x left-pad))
	 (bits-per-pixel (image-x-bits-per-pixel image))
	 (unit (image-x-unit image))
	 (byte-lsb-first-p (image-x-byte-lsb-first-p image))
	 (bit-lsb-first-p (image-x-bit-lsb-first-p image))
	 (pad (image-x-pad image))
	 (bits-per-line (index* (index+ width left-pad) bits-per-pixel))
	 (padded-bits-per-line (index* (index-ceiling bits-per-line pad) pad))
	 (padded-bytes-per-line (index-ceiling padded-bits-per-line 8))
	 (padded-bytes-per-plane (index* padded-bytes-per-line height))
	 (length (index* padded-bytes-per-plane (image-depth image)))
	 (obuf (make-array length :element-type 'card8)))
    (declare (type card16 x)
	     (type card8 left-pad)
	     (type (member 8 16 32) unit pad)
	     (type array-index bits-per-pixel padded-bytes-per-line
		   padded-bytes-per-plane length)
	     (type buffer-bytes obuf))
    (write-z-format-image-x-data
      (image-x-data image) obuf 0 0
      x y width height 
      (image-x-bytes-per-line image) padded-bytes-per-line
      bits-per-pixel
      unit byte-lsb-first-p bit-lsb-first-p
      unit byte-lsb-first-p bit-lsb-first-p)
    (create-image
      :width width :height height :depth (image-depth image)
      :data obuf :format :z-pixmap :bits-per-pixel bits-per-pixel
      :bytes-per-line padded-bytes-per-line
      :unit unit :pad pad :left-pad left-pad
      :byte-lsb-first-p byte-lsb-first-p :bit-lsb-first-p bit-lsb-first-p)))

(defun image-x->image-x  (image x y width height)
  (declare (type image-x image)
	   (type card16 x y width height)
	   (clx-values image-x))
  (ecase (image-x-format image)
    ((:bitmap :xy-pixmap)
      (xy-format-image-x->image-x image x y width height))
    (:z-pixmap
      (z-format-image-x->image-x image x y width height))))

(defun image-x->image-xy (image x y width height)
  (declare (type image-x image)
	   (type card16 x y width height)
	   (clx-values image-xy))
  (unless (or (eq (image-x-format image) :bitmap)
	      (eq (image-x-format image) :xy-pixmap)
	      (and (eq (image-x-format image) :z-pixmap)
		   (index= (image-depth image) 1)))
    (error "Format conversion from ~S to ~S not supported"
	   (image-x-format image) :xy-pixmap))
  (read-image-xy
    (image-x-data image) 0 (length (image-x-data image)) nil
    (index+ x (image-x-left-pad image)) y width height
    (image-depth image) (image-x-bytes-per-line image)
    (index* (image-x-bytes-per-line image) (image-height image))
    (image-x-unit image) (image-x-byte-lsb-first-p image)
    (image-x-bit-lsb-first-p image)))

(defun image-x->image-z  (image x y width height)
  (declare (type image-x image)
	   (type card16 x y width height)
	   (clx-values image-z))
  (unless (or (eq (image-x-format image) :z-pixmap)
	      (eq (image-x-format image) :bitmap)
	      (and (eq (image-x-format image) :xy-pixmap)
		   (index= (image-depth image) 1)))
    (error "Format conversion from ~S to ~S not supported"
	   (image-x-format image) :z-pixmap))
  (read-image-z
    (image-x-data image) 0 (length (image-x-data image)) nil
    (index+ x (image-x-left-pad image)) y width height
    (image-depth image) (image-x-bytes-per-line image)
    (image-x-bits-per-pixel image)
    (image-x-unit image) (image-x-byte-lsb-first-p image)
    (image-x-bit-lsb-first-p image)))

(defun copy-pixarray (array x y width height bits-per-pixel)
  (declare (type pixarray array)
	   (type card16 x y width height)
	   (type (member 1 4 8 16 24 32) bits-per-pixel))
  (let* ((bits-per-line (index* bits-per-pixel width))
	 (padded-bits-per-line
	   (index* (index-ceiling bits-per-line +image-pad+) +image-pad+))
	 (padded-width (index-ceiling padded-bits-per-line bits-per-pixel))
	 (copy (make-array (list height padded-width)
			   :element-type (array-element-type array))))
    (declare (type array-index bits-per-line padded-bits-per-line padded-width)
	     (type pixarray copy))
    #.(declare-buffun)
    (unless (fast-copy-pixarray array copy x y width height bits-per-pixel)
      (macrolet
	((copy (array-type element-type)
	   `(let ((array array)
		  (copy copy))
	      (declare (type ,array-type array copy))
	      (do* ((dst-y 0 (index1+ dst-y))
		    (src-y y (index1+ src-y)))
		   ((index>= dst-y height))
		(declare (type card16 dst-y src-y))
		(do* ((dst-x 0 (index1+ dst-x))
		      (src-x x (index1+ src-x)))
		     ((index>= dst-x width))
		  (declare (type card16 dst-x src-x))
		  (setf (aref copy dst-y dst-x)
			(the ,element-type
			     (aref array src-y src-x))))))))
	(ecase bits-per-pixel
	  (1  (copy pixarray-1  pixarray-1-element-type))
	  (4  (copy pixarray-4  pixarray-4-element-type))
	  (8  (copy pixarray-8  pixarray-8-element-type))
	  (16 (copy pixarray-16 pixarray-16-element-type))
	  (24 (copy pixarray-24 pixarray-24-element-type))
	  (32 (copy pixarray-32 pixarray-32-element-type)))))
    copy))

(defun image-xy->image-x (image x y width height)
  (declare (type image-xy image)
	   (type card16 x y width height)
	   (clx-values image-x))
  (let* ((padded-bits-per-line
	   (index* (index-ceiling width +image-pad+) +image-pad+))
	 (padded-bytes-per-line (index-ceiling padded-bits-per-line 8))
	 (padded-bytes-per-plane (index* padded-bytes-per-line height))
	 (bytes-total (index* padded-bytes-per-plane (image-depth image)))
	 (data (make-array bytes-total :element-type 'card8)))
    (declare (type array-index padded-bits-per-line padded-bytes-per-line
		   padded-bytes-per-plane bytes-total)
	     (type buffer-bytes data))
    (let ((index 0))
      (declare (type array-index index))
      (dolist (bitmap (image-xy-bitmap-list image))
	(declare (type pixarray-1 bitmap))
	(write-pixarray
	  data index bitmap x y width height padded-bytes-per-line 1
	  +image-unit+ +image-byte-lsb-first-p+ +image-bit-lsb-first-p+)
	(index-incf index padded-bytes-per-plane)))
    (create-image
      :width width :height height :depth (image-depth image)
      :data data :format :xy-pixmap :bits-per-pixel 1
      :bytes-per-line padded-bytes-per-line
      :unit +image-unit+ :pad +image-pad+
      :byte-lsb-first-p +image-byte-lsb-first-p+
      :bit-lsb-first-p +image-bit-lsb-first-p+)))

(defun image-xy->image-xy (image x y width height)
  (declare (type image-xy image)
	   (type card16 x y width height)
	   (clx-values image-xy))
  (create-image
    :width width :height height :depth (image-depth image)
    :data (mapcar
	    #'(lambda (array)
		(declare (type pixarray-1 array))
		(copy-pixarray array x y width height 1))
	    (image-xy-bitmap-list image))))

(defun image-xy->image-z (image x y width height)
  (declare (type image-xy image)
	   (type card16 x y width height)
	   (ignore image x y width height))
  (error "Format conversion from ~S to ~S not supported"
	 :xy-pixmap :z-pixmap))

(defun image-z->image-x (image x y width height)
  (declare (type image-z image)
	   (type card16 x y width height)
	   (clx-values image-x))
  (let* ((bits-per-line (index* width (image-z-bits-per-pixel image)))
	 (padded-bits-per-line
	   (index* (index-ceiling bits-per-line +image-pad+) +image-pad+))
	 (padded-bytes-per-line (index-ceiling padded-bits-per-line 8))
	 (bytes-total
	   (index* padded-bytes-per-line height (image-depth image)))
	 (data (make-array bytes-total :element-type 'card8))
	 (bits-per-pixel (image-z-bits-per-pixel image)))
    (declare (type array-index bits-per-line padded-bits-per-line
		   padded-bytes-per-line bytes-total)
	     (type buffer-bytes data)
	     (type (member 1 4 8 16 24 32) bits-per-pixel))
    (write-pixarray
      data 0 (image-z-pixarray image) x y width height padded-bytes-per-line 
      (image-z-bits-per-pixel image)
      +image-unit+ +image-byte-lsb-first-p+ +image-bit-lsb-first-p+)
    (create-image
      :width width :height height :depth (image-depth image)
      :data data :format :z-pixmap
      :bits-per-pixel bits-per-pixel
      :bytes-per-line padded-bytes-per-line
      :unit +image-unit+ :pad +image-pad+
      :byte-lsb-first-p +image-byte-lsb-first-p+
      :bit-lsb-first-p +image-bit-lsb-first-p+)))

(defun image-z->image-xy (image x y width height)
  (declare (type image-z image)
	   (type card16 x y width height)
	   (ignore image x y width height))
  (error "Format conversion from ~S to ~S not supported"
	 :z-pixmap :xy-pixmap))

(defun image-z->image-z (image x y width height)
  (declare (type image-z image)
	   (type card16 x y width height)
	   (clx-values image-z))
  (create-image
    :width width :height height :depth (image-depth image)
    :data (copy-pixarray
	    (image-z-pixarray image) x y width height
	    (image-z-bits-per-pixel image))))

(defun copy-image (image &key (x 0) (y 0) width height result-type)
  ;; Copy with optional sub-imaging and format conversion.
  ;; result-type defaults to (type-of image)
  (declare (type image image)
	   (type card16 x y)
	   (type (or null card16) width height) ;; Default from image
	   (type (or null (member image-x image-xy image-z)) result-type))
  (declare (clx-values image))
  (let* ((image-width (image-width image))
	 (image-height (image-height image))
	 (width (or width image-width))
	 (height (or height image-height)))
    (declare (type card16 image-width image-height width height))
    (unless (<= 0 x (the fixnum (1- image-width)))
      (error "x not inside image"))
    (unless (<= 0 y (the fixnum (1- image-height)))
      (error "y not inside image"))
    (setq width (index-min width (max (the fixnum (- image-width x)) 0)))
    (setq height (index-min height (max (the fixnum (- image-height y)) 0)))
    (let ((copy
	    (etypecase image
	      (image-x
		(ecase result-type
		  ((nil image-x) (image-x->image-x image x y width height))
		  (image-xy (image-x->image-xy image x y width height))
		  (image-z  (image-x->image-z  image x y width height))))
	      (image-xy
		(ecase result-type
		  (image-x (image-xy->image-x image x y width height))
		  ((nil image-xy) (image-xy->image-xy image x y width height))
		  (image-z  (image-xy->image-z image x y width height))))
	      (image-z 
		(ecase result-type
		  (image-x (image-z->image-x image x y width height))
		  (image-xy  (image-z->image-xy image x y width height))
		  ((nil image-z) (image-z->image-z image x y width height)))))))
      (declare (type image copy))
      (setf (image-plist copy) (copy-list (image-plist image)))
      (when (and (image-x-hot image) (not (index-zerop x)))
	(setf (image-x-hot copy) (index- (image-x-hot image) x)))
      (when (and (image-y-hot image) (not (index-zerop y)))
	(setf (image-y-hot copy) (index- (image-y-hot image) y)))
      copy)))


;;;-----------------------------------------------------------------------------
;;; Image I/O functions


(defun read-bitmap-file (pathname)
  ;; Creates an image from a C include file in standard X11 format
  (declare (type (or pathname string stream) pathname))
  (declare (clx-values image))
  (with-open-file (fstream pathname :direction :input)
    (let ((line "")
	  (properties nil)
	  (name nil)
	  (name-end nil))
      (declare (type string line)
	       (type stringable name)
	       (type list properties))
      ;; Get properties
      (loop
	(setq line (read-line fstream))
	(unless (char= (aref line 0) #\#) (return))
	(flet ((read-keyword (line start end)
		 (kintern
		   (substitute
		     #\- #\_
		     (#-excl string-upcase
		      #+excl correct-case
		      (subseq line start end))
		     :test #'char=))))
	  (when (null name)
	    (setq name-end (position #\_ line :test #'char= :from-end t)
		  name (read-keyword line 8 name-end))
	    (unless (eq name :image)
	      (setf (getf properties :name) name)))
	  (let* ((ind-start (index1+ name-end))
		 (ind-end (position #\Space line :test #'char=
				    :start ind-start))
		 (ind (read-keyword line ind-start ind-end))
		 (val-start (index1+ ind-end))
		 (val (parse-integer line :start val-start)))
	    (setf (getf properties ind) val))))
      ;; Calculate sizes
      (multiple-value-bind (width height depth left-pad)
	  (flet ((extract-property (ind &rest default)
		   (prog1 (apply #'getf properties ind default)
			  (remf properties ind))))
	    (values (extract-property :width)
		    (extract-property :height)
		    (extract-property :depth 1)
		    (extract-property :left-pad 0)))
	(declare (type (or null card16) width height)
		 (type image-depth depth)
		 (type card8 left-pad))
	(unless (and width height) (error "Not a BITMAP file"))
	(let* ((bits-per-pixel
		 (cond ((index> depth 24) 32)
		       ((index> depth 16) 24)
		       ((index> depth 8)  16)
		       ((index> depth 4)   8)
		       ((index> depth 1)   4)
		       (t                  1)))
	       (bits-per-line (index* width bits-per-pixel))
	       (bytes-per-line (index-ceiling bits-per-line 8))
	       (padded-bits-per-line
		 (index* (index-ceiling bits-per-line 32) 32))
	       (padded-bytes-per-line
		 (index-ceiling padded-bits-per-line 8))
	       (data (make-array (* padded-bytes-per-line height)
				 :element-type 'card8))
	       (line-base 0)
	       (byte 0))
	  (declare (type array-index bits-per-line bytes-per-line
			 padded-bits-per-line padded-bytes-per-line
			 line-base byte)
		   (type buffer-bytes data))
	  (with-vector (data buffer-bytes)
	    (flet ((parse-hex (char)
		     (second
		       (assoc char
			      '((#\0  0) (#\1  1) (#\2  2) (#\3  3)
				(#\4  4) (#\5  5) (#\6  6) (#\7  7)
				(#\8  8) (#\9  9) (#\a 10) (#\b 11)
				(#\c 12) (#\d 13) (#\e 14) (#\f 15))
			      :test #'char-equal))))
	      (declare (inline parse-hex))
	      ;; Read data
	      ;; Note: using read-line instead of read-char would be 20% faster,
	      ;;       but would cons a lot of garbage...
	      (dotimes (i height)
		(dotimes (j bytes-per-line)
		  (loop (when (eql (read-char fstream) #\x) (return)))
		  (setf (aref data (index+ line-base byte))
			(index+ (index-ash (parse-hex (read-char fstream)) 4)
				(parse-hex (read-char fstream))))
		  (incf byte))
		(setq byte 0
		      line-base (index+ line-base padded-bytes-per-line)))))
	  ;; Compensate for left-pad in width and x-hot
	  (index-decf width left-pad)
	  (when (and (getf properties :x-hot) (plusp (getf properties :x-hot)))
	    (index-decf (getf properties :x-hot) left-pad))
	  (create-image
	    :width width :height height
	    :depth depth :bits-per-pixel bits-per-pixel
	    :data data :plist properties :format :z-pixmap
	    :bytes-per-line padded-bytes-per-line
	    :unit 32 :pad 32 :left-pad left-pad
	    :byte-lsb-first-p t :bit-lsb-first-p t))))))

(defun write-bitmap-file (pathname image &optional name)
  ;; Writes an image to a C include file in standard X11 format
  ;; NAME argument used for variable prefixes.  Defaults to "image"
  (declare (type (or pathname string stream) pathname)
	   (type image image)
	   (type (or null stringable) name))
  (unless (typep image 'image-x)
    (setq image (copy-image image :result-type 'image-x)))
  (let* ((plist (image-plist image))
	 (name (or name (image-name image) 'image))
	 (left-pad (image-x-left-pad image))
	 (width (index+ (image-width image) left-pad))
	 (height (image-height image))
	 (depth
	   (if (eq (image-x-format image) :z-pixmap)
	       (image-depth image)
	     1))
	 (bits-per-pixel (image-x-bits-per-pixel image))
	 (bits-per-line (index* width bits-per-pixel))
	 (bytes-per-line (index-ceiling bits-per-line 8))
	 (last (index* bytes-per-line height))
	 (count 0))
    (declare (type list plist)
	     (type stringable name)
	     (type card8 left-pad)
	     (type card16 width height)
	     (type (member 1 4 8 16 24 32) bits-per-pixel)
	     (type image-depth depth)
	     (type array-index bits-per-line bytes-per-line count last))
    ;; Move x-hot by left-pad, if there is an x-hot, so image readers that
    ;; don't know about left pad get the hot spot in the right place.  We have
    ;; already increased width by left-pad.
    (when (getf plist :x-hot)
      (setq plist (copy-list plist))
      (index-incf (getf plist :x-hot) left-pad))
    (with-image-data-buffer (data last)
      (multiple-value-bind (image-swap-function image-swap-lsb-first-p)
	  (image-swap-function
	    bits-per-pixel
	    (image-x-unit image) (image-x-byte-lsb-first-p image)
	    (image-x-bit-lsb-first-p image) 32 t t)
	(declare (type symbol image-swap-function)
		 (type generalized-boolean image-swap-lsb-first-p))
	(funcall
	  (symbol-function image-swap-function) (image-x-data image)
	  data 0 0 bytes-per-line (image-x-bytes-per-line image)
	  bytes-per-line height image-swap-lsb-first-p))
      (with-vector (data buffer-bytes)
	(setq name (string-downcase (string name)))
	(with-open-file (fstream pathname :direction :output)
	  (format fstream "#define ~a_width ~d~%" name width)
	  (format fstream "#define ~a_height ~d~%" name height)
	  (unless (= depth 1)
	    (format fstream "#define ~a_depth ~d~%" name depth))
	  (unless (zerop left-pad)
	    (format fstream "#define ~a_left_pad ~d~%" name left-pad))
	  (do ((prop plist (cddr prop)))
	      ((endp prop))
	    (when (and (not (member (car prop) '(:width :height)))
		       (numberp (cadr prop)))
	      (format fstream "#define ~a_~a ~d~%"
		      name
		      (substitute
			#\_ #\- (string-downcase (string (car prop)))
			:test #'char=)
		      (cadr prop))))
	  (format fstream "static char ~a_bits[] = {" name)
	  (dotimes (i height)
	    (dotimes (j bytes-per-line)
	      (when (zerop (index-mod count 15))
		(terpri fstream)
		(write-char #\space fstream))
	      (write-string "0x" fstream)
	      ;; Faster than (format fstream "0x~2,'0x," byte)
	      (let ((byte (aref data count))
		    (translate "0123456789abcdef"))
		(declare (type card8 byte))
		(write-char (char translate (ldb (byte 4 4) byte)) fstream)
		(write-char (char translate (ldb (byte 4 0) byte)) fstream))
	      (index-incf count)
	      (unless (index= count last)
		(write-char #\, fstream))))
	  (format fstream "};~%"))))))

(defun bitmap-image (&optional plist &rest patterns)
  ;; Create an image containg pattern
  ;; PATTERNS are bit-vector constants (e.g. #*10101)
  ;; If the first parameter is a list, its used as the image property-list.
  (declare (type (or list bit-vector) plist)
	   (type list patterns)) ;; list of bitvector
  (declare (clx-values image))
  (unless (listp plist)
    (push plist patterns)
    (setq plist nil))
  (let* ((width (length (first patterns)))
	 (height (length patterns))
	 (bitarray (make-array (list height width) :element-type 'bit))
	 (row 0))
    (declare (type card16 width height row)
	     (type pixarray-1 bitarray))
    (dolist (pattern patterns)
      (declare (type simple-bit-vector pattern))
      (dotimes (col width)
	(declare (type card16 col))
	(setf (aref bitarray row col) (the bit (aref pattern col))))
      (incf row))
    (create-image :width width :height height :plist plist :data bitarray)))

(defun image-pixmap (drawable image &key gcontext width height depth)
  ;; Create a pixmap containing IMAGE. Size defaults from the image.
  ;; DEPTH is the pixmap depth.
  ;; GCONTEXT is used for putting the image into the pixmap.
  ;; If none is supplied, then one is created, used then freed.
  (declare (type drawable drawable)
	   (type image image)
	   (type (or null gcontext) gcontext)
	   (type (or null card16) width height)
	   (type (or null card8) depth))
  (declare (clx-values pixmap))
  (let* ((image-width (image-width image))
	 (image-height (image-height image))
	 (image-depth (image-depth image))
	 (width (or width image-width))
	 (height (or height image-height))
	 (depth (or depth image-depth))
	 (pixmap (create-pixmap :drawable drawable
			       :width width
			       :height height
			       :depth depth))
	 (gc (or gcontext (create-gcontext
			    :drawable pixmap
			    :foreground 1
			    :background 0))))
    (unless (= depth image-depth)
      (if (= image-depth 1)
	  (unless gcontext (xlib::required-arg gcontext))
	(error "Pixmap depth ~d incompatible with image depth ~d"
	       depth image-depth)))	       
    (put-image pixmap gc image :x 0 :y 0 :bitmap-p (and (= image-depth 1)
							gcontext))
    ;; Tile when image-width is less than the pixmap width, or
    ;; the image-height is less than the pixmap height.
    ;; ??? Would it be better to create a temporary pixmap and 
    ;; ??? let the server do the tileing?
    (do ((x image-width (+ x image-width)))
	((>= x width))
      (copy-area pixmap gc 0 0 image-width image-height pixmap x 0)
      (incf image-width image-width))
    (do ((y image-height (+ y image-height)))
	((>= y height))
      (copy-area pixmap gc 0 0 image-width image-height pixmap 0 y)
      (incf image-height image-height))
    (unless gcontext (free-gcontext gc))
    pixmap))

