;;; -*- Mode: LISP; Syntax: Common-lisp; Package: XLIB; Base: 10; Lowercase: Yes -*-

;;; CLX text keyboard and pointer requests

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

;; Strings are broken up into chunks of this size
(defparameter *max-string-size* 254)

;; In the functions below, the transform is used to convert an element of the
;; sequence into a font index.  The transform is applied to each element of the
;; (sub)sequence, until either the transform returns nil or the end of the
;; (sub)sequence is reached.  If transform returns nil for an element, the
;; index of that element in the sequence is returned, otherwise nil is
;; returned.

(deftype translation-function ()
  #+explorer t
  #-explorer
  '(function (sequence array-index array-index (or null font) vector array-index)
	     (values array-index (or null int16 font) (or null int32))))

;; In the functions below, if width is specified, it is assumed to be the pixel
;; width of whatever string of glyphs is actually drawn.  Specifying width will
;; allow for appending the output of subsequent calls to the same protocol
;; request, provided gcontext has not been modified in the interim.  If width
;; is not specified, appending of subsequent output might not occur.
;; Specifying width is simply a hint, for performance.  Note that specifying
;; width may be difficult if transform can return nil.

(defun translate-default (src src-start src-end font dst dst-start)
  ;; dst is guaranteed to have room for (- src-end src-start) integer elements,
  ;; starting at dst-start; whether dst holds 8-bit or 16-bit elements depends
  ;; on context.  font is the current font, if known.  The function should
  ;; translate as many elements of src as possible into indexes in the current
  ;; font, and store them into dst.
  ;;
  ;; The first return value should be the src index of the first untranslated
  ;; element.  If no further elements need to be translated, the second return
  ;; value should be nil.  If a horizontal motion is required before further
  ;; translation, the second return value should be the delta in x coordinate.
  ;; If a font change is required for further translation, the second return
  ;; value should be the new font.  If known, the pixel width of the translated
  ;; text can be returned as the third value; this can allow for appending of
  ;; subsequent output to the same protocol request, if no overall width has
  ;; been specified at the higher level.
  ;; (returns values: ending-index
  ;;                  (OR null horizontal-motion font)
  ;;                  (OR null translated-width))
  (declare (type sequence src)
	   (type array-index src-start src-end dst-start)
	   (type (or null font) font)
	   (type vector dst)
	   (inline graphic-char-p))
  (declare (clx-values integer (or null integer font) (or null integer)))

  (let ((min-char-index (and font (xlib:font-min-char font)))
        (max-char-index (and font (xlib:font-max-char font))))
    (if (stringp src)
	(do ((i src-start (index+ i 1))
	     (j dst-start (index+ j 1))
	     (char))
	    ((index>= i src-end)
	     i)
	  (declare (type array-index i j))
	  (setf char (char->card8 (char src i)))
	  (if (and font (or (< char min-char-index) (> char max-char-index)))
	      (return i)
	      (setf (aref dst j) char)))
	(do ((i src-start (index+ i 1))
	     (j dst-start (index+ j 1))
	     (elt))
	    ((index>= i src-end)
	     i)
	  (declare (type array-index i j))
	  (setq elt (elt src i))
	  (when (characterp elt) (setq elt (char->card8 elt)))
	  (if (or (not (integerp elt))
                  (and font
                       (< elt min-char-index)
                       (> elt max-char-index)))
	      (return i)
	      (setf (aref dst j) elt))))))

;; There is a question below of whether translate should always be required, or
;; if not, what the default should be or where it should come from.  For
;; example, the default could be something that expected a string as src and
;; translated the CL standard character set to ASCII indexes, and ignored fonts
;; and bits.  Or the default could expect a string but otherwise be "system
;; dependent".  Or the default could be something that expected a vector of
;; integers and did no translation.  Or the default could come from the
;; gcontext (but what about text-extents and text-width?).

(defun text-extents (font sequence &key (start 0) end translate)
  ;; If multiple fonts are involved, font-ascent and font-descent will be the
  ;; maximums.  If multiple directions are involved, the direction will be nil.
  ;; Translate will always be called with a 16-bit dst buffer.
  (declare (type sequence sequence)
	   (type (or font gcontext) font))
  (declare (type (or null translation-function) translate)
	   #+clx-ansi-common-lisp
	   (dynamic-extent translate)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg #+Genera * #-Genera translate))
  (declare (clx-values width ascent descent left right
		  font-ascent font-descent direction
		  (or null array-index)))
  (when (type? font 'gcontext)
    (force-gcontext-changes font)
    (setq font (gcontext-font font t)))
  (check-type font font)
  (let* ((left-bearing 0)
	 (right-bearing 0)
	 ;; Sum of widths
	 (width 0)
	 (ascent 0)
	 (descent 0)
	 (overall-ascent (font-ascent font))
	 (overall-descent (font-descent font))
	 (overall-direction (font-direction font))	 
	 (next-start nil)
	 (display (font-display font)))
    (declare (type int16 ascent descent overall-ascent overall-descent)
	     (type int32 left-bearing right-bearing width)
	     (type (or null array-index) next-start)
	     (type display display))
    (with-display (display)
      (do* ((wbuf (display-tbuf16 display))
	    (src-end (or end (length sequence)))
	    (src-start start (index+ src-start buf-end))
	    (end (index-min src-end (index+ src-start +buffer-text16-size+))
		 (index-min src-end (index+ src-start +buffer-text16-size+)))
	    (buf-end 0)
	    (new-font)
	    (font-ascent 0)
	    (font-descent 0)
	    (font-direction)
	    (stop-p nil))
	   ((or stop-p (index>= src-start src-end))
	    (when (index< src-start src-end)
	      (setq next-start src-start)))
	(declare (type buffer-text16 wbuf)
		 (type array-index src-start src-end end buf-end)
		 (type int16 font-ascent font-descent)
		 (type generalized-boolean stop-p))
	;; Translate the text
	(multiple-value-setq (buf-end new-font)
	  (funcall (or translate #'translate-default)
		   sequence src-start end font wbuf 0))
	(setq buf-end (- buf-end src-start))
	(cond ((null new-font) (setq stop-p t))
	      ((integerp new-font) (incf width (the int32 new-font))))
	
	(let (w a d l r)
	  (if (or (font-char-infos-internal font) (font-local-only-p font))
	      ;; Calculate text extents locally
	      (progn
		(multiple-value-setq (w a d l r)
		  (text-extents-local font wbuf 0 buf-end nil))
		(setq font-ascent (the int16 (font-ascent font))
		      font-descent (the int16 (font-descent font))
		      font-direction (font-direction font)))
	    ;; Let the server calculate text extents
	    (multiple-value-setq
	      (w a d l r font-ascent font-descent font-direction)
	      (text-extents-server font wbuf 0 buf-end)))
	  (incf width (the int32 w))
	  (cond ((index= src-start start)
		 (setq left-bearing (the int32 l))
		 (setq right-bearing (the int32 r))
		 (setq ascent (the int16 a))
		 (setq descent (the int16 d)))
		(t
		 (setq left-bearing (the int32 (min left-bearing (the int32 l))))
		 (setq right-bearing (the int32 (max right-bearing (the int32 r))))
		 (setq ascent (the int16 (max ascent (the int16 a))))
		 (setq descent (the int16 (max descent (the int16 d)))))))

	(when (type? new-font 'font)
	  (setq font new-font))

	(setq overall-ascent (the int16 (max overall-ascent font-ascent)))
	(setq overall-descent (the int16 (max overall-descent font-descent)))
	(case overall-direction
	  (:unknown (setq overall-direction font-direction))
	  (:left-to-right (unless (eq font-direction :left-to-right)
			    (setq overall-direction nil)))
	  (:right-to-left (unless (eq font-direction :right-to-left)
			    (setq overall-direction nil))))))
    
    (values width
	    ascent
	    descent
	    left-bearing
	    right-bearing
	    overall-ascent
	    overall-descent
	    overall-direction
	    next-start)))

(defun text-width (font sequence &key (start 0) end translate)
  ;; Translate will always be called with a 16-bit dst buffer.
  (declare (type sequence sequence)
	   (type (or font gcontext) font)
	   (type array-index start)
	   (type (or null array-index) end))
  (declare (type (or null translation-function) translate)
	   #+clx-ansi-common-lisp
	   (dynamic-extent translate)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg #+Genera * #-Genera translate))
  (declare (clx-values integer (or null integer)))
  (when (type? font 'gcontext)
    (force-gcontext-changes font)
    (setq font (gcontext-font font t)))
  (check-type font font)
  (let* ((width 0)
	 (next-start nil)
	 (display (font-display font)))
    (declare (type int32 width)
	     (type (or null array-index) next-start)
	     (type display display))
    (with-display (display)
      (do* ((wbuf (display-tbuf16 display))
	    (src-end (or end (length sequence)))
	    (src-start start (index+ src-start buf-end))
	    (end (index-min src-end (index+ src-start +buffer-text16-size+))
		 (index-min src-end (index+ src-start +buffer-text16-size+)))
	    (buf-end 0)
	    (new-font)
	    (stop-p nil))
	   ((or stop-p (index>= src-start src-end))
	    (when (index< src-start src-end)
	      (setq next-start src-start)))
	(declare (type buffer-text16 wbuf)
		 (type array-index src-start src-end end buf-end)
		 (type generalized-boolean stop-p))
	;; Translate the text
	(multiple-value-setq (buf-end new-font)
	  (funcall (or translate #'translate-default)
		   sequence src-start end font wbuf 0))
	(setq buf-end (- buf-end src-start))
	(cond ((null new-font) (setq stop-p t))
	      ((integerp new-font) (incf width (the int32 new-font))))
	
	(incf width
	      (if (or (font-char-infos-internal font) (font-local-only-p font))
		  (text-extents-local font wbuf 0 buf-end :width-only)
		(text-width-server font wbuf 0 buf-end)))
	(when (type? new-font 'font)
	  (setq font new-font))))
    (values width next-start)))

(defun text-extents-server (font sequence start end)
  (declare (type font font)
	   (type sequence sequence)
	   (type array-index start end))
  (declare (clx-values width ascent descent left right font-ascent font-descent direction))
  (let ((display (font-display font))
	(length (index- end start))
	(font-id (font-id font)))
    (declare (type display display)
	     (type array-index length)
	     (type resource-id font-id))
    (with-buffer-request-and-reply (display +x-querytextextents+ 28 :sizes (8 16 32))
	 (((data boolean) (oddp length))
	  (length (index+ (index-ceiling length 2) 2))
	  (resource-id font-id)
	  ((sequence :format char2b :start start :end end :appending t)
	   sequence))
      (values
	(integer-get 16)
	(int16-get 12)
	(int16-get 14)
	(integer-get 20)
	(integer-get 24)
	(int16-get 8)
	(int16-get 10)
	(member8-get 1 :left-to-right :right-to-left)))))

(defun text-width-server (font sequence start end)
  (declare (type (or font gcontext) font)
	   (type sequence sequence)
	   (type array-index start end))
  (declare (clx-values integer))
  (let ((display (font-display font))
	(length (index- end start))
	(font-id (font-id font)))
    (declare (type display display)
	     (type array-index length)
	     (type resource-id font-id))
    (with-buffer-request-and-reply (display +x-querytextextents+ 28 :sizes 32)
	 (((data boolean) (oddp length))
	  (length (index+ (index-ceiling length 2) 2))
	  (resource-id font-id)
	  ((sequence :format char2b :start start :end end :appending t)
	   sequence))
      (values (integer-get 16)))))

(defun text-extents-local (font sequence start end width-only-p)
  (declare (type font font)
	   (type sequence sequence)
	   (type integer start end)
	   (type generalized-boolean width-only-p))
  (declare (clx-values width ascent descent overall-left overall-right))
  (let* ((char-infos (font-char-infos font))
	 (font-info (font-font-info font)))
    (declare (type font-info font-info))
    (declare (type (simple-array int16 (*)) char-infos))
    (if (zerop (length char-infos))
	;; Fixed width font
	(let* ((font-width (max-char-width font))
	       (font-ascent (max-char-ascent font))
	       (font-descent (max-char-descent font))
	       (width (* (index- end start) font-width)))
	  (declare (type int16 font-width font-ascent font-descent)
		   (type int32 width))
	  (if width-only-p
	      width
	    (values width
		    font-ascent
		    font-descent
		    (max-char-left-bearing font)
		    (+ width (- font-width) (max-char-right-bearing font)))))
      
      ;; Variable-width font
      (let* ((first-col (font-info-min-byte2 font-info))
	     (num-cols (1+ (- (font-info-max-byte2 font-info) first-col)))
	     (first-row (font-info-min-byte1 font-info))
	     (last-row (font-info-max-byte1 font-info))
	     (num-rows (1+ (- last-row first-row))))
	(declare (type card8 first-col first-row last-row)
		 (type card16 num-cols num-rows))
	(if (or (plusp first-row) (plusp last-row))
	    
	    ;; Matrix (16 bit) font
	    (macrolet ((char-info-elt (sequence elt)
			 `(let* ((char (the card16 (elt ,sequence ,elt)))
				 (row (- (ash char -8) first-row))
				 (col (- (logand char #xff) first-col)))
			    (declare (type card16 char)
				     (type int16 row col))
			    (if (and (< -1 row num-rows) (< -1 col num-cols))
				(index* 6 (index+ (index* row num-cols) col))
			      -1))))
	      (if width-only-p
		  (do ((i start (index1+ i))
		       (width 0))
		      ((index>= i end) width)
		    (declare (type array-index i)
			     (type int32 width))
		    (let ((n (char-info-elt sequence i)))
		      (declare (type fixnum n))
		      (unless (minusp n)  ;; Ignore characters not in the font
			(incf width (the int16 (aref char-infos (index+ 2 n)))))))
		;; extents
		(do ((i start (index1+ i))
		     (width 0)
		     (ascent #x-7fff)
		     (descent #x-7fff)
		     (left #x7fff)
		     (right #x-7fff))
		    ((index>= i end)
		     (values width ascent descent left right))
		  (declare (type array-index i)
			   (type int16 ascent descent)
			   (type int32 width left right))
		  (let ((n (char-info-elt sequence i)))
		    (declare (type fixnum n))
		    (unless (minusp n) ;; Ignore characters not in the font
		      (setq left (min left (+ width (aref char-infos n))))
		      (setq right (max right (+ width (aref char-infos (index1+ n)))))
		      (incf width (aref char-infos (index+ 2 n)))
		      (setq ascent (max ascent (aref char-infos (index+ 3 n))))
		      (setq descent (max descent (aref char-infos (index+ 4 n)))))))))
	  
	  ;; Non-matrix (8 bit) font
	  ;; The code here is identical to the above, except for the following macro:
	  (macrolet ((char-info-elt (sequence elt)
		       `(let ((col (- (the card16 (elt ,sequence ,elt)) first-col)))
			  (declare (type int16 col))
			  (if (< -1 col num-cols)
			      (index* 6 col)
			    -1))))
	    (if width-only-p
		(do ((i start (index1+ i))
		     (width 0))
		    ((index>= i end) width)
		  (declare (type array-index i)
			   (type int32 width))
		  (let ((n (char-info-elt sequence i)))
		    (declare (type fixnum n))
		    (unless (minusp n) ;; Ignore characters not in the font
		      (incf width (the int16 (aref char-infos (index+ 2 n)))))))
	      ;; extents
	      (do ((i start (index1+ i))
		   (width 0)
		   (ascent #x-7fff)
		   (descent #x-7fff)
		   (left #x7fff)
		   (right #x-7fff))
		  ((index>= i end)
		   (values width ascent descent left right))
		(declare (type array-index i)
			 (type int16 ascent descent)
			 (type int32 width left right))
		(let ((n (char-info-elt sequence i)))
		  (declare (type fixnum n))
		  (unless (minusp n) ;; Ignore characters not in the font
		    (setq left (min left (+ width (aref char-infos n))))
		    (setq right (max right (+ width (aref char-infos (index1+ n)))))
		    (incf width (aref char-infos (index+ 2 n)))
		    (setq ascent (max ascent (aref char-infos (index+ 3 n))))
		    (setq descent (max descent (aref char-infos (index+ 4 n)))))
		  ))))
	  )))))

;;-----------------------------------------------------------------------------

;; This controls the element size of the dst buffer given to translate.  If
;; :default is specified, the size will be based on the current font, if known,
;; and otherwise 16 will be used.  [An alternative would be to pass the buffer
;; size to translate, and allow it to return the desired size if it doesn't
;; like the current size.  The problem is that the protocol doesn't allow
;; switching within a single request, so to allow switching would require
;; knowing the width of text, which isn't necessarily known.  We could call
;; text-width to compute it, but perhaps that is doing too many favors?]  [An
;; additional possibility is to allow an index-size of :two-byte, in which case
;; translate would be given a double-length 8-bit array, and translate would be
;; expected to store first-byte/second-byte instead of 16-bit integers.]

(deftype index-size () '(member :default 8 16))

;; In the functions below, if width is specified, it is assumed to be the total
;; pixel width of whatever string of glyphs is actually drawn.  Specifying
;; width will allow for appending the output of subsequent calls to the same
;; protocol request, provided gcontext has not been modified in the interim.
;; If width is not specified, appending of subsequent output might not occur
;; (unless translate returns the width).  Specifying width is simply a hint,
;; for performance.

(defun draw-glyph (drawable gcontext x y elt
		   &key translate width (size :default))
  ;; Returns true if elt is output, nil if translate refuses to output it.
  ;; Second result is width, if known.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x y)
	   (type (or null int32) width)
	   (type index-size size))
  (declare (type (or null translation-function) translate)
	   #+clx-ansi-common-lisp
	   (dynamic-extent translate)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg #+Genera * #-Genera translate))
  (declare (clx-values generalized-boolean (or null int32)))
  (let* ((display (gcontext-display gcontext))
	 (result t)
	 (opcode +x-polytext8+))
    (declare (type display display))
    (let ((vector (allocate-gcontext-state)))
      (declare (type gcontext-state vector))
      (setf (aref vector 0) elt)
      (multiple-value-bind (new-start new-font translate-width)
	  (funcall (or translate #'translate-default)
		   vector 0 1 (gcontext-font gcontext nil) vector 1)
	;; Allow translate to set a new font
	(when (type? new-font 'font) 
	  (setf (gcontext-font gcontext) new-font)
	  (multiple-value-setq (new-start new-font translate-width)
	    (funcall translate vector 0 1 new-font vector 1)))
	;; If new-start is zero, translate refuses to output it
	(setq result (index-plusp new-start)
	      elt (aref vector 1))
	(deallocate-gcontext-state vector)
	(when translate-width (setq width translate-width))))
    (when result
      (when (eql size 16)
	(setq opcode +x-polytext16+)
	(setq elt (dpb elt (byte 8 8) (ldb (byte 8 8) elt))))
      (with-buffer-request (display opcode :gc-force gcontext)
	(drawable drawable)
	(gcontext gcontext)
	(int16 x y)
	(card8 1 0)
	(card8 (ldb (byte 8 0) elt))
	(card8 (ldb (byte 8 8) elt)))
      (values t width))))
  
(defun draw-glyphs (drawable gcontext x y sequence
		    &key (start 0) end translate width (size :default))
  ;; First result is new start, if end was not reached.  Second result is
  ;; overall width, if known.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x y)
	   (type array-index start)
	   (type sequence sequence)
	   (type (or null array-index) end)
	   (type (or null int32) width)
	   (type index-size size))
  (declare (type (or null translation-function) translate)
	   #+clx-ansi-common-lisp
	   (dynamic-extent translate)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg #+Genera * #-Genera translate))
  (declare (clx-values (or null array-index) (or null int32)))
  (unless end (setq end (length sequence)))
  (ecase size
    ((:default 8) (draw-glyphs8 drawable gcontext x y sequence start end
				(or translate #'translate-default) width))
    (16 (draw-glyphs16 drawable gcontext x y sequence start end
		       (or translate #'translate-default) width))))

(defun draw-glyphs8 (drawable gcontext x y sequence start end translate width)
  ;; First result is new start, if end was not reached.  Second result is
  ;; overall width, if known.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x y)
	   (type array-index start)
	   (type sequence sequence)
	   (type (or null array-index) end)
	   (type (or null int32) width))
  (declare (clx-values (or null array-index) (or null int32)))
  (declare (type translation-function translate)
	   #+clx-ansi-common-lisp
	   (dynamic-extent translate)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg translate)) 
  (let* ((src-start start)
	 (src-end (or end (length sequence)))
	 (next-start nil)
	 (length (index- src-end src-start))
	 (request-length (* length 2))		; Leave lots of room for font shifts.
	 (display (gcontext-display gcontext))
	 (font (gcontext-font gcontext nil)))
    (declare (type array-index src-start src-end length)
	     (type (or null array-index) next-start)
	     (type display display))
    (with-buffer-request (display +x-polytext8+ :gc-force gcontext :length request-length)
      (drawable drawable)
      (gcontext gcontext)
      (int16 x y)
      (progn
	;; Don't let any flushes happen since we manually set the request
	;; length when we're done.
	(with-buffer-flush-inhibited (display)
	  (do* ((boffset (index+ buffer-boffset 16))
		(src-chunk 0)
		(dst-chunk 0)
		(offset 0)
		(overall-width 0)
		(stop-p nil))
	       ((or stop-p (zerop length))
		;; Ensure terminated with zero bytes
		(do ((end (the array-index (lround boffset))))
		    ((index>= boffset end))
		  (setf (aref buffer-bbuf boffset) 0)
		  (index-incf boffset))
		(length-put 2 (index-ash (index- boffset buffer-boffset) -2))
		(setf (buffer-boffset display) boffset)
		(unless (index-zerop length) (setq next-start src-start))
		(when overall-width (setq width overall-width)))

	    (declare (type array-index src-chunk dst-chunk offset)
		     (type (or null int32) overall-width)
		     (type generalized-boolean stop-p))
	    (setq src-chunk (index-min length *max-string-size*))
	    (multiple-value-bind (new-start new-font translated-width)
		(funcall translate
			 sequence src-start (index+ src-start src-chunk)
			 font buffer-bbuf (index+ boffset 2))
	      (setq dst-chunk (index- new-start src-start)
		    length (index- length dst-chunk)
		    src-start new-start)
	      (if translated-width
		  (when overall-width (incf overall-width translated-width))
		(setq overall-width nil))
	      (when (index-plusp dst-chunk)
		(setf (aref buffer-bbuf boffset) dst-chunk)
		(setf (aref buffer-bbuf (index+ boffset 1)) offset)
		(incf boffset (index+ dst-chunk 2)))
	      (setq offset 0)
	      (cond ((null new-font)
		     ;; Don't stop if translate copied whole chunk
		     (unless (index= src-chunk dst-chunk)
		       (setq stop-p t)))
		    ((integerp new-font) (setq offset new-font))
		    ((type? new-font 'font)
		     (setq font new-font)
		     (let ((font-id (font-id font))
			   (buffer-boffset boffset))
		       (declare (type resource-id font-id)
				(type array-index buffer-boffset))
		       ;; This changes the gcontext font in the server
		       ;; Update the gcontext cache (both local and server state)
		       (let ((local-state (gcontext-local-state gcontext))
			     (server-state (gcontext-server-state gcontext)))
			 (declare (type gcontext-state local-state server-state))
			 (setf (gcontext-internal-font-obj server-state) font
			       (gcontext-internal-font server-state) font-id)
			 (without-interrupts
			   (setf (gcontext-internal-font-obj local-state) font
				 (gcontext-internal-font local-state) font-id)))
		       (card8-put 0 #xff)
		       (card8-put 1 (ldb (byte 8 24) font-id))
		       (card8-put 2 (ldb (byte 8 16) font-id))
		       (card8-put 3 (ldb (byte 8 8) font-id))
		       (card8-put 4 (ldb (byte 8 0) font-id)))
		     (index-incf boffset 5)))
	      )))))
    (values next-start width)))

;; NOTE: After the first font change by the TRANSLATE function, characters are no-longer
;;       on 16bit boundaries and this function garbles the bytes.
(defun draw-glyphs16 (drawable gcontext x y sequence start end translate width)
  ;; First result is new start, if end was not reached.  Second result is
  ;; overall width, if known.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x y)
	   (type array-index start)
	   (type sequence sequence)
	   (type (or null array-index) end)
	   (type (or null int32) width))
  (declare (clx-values (or null array-index) (or null int32)))
  (declare (type translation-function translate)
	   #+clx-ansi-common-lisp
	   (dynamic-extent translate)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg translate))
  (let* ((src-start start)
	 (src-end (or end (length sequence)))
	 (next-start nil)
	 (length (index- src-end src-start))
	 (request-length (* length 3))		; Leave lots of room for font shifts.
	 (display (gcontext-display gcontext))
	 (font (gcontext-font gcontext nil))
	 (buffer (display-tbuf16 display)))
    (declare (type array-index src-start src-end length)
	     (type (or null array-index) next-start)
	     (type display display)
	     (type buffer-text16 buffer))
    (with-buffer-request (display +x-polytext16+ :gc-force gcontext :length request-length)
      (drawable drawable)
      (gcontext gcontext)
      (int16 x y)
      (progn
	;; Don't let any flushes happen since we manually set the request
	;; length when we're done.
	(with-buffer-flush-inhibited (display)
	  (do* ((boffset (index+ buffer-boffset 16))
		(src-chunk 0)
		(dst-chunk 0)
		(offset 0)
		(overall-width 0)
		(stop-p nil))
	       ((or stop-p (zerop length))
		;; Ensure terminated with zero bytes
		(do ((end (lround boffset)))
		    ((index>= boffset end))
		  (setf (aref buffer-bbuf boffset) 0)
		  (index-incf boffset))
		(length-put 2 (index-ash (index- boffset buffer-boffset) -2))
		(setf (buffer-boffset display) boffset)
		(unless (zerop length) (setq next-start src-start))
		(when overall-width (setq width overall-width)))

	    (declare (type array-index boffset src-chunk dst-chunk offset)
		     (type (or null int32) overall-width)
		     (type generalized-boolean stop-p))
	    (setq src-chunk (index-min length *max-string-size*))
	    (multiple-value-bind (new-start new-font translated-width)
		(funcall translate
			 sequence src-start (index+ src-start src-chunk)
			 font buffer 0)
	      (setq dst-chunk (index- new-start src-start)
		    length (index- length dst-chunk)
		    src-start new-start)
	      (write-sequence-char2b display (index+ boffset 2) buffer 0 dst-chunk)
	      (if translated-width
		  (when overall-width (incf overall-width translated-width))
		(setq overall-width nil))
	      (when (index-plusp dst-chunk)
		(setf (aref buffer-bbuf boffset) dst-chunk)
		(setf (aref buffer-bbuf (index+ boffset 1)) offset)
		(index-incf boffset (index+ dst-chunk dst-chunk 2)))
	      (setq offset 0)
	      (cond ((null new-font)
		     ;; Don't stop if translate copied whole chunk
		     (unless (index= src-chunk dst-chunk) 
		       (setq stop-p t)))
		    ((integerp new-font) (setq offset new-font))
		    ((type? new-font 'font)
		     (setq font new-font)
		     (let ((font-id (font-id font))
			   (buffer-boffset boffset))
		       (declare (type resource-id font-id)
				(type array-index buffer-boffset))
		       ;; This changes the gcontext font in the SERVER
		       ;; Update the gcontext cache (both local and server state)
		       (let ((local-state (gcontext-local-state gcontext))
			     (server-state (gcontext-server-state gcontext)))
			 (declare (type gcontext-state local-state server-state))
			 (setf (gcontext-internal-font-obj server-state) font
			       (gcontext-internal-font server-state) font-id)
			 (without-interrupts
			   (setf (gcontext-internal-font-obj local-state) font
				 (gcontext-internal-font local-state) font-id)))
		       (card8-put 0 #xff)
		       (card8-put 1 (ldb (byte 8 24) font-id))
		       (card8-put 2 (ldb (byte 8 16) font-id))
		       (card8-put 3 (ldb (byte 8 8) font-id))
		       (card8-put 4 (ldb (byte 8 0) font-id)))
		     (index-incf boffset 5)))
	      )))))
    (values next-start width)))

(defun draw-image-glyph (drawable gcontext x y elt
			 &key translate width (size :default))
  ;; Returns true if elt is output, nil if translate refuses to output it.
  ;; Second result is overall width, if known.  An initial font change is
  ;; allowed from translate.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x y)
	   (type (or null int32) width)
	   (type index-size size))
  (declare (type (or null translation-function) translate)
	   #+clx-ansi-common-lisp
	   (dynamic-extent translate)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg #+Genera * #-Genera translate))
  (declare (clx-values generalized-boolean (or null int32)))
  (let* ((display (gcontext-display gcontext))
	 (result t)
	 (opcode +x-imagetext8+))
    (declare (type display display))
    (let ((vector (allocate-gcontext-state)))
      (declare (type gcontext-state vector))
      (setf (aref vector 0) elt)
      (multiple-value-bind (new-start new-font translate-width)
	  (funcall (or translate #'translate-default)
		   vector 0 1 (gcontext-font gcontext nil) vector 1)
	;; Allow translate to set a new font
	(when (type? new-font 'font) 
	  (setf (gcontext-font gcontext) new-font)
	  (multiple-value-setq (new-start new-font translate-width)
	    (funcall translate vector 0 1 new-font vector 1)))
	;; If new-start is zero, translate refuses to output it
	(setq result (index-plusp new-start)
	      elt (aref vector 1))
	(deallocate-gcontext-state vector)
	(when translate-width (setq width translate-width))))
    (when result
      (when (eql size 16)
	(setq opcode +x-imagetext16+)
	(setq elt (dpb elt (byte 8 8) (ldb (byte 8 8) elt))))
      (with-buffer-request (display opcode :gc-force gcontext)
	(drawable drawable)
	(gcontext gcontext)
	(data 1) ;; 1 character
	(int16 x y)
	(card8 (ldb (byte 8 0) elt))
	(card8 (ldb (byte 8 8) elt)))
      (values t width))))

(defun draw-image-glyphs (drawable gcontext x y sequence
			  &key (start 0) end translate width (size :default))
  ;; An initial font change is allowed from translate, but any subsequent font
  ;; change or horizontal motion will cause termination (because the protocol
  ;; doesn't support chaining).  [Alternatively, font changes could be accepted
  ;; as long as they are accompanied with a width return value, or always
  ;; accept font changes and call text-width as required.  However, horizontal
  ;; motion can't really be accepted, due to semantics.]  First result is new
  ;; start, if end was not reached.  Second result is overall width, if known.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x y)
	   (type array-index start)
	   (type (or null array-index) end)
	   (type sequence sequence)
	   (type (or null int32) width)
	   (type index-size size))
  (declare (type (or null translation-function) translate)
	   #+clx-ansi-common-lisp
	   (dynamic-extent translate)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg #+Genera * #-Genera translate))
  (declare (clx-values (or null array-index) (or null int32)))
  (setf end (index-min (index+ start 255) (or end (length sequence))))
  (ecase size
    ((:default 8)
     (draw-image-glyphs8 drawable gcontext x y sequence start end translate width))
    (16
     (draw-image-glyphs16 drawable gcontext x y sequence start end translate width))))

(defun draw-image-glyphs8 (drawable gcontext x y sequence start end translate width)
  ;; An initial font change is allowed from translate, but any subsequent font
  ;; change or horizontal motion will cause termination (because the protocol
  ;; doesn't support chaining).  [Alternatively, font changes could be accepted
  ;; as long as they are accompanied with a width return value, or always
  ;; accept font changes and call text-width as required.  However, horizontal
  ;; motion can't really be accepted, due to semantics.]  First result is new
  ;; start, if end was not reached.  Second result is overall width, if known.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x y)
	   (type array-index start)
	   (type sequence sequence)
	   (type (or null array-index) end)
	   (type (or null int32) width)) 
  (declare (type (or null translation-function) translate)
	   #+clx-ansi-common-lisp
	   (dynamic-extent translate)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg translate))
  (declare (clx-values (or null array-index) (or null int32)))
  (do* ((display (gcontext-display gcontext))
	(length (index- end start))
	(font (gcontext-font gcontext nil))
	(font-change nil)
	(new-start) (translated-width) (chunk))
       (nil) ;; forever
    (declare (type display display)
	     (type array-index length)
	     (type (or null array-index) new-start chunk))
    
    (when font-change
      (setf (gcontext-font gcontext) font))
    (block change-font
      (with-buffer-request (display +x-imagetext8+ :gc-force gcontext :length length)
	(drawable drawable)
	(gcontext gcontext)
	(int16 x y)
	(progn
	  ;; Don't let any flushes happen since we manually set the request
	  ;; length when we're done.
	  (with-buffer-flush-inhibited (display)
	    ;; Translate the sequence into the buffer
	    (multiple-value-setq (new-start font translated-width)
	      (funcall (or translate #'translate-default) sequence start end
		       font buffer-bbuf (index+ buffer-boffset 16)))
	    ;; Number of glyphs translated
	    (setq chunk (index- new-start start))		
	    ;; Check for initial font change
	    (when (and (index-zerop chunk) (type? font 'font))
	      (setq font-change t) ;; Loop around changing font
	      (return-from change-font))
	    ;; Quit when nothing translated
	    (when (index-zerop chunk)
	      (return-from draw-image-glyphs8 new-start))
	    ;; Update buffer pointers
	    (data-put 1 chunk)
	    (let ((blen (lround (index+ 16 chunk))))
	      (length-put 2 (index-ash blen -2))
	      (setf (buffer-boffset display) (index+ buffer-boffset blen))))))
      ;; Normal exit
      (return-from draw-image-glyphs8
	(values (if (index= chunk length) nil new-start)
		(or translated-width width))))))

(defun draw-image-glyphs16 (drawable gcontext x y sequence start end translate width)
  ;; An initial font change is allowed from translate, but any subsequent font
  ;; change or horizontal motion will cause termination (because the protocol
  ;; doesn't support chaining).  [Alternatively, font changes could be accepted
  ;; as long as they are accompanied with a width return value, or always
  ;; accept font changes and call text-width as required.  However, horizontal
  ;; motion can't really be accepted, due to semantics.]  First result is new
  ;; start, if end was not reached.  Second result is overall width, if known.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x y)
	   (type array-index start)
	   (type sequence sequence)
	   (type (or null array-index) end)
	   (type (or null int32) width))
  (declare (type (or null translation-function) translate)
	   #+clx-ansi-common-lisp
	   (dynamic-extent translate)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg translate))
  (declare (clx-values (or null array-index) (or null int32)))
  (do* ((display (gcontext-display gcontext))
	(length (index- end start))
	(font (gcontext-font gcontext nil)) 
	(font-change nil)
	(new-start) (translated-width) (chunk)
	(buffer (buffer-tbuf16 display)))
       (nil) ;; forever
    
    (declare (type display display)
	     (type array-index length)
	     (type (or null array-index) new-start chunk)
	     (type buffer-text16 buffer))
    (when font-change
      (setf (gcontext-font gcontext) font))

    (block change-font
      (with-buffer-request (display +x-imagetext16+ :gc-force gcontext :length length)
	(drawable drawable)
	(gcontext gcontext)
	(int16 x y)
	(progn
	  ;; Don't let any flushes happen since we manually set the request
	  ;; length when we're done.
	  (with-buffer-flush-inhibited (display)
	    ;; Translate the sequence into the buffer
	    (multiple-value-setq (new-start font translated-width)
	      (funcall (or translate #'translate-default) sequence start end
		       font buffer 0))
	    ;; Number of glyphs translated
	    (setq chunk (index- new-start start))
	    ;; Check for initial font change
	    (when (and (index-zerop chunk) (type? font 'font))
	      (setq font-change t) ;; Loop around changing font
	      (return-from change-font))
	    ;; Quit when nothing translated
	    (when (index-zerop chunk)
	      (return-from draw-image-glyphs16 new-start))
	    (write-sequence-char2b display (index+ buffer-boffset 16) buffer 0 chunk)
	    ;; Update buffer pointers
	    (data-put 1 chunk)
	    (let ((blen (lround (index+ 16 (index-ash chunk 1)))))
	      (length-put 2 (index-ash blen -2))
	      (setf (buffer-boffset display) (index+ buffer-boffset blen))))))
      ;; Normal exit
      (return-from draw-image-glyphs16
	(values (if (index= chunk length) nil new-start)
		(or translated-width width))))))


;;-----------------------------------------------------------------------------

(defun display-keycode-range (display)
  (declare (type display display))
  (declare (clx-values min max))
  (values (display-min-keycode display)
	  (display-max-keycode display)))

;; Should this signal device-busy like the pointer-mapping setf, and return a
;; generalized-boolean instead (true for success)?  Alternatively, should the
;; pointer-mapping setf be changed to set-pointer-mapping with a (member
;; :success :busy) result?

(defun set-modifier-mapping (display &key shift lock control mod1 mod2 mod3 mod4 mod5)
  ;; Setf ought to allow multiple values.
  (declare (type display display)
	   (type sequence shift lock control mod1 mod2 mod3 mod4 mod5))
  (declare (clx-values (member :success :busy :failed)))
  (let* ((keycodes-per-modifier (index-max (length shift)
					   (length lock)
					   (length control)
					   (length mod1)
					   (length mod2)
					   (length mod3)
					   (length mod4)
					   (length mod5)))
	 (data (make-array (index* 8 keycodes-per-modifier)
			   :element-type 'card8
			   :initial-element 0)))
    (replace data shift)
    (replace data lock :start1 keycodes-per-modifier)
    (replace data control :start1 (index* 2 keycodes-per-modifier))
    (replace data mod1 :start1 (index* 3 keycodes-per-modifier))
    (replace data mod2 :start1 (index* 4 keycodes-per-modifier))
    (replace data mod3 :start1 (index* 5 keycodes-per-modifier))
    (replace data mod4 :start1 (index* 6 keycodes-per-modifier))
    (replace data mod5 :start1 (index* 7 keycodes-per-modifier))
    (with-buffer-request-and-reply (display +x-setmodifiermapping+ 4 :sizes 8)
	 ((data keycodes-per-modifier)
	  ((sequence :format card8) data))
      (values (member8-get 1 :success :busy :failed)))))

(defun modifier-mapping (display)
  ;; each value is a list of integers
  (declare (type display display))
  (declare (clx-values shift lock control mod1 mod2 mod3 mod4 mod5))
  (let ((lists nil))
    (with-buffer-request-and-reply (display +x-getmodifiermapping+ nil :sizes 8)
	 ()
      (do* ((keycodes-per-modifier (card8-get 1))
	    (advance-by +replysize+ keycodes-per-modifier)
	    (keys nil nil)
	    (i 0 (index+ i 1)))
	   ((index= i 8))
	(advance-buffer-offset advance-by)
	(dotimes (j keycodes-per-modifier)
	  (let ((key (read-card8 j)))
	    (unless (zerop key)
	      (push key keys))))
	(push (nreverse keys) lists)))
    (values-list (nreverse lists))))

;; Either we will want lots of defconstants for well-known values, or perhaps
;; an integer-to-keyword translation function for well-known values.

(defun change-keyboard-mapping
       (display keysyms &key (start 0) end (first-keycode start))
  ;; start/end give subrange of keysyms
  ;; first-keycode is the first-keycode to store at
  (declare (type display display)
	   (type array-index start)
	   (type card8 first-keycode)
	   (type (or null array-index) end)
	   (type (array * (* *)) keysyms))
  (let* ((keycode-end (or end (array-dimension keysyms 0)))
	 (keysyms-per-keycode (array-dimension keysyms 1))
	 (length (index- keycode-end start))
	 (size (index* length keysyms-per-keycode))
	 (request-length (index+ size 2)))
    (declare (type array-index keycode-end keysyms-per-keycode length request-length))
    (with-buffer-request (display +x-setkeyboardmapping+
				  :length (index-ash request-length 2)
				  :sizes (32))
      (data length)
      (length request-length)
      (card8 first-keycode keysyms-per-keycode)
      (progn
	(do ((limit (index-ash (buffer-size display) -2))
	     (w (index+ 2 (index-ash buffer-boffset -2)))
	     (i start (index+ i 1)))
	    ((index>= i keycode-end)
	     (setf (buffer-boffset display) (index-ash w 2)))
	  (declare (type array-index limit w i))
	  (when (index> w limit)
	    (buffer-flush display)
	    (setq w (index-ash (buffer-boffset display) -2)))
	  (do ((j 0 (index+ j 1)))
	      ((index>= j keysyms-per-keycode))
	    (declare (type array-index j))
	    (card29-put (index* w 4) (aref keysyms i j))
	    (index-incf w))))))) 

(defun keyboard-mapping (display &key first-keycode start end data)
  ;; First-keycode specifies which keycode to start at (defaults to min-keycode).
  ;; Start specifies where (in result) to put first-keycode. (defaults to first-keycode)
  ;; (- end start) is the number of keycodes to get. (End defaults to (1+ max-keycode)).
  ;; If DATA is specified, the results are put there.
  (declare (type display display)
	   (type (or null card8) first-keycode)
	   (type (or null array-index) start end)
	   (type (or null (array * (* *))) data))
  (declare (clx-values (array * (* *))))
  (unless first-keycode (setq first-keycode (display-min-keycode display)))
  (unless start (setq start first-keycode))
  (unless end (setq end (1+ (display-max-keycode display))))
  (with-buffer-request-and-reply (display +x-getkeyboardmapping+ nil :sizes (8 32))
       ((card8 first-keycode (index- end start)))
    (do* ((keysyms-per-keycode (card8-get 1))
	  (bytes-per-keycode (* keysyms-per-keycode 4))
	  (advance-by +replysize+ bytes-per-keycode)
	  (keycode-count (floor (card32-get 4) keysyms-per-keycode)
			 (index- keycode-count 1))
	  (result (if (and (arrayp data)
			   (= (array-rank data) 2)
			   (>= (array-dimension data 0) (index+ start keycode-count))
			   (>= (array-dimension data 1) keysyms-per-keycode))
		      data
		    (make-array `(,(index+ start keycode-count) ,keysyms-per-keycode)
				:element-type 'keysym :initial-element 0)))
	  (i start (1+ i)))
	 ((zerop keycode-count) (setq data result))
      (advance-buffer-offset advance-by)
      (dotimes (j keysyms-per-keycode)
	(setf (aref result i j) (card29-get (* j 4))))))
  data)
