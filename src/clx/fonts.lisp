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

(in-package :xlib)

;; The char-info stuff is here instead of CLX because of uses of int16->card16.

; To allow efficient storage representations, the type char-info is not
; required to be a structure.

;; For each of left-bearing, right-bearing, width, ascent, descent, attributes:

;(defun char-<metric> (font index)
;  ;; Note: I have tentatively chosen to return nil for an out-of-bounds index
;  ;; (or an in-bounds index on a pseudo font), although returning zero or
;  ;; signalling might be better.
;  (declare (type font font)
;	   (type integer index)
;	   (clx-values (or null integer))))

;(defun max-char-<metric> (font)
;  ;; Note: I have tentatively chosen separate accessors over allowing :min and
;  ;; :max as an index above.
;  (declare (type font font)
;	   (clx-values integer)))

;(defun min-char-<metric> (font)
;  (declare (type font font)
;	   (clx-values integer)))

;; Note: char16-<metric> accessors could be defined to accept two-byte indexes.

(deftype char-info-vec () '(simple-array int16 (*)))

(macrolet ((def-char-info-accessors (useless-name &body fields)
	    `(within-definition (,useless-name def-char-info-accessors)
	       ,@(do ((field fields (cdr field))
		      (n 0 (1+ n))
		      (name) (type)
		      (result nil))
		     ((endp field) result)
		   (setq name (xintern 'char- (caar field)))
		   (setq type (cadar field))
		   (flet ((from (form)
			    (if (eq type 'int16)
				form
				`(,(xintern 'int16-> type) ,form))))
		     (push
		       `(defun ,name (font index)
			  (declare (type font font)
				   (type array-index index))
			  (declare (clx-values (or null ,type)))
			  (when (and (font-name font)
				     (index>= (font-max-char font) index (font-min-char font)))
			    (the ,type
				 ,(from
				    `(the int16
					  (let ((char-info-vector (font-char-infos font)))
					    (declare (type char-info-vec char-info-vector))
					    (if (index-zerop (length char-info-vector))
						;; Fixed width font
						(aref (the char-info-vec
							   (font-max-bounds font))
						      ,n)
						;; Variable width font
						(aref char-info-vector
						      (index+
							(index*
							  6
							  (index-
							    index
							    (font-min-char font)))
							,n)))))))))
		       result)
		     (setq name (xintern 'min-char- (caar field)))
		     (push
		       `(defun ,name (font)
			  (declare (type font font))
			  (declare (clx-values (or null ,type)))
			  (when (font-name font)
			    (the ,type
				 ,(from
				    `(the int16
					  (aref (the char-info-vec (font-min-bounds font))
						,n))))))
		       result)
		     (setq name (xintern 'max-char- (caar field)))
		     (push
		       `(defun ,name (font)
			  (declare (type font font))
			  (declare (clx-values (or null ,type)))
			  (when (font-name font)
			    (the ,type
				 ,(from
				    `(the int16
					  (aref (the char-info-vec (font-max-bounds font))
						,n))))))
		       result)))
	  
	       (defun make-char-info
		      (&key ,@(mapcar
				#'(lambda (field)
				    `(,(car field) (required-arg ,(car field))))
				fields))
		 (declare ,@(mapcar #'(lambda (field) `(type ,@(reverse field))) fields))
		 (let ((result (make-array ,(length fields) :element-type 'int16)))
		   (declare (type char-info-vec result))
		   ,@(do* ((field fields (cdr field))
			   (var (caar field) (caar field))
			   (type (cadar field) (cadar field))
			   (n 0 (1+ n))
			   (result nil))
			  ((endp field) (nreverse result))
		       (push `(setf (aref result ,n)
				    ,(if (eq type 'int16)
					 var
					 `(,(xintern type '->int16) ,var)))
			     result))
		   result)))))
  (def-char-info-accessors ignore
    (left-bearing int16)
    (right-bearing int16)
    (width int16)
    (ascent int16)
    (descent int16)
    (attributes card16)))
    
(defun open-font (display name)
  ;; Font objects may be cached and reference counted locally within the display
  ;; object.  This function might not execute a with-display if the font is cached.
  ;; The protocol QueryFont request happens on-demand under the covers.
  (declare (type display display)
	   (type stringable name))
  (declare (clx-values font))
  (let* ((name-string (string-downcase (string name)))
	 (font (car (member name-string (display-font-cache display)
			    :key 'font-name
			    :test 'equal)))
	 font-id)
    (unless font
      (setq font (make-font :display display :name name-string))
      (setq font-id (allocate-resource-id display font 'font))
      (setf (font-id-internal font) font-id)
      (with-buffer-request (display +x-openfont+)
	(resource-id font-id)
	(card16 (length name-string))
	(pad16 nil)
	(string name-string))
      (push font (display-font-cache display)))
    (incf (font-reference-count font))
    (unless (font-font-info-internal font)
      (query-font font))
    font))

(defun open-font-internal (font)
  ;; Called "under the covers" to open a font object
  (declare (type font font))
  (declare (clx-values resource-id))
  (let* ((name-string (font-name font))
	 (display (font-display font))
	 (id (allocate-resource-id display font 'font)))
    (setf (font-id-internal font) id)
    (with-buffer-request (display +x-openfont+)
      (resource-id id)
      (card16 (length name-string))
      (pad16 nil)
      (string name-string))
    (push font (display-font-cache display))
    (incf (font-reference-count font))
    id))

(defun discard-font-info (font)
  ;; Discards any state that can be re-obtained with QueryFont.  This is
  ;; simply a performance hint for memory-limited systems.
  (declare (type font font))
  (setf (font-font-info-internal font) nil
	(font-char-infos-internal font) nil))

(defun query-font (font)
  ;; Internal function called by font and char info accessors
  (declare (type font font))
  (declare (clx-values font-info))
  (let ((display (font-display font))
	font-id
	font-info
	props)
    (setq font-id (font-id font)) ;; May issue an open-font request
    (with-buffer-request-and-reply (display +x-queryfont+ 60)
	 ((resource-id font-id))
      (let* ((min-byte2 (card16-get 40))
	     (max-byte2 (card16-get 42))
	     (min-byte1 (card8-get 49))
	     (max-byte1 (card8-get 50))
	     (min-char  min-byte2)
	     (max-char  (index+ (index-ash max-byte1 8) max-byte2))
	     (nfont-props (card16-get 46))
	     (nchar-infos (index* (card32-get 56) 6))
	     (char-info (make-array nchar-infos :element-type 'int16)))
	(setq font-info
	      (make-font-info
		:direction (member8-get 48 :left-to-right :right-to-left)
		:min-char min-char
		:max-char max-char
		:min-byte1 min-byte1
		:max-byte1 max-byte1
		:min-byte2 min-byte2
		:max-byte2 max-byte2
		:all-chars-exist-p (boolean-get 51)
		:default-char (card16-get 44)
		:ascent (int16-get 52)
		:descent (int16-get 54)
		:min-bounds (char-info-get 8)
		:max-bounds (char-info-get 24)))
	(setq props (sequence-get :length (index* 2 nfont-props) :format int32
				  :result-type 'list :index 60))
	(sequence-get :length nchar-infos :format int16 :data char-info
		      :index (index+ 60 (index* 2 nfont-props 4)))
	(setf (font-char-infos-internal font) char-info)
	(setf (font-font-info-internal font) font-info)))
    ;; Replace atom id's with keywords in the plist
    (do ((p props (cddr p)))
	((endp p))
      (setf (car p) (atom-name display (car p))))
    (setf (font-info-properties font-info) props)
    font-info))

(defun close-font (font)
  ;; This might not generate a protocol request if the font is reference
  ;; counted locally.
  (declare (type font font))
  (when (and (not (plusp (decf (font-reference-count font))))
	     (font-id-internal font))
    (let ((display (font-display font))
	  (id (font-id-internal font)))
      (declare (type display display))
      ;; Remove font from cache
      (setf (display-font-cache display) (delete font (display-font-cache display)))
      ;; Close the font
      (with-buffer-request (display +x-closefont+)
	(resource-id id)))))

(defun list-font-names (display pattern &key (max-fonts 65535) (result-type 'list))
  (declare (type display display)
	   (type string pattern)
	   (type card16 max-fonts)
	   (type t result-type)) ;; CL type
  (declare (clx-values (clx-sequence string)))
  (let ((string (string pattern)))
    (with-buffer-request-and-reply (display +x-listfonts+ size :sizes (8 16))
	 ((card16 max-fonts (length string))
	  (string string))
      (values
	(read-sequence-string
	  buffer-bbuf (index- size +replysize+) (card16-get 8) result-type +replysize+)))))

(defun list-fonts (display pattern &key (max-fonts 65535) (result-type 'list))
  ;; Note: Was called list-fonts-with-info.
  ;; Returns "pseudo" fonts that contain basic font metrics and properties, but
  ;; no per-character metrics and no resource-ids.  These pseudo fonts will be
  ;; converted (internally) to real fonts dynamically as needed, by issuing an
  ;; OpenFont request.  However, the OpenFont might fail, in which case the
  ;; invalid-font error can arise.
  (declare (type display display)
	   (type string pattern)
	   (type card16 max-fonts)
	   (type t result-type)) ;; CL type
  (declare (clx-values (clx-sequence font)))
  (let ((string (string pattern))
	(result nil))
    (with-buffer-request-and-reply (display +x-listfontswithinfo+ 60
					    :sizes (8 16) :multiple-reply t)
	 ((card16 max-fonts (length string))
	  (string string))
      (cond ((zerop (card8-get 1)) t)
	    (t
	(let* ((name-len (card8-get 1))
	       (min-byte2 (card16-get 40))
	       (max-byte2 (card16-get 42))
	       (min-byte1 (card8-get 49))
	       (max-byte1 (card8-get 50))
	       (min-char  min-byte2)
	       (max-char  (index+ (index-ash max-byte1 8) max-byte2))
	       (nfont-props (card16-get 46))
	       (font
		 (make-font
		   :display display
		   :name nil
		   :font-info-internal
		   (make-font-info
		     :direction (member8-get 48 :left-to-right :right-to-left)
		     :min-char min-char
		     :max-char max-char
		     :min-byte1 min-byte1
		     :max-byte1 max-byte1
		     :min-byte2 min-byte2
		     :max-byte2 max-byte2
		     :all-chars-exist-p (boolean-get 51)
		     :default-char (card16-get 44)
		     :ascent (int16-get 52)
		     :descent (int16-get 54)
		     :min-bounds (char-info-get 8)
		     :max-bounds (char-info-get 24)
		     :properties (sequence-get :length (index* 2 nfont-props)
					       :format int32
					       :result-type 'list
					       :index 60)))))
	  (setf (font-name font) (string-get name-len (index+ 60 (index* 2 nfont-props 4))))
	  (push font result))
	nil)))
    ;; Replace atom id's with keywords in the plist
    (dolist (font result)
      (do ((p (font-properties font) (cddr p)))
	  ((endp p))
	(setf (car p) (atom-name display (car p)))))
    (coerce (nreverse result) result-type)))

(defun font-path (display &key (result-type 'list))
  (declare (type display display)
	   (type t result-type)) ;; CL type
  (declare (clx-values (clx-sequence (or string pathname))))
  (with-buffer-request-and-reply (display +x-getfontpath+ size :sizes (8 16))
       ()
    (values
      (read-sequence-string
	buffer-bbuf (index- size +replysize+) (card16-get 8) result-type +replysize+))))

(defun set-font-path (display paths)
  (declare (type display display)
	   (type (clx-sequence (or string pathname)) paths))
  (let ((path-length (length paths))
	(request-length 8))
    ;; Find the request length
    (dotimes (i path-length)
      (let* ((string (string (elt paths i)))
	     (len (length string)))
	(incf request-length (1+ len))))
    (with-buffer-request (display +x-setfontpath+ :length request-length)
      (length (ceiling request-length 4))
      (card16 path-length)
      (pad16 nil)
      (progn
	(incf buffer-boffset 8)
	(dotimes (i path-length)
	  (let* ((string (string (elt paths i)))
		 (len (length string)))
	    (card8-put 0 len)
	    (string-put 1 string :appending t :header-length 1)
	    (incf buffer-boffset (1+ len))))
	(setf (buffer-boffset display) (lround buffer-boffset)))))
  paths)

(defsetf font-path set-font-path)
