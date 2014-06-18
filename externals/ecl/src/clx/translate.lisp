;;; -*- Mode:Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Lowercase:YES -*-

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

(defvar *keysym-sets* nil) ;; Alist of (name first-keysym last-keysym)

(defun define-keysym-set (set first-keysym last-keysym)
  ;; Define all keysyms from first-keysym up to and including
  ;; last-keysym to be in SET (returned from the keysym-set function).
  ;; Signals an error if the keysym range overlaps an existing set.
 (declare (type keyword set)
	  (type keysym first-keysym last-keysym))
  (when (> first-keysym last-keysym)
    (rotatef first-keysym last-keysym))
  (setq *keysym-sets* (delete set *keysym-sets* :key #'car))
  (dolist (set *keysym-sets*)
    (let ((first (second set))
	  (last (third set)))
      (when (or (<= first first-keysym last)
		(<= first last-keysym last))
	(error "Keysym range overlaps existing set ~s" set))))
  (push (list set first-keysym last-keysym) *keysym-sets*)
  set)

(defun keysym-set (keysym)
  ;; Return the character code set name of keysym
  (declare (type keysym keysym)
	   (clx-values keyword))
  (dolist (set *keysym-sets*)
    (let ((first (second set))
	  (last (third set)))
      (when (<= first keysym last)
	(return (first set))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro keysym (keysym &rest bytes)
    ;; Build a keysym.
    ;;
    ;; If KEYSYM is an integer, it is used as the most significant
    ;; bits of the keysym, and BYTES are used to specify low order
    ;; bytes. The last parameter is always byte4 of the keysym.  If
    ;; KEYSYM is not an integer, the keysym associated with KEYSYM is
    ;; returned.
    ;;
    ;; This is a macro and not a function macro to promote
    ;; compile-time lookup. All arguments are evaluated.
    ;;
    ;; FIXME: The above means that this shouldn't really be a macro at
    ;; all, but a compiler macro.  Probably, anyway.
    (declare (type t keysym)
	     (type list bytes)
	     (clx-values keysym))
    (typecase keysym
      ((integer 0 *)
       (dolist (b bytes keysym) (setq keysym (+ (ash keysym 8) b))))
      (otherwise
       (or (car (character->keysyms keysym))
	   (error "~s Isn't the name of a keysym" keysym))))))

(defvar *keysym->character-map*
	(make-hash-table :test (keysym->character-map-test) :size 400))

;; Keysym-mappings are a list of the form (object translate lowercase modifiers mask)
;; With the following accessor macros. Everything after OBJECT is optional.

(defmacro keysym-mapping-object (keysym-mapping)
  ;; Parameter to translate
  `(first ,keysym-mapping))

(defmacro keysym-mapping-translate (keysym-mapping)
  ;; Function to be called with parameters (display state OBJECT)
  ;; when translating KEYSYM and modifiers and mask are satisfied.
  `(second ,keysym-mapping))

(defmacro keysym-mapping-lowercase (keysym-mapping)
  ;; LOWERCASE is used for uppercase alphabetic keysyms.  The value
  ;; is the associated lowercase keysym.
  `(third ,keysym-mapping))

(defmacro keysym-mapping-modifiers (keysym-mapping)
  ;; MODIFIERS is either a modifier-mask or list containing intermixed
  ;; keysyms and state-mask-keys specifying when to use this
  ;; keysym-translation.
  `(fourth ,keysym-mapping))

(defmacro keysym-mapping-mask (keysym-mapping)
  ;; MASK is either a modifier-mask or list containing intermixed
  ;; keysyms and state-mask-keys specifying which modifiers to look at
  ;; (i.e.  modifiers not specified are don't-cares)
  `(fifth ,keysym-mapping))

(defvar *default-keysym-translate-mask*
	(the (or (member :modifiers) mask16 (clx-list (or keysym state-mask-key)))
	     (logand #xff (lognot (make-state-mask :lock))))
  "Default keysym state mask to use during keysym-translation.")

(defun define-keysym (object keysym &key lowercase translate modifiers mask display)	              
  ;; Define the translation from keysym/modifiers to a (usually
  ;; character) object.  ANy previous keysym definition with
  ;; KEYSYM and MODIFIERS is deleted before adding the new definition.
  ;;
  ;; MODIFIERS is either a modifier-mask or list containing intermixed
  ;; keysyms and state-mask-keys specifying when to use this
  ;; keysym-translation.  The default is NIL.
  ;;
  ;; MASK is either a modifier-mask or list containing intermixed
  ;; keysyms and state-mask-keys specifying which modifiers to look at
  ;; (i.e.  modifiers not specified are don't-cares).
  ;; If mask is :MODIFIERS then the mask is the same as the modifiers
  ;; (i.e.  modifiers not specified by modifiers are don't cares)
  ;; The default mask is *default-keysym-translate-mask*
  ;;
  ;; If DISPLAY is specified, the translation will be local to DISPLAY,
  ;; otherwise it will be the default translation for all displays.
  ;;
  ;; LOWERCASE is used for uppercase alphabetic keysyms.  The value
  ;; is the associated lowercase keysym.  This information is used
  ;; by the keysym-both-case-p predicate (for caps-lock computations)
  ;; and by the keysym-downcase function.
  ;;
  ;; TRANSLATE will be called with parameters (display state OBJECT)
  ;; when translating KEYSYM and modifiers and mask are satisfied.
  ;; [e.g (zerop (logxor (logand state (or mask *default-keysym-translate-mask*))
  ;;                     (or modifiers 0)))
  ;;      when mask and modifiers aren't lists of keysyms]
  ;; The default is #'default-keysym-translate
  ;;
  (declare (type (or base-char t) object)
	   (type keysym keysym)
	   (type (or null mask16 (clx-list (or keysym state-mask-key)))
	         modifiers)
	   (type (or null (member :modifiers) mask16 (clx-list (or keysym state-mask-key)))
	         mask)
	   (type (or null display) display)
           (type (or null keysym) lowercase)
	   (type (or null (function (display card16 t) t)) translate))
  (flet ((merge-keysym-mappings (new old)
	   ;; Merge new keysym-mapping with list of old mappings.
	   ;; Ensure that the mapping with no modifiers or mask comes first.
	   (let* ((key (keysym-mapping-modifiers new))
		  (merge (delete key old :key #'cadddr :test #'equal)))
	     (if key
		 (nconc merge (list new))
	       (cons new merge))))
	 (mask-check (mask)
	   (unless (or (numberp mask)
		       (dolist (element mask t)
			 (unless (or (find element +state-mask-vector+)
				     (gethash element *keysym->character-map*))
			   (return nil))))
	     (x-type-error mask '(or mask16 (clx-list (or modifier-key modifier-keysym)))))))
    (let ((entry
	    ;; Create with a single LIST call, to ensure cdr-coding
	    (cond
	      (mask
	       (unless (eq mask :modifiers)
		 (mask-check mask))
	       (when (or (null modifiers) (and (numberp modifiers) (zerop modifiers)))
		 (error "Mask with no modifiers"))
	       (list object translate lowercase modifiers mask))
	      (modifiers (mask-check modifiers)
			 (list object translate lowercase modifiers))
	      (lowercase	(list object translate lowercase))
	      (translate	(list object translate))
	      (t	(list object)))))
      (if display
	  (let ((previous (assoc keysym (display-keysym-translation display))))
	    (if previous
		(setf (cdr previous) (merge-keysym-mappings entry (cdr previous)))
	      (push (list keysym entry) (display-keysym-translation display))))
	(setf (gethash keysym *keysym->character-map*)
	      (merge-keysym-mappings entry (gethash keysym *keysym->character-map*)))))
    object))

(defun undefine-keysym (object keysym &key display modifiers &allow-other-keys)	              
  ;; Undefine the keysym-translation translating KEYSYM to OBJECT with MODIFIERS.
  ;; If DISPLAY is non-nil, undefine the translation for DISPLAY if it exists.
  (declare (type (or base-char t) object)
	   (type keysym keysym)
	   (type (or null mask16 (clx-list (or keysym state-mask-key)))
	         modifiers)
	   (type (or null display) display))
  (flet ((match (key entry)
	   (let ((object (car key))
		 (modifiers (cdr key)))
	     (or (eql object (keysym-mapping-object entry))
		 (equal modifiers (keysym-mapping-modifiers entry))))))
    (let* (entry
	   (previous (if display
			 (cdr (setq entry (assoc keysym (display-keysym-translation display))))
		       (gethash keysym *keysym->character-map*)))
	   (key (cons object modifiers)))
      (when (and previous (find key previous :test #'match))
	(setq previous (delete key previous :test #'match))
	(if display
	    (setf (cdr entry) previous)
	  (setf (gethash keysym *keysym->character-map*) previous))))))

(defun keysym-downcase (keysym)
  ;; If keysym has a lower-case equivalent, return it, otherwise return keysym.
  (declare (type keysym keysym))
  (declare (clx-values keysym))
  (let ((translations (gethash keysym *keysym->character-map*)))
    (or (and translations (keysym-mapping-lowercase (first translations))) keysym)))

(defun keysym-uppercase-alphabetic-p (keysym)
  ;; Returns T if keysym is uppercase-alphabetic.
  ;; I.E. If it has a lowercase equivalent.
  (declare (type keysym keysym))
  (declare (clx-values (or null keysym)))
  (let ((translations (gethash keysym *keysym->character-map*)))
    (and translations
	 (keysym-mapping-lowercase (first translations)))))

(defun character->keysyms (character &optional display)
  ;; Given a character, return a list of all matching keysyms.
  ;; If DISPLAY is given, translations specific to DISPLAY are used,
  ;; otherwise only global translations are used.
  ;; Implementation dependent function.
  ;; May be slow [i.e. do a linear search over all known keysyms]
  (declare (type t character)
	   (type (or null display) display)
	   (clx-values (clx-list keysym)))
  (let ((result nil))
    (when display
      (dolist (mapping (display-keysym-translation display))
	(when (eql character (second mapping))
	  (push (first mapping) result))))
    (maphash #'(lambda (keysym mappings)
		 (dolist (mapping mappings)
		   (when (eql (keysym-mapping-object mapping) character)
		     (pushnew keysym result))))
	     *keysym->character-map*)
    result))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant character-set-switch-keysym (keysym 255 126))
  (defconstant left-shift-keysym (keysym 255 225))
  (defconstant right-shift-keysym (keysym 255 226))
  (defconstant left-control-keysym (keysym 255 227))
  (defconstant right-control-keysym (keysym 255 228))
  (defconstant caps-lock-keysym (keysym 255 229))
  (defconstant shift-lock-keysym (keysym 255 230))
  (defconstant left-meta-keysym (keysym 255 231))
  (defconstant right-meta-keysym (keysym 255 232))
  (defconstant left-alt-keysym (keysym 255 233))
  (defconstant right-alt-keysym (keysym 255 234))
  (defconstant left-super-keysym (keysym 255 235))
  (defconstant right-super-keysym (keysym 255 236))
  (defconstant left-hyper-keysym (keysym 255 237))
  (defconstant right-hyper-keysym (keysym 255 238)))


;;-----------------------------------------------------------------------------
;; Keysym mapping functions

(defun display-keyboard-mapping (display)
  (declare (type display display))
  (declare (clx-values (simple-array keysym (display-max-keycode keysyms-per-keycode))))
  (or (display-keysym-mapping display)
      (setf (display-keysym-mapping display) (keyboard-mapping display))))

(defun keycode->keysym (display keycode keysym-index)
  (declare (type display display)
	   (type card8 keycode)
	   (type card8 keysym-index)
	   (clx-values keysym))
  (let* ((mapping (display-keyboard-mapping display))
	 (keysym (aref mapping keycode keysym-index)))
    (declare (type (simple-array keysym (* *)) mapping)
	     (type keysym keysym))
    ;; The keysym-mapping is brain dammaged.
    ;; Mappings for both-case alphabetic characters have the
    ;; entry for keysym-index zero set to the uppercase keysym
    ;; (this is normally where the lowercase keysym goes), and the
    ;; entry for keysym-index one is zero.
    (cond ((zerop keysym-index)			; Lowercase alphabetic keysyms
	   (keysym-downcase keysym))
	  ((and (zerop keysym) (plusp keysym-index)) ; Get the uppercase keysym
	   (aref mapping keycode 0))
	  (t keysym))))

(defun keysym->character (display keysym &optional (state 0))
  ;; Find the character associated with a keysym.
  ;; STATE can be used to set character attributes.
  ;; Implementation dependent function.
  (declare (type display display)
	   (type keysym keysym)
	   (type card16 state))
  (declare (clx-values (or null character)))
  (let* ((display-mappings (cdr (assoc keysym (display-keysym-translation display))))
	 (mapping (or ;; Find the matching display mapping
		      (dolist (mapping display-mappings)
			(when (mapping-matches-p display state mapping)
			  (return mapping)))
		      ;; Find the matching static mapping
		      (dolist (mapping (gethash keysym *keysym->character-map*))
			(when (mapping-matches-p display state mapping)
			  (return mapping))))))
    (when mapping
      (funcall (or (keysym-mapping-translate mapping) 'default-keysym-translate)
	       display state (keysym-mapping-object mapping)))))

(defun mapping-matches-p (display state mapping)
  ;; Returns T when the modifiers and mask in MAPPING satisfies STATE for DISPLAY
  (declare (type display display)
	   (type mask16 state)
	   (type list mapping))
  (declare (clx-values generalized-boolean))
  (flet
    ((modifiers->mask (display-mapping modifiers errorp &aux (mask 0))
       ;; Convert MODIFIERS, which is a modifier mask, or a list of state-mask-keys into a mask.
       ;; If ERRORP is non-nil, return NIL when an unknown modifier is specified,
       ;; otherwise ignore unknown modifiers.
       (declare (type list display-mapping)	; Alist of (keysym . mask)
		(type (or mask16 list) modifiers)
		(type mask16 mask))
       (declare (clx-values (or null mask16)))
       (if (numberp modifiers)
	   modifiers
	 (dolist (modifier modifiers mask)
	   (declare (type symbol modifier))
	   (let ((bit (position modifier (the simple-vector +state-mask-vector+) :test #'eq)))
	     (setq mask
		   (logior mask
			   (if bit
			       (ash 1 bit)
			     (or (cdr (assoc modifier display-mapping))
				 ;; bad modifier
				 (if errorp
				     (return-from modifiers->mask nil)
				   0))))))))))

    (let* ((display-mapping (get-display-modifier-mapping display))
	   (mapping-modifiers (keysym-mapping-modifiers mapping))
	   (modifiers (or (modifiers->mask display-mapping (or mapping-modifiers 0) t)
			  (return-from mapping-matches-p nil)))
	   (mapping-mask (or (keysym-mapping-mask mapping)	; If no mask, use the default.
			     (if mapping-modifiers	        ; If no modifiers, match anything.
				 *default-keysym-translate-mask*
			       0)))
	   (mask (if (eq mapping-mask :modifiers)
		     modifiers
		   (modifiers->mask display-mapping mapping-mask nil))))
      (declare (type mask16 modifiers mask))
      (= (logand state mask) modifiers))))

(defun default-keysym-index (display keycode state)
  ;; Returns a keysym-index for use with keycode->character
  (declare (clx-values card8))
  (macrolet ((keystate-p (state keyword)
	       `(logbitp ,(position keyword +state-mask-vector+) ,state)))
    (let* ((mapping (display-keyboard-mapping display))
	   (keysyms-per-keycode (array-dimension mapping 1))
	   (symbolp (and (> keysyms-per-keycode 2)
			 (state-keysymp display state character-set-switch-keysym)))
	   (result (if symbolp 2 0)))
      (declare (type (simple-array keysym (* *)) mapping)
	       (type generalized-boolean symbolp)
	       (type card8 keysyms-per-keycode result))
      (when (and (< result keysyms-per-keycode)
		 (keysym-shift-p display state (keysym-uppercase-alphabetic-p
						 (aref mapping keycode 0))))
	(incf result))
      result)))

(defun keysym-shift-p (display state uppercase-alphabetic-p &key
		       shift-lock-xors
		       (control-modifiers
			 '#.(list left-meta-keysym left-super-keysym left-hyper-keysym)))
  (declare (type display display)
	   (type card16 state)
	   (type generalized-boolean uppercase-alphabetic-p)
	   (type generalized-boolean shift-lock-xors));;; If T, both SHIFT-LOCK and SHIFT is the same
	                                  ;;; as neither if the character is alphabetic.
  (declare (clx-values generalized-boolean))
  (macrolet ((keystate-p (state keyword)
	       `(logbitp ,(position keyword +state-mask-vector+) ,state)))
    (let* ((controlp (or (keystate-p state :control)
			 (dolist (modifier control-modifiers)
			   (when (state-keysymp display state modifier)
			     (return t)))))
	   (shiftp (keystate-p state :shift))
	   (lockp  (keystate-p state :lock))
	   (alphap (or uppercase-alphabetic-p
		       (not (state-keysymp display #.(make-state-mask :lock)
					   caps-lock-keysym)))))
      (declare (type generalized-boolean controlp shiftp lockp alphap))
      ;; Control keys aren't affected by lock
      (unless controlp
	;; Not a control character - check state of lock modifier
	(when (and lockp
		   alphap
		   (or (not shiftp) shift-lock-xors))	; Lock doesn't unshift unless shift-lock-xors
	  (setq shiftp (not shiftp))))
      shiftp)))

;;; default-keysym-index implements the following tables:
;;;
;;; control shift caps-lock character               character
;;;   0       0       0       #\a                      #\8
;;;   0       0       1       #\A                      #\8
;;;   0       1       0       #\A                      #\*
;;;   0       1       1       #\A                      #\*
;;;   1       0       0       #\control-A              #\control-8
;;;   1       0       1       #\control-A              #\control-8
;;;   1       1       0       #\control-shift-a        #\control-*
;;;   1       1       1       #\control-shift-a        #\control-*
;;;
;;; control shift shift-lock character               character
;;;   0       0       0       #\a                      #\8
;;;   0       0       1       #\A                      #\*
;;;   0       1       0       #\A                      #\*
;;;   0       1       1       #\A                      #\8
;;;   1       0       0       #\control-A              #\control-8
;;;   1       0       1       #\control-A              #\control-*
;;;   1       1       0       #\control-shift-a        #\control-*
;;;   1       1       1       #\control-shift-a        #\control-8

(defun keycode->character (display keycode state &key keysym-index
	                   (keysym-index-function #'default-keysym-index))
  ;; keysym-index defaults to the result of keysym-index-function which
  ;; is called with the following parameters:
  ;; (char0 state caps-lock-p keysyms-per-keycode)
  ;; where char0 is the "character" object associated with keysym-index 0 and
  ;; caps-lock-p is non-nil when the keysym associated with the lock
  ;; modifier is for caps-lock.
  ;; STATE can also used for setting character attributes.
  ;; Implementation dependent function.
  (declare (type display display)
	   (type card8 keycode)
	   (type card16 state)
	   (type (or null card8) keysym-index)
	   (type (or null (function (base-char card16 generalized-boolean card8) card8))
		 keysym-index-function))
  (declare (clx-values (or null character)))
  (let* ((index (or keysym-index
		    (funcall keysym-index-function display keycode state)))
	 (keysym (if index (keycode->keysym display keycode index) 0)))
    (declare (type (or null card8) index)
	     (type keysym keysym))
    (when (plusp keysym)
      (keysym->character display keysym state))))

(defun get-display-modifier-mapping (display)
  (labels ((keysym-replace (display modifiers mask &aux result)
	     (dolist (modifier modifiers result)
	       (push (cons (keycode->keysym display modifier 0) mask) result))))
    (or (display-modifier-mapping display)
	(multiple-value-bind (shift lock control mod1 mod2 mod3 mod4 mod5)
	    (modifier-mapping display)
	  (setf (display-modifier-mapping display)
		(nconc (keysym-replace display shift #.(make-state-mask :shift))
		       (keysym-replace display lock #.(make-state-mask :lock))
		       (keysym-replace display control #.(make-state-mask :control))
		       (keysym-replace display mod1 #.(make-state-mask :mod-1))
		       (keysym-replace display mod2 #.(make-state-mask :mod-2))
		       (keysym-replace display mod3 #.(make-state-mask :mod-3))
		       (keysym-replace display mod4 #.(make-state-mask :mod-4))
		       (keysym-replace display mod5 #.(make-state-mask :mod-5))))))))

(defun state-keysymp (display state keysym)
  ;; Returns T when a modifier key associated with KEYSYM is on in STATE
  (declare (type display display)
	   (type card16 state)
	   (type keysym keysym))
  (declare (clx-values generalized-boolean))
  (let* ((mapping (get-display-modifier-mapping display))
	 (mask (assoc keysym mapping)))
    (and mask (plusp (logand state (cdr mask))))))

(defun mapping-notify (display request start count)
  ;; Called on a mapping-notify event to update
  ;; the keyboard-mapping cache in DISPLAY
  (declare (type display display)
	   (type (member :modifier :keyboard :pointer) request)
	   (type card8 start count)
	   (ignore count start))
  ;; Invalidate the keyboard mapping to force the next key translation to get it
  (case request
    (:modifier 
     (setf (display-modifier-mapping display) nil))
    (:keyboard
     (setf (display-keysym-mapping display) nil))))

(defun keysym-in-map-p (display keysym keymap)
  ;; Returns T if keysym is found in keymap
  (declare (type display display)
	   (type keysym keysym)
	   (type (bit-vector 256) keymap))
  (declare (clx-values generalized-boolean))
  ;; The keysym may appear in the keymap more than once,
  ;; So we have to search the entire keysym map.
  (do* ((min (display-min-keycode display))
	(max (display-max-keycode display))
	(map (display-keyboard-mapping display))
	(jmax (min 2 (array-dimension map 1)))
	(i min (1+ i)))
      ((> i max))
    (declare (type card8 min max jmax)
	     (type (simple-array keysym (* *)) map))
    (when (and (plusp (aref keymap i))
	       (dotimes (j jmax)
		 (when (= keysym (aref map i j)) (return t))))
      (return t))))

(defun character-in-map-p (display character keymap)
  ;; Implementation dependent function.
  ;; Returns T if character is found in keymap
  (declare (type display display)
	   (type character character)
	   (type (bit-vector 256) keymap))
  (declare (clx-values generalized-boolean))
  ;; Check all one bits in keymap
  (do* ((min (display-min-keycode display))
	(max (display-max-keycode display))
	(jmax (array-dimension (display-keyboard-mapping display) 1))
	(i min (1+ i)))
      ((> i max))
    (declare (type card8 min max jmax))
    (when (and (plusp (aref keymap i))
	       ;; Match when character is in mapping for this keycode
	       (dotimes (j jmax)
		 (when (eql character (keycode->character display i 0 :keysym-index j))
		   (return t))))
      (return t))))

(defun keysym->keycodes (display keysym)
  ;; Return keycodes for keysym, as multiple values
  (declare (type display display)
	   (type keysym keysym))
  (declare (clx-values (or null keycode) (or null keycode) (or null keycode)))
  ;; The keysym may appear in the keymap more than once,
  ;; So we have to search the entire keysym map.
  (do* ((min (display-min-keycode display))
	(max (display-max-keycode display))
	(map (display-keyboard-mapping display))
	(jmax (min 2 (array-dimension map 1)))
	(i min (1+ i))
	(result nil))
      ((> i max) (values-list result))
    (declare (type card8 min max jmax)
	     (type (simple-array keysym (* *)) map))
    (dotimes (j jmax)
      (when (= keysym (aref map i j))
	(push i result)))))
