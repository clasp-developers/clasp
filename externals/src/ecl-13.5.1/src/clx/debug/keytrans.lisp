;;; -*- Mode:Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;; CLX keysym-translation test programs

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

(defun list-missing-keysyms ()
  ;; Lists explorer characters which have no keysyms
  (dotimes (i 256)
    (unless (character->keysyms (int-char i))
      (format t "~%(define-keysym ~@c ~d)" (int-char i) i))))

(defun list-multiple-keysyms ()
  ;; Lists characters with more than one keysym
  (dotimes (i 256)
    (when (cdr (character->keysyms (int-char i)))
      (format t "~%Character ~@c [~d] has keysyms" (int-char i) i)
      (dolist (keysym (character->keysyms (int-char i)))
	(format t "  ~d ~d" (ldb (byte 8 8) keysym) (ldb (byte 8 0) keysym))))))

(defun check-lowercase-keysyms ()
  ;; Checks for keysyms with incorrect :lowercase parameters
  (maphash #'(lambda (key mapping)
	       (let* ((value (car mapping))
		      (char (keysym-mapping-object value)))
		 (if (and (characterp char) (both-case-p char)
			  (= (char-int char) (char-int (char-upcase char))))
		     ;; uppercase alphabetic character
		     (unless (eq (keysym-mapping-lowercase value)
				 (char-int (char-downcase char)))
		       (let ((lowercase (keysym-mapping-lowercase value))
			     (should-be (char-downcase char)))
			 (format t "~%Error keysym ~3d ~3d (~@c) has :Lowercase ~3d ~3d (~s) Should be ~3d ~3d (~@c)"
				 (ldb (byte 8 8) key)
				 (ldb (byte 8 0) key)
				 char
				 (and lowercase (ldb (byte 8 8) lowercase))
				 (and lowercase (ldb (byte 8 0) lowercase))
				 (int-char lowercase)
				 (ldb (byte 8 8) (char-int should-be))
				 (ldb (byte 8 0) (char-int should-be))
				 should-be)))
		   (when (keysym-mapping-lowercase value)
		     (let ((lowercase (keysym-mapping-lowercase value)))
		       (format t "~%Error keysym ~3d ~3d (~@c) has :lowercase ~3d ~3d (~@c) and shouldn't"
			       (ldb (byte 8 8) key)
			       (ldb (byte 8 0) key)
			       char
			       (and lowercase (ldb (byte 8 8) (char-int lowercase)))
			       (and lowercase (ldb (byte 8 0) (char-int lowercase)))
			       lowercase
			       ))))))
	   *keysym->character-map*))

(defun print-all-keysyms ()
  (let ((all nil))
    (maphash #'(lambda (key value) (push (cons key value) all)) *keysym->character-map*)
    (setq all (sort all #'< :key #'car))
    (format t "~%~d keysyms:" (length all))
    
    (dolist (keysym all)
      (format t "~%~3d ~3d~{ ~s~}"
	      (ldb (byte 8 8) (car keysym))
	      (ldb (byte 8 0) (car keysym))
	      (cadr keysym))
      (dolist (mapping (cddr keysym))
	(format t "~%~7@t~{ ~s~}" mapping)))))

(defun keysym-mappings (keysym &key display (mask-format #'identity))
  ;; Return all the keysym mappings for keysym.
  ;; Returns a list of argument lists that are argument-lists to define-keysym.
  ;; The following will re-create the mappings for KEYSYM:
  ;; (dolist (mapping (keysym-mappings) keysym)
  ;;    (apply #'define-keysym mapping))
  (let ((mappings (append (and display (cdr (assoc keysym (display-keysym-translation display))))
			  (gethash keysym *keysym->character-map*)))
	(result nil))
    (dolist (mapping mappings)
      (let ((object (keysym-mapping-object mapping))
	    (translate (keysym-mapping-translate mapping))
	    (lowercase (keysym-mapping-lowercase mapping))
	    (modifiers (keysym-mapping-modifiers mapping))
	    (mask (keysym-mapping-mask mapping)))
	(push (append (list object keysym)
		      (when translate (list :translate translate))
		      (when lowercase (list :lowercase lowercase))
		      (when modifiers (list :modifiers (funcall mask-format modifiers)))
		      (when mask (list :mask (funcall mask-format mask))))
	      result)))
    (nreverse result)))

#+comment
(defun print-keysym-mappings (keysym &optional display)
    (format t "~%(keysym ~d ~3d) "
	    (ldb (byte 8 8) keysym)
	    (ldb (byte 8 0) keysym))
  (dolist (mapping (keysym-mappings keysym :display display))
    (format t "~16t~{ ~s~}~%" mapping)))

(defun print-keysym-mappings (keysym &optional display)
  (flet ((format-mask (mask)
		      (cond ((numberp mask)
			     `(make-state-mask ,@(make-state-keys mask)))
			    ((atom mask) mask)
			    (t `(list ,@(mapcar
					  #'(lambda (item)
					      (if (numberp item)
						  `(keysym ,(keysym-mapping-object
							      (car (gethash item *keysym->character-map*))))
						item))
					  mask))))))
    (dolist (mapping (keysym-mappings keysym :display display :mask-format #'format-mask))
      (format t "~%(define-keysym ~s (keysym ~d ~3d)~{ ~s~})"
	      (car mapping)
	      (ldb (byte 8 8) keysym)
	      (ldb (byte 8 0) keysym)
	      (cdr mapping)))))

(defun keysym-test (host)
  ;; Server key-press Loop-back test
  (let* ((display (open-display host))
	 (width 400)
	 (height 400)
	 (screen (display-default-screen display))
	 (black (screen-black-pixel screen))
	 (white (screen-white-pixel screen))
	 (win (create-window
		:parent (screen-root screen)
		:background black
		:border white
		:border-width 1
		:colormap (screen-default-colormap screen)
		:bit-gravity :center
		:event-mask '(:exposure :key-press)
		:x 20 :y 20
		:width width :height height))
	 #+comment
	 (gc (create-gcontext
	       :drawable win
	       :background black
	       :foreground white)))
    (initialize-extensions display)
    
    (map-window win)				; Map the window
    ;; Handle events
    (unwind-protect
	(dotimes (state 64)
	  (do ((code (display-min-keycode display) (1+ code)))
	      ((> code (display-max-keycode display)))
	    (send-event win :key-press '(:key-press) :code code :state state
			:window win :root (screen-root screen) :time 0
			:x 1 :y 2 :root-x 10 :root-y 20 :same-screen-p t)
	    (event-case (display :force-output-p t :discard-p t)
	      (exposure  ;; Come here on exposure events
		(window count)
		(when (zerop count) ;; Ignore all but the last exposure event
		  (clear-area window))
		nil)
	      (key-press (display code state)
			 (princ (keycode->character display code state))
			 t))))
      (close-display display))))

(defun keysym-echo (host &optional keymap-p)
  ;; Echo characters typed to a window
  (let* ((display (open-display host))
	 (width 400)
	 (height 400)
	 (screen (display-default-screen display))
	 (black (screen-black-pixel screen))
	 (white (screen-white-pixel screen))
	 (win (create-window
		:parent (screen-root screen)
		:background black
		:border white
		:border-width 1
		:colormap (screen-default-colormap screen)
		:bit-gravity :center
		:event-mask '(:exposure :key-press :keymap-state :enter-window)
		:x 20 :y 20
		:width width :height height))
	 (gc (create-gcontext
	       :drawable win
	       :background black
	       :foreground white)))
    (initialize-extensions display)
    
    (map-window win)				; Map the window
    ;; Handle events
    (unwind-protect
	(event-case (display :force-output-p t :discard-p t)
	  (exposure  ;; Come here on exposure events
	    (window count)
	    (when (zerop count) ;; Ignore all but the last exposure event
	      (clear-area window)
	      (draw-glyphs window gc 10 10 "Press <escape> to exit"))
	    nil)
	  (key-press (display code state)
		     (let ((char (keycode->character display code state)))
		       (format t "~%Code: ~s State: ~s Char: ~s" code state char)
		       ;; (PRINC char) (PRINC " ")
		       (when keymap-p
			 (let ((keymap (query-keymap display)))
			   (unless (character-in-map-p display char keymap)
			     (print "character-in-map-p failed")
			     (print-keymap keymap))))
		       ;; (when (eql char #\0) (setq disp display) (break))
		       (eql char #\escape)))
	  (keymap-notify (keymap)
	    (print "Keymap-notify")  ;; we never get here.  Server bug?
	    (when (keysym-in-map-p display 65 keymap)
	      (print "Found A"))
	    (when (character-in-map-p display #\b keymap)
	      (print "Found B")))
	  (enter-notify (event-window) (format t "~%Enter ~s" event-window)))
      (close-display display))))

(defun print-keymap (keymap)
  (do ((j 32 (+ j 32))) ;; first 32 bits is for window
      ((>= j 256))
      (format t "~% ~3d: " j)
      (do ((i j (1+ i)))
	  ((>= i (+ j 32)))
	(when (zerop (logand i 7))
	  (princ " "))
	(princ (aref keymap i)))))

(defun define-keysym-test (&key display printp
			   (modifiers (list (keysym :left-meta))) (mask :modifiers))
  (let* ((keysym 067)
	 (args `(baz ,keysym :modifiers ,modifiers ,@(and mask `(:mask ,mask))))
	 (original (copy-tree (keysym-mappings keysym :display display))))
    (when printp (print-keysym-mappings 67) (terpri))
    (apply #'define-keysym args)
    (when printp (print-keysym-mappings 67) (terpri))
    (let ((is (keysym-mappings keysym :display display))
	  (should-be (append original (list args))))
      (unless (equal is should-be)
	(cerror "Ignore" "define-keysym error. ~%is:        ~s ~%Should be: ~s" is should-be)))
    (apply #'undefine-keysym args)
    (when printp (print-keysym-mappings 67) (terpri))
    (let ((is (keysym-mappings keysym :display display)))
      (unless (equal is original)
	(cerror "Ignore" "undefine-keysym error. ~%is:        ~s ~%Should be: ~s" is original)))))

(define-keysym-test)
(define-keysym-test :modifiers (make-state-mask :shift :lock))
(define-keysym-test :modifiers (list :shift (keysym :left-meta) :control))
(define-keysym-test :modifiers (make-state-mask :shift :lock) :mask nil)

