;;; -*- Mode:Lisp; Syntax: Common-lisp; Package:XLIB; Base:10; Lowercase: Yes -*-

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 2909
;;;			       AUSTIN, TEXAS 78769
;;;
;;;            Copyright (C) 1988 Texas Instruments Incorporated.
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


;;;----------------------------------------------------------------------------------+
;;;                                                                                  |
;;; These functions demonstrate a simple menu implementation described in            |
;;; Kimbrough, Kerry, "Windows to the Future", Lisp Pointers, Oct-Nov, 1987.         |
;;; See functions JUST-SAY-LISP and POP-UP for demonstrations.                       |
;;;                                                                                  |
;;;----------------------------------------------------------------------------------+



(defstruct (menu)
  "A simple menu of text strings."
  (title "choose an item:")
  item-alist					;((item-window item-string))
  window
  gcontext
  width
  title-width
  item-width
  item-height
  (geometry-changed-p t))			;nil iff unchanged since displayed



(defun create-menu (parent-window text-color background-color text-font)
  (make-menu
    ;; Create menu graphics context
    :gcontext (CREATE-GCONTEXT :drawable   parent-window
			       :foreground text-color
			       :background background-color
			       :font       text-font)
    ;; Create menu window
    :window   (CREATE-WINDOW
		:parent       parent-window
		:class        :input-output
		:x            0			;temporary value
		:y            0			;temporary value
		:width        16		;temporary value
		:height       16		;temporary value		
		:border-width 2
		:border       text-color
		:background   background-color
		:save-under   :on
		:override-redirect :on		;override window mgr when positioning
		:event-mask   (MAKE-EVENT-MASK :leave-window					       
					       :exposure))))


(defun menu-set-item-list (menu &rest item-strings)
  ;; Assume the new items will change the menu's width and height
  (setf (menu-geometry-changed-p menu) t)

  ;; Destroy any existing item windows
  (dolist (item (menu-item-alist menu))
    (DESTROY-WINDOW (first item)))

  ;; Add (item-window item-string) elements to item-alist
  (setf (menu-item-alist menu)
	(let (alist)
	  (dolist (item item-strings (nreverse alist))
	    (push (list (CREATE-WINDOW
			  :parent     (menu-window menu)
			  :x          0         ;temporary value
			  :y          0         ;temporary value
			  :width      16        ;temporary value
			  :height     16        ;temporary value
			  :background (GCONTEXT-BACKGROUND (menu-gcontext menu))
			  :event-mask (MAKE-EVENT-MASK :enter-window
						       :leave-window
						       :button-press
						       :button-release))
			item)
		  alist)))))

(defparameter *menu-item-margin* 4
  "Minimum number of pixels surrounding menu items.")


(defun menu-recompute-geometry (menu)
  (when (menu-geometry-changed-p menu)
    (let* ((menu-font   (GCONTEXT-FONT (menu-gcontext menu)))
	   (title-width (TEXT-EXTENTS menu-font (menu-title menu)))
	   (item-height (+ (FONT-ASCENT menu-font) (FONT-DESCENT menu-font)))
	   (item-width  0)
	   (items       (menu-item-alist menu))
	   menu-width)
      
      ;; Find max item string width
      (dolist (next-item items)
	(setf item-width (max item-width 
			      (TEXT-EXTENTS menu-font (second next-item)))))
      
      ;; Compute final menu width, taking margins into account
      (setf menu-width (max title-width
			    (+ item-width *menu-item-margin* *menu-item-margin*)))      
      (let ((window  (menu-window menu))
	    (delta-y (+ item-height *menu-item-margin*)))
	
	;; Update width and height of menu window        
	(WITH-STATE (window)
	  (setf (DRAWABLE-WIDTH  window) menu-width
		(DRAWABLE-HEIGHT window) (+ *menu-item-margin*
					    (* (1+ (length items))
					       delta-y))))
	
	;; Update width, height, position of item windows
	(let ((item-left     (round (- menu-width item-width) 2))
	      (next-item-top delta-y))
	  (dolist (next-item items)
	    (let ((window (first next-item)))
	      (WITH-STATE (window)
		(setf (DRAWABLE-HEIGHT window) item-height
		      (DRAWABLE-WIDTH  window) item-width
		      (DRAWABLE-X      window) item-left
		      (DRAWABLE-Y      window) next-item-top)))
	    (incf next-item-top delta-y))))
      
      ;; Map all item windows
      (MAP-SUBWINDOWS (menu-window menu))

      ;; Save item geometry
      (setf (menu-item-width menu)         item-width
	    (menu-item-height menu)        item-height
	    (menu-width menu)              menu-width
	    (menu-title-width menu)        title-width
	    (menu-geometry-changed-p menu) nil))))


(defun menu-refresh (menu)
 (let* ((gcontext   (menu-gcontext menu))
        (baseline-y (FONT-ASCENT (GCONTEXT-FONT gcontext))))
   
   ;; Show title centered in "reverse-video"
   (let ((fg (GCONTEXT-BACKGROUND gcontext))
	 (bg (GCONTEXT-FOREGROUND gcontext)))
     (WITH-GCONTEXT (gcontext :foreground fg :background bg)
       (DRAW-IMAGE-GLYPHS
	 (menu-window menu)
	 gcontext
	 (round (- (menu-width menu)
		   (menu-title-width menu)) 2)	;start x
	 baseline-y				;start y
	 (menu-title menu))))
   
   ;; Show each menu item (position is relative to item window)
   (dolist (item (menu-item-alist menu))
     (DRAW-IMAGE-GLYPHS
       (first item) gcontext
       0					;start x
       baseline-y				;start y
       (second item)))))


(defun menu-choose (menu x y)
  ;; Display the menu so that first item is at x,y.
  (menu-present menu x y)
  
  (let ((items (menu-item-alist menu))
	(mw    (menu-window menu))
	selected-item)

    ;; Event processing loop
    (do () (selected-item)				
      (EVENT-CASE ((DRAWABLE-DISPLAY mw) :force-output-p t)
	(:exposure     (count)
		       
	 ;; Discard all but final :exposure then display the menu
	 (when (zerop count) (menu-refresh menu))
	 t)
	
	(:button-release (event-window)
	 ;;Select an item
	 (setf selected-item (second (assoc event-window items)))
	 t)
	
	(:enter-notify (window)
	 ;;Highlight an item
	 (let ((position (position window items :key #'first)))
	   (when position
	     (menu-highlight-item menu position)))
	 t)
	
	(:leave-notify (window kind)
	 (if (eql mw window)
	     ;; Quit if pointer moved out of main menu window
	     (setf selected-item (when (eq kind :ancestor) :none))

	   ;; Otherwise, unhighlight the item window left
	   (let ((position (position window items :key #'first)))
	     (when position
	       (menu-unhighlight-item menu position))))
	 t)
	
	(otherwise ()
		   ;;Ignore and discard any other event
		   t)))
    
    ;; Erase the menu
    (UNMAP-WINDOW mw)
    
    ;; Return selected item string, if any
    (unless (eq selected-item :none) selected-item)))


(defun menu-highlight-item (menu position)
  (let* ((box-margin  (round *menu-item-margin* 2))
	 (left        (- (round (- (menu-width menu) (menu-item-width menu)) 2)
			 box-margin))
	 (top         (- (* (+ *menu-item-margin* (menu-item-height menu))
			    (1+ position))
			 box-margin))
	 (width       (+ (menu-item-width menu) box-margin box-margin))
	 (height      (+ (menu-item-height menu) box-margin box-margin)))
    
    ;; Draw a box in menu window around the given item.
    (DRAW-RECTANGLE (menu-window menu)
		    (menu-gcontext menu)
		    left top
		    width height)))

(defun menu-unhighlight-item (menu position)
  ;; Draw a box in the menu background color
  (let ((gcontext (menu-gcontext menu)))
    (WITH-GCONTEXT (gcontext :foreground (gcontext-background gcontext))
      (menu-highlight-item menu position))))


(defun menu-present (menu x y)
  ;; Make sure menu geometry is up-to-date
  (menu-recompute-geometry menu)
  
  ;; Try to center first item at the given location, but
  ;; make sure menu is completely visible in its parent
  (let ((menu-window (menu-window menu)))
    (multiple-value-bind (tree parent) (QUERY-TREE menu-window)
      (declare (ignore tree))
      (WITH-STATE (parent)
	(let* ((parent-width  (DRAWABLE-WIDTH parent))
	       (parent-height (DRAWABLE-HEIGHT parent))
	       (menu-height   (+ *menu-item-margin*
				 (* (1+ (length (menu-item-alist menu)))
				    (+ (menu-item-height menu)  *menu-item-margin*))))
	       (menu-x        (max 0 (min (- parent-width (menu-width menu))
					  (- x (round (menu-width menu) 2)))))
	       (menu-y        (max 0 (min (- parent-height menu-height)
					  (- y (round (menu-item-height menu) 2/3)
					     *menu-item-margin*)))))
	  (WITH-STATE (menu-window)
	    (setf (DRAWABLE-X menu-window) menu-x
		  (DRAWABLE-Y menu-window) menu-y)))))

    ;; Make menu visible
    (MAP-WINDOW menu-window)))

(defun just-say-lisp (&optional (font-name "fixed"))
  (let* ((display   (open-default-display))
	 (screen    (first (DISPLAY-ROOTS display)))
	 (fg-color  (SCREEN-BLACK-PIXEL screen))
	 (bg-color  (SCREEN-WHITE-PIXEL screen))
	 (nice-font (OPEN-FONT display font-name))
	 (a-menu    (create-menu (screen-root screen)	;the menu's parent
				 fg-color bg-color nice-font)))
    
    (setf (menu-title a-menu) "Please pick your favorite language:")
    (menu-set-item-list a-menu "Fortran" "APL" "Forth" "Lisp")
    
    ;; Bedevil the user until he picks a nice programming language
    (unwind-protect
	(do (choice)
	    ((and (setf choice (menu-choose a-menu 100 100))
		  (string-equal "Lisp" choice))))

      (CLOSE-DISPLAY display))))
  

(defun pop-up (host strings &key (title "Pick one:") (font "fixed"))
  (let* ((display   (OPEN-DISPLAY host))
	 (screen    (first (DISPLAY-ROOTS display)))
	 (fg-color  (SCREEN-BLACK-PIXEL screen))
	 (bg-color  (SCREEN-WHITE-PIXEL screen))
	 (font      (OPEN-FONT display font))
	 (parent-width 400)
	 (parent-height 400)
	 (parent    (CREATE-WINDOW :parent (SCREEN-ROOT screen)
				   :override-redirect :on
				   :x 100 :y 100
				   :width parent-width :height parent-height
				   :background bg-color
				   :event-mask (MAKE-EVENT-MASK :button-press
								:exposure)))
	 (a-menu    (create-menu parent fg-color bg-color font))
	 (prompt    "Press a button...")	 
	 (prompt-gc (CREATE-GCONTEXT :drawable parent
				     :foreground fg-color
				     :background bg-color
				     :font font))
	 (prompt-y  (FONT-ASCENT font))
	 (ack-y     (- parent-height  (FONT-DESCENT font))))
    
    (setf (menu-title a-menu) title)
    (apply #'menu-set-item-list a-menu strings)
    
    ;; Present main window
    (MAP-WINDOW parent)
    
    (flet ((display-centered-text
	     (window string gcontext height width)	     
	     (multiple-value-bind (w a d l r fa fd) (text-extents gcontext string)
	       (declare (ignore a d l r))
	       (let ((box-height (+ fa fd)))
		 
		 ;; Clear previous text
		 (CLEAR-AREA window
			     :x 0 :y (- height fa)
			     :width width :height box-height)
		 
		 ;; Draw new text
		 (DRAW-IMAGE-GLYPHS window gcontext (round (- width w) 2) height string)))))
      
      (unwind-protect
	  (loop
	    (EVENT-CASE (display :force-output-p t)
	      
	      (:exposure (count)
			 
			 ;; Display prompt
			 (when (zerop count)
			   (display-centered-text
			     parent
			     prompt
			     prompt-gc
			     prompt-y
			     parent-width))
			 t)
	      
	      (:button-press (x y)
			     
			     ;; Pop up the menu
			     (let ((choice (menu-choose a-menu x y)))
			       (if choice
				   (display-centered-text
				     parent
				     (format nil "You have selected ~a." choice)
				     prompt-gc
				     ack-y
				     parent-width)
				   
				   (display-centered-text
				     parent
				     "No selection...try again."
				     prompt-gc
				     ack-y
				     parent-width)))
			     t)	    	    
	      
	      (otherwise ()
			 ;;Ignore and discard any other event
			 t)))
	
	(CLOSE-DISPLAY display)))))

