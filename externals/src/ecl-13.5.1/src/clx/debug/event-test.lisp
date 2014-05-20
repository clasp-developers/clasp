;;; -*- Mode: LISP; Syntax: Common-lisp; Package: (XTEST (XLIB LISP)); Base: 10; Lowercase: Yes -*-

(in-package :xtest :use '(:xlib :lisp))

(defstruct event
  key					   ; Event key
  display				   ; Display event was reported to
  ;; The following are from the CLX event
  code
  state
  time
  event-window
  root
  drawable
  window
  child
  parent
  root-x
  root-y
  x
  y
  width
  height
  border-width
  override-redirect-p
  same-screen-p
  configure-p
  hint-p
  kind
  mode
  keymap
  focus-p
  count
  major
  minor
  above-sibling
  place
  atom
  selection
  requestor
  target
  property
  colormap
  new-p
  installed-p
  format
  type
  data
  send-event-p
  )

(defun process-input (display &optional timeout)
  "Process one event"
  (declare (type display display)		; The display (from initialize-clue)
	   (type (or null number) timeout)	; optional timeout in seconds
	   (values (or null character)))        ; Returns NIL only if timeout exceeded
  (let ((event (make-event)))
    (setf (event-display event) display)
    (macrolet ((set-event (&rest parameters)
		 `(progn ,@(mapcar #'(lambda (parm)
				       `(setf (,(intern (concatenate 'string
							  (string 'event-)
							  (string parm)))
					       event) ,parm))
				   parameters)))
	       (dispatch (contact)
		  `(dispatch-event event event-key send-event-p ,contact)))

      (let ((result
	      (xlib:event-case (display :timeout timeout :force-output-p t)
		((:key-press :key-release :button-press :button-release)
		 (code time root window child root-x root-y x y
		       state same-screen-p event-key send-event-p)
		 (set-event code time root window child root-x root-y x y
			    state same-screen-p)
		 (dispatch window))
		
		(:motion-notify
		  (hint-p time root window child root-x root-y x y
			  state same-screen-p event-key send-event-p)
		  (set-event hint-p time root window child root-x root-y x y
			     state same-screen-p)
		  (dispatch window))
		
		((:enter-notify :leave-notify)
		 (kind time root window child root-x root-y x y
		       state mode focus-p same-screen-p event-key send-event-p)
		 (set-event kind time root window child root-x root-y x y
			    state mode focus-p same-screen-p)
		 (dispatch window))
		
		((:focus-in :focus-out)
		 (kind window mode event-key send-event-p)
		 (set-event kind window mode)
		 (dispatch window))
		
		(:keymap-notify
		  (window keymap event-key send-event-p)
		  (set-event window keymap)
		  (dispatch window))
		
		(:exposure
		  (window x y width height count event-key send-event-p)
		  (set-event window x y width height count)
		  (dispatch window))
		
		(:graphics-exposure
		  (drawable x y width height count major minor event-key send-event-p)
		  (set-event drawable x y width height count major minor)
		  (dispatch drawable))
		
		(:no-exposure
		  (drawable major minor event-key send-event-p)
		  (set-event drawable major minor)
		  (dispatch drawable))
		
		(:visibility-notify
		  (window state event-key send-event-p)
		  (set-event window state)
		  (dispatch window))
		
		(:create-notify
		  (parent window x y width height border-width
			  override-redirect-p event-key send-event-p)
		  (set-event parent window x y width height border-width
			     override-redirect-p)
		  (dispatch parent))
		
		(:destroy-notify
		  (event-window window event-key send-event-p)
		  (set-event event-window window)
		  (dispatch event-window))
		
		(:unmap-notify
		  (event-window window configure-p event-key send-event-p)
		  (set-event event-window window configure-p)
		  (dispatch event-window))
		
		(:map-notify
		  (event-window window override-redirect-p event-key send-event-p)
		  (set-event event-window window override-redirect-p)
		  (dispatch event-window))
		
		(:map-request
		  (parent window event-key send-event-p)
		  (set-event parent window)
		  (dispatch parent))
		
		(:reparent-notify
		  (event-window window parent x y override-redirect-p event-key send-event-p)
		  (set-event event-window window parent x y override-redirect-p)
		  (dispatch event-window))
		
		(:configure-notify
		  (event-window window above-sibling x y width height border-width
				override-redirect-p event-key send-event-p)
		  (set-event event-window window above-sibling x y width height
			     border-width override-redirect-p)
		  (dispatch event-window))
		
		(:configure-request
		  (parent window above-sibling x y width height border-width event-key send-event-p)
		  (set-event parent window above-sibling x y width height border-width)
		  (dispatch parent))
		
		(:gravity-notify
		  (event-window window x y event-key send-event-p)
		  (set-event event-window window x y)
		  (dispatch event-window))
		
		(:resize-request
		  (window width height event-key send-event-p)
		  (set-event window width height)
		  (dispatch window))
		
		(:circulate-notify
		  (event-window window parent place event-key send-event-p)
		  (set-event event-window window parent place)
		  (dispatch event-window))
		
		(:circulate-request
		  (parent window place event-key send-event-p)
		  (set-event parent window place)
		  (dispatch parent))
		
		(:property-notify
		  (window atom time state event-key send-event-p)
		  (set-event window atom time state)
		  (dispatch window))
		
		(:selection-clear
		  (time window selection event-key send-event-p)
		  (set-event time window selection)
		  (dispatch window))
		
		(:selection-request
		  (time window requestor selection target property event-key send-event-p)
		  (set-event time window requestor selection target property)
		  (dispatch window))
		
		(:selection-notify
		  (time window selection target property event-key send-event-p)
		  (set-event time window selection target property)
		  (dispatch window))
		
		(:colormap-notify
		  (window colormap new-p installed-p event-key send-event-p)
		  (set-event window colormap new-p installed-p)
		  (dispatch window))
		
		(:client-message
		  (format window type data event-key send-event-p)
		  (set-event format window type data)
		  (dispatch window))
		
		(:mapping-notify
		  (request start count)
		  (mapping-notify display request start count)) ;; Special case
		)))
	(and result t)))))

(defun event-case-test (display)
  ;; Tests universality of display, event-key, event-code, send-event-p and event-window
  (event-case (display)
    ((key-press key-release button-press button-release motion-notify
      enter-notify leave-notify focus-in focus-out keymap-notify
      exposure graphics-exposure no-exposure visibility-notify
      create-notify destroy-notify unmap-notify map-notify map-request
      reparent-notify configure-notify gravity-notify resize-request
      configure-request circulate-notify circulate-request property-notify
      selection-clear selection-request selection-notify colormap-notify client-message)
     (display event-key event-code send-event-p event-window)
     (print (list display event-key event-code send-event-p event-window)))
    (mapping-notify ;; mapping-notify doesn't have event-window
      (display event-key event-code send-event-p)
      (print (list display event-key event-code send-event-p)))
    ))
