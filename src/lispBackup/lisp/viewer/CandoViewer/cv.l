(include "CANDO/viewer/CandoViewer/manipulator.l")
(include "CANDO/viewer/CandoViewer/sideView.l")
(usePackage :mwx)


(defClass CandoViewerPresenter () ( _ViewerFrame _FrameFrame _FrameListBox _ReplFrame _ReplShell
						 _InfoFrame _InfoTextBox _SliderName _Canvas _Marks _KeyMode
						 _DragControl
						 _Manipulator _CurrentLine )
  (method __init__ (self &key frameFrame infoFrame viewerFrame canvas replFrame )
	  (setq (slot self _Canvas) canvas )
	  (setq (slot self _ViewerFrame) viewerFrame)
	  (setq (slot self _DragControl) () )
	  (Show (slot self _ViewerFrame) true)
	  (SetSize (slot self _ViewerFrame) 470 40 800 800)
	  (setq (slot self _ReplFrame) replFrame)
	  (Show (slot self _ReplFrame) true )
	  (SetSize (slot self _ReplFrame) 60 40 400 600)
	  (setq (slot self _FrameFrame) frameFrame)
	  (setq (slot self _FrameListBox) (FindWindowByName "m_listBox_frames" (slot self _FrameFrame)))
	  (setq (slot self _InfoFrame) infoFrame)
	  (setq (slot self _InfoTextBox) (FindWindowByName "m_textCtrl_info" (slot self _InfoFrame)))
	  (setq (slot self _Marks) ())
	  (setq (slot self _CurrentLine) "")
	  (setq (slot self _Manipulator) (Manipulator canvas))
	  (setq (slot self _KeyMode) ())
	  (assert (slot self _InfoTextBox) "InfoTextBox is nil")
	  (setq (slot self _SliderName) ())
	  (Bind (slot self _ViewerFrame)
		wxEVT_COMMAND_MENU_SELECTED (lambda (event data)
					      (menuOpen self)) (XRCID "m_menu_open"))
	  (Bind (slot self _ViewerFrame)
		wxEVT_COMMAND_MENU_SELECTED (lambda (event data)
					      (menuOpenScript self)) (XRCID "m_menu_openScript"))
	  (Bind (slot self _ViewerFrame)
		wxEVT_COMMAND_MENU_SELECTED (lambda (event data)
					      (menuSaveScript self)) (XRCID "m_menu_saveScript"))
	  (Bind (slot self _ViewerFrame)
		wxEVT_COMMAND_MENU_SELECTED (lambda (event data)
					      (menuSaveScriptAs self)) (XRCID "m_menu_saveScriptAs"))
	  (Bind (slot self _FrameListBox)
		wxEVT_COMMAND_LISTBOX_SELECTED	(lambda (event data)
						  (frameSelected self (GetInt event))) )

	  (bindTrigger (virtualSphere (layerManager (slot self _Canvas)))
		       :changed (lambda (sender) (onVirtualSphereChanged self sender)))
	  (bindTrigger (layerManager (slot self _Canvas))
		       :backgroundColorChanged (lambda (sender) (raise "Handle this")))
	  (bindTrigger (layerManager (slot self _Canvas))
		       :fogChanged (lambda (sender) (raise "handle this")))
	  (bindTrigger (layerManager (slot self _Canvas))
		       :selectionRectangleChanged (lambda (sender) (onSelectionRectangleChanged self sender)))
	  (bindTrigger (layerManager (slot self _Canvas))
		       :selectionChanged (lambda (sender) (onSelectionChanged self sender)))
	  (bindCanvasEvents self (slot self _Canvas))
	  (bindSideBarButtons self *viewerFrame*)
	  (createLayers self (slot self _Canvas))
	  (dumpChildren (slot self _ReplFrame))
	  (setq (slot self _ReplShell) (FindWindowByName "repl" (slot self _ReplFrame))) ; "repl"
	  (let ((replOutput (FindWindowByName "replOutput" (slot self _ReplFrame))))
	    (SetOutput (slot self _ReplShell) replOutput))
	  (print (% "Found replShell = %s" (repr (slot self _ReplShell))))
	  )

  (method bindSideBarButtons (self viewerFrame)
	  (let ((dumpCameraButton (FindWindowByName "m_button_dumpCamera" viewerFrame)))
	    (Bind dumpCameraButton wxEVT_COMMAND_BUTTON_CLICKED
		  (lambda (event data) (dumpCamera self))))
	  (Bind (FindWindowByName "m_checkBoxAutoFocus" viewerFrame)
		wxEVT_COMMAND_CHECKBOX_CLICKED
		(lambda (event data) (onChangeAutoFocus self event)))
	  (Bind (FindWindowByName "m_buttonFocus" viewerFrame)
		wxEVT_COMMAND_BUTTON_CLICKED
		(lambda (event data) (onButtonFocus self event)))
	  )



  (method bindCanvasEvents (self canvas)
	  (let ((manipulator (slot self _Manipulator)))
	    (Bind canvas wxEVT_KEY_DOWN (lambda (event data) (onKeyDownInCanvas self event data)))
	    (Bind canvas wxEVT_KEY_UP (lambda (event data) (onKeyUpInCanvas self event data)))
	    (Bind canvas wxEVT_LEFT_DOWN (lambda (event data) (onMouse1Down self event)))
	    (Bind canvas wxEVT_LEFT_UP (lambda (event data) (onMouse1Up self event)))
	    (Bind canvas wxEVT_MOTION (lambda (event data) (onMouseMotion self event)))
	    (Bind canvas wxEVT_MIDDLE_DOWN (lambda (event data) (onMouse2Down self event)))
	    (Bind canvas wxEVT_MIDDLE_UP (lambda (event data) (onMouse2Up self event)))
	    (Bind canvas wxEVT_MOUSEWHEEL (lambda (event data) (onMouseWheel self event)))
	  ;;;	    (setMouse1Mode manipulator :idle)
		  (setMouseOverrideMode manipulator :rotate)
		  (setMouse2Mode manipulator :idle)
		  )
	  )



  (method createLayers (self canvas)
	  (log "Appending fixed layer")
	  (print (% "Appending fixed layer"))
	  (let ((lm (layerManager canvas))
		(camera (getCamera canvas)))
	    (appendWorldLayer lm (Layer :world))
	    (appendFixedLayer lm (createSideViewFixedLayer canvas camera))))

  (method onSelectionRectangleChanged (self sender)
	  (Refresh (slot self _Canvas)))

  (method onChangeAutoFocus (self event)
	  (let* ((rb (GetEventObject event))
		(v (GetValue rb)))
	    (print (% "onChangeAutoFocus value[%d]" v ))
	    (setAutoFocus (getCamera (slot self _Canvas)) v)
	    ))

  (method onVirtualSphereChanged (self sender)
	  (Refresh (slot self _Canvas)))
  (method onSelectionChanged (self sender)
	  (Refresh (slot self _Canvas)))


  (method toOglY (self eventy)
	  (- (getHeight (slot self _Canvas)) eventy))

  (method onMouse1Down (self event)
	  (let* ((canvas (GetEventObject event))
		 (x (GetX event))
		 (y (toOglY self (GetY event)))
		 pickedObject )
	    (setq pickedObject (pick canvas x y))
	    (cond 
	     ((isA pickedObject DragObject)
	      (setq (slot self _DragControl) pickedObject)
	      (apply (getStartCode pickedObject) event))
	     (true
	      ;; By default you are selecting
	      (if pickedObject
		  (progn
		    (let ((addToSelection (ShiftDown event)))
		      (oneObjectSelected (layerManager (slot self _Canvas)) pickedObject addToSelection)))
		(progn
		  (startSelectionBox self event)))))))

  (method startSelectionBox (self event)
	  (setq (slot self _DragControl)
		(let ( cameraMode viewIndex 
				  (layerManager (layerManager (slot self _Canvas))))
		  (DragObject :start (lambda (event)
				       (let ((rawx (GetX event))
					     (rawy (toOglY self (GetY event)))
					     viewx viewy)
					 (setq cameraMode (mode (getCamera (slot self _Canvas))))
					 (setq viewIndex (viewContainingPoint cameraMode rawx rawy))
					 (setq viewx (relativeViewX cameraMode viewIndex rawx))
					 (setq viewy (relativeViewY cameraMode viewIndex rawy))
					 (selectionRectangleStart layerManager viewx viewy)))
			      :drag (lambda (event) 
				      (let ((rx (relativeViewX cameraMode viewIndex (GetX event)))
					    (ry (relativeViewY cameraMode viewIndex (toOglY self (GetY event)))))
					(selectionRectangleDrag layerManager rx ry )))
			      :release (lambda (event) 
					 (let ((addToSelection (ShiftDown event)))
					   (selectionRectangleEnd layerManager)
					   (doSelection self event))))))
	  (apply (getStartCode (slot self _DragControl)) event)
	  )


  (method doSelection (self event)
	  (let ((addToSelection (ShiftDown event)))
	    (select (slot self _Canvas) addToSelection)
	    (let ((selected (getSelection (layerManager (slot self _Canvas)))))
	      (if selected
		  (print (% "There were %d things selected" (length selected)))
		(print "There was nothing selected")))))


  (method startRotatingVirtualSphere (self event)
	  (setq (slot self _DragControl)
		(let ((virtualSphere (virtualSphere (layerManager (slot self _Canvas))))
		      cameraMode viewIndex
		      viewWidth viewHeight )
		  (DragObject :start (lambda (event)
				       (let ((rawx (GetX event))
					     (rawy (toOglY self (GetY event)))
					     viewx viewy)
					 (setq cameraMode (mode (getCamera (slot self _Canvas))))
					 (setq viewIndex (viewContainingPoint cameraMode rawx rawy))
					 (setq viewWidth (viewWidth cameraMode viewIndex))
					 (setq viewHeight (viewHeight cameraMode viewIndex))
					 (setq viewx (relativeViewX cameraMode viewIndex rawx))
					 (setq viewy (relativeViewY cameraMode viewIndex rawy))
					 (setMouseStart virtualSphere :rotate viewx viewy viewWidth viewHeight )))
			      :drag (lambda (event) 
				      (let ((rx (relativeViewX cameraMode viewIndex (GetX event)))
					    (ry (relativeViewY cameraMode viewIndex (toOglY self (GetY event)))))
					(setMouseMove virtualSphere rx ry)))
			      :release (lambda (event) (print "Released")))))
	  (apply (getStartCode (slot self _DragControl)) event)
	  )


  (method onMouseWheel (self event)
	  (let ((wr (* 0.01 (GetWheelRotation event)))
		(wa (GetWheelAxis event))
		(virtualSphere (virtualSphere (layerManager (slot self _Canvas)))))
	    (log (% "Wheel rotation = %f  axis=%2d" wr wa))
	    (cond
	     ((== wa 1)
	      (log "Rotating around X-axis")
	      (setMouseXAxisWheelChange virtualSphere :rotate wr)
	      (Refresh (slot self _Canvas)))
	     ((== wa 0)
	      (log "Rotating around Y-axis")
	      (setMouseYAxisWheelChange virtualSphere :rotate wr)
	      (Refresh (slot self _Canvas)))
	     (true (print "Unkown wheel rotation")))))

  (method onMouse2Down (self event)
	  (startRotatingVirtualSphere self event))

  (method onMouseMotion (self event)
	  (when (slot self _DragControl) (apply (getDragCode (slot self _DragControl)) event)))

  (method endDrag (self event)
	  (when (slot self _DragControl)
	    (apply (getReleaseCode (slot self _DragControl)) event)
	    (setq (slot self _DragControl) ())))
	  
  (method onMouse1Up (self event) (endDrag self event))
  (method onMouse2Up (self event) (endDrag self event))

  

  (method onButtonFocus (self event)
	  (refocusViewOnSelection (slot self _Canvas))
	  (Refresh (slot self _Canvas)))

  (method getDatabaseRenderController (self)
	  (let* ((canvas (slot self _Canvas))
		(sliderLayer (worldLayerWithName (layerManager canvas) :world)))
	    (assert sliderLayer (% "There is no layer named :databaseLayer"))
	    (getRenderController sliderLayer)))

  (method onKeyDownInCanvas (self event data)
	  (let* ((keyCode (GetKeyCode event))
		(keyChar (asChar keyCode))
		(canvas (GetEventObject event))
		(manipulator (slot self _Manipulator)))
	    (cond
	     ((== keyChar "F" )
	      (progn
		(refocusViewOnSelection canvas)
		(Refresh canvas)))
	     ((== keyCode WXK_RIGHT)
	       (progn
		 (print "----increment slider")
		 (incrementSlider (getDatabaseRenderController self) (slot self _SliderName))
		 (updateFramesListAndCanvas self)))
	     ((== keyCode WXK_LEFT)
	       (progn
		 (print "----decrement slider")
		 (decrementSlider (getDatabaseRenderController self) (slot self _SliderName))
		 (updateFramesListAndCanvas self)))
	     ( true
	       (print (% "Caught unhandled KeyPressCallback with key[%d]" keyCode))))))


  (method onKeyUpInCanvas (self event data)
	  (print "onKeyUpInCanvas"))

;;  (method clipWidthChanged (self event)
;;	  (let* ((width (GetInt event))
;;		 (canvas (slot self _Canvas))
;;		 (dist (getDistance canvas))
;;		 (halfWidth (/ (real width) 2.0))
;;		 (near (- dist halfWidth))
;;		 (far (+ dist halfWidth)))
;;	    (setNearFarClippingDistances (slot self _Canvas) near far))
;;	  (Refresh (slot self _Canvas)))
;;
;;  (method viewDistanceChanged (self event)
;;	  (let ((val (GetInt event)))
;;	    (setDistance (slot self _Canvas) (real val)))
;;	  (Refresh (slot self _Canvas)))



  (method dumpCamera (self)
	  (print "dumping camera in method dumpCamera")
	  (dump (getCamera (slot self _Canvas)))
	  (dump (virtualSphere (layerManager (slot self _Canvas)))))


  (method evaluateRepl (self event)
	  (print "evaluateRepl - calling test")
	  (mwx:test (slot self _ReplShell)))

  (method updateFramesListAndCanvas (self)
	  (print "in updateFramesListAndCanvas")
	  (when (slot self _SliderName)
	    (let ((sliderIndex (getSliderValue (getDatabaseRenderController self) (slot self _SliderName))))
	      (print (% "updateFramesListAndCanvas sliderIndex[%d]" sliderIndex))
	      (Select (slot self _FrameListBox) sliderIndex)
	      (SetFirstItem (slot self _FrameListBox) sliderIndex ))
	    (Refresh (slot self _Canvas))
	    (updateInfo self)))


;;
;; A frame was selected by clicking within the list box
;; Set the slider of the RenderController, update the Canvas and the info window
  (method frameSelected (self frameIndex)
	  (print (% "Event item selected = %d" frameIndex ))
	  (setSliderValue (getDatabaseRenderController self) (slot self _SliderName) frameIndex)
	  (Refresh (slot self _Canvas))
	  (updateInfo self)
	  )

;; Update the Information window with the currently selected frame
  (method updateInfo (self)
	  (let ((info (getInformation (getDatabaseRenderController self))))
	    (Show (slot self _InfoFrame) true)
	    (Clear (slot self _InfoTextBox))
	    (AppendText (slot self _InfoTextBox) info)))

  (method updateFrames (self)
	  (let* ((frameListNames (sliderNamesAsCons (getDatabaseRenderController self)))
		 (sliderName (car frameListNames)))
	    (when (> (length frameListNames) 1)
	      (print "Only one FrameList will be used"))
	    (log (% "updateFrames sliderName[%s]" sliderName))
	    (setq (slot self _SliderName) sliderName)
	    (when sliderName
	      (let ((labels (sliderLabels (getDatabaseRenderController self) (slot self _SliderName))))
		(print (% "labels = %s" (asString labels)))
		(Show (slot self _FrameFrame) true)
		(Clear (slot self _FrameListBox))
		(InsertItems (slot self _FrameListBox) labels 0))))
	  )

  (method loadFileAccordingToExtension (self fileName)
	  (let* ((path (Path fileName))
		(ext (extension path))
		obj )
	    (print (% " extension is[%s]" ext) )
	    (cond
	     ((== ext ".mol2")
	      (loadMol2 fileName))
	     ((== ext ".cxml")
	      (loadArchive fileName))
	     (true
	      (print (% "Unknown file type for file[%s]" fileName ))
	      ()))))

  (method loadFile (self fileName)
	  (let ((pp (parent_path (Path fileName))))
	    (setCurrentWorkingDirectory (asString pp)))
	  (let* ((contents (loadFileAccordingToExtension self fileName))
		 (dl (rendered contents () ))
		 (rc (createRenderController dl)))
	    (describe rc)
	    (let* ((manager (layerManager *canvas*))
		   (print "loadFile ---- loading file and creating layer called :databaseLayer")
		   (layer (worldLayerWithName manager :world)))
	      (setRenderController layer rc))
	    (updateFrames self)
	    (updateInfo self)
	    (log "------Loaded file: GraphicsChanged")
	    (refocusViewOnSelection *canvas*)
	    (Refresh *canvas*)))

  (method menuOpen (self)
	  (print "You pressed open")
	  (let* ((curDir (asString *currentWorkingDirectory*))
		 (fd (wxFileDialog () "Select the file to display"
				   curDir "" "CandoXML files (*.cxml)|*.cxml|Mol2 files (*.mol2)|*.mol2" ))
		 (result (ShowModal fd ))
		 (fileName ()))
	    (cond
	     ((== result wxID_OK )
	      (setq fileName (GetPath fd ))
	      (print (% "Selected file[%s]" fileName ))
	      (loadFile self fileName))
	     (true
	      (print  "Dialog cancelled" )
	      ))))

  (method setScriptTitle (self)
	  (SetTitle (slot self _ReplFrame) 
		    (let ((fn (mwx:getFileName (slot self _ReplShell))))
		      (if (!= fn "")
			  (% "REPL: %s" fn)
		      "REPL: -noname-"))))

  (method loadScript (self fileName)
;;	  (let* ((fin (FileInStream fileName))
;;		 (contents (readEntireFile fin))
;;		 )
;;	    (close fin)
	    (LoadFile (slot self _ReplShell) fileName)
	    (setFileName (slot self _ReplShell) fileName)
	    (setScriptTitle self))

  (method saveScript (self fileName)
	  (let ((fout (FileOutStream fileName))
		(contents (GetValue (slot self _ReplShell))))
	    (write fout contents)
	    (setFileName (slot self _ReplShell) fileName)
	    (setScriptTitle self)))

  (method menuOpenScript (self)
	  (let* ((curDir (asString *currentWorkingDirectory*))
		 (fd (wxFileDialog () "Select the file to open" curDir "" "Lisp files (*.l)|*.l" wxFD_OPEN ))
		 (result (ShowModal fd ))
		 (fileName ()))
	    (cond
	     ((== result wxID_OK )
	      (setq fileName (GetPath fd ))
	      (log (% "Selected file[%s]" fileName ))
	      (loadScript self fileName))
	     (true
	      (print  "Dialog cancelled" )
	      ))))


  (method menuSaveScript (self)
	  (print "You pressed saveScript")
	  (let ((currentFileName (mwx:getFileName (slot self _ReplShell))))
	    (if (!= currentFileName "")
		(progn
		  (print (% "Saving to file: %s" currentFileName))
		  (saveScript self currentFileName))
	      (menuSaveScriptAs self))))

	      
  (method menuSaveScriptAs (self)
	  (print "You pressed saveScriptAs")
	  (let* ((curDir (asString *currentWorkingDirectory*))
		 (fd (wxFileDialog () "File to save to"
				   curDir "" "Lisp files (*.l)|*.l"
				   (logior wxFD_SAVE wxFD_OVERWRITE_PROMPT )))
		 (result (ShowModal fd ))
		 (fileName ()))
	    (cond
	     ((== result wxID_OK )
	      (setq fileName (GetPath fd ))
	      (log (% "Selected file[%s]" fileName ))
	      (saveScript self fileName))
	     (true
	      (log  "Dialog cancelled" )
	      ))))



  )

(Load wxXmlResource_Get (asString (findFileInLispPath "CANDO/viewer/CandoViewer/candoViewer.xrc")))
(defvar *viewerFrame* (LoadFrame wxXmlResource_Get () "CandoViewer"))
(defvar *replFrame* (LoadFrame wxXmlResource_Get () "REPL"))
(defvar *infoFrame* (LoadFrame wxXmlResource_Get () "Information"))
(defvar *framesFrame* (LoadFrame wxXmlResource_Get () "Frames"))
(defvar *menubar* (LoadMenuBar wxXmlResource_Get *viewerFrame* "mainMenuBar"))





(defvar *canvas* (FindWindowByName "GLCanvas" *viewerFrame*))
(assert *canvas* "Could not find [GLCanvas]")

(print "Defining the *presenter*")
(defvar *presenter* (CandoViewerPresenter :viewerFrame *viewerFrame*
					  :replFrame *replFrame*
					  :frameFrame *framesFrame*
					  :infoFrame *infoFrame*
					  :canvas *canvas*))



;; (let ((sl (StringList)))
;;   (append sl "One")
;;   (append sl "Two")
;;   (append sl "Three")
;;   (InsertItems *frames* sl 0))
;; (Bind *viewerFrame* wxEVT_COMMAND_MENU_SELECTED (lambda (event data) (menuOpen *presenter*)) (XRCID "m_menu_open"))
;; ;;(Show *framesFrame* true)
;; (Show *viewerFrame* true)
;; ;;(Show *infoFrame* true)
;; (AppendText *info* "This is information\n")
;; (AppendText *info* "This is the second line")


;;
;; Start the program
;;
(print (% "Args = %s" *ARGS*))
(when (> (length *ARGS* ) 1)
  (let ((fn (@ *ARGS* 1)))
    (print (% "Loading file: %s" fn))
    (loadFile *presenter* fn)))
