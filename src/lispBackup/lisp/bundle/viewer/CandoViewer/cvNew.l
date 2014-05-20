(usePackage :wx)

(defClass CandoViewerPresenter () ( _ViewerFrame _FrameFrame _FrameListBox _ReplFrame _ReplShell
						 _InfoFrame _InfoTextBox _SliderName _Canvas _Marks _KeyMode
						 _CurrentLine )
  (method __init__ (self &key frameFrame infoFrame viewerFrame canvas replFrame )
	  (setq (slot self _Canvas) canvas )
	  (setq (slot self _ViewerFrame) viewerFrame)
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
	  (setq (slot self _KeyMode) ())
	  (assert (slot self _InfoTextBox) "InfoTextBox is nil")
	  (setq (slot self _SliderName) ())
	  (Bind (slot self _ViewerFrame)
		wxEVT_COMMAND_MENU_SELECTED (lambda (event data) (menuOpen self)) (XRCID "m_menu_open"))
	  (Bind (slot self _ViewerFrame)
		wxEVT_COMMAND_MENU_SELECTED (lambda (event data) (menuOpenScript self)) (XRCID "m_menu_openScript"))
	  (Bind (slot self _ViewerFrame)
		wxEVT_COMMAND_MENU_SELECTED (lambda (event data) (menuSaveScript self)) (XRCID "m_menu_saveScript"))
	  (Bind (slot self _ViewerFrame)
		wxEVT_COMMAND_MENU_SELECTED (lambda (event data) (menuSaveScriptAs self)) (XRCID "m_menu_saveScriptAs"))
	  (Bind (slot self _FrameListBox)
		wxEVT_COMMAND_LISTBOX_SELECTED	(lambda (event data) (frameSelected self (GetInt event))) )
	  (bindCanvasEvents self (slot self _Canvas))
	  (let ((dumpCameraButton (FindWindowByName "m_button_dumpCamera" *viewerFrame*)))
	    (Bind dumpCameraButton wxEVT_COMMAND_BUTTON_CLICKED (lambda (event data) (dumpCamera self))))
	  (dumpChildren (slot self _ReplFrame))
	  (setq (slot self _ReplShell) (FindWindowByName "repl" (slot self _ReplFrame))) ; "repl"
	  (let ((replOutput (FindWindowByName "replOutput" (slot self _ReplFrame))))
	    (SetOutput (slot self _ReplShell) replOutput))
	  (print (% "Found replShell = %s" (repr (slot self _ReplShell))))
	  )

  (method bindCanvasEvents (self canvas)
	  (Bind canvas wxEVT_KEY_DOWN (lambda (event data) (onKeyPressDown self event data))))




  (method clipWidthChanged (self event)
	  (let* ((width (GetInt event))
		 (canvas (slot self _Canvas))
		 (dist (getDistance canvas))
		 (halfWidth (/ (real width) 2.0))
		 (near (- dist halfWidth))
		 (far (+ dist halfWidth)))
	    (setNearFarClippingDistances (slot self _Canvas) near far))
	  (Refresh (slot self _Canvas)))

  (method viewDistanceChanged (self event)
	  (let ((val (GetInt event)))
	    (setDistance (slot self _Canvas) (real val)))
	  (Refresh (slot self _Canvas)))


  (method onKeyPressDown (self event data)
	  (let ((key (GetKeyCode event)))
	    (cond
	     ( (== (asChar key) "F" )
	       (progn
		 (print "You hit focus")
		 (let ((canvas (GetEventObject event)))
		   (refocusViewOnSelection canvas))))
	     ( (== key WXK_RIGHT)
	       (progn
		 (incrementSlider (getRenderController (slot self _Canvas)) (slot self _SliderName))
		 (updateFramesListAndCanvas self)
		 (return-from keyPressedInCanvas ())))
	     ( (== key WXK_LEFT)
	       (progn
		 (decrementSlider (getRenderController (slot self _Canvas)) (slot self _SliderName))
		 (updateFramesListAndCanvas self)
		 (return-from keyPressedInCanvas ())))
	     ( true
	       ()))
	    (print (% "Caught unhandled KeyPressCallback with key[%d]" key))))


  (method dumpCamera (self)
	  (dump (getCamera (slot self _Canvas)))
	  (dump (getVirtualSphere (slot self _Canvas))))


  (method evaluateRepl (self event)
	  (print "evaluateRepl - calling test")
	  (wx:test (slot self _ReplShell)))

  (method updateFramesListAndCanvas (self)
	  (when (slot self _SliderName)
	    (let ((sliderIndex (getSliderValue (getRenderController (slot self _Canvas)) (slot self _SliderName))))
	      (Select (slot self _FrameListBox) sliderIndex)
	      (SetFirstItem (slot self _FrameListBox) sliderIndex ))
	    (Refresh (slot self _Canvas))
	    (updateInfo self)))


;;
;; A frame was selected by clicking within the list box
;; Set the slider of the RenderController, update the Canvas and the info window
  (method frameSelected (self frameIndex)
	  (print (% "Event item selected = %d" frameIndex ))
	  (setSliderValue (getRenderController (slot self _Canvas)) (slot self _SliderName) frameIndex)
	  (Refresh (slot self _Canvas))
	  (updateInfo self)
	  )

;; Update the Information window with the currently selected frame
  (method updateInfo (self)
	  (let ((info (getInformation (getRenderController (slot self _Canvas)))))
	    (Show (slot self _InfoFrame) true)
	    (Clear (slot self _InfoTextBox))
	    (AppendText (slot self _InfoTextBox) info)))

  (method updateFrames (self)
	  (let* ((frameListNames (sliderNamesAsCons (getRenderController (slot self _Canvas))))
		 (sliderName (car frameListNames)))
	    (when (> (length frameListNames) 1)
	      (print "Only one FrameList will be used"))
	    (setq (slot self _SliderName) sliderName)
	    (when sliderName
	      (let ((labels (sliderLabels (getRenderController (slot self _Canvas)) (slot self _SliderName))))
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
	    (updateFrames self)
	    (updateInfo self)
	    (GraphicsChanged *canvas* rc)))

  (method menuOpen (self)
	  (print "You pressed open")
	  (let* ((curDir (asString *currentWorkingDirectory*))
		 (fd (wxFileDialog () "Select the file to display" curDir "" "CandoXML files (*.cxml)|*.cxml|Mol2 files (*.mol2)|*.mol2" ))
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
		    (let ((fn (wx:getFileName (slot self _ReplShell))))
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
	  (let ((currentFileName (wx:getFileName (slot self _ReplShell))))
	    (if (!= currentFileName "")
		(progn
		  (print (% "Saving to file: %s" currentFileName))
		  (saveScript self currentFileName))
	      (menuSaveScriptAs self))))

	      
  (method menuSaveScriptAs (self)
	  (print "You pressed saveScriptAs")
	  (let* ((curDir (asString *currentWorkingDirectory*))
		 (fd (wxFileDialog () "File to save to" curDir "" "Lisp files (*.l)|*.l" (logior wxFD_SAVE wxFD_OVERWRITE_PROMPT )))
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

(Load wxXmlResource_Get (findFileInLispPath "CANDO/viewer/CandoViewer/candoViewer.xrc"))
(defvar *viewerFrame* (LoadFrame wxXmlResource_Get () "CandoViewer"))
(defvar *replFrame* (LoadFrame wxXmlResource_Get () "REPL"))
(defvar *infoFrame* (LoadFrame wxXmlResource_Get () "Information"))
(defvar *framesFrame* (LoadFrame wxXmlResource_Get () "Frames"))
(defvar *menubar* (LoadMenuBar wxXmlResource_Get *viewerFrame* "mainMenuBar"))





(defvar *canvas* (FindWindowByName "GLCanvas" *viewerFrame*))
(assert *canvas* "Could not find [GLCanvas]")

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
(print (% "Args = %s" l:*ARGS*))
(when (> (length *ARGS* ) 1)
  (let ((fn (@ *ARGS* 1)))
    (print (% "Loading file: %s" fn))
    (loadFile *presenter* fn)))
