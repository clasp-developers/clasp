; fixed code

(usePackage :wx)


(defClass MyFrame wxFrame ()
  (method __init__ (self parent id name pos size )
	  (callAncestorMethod self parent id name pos size))
  (method OnIconize (self event userData )
	  (println  "Caught Iconize event" ))
  (method OnButton (self event userData )
	  (println  "Caught Button event" )))


(defvar *w1* (MyFrame () -1 "Frame1" (wxPoint 600 300 ) (wxSize 200 200 )))
(Load wxXmlResource_Get "./viewerInterface.xrc" )
(LoadFrame wxXmlResource_Get *w1* ()"mwxCandoViewBase" )
(defvar *canvas* (FindWindow_withName *w1* "GLCanvas" ))
(print (% "canvas = %s" (repr *canvas* ))) 
;;global b1 [w1 FindWindow_withName "m_button1" ]
;;assert [b1 notNil] "Could not find m_button1"
;;
;;
;;[b1 Bind wxEVT_COMMAND_BUTTON_CLICKED (lambda (event data) [w1 OnButton event data] ) ]

(Show *w1* true )
(let* ((fd (wxFileDialog () "Select the file to display" "./" "" "*.cxml" ))
       (result (ShowModal fd ))
       (fileName ()))
       (cond 
	((== result wxID_OK )	;E-A
	 (setq fileName (GetPath fd ))
	 (let* ((contents (loadArchive fileName))
		(dl (rendered contents ()))
		(rc (RenderController dl)))
	   (GraphicsChanged *canvas* rc)))
	(true 
	 (print  "Dialog cancelled" )
	 (exit 1 ))))

