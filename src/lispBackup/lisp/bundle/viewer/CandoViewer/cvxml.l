(usePackage :wx)


(Load wxXmlResource_Get (findFileInLispPath "CANDO/viewer/CandoViewer/candoViewer.xrc"))
(defvar *viewerFrame* (LoadFrame wxXmlResource_Get () "CandoViewer"))
(defvar *replFrame* (LoadFrame wxXmlResource_Get () "REPL"))
(defvar *infoFrame* (LoadFrame wxXmlResource_Get () "Information"))
(defvar *framesFrame* (LoadFrame wxXmlResource_Get () "Frames"))
(defvar *menubar* (LoadMenuBar wxXmlResource_Get *viewerFrame* "mainMenuBar"))



(dumpChildren *replFrame*)
(let ((a (FindWindowByName "repl" *replFrame*)))
  (print (% "I found a window = %s" (repr a))))