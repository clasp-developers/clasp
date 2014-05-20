(usePackage :wx)

(defvar *topFrame* (wxFrame () -1 "TopFrame"))
(Load wxXmlResource_Get "./helloWorld.xrc")
(let ((ok (LoadFrame wxXmlResource_Get *topFrame* () "MyFrame1")))
  (print (% "LoadFrame MyFrame1 returned[%d]" ok)))
(let* ((label (FindWindowByName "m_staticText1" *topFrame*))
       (button (FindWindowByName "m_button1" *topFrame*))
       (timesClicked 0))
  (Bind button wxEVT_COMMAND_BUTTON_CLICKED
	(lambda (event data) (progn
			       (setq timesClicked (+ timesClicked 1))
			       (let ((msg (% "timesClicked=%d" timesClicked)))
				 (SetLabel label msg))))))
