(defvar *a* (loadArchive "_trainer00004688.cxml"))
(defvar *render* (rendered *a* ()))
(prognLog "Now create the RenderController"
	  (defvar *renderController* (createRenderController *render*))
	  (describe *renderController*))