


(defun defstruct-expand-initializers (objName slotNames)
  (let (initializers)
    (dolist (slotName slotNames)
      (setq initializers (Cons `(setf (slot ,objName ,slotName) ,slotName) initializers))
      )
    initializers))



(defmacro defstruct (name-and-options &rest slotSetup)
  (log (% "defstruct name-and-options[%s] slotSetup[%s] (length slotSetup)==>%d\n" name-and-options slotSetup (length slotSetup)))
  (let (structName maker slotNames slotReaderMethods slotReaderNames slotWriters)
    (if (symbolp name-and-options)
	(progn
	  (setq structName name-and-options))
	(raise (LispError "Add support for defstruct options")))
    (dolist (slotInfo slotSetup)
      (log (% "Processing slotInfo=%s\n" slotInfo))
      (if (symbolp slotInfo)
	  (let* ((slotName slotInfo)
		 (slotReaderName (intern (bformat nil "%s-%s" (symbol-name structName) (symbol-name slotName))))
		 (slotWriterDefineSetfExpander `(define-setf-slot-value-expander ,slotReaderName ,slotName))
		 (slotReaderMethod `(defMethod ,slotReaderName ,structName (self) (slot-value self ,slotName)))
		 )
	    (setq slotNames (Cons slotName slotNames))
	    (setq slotReaderNames (Cons slotReaderName slotReaderNames))
	    (setq slotReaderMethods (Cons slotReaderMethod slotReaderMethods))
	    (setq slotWriters (Cons slotWriterDefineSetfExpander slotWriters)))
	  (raise (LispError "Add support for defstruct slot options"))))
    (let ((objname (gensym)))
      (setq maker `(defun ,(intern (bformat nil "make-%s" (symbol-name structName))) (&key ,@slotNames)
		     (let ((,objname (,structName)))
		       ,@(defstruct-expand-initializers objname slotNames)
		     ,objname))))
    (log (% "\n"))
    (log (% "maker = %s\n" maker))
    (log (% "structName = %s\n" structName))
    (log (% "slotNames = %s\n" slotNames))
    (log (% "slotReaderMethods = %s\n" slotReaderMethods))
    (log (% "slotWriters = %s\n" slotWriters))
    (let ((result `(progn
		     (defClass ,structName () ( ,@slotNames ))
		     ,maker 
		     ,@slotReaderMethods
		     ,@slotWriters )))
      (log (% "result = %s\n" result))
      result)))
     
