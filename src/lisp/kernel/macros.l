

(defmacro prog1 (form1 &body forms)
  (let ((res (gensym)))
    `(let ((,res ,form1))
       ,@forms
       ,res)))


(defmacro prog2 (form1 form2 &body forms)
  (let ((res (gensym)))
    `(progn
       ,form1
       (prog1
	   ,form2
	 ,@forms))))


(defmacro with-open-stream ((var stream) &body forms)
  (let ((abortp (gensym)))
    `(let ((,var ,stream)
	   (,abortp t))
       (unwind-protect
	    (multiple-value-prog1
		(progn ,@forms)
              (setq ,abortp nil))
	 (when ,var
	   (close ,var :abort ,abortp))))))


(defmacro with-open-file ((stream filespec &rest options)
                                    &body body)
  `(with-open-stream (,stream (open ,filespec ,@options))
     ,@body))


(defmacro with-output-to-string ((stream)
                                    &body body)
  `(with-open-stream (,stream (StringOutStream))
     ,@body
     (core::str ,stream)
     )
  )					







(defun defstruct-expand-initializers (objName slotNames)
  (let (initializers)
    (dolist (slotName slotNames)
      (setq initializers (Cons `(setf (slot ,objName ,slotName) ,slotName) initializers))
      )
    initializers))


(defun defstruct-expand-copiers (destName slotNames sourceName)
  (let (copiers)
    (dolist (slotName slotNames)
      (setq copiers (Cons `(setf (slot ,destName ,slotName) (slot ,sourceName ,slotName)) copiers))
      )
    copiers))






(defmacro defstruct (name-and-options &optional &rest slotSetup)
  (let ((documentation ""))
    ;; Eat documentation
    (when (isA (car slotSetup) String)
      (setq documentation (car slotSetup))
      (setq slotSetup (cdr slotSetup)))
  
    (log (% "defstruct name-and-options[%s] slotSetup[%s] (length slotSetup)==>%d\n"
	    name-and-options slotSetup (length slotSetup)))
    (let (structName maker copier
		     slotNames
		     slotReaderMethods slotReaderNames slotWriters
		     slotInitializers)
      (if (symbolp name-and-options)
	  (progn
	    (setq structName name-and-options))
	  (raise (LispError "Add support for defstruct options")))
      (dolist (slotDescription slotSetup)
	(log (% "Processing slotDescription=%s\n" slotDescription))
	(let ((slotInfo slotDescription))
	  ;; If slotInfo is a symbol then convert slotInfo to (slotInfo ())
	  (when (symbolp slotInfo)
	    (setq slotInfo (list slotInfo ())))
	  (let* ((slotName (car slotInfo))
		 (slotInitForm (cadr slotInfo))
		 (slotReaderName (intern (bformat nil "%s-%s" (symbol-name structName) (symbol-name slotName))))
		 (slotWriterDefineSetfExpander `(define-setf-slot-value-expander ,slotReaderName ,slotName))
		 (slotReaderMethod `(defMethod ,slotReaderName ,structName (self) (slot-value self ,slotName)))
		 )
	    (setq slotNames (Cons slotName slotNames))
	    (setq slotInitializers (Cons (list slotName slotInitForm) slotInitializers))
	    (setq slotReaderNames (Cons slotReaderName slotReaderNames))
	    (setq slotReaderMethods (Cons slotReaderMethod slotReaderMethods))
	    (setq slotWriters (Cons slotWriterDefineSetfExpander slotWriters)))
	  ))
      ;; Setup the maker and the copier
      (let ((objname (gensym))
	    (makerName (intern (bformat nil "make-%s" (symbol-name structName))))
	    (copierName (intern (bformat nil "copy-%s" (symbol-name structName))))
	    )
	(setq maker `(defun ,makerName (&key ,@slotInitializers)
		       (let ((,objname (,structName)))
			 ,@(defstruct-expand-initializers objname slotNames)
			 ,objname)))
	(setq copier `(defun ,copierName (other)
			(let ((,objname (,structName)))
			  ,@(defstruct-expand-copiers objname slotNames 'other)
			  ,objname)))
	)
      (log (% "\n"))
      (log (% "maker = %s\n" maker))
      (log (% "copier = %s\n" copier ))
      (log (% "structName = %s\n" structName))
      (log (% "slotNames = %s\n" slotNames))
      (log (% "slotReaderMethods = %s\n" slotReaderMethods))
      (log (% "slotWriters = %s\n" slotWriters))
      (let ((result `(progn
		       (defClass ,structName () ( ,@slotNames ))
		       ,maker 
		       ,copier
		       ,@slotReaderMethods
		       ,@slotWriters
		       )))
	(log (% "result = %s\n" result))
	result))
    )
)
     




(export '(prog1 prog2 with-open-stream with-open-file with-output-to-string defstruct))


(defmacro with-input-from-string ((stream string &key index start end)
                                    &body body)
  (assert (not index) "I can't handle index yet")
  (assert (not (or start end)) "I can't handle start/end yet")
  `(with-open-stream (,stream (StringInStream ,string))
     ,@body
     )
  )
(export 'with-input-from-string)





(defmacro incf ( place &optional (delta-form 1) )
  `(setf ,place (+ ,place ,delta-form)))

(defmacro decf ( place &optional (delta-form 1) )
  `(setf ,place (- ,place ,delta-form)))

(export '(incf decf))

