;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; clasp - changes approved May1 2013

;;#-clasp
(defpackage "CLOS"
  (:use "CL" "EXT")
  (:import-from "SI" "UNBOUND" "GET-SYSPROP" "PUT-SYSPROP" "REM-SYSPROP"
		"SIMPLE-PROGRAM-ERROR"))


#+clasp (in-package "CLOS")
#+clasp (use-package '(:CORE :ext) :clos)
#+clasp (import '(unbound get-sysprop put-sysprop rem-sysprop simple-program-error
		 slot-descriptions
		 SLOT-NAMES SLOT-NAME CLASS-PRECEDENCE-LIST PRINT-FUNCTION
		 CONSTRUCTORS COUNT-FUNCTION-CONTAINER-ENVIRONMENTS
		 SETF-FIND-CLASS ALLOCATE-RAW-CLASS SPECIALIZER
		 FORWARD-REFERENCED-CLASS METAOBJECT STD-CLASS ) :si)




#+compare (print "MLOG ********* Starting package.lsp **********")
#+clasp
(defmacro clos-log (fmt &rest args)
  `(bformat t ,fmt ,@args))

#+clasp
(export '(WITH-SLOTS WITH-ACCESSORS UPDATE-INSTANCE-FOR-REDEFINED-CLASS
	  UPDATE-INSTANCE-FOR-DIFFERENT-CLASS STANDARD-METHOD
	  STANDARD SLOT-UNBOUND SLOT-MISSING SLOT-MAKUNBOUND
	  SLOT-EXISTS-P SLOT-BOUNDP SHARED-INITIALIZE REMOVE-METHOD
	  REINITIALIZE-INSTANCE NO-NEXT-METHOD METHOD-QUALIFIERS
	  METHOD-COMBINATION-ERROR METHOD-COMBINATION MAKE-METHOD
	  MAKE-LOAD-FORM-SAVING-SLOTS MAKE-LOAD-FORM MAKE-INSTANCES-OBSOLETE
	  INVALID-METHOD-ERROR INITIALIZE-INSTANCE FUNCTION-KEYWORDS
	  FIND-METHOD ENSURE-GENERIC-FUNCTION DESCRIBE-OBJECT
	  CHANGE-CLASS CALL-METHOD ALLOCATE-INSTANCE
	  ADD-METHOD ))


