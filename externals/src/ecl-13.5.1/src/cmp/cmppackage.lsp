;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2009, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;;  CMPPACKAGE -- Package definitions and exported symbols
;;;;

(ext:package-lock "CL" nil)

(defpackage "C"
  (:nicknames "COMPILER")
  (:use "FFI" "EXT" #+threads "MP" "CL")
  (:export "*COMPILER-BREAK-ENABLE*"
	   "*COMPILE-PRINT*"
	   "*COMPILE-TO-LINKING-CALL*"
	   "*COMPILE-VERBOSE*"
           "*COMPILER-FEATURES*"
	   "*CC*"
	   "*CC-OPTIMIZE*"
           "*USER-CC-FLAGS*"
           "*USER-LD-FLAGS*"
           "*SUPPRESS-COMPILER-NOTES*"
           "*SUPPRESS-COMPILER-WARNINGS*"
           "*SUPPRESS-COMPILER-MESSAGES*"
	   "BUILD-ECL"
	   "BUILD-PROGRAM"
           "BUILD-FASL"
	   "BUILD-STATIC-LIBRARY"
	   "BUILD-SHARED-LIBRARY"
	   "COMPILER-WARNING"
	   "COMPILER-NOTE"
	   "COMPILER-MESSAGE"
	   "COMPILER-ERROR"
	   "COMPILER-FATAL-ERROR"
	   "COMPILER-INTERNAL-ERROR"
	   "COMPILER-UNDEFINED-VARIABLE"
	   "COMPILER-MESSAGE-FILE"
	   "COMPILER-MESSAGE-FILE-POSITION"
	   "COMPILER-MESSAGE-FORM"
	   "*SUPPRESS-COMPILER-WARNINGS*"
	   "*SUPPRESS-COMPILER-NOTES*"
	   "*SUPPRESS-COMPILER-MESSAGES*"
	   "INSTALL-C-COMPILER"
           "UPDATE-COMPILER-FEATURES")
  (:import-from "SI" "GET-SYSPROP" "PUT-SYSPROP" "REM-SYSPROP" "MACRO"
		"*COMPILER-CONSTANTS*" "REGISTER-GLOBAL" "CMP-ENV-REGISTER-MACROLET"
		"COMPILER-LET"))
