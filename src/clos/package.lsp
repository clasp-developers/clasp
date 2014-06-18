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

(defpackage "CLOS"
  (:use "CL" "EXT")
  (:import-from "SI" "UNBOUND" "GET-SYSPROP" "PUT-SYSPROP" "REM-SYSPROP"
		"SIMPLE-PROGRAM-ERROR"))

