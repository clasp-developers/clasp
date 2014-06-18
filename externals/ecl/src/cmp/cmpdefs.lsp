;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPDEF -- Definitions created at compile / configuration time

(in-package "COMPILER")

;;; This is copied into each .h file generated, EXCEPT for system-p calls.
;;; The constant string *include-string* is the content of file "ecl.h".
;;; Here we use just a placeholder: it will be replaced with sed.
(defvar *cmpinclude* "<ecl/ecl-cmp.h>")

(defvar *cc* "@ECL_CC@"
"This variable controls how the C compiler is invoked by ECL.
The default value is \"cc -I. -I/usr/local/include/\".
The second -I option names the directory where the file ECL.h has been installed.
One can set the variable appropriately adding for instance flags which the 
C compiler may need to exploit special hardware features (e.g. a floating point
coprocessor).")

(defvar *ld* "@ECL_CC@"
"This variable controls the linker which is used by ECL.")

(defvar *ranlib* "@RANLIB@"
  "Name of the `ranlib' program on the hosting platform.")

(defvar *ar* "@AR@"
  "Name of the `AR' program on the hosting platform.")

(defvar *cc-flags* "@CPPFLAGS@ @CFLAGS@ @ECL_CFLAGS@")

(defvar *cc-optimize* #-msvc "-O2"
                      #+msvc "@CFLAGS_OPTIMIZE@")

(defvar *ld-format* #-msvc "~A -o ~S -L~S ~{~S ~} ~@[~S~]~{ '~A'~} ~A"
                    #+msvc "~A -Fe~S~* ~{~S ~} ~@[~S~]~{ '~A'~} ~A")

(defvar *cc-format* #-msvc "~A -I. \"-I~A\" ~A ~:[~*~;~A~] -w -c \"~A\" -o \"~A\"~{ '~A'~}"
                    #+msvc "~A -I. -I\"~A\" ~A ~:[~*~;~A~] -w -c \"~A\" -Fo\"~A\"~{ '~A'~}")

#-dlopen
(defvar *ld-flags* "@LDFLAGS@ -lecl @CORE_LIBS@ @FASL_LIBS@ @LIBS@")
#+dlopen
(defvar *ld-flags* #-msvc "@LDFLAGS@ -lecl @FASL_LIBS@ @LIBS@"
                   #+msvc "@LDFLAGS@ ecl.lib @CLIBS@")
#+dlopen
(defvar *ld-shared-flags* #-msvc "@SHARED_LDFLAGS@ @LDFLAGS@ -lecl @FASL_LIBS@ @LIBS@"
                          #+msvc "@SHARED_LDFLAGS@ @LDFLAGS@ ecl.lib @CLIBS@")
#+dlopen
(defvar *ld-bundle-flags* #-msvc "@BUNDLE_LDFLAGS@ @LDFLAGS@ -lecl @FASL_LIBS@ @LIBS@"
                          #+msvc "@BUNDLE_LDFLAGS@ @LDFLAGS@ ecl.lib @CLIBS@")

(defvar +shared-library-prefix+ "@SHAREDPREFIX@")
(defvar +shared-library-extension+ "@SHAREDEXT@")
(defvar +shared-library-format+ "@SHAREDPREFIX@~a.@SHAREDEXT@")
(defvar +static-library-prefix+ "@LIBPREFIX@")
(defvar +static-library-extension+ "@LIBEXT@")
(defvar +static-library-format+ "@LIBPREFIX@~a.@LIBEXT@")
(defvar +object-file-extension+ "@OBJEXT@")
(defvar +executable-file-format+ "~a@EXEEXT@")

(defvar *ecl-include-directory* @includedir\@)
(defvar *ecl-library-directory* @libdir\@)

(defvar *ld-rpath*
  (let ((x "@ECL_LDRPATH@"))
    (and (plusp (length x))
         (format nil x *ecl-library-directory*))))
