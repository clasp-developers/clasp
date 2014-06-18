;;; -*- Mode: Lisp; Package: Xlib; Log: clx.log -*-

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 2909
;;;			       AUSTIN, TEXAS 78769
;;;
;;; Portions Copyright (C) 1987 Texas Instruments Incorporated.
;;; Portions Copyright (C) 1988, 1989 Franz Inc, Berkeley, Ca.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;
;;; Franz Incorporated provides this software "as is" without express or
;;; implied warranty.

;;; #+ features used in this file
;;;   clx-ansi-common-lisp
;;;   lispm
;;;   genera
;;;   minima
;;;   lucid
;;;   lcl3.0
;;;   apollo
;;;   kcl
;;;   ibcl
;;;   excl
;;;   CMU
;;;   sbcl

#+(or Genera Minima sbcl ecl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (common-lisp:pushnew :clx-ansi-common-lisp common-lisp:*features*))

#+(and Genera clx-ansi-common-lisp)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* si:*ansi-common-lisp-readtable*))

#-(or clx-ansi-common-lisp cmu)
(lisp:in-package :user)

#+cmu
(lisp:in-package "XLIB")
#+cmu 
(export 'load-clx)

#+clx-ansi-common-lisp
(common-lisp:in-package :common-lisp-user)


;;;; Lisp Machines

#+(and lispm (not genera))
(global:defsystem CLX
  (:pathname-default "clx:clx;")
  (:patchable "clx:patch;" clx-ti)
  (:initial-status :experimental)

  (:module package "package")
  (:module depdefs "depdefs")
  (:module clx "clx")
  (:module dependent "dependent")
  (:module macros "macros")
  (:module bufmac "bufmac")
  (:module buffer "buffer")
  (:module display "display")
  (:module gcontext "gcontext")
  (:module requests "requests")
  (:module input "input")
  (:module fonts "fonts")
  (:module graphics "graphics")
  (:module text "text")
  (:module attributes "attributes")
  (:module translate "translate")
  (:module keysyms "keysyms")
  (:module manager "manager")
  (:module image "image")
  (:module resource "resource")
  (:module doc "doc")

  (:compile-load package)
  (:compile-load depdefs
   (:fasload package))
  (:compile-load clx
   (:fasload package depdefs))
  (:compile-load dependent
   (:fasload package depdefs clx))
  ;; Macros only needed for compilation
  (:skip :compile-load macros
   (:fasload package depdefs clx dependent))
  ;; Bufmac only needed for compilation
  (:skip :compile-load bufmac
   (:fasload package depdefs clx dependent macros))
  (:compile-load buffer
   (:fasload package depdefs clx dependent macros bufmac))
  (:compile-load display
   (:fasload package depdefs clx dependent macros bufmac buffer))
  (:compile-load gcontext
   (:fasload package depdefs clx dependent macros bufmac buffer display))
  (:compile-load input
   (:fasload package depdefs clx dependent macros bufmac buffer display))
  (:compile-load requests
   (:fasload package depdefs clx dependent macros bufmac buffer display input))
  (:compile-load fonts
   (:fasload package depdefs clx dependent macros bufmac buffer display))
  (:compile-load graphics
   (:fasload package depdefs clx dependent macros fonts bufmac buffer display
	     fonts))
  (:compile-load text
   (:fasload package depdefs clx dependent macros fonts bufmac buffer display
	     gcontext fonts))
  (:compile-load-init attributes
   (dependent)
   (:fasload package depdefs clx dependent macros bufmac buffer display))
  (:compile-load translate
   (:fasload package depdefs clx dependent macros bufmac buffer display))
  (:compile-load keysyms
   (:fasload package depdefs clx dependent macros bufmac buffer display
	     translate))
  (:compile-load manager
   (:fasload package depdefs clx dependent macros bufmac buffer display))
  (:compile-load image
   (:fasload package depdefs clx dependent macros bufmac buffer display))
  (:compile-load resource
   (:fasload package depdefs clx dependent macros bufmac buffer display))
  (:auxiliary doc)
  )


;;; Symbolics Lisp Machines
#+Genera
(scl:defsystem CLX
    (:default-pathname "SYS:X11;CLX;"
     :pretty-name "CLX"
     :maintaining-sites (:scrc)
     :distribute-sources t
     :distribute-binaries t
     :source-category :basic)
  (:module doc ("doc")
	   (:type :lisp-example))
  (:serial
    "package" "depdefs" "generalock" "clx" "dependent" "macros" "bufmac"
    "buffer" "display" "gcontext" "input" "requests" "fonts" "graphics"
    "text" "attributes" "translate" "keysyms" "manager" "image" "resource"))

#+Minima
(zl:::scl:defsystem Minima-CLX
    (:default-pathname "SYS:X11;CLX;"
     :pretty-name "Minima CLX"
     :maintain-journals nil
     :maintaining-sites (:scrc)
     :distribute-sources t
     :distribute-binaries t
     :source-category :basic
     :default-module-type :minima-lisp)
  (:module doc ("doc")
	   (:type :lisp-example))
  (:serial
    "package" "depdefs" "clx" "dependent" "macros" "bufmac"
    "buffer" "display" "gcontext" "input" "requests" "fonts" "graphics"
    "text" "attributes" "translate" "keysyms" "manager" "image" "resource"))


;;; Franz

;;
;; The following is a suggestion.  If you comment out this form be
;; prepared for possible deadlock, since no interrupts will be recognized
;; while reading from the X socket if the scheduler is not running.
;;
#+excl
(setq compiler::generate-interrupt-checks-switch
      (compile nil
	       '(lambda (safety size speed &optional debug)
		  (declare (ignore size debug))
		  (or (< speed 3) (> safety 0)))))


;;; Allegro

#+allegro
(excl:defsystem :clx 
  ()
  |package|
  (|excldep|
    :load-before-compile (|package|)
    :recompile-on (|package|))
  (|depdefs|
    :load-before-compile (|package| |excldep|)
    :recompile-on (|excldep|))
  (|clx|
    :load-before-compile (|package| |excldep| |depdefs|)
    :recompile-on (|package| |excldep| |depdefs|))
  (|dependent|
    :load-before-compile (|package| |excldep| |depdefs| |clx|)
    :recompile-on (|clx|))
  (|exclcmac|
    :load-before-compile (|package| |excldep| |depdefs| |clx| |dependent|)
    :recompile-on (|dependent|))
  (|macros|
    :load-before-compile (|package| |excldep| |depdefs| |clx| |dependent|
			  |exclcmac|)
    :recompile-on (|exclcmac|))
  (|bufmac|
    :load-before-compile (|package| |excldep| |depdefs| |clx| |dependent|
			  |exclcmac| |macros|)
    :recompile-on (|macros|))
  (|buffer|
    :load-before-compile (|package| |excldep| |depdefs| |clx| |dependent|
			  |exclcmac| |macros| |bufmac|)
    :recompile-on (|bufmac|))
  (|display|
    :load-before-compile (|package| |excldep| |depdefs| |clx| |dependent|
			  |exclcmac| |macros| |bufmac| |buffer|)
    :recompile-on (|buffer|))
  (|gcontext|
    :load-before-compile (|package| |excldep| |depdefs| |clx| |dependent|
			  |exclcmac| |macros| |bufmac| |buffer| |display|)
    :recompile-on (|display|))
  (|input|
    :load-before-compile (|package| |excldep| |depdefs| |clx| |dependent|
			  |exclcmac| |macros| |bufmac| |buffer| |display|)
    :recompile-on (|display|))
  (|requests|
    :load-before-compile (|package| |excldep| |depdefs| |clx| |dependent|
			  |exclcmac| |macros| |bufmac| |buffer| |display|
			  |input|)
    :recompile-on (|display|))
  (|fonts|
    :load-before-compile (|package| |excldep| |depdefs| |clx| |dependent|
			  |exclcmac| |macros| |bufmac| |buffer| |display|)
    :recompile-on (|display|))
  (|graphics|
    :load-before-compile (|package| |excldep| |depdefs| |clx| |dependent|
			  |exclcmac| |macros| |bufmac| |buffer| |display|
			  |fonts|)
    :recompile-on (|fonts|))
  (|text|
    :load-before-compile (|package| |excldep| |depdefs| |clx| |dependent|
			  |exclcmac| |macros| |bufmac| |buffer| |display|
			  |gcontext| |fonts|)
    :recompile-on (|gcontext| |fonts|)
    :load-after (|translate|))
  ;; The above line gets around a compiler macro expansion bug.
  
  (|attributes|
    :load-before-compile (|package| |excldep| |depdefs| |clx| |dependent|
			  |exclcmac| |macros| |bufmac| |buffer| |display|)
    :recompile-on (|display|))
  (|translate|
    :load-before-compile (|package| |excldep| |depdefs| |clx| |dependent|
			  |exclcmac| |macros| |bufmac| |buffer| |display|
			  |text|)
    :recompile-on (|display|))
  (|keysyms|
    :load-before-compile (|package| |excldep| |depdefs| |clx| |dependent|
			  |exclcmac| |macros| |bufmac| |buffer| |display|
			  |translate|)
    :recompile-on (|translate|))
  (|manager|
    :load-before-compile (|package| |excldep| |depdefs| |clx| |dependent|
			  |exclcmac| |macros| |bufmac| |buffer| |display|)
    :recompile-on (|display|))
  (|image|
    :load-before-compile (|package| |excldep| |depdefs| |clx| |dependent|
			  |exclcmac| |macros| |bufmac| |buffer| |display|)
    :recompile-on (|display|))
  
  ;; Don't know if l-b-c list is correct.  XX
  (|resource|
    :load-before-compile (|package| |excldep| |depdefs| |clx| |dependent|
			  |exclcmac| |macros| |bufmac| |buffer| |display|)
    :recompile-on (|display|))
  )

#+allegro
(excl:defsystem :clx-debug
    (:default-pathname "debug/"
     :needed-systems (:clx)
     :load-before-compile (:clx))
  |describe| |keytrans| |trace| |util|)


;;;; Compile CLX

;;; COMPILE-CLX compiles the lisp source files and loads the binaries.
;;; It goes to some trouble to let the source files be in one directory
;;; and the binary files in another.  Thus the same set of sources can
;;; be used for different machines and/or lisp systems.  It also allows
;;; you to supply explicit extensions, so source files do not have to
;;; be renamed to fit into the naming conventions of an implementation.

;;; For example,
;;;     (compile-clx "*.lisp" "machine/")
;;; compiles source files from the connected directory and puts them
;;; into the "machine" subdirectory.  You can then load CLX out of the
;;; machine directory.

;;; The code has no knowledge of the source file types (eg, ".l" or
;;; ".lisp") or of the binary file types (eg, ".b" or ".sbin").  Calling
;;; compile-file and load with a file type of NIL usually sorts things
;;; out correctly, but you may have to explicitly give the source and
;;; binary file types.

;;; An attempt at compiling the C language sources is also made,
;;; but you may have to set different compiler switches
;;; should be.  If it doesn't do the right thing, then do
;;;     (compile-clx "" "" :compile-c NIL)
;;; to prevent the compilation.

;;; compilation notes
;;;   lucid2.0/hp9000s300
;;;     must uudecode the file make-sequence-patch.uu

#+(or lucid kcl ibcl cmu)
(defun clx-foreign-files (binary-path)

  #+(and lucid (not lcl3.0) (or mc68000 mc68020))
  (load (merge-pathnames "make-sequence-patch" binary-path))

  #+(and lucid apollo)
  (lucid::load-foreign-file
    (namestring (merge-pathnames "socket" binary-path))
    :preserve-pathname t)

  #+(and lucid (not apollo))
  (lucid::load-foreign-files
    (list (namestring (merge-pathnames "socket.o" binary-path)))
    '("-lc"))

  #+cmu
  (declare (ignore binary-path))
  #+(or cmu sbcl)
  (alien:def-alien-routine ("connect_to_server" xlib::connect-to-server)
			   c-call:int
    (host c-call:c-string)
    (port c-call:int))

  #+(or kcl ibcl)
  (progn
    (let ((pathname (merge-pathnames "sockcl.o" binary-path))
	  (options
	    (concatenate
	      'string
	      (namestring (merge-pathnames "socket.o" binary-path))
	      " -lc")))
      (format t "~&Faslinking ~A with ~A.~%" pathname options)
      (si:faslink (namestring pathname) options)
      (format t "~&Finished faslinking ~A.~%" pathname)))
  )

#-(or lispm allegro Minima)
(defun compile-clx (&optional
		    (source-pathname-defaults "")
		    (binary-pathname-defaults "")
		    &key
		    (compile-c t))

  ;; The pathname-defaults above might only be strings, so coerce them
  ;; to pathnames.  Build a default binary path with every component
  ;; of the source except the file type.  This should prevent
  ;; (compile-clx "*.lisp") from destroying source files.
  (let* ((source-path (pathname source-pathname-defaults))
	 (path        (make-pathname
			:host      (pathname-host      source-path)
			:device    (pathname-device    source-path)
			:directory (pathname-directory source-path)
			:name      (pathname-name      source-path)
			:type      nil
			:version   (pathname-version   source-path)))
	 (binary-path (merge-pathnames binary-pathname-defaults
				       path))
	 #+clx-ansi-common-lisp (*compile-verbose* t)
	 (*load-verbose* t))
				       
    ;; Make sure source-path and binary-path file types are distinct so
    ;; we don't accidently overwrite the source files.  NIL should be an
    ;; ok type, but anything else spells trouble.
    (if (and (equal (pathname-type source-path)
		    (pathname-type binary-path))
	     (not (null (pathname-type binary-path))))
	(error "Source and binary pathname defaults have same type ~s ~s"
	       source-path binary-path))

    (format t "~&;;; Default paths: ~s ~s~%" source-path binary-path)

    ;; In lucid make sure we're using the compiler in production mode.
    #+lcl3.0
    (progn
      (unless (member :pqc *features*)
	(cerror
	  "Go ahead anyway."
	  "Lucid's production mode compiler must be loaded to compile CLX."))
      (proclaim '(optimize (speed 3)
			   (safety 1)
			   (space 0)
			   (compilation-speed 0))))

    (labels ((compile-lisp (filename)
	       (let ((source (merge-pathnames filename source-path))
		     (binary (merge-pathnames filename binary-path)))
		 ;; If the source and binary pathnames are the same,
		 ;; then don't supply an output file just to be sure
		 ;; compile-file defaults correctly.
		 #+(or kcl ibcl) (load source)
		 (if (equal source binary)
		     (compile-file source)
		     (compile-file source :output-file binary))
		 binary))
	     (compile-and-load (filename)
	       (load (compile-lisp filename)))
	     #+(or lucid kcl ibcl)
	     (compile-c (filename)
	       (let* ((c-filename (concatenate 'string filename ".c"))
		      (o-filename (concatenate 'string filename ".o"))
		      (src (merge-pathnames c-filename source-path))
		      (obj  (merge-pathnames o-filename binary-path))
		      (args (list "-c" (namestring src)
				  "-o" (namestring obj)
				  #+mips "-G 0"
				  #+(or hp sysv) "-DSYSV"
				  #+(and mips (not dec)) "-I/usr/include/bsd"
				  #-(and mips (not dec)) "-DUNIXCONN"
				  #+(and lucid pa) "-DHPUX -DHPUX7.0"
				  )))
		 (format t ";;; cc~{ ~A~}~%" args)
		 (unless
		   (zerop 
		     #+lucid
		     (multiple-value-bind (iostream estream exitstatus pid)
			 ;; in 2.0, run-program is exported from system:
			 ;; in 3.0, run-program is exported from lcl:
			 ;; system inheirits lcl
			 (system::run-program "cc" :arguments args)
		       (declare (ignore iostream estream pid))
		       exitstatus)
		     #+(or kcl ibcl)
		     (system (format nil "cc~{ ~A~}" args)))
		   (error "Compile of ~A failed." src)))))

      ;; Now compile and load all the files.
      ;; Defer compiler warnings until everything's compiled, if possible.
      (#+(or clx-ansi-common-lisp CMU) with-compilation-unit
       #+lcl3.0 lucid::with-deferred-warnings
       #-(or lcl3.0 clx-ansi-common-lisp CMU) progn
       ()
       
       (compile-and-load "package")
       #+(or lucid kcl ibcl) (when compile-c (compile-c "socket"))
       #+(or kcl ibcl) (compile-lisp "sockcl")
       #+(or lucid kcl ibcl) (clx-foreign-files binary-path)
       #+excl (compile-and-load "excldep")
       (compile-and-load "depdefs")
       (compile-and-load "clx")
       (compile-and-load "dependent")
       #+excl (compile-and-load "exclcmac")	; these are just macros
       (compile-and-load "macros")		; these are just macros
       (compile-and-load "bufmac")		; these are just macros
       (compile-and-load "buffer")
       (compile-and-load "display")
       (compile-and-load "gcontext")
       (compile-and-load "input")
       (compile-and-load "requests")
       (compile-and-load "fonts")
       (compile-and-load "graphics")
       (compile-and-load "text")
       (compile-and-load "attributes")
       (compile-and-load "translate")
       (compile-and-load "keysyms")
       (compile-and-load "manager")
       (compile-and-load "image")
       (compile-and-load "resource")
       ))))


;;;; Load CLX

;;; This procedure loads the binaries for CLX.  All of the binaries
;;; should be in the same directory, so setting the default pathname
;;; should point load to the right place.

;;; You should have a module definition somewhere so the require/provide
;;; mechanism can avoid reloading CLX.  In an ideal world, somebody would
;;; just put
;;;		(REQUIRE 'CLX)
;;; in their file (some implementations don't have a central registry for
;;; modules, so a pathname needs to be supplied).

;;; The REQUIRE should find a file that does
;;;		(IN-PACKAGE 'XLIB :USE '(LISP))
;;;		(PROVIDE 'CLX)
;;;		(LOAD <clx-defsystem-file>)
;;;		(LOAD-CLX <binary-specific-clx-directory>)

#-(or lispm allegro Minima)
(defun load-clx (&optional (binary-pathname-defaults "")
		 &key (macrosp nil))

  (let* ((source-path (pathname ""))
	 (path        (make-pathname
			:host      (pathname-host      source-path)
			:device    (pathname-device    source-path)
			:directory (pathname-directory source-path)
			:name      (pathname-name      source-path)
			:type      nil
			:version   (pathname-version   source-path)))
	 (binary-path (merge-pathnames binary-pathname-defaults
				       path))
	 (*load-verbose* t))

    (flet ((load-binary (filename)
	     (let ((binary (merge-pathnames filename binary-path)))
	       (load binary))))

      (load-binary "package")
      #+(or lucid kcl ibcl cmu) (clx-foreign-files binary-path)
      #+excl (load-binary "excldep")
      (load-binary "depdefs")
      (load-binary "clx")
      (load-binary "dependent")
      (when macrosp
	#+excl (load-binary "exclcmac")
	(load-binary "macros")
	(load-binary "bufmac"))
      (load-binary "buffer")
      (load-binary "display")
      (load-binary "gcontext")
      (load-binary "input")
      (load-binary "requests")
      (load-binary "fonts")
      (load-binary "graphics")
      (load-binary "text")
      (load-binary "attributes")
      (load-binary "translate")
      (load-binary "keysyms")
      (load-binary "manager")
      (load-binary "image")
      (load-binary "resource")
      )))

;;;
;;; ECL likes to combine several files into a single dynamically loadable
;;; library.
;;;
#+ecl
(defconstant +clx-modules+
  '("package" "depdefs" "clx" "dependent" "macros" "bufmac" "buffer"
    "display" "gcontext" "input" "requests" "fonts" "graphics" "text"
    "attributes" "translate" "keysyms" "manager" "image" "resource"))

#+(or) ;ecl
(flet ((compile-if-old (destdir sources &rest options)
	 (mapcar #'(lambda (source)
		     (let ((object (merge-pathnames destdir (compile-file-pathname source :type :object))))
		       (unless (and (probe-file object)
				    (>= (file-write-date object) (file-write-date source)))
			 (apply #'compile-file source :output-file object options))
		       object))
		 sources)))
  (let ((clx-objects (compile-if-old "./" +clx-modules+ :system-p t)))
    (c::build-fasl "clx" :lisp-files clx-objects)))

(mapcar #'load +clx-modules+)
