;;; -*- Lisp -*- mode

;;; Original copyright message from defsystem.lisp:

;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 2909
;;;			       AUSTIN, TEXAS 78769
;;;
;;; Portions Copyright (C) 1987 Texas Instruments Incorporated.
;;; Portions Copyright (C) 1988, 1989 Franz Inc, Berkeley, Ca.
;;;
;;; Permission is granted to any individual or institution to use,
;;; copy, modify, and distribute this software, provided that this
;;; complete copyright and permission notice is maintained, intact, in
;;; all copies and supporting documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is"
;;; without express or implied warranty.
;;;
;;; Franz Incorporated provides this software "as is" without express
;;; or implied warranty.

(defpackage :clx-system (:use :cl :asdf))
(in-package :clx-system)  

(pushnew :clx-ansi-common-lisp *features*)

(defclass clx-source-file (cl-source-file) ())
(defclass xrender-source-file (clx-source-file) ())

;;; CL-SOURCE-FILE, not CLX-SOURCE-FILE, so that we're not accused of
;;; cheating by rebinding *DERIVE-FUNCTION-TYPES* :-)
(defclass example-source-file (cl-source-file) ())

(defclass legacy-file (static-file) ())

(defsystem CLX
    :depends-on (#+sbcl sb-bsd-sockets)
    :version "0.7.2"
    :serial t
    :default-component-class clx-source-file
    :components
    ((:file "package")
     (:file "depdefs")
     (:file "clx")
     #-(or openmcl allegro) (:file "dependent")
     #+openmcl (:file "dep-openmcl")
     #+allegro (:file "dep-allegro")
     (:file "macros")
     (:file "bufmac")
     (:file "buffer")
     (:file "display")
     (:file "gcontext")
     (:file "input")
     (:file "requests")
     (:file "fonts")
     (:file "graphics")
     (:file "text")
     (:file "attributes")
     (:file "translate")
     (:file "keysyms")
     (:file "manager")
     (:file "image")
     (:file "resource")
     #+allegro
     (:file "excldep" :pathname "excldep.lisp")
     (:module extensions
	      :pathname #.(make-pathname :directory '(:relative))
	      :components
	      ((:file "shape")
	       (:file "big-requests")
	       (:file "xvidmode")
	       (:xrender-source-file "xrender")
               (:file "glx")
               (:file "gl" :depends-on ("glx"))
	       (:file "dpms")
               (:file "xtest")
               (:file "screensaver")
               (:file "xinerama")))
     (:module demo
	      :default-component-class example-source-file
	      :components
	      ((:file "bezier")
	       ;; KLUDGE: this requires "bezier" for proper operation,
	       ;; but we don't declare that dependency here, because
	       ;; asdf doesn't load example files anyway.
	       (:file "beziertest")
	       (:file "clclock")
               (:file "clipboard")
	       (:file "clx-demos")
	       (:file "gl-test")
	       ;; FIXME: compiling this generates 30-odd spurious code
	       ;; deletion notes.  Find out why, and either fix or
	       ;; workaround the problem.
	       (:file "mandel")
	       (:file "menu")
	       (:file "zoid")))
     (:module test
	      :default-component-class example-source-file
	      :components
	      ((:file "image")
	       ;; KLUDGE: again, this depends on "zoid"
	       (:file "trapezoid")))
     (:static-file "NEWS")
     (:static-file "CHANGES")
     (:static-file "README")
     (:static-file "README-R5")
     (:legacy-file "exclMakefile")
     (:legacy-file "exclREADME")
     (:legacy-file "exclcmac" :pathname "exclcmac.lisp")
     (:legacy-file "excldepc" :pathname "excldep.c")
     (:legacy-file "sockcl" :pathname "sockcl.lisp")
     (:legacy-file "socket" :pathname "socket.c")
     (:legacy-file "defsystem" :pathname "defsystem.lisp")
     (:legacy-file "provide" :pathname "provide.lisp")
     (:legacy-file "cmudep" :pathname "cmudep.lisp")
     (:module manual
	      ;; TODO: teach asdf how to process texinfo files
	      :components ((:static-file "clx.texinfo")))
     (:module debug
	      :default-component-class legacy-file
	      :components
	      ((:file "debug" :pathname "debug.lisp")
	       (:file "describe" :pathname "describe.lisp")
	       (:file "event-test" :pathname "event-test.lisp")
	       (:file "keytrans" :pathname "keytrans.lisp")
	       (:file "trace" :pathname "trace.lisp")
	       (:file "util" :pathname "util.lisp")))))

(defmethod perform ((o load-op) (f example-source-file))
  ;; do nothing.  We want to compile them when CLX is compiled, but
  ;; not load them when CLX is loaded.
  t)

#+sbcl
(defmethod perform :around ((o compile-op) (f xrender-source-file))
  ;; RENDER would appear to be an inherently slow protocol; further,
  ;; it's not set in stone, and consequently we care less about speed
  ;; than we do about correctness.
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (call-next-method)))

#+sbcl
(defmethod perform :around ((o compile-op) (f clx-source-file))
  ;; our CLX library should compile without WARNINGs, and ideally
  ;; without STYLE-WARNINGs.  Since it currently does, let's enforce
  ;; it here so that we can catch regressions easily.
  (let ((on-warnings (operation-on-warnings o))
	(on-failure (operation-on-failure o)))
    (unwind-protect
	 (progn
	   (setf (operation-on-warnings o) :error
		 (operation-on-failure o) :error)
	   ;; a variety of accessors, such as AREF-CARD32, are not
	   ;; declared INLINE.  Without this (non-ANSI)
	   ;; static-type-inference behaviour, SBCL emits an extra 100
	   ;; optimization notes (roughly one fifth of all of the
	   ;; notes emitted).  Since the internals are unlikely to
	   ;; change much, and certainly the internals should stay in
	   ;; sync, enabling this extension is a win.  (Note that the
	   ;; use of this does not imply that applications using CLX
	   ;; calls that expand into calls to these accessors will be
	   ;; optimized in the same way).
	   (let ((sb-ext:*derive-function-types* t)
                 (sadx (find-symbol "STACK-ALLOCATE-DYNAMIC-EXTENT" :sb-c))
                 (sadx-var (find-symbol "*STACK-ALLOCATE-DYNAMIC-EXTENT*" :sb-ext)))
	     ;; deeply unportable stuff, this.  I will be shot.  We
	     ;; want to enable the dynamic-extent declarations in CLX.
	     (when (and sadx (sb-c::policy-quality-name-p sadx))
	       ;; no way of setting it back short of yet more yukky stuff
	       (proclaim `(optimize (,sadx 3))))
             (if sadx-var
                 (progv (list sadx-var) (list t)
                   (call-next-method))
                 (call-next-method))))
      (setf (operation-on-warnings o) on-warnings
	    (operation-on-failure o) on-failure))))

#+sbcl
(defmethod perform :around (o (f clx-source-file))
  ;; SBCL signals an error if DEFCONSTANT is asked to redefine a
  ;; constant unEQLly.  For CLX's purposes, however, we are defining
  ;; structured constants (lists and arrays) not for EQLity, but for
  ;; the purposes of constant-folding operations such as (MEMBER FOO
  ;; +BAR+), so it is safe to abort the redefinition provided the
  ;; structured data is sufficiently equal.
  (handler-bind
      ((sb-ext:defconstant-uneql
	   (lambda (c)
	     ;; KLUDGE: this really means "don't warn me about
	     ;; efficiency of generic array access, please"
	     (declare (optimize (sb-ext:inhibit-warnings 3)))
	     (let ((old (sb-ext:defconstant-uneql-old-value c))
		   (new (sb-ext:defconstant-uneql-new-value c)))
	       (typecase old
		 (list (when (equal old new) (abort c)))
		 (string (when (and (typep new 'string)
				    (string= old new))
			   (abort c)))
		 (simple-vector
		  (when (and (typep new 'simple-vector)
			     (= (length old) (length new))
			     (every #'eql old new))
		    (abort c)))
		 (array
		  (when (and (typep new 'array)
			     (equal (array-dimensions old)
				    (array-dimensions new))
			     (equal (array-element-type old)
				    (array-element-type new))
			     (dotimes (i (array-total-size old) t)
			       (unless (eql (row-major-aref old i)
					    (row-major-aref new i))
				 (return nil))))
		    (abort c))))))))
    (call-next-method)))
