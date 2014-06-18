;;; -*- Mode: common-lisp; Package: xlib; Base: 10; Lowercase: Yes -*-
;;;
;;; Copyright (c) 1988, 1989 Franz Inc, Berkeley, Ca.
;;;
;;; Permission is granted to any individual or institution to use, copy,
;;; modify, and distribute this software, provided that this complete
;;; copyright and permission notice is maintained, intact, in all copies and
;;; supporting documentation.
;;;
;;; Franz Incorporated provides this software "as is" without express or
;;; implied warranty.
;;;

(in-package :xlib :use '(:foreign-functions :lisp :excl))

#+allegro
(require :defsystem "defsys")

(eval-when (load)
  (require :clxexcldep "excldep"))

;;
;; The following is a suggestion.  If you comment out this form be
;; prepared for possible deadlock, since no interrupts will be recognized
;; while reading from the X socket if the scheduler is not running.
;;
(setq compiler::generate-interrupt-checks-switch
  (compile nil '(lambda (safety size speed)
		   (declare (ignore size))
		  (or (< speed 3) (> safety 0)))))


#+allegro
(excl:defsystem :clx
    ()
  |depdefs|
  (|clx| :load-before-compile (|depdefs|)
	 :recompile-on (|depdefs|))
  (|dependent| :load-before-compile (|depdefs| |clx|)
	       :recompile-on (|clx|))
  (|exclcmac| :load-before-compile (|depdefs| |clx| |dependent|)
	      :recompile-on (|dependent|))
  (|macros| :load-before-compile (|depdefs| |clx| |dependent| |exclcmac|)
	    :recompile-on (|exclcmac|))
  (|bufmac| :load-before-compile (|depdefs| |clx| |dependent| |exclcmac|
					    |macros|)
	    :recompile-on (|macros|))
  (|buffer| :load-before-compile (|depdefs| |clx| |dependent| |exclcmac|
					    |macros| |bufmac|)
	    :recompile-on (|bufmac|))
  (|display| :load-before-compile (|depdefs| |clx| |dependent| |exclcmac|
					     |macros| |bufmac| |buffer|)
	     :recompile-on (|buffer|))
  (|gcontext| :load-before-compile (|depdefs| |clx| |dependent| |exclcmac|
					      |macros| |bufmac| |buffer|
					      |display|)
	      :recompile-on (|display|))
  (|input| :load-before-compile (|depdefs| |clx| |dependent| |exclcmac|
					   |macros| |bufmac| |buffer| |display|
					   )
	   :recompile-on (|display|))
  (|requests| :load-before-compile (|depdefs| |clx| |dependent| |exclcmac|
					      |macros| |bufmac| |buffer|
					      |display| |input|)
	      :recompile-on (|display|))
  (|fonts| :load-before-compile (|depdefs| |clx| |dependent| |exclcmac|
					   |macros| |bufmac| |buffer| |display|
					   )
	   :recompile-on (|display|))
  (|graphics| :load-before-compile (|depdefs| |clx| |dependent| |exclcmac|
					      |macros| |bufmac| |buffer|
					      |display| |fonts|)
	      :recompile-on (|fonts|))
  (|text| :load-before-compile (|depdefs| |clx| |dependent| |exclcmac| |macros|
					  |bufmac| |buffer| |display|
					  |gcontext| |fonts|)
	  :recompile-on (|gcontext| |fonts|)
	  :load-after (|translate|))
  ;; The above line gets around a compiler macro expansion bug.
  
  (|attributes| :load-before-compile (|depdefs| |clx| |dependent| |exclcmac|
						|macros| |bufmac| |buffer|
						|display|)
		:recompile-on (|display|))
  (|translate| :load-before-compile (|depdefs| |clx| |dependent| |exclcmac|
					       |macros| |bufmac| |buffer|
					       |display| |text|)
	       :recompile-on (|display|))
  (|keysyms| :load-before-compile (|depdefs| |clx| |dependent| |exclcmac|
					     |macros| |bufmac| |buffer|
					     |display| |translate|)
	     :recompile-on (|translate|))
  (|manager| :load-before-compile (|depdefs| |clx| |dependent| |exclcmac|
					     |macros| |bufmac| |buffer|
					     |display|)
	     :recompile-on (|display|))
  (|image| :load-before-compile (|depdefs| |clx| |dependent| |exclcmac|
					   |macros| |bufmac| |buffer| |display|
					   )
	   :recompile-on (|display|))
  
  ;; Don't know if l-b-c list is correct.  XX
  (|resource| :load-before-compile (|depdefs| |clx| |dependent| |exclcmac|
					      |macros| |bufmac| |buffer|
					      |display|)
	      :recompile-on (|display|))
  )

#+allegro
(excl:defsystem :clx-debug
    (:default-pathname "debug/"
     :needed-systems (:clx)
     :load-before-compile (:clx))
  |describe| |keytrans| |trace| |util|)


(defun compile-clx (&optional pathname-defaults)
  (let ((*default-pathname-defaults*
	  (or pathname-defaults *default-pathname-defaults*)))
    (declare (special *default-pathname-defaults*))
    (compile-file "depdefs")
    (load "depdefs")
    (compile-file "clx")
    (load "clx")
    (compile-file "dependent")
    (load "dependent")
    (compile-file "macros")
    (load "macros")
    (compile-file "bufmac")
    (load "bufmac")
    (compile-file "buffer")
    (load "buffer")
    (compile-file "display")
    (load "display")
    (compile-file "gcontext")
    (load "gcontext")
    (compile-file "input")
    (load "input")
    (compile-file "requests")
    (load "requests")
    (compile-file "fonts")
    (load "fonts")
    (compile-file "graphics")
    (load "graphics")
    (compile-file "text")
    (load "text")
    (compile-file "attributes")
    (load "attributes")
    (load "translate")
    (compile-file "translate")		; work-around bug in 2.0 and 2.2
    (load "translate")
    (compile-file "keysyms")
    (load "keysyms")
    (compile-file "manager")
    (load "manager")
    (compile-file "image")
    (load "image")
    (compile-file "resource")
    (load "resource")
    ))


(defun load-clx (&optional pathname-defaults)
  (let ((*default-pathname-defaults*
	  (or pathname-defaults *default-pathname-defaults*)))
    (declare (special *default-pathname-defaults*))
    (load "depdefs")
    (load "clx")
    (load "dependent")
    (load "macros")
    (load "bufmac")
    (load "buffer")
    (load "display")
    (load "gcontext")
    (load "input")
    (load "requests")
    (load "fonts")
    (load "graphics")
    (load "text")
    (load "attributes")
    (load "translate")
    (load "keysyms")
    (load "manager")
    (load "image")
    (load "resource")
    ))
