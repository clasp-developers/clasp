;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase: Yes; Package: USER;  -*-

;;;; Module definition for CLX

;;; This file is a Common Lisp Module description, but you will have to edit
;;; it to meet the needs of your site.

;;; Ideally, this file (or a file that loads this file) should be
;;; located in the system directory that REQUIRE searches.  Thus a user
;;; would say
;;;			(require :clx)
;;; to load CLX.  If there is no such registry, then the user must
;;; put in a site specific
;;;			(require :clx <pathname-of-this-file>)
;;;

#-clx-ansi-common-lisp 
(in-package :user)

#+clx-ansi-common-lisp
(in-package :common-lisp-user)

#-clx-ansi-common-lisp
(provide :clx)

(defvar *clx-source-pathname*
	(pathname "/src/local/clx/*.l"))

(defvar *clx-binary-pathname*
	(let ((lisp
		(or #+lucid "lucid"
		    #+akcl  "akcl"
		    #+kcl   "kcl"
		    #+ibcl  "ibcl"
		    (error "Can't provide CLX for this lisp.")))
	      (architecture
		(or #+(or sun3 (and sun (or mc68000 mc68020))) "sun3"
		    #+(or sun4 sparc) "sparc"
		    #+(and hp (or mc68000 mc68020)) "hp9000s300"
		    #+vax "vax"
		    #+prime "prime"
		    #+sunrise "sunrise"
		    #+ibm-rt-pc "ibm-rt-pc"
		    #+mips "mips"
		    #+prism "prism"
		    (error "Can't provide CLX for this architecture."))))
	  (pathname (format nil "/src/local/clx/~A.~A/" lisp architecture))))

(defvar *compile-clx*
	nil)

(load (merge-pathnames "defsystem" *clx-source-pathname*))

(if *compile-clx*
    (compile-clx *clx-source-pathname* *clx-binary-pathname*)
  (load-clx *clx-binary-pathname*))
