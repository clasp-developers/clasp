
;; Set features :ecl-min for minimal system without CLOS
;; :clos to compile with CLOS
;;

(SYS:*MAKE-SPECIAL 'core:*echo-repl-tpl-read*)
(setq core:*echo-repl-tpl-read*
  #+emacs-inferior-lisp t
  #-emacs-inferior-lisp nil)

(setq cl:*print-circle* nil)
(setq *echo-repl-read* nil)
(setq *load-print* nil)
(setq *print-source-code-cons* nil)

(setq *features* (cons :brcl *features*))
;;(setq *features* (cons :ecl-min *features*))
(setq *features* (cons :ecl *features*))
(setq *features* (cons :clasp *features*))
;;(setq *features* (cons :clos *features*))
(setq *features* (cons :debug-compiler *features*))
(setq *features* (cons :compile-mcjit *features*))


(progn
  (core:pathname-translations "min-boehm" '(("**;*.*" #P"SYS:build;system;min-boehm;**;*.*")))
  (core:pathname-translations "full-boehm" '(("**;*.*" #P"SYS:build;system;full-boehm;**;*.*")))
  (core:pathname-translations "min-mps" '(("**;*.*" #P"SYS:build;system;min-mps;**;*.*")))
  (core:pathname-translations "full-mps" '(("**;*.*" #P"SYS:build;system;full-mps;**;*.*")))
  )



;;(setq *features* (cons :compare *features*)) ;; compare ecl to brcl

;; When boostrapping in stages, set this feature,
;; it guarantees that everything that is declared at compile/eval time
;; gets declared at load-time
;; Turn this off and recompile everything once the system has
;; been bootstrapped
(setq *features* (cons :brcl-boot *features*)) ;; When bootstrapping in stages

;; Set up a few things for the CLOS package
;;(core::select-package :clos)
;;(use-package :core)

;; Setup a few things for the GRAY streams package
(core::select-package :gray)
(core:shadow '(STREAM-ELEMENT-TYPE OPEN-STREAM-P OUTPUT-STREAM-P INPUT-STREAM-P STREAMP CLOSE))
(core:use-package :core)

;; Setup a few things for the CORE package
(select-package :core)
(use-package '(:compiler :clos :ext))

;; Setup a few things for the CMP package
(select-package :cmp)

;;; cmp:*implicit-compilation* is set to T in cmp/cmprepl.lsp
(SYS:*MAKE-SPECIAL 'cmp:*implicit-compilation*)
(setq cmp:*implicit-compilation* nil)
(export '(cmp:*implicit-compilation*))

(export '(link-system))
(use-package :core)

(select-package :core)
(if (find-package "C") nil
    (make-package "C" :use '(:cl :core)))

(select-package :core)
(if (find-package "FFI") nil
  (make-package "FFI" :use '(:CL :CORE)))

;; Setup a few things for the EXT package
(select-package :ext)
(core:*make-special '*register-with-pde-hook*)
(core:*make-special '*module-provider-functions*)
(export '*module-provider-functions*)
(setq *register-with-pde-hook* ())
(core:*make-special '*source-location*)
(setq *source-location* nil)
(export '*register-with-pde-hook*)
(core::*fset 'register-with-pde
      #'(lambda (whole env)
	  (let* ((definition (second whole))
		 (output-form (third whole)))
	    `(if ext:*register-with-pde-hook*
		 (funcall ext:*register-with-pde-hook*
			  (copy-tree *source-location*)
			  ,definition
			  ,output-form)
		 ,output-form)))
      t)
(core:*make-special '*invoke-debugger-hook*)
(setq *invoke-debugger-hook* nil)



(core:select-package :core)


(si::*fset 'core::defvar #'(lambda (whole env)
			     (let ((var (cadr whole))
				   (formp (cddr whole))
				   (form (caddr whole))
				   (doc-string (cadddr whole)))
				  "Syntax: (defparameter name form [doc])
Declares the global variable named by NAME as a special variable and assigns
the value of FORM to the variable.  The doc-string DOC, if supplied, is saved
as a VARIABLE doc and can be retrieved by (documentation 'NAME 'variable)."
				  `(LOCALLY (DECLARE (SPECIAL ,var))
				     (SYS:*MAKE-SPECIAL ',var)
				     ,@(if formp
					     `((if (boundp ',var)
						   nil
						   (setq ,var ,form)))))))
	  t )
(export 'defvar)

(si::*fset 'core::defparameter #'(lambda (whole env)
			    (let ((var (cadr whole))
				  (form (caddr whole))
				  (doc-string (cadddr whole)))
				  "Syntax: (defparameter name form [doc])
Declares the global variable named by NAME as a special variable and assigns
the value of FORM to the variable.  The doc-string DOC, if supplied, is saved
as a VARIABLE doc and can be retrieved by (documentation 'NAME 'variable)."
				  `(LOCALLY (DECLARE (SPECIAL ,var))
				     (SYS:*MAKE-SPECIAL ',var)
				     (SETQ ,var ,form))))
	  t )
(export 'defparameter)


(si::*fset 'core::defconstant #'(lambda (whole env)
			    (let ((var (cadr whole))
				  (form (caddr whole))
				  (doc-string (cadddr whole)))
				  "Syntax: (defconstant name form [doc])
Declares the global variable named by NAME as a special variable and assigns
the value of FORM to the variable.  The doc-string DOC, if supplied, is saved
as a VARIABLE doc and can be retrieved by (documentation 'NAME 'variable)."
				  `(LOCALLY (DECLARE (SPECIAL ,var))
				     (SYS:*MAKE-SPECIAL ',var)
				     (SETQ ,var ,form))))
	  t )
(export 'defconstant)




;; TODO: Really - find a better way to do this than hard-coding paths
(defconstant +intrinsics-bitcode-pathname+
  #+use-refcount "app-resources:lib;release;intrinsics_bitcode_refcount.o"
  #+use-boehm "app-resources:lib;release;intrinsics_bitcode_boehm.o"
  #+use-mps "app-resources:lib;release;intrinsics_bitcode_mps.o"
)
(defconstant +image-pathname+ (pathname "image.bundle"))
(defconstant +imagelto-pathname+ (pathname "imagelto.bundle"))
(defconstant +min-image-pathname+ (pathname "min-image.bundle"))
(export '(+image-pathname+ +min-image-pathname+ +intrinsics-bitcode-pathname+ +imagelto-pathname+))


(let ((imagelto-date (file-write-date +imagelto-pathname+))
      (intrinsics-date (file-write-date +intrinsics-bitcode-pathname+)))
  (if imagelto-date
      (if intrinsics-date
          (if (< imagelto-date intrinsics-date)
              (progn
                (bformat t "!\n")
                (bformat t "!\n")
                (bformat t "!\n")
                (bformat t "! WARNING:   The file %s is out of date \n" +imagelto-pathname+ )
                (bformat t "!            relative to %s\n" +intrinsics-bitcode-pathname+)
                (bformat t "!\n")
                (bformat t "!  Solution: Recompile the Common Lisp code\n")
                (bformat t "!\n")
                (bformat t "!\n")
                (bformat t "!\n")
                ))
          (progn
            (bformat t "!\n")
            (bformat t "!\n")
            (bformat t "!\n")
            (bformat t "WARNING:   Could not determine file-write-date of %s\n" +intrinsics-bitcode-pathname+)
            (bformat t "!\n")
            (bformat t "!\n")
            (bformat t "!\n")
            )
          )
      (progn
        (bformat t "!\n")
        (bformat t "!\n")
        (bformat t "!\n")
        (bformat t "WARNING:   Could not determine file-write-date of %s\n" +imagelto-pathname+)
        (bformat t "!\n")
        (bformat t "!\n")
        (bformat t "!\n")
        )
      ))







(defconstant +ecl-optimization-settings+ 
  '((optimize (safety 2) (speed 1) (debug 1) (space 1))
    (ext:check-arguments-type nil)))
(defconstant +ecl-unsafe-declarations+
  '(optimize (safety 0) (speed 3) (debug 0) (space 0)))
(defconstant +ecl-safe-declarations+
  '(optimize (safety 2) (speed 1) (debug 1) (space 1)))



(defvar +ecl-syntax-progv-list+
  (list
   '(
     *print-pprint-dispatch*'
     *print-array*
     *print-base*
     *print-case*
     *print-circle*
     *print-escape*
     *print-gensym*
     *print-length*
     *print-level*
     *print-lines*
     *print-miser-width*
     *print-pretty*
     *print-radix*
     *print-readably*
     *print-right-margin*
     *read-base*
     *read-default-float-format*
     *read-eval*
     *read-suppress*
     *readtable*
     si::*print-package*
     si::*print-structure*
     si::*sharp-eq-context*
     si::*circle-counter*)
   nil                              ;;  *pprint-dispatch-table* 
   t                                ;;  *print-array* 
   10                               ;;  *print-base* 
   :downcase                        ;;  *print-case* 
   t                                ;;  *print-circle* 
   t                                ;;  *print-escape* 
   t                                ;;  *print-gensym* 
   nil                              ;;  *print-length* 
   nil                              ;;  *print-level* 
   nil                              ;;  *print-lines* 
   nil                              ;;  *print-miser-width* 
   nil                              ;;  *print-pretty* 
   nil                              ;;  *print-radix* 
   t                                ;;  *print-readably* 
   nil                              ;;  *print-right-margin* 
   10                               ;;  *read-base* 
   'single-float                    ;;  *read-default-float-format* 
   t                                ;;  *read-eval* 
   nil                              ;;  *read-suppress* 
   *readtable*                      ;;  *readtable* 
   (find-package :cl)               ;;  si::*print-package* 
   t                                ;  si::*print-structure* 
   nil                              ;  si::*sharp-eq-context* 
   nil                              ;  si::*circle-counter* 
   ))

(defvar +io-syntax-progv-list+
  (list
   '(
     *print-pprint-dispatch* #|  See end of pprint.lsp  |#
     *print-array*
     *print-base*
     *print-case*
     *print-circle*
     *print-escape*
     *print-gensym*
     *print-length*
     *print-level*
     *print-lines*
     *print-miser-width*
     *print-pretty*
     *print-radix*
     *print-readably*
     *print-right-margin*
     *read-base*
     *read-default-float-format*
     *read-eval*
     *read-suppress*
     *readtable*
     *package*
     si::*sharp-eq-context*
     si::*circle-counter*)               ;
   nil                                   ;;  *pprint-dispatch-table* 
   t                                     ;;  *print-array* 
   10                                    ;;  *print-base* 
   :upcase                               ;;  *print-case* 
   nil                                   ;;  *print-circle* 
   t                                     ;;  *print-escape* 
   t                                     ;;  *print-gensym* 
   nil                                   ;;  *print-length* 
   nil                                   ;;  *print-level* 
   nil                                   ;;  *print-lines* 
   nil                                   ;;  *print-miser-width* 
   nil                                   ;;  *print-pretty* 
   nil                                   ;;  *print-radix* 
   t                                     ;;  *print-readably* 
   nil                                   ;;  *print-right-margin* 
   10                                    ;;  *read-base* 
   'single-float ;;  *read-default-float-format* 
   t             ;;  *read-eval* 
   nil           ;;  *read-suppress* 
   *readtable*   ;;  *readtable* 
   (find-package :CL-USER)               ;;  *package* 
   nil                                   ;;  si::*sharp-eq-context* 
   nil                                   ;;  si::*circle-counter* 
   ))










(core::select-package :cl)
(defvar *print-pretty* nil)  ;; Turn this on by default
(defvar *print-level* nil)
(defvar *print-length* nil)
(defvar *print-base* 10)
(defvar *print-radix* nil)
(defvar *read-default-float-format* 'double-float)



(core::export 'defun)
(core::select-package :core)

(si::*fset 'defun
	  #'(lambda (def env)
	      (let* ((name (second def))
		     (func `(function (ext::lambda-block ,@(cdr def)))))
		(ext:register-with-pde def `(si::*fset ',name ,func))))
	  t)
(export '(defun))


;; Discard documentation until helpfile.lsp is loaded
(defun set-documentation (o d s) nil)


;; This is used extensively in the ecl compiler and once in predlib.lsp
(defvar *alien-declarations* ())




(defun get-pathname-with-type (module &optional (type "lsp"))
  (merge-pathnames (pathname (string module))
		   (make-pathname :host "sys" :directory '(:absolute "kernel") :type type)))

(defun lisp-source-pathname (module)
  (or (probe-file (get-pathname-with-type module "lsp"))
      (probe-file (get-pathname-with-type module "lisp"))
      (error "Could not find source file for ~a" module)))





(si::*fset 'interpreter-iload
	  #'(lambda (module &aux (pathname (lisp-source-pathname module)))
	      (let ((name (namestring pathname)))
                (if cmp:*implicit-compilation*
                    (bformat t "Loading/compiling source: %s\n" (namestring name))
                    (bformat t "Loading/interpreting source: %s\n" (namestring name)))
		(load pathname)))
	  nil)

(si::*fset 'ibundle
	   #'(lambda (path)
	       (multiple-value-call #'(lambda (loaded &optional error-msg)
					(if loaded
					    loaded
					    (bformat t "Could not load bundle %s - error: %s\n" (truename path) error-msg)))
                 (let* ((tb (default-target-backend))
                        (image-path (target-backend-pathname path :target-backend tb))
                        (image-file (probe-file image-path)))
                   (if image-file nil (error "Could not find ~a" image-path))
                   (bformat t "Loading image %s\n" image-path)
                   (load-bundle image-path)))))

(*fset 'fset
       #'(lambda (whole env)
	   `(*fset ,(cadr whole) ,(caddr whole) ,(cadddr whole)))
       t)




(fset 'and
      #'(lambda (whole env)
	  (let ((forms (cdr whole)))
	    (if (null forms)
		t
		(if (null (cdr forms))
		    (car forms)
		    `(if ,(car forms)
			 (and ,@(cdr forms)))))))
      t)


(fset 'or
      #'(lambda (whole env)
	  (let ((forms (cdr whole)))
	    (if (null forms)
		nil
		(if ( null (cdr forms))
		    (car forms)
		    (let ((tmp (gensym)))
		      `(let ((,tmp ,(car forms)))
			 (if ,tmp
			     ,tmp
			     (or ,@(cdr forms)))))))))
      t )
(export '(and or))



(eval-when (:execute)
  (interpreter-iload 'cmp/jit-setup)
  (interpreter-iload 'clsymbols))




#| If we aren't using the compiler then just load everything with the interpreter |#


(defvar *reversed-init-filenames* ())

(si::*fset 'iload #'interpreter-iload)
#| Otherwise use the compiler |#
#-nocmp
(defun iload (fn &key load-bitcode )
  (setq *reversed-init-filenames* (cons fn *reversed-init-filenames*))
  (let* ((lsp-path (lisp-source-pathname fn))
	 (bc-path (target-backend-pathname (get-pathname-with-type fn "bc") ))
	 (load-bc (if (not (probe-file lsp-path))
		      t
		      (if (not (probe-file bc-path))
			  (progn
			    ;;			(bformat t "Bitcode file %s doesn't exist\n" (path-file-name bc-path))
			    nil)
			  (if load-bitcode
			      (progn
				;;			    (bformat t "Bitcode file %s exists - force loading\n" (path-file-name bc-path))
				t)
			      (let ((bc-newer (> (file-write-date bc-path) (file-write-date lsp-path))))
				(if bc-newer
				    (progn
				      ;;				  (bformat t "Bitcode recent - loading %s\n" (path-file-name bc-path))
				      t)
				    (progn
				      ;;				  (bformat t "Bitcode file %s is older than lsp file - loading lsp file\n" (path-file-name bc-path))
				      nil))))))))
    (if load-bc
	(progn
	  (bformat t "Loading bitcode file: %s\n" (namestring (truename bc-path)))
	  (load-bitcode bc-path))
	(if (probe-file lsp-path)
	    (progn
              (if cmp:*implicit-compilation*
                  (bformat t "Loading/compiling source: %s\n" (truename lsp-path))
                  (bformat t "Loading/interpreting source: %s\n" (truename lsp-path)))
	      (load lsp-path))
	    (bformat t "No interpreted or bitcode file for %s could be found\n" (truename lsp-path))))
    ))




(defun delete-init-file (module &key (really-delete t))
  (let ((bitcode-path (target-backend-pathname (get-pathname-with-type module "bc"))))
    (bformat t "Module: %s\n" module)
    (if (probe-file bitcode-path)
	(if really-delete
	    (progn
	      (bformat t "     Deleting: %s\n" (truename bitcode-path))
	      (delete-file bitcode-path))
	      )
	(bformat t "    File doesn't exist: %s\n" (namestring bitcode-path))
	)))

(defun recursive-find (item seq)
  (if seq
      (if (eq item (car seq))
          t
          (recursive-find item (cdr seq)))))

;; I need to search the list rather than using features because *features* may change at runtime
(defun default-target-backend ()
  (let* ((stage (if (recursive-find :ecl-min *features*) "min" "full"))
         (garbage-collector (if (recursive-find :use-mps *features*) "mps" "boehm"))
         (target-backend (bformat nil "%s-%s" stage garbage-collector)))
    target-backend))

(defvar *target-backend* (default-target-backend))
(export '*target-backend*)


(defun target-backend-pathname (pathname &key (target-backend *target-backend*) &allow-other-keys)
;;  (if target-backend nil (error "target-backend is nil"))
  (merge-pathnames (make-pathname :host target-backend) pathname))

(export '(default-target-backend target-backend-pathname))
        

(defun compile-kernel-file (filename &key (reload nil) load-bitcode (recompile nil))
;;  (if *target-backend* nil (error "*target-backend* is undefined"))
  (let* ((source-path (lisp-source-pathname filename))
	 (bitcode-path (target-backend-pathname (get-pathname-with-type filename "bc")))
	 (load-bitcode (if (probe-file bitcode-path)
			   (if load-bitcode
			       t
			       (if (> (file-write-date bitcode-path)
				      (file-write-date source-path))
				   t
				   nil))
			   nil)))
    (if (and load-bitcode (not recompile))
	(progn
	  (bformat t "Skipping compilation of %s - its bitcode file %s is more recent\n" (truename source-path) (translate-logical-pathname bitcode-path))
	  ;;	  (bformat t "   Loading the compiled file: %s\n" (path-file-name bitcode-path))
	  ;;	  (load-bitcode (as-string bitcode-path))
	  )
	(progn
	  (bformat t "\n")
	  (bformat t "Compiling %s to %s - will reload: %s\n" (truename source-path) bitcode-path reload)
	  (let ((cmp::*module-startup-prefix* "kernel"))
            (compile-file source-path :output-file bitcode-path :print t :verbose t :type :kernel)
	    (if reload
		(progn
		  (bformat t "    Loading newly compiled file: %s\n" (truename bitcode-path))
		  (load-bitcode bitcode-path))
		(bformat t "      Skipping reload\n"))
	    )))
    bitcode-path
    ))





(defvar *init-files*
  '(
    :base
    init
    cmp/jit-setup
    :start
    lsp/foundation
    lsp/export
    lsp/defmacro
    :defmacro
    lsp/helpfile
    lsp/evalmacros
    lsp/claspmacros
    lsp/testing
    lsp/profiling    ;; Do micro-profiling of the GC
    lsp/logging
    lsp/makearray
    lsp/setf
    lsp/listlib
    lsp/mislib
    #-defstruct-test lsp/defstruct
    #+defstruct-test lsp/defstruct-test
    :defstruct-test
    lsp/predlib
    lsp/iolib
    lsp/seq
    lsp/cmuutil
    lsp/seqmacros
    lsp/seqlib
    lsp/trace
    :tiny
    :pre-cmp
    ;; Compiler code
    cmp/cmpsetup
    cmp/cmpenv-proclaim
    cmp/cmpglobals
    cmp/cmptables
    cmp/cmpvar
    cmp/cmpintrinsics
    cmp/cmpir
    cmp/cmpeh
    cmp/debuginfo
    cmp/lambdalistva
    cmp/cmpvars
    cmp/cmpquote
    cmp/compiler
    cmp/compilefile
    cmp/cmpbundle
    cmp/cmpwalk
    :cmp
    :stage1
    cmp/cmprepl
    :cmprepl
    lsp/sharpmacros
    lsp/assert
    lsp/describe
    lsp/module
    lsp/loop2
    lsp/packlib
;;    cmp/cmpinterpreted
    lsp/defpackage
    lsp/format
    #|
    arraylib
    numlib
    |#
    :min
    clos/package
    clos/hierarchy
    clos/cpl
    clos/std-slot-value
    clos/slot
    clos/boot
    clos/kernel
    clos/method
    clos/combin
    clos/std-accessors
    clos/defclass
    clos/slotvalue
    clos/standard
    clos/builtin
    clos/change
    clos/stdmethod
    clos/generic
    :generic
    clos/fixup
    clos/extraclasses
    lsp/defvirtual
    :stage3
    clos/conditions
    clos/print
    clos/streams
    lsp/pprint
    clos/inspect
    :clos
    lsp/ffi
;;    asdf/build/asdf
    :front
    lsp/top
    :all

;;    lsp/pprint
))


(defun select-source-files (last-file &key first-file system)
  (or first-file (error "You must provide first-file to select-source-files"))
  (or system (error "You must provide system to select-source-files"))
  (let ((cur (member first-file system))
	files
	file)
    (or cur (error "first-file ~a was not a member of ~a" first-file system))
    (tagbody
     top
       (setq file (car cur))
       (if (endp cur) (go done))
       (if last-file
	   (if (eq last-file file)
	       (progn
		 (if (not (keywordp file))
		     (setq files (cons file files)))
		 (go done))))
       (if (not (keywordp file)) 
	   (setq files (cons file files)))
       (setq cur (cdr cur))
       (go top)
     done
       )
    (nreverse files)
    ))




(defun select-trailing-source-files (after-file &key system)
  (or system (error "You must provide :system to select-trailing-source-files"))
  (let ((cur (reverse system))
	files file)
    (tagbody
     top
       (setq file (car cur))
       (if (endp cur) (go done))
       (if after-file
	   (if (eq after-file file)
	       (go done)))
       (if (not (keywordp file)) 
	   (setq files (cons file files)))
       (setq cur (cdr cur))
       (go top)
     done
       )
    files
    ))




(defun load-system ( first-file last-file &key interp load-bitcode (target-backend (default-target-backend)) (system *init-files*))
  (let* ((*target-backend* target-backend)
         (files (select-source-files last-file :first-file first-file :system system))
	 (cur files))
    (tagbody
     top
       (if (endp cur) (go done))
       (if (not interp)
	   (iload (car cur) :load-bitcode load-bitcode)
	   (interpreter-iload (car cur)))
       (gctools:cleanup)
       (setq cur (cdr cur))
       (go top)
     done
       )))


(defun compile-system (first-file last-file &key recompile reload (system *init-files*))
;;  (if *target-backend* nil (error "*target-backend* is undefined"))
  (bformat t "compile-system  from: %s  to: %s\n" first-file last-file)
  (if (not recompile)
        (load-system first-file last-file :system system))
  (let* ((files (select-source-files last-file :first-file first-file :system system))
	 (cur files)
         bitcode-files)
    (tagbody
     top
       (if (endp cur) (go done))
       (let ((one-bitcode (compile-kernel-file (car cur) :recompile recompile :reload reload )))
         (setq bitcode-files (cons one-bitcode bitcode-files)))
       (setq cur (cdr cur))
       (go top)
     done
       )
    (reverse bitcode-files)))



;; Clean out the bitcode files.
;; passing :no-prompt t will not prompt the user
(defun clean-system ( &optional after-file &key no-prompt (target-backend (default-target-backend))
                                           (system *init-files*))
  (let* ((*target-backend* target-backend)
         (files (select-trailing-source-files after-file :system system))
	 (cur files))
    (bformat t "Will remove modules: %s\n" files)
    (bformat t "cur=%s\n" cur)
    (let ((proceed (or no-prompt
		       (progn
			 (bformat *query-io* "Delete? (Y or N) ")
			 (string-equal (read *query-io*) "Y")))))
      (if proceed
	  (tagbody
	   top
	     (if (endp cur) (go done))
	     (delete-init-file (car cur) :really-delete t )
	     (setq cur (cdr cur))
	     (go top)
	   done
	     )
	  (bformat t "Not deleting\n"))))
  )



(defun compile-cmp (&key (target-backend (default-target-backend)))
  (let ((*target-backend* target-backend))
        (compile-system :start :cmp :reload t )
        (compile-system :base :start :reload nil )
        ))


(defun compile-min-system (&key (target-backend (default-target-backend)))
  (let* ((*target-backend* target-backend)
         (bitcodes1 (compile-system :start :cmp :reload t ))
         ;(bitcodes0 (compile-system :base :start :reload nil :recompile t ))
         (bitcodes2 (compile-system :cmp :min ))
         (all-bitcodes (nconc bitcodes1 bitcodes2)))
    (cmp:link-system
           (target-backend-pathname +min-image-pathname+ )
           :lisp-bitcode-files all-bitcodes
           )))


(defun compile-min-recompile (&key (target-backend (default-target-backend)))
  (let ((*target-backend* target-backend)
        (bitcode-files (compile-system :start :min :recompile t )))
    (cmp:link-system (target-backend-pathname +min-image-pathname+ )
                     :lisp-bitcode-files bitcode-files )))

(defun switch-to-full ()
  (setq *features* (remove :ecl-min *features*))
  (push :clos *features*)
  (bformat t "Removed :ecl-min from and added :clos to *features* --> %s\n" *features*)
)
  
(defun compile-full () ;; &key (target-backend (default-target-backend)))
  (switch-to-full)
  (let ((*target-backend* (default-target-backend)))
    (load-system :start :all :interp t )
;;    (switch-to-full)
    ;; Compile everything - ignore old bitcode
    (let ((bitcode-files (compile-system :base :all :recompile t )))
      (cmp:link-system-lto (target-backend-pathname +imagelto-pathname+)
                           :lisp-bitcode-files bitcode-files ))))

(defun compile-clos () ;; &key (target-backend (default-target-backend)))
  (switch-to-full)
  (let ((*target-backend* (default-target-backend)))
    (load-system :start :clos :interp t )
;;    (switch-to-full)
    ;; Compile everything - ignore old bitcode
    (let ((bitcode-files (compile-system :base :all :recompile t )))
      (cmp:link-system-lto (target-backend-pathname +imagelto-pathname+)
                           :lisp-bitcode-files bitcode-files ))))


(defun help-build ()
  (bformat t "Useful build commands:\n")
  (bformat t "(load-system from-stage to-stage &key (interp t/nil) (system *init-files*))\n")
  (bformat t "          - Load parts of the system\n")
  (bformat t "(compile-system from-stage to-stage &key :reload t (system *init-files*))\n")
  (bformat t "          - Compile whatever parts of the system have changed\n")
  (bformat t "(clean-system after-stage)\n")
  (bformat t "          - Remove all built files after after-stage\n")
  )


(defun tpl-default-pathname-defaults-command ()
  (print *default-pathname-defaults*))

(defun tpl-change-default-pathname-defaults-dir-command (raw-dir)
  (let* ((corrected-dir (format nil "~a/" (string-right-trim "/" (string raw-dir))))
	 (dir (pathname-directory (parse-namestring corrected-dir)))
	 (pn-dir (mapcar #'(lambda (x) (if (eq x :up) :back x)) dir))
	 (new-pathname (merge-pathnames (make-pathname :directory pn-dir) *default-pathname-defaults*))
	 )
    (setq *default-pathname-defaults* new-pathname)
    )
  )


(defun tpl-hook (cmd)
  (cond 
    ((eq (car cmd) :pwd) (tpl-default-pathname-defaults-command))
    ((eq (car cmd) :cd) (tpl-change-default-pathname-defaults-dir-command (cadr cmd)))
    (t (bformat t "Unknown command %s\n" cmd)))
)

(setq *top-level-command-hook* #'tpl-hook)


(defun my-do-time (closure)
  (let* ((real-start (get-internal-real-time))
         (run-start (get-internal-run-time))
         real-end
         run-end)
    (funcall closure)
    (setq real-end (get-internal-real-time)
          run-end (get-internal-run-time))
    (bformat t "real time: %lf secs\nrun time : %lf secs\n"
             (float (/ (- real-end real-start) internal-time-units-per-second))
             (float (/ (- run-end run-start) internal-time-units-per-second)))))

(core:*make-special 'my-time)
(fset 'my-time
           #'(lambda (def env)
               (let ((form (cadr def)))
                 `(my-do-time #'(lambda () ,form))))
           t)
(export 'my-time)
                 

(defun load-clasprc ()
  "Load the users startup code"
  (let ((clasprc (make-pathname :name ""
			       :type "clasprc"
			       :defaults (user-homedir-pathname))))
    (if (probe-file clasprc)
	(load clasprc)
	(format t "Could not find pathname: ~a~%" clasprc))))


(defun load-pos ()
  (bformat t "Load pos: %s %s\n" core:*load-current-source-file-info* core:*load-current-linenumber*))
(export 'load-pos)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Setup the build system for SICL
;;
;;
(defun setup-sicl () (load "sys:kernel;sicl;sinit.lsp"))
(export 'setup-sicl)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Setup the build system for ASDF
;;
;;
(defun setup-asdf () (load "sys:kernel;sys-asdf.lsp"))
(export 'setup-asdf)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Start everything up
;;

;; Print the features
(eval-when (:execute)
  (bformat t "--------Features\n")
  (mapc #'(lambda (x) (bformat t "%s\n" x)) *features*))


#+ignore-init-image
(eval-when (:execute)
  (if core:*command-line-load*
      (load core:*command-line-load*)
      (core:low-level-repl)))

(defvar *loaded-image* nil)
#-ignore-init-image
(eval-when (:execute)
  (if (not *loaded-image*)
      (let ((image-pathname (target-backend-pathname #+ecl-min +min-image-pathname+
                                                     #-ecl-min +imagelto-pathname+
                                                     :target-backend (default-target-backend))))
        (setq *loaded-image* t)
        (bformat t "init.lsp> Loading image bundle %s\n" image-pathname)
        (my-time (ibundle image-pathname))
        (require 'system)
        (load-clasprc)
        (if core:*command-line-load*
            (load core:*command-line-load*)
            #-ecl-min(progn
                       (bformat t "Starting Clasp\n")
                       (top-level))
            #+ecl-min(progn
                       (bformat t "Starting Clasp-min\n")
                       (core::low-level-repl))
            ))
      ))



