;; Set features :ecl-min for minimal system without CLOS
;; :clos to compile with CLOS
;;

#+(or)(setq *features* (cons :debug-format *features*))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (core:select-package "CORE"))

(SYS:*MAKE-SPECIAL '*echo-repl-tpl-read*)
(export '*echo-repl-tpl-read*)
(export '(*echo-repl-tpl-read* 
          run-repl 
          *load-current-source-file-info* 
          *load-current-linenumber*
          cons-car
          cons-cdr
          ))

:pause-hir

(setq *echo-repl-tpl-read* (member :emacs-inferior-lisp *features*))
(setq *echo-repl-read* t)
(setq *load-print* nil)
(setq *print-source-code-cons* nil)

(sys:*make-special 'core::*boot-verbose*)
(setq core::*boot-verbose* nil)
(setq cl:*print-circle* nil)

;;(setq *features* (cons :ecl-min *features*))
(setq *features* (cons :clasp *features*))
;;(setq *features* (cons :clos *features*))
;;(setq *features* (cons :debug-compiler *features*))
(setq *features* (cons :compile-mcjit *features*))


#+(or)(progn
        (core:pathname-translations "min-boehm" '(("**;*.*" #P"SYS:build;system;min-boehm;**;*.*")))
        (core:pathname-translations "full-boehm" '(("**;*.*" #P"SYS:build;system;full-boehm;**;*.*")))
        (core:pathname-translations "min-mps" '(("**;*.*" #P"SYS:build;system;min-mps;**;*.*")))
        (core:pathname-translations "full-mps" '(("**;*.*" #P"SYS:build;system;full-mps;**;*.*")))
        )



;;(setq *features* (cons :compare *features*)) ;; compare ecl to clasp

;; When boostrapping in stages, set this feature,
;; it guarantees that everything that is declared at compile/eval time
;; gets declared at load-time
;; Turn this off and recompile everything once the system has
;; been bootstrapped
(setq *features* (cons :clasp-boot *features*)) ;; When bootstrapping in stages

;; Set up a few things for the CLOS package
(eval-when (:execute :compile-toplevel :load-toplevel)
  (core::select-package :clos))
(export '(standard-class
          ))

;; Setup a few things for the GRAY streams package
(eval-when (:execute :compile-toplevel :load-toplevel)
  (core::select-package :gray))
(shadow '(STREAM-ELEMENT-TYPE OPEN-STREAM-P OUTPUT-STREAM-P INPUT-STREAM-P STREAMP CLOSE))

;; Setup a few things for the CORE package
(eval-when (:execute :compile-toplevel :load-toplevel)
  (core::select-package :core))
(use-package '(:compiler :ext))

;; Setup a few things for the CMP package
(eval-when (:execute :compile-toplevel :load-toplevel)
  (core::select-package :cmp))
(export '(link-system-lto))


(export '(link-system))
(use-package :core)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (select-package :core))
(if (find-package "C")
    nil
    (make-package "C" :use '(:cl :core)))

;; Compiling with Cleavir injects some symbols that
;; need to be interned in this package
(if (find-package "CLASP-CLEAVIR-GENERATE-AST")
    nil
    (make-package "CLASP-CLEAVIR-GENERATE-AST"))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (select-package :core))
(if (find-package "FFI") nil
  (make-package "FFI" :use '(:CL :CORE)))

;; Setup a few things for the EXT package
(eval-when (:execute :compile-toplevel :load-toplevel)
  (select-package :ext))
#+(or)(export '(*module-provider-functions*
          check-arguments-type
          array-index
          byte8
          integer8
          byte16
          integer16
          byte32
          integer32
          byte64
          integer64
          cl-fixnum
          cl-index
          assume-no-errors
          sequence-stream
          all-encodings
          load-encoding
          make-encoding
          assume-right-type
          ))
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
(export 'register-with-pde)
(core:*make-special '*invoke-debugger-hook*)
(setq *invoke-debugger-hook* nil)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (core:select-package :core))


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
  #+use-boehm "app-resources:lib;release;intrinsics_bitcode_boehm.sbc"
  #+use-mps "app-resources:lib;release;intrinsics_bitcode_mps.sbc"
)
(defconstant +image-pathname+ (make-pathname :directory '(:relative) :name "image" :type "fasl"))
(export '(+image-pathname+ +intrinsics-bitcode-pathname+))
;; +min-image-pathname+ +intrinsics-bitcode-pathname+ +imagelto-pathname+))

;; Don't bother with this test anymore
#+(or)
(let ((image-date (file-write-date core:*command-line-image*))
      (intrinsics-date (file-write-date +intrinsics-bitcode-pathname+)))
  (if image-date
      (if intrinsics-date
          (if (< image-date intrinsics-date)
              (progn
                (bformat t "!\n")
                (bformat t "! WARNING:   The file %s is out of date \n" +image-pathname+ )
                (bformat t "!            relative to %s\n" +intrinsics-bitcode-pathname+)
                (bformat t "!\n")
                (bformat t "!  Solution: Recompile the Common Lisp code\n")
                (bformat t "!\n")
                ))
	(progn
	  (bformat t "!\n")
	  (bformat t "! WARNING:   Could not determine file-write-date of %s\n" +intrinsics-bitcode-pathname+)
	  (bformat t "!\n")
	  )
	)
    (progn
      (bformat t "!\n")
      (bformat t "WARNING:   Could not determine file-write-date of %s\n" *command-line-image*)
      (bformat t "!\n")
      )
    ))







(defconstant +ecl-optimization-settings+
  '((optimize (safety 2) (speed 1) (debug 1) (space 1))
    (ext::check-arguments-type nil)))
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









(eval-when (:execute :compile-toplevel :load-toplevel)
  (core::select-package :cl))
(defvar *print-pretty* nil)  ;; Turn this on by default
(defvar *print-level* nil)
(defvar *print-length* nil)
(defvar *print-base* 10)
(defvar *print-radix* nil)
(defvar *read-default-float-format* 'double-float)



(core::export 'defun)
(eval-when (:execute :compile-toplevel :load-toplevel)
  (core::select-package :core))

;;; A temporary definition of defun - the real one is in evalmacros
(si::*fset 'defun
	   #'(lambda (def env)
	       (let ((name (second def))	;cadr
		     (lambda-list (third def))	; caddr
		     (lambda-body (cdddr def))) ; cdddr
		 (multiple-value-call
		     (function (lambda (&optional (decl) (body) (doc) &rest rest)
		       (declare (ignore rest))
		       (if decl (setq decl (list (cons 'declare decl))))
		       (let ((func `#'(lambda ,lambda-list ,@decl ,@doc (block ,name ,@body))))
			 ;;(bformat t "PRIMITIVE DEFUN defun --> %s\n" func )
			 (ext::register-with-pde def `(si::*fset ',name ,func)))))
		   (si::process-declarations lambda-body nil #| No documentation until the real DEFUN is defined |#)) 

		 ))
	   t)

(export '(defun))


;;; Define these here so that Cleavir can do inlining
(defvar *defun-inline-hook* nil)
(defvar *do-inline-hook* nil)
(defvar *proclaim-hook* nil)
(export '(*defun-inline-hook*
          *do-inline-hook*
          *proclaim-hook*))

;; Discard documentation until helpfile.lsp is loaded
(defun set-documentation (o d s) nil)

(defvar *functions-to-inline* (make-hash-table :test #'equal))
(defvar *functions-to-notinline* (make-hash-table :test #'equal))

(defun proclaim (decl)
  "Args: (decl-spec)
Gives a global declaration.  See DECLARE for possible DECL-SPECs."
  (cond
    ((eq (car decl) 'SPECIAL)
     (mapc #'sys::*make-special (cdr decl)))
    ((eq (car decl) 'cl:inline)
     (let ((name (cadr decl)))
       (core:hash-table-setf-gethash *functions-to-inline* name t)
       (remhash name *functions-to-notinline*)))
    ((eq (car decl) 'cl:notinline)
     (let ((name (cadr decl)))
       (core:hash-table-setf-gethash *functions-to-notinline* name t)
       (remhash name *functions-to-inline*)))
    (*proclaim-hook*
     (funcall *proclaim-hook* decl))))

(defun declared-global-inline-p (name)
  (gethash name *functions-to-inline*))

(defun declared-global-notinline-p (name)
  (gethash name *functions-to-notinline*))

(defun global-inline-status (name)
  "Return 'cl:inline 'cl:notinline or nil"
  (cond
    ((declared-global-inline-p name) 'cl:inline)
    ((declared-global-notinline-p name) 'cl:notinline)
    (t nil)))

(export '(global-inline-status
          declared-global-notinline-p
          declared-global-inline-p))

;; This is used extensively in the ecl compiler and once in predlib.lsp
(defvar *alien-declarations* ())
(export '*alien-declarations*)

(defun get-pathname-with-type (module &optional (type "lsp"))
  (cond
    ((pathnamep module)
     (merge-pathnames module
                      (make-pathname
                       :type type
                       :defaults (translate-logical-pathname
                                  (make-pathname :host "sys")))))
    ((symbolp module)
     (merge-pathnames (pathname (string module))
                      (make-pathname :host "sys" :directory '(:absolute) :type type)))
    (t (error "bad module name: ~s" module))))

(defun lisp-source-pathname (module)
  (or (probe-file (get-pathname-with-type module "lsp"))
      (probe-file (get-pathname-with-type module "lisp"))
      (error "Could not find source file for ~a" module)))
(export 'lisp-source-pathname)

(si::*fset 'interpreter-iload
           #'(lambda (module)
               (let* ((pathname (lisp-source-pathname module))
		      (name (namestring pathname)))
                 (if cmp:*implicit-compile-hook*
                     (bformat t "Loading/compiling source: %s\n" (namestring name))
                     (bformat t "Loading/interpreting source: %s\n" (namestring name)))
		 (load pathname)))
           nil)

(si::*fset 'ibundle
	   #'(lambda (path)
	       (multiple-value-call
                #'(lambda (loaded &optional error-msg)
                    (if loaded
                        loaded
                        (bformat t "Could not load bundle %s - error: %s\n" (truename path) error-msg)))
                (let* ((tb (default-target-backend))
                       (image-path (target-backend-pathname path :target-backend tb))
                       (image-file (probe-file image-path)))
                  (if image-file nil (error "Could not find ~a" image-path))
                  (bformat t "Loading image %s\n" image-path)
                  (load-bundle image-path llvm-sys:+clasp-main-function-name+)))))

(si::*fset 'fset
		 #'(lambda (whole env)
		     `(si::*fset ,(cadr whole) ,(caddr whole) ,(cadddr whole)))
		 t)
(export 'fset)



(si::*fset 'and
	   #'(lambda (whole env)
	       (let ((forms (cdr whole)))
		 (if (null forms)
		     t
		     (if (null (cdr forms))
			 (car forms)
			 `(if ,(car forms)
			      (and ,@(cdr forms)))))))
	   t)


(si::*fset 'or
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
  (load (lisp-source-pathname 'kernel/cmp/jit-setup))
  (load (lisp-source-pathname 'kernel/clsymbols)))




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
              (if cmp:*implicit-compile-hook*
                  (bformat t "Loading/compiling source: %s\n" (truename lsp-path))
                  (bformat t "Loading/interpreting source: %s\n" (truename lsp-path)))
	      (load lsp-path))
	    (bformat t "No interpreted or bitcode file for %s could be found\n" (truename lsp-path))))
    ))




(defun delete-init-file (module &key (really-delete t))
  (let ((bitcode-path (target-backend-pathname (get-pathname-with-type module "bc"))))
    (if (probe-file bitcode-path)
	(if really-delete
	    (progn
	      (bformat t "     Deleting bitcode: %s\n" (truename bitcode-path))
	      (delete-file bitcode-path))
	      )
	)))

(defun recursive-find (item seq)
  (if seq
      (if (eq item (car seq))
          t
          (recursive-find item (cdr seq)))))

;; I need to search the list rather than using features because *features* may change at runtime
(defun default-target-backend (&optional given-stage)
  (let* ((stage (if given-stage given-stage (if (recursive-find :ecl-min *features*) "min" (if (recursive-find :cclasp *features*) "cclasp" "full"))))
         (garbage-collector (if (recursive-find :use-mps *features*) "mps" "boehm"))
         (target-backend (bformat nil "%s-%s" stage garbage-collector)))
    target-backend))

(defvar *target-backend* (default-target-backend))
(export '*target-backend*)

(defun strip-root (l orig-sys)
  (let ((cur l)
        (root orig-sys))
    (tagbody
     top
       (if (eql (car cur) (car root))
           (progn
             (setq cur (cdr cur))
             (setq root (cdr root))
             (go top))))
    (if root
        (error "Roots don't match - could not strip all of root ~s from ~s" orig-sys l))
    cur))


(defun maybe-relative-pathname-to-sys (x &optional (sys-pn (translate-logical-pathname "SYS:")))
  (let ((dir-x (pathname-directory x)))
    (if (eq (car dir-x) :absolute)
	(make-pathname :directory
		       (cons :relative 
			     (strip-root (cdr dir-x) (cdr (pathname-directory sys-pn))))
		       :defaults x)
	x)))

(defun target-backend-pathname (pathname &key (target-backend *target-backend*) &allow-other-keys)
  ;;  (if target-backend nil (error "target-backend is nil"))
  (let ((relative (maybe-relative-pathname-to-sys pathname)))
    (merge-pathnames relative (translate-logical-pathname (make-pathname :host target-backend)))))

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
	  (bformat t "Compiling %s\n   to %s - will reload: %s\n" (truename source-path) bitcode-path reload)
	  (let ((cmp::*module-startup-prefix* "kernel"))
            (compile-file source-path :output-file bitcode-path :print t :verbose t :output-type :bitcode :type :kernel)
	    (if reload
		(progn
		  (bformat t "    Loading newly compiled file: %s\n" (truename bitcode-path))
		  (load-bitcode bitcode-path))
		(bformat t "      Skipping reload\n"))
	    )))
    bitcode-path
    ))
(export 'compile-kernel-file)





(defvar *init-files*
  '(
    :init
    #P"kernel/init"
    :cleavir-injection
    #P"kernel/cmp/jit-setup"
    #P"kernel/clsymbols"
    :start
    #P"kernel/lsp/packages"
    #P"kernel/lsp/foundation"
    #P"kernel/lsp/export"
    #P"kernel/lsp/defmacro"
    :defmacro
    #P"kernel/lsp/helpfile"
    #P"kernel/lsp/evalmacros"
    #P"kernel/lsp/claspmacros"
    #P"kernel/lsp/source-transformations"
    :macros
    #P"kernel/lsp/testing"
    #P"kernel/lsp/makearray"
    #P"kernel/lsp/arraylib"
    #P"kernel/lsp/setf"
    #P"kernel/lsp/listlib"
    #P"kernel/lsp/mislib"
    #P"kernel/lsp/defstruct"
    #P"kernel/lsp/predlib"
    #P"kernel/lsp/seq"
    #P"kernel/lsp/cmuutil"
    #P"kernel/lsp/seqmacros"
    #P"kernel/lsp/iolib"
;;    #P"kernel/lsp/profiling"    ;; Do micro-profiling of the GC
    :tiny
    :pre-cmp
    ;; Compiler code
    #P"kernel/cmp/packages"
    #P"kernel/cmp/cmpsetup"
;;    #P"kernel/cmp/cmpenv-fun"
;;    #P"kernel/cmp/cmpenv-proclaim"
    #P"kernel/cmp/cmpglobals"
    #P"kernel/cmp/cmptables"
    #P"kernel/cmp/cmpvar"
    #P"kernel/cmp/cmputil"
    #P"kernel/cmp/cmpintrinsics"
    #P"kernel/cmp/cmpir"
    #P"kernel/cmp/cmpeh"
    #P"kernel/cmp/debuginfo"
;;    #P"kernel/cmp/arguments"
    #P"kernel/cmp/lambdalistva"
    #P"kernel/cmp/cmpvars"
    #P"kernel/cmp/cmpquote"
    #P"kernel/cmp/cmpobj"
;;    #P"kernel/cmp/mincomp"
    #P"kernel/cmp/compiler"
    #P"kernel/cmp/compilefile"
    #P"kernel/cmp/cmpbundle"
    #P"kernel/cmp/cmpwalk"
    :cmp
    :stage1
    #P"kernel/cmp/cmprepl"
    :cmprepl
    #P"kernel/lsp/logging"
    #P"kernel/lsp/seqlib"
    #P"kernel/lsp/trace"
    :was-pre-cmp
    #P"kernel/lsp/sharpmacros"
    #P"kernel/lsp/assert"
    #P"kernel/lsp/numlib"
    #P"kernel/lsp/describe"
    #P"kernel/lsp/module"
    #P"kernel/lsp/loop2"
    #P"kernel/lsp/shiftf-rotatef"
    #P"kernel/lsp/assorted"
    #P"kernel/lsp/packlib"
;;    cmp/cmpinterpreted
    #P"kernel/lsp/defpackage"
    #P"kernel/lsp/format"
    #|
    arraylib
    numlib
    |#
    :min
    #P"kernel/clos/package"
    #P"kernel/clos/hierarchy"
    #P"kernel/clos/cpl"
    #P"kernel/clos/std-slot-value"
    #P"kernel/clos/slot"
    #P"kernel/clos/boot"
    #P"kernel/clos/kernel"
    #P"kernel/clos/method"
    #P"kernel/clos/combin"
    #P"kernel/clos/std-accessors"
    #P"kernel/clos/defclass"
    #P"kernel/clos/slotvalue"
    #P"kernel/clos/standard"
    #P"kernel/clos/builtin"
    #P"kernel/clos/change"
    #P"kernel/clos/stdmethod"
    #P"kernel/clos/generic"
    :generic
    #P"kernel/clos/fixup"
    #P"kernel/clos/extraclasses"
    #P"kernel/lsp/defvirtual"
    :stage3
    #P"kernel/clos/conditions"
    #P"kernel/clos/print"
    #P"kernel/clos/streams"
    #P"kernel/lsp/pprint"
    #P"kernel/clos/inspect"
    :clos
    #P"kernel/lsp/ffi"
    #P"sockets/sockets"
;;    asdf/build/asdf
    :front
    #P"kernel/lsp/top"
    :all
;;    lsp/pprint
    ))
(export '*init-files*)


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
	   (if (equal last-file file)
	       (progn
		 (if (not (keywordp file))
		     (setq files (cons file files)))
		 (go done))))
       (if (not (keywordp file))
	   (setq files (cons file files)))
       (setq cur (cdr cur))
       (go top)
     done)
    (nreverse files)))

(export 'select-source-files)



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




(defun load-system ( first-file last-file &key interp load-bitcode (target-backend *target-backend*) (system *init-files*))
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
(export 'compile-system)

(defun copy-system (first-file last-file &key (from-target-backend nil) (to-target-backend nil) (system *init-files*))
  ;;  (if *target-backend* nil (error "*target-backend* is undefined"))
  (or from-target-backend (error "from-target-backend must be defined"))
  (or to-target-backend (error "to-target-backend must be defined"))
  (bformat t "copy-system  from: %s  to: %s  from-target-backend: %s  to-target-backend: %s\n" first-file last-file from-target-backend to-target-backend)
  (let* ((files (select-source-files last-file :first-file first-file :system system))
	 (cur files))
    (tagbody
     top
       (if (endp cur) (go done))
       (let* ((bitcode-raw (get-pathname-with-type (car cur) "bc"))
	      (bitcode-from (target-backend-pathname bitcode-raw :target-backend from-target-backend))
	      (bitcode-to (target-backend-pathname bitcode-raw :target-backend to-target-backend)))
	 (core:copy-file bitcode-from bitcode-to))
       (setq cur (cdr cur))
       (go top)
     done
       )
))


;; Clean out the bitcode files.
;; passing :no-prompt t will not prompt the user
(export 'clean-system)
(defun clean-system ( &optional after-file &key no-prompt (target-backend *target-backend*)
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
        (compile-system :init :start :reload nil )
        ))


(defconstant +minimal-epilogue-form+ '(progn
                                        (process-command-line-load-eval-sequence)
                                        (bformat t "Starting clasp-min low-level-repl\n")
                                        (core::low-level-repl)))



(export 'compile-min-system)
(defun compile-min-system (&key (source-backend (default-target-backend))(target-backend (default-target-backend)))
  (let* ((*target-backend* source-backend)
         (bitcodes1 (compile-system :start :cmp :reload t ))
         (bitcodes0 (compile-system :init :start :reload nil :recompile t ))
         (bitcodes2 (compile-system :cmp :min ))
         (all-bitcodes (nconc bitcodes0 bitcodes1 bitcodes2)))
    (let ((*target-backend* target-backend))
    (cmp::link-system-lto
           (target-backend-pathname +image-pathname+ )
           :lisp-bitcode-files all-bitcodes
           :epilogue-form +minimal-epilogue-form+
           ))))


(export 'compile-min-recompile)
(defun compile-min-recompile (&key (target-backend (default-target-backend)))
  (let ((*target-backend* target-backend)
        (bitcode-files0 (compile-system :init :start :recompile t))
        (bitcode-files1 (compile-system :start :min :recompile t ))
        )
    (cmp:link-system-lto (target-backend-pathname +image-pathname+ )
                     :lisp-bitcode-files (nconc bitcode-files0 bitcode-files1)
                     :epilogue-form +minimal-epilogue-form+)))



(export 'switch-to-full)
(defun switch-to-full ()
  (setq *features* (remove :ecl-min *features*))
  (push :clos *features*)
  (bformat t "Removed :ecl-min from and added :clos to *features* --> %s\n" *features*))

(defun partial-compile-full (end)
  (if (member :ecl-min *features*) (switch-to-full))
  (let ((*target-backend* (default-target-backend)))
    (load-system :start end :interp t )))


(export 'compile-full)
(defun compile-full ()
  (if (member :ecl-min *features*) (switch-to-full))
  (let ((*target-backend* (default-target-backend)))
    (load-system :start :all :interp t )
    (let ((bitcode-files (compile-system :init :all :recompile t )))
      (cmp:link-system-lto (target-backend-pathname +image-pathname+)
			   :lisp-bitcode-files bitcode-files
			   :prologue-form '(progn
					    (push :clos *features*)
					    (if (member :interactive *features*) 
						(bformat t "Starting Clasp %s ... loading image... it takes a few seconds\n"  (lisp-implementation-version))))
			   :epilogue-form '(progn
					    (cl:in-package :cl-user)
					    (process-command-line-load-eval-sequence)
					    (when (member :interactive *features*) (core:run-repl))))
      )))






(defun compile-clos () ;; &key (target-backend (default-target-backend)))
  (switch-to-full)
  (let ((*target-backend* (default-target-backend)))
    (load-system :start :clos :interp t )
;;    (switch-to-full)
    ;; Compile everything - ignore old bitcode
    (let ((bitcode-files (compile-system :init :all :recompile t )))
      (cmp:link-system-lto (target-backend-pathname +image-pathname+)
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
(si::*fset 'my-time
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
	(load clasprc))))
(export 'load-clasprc)

(defun load-pos ()
  (declare (special core:*load-current-source-file-info* core:*load-current-linenumber*))
  (bformat t "Load pos: %s %s\n" core:*load-current-source-file-info* core:*load-current-linenumber*))
(export 'load-pos)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Setup the build system for ASDF
;;
;;
(defun setup-asdf () (load "sys:kernel;asdf;build;asdf.lsp"))
(export 'setup-asdf)

(defun compile-asdf ()
  ;; Run make on asdf wherever it is installed
;  (core:system (bformat nil "(cd %s; make)" (namestring (translate-logical-pathname "sys:kernel;asdf;"))))
  (compile-file "sys:kernel;asdf;build;asdf.lisp" :output-file (compile-file-pathname "sys:modules;asdf;asdf.fasl"
										      :target-backend (default-target-backend)))
  #+(or)(cmp::link-system-lto "sys:kernel;asdf;build;asdf.fasl"
			      :lisp-bitcode-files (list #P"sys:kernel;asdf;build;asdf.bc"))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Setup the build system for SICL
;;
(defun setup-cleavir ()
  (load "sys:kernel;asdf;build;asdf.fasl")
  (load "sys:kernel;cleavir;ccmp-all.lsp")
  )

(export 'setup-sicl)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Setup the swank
;;
(defun load-swank ()
  (load "sys:swank.lsp"))
(export '(load-swank))


(defun load-cleavir-system ()
  (let* ((fin (open "sys:kernel;cleavir-system.lsp")))
    (read fin)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Start everything up
;;


(defun process-command-line-load-eval-sequence ()
  (mapcar #'(lambda (entry)
              (if (eq (car entry) :load)
                  (load (cdr entry))
                (eval (read-from-string (cdr entry)))))
          core::*command-line-load-eval-sequence*)
  )

(export 'core:top-level)
(defun run-repl ()
  (if (fboundp 'core:top-level)
      (progn
	(require 'system)
	(load-clasprc)
	(core:top-level))
      (core:low-level-repl)))


(eval-when (:execute)
  (process-command-line-load-eval-sequence)
  (core:low-level-repl))
