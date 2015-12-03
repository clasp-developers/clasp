;; Set features :ecl-min for minimal system without CLOS
;; :clos to compile with CLOS
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (core:select-package "CORE"))

;;(setq *features* (cons :dbg-print *features*))
(SYS:*MAKE-SPECIAL '*echo-repl-tpl-read*)
(export '*echo-repl-tpl-read*)
(export '(*echo-repl-tpl-read* 
          run-repl 
          *load-current-source-file-info* 
          *load-current-linenumber*
          cons-car
          cons-cdr
          ))
(sys:*make-special 'core::*notify-on-compile*)
(setq *notify-on-compile* (member :notify-on-compile *features*))
(export '*notify-on-compile*)

(sys:*make-special 'core::*trace-startup*)
(setq *trace-startup* (member :trace-startup *features*))
(export '*trace-startup*)


(setq *echo-repl-tpl-read* (member :emacs-inferior-lisp *features*))
(setq *echo-repl-read* nil)
(setq *load-print* nil)
(setq *print-source-code-cons* nil)

(sys:*make-special 'core::*boot-verbose*)
(setq core::*boot-verbose* nil)
(setq cl:*print-circle* nil)

(sys:*make-special 'core::*clang-bin*)
(setq core::*clang-bin* (ext:getenv "CLASP_CLANG_PATH"))
(export 'core::*clang-bin*)

;;(setq *features* (cons :ecl-min *features*))
(setq *features* (cons :clasp *features*))
;;(setq *features* (cons :clos *features*))
;;(setq *features* (cons :debug-compiler *features*))
(setq *features* (cons :compile-mcjit *features*))


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

(defun build-configuration ()
  (cond
    ((member :use-mps *features*) "mps")
    ((member :use-boehmdc *features*) "boehmdc")
    ((member :use-boehm *features*) "boehm")
    (t (error "Unknown clasp configuration"))))

(defun build-intrinsics-bitcode-pathname ()
  (let ((variant (cond
                   ((member :release-build *features*) "release")
                   ((member :debug-build *features*) "debug")
                   (t (error "Unknown build type")))))
    (bformat nil "app-contents:execs;%s;%s;bin;intrinsics_bitcode.sbc" (build-configuration) variant)))


(defconstant +image-pathname+ (make-pathname :directory '(:relative) :name "image" :type "fasl"))
(export '(+image-pathname+ build-intrinsics-bitcode-pathname))

(defun build-hostname (type &optional stage)
  (let* ((stage (if stage 
                    stage 
                    (if (member :ecl-min *features*) 
                        "min" 
                        (if (member :cclasp *features*) 
                            "cclasp" 
                            "full"))))
         (type-modified-host-suffix (cond
                                      ((eq type :bc) "bitcode")
                                      (t (build-configuration))))
         (bitcode-host (bformat nil "%s-%s" stage type-modified-host-suffix)))
    bitcode-host))

(defun maybe-relative-pathname-to-sys (x &optional (sys-pn (translate-logical-pathname "SYS:")))
  (flet ((relative-pathname-p (pathname)
           (eq :relative (pathname-directory pathname))))
    (if (relative-pathname-p x)
        (make-pathname :directory (pathname-directory x)
                       :name (pathname-name x)
                       :type (pathname-type x))
        (let ((dir-x (pathname-directory x)))
          (if (eq (car dir-x) :absolute)
              (make-pathname :directory
                             (cons :relative
                                   (strip-root (cdr dir-x) (cdr (pathname-directory sys-pn))))
                             :defaults x)
              x)))))

(defun build-pathname (partial-pathname &optional (type :lisp) stage)
  (let ((module (maybe-relative-pathname-to-sys partial-pathname))
        (target-host (build-hostname type stage))
        (sys-root (translate-logical-pathname "SYS:"))
        pn)
    #+dbg-print(bformat t "DBG-PRINT build-pathname  module: %s\n" module)
    #+dbg-print(bformat t "DBG-PRINT   target-host: %s\n" target-host)
    (cond
      ((eq type :lisp)
       (cond
         ((probe-file (merge-pathnames (merge-pathnames module (make-pathname :type "lsp")) sys-root)))
         ((probe-file (merge-pathnames (merge-pathnames module (make-pathname :type "lisp")) sys-root)))
         (t (error "Could not find source file with lsp or lisp extension for ~s" module))))
      (t
       (merge-pathnames (merge-pathnames module (make-pathname :type (string-downcase (string type))))
                        (translate-logical-pathname (make-pathname :host target-host)) )))))
(export '(build-pathname build-host))
  
(defun get-pathname-with-type (module &optional (type "lsp"))
  (error "Depreciated get-pathname-with-type")
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
  (load (build-pathname #P"kernel/cmp/jit-setup"))
  (load (build-pathname #P"kernel/clsymbols")))

(defvar *reversed-init-filenames* ())

(si::*fset 'interpreter-iload
           #'(lambda (module)
               (let* ((pathname (probe-file (build-pathname module :lisp)))
		      (name (namestring pathname)))
                 (if cmp:*implicit-compile-hook*
                     (bformat t "Loading/compiling source: %s\n" (namestring name))
                     (bformat t "Loading/interpreting source: %s\n" (namestring name)))
		 (load pathname)))
           nil)

(defun iload (fn &key load-bitcode )
  #+dbg-print(bformat t "DBG-PRINT iload fn: %s\n" fn)
  (setq *reversed-init-filenames* (cons fn *reversed-init-filenames*))
  (let* ((lsp-path (build-pathname fn))
	 (bc-path (build-pathname fn :bc)) ;; target-backend-pathname (get-pathname-with-type fn "bc") ))
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
	  (bformat t "Loading bitcode file: %s\n" bc-path)
	  (load-bitcode bc-path))
	(if (probe-file-case lsp-path)
	    (progn
              (if cmp:*implicit-compile-hook*
                  (bformat t "Loading/compiling source: %s\n" lsp-path)
                  (bformat t "Loading/interpreting source: %s\n" lsp-path))
	      (load (probe-file lsp-path)))
	    (bformat t "No interpreted or bitcode file for %s could be found\n" lsp-path)))))

(defun delete-init-file (module &key (really-delete t) stage)
  (let ((bitcode-path (build-pathname module :bc stage))) ;; (target-backend-pathname (get-pathname-with-type module "bc"))))
    (if (probe-file bitcode-path)
	(if really-delete
	    (progn
	      (bformat t "     Deleting bitcode: %s\n" bitcode-path)
	      (delete-file bitcode-path))))))

;; I need to search the list rather than using features because *features* may change at runtime
(defun default-target-backend (&optional given-stage)
  (let* ((stage (if given-stage given-stage (if (member :ecl-min *features*) "min" (if (member :cclasp *features*) "cclasp" "full"))))
         (garbage-collector (build-configuration))
         (target-backend (bformat nil "%s-%s" stage garbage-collector)))
    target-backend))

(defvar *target-backend* (default-target-backend))
(export '*target-backend*)

(defun strip-root (l orig-sys)
  (let ((cur l)
        (root orig-sys))
    (tagbody
     top
       (if (and (car cur) (eql (car cur) (car root)))
           (progn
             (setq cur (cdr cur))
             (setq root (cdr root))
             (go top))))
    (if root
        (error "Roots don't match - could not strip all of root ~s from ~s" orig-sys l))
    cur))



(defun target-backend-pathname (pathname &key (target-backend *target-backend*) &allow-other-keys)
  ;;  (if target-backend nil (error "target-backend is nil"))
  (let ((relative (maybe-relative-pathname-to-sys pathname)))
    (merge-pathnames relative (translate-logical-pathname (make-pathname :host target-backend)))))

(export '(default-target-backend target-backend-pathname))

(defun bitcode-exists-and-up-to-date (filename)
  (let* ((source-path (build-pathname filename))
         (bitcode-path (build-pathname filename :bc))
         (found-bitcode (probe-file bitcode-path)))
    (if found-bitcode
        (> (file-write-date bitcode-path)
           (file-write-date source-path))
        nil)))

(defun out-of-date-bitcodes (start end &key (system *system-files*))
  (let ((sources (select-source-files end :first-file start :system system))
        out-of-dates)
    (mapc #'(lambda (f) (if out-of-dates
                            (setq out-of-dates (list* f out-of-dates))
                            (if (not (bitcode-exists-and-up-to-date f))
                                (setq out-of-dates (cons f nil)))))
          sources)
    (nreverse out-of-dates)))

(defun out-of-date-image (image source-files)
  (let* ((last-source (car (reverse source-files)))
         (last-bitcode (build-pathname last-source :bc))
         (image-file image))
    (format t "last-bitcode: ~a~%" last-bitcode)
    (format t "image-file: ~a~%" image-file)
    (if (probe-file image-file)
        (> (file-write-date last-bitcode)
           (file-write-date image-file))
        t)))

(defun compile-kernel-file (filename &key (reload nil) load-bitcode (force-recompile nil))
  #+dbg-print(bformat t "DBG-PRINT compile-kernel-file: %s\n" filename)
;;  (if *target-backend* nil (error "*target-backend* is undefined"))
  (let* ((source-path (build-pathname filename :lisp))
	 (bitcode-path (build-pathname filename :bc)) ;; (target-backend-pathname (get-pathname-with-type filename "bc")))
	 (load-bitcode (and (bitcode-exists-and-up-to-date filename) load-bitcode)))
    (if (and load-bitcode (not force-recompile))
	(progn
	  (bformat t "Skipping compilation of %s - its bitcode file %s is more recent\n" source-path bitcode-path)
	  ;;	  (bformat t "   Loading the compiled file: %s\n" (path-file-name bitcode-path))
	  ;;	  (load-bitcode (as-string bitcode-path))
	  )
	(progn
	  (bformat t "\n")
	  (bformat t "Compiling source %s\n   to %s - will reload: %s\n" source-path bitcode-path reload)
	  (let ((cmp::*module-startup-prefix* "kernel"))
            #+dbg-print(bformat t "DBG-PRINT  source-path = %s\n" source-path)
            (compile-file (probe-file source-path) :output-file bitcode-path :print t :verbose t :output-type :bitcode :type :kernel)
	    (if reload
		(progn
		  (bformat t "    Loading newly compiled file: %s\n" bitcode-path)
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
    :start
    #P"kernel/cmp/jit-setup"
    #P"kernel/clsymbols"
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
    #P"kernel/lsp/seqlib"
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
    #P"kernel/cmp/cmprepl"
    :cmp
    :min
    :cmprepl
    #P"kernel/cmp/cmpwalk"
    #P"kernel/lsp/logging"
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
    #P"modules/sockets/sockets.lisp"
    ;;    asdf/build/asdf
    #P"kernel/cmp/external-clang"
    :front
    #P"kernel/lsp/top"
    :all
;;    lsp;pprint
    ))
(defvar *system-files* *init-files*)
(export '(*system-files* *init-files*))

(defun add-cleavir-to-*system-files* ()
  (let* ((fin (open (build-pathname #P"kernel/cleavir-system" :lisp)))
         cleavir-files)
    (unwind-protect
         (progn
           (setq cleavir-files (read fin)))
      (close fin))
    (setq *system-files* (append *init-files*
                                 (list :bclasp)
                                 cleavir-files
                                 (list :cclasp)))))
(export 'add-cleavir-to-*system-files*)

(defvar *asdf-files*
  '(:init
    #P"kernel/asdf/build/asdf"
    :end))
(export '*asdf-files*)


(defun select-source-files (last-file &key first-file (system *system-files*))
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
     done)
    files))




(defun load-system ( first-file last-file &key interp load-bitcode (target-backend *target-backend*) (system *system-files*))
  #+dbg-print(bformat t "DBG-PRINT  load-system: %s - %s\n" first-file last-file )
  (let* ((*target-backend* target-backend)
         (files (select-source-files last-file :first-file first-file :system system))
	 (cur files))
    (tagbody
     top
       (if (endp cur) (go done))
       (if (not interp)
	   (if (bitcode-exists-and-up-to-date (car cur))
               (iload (car cur) :load-bitcode load-bitcode)
               (progn
                 (setq load-bitcode nil)
                 (interpreter-iload (car cur))))
	   (interpreter-iload (car cur)))
       (gctools:cleanup)
       (setq cur (cdr cur))
       (go top)
     done
       )))


(defun compile-system (files &key reload (system *system-files*))
  #+dbg-print(bformat t "DBG-PRINT compile-system files: %s\n" files)
  (let* ((cur files))
    (tagbody
     top
       (if (endp cur) (go done))
       (compile-kernel-file (car cur) :reload reload )
       (setq cur (cdr cur))
       (go top)
     done)))
(export 'compile-system)


;; Clean out the bitcode files.
;; passing :no-prompt t will not prompt the user
(export 'clean-system)
(defun clean-system (after-file &key no-prompt stage
                                           (system *system-files*))
  (let* ((files (select-trailing-source-files after-file :system system))
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
	     (delete-init-file (car cur) :really-delete t :stage stage )
	     (setq cur (cdr cur))
	     (go top)
	   done)
	  (bformat t "Not deleting\n")))))



(defun default-prologue-form (&optional features)
  `(progn
     ,@(mapcar #'(lambda (f) `(push ,f *features*)) features)
     (if (member :interactive *features*)
         (bformat t "Starting %s ... loading image... it takes a few seconds\n" (lisp-implementation-version)))))

(defun default-epilogue-form ()
  '(progn
    (cl:in-package :cl-user)
    (process-command-line-load-eval-sequence)
    (let ((core:*use-interpreter-for-eval* nil))
      (when (member :interactive *features*) (core:run-repl)))))

(defconstant +minimal-epilogue-form+ '(progn
                                       (process-command-line-load-eval-sequence)
                                       (bformat t "Starting clasp-min low-level-repl\n")
                                       (core::select-package :core)
                                       (core::low-level-repl)))


(defun link-system (start end prologue-form epilogue-form &key (system *system-files*))
  #+dbg-print(bformat t "DBG-PRINT About to link-system\n")
  (let ((bitcode-files (mapcar #'(lambda (x) (build-pathname x :bc)) (select-source-files end :first-file start :system system))))
    (cmp:link-system-lto (build-pathname +image-pathname+ :fasl)
                         :lisp-bitcode-files bitcode-files
                         :prologue-form prologue-form
                         :epilogue-form epilogue-form)))
(export '(link-system)) ;; core

(export '(compile-mino))
(defun compile-min (&key (target-backend (default-target-backend)) (system *system-files*))
  (if (out-of-date-bitcodes :init :cmp)
      (progn
        (load-system :start :cmp :system system)
        (let* ((*target-backend* target-backend)
               (files (out-of-date-bitcodes :init :cmp :system system)))
          (compile-system files :reload t)))))

(export 'link-min)
(defun link-min (&key force)
  (min-features)
  (if (or force (out-of-date-image (build-pathname +image-pathname+ :fasl) (select-source-files :cmp :first-file :init)))
      (progn
        (load-system :start :cmp)
        (link-system :init :cmp (default-prologue-form) +minimal-epilogue-form+))))
                   
(defun recursive-remove-from-list (item list)
  (if list
      (if (equal item (car list))
          (recursive-remove-from-list item (cdr list))
          (list* (car list) (recursive-remove-from-list item (cdr list))))
      nil))
(export 'recursive-remove-from-list)

(defun remove-stage-features ()
  (setq *features* (recursive-remove-from-list :ecl-min *features*))
  (setq *features* (recursive-remove-from-list :clos *features*))
  (setq *features* (recursive-remove-from-list :bclasp *features*))
  (setq *features* (recursive-remove-from-list :cclasp *features*)))

(export 'process-command-line-load-eval-sequence)
(defun process-command-line-load-eval-sequence ()
  (mapcar #'(lambda (entry)
              (if (eq (car entry) :load)
                  (load (cdr entry))
                (eval (read-from-string (cdr entry)))))
          core::*command-line-load-eval-sequence*)
  )


(defun load-clasprc ()
  "Load the users startup code"
  (let ((clasprc (make-pathname :name ""
			       :type "clasprc"
			       :defaults (user-homedir-pathname))))
    (if (probe-file clasprc)
	(load clasprc))))
(export 'load-clasprc)

(export 'min-features)
(defun min-features ()
  (remove-stage-features)
  (setq *features* (list* :ecl-min *features*)))

(export 'bclasp-features)
(defun bclasp-features()
  (remove-stage-features)
  (setq *features* (list* :clos :bclasp *features*)))

(export 'cclasp-features)
(defun cclasp-features ()
  (remove-stage-features)
  (setq *features* (list* :clos :cclasp *features*)))

(export '(compile-bclasp))
(defun compile-bclasp ()
  (bclasp-features)
  (let ((*target-backend* (default-target-backend)))
    (if (out-of-date-bitcodes :init :all)
        (progn
          (load-system :start :all :interp t )
          (let ((files (out-of-date-bitcodes :init :all)))
            (compile-system files))))))
(export 'link-bclasp)
(defun link-bclasp ()
  (bclasp-features)
  (let ((*target-backend* (default-target-backend)))
    (if (out-of-date-image (build-pathname +image-pathname+ :fasl) (select-source-files :all :first-file :init))
        (link-system :init :all (default-prologue-form '(:clos)) (default-epilogue-form)))))

(export '(compile-cclasp))
(defun compile-cclasp ()
  (cclasp-features)
  (add-cleavir-to-*system-files*)
  (let ((*target-backend* (default-target-backend)))
    (if (out-of-date-bitcodes :init :cclasp)
        (progn
          (load-system :bclasp :cclasp :interp t )
          (let ((files (out-of-date-bitcodes :init :cclasp)))
            (compile-system files))))))
(export 'link-cclasp)
(defun link-cclasp (&key force)
  (cclasp-features)
  (add-cleavir-to-*system-files*)
  (let ((*target-backend* (default-target-backend)))
    (if (or force (out-of-date-image (build-pathname +image-pathname+ :fasl) (select-source-files :cclasp :first-file :init)))
        (progn
          (link-system :init :cclasp
                       '(progn
                         (make-package "CLEAVIR-AST")
                         (make-package "CLASP-CLEAVIR-AST")
                         (make-package "CLASP-CLEAVIR")
                         (if (member :clos *features*) nil (setq *features* (cons :clos *features*)))
                         (if (member :cclasp *features*) nil (setq *features* (cons :cclasp *features*)))
                         (if (member :interactive *features*) 
                             (core:bformat t "Starting %s cclasp %s ... loading image... it takes a few seconds\n"
                                           (if (member :use-mps *features*) "MPS" "Boehm" ) (software-version))))
                       '(progn
                         (cl:in-package :cl-user)
                         (require 'system)
                         (core:load-clasprc)
                         (core:process-command-line-load-eval-sequence)
                         (let ((core:*use-interpreter-for-eval* nil))
                           (when (member :interactive *features*) (core:top-level)))))))))

(defun help-build ()
  (bformat t "Useful build commands:\n")
  (bformat t "(load-system from-stage to-stage &key (interp t/nil) (system *system-files*))\n")
  (bformat t "          - Load parts of the system\n")
  (bformat t "(compile-system from-stage to-stage &key :reload t (system *system-files*))\n")
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


(defun load-pos ()
  (declare (special core:*load-current-source-file-info* core:*load-current-linenumber*))
  (bformat t "Load pos: %s %s\n" core:*load-current-source-file-info* core:*load-current-linenumber*))
(export 'load-pos)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Setup the build system for ASDF
;;
;;
(defun setup-asdf () (load "kernel;asdf;build;asdf.lsp"))
(export 'setup-asdf)

(defun compile-asdf ()
  ;; Run make on asdf wherever it is installed
;  (core:system (bformat nil "(cd %s; make)" (namestring (translate-logical-pathname "kernel;asdf;"))))
  (compile-file "kernel;asdf;build;asdf.lisp" :output-file (compile-file-pathname "modules;asdf;asdf.fasl"
										      :target-backend (default-target-backend)))
  #+(or)(cmp::link-system-lto "kernel;asdf;build;asdf.fasl"
			      :lisp-bitcode-files (list #P"kernel/asdf/build/asdf.bc"))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Setup the build system for SICL
;;
(defun setup-cleavir ()
  (load "kernel;asdf;build;asdf.fasl")
  (load "kernel;cleavir;ccmp-all.lsp")
  )

(export 'setup-sicl)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Setup the swank
;;
(defun load-swank ()
  (load "swank.lsp"))
(export '(load-swank))


(defun load-cleavir-system ()
  (let* ((fin (open "kernel;cleavir-system.lsp")))
    (read fin)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Start everything up
;;


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
