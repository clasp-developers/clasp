;; Set features :clasp-min for minimal system without CLOS
;; :clos to compile with CLOS
;;


(eval-when (:compile-toplevel :load-toplevel :execute)
  (core:select-package "CORE"))


#+(or)(setq *features* (cons :dbg-print *features*))
(SYS:*MAKE-SPECIAL '*echo-repl-tpl-read*)
(export '(*echo-repl-tpl-read*
          run-repl
          cons-car
          cons-cdr))
(sys:*make-special 'core::*dump-defmacro-definitions*)
(setq *dump-defmacro-definitions* nil)
(sys:*make-special 'core::*dump-defun-definitions*)
(setq *dump-defun-definitions* nil)
(export '*trace-startup*)

;;; ------------------------------------------------------------
;;;
;;;   Turn on flow tracker
;;;

#+debug-flow-tracker
(if (member :flow-tracker *features*)
    (progn
      (core:bformat t "Turning flow-tracker on\n")
      (gctools:flow-tracker-on)))


;;; ------------------------------------------------------------
;;;
;;; Set *echo-repl-read* to t to print each repl form
;;;
(setq *echo-repl-read* nil)

(setq *echo-repl-tpl-read* (member :emacs-inferior-lisp *features*))
(setq *load-print* nil)

(sys:*make-special 'core::*boot-verbose*)
(setq core::*boot-verbose* nil)
(setq cl:*print-circle* nil)

(sys:*make-special 'core::*clang-bin*)
(setq core::*clang-bin* (ext:getenv "CLASP_CLANG_PATH"))
(export 'core::*clang-bin*)


;; When boostrapping in stages, set this feature,
;; it guarantees that everything that is declared at compile/eval time
;; gets declared at load-time
;; Turn this off and recompile everything once the system has
;; been bootstrapped
(setq *features* (cons :clasp-boot *features*)) ;; When bootstrapping in stages

;; Set up a few things for the CLOS package
(export '(clos::standard-class) "CLOS")

;; Setup a few things for the GRAY streams package
(eval-when (:execute :compile-toplevel :load-toplevel)
  (core::select-package :gray))
(shadow '(STREAM-ELEMENT-TYPE OPEN-STREAM-P OUTPUT-STREAM-P INPUT-STREAM-P STREAMP CLOSE))

;; Setup a few things for the CORE package
(eval-when (:execute :compile-toplevel :load-toplevel)
  (core::select-package :core))

(sys:*make-special '*use-cleavir-compiler*)
(sys:*make-special '*eval-with-env-hook*)

;; Setup a few things for the CMP package
(eval-when (:execute :compile-toplevel :load-toplevel)
  (core::select-package :cmp))
(export '(llvm-link link-bitcode-modules))
;;; Turn on aclasp/bclasp activation-frame optimization
(sys:*make-special '*activation-frame-optimize*)
(setq *activation-frame-optimize* t)
#-dont-optimize-bclasp (setq *features* (cons :optimize-bclasp *features*))
(sys:*make-special '*use-human-readable-bitcode*)
(setq *use-human-readable-bitcode* (member :use-human-readable-bitcode *features*))
(sys:*make-special '*compile-file-debug-dump-module*)
(sys:*make-special '*debug-compile-file*)
(if (boundp '*compile-file-debug-dump-module*)
    nil
    (setq *compile-file-debug-dump-module* t))
(sys:*make-special '*compile-debug-dump-module*)
(if (boundp '*compile-debug-dump-module*)
    nil
    (setq *compile-debug-dump-module* nil))
(setq *debug-compile-file* (member :debug-compile-file *features*))
(export '(*compile-file-debug-dump-module* *compile-debug-dump-module*))
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

(if (find-package "CLASP-CLEAVIR")
    nil
    (make-package "CLASP-CLEAVIR" :use '(:CL)))

;;; Setup a few things for the EXT package
;;; EXT exports
(eval-when (:execute :compile-toplevel :load-toplevel)
  (select-package :ext))

(export '(*module-provider-functions*
          *source-location-kinds*
          current-source-location
          source-location
          source-location-pathname
          source-location-offset
          where
          compiled-function-name
          compiled-function-file
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
          assume-right-type))
(core:*make-special '*module-provider-functions*)
(core:*make-special '*source-location*)
(setq *source-location* nil)
(export 'current-source-location)
;;; Macro: (EXT:CURRENT-SOURCE-LOCATION)
;;; - Returns the source location of the current top-level form
;;;   or nil if it's not known.
(core:fset
 'current-source-location
 #'(lambda ()
     (block cur-src-loc
       (if core:*source-database*
           (let ((cur core:*top-level-form-stack*))
             (tagbody
              top
                (if (null cur) (return-from cur-src-loc nil))
                (let ((location (core:source-manager-lookup core:*source-database* (car cur))))
                  (if location (return-from cur-src-loc location)))
                (setq cur (cdr cur))
                (go top)))))))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (core:select-package :core))


(si:fset 'core::defvar #'(lambda (whole env)
			     (let ((var (cadr whole))
				   (formp (cddr whole))
				   (form (caddr whole))
				   (doc-string (cadddr whole)))
				  "Syntax: (defvar name form [doc])
Declares the global variable named by NAME as a special variable and assigns
the value of FORM to the variable.  The doc-string DOC, if supplied, is saved
as a VARIABLE doc and can be retrieved by (documentation 'NAME 'variable)."
				  `(LOCALLY (DECLARE (SPECIAL ,var))
				     (SYS:*MAKE-SPECIAL ',var)
				     ,@(if formp
					     `((if (boundp ',var)
						   ',var
						   (progn
                                                     (setq ,var ,form)
                                                     ',var)))))))
	  t )
(export 'defvar)

(si:fset 'core::defparameter #'(lambda (whole env)
			    (let ((var (cadr whole))
				  (form (caddr whole))
				  (doc-string (cadddr whole)))
				  "Syntax: (defparameter name form [doc])
Declares the global variable named by NAME as a special variable and assigns
the value of FORM to the variable.  The doc-string DOC, if supplied, is saved
as a VARIABLE doc and can be retrieved by (documentation 'NAME 'variable)."
				  `(LOCALLY (DECLARE (SPECIAL ,var))
				     (SYS:*MAKE-SPECIAL ',var)
				     (SETQ ,var ,form)
                                     ',var)))
	  t )
(export 'defparameter)



(si:fset 'core::defconstant #'(lambda (whole env)
			    (let ((var (cadr whole))
				  (form (caddr whole))
				  (doc-string (cadddr whole)))
				  "Syntax: (defconstant name form [doc])
Declares the global variable named by NAME as a special variable and assigns
the value of FORM to the variable.  The doc-string DOC, if supplied, is saved
as a VARIABLE doc and can be retrieved by (documentation 'NAME 'variable)."
                              `(if (core:symbol-constantp ',var)
                                   nil
                                   (progn
                                     (set ',var ,form)
                                     (funcall #'(setf core:symbol-constantp) t ',var)))))
	  t )
(export 'defconstant)

(if (boundp '+ecl-safe-declarations+)
    nil ; don't redefine constant
    (defconstant +ecl-safe-declarations+
      '(optimize (safety 2) (speed 1) (debug 1) (space 1))))




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
     si:*print-structure*
     si::*sharp-eq-context*
     si:*circle-counter*)
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
(export 'bind-va-list)

(defparameter *debug-bclasp* (member :debug-bclasp-lisp *features*))

(defvar *special-init-defun-symbol* (gensym "special-init-defun-symbol"))

;;; A temporary definition of defun - the real one is in evalmacros
#+clasp-min
(eval-when (:execute)
  (si:fset 'defun
           #'(lambda (def env)
               (let ((name (second def))      ;cadr
                     (lambda-list (third def)) ; caddr
                     (lambda-body (cdddr def))) ; cdddr
                 (multiple-value-call
                     (function (lambda (&optional (decl) (body) (doc) &rest rest)
                       (declare (ignore rest))
                       (if decl (setq decl (list (cons 'declare decl))))
                       (let ((func `#'(lambda ,lambda-list ,@decl ,@doc (block ,(si::function-block-name name) ,@body))))
                         ;;(bformat t "PRIMITIVE DEFUN defun --> %s\n" func )
                          `(progn (eval-when (:compile-toplevel)
                                    (cmp::register-global-function-def 'defun ',name))
                                  (si:fset ',name ,func nil ',lambda-list)))))
                   (si::process-declarations lambda-body nil #| No documentation until the real DEFUN is defined |#))))
           t))

(export '(defun))

;;; Define these here so that Cleavir can do inlining
(defvar *defun-inline-hook* nil)
(defvar *inline-on* nil)
(defvar *do-inline-hook* nil)
(defvar *proclaim-hook* nil)
(export '(*defun-inline-hook*
          *do-inline-hook*
          *inline-on*
          *proclaim-hook*))

;; Discard documentation until helpfile.lsp is loaded
(defun set-documentation (o d s) nil)

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

(defun split-at-white-space (s) (split s " "))

(defun default-link-flags ()
  "Return the link flags and the library dir where libLTO.<library-extension> can be found and the library extension"
  (let (err error-msg stream)
    (multiple-value-setq (err error-msg stream)
      (ext:vfork-execvp (list "llvm-config" "--ldflags" "--libdir" "--libs") t))
    (let* ((ldflags (split-at-white-space (read-line stream)))
           #+(or)(clasp-lib-dir (bformat nil "-L%s" (namestring (translate-logical-pathname "app-resources:lib;common;lib;"))))
           (libdir (read-line stream))
           (libdir-flag (list (bformat nil "-L%s" libdir)))
           (libs (split-at-white-space (read-line stream)))
           (build-lib (split-at-white-space *build-lib*))
           (build-stlib (split-at-white-space *build-stlib*))
           (build-linkflags (split-at-white-space *build-linkflags*))
           (link-flags (append ldflags #+(or)(list clasp-lib-dir) build-linkflags libdir-flag libs build-stlib build-lib)))
      (close stream)
      (if (or (member :use-boehm *features*) (member :use-boehmdc *features*))
          (setq link-flags (cons "-lgc" link-flags)))
      (let ((library-extension (if (member :target-os-darwin *features*)
                                   "dylib"
                                   "so")))
        (values link-flags libdir library-extension)))))

(defun link-flags ()
  (default-link-flags))
(export 'link-flags)


(si:fset 'and
	   #'(lambda (whole env)
	       (let ((forms (cdr whole)))
		 (if (null forms)
		     t
		     (if (null (cdr forms))
			 (car forms)
			 `(if ,(car forms)
			      (and ,@(cdr forms)))))))
	   t)

(si:fset 'or
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

(defun build-target-dir (type &optional stage)
  (let* ((stage (if stage
                    stage
                    (default-target-stage)))
         (type-modified-host-suffix (build-configuration))
         (bitcode-host (bformat nil "%s%s-bitcode" stage type-modified-host-suffix)))
    bitcode-host))

(defun default-target-stage ()
  (let ((stage (if (member :cclasp *features*)
                   "c"
                   (if (member :bclasp *features*)
                       (if (member :compiling-cleavir *features*)
                           "pre"
                           "b")
                       "a"))))
    stage))


(defun build-configuration ()
  (let ((gc (cond
              ((member :use-mps *features*) "mps")
              ((member :use-boehmdc *features*) "boehmdc")
              ((member :use-boehm *features*) "boehm")
              (t (error "Unknown clasp configuration")))))
    (bformat nil "%s-%s" (lisp-implementation-type) gc)))

(defun ensure-relative-pathname (input)
  "If the input pathname is absolute then search for src, or generated and return
a relative path from there."
  #+(or)(bformat t "ensure-relative-pathname input = %s   sys-pn = %s\n" input sys-pn)
  (let ((result
         (cond
           ((eq :relative (car (pathname-directory input)))
            (make-pathname :directory (pathname-directory input)
                           :name (pathname-name input)))
           ((eq :absolute (car (pathname-directory input)))
            (make-pathname :directory (cons :relative (strip-root (pathname-directory input)))
                           :name (pathname-name input)))
           (t (error "ensure-relative-pathname could not handle ~a" input)))))
    #+(or)(bformat t "ensure-relative-pathname result = %s\n" result)
    result))


(defun build-inline-bitcode-pathname (link-type &optional (filetype :intrinsics))
  (let ((name (cond
                ((eq filetype :intrinsics) "intrinsics")
                ((eq filetype :builtins) "builtins")
                ((eq filetype :fastgf) "fastgf")
                (t (error "illegal filetype - only :intrinsics, :builtins or :fastgf allowed")))))
    (cond
      ((eq link-type :fasl)
       (translate-logical-pathname (bformat nil "lib:%s-%s-cxx.a" +bitcode-name+ name)))
      ((eq link-type :compile)
       (translate-logical-pathname (bformat nil "app-bitcode:%s-%s-cxx.bc" +bitcode-name+ name)))
      ((eq link-type :executable)
       (translate-logical-pathname (bformat nil "lib:%s-all-cxx.a" +bitcode-name+)))
      (t (error "Provide a bitcode file for the link-type ~a" link-type)))))

(defun build-common-lisp-bitcode-pathname ()
  (translate-logical-pathname (pathname (bformat nil "lib:%sclasp-%s-common-lisp.bc" (default-target-stage) +variant-name+))))
(export '(build-inline-bitcode-pathname build-common-lisp-bitcode-pathname))
#+(or)
(progn
  (defconstant +image-pathname+ (make-pathname :directory '(:relative) :name "image" :type "fasl"))
  (export '(+image-pathname+ )))
(defun bitcode-extension ()
  (if cmp::*use-human-readable-bitcode*
      "ll"
      "bc"))
(export 'bitcode-extension)

(defun build-extension (type)
  (if (eq type :fasl)
      "fasl"
      (if (eq type :bitcode)
          "bc"
          (if (eq type :ll)
              "ll"
              (error "Unsupported build-extension type ~a" type)))))

(defun build-pathname (partial-pathname &optional (type :lisp) stage)
  "If partial-pathname is nil and type is :fasl or :executable then construct the name using
the stage, the +application-name+ and the +bitcode-name+"
  (flet ((find-lisp-source (module root)
           (or
            (probe-file (merge-pathnames (merge-pathnames module (make-pathname :type "lsp")) root))
            (probe-file (merge-pathnames (merge-pathnames module (make-pathname :type "lisp")) root))
            (error "Could not find a lisp source file with root: ~a module: ~a" root module))))
    (let ((target-host "lib")
          (target-dir (build-target-dir type stage))
          pn)
      #+dbg-print(bformat t "DBG-PRINT build-pathname module: %s\n" module)
      #+dbg-print(bformat t "DBG-PRINT build-pathname target-host: %s\n" target-host)
      #+dbg-print(bformat t "DBG-PRINT build-pathname target-dir: %s\n" target-dir)
      (let ((result
              (cond
                ((eq type :lisp)
                 (let ((module (ensure-relative-pathname partial-pathname)))
                   (cond
                     ((string= "generated" (second (pathname-directory module)))
                      ;; Strip the "generated" part of the directory
                      (find-lisp-source (make-pathname
                                         :directory (cons :relative (cddr (pathname-directory module)))
                                         :name (pathname-name module))
                                        (translate-logical-pathname "GENERATED:")))
                     (t
                      (find-lisp-source module (translate-logical-pathname "SOURCE-DIR:"))))))
                ((and partial-pathname (or (eq type :fasl) (eq type :bitcode)))
                 (if cmp::*use-human-readable-bitcode* (setq type :ll))
                 (merge-pathnames (merge-pathnames (ensure-relative-pathname partial-pathname)
                                                   (make-pathname :directory (list :relative target-dir) :type (build-extension type)))
                                  (translate-logical-pathname (make-pathname :host target-host))))
                ((and (null partial-pathname) (eq type :fasl))
                 (let* ((stage-char (default-target-stage))
                        (filename (bformat nil "%s%s-%s-image" stage-char +application-name+ +bitcode-name+))
                        (exec-pathname (merge-pathnames (make-pathname :name filename :type "fasl") (translate-logical-pathname "app-fasl:"))))
                   exec-pathname))
                ((eq type :executable)
                 (let* ((stage-char (default-target-stage))
                        (filename (bformat nil "%s%s-%s" stage-char +application-name+ +bitcode-name+))
                        (exec-pathname (merge-pathnames (make-pathname :name filename :type nil) (translate-logical-pathname "app-executable:") )))
                   exec-pathname))
                (t (error "Add support for build-pathname type: ~a" type)))))
        result))))
(export '(build-pathname))


(eval-when (:execute)
  (load (build-pathname #P"src/lisp/kernel/cmp/jit-setup"))
  (load (build-pathname #P"src/lisp/kernel/clsymbols")))

(defun entry-filename (filename-or-cons)
  "If filename-or-cons is a list then the first entry is a filename"
  (if (consp filename-or-cons)
      (car filename-or-cons)
      filename-or-cons))

(defun entry-compile-file-options (entry)
  (if (consp entry)
      (cadr entry)
      nil))

(defun interpreter-iload (entry)
  (let* ((filename (entry-filename entry))
         (pathname (probe-file (build-pathname filename :lisp)))
         (name (namestring pathname)))
    (if cmp:*implicit-compile-hook*
        (bformat t "Loading/compiling source: %s\n" (namestring name))
        (bformat t "Loading/interpreting source: %s\n" (namestring name)))
    (load pathname)))

(defun iload (entry &key load-bitcode )
  #+dbg-print(bformat t "DBG-PRINT iload fn: %s\n" fn)
  (let* ((fn (entry-filename entry))
         (lsp-path (build-pathname fn))
	 (bc-path (build-pathname fn :bitcode))
	 (load-bc (if (not (probe-file lsp-path))
		      t
		      (if (not (probe-file bc-path))
                          nil
			  (if load-bitcode
			      t
			      (let ((bc-newer (> (file-write-date bc-path) (file-write-date lsp-path))))
				bc-newer))))))
    (if load-bc
	(progn
	  (bformat t "Loading bitcode file: %s\n" bc-path)
	  (cmp:load-bitcode bc-path))
	(if (probe-file-case lsp-path)
	    (progn
              (if cmp:*implicit-compile-hook*
                  (bformat t "Loading/compiling source: %s\n" lsp-path)
                  (bformat t "Loading/interpreting source: %s\n" lsp-path))
	      (load (probe-file lsp-path)))
	    (bformat t "No interpreted or bitcode file for %s could be found\n" lsp-path)))))

(defun delete-init-file (entry &key (really-delete t) stage)
  (let* ((module (entry-filename entry))
         (bitcode-path (build-pathname module :bitcode stage)))
    (if (probe-file bitcode-path)
	(if really-delete
	    (progn
	      (bformat t "     Deleting bitcode: %s\n" bitcode-path)
	      (delete-file bitcode-path))))))


;; I need to search the list rather than using features because *features* may change at runtime
(defun default-target-backend (&optional given-stage)
  (let* ((stage (if given-stage
                    given-stage
                    (default-target-stage)))
         (garbage-collector (build-configuration))
         (target-backend (bformat nil "%s%s" stage garbage-collector)))
    target-backend))
(export 'default-target-backend)

(defvar *target-backend* (default-target-backend))
(export '*target-backend*)

(defun bitcode-exists-and-up-to-date (entry)
  (let* ((filename (entry-filename entry))
         (source-path (build-pathname filename))
         (bitcode-path (build-pathname filename :bitcode))
         (found-bitcode (probe-file bitcode-path)))
    (if found-bitcode
        (> (file-write-date bitcode-path)
           (file-write-date source-path))
        nil)))


(defun read-cleavir-system ()
  (let* ((fin (open (build-pathname #P"src/lisp/kernel/cleavir-system" :lisp))))
    (unwind-protect (read fin) (close fin))))

(defun add-cleavir-build-files ()
  (compile-execute-time-value (read-cleavir-system)))

#+(or)(defvar *build-files*
        (list
         #P"src/lisp/kernel/tag/start"
         #P"src/lisp/kernel/lsp/prologue"
         #P"src/lisp/kernel/lsp/direct-calls"
         #P"generated/cl-wrappers"
         #P"src/lisp/kernel/tag/min-start"
         #P"src/lisp/kernel/init"
         #P"src/lisp/kernel/tag/after-init"
         #P"src/lisp/kernel/cmp/jit-setup"
         #P"src/lisp/kernel/clsymbols"
         #P"src/lisp/kernel/lsp/packages"
         #P"src/lisp/kernel/lsp/foundation"
         #P"src/lisp/kernel/lsp/export"
         #P"src/lisp/kernel/lsp/defmacro"
         #P"src/lisp/kernel/lsp/helpfile"
         #P"src/lisp/kernel/lsp/source-location"
         #P"src/lisp/kernel/lsp/evalmacros"
         #P"src/lisp/kernel/lsp/claspmacros"
         #P"src/lisp/kernel/lsp/source-transformations"
         #P"src/lisp/kernel/lsp/testing"
         #P"src/lisp/kernel/lsp/arraylib"
         #P"src/lisp/kernel/lsp/setf"
         #P"src/lisp/kernel/lsp/listlib"
         #P"src/lisp/kernel/lsp/mislib"
         #P"src/lisp/kernel/lsp/defstruct"
         #P"src/lisp/kernel/lsp/predlib"
         #P"src/lisp/kernel/lsp/seq"
         #P"src/lisp/kernel/lsp/cmuutil"
         #P"src/lisp/kernel/lsp/seqmacros"
         #P"src/lisp/kernel/lsp/seqlib"
         #P"src/lisp/kernel/lsp/iolib"
         #P"src/lisp/kernel/lsp/logging"
         #P"src/lisp/kernel/lsp/trace"
         #P"src/lisp/kernel/cmp/packages"
         #P"src/lisp/kernel/cmp/cmpsetup"
         #P"src/lisp/kernel/cmp/cmpglobals"
         #P"src/lisp/kernel/cmp/cmptables"
         #P"src/lisp/kernel/cmp/cmpvar"
         #P"src/lisp/kernel/cmp/cmputil"
         #P"src/lisp/kernel/cmp/cmpintrinsics"
         #P"src/lisp/kernel/cmp/cmpir"
         #P"src/lisp/kernel/cmp/cmpeh"
         #P"src/lisp/kernel/cmp/debuginfo"
         #P"src/lisp/kernel/cmp/lambdalistva"
         #P"src/lisp/kernel/cmp/cmpvars"
         #P"src/lisp/kernel/cmp/cmpquote"
         #P"src/lisp/kernel/cmp/cmpobj"
         #P"src/lisp/kernel/cmp/compiler"
         #P"src/lisp/kernel/cmp/compilefile"
         #P"src/lisp/kernel/cmp/external-clang"
         #P"src/lisp/kernel/cmp/cmpbundle"
         #P"src/lisp/kernel/cmp/cmprepl"
         #P"src/lisp/kernel/tag/min-pre-epilogue"
         #P"src/lisp/kernel/lsp/epilogue-aclasp"
         #P"src/lisp/kernel/tag/min-end"
         #P"src/lisp/kernel/cmp/cmpwalk"
         #P"src/lisp/kernel/lsp/sharpmacros"
         #P"src/lisp/kernel/lsp/assert"
         #P"src/lisp/kernel/lsp/numlib"
         #P"src/lisp/kernel/lsp/describe"
         #P"src/lisp/kernel/lsp/module"
         #P"src/lisp/kernel/lsp/loop2"
         #P"src/lisp/kernel/lsp/shiftf-rotatef"
         #P"src/lisp/kernel/lsp/assorted"
         #P"src/lisp/kernel/lsp/packlib"
         #P"src/lisp/kernel/lsp/defpackage"
         #P"src/lisp/kernel/lsp/format"
         #P"src/lisp/kernel/clos/package"
         #P"src/lisp/kernel/clos/hierarchy"
         #P"src/lisp/kernel/clos/cpl"
         #P"src/lisp/kernel/clos/std-slot-value"
         #P"src/lisp/kernel/clos/slot"
         #P"src/lisp/kernel/clos/boot"
         #P"src/lisp/kernel/clos/kernel"
         #P"src/lisp/kernel/clos/method"
         #P"src/lisp/kernel/clos/combin"
         #P"src/lisp/kernel/clos/std-accessors"
         #P"src/lisp/kernel/clos/defclass"
         #P"src/lisp/kernel/clos/slotvalue"
         #P"src/lisp/kernel/clos/standard"
         #P"src/lisp/kernel/clos/builtin"
         #P"src/lisp/kernel/clos/change"
         #P"src/lisp/kernel/clos/stdmethod"
         #P"src/lisp/kernel/clos/generic"
         #P"src/lisp/kernel/clos/fixup"
         #P"src/lisp/kernel/clos/extraclasses"
         #P"src/lisp/kernel/lsp/defvirtual"
         #P"src/lisp/kernel/clos/conditions"
         #P"src/lisp/kernel/clos/print"
         #P"src/lisp/kernel/clos/streams"
         #P"src/lisp/kernel/lsp/pprint"
         #P"src/lisp/kernel/clos/inspect"
         #P"src/lisp/kernel/lsp/ffi"
         #P"src/lisp/modules/sockets/sockets"
         #P"src/lisp/kernel/lsp/top"
         #P"src/lisp/kernel/lsp/epilogue-bclasp"
         #P"src/lisp/kernel/tag/bclasp"
         #'add-cleavir-build-files
         #P"src/lisp/kernel/lsp/epilogue-cclasp"
         #P"src/lisp/kernel/tag/cclasp"
         ))


#+(or)(defvar *system-files* (expand-build-file-list *build-files*))
#+(or)(export '(*system-files*))




(defun default-prologue-form (&optional features)
  `(progn
     ,@(mapcar #'(lambda (f) `(push ,f *features*)) features)
     (if (member :interactive *features*)
         (bformat t "Starting %s ... loading image... it takes a few seconds\n" (lisp-implementation-version)))))


(export '*extension-startup-loads*) ;; ADDED: frgo, 2016-08-10
(defvar *extension-startup-loads* nil)

(export 'process-extension-loads)
(defun process-extension-loads ()
  (if (not (member :ignore-extensions *features*))
      (mapcar #'(lambda (entry)
                  (if (eq (car entry) 'cl:load)
                      (load (cadr entry))
                      (let ((cmd (read-from-string (cdr entry))))
                        (apply (car cmd) (cdr cmd)))))
              core:*extension-startup-loads*)))

(export 'process-command-line-load-eval-sequence)
(defun process-command-line-load-eval-sequence ()
  (mapcar #'(lambda (entry)
              (if (eq (car entry) :load)
                  (load (cdr entry))
                  (let ((cmd (read-from-string (cdr entry))))
                    (eval cmd))))
          core::*command-line-load-eval-sequence*))

(export 'maybe-load-clasprc)
(defun maybe-load-clasprc ()
  "Maybe load the users startup code"
  (if (not (member :no-rc *features*))
      (let ((clasprc (make-pathname :name ""
                                    :type "clasprc"
                                    :defaults (user-homedir-pathname))))
        (if (probe-file clasprc)
            (load clasprc)))))

(defun tpl-default-pathname-defaults-command ()
  (print *default-pathname-defaults*))

(defun tpl-change-default-pathname-defaults-dir-command (raw-dir)
  (let* ((corrected-dir (format nil "~a/" (string-right-trim "/" (string raw-dir))))
	 (dir (pathname-directory (parse-namestring corrected-dir)))
	 (pn-dir (mapcar #'(lambda (x) (if (eq x :up) :back x)) dir))
	 (new-pathname (merge-pathnames (make-pathname :directory pn-dir) *default-pathname-defaults*))
	 )
    (setq *default-pathname-defaults* new-pathname)))


;;; I moved the build system code out of init.lsp and
;;; put it in clasp-builder.lsp

(if (member :clasp-builder *features*)
    (load "source-dir:src;lisp;kernel;clasp-builder.lsp"))


(defun tpl-hook (cmd)
  (cond
    ((eq (car cmd) :pwd) (tpl-default-pathname-defaults-command))
    ((eq (car cmd) :cd) (tpl-change-default-pathname-defaults-dir-command (cadr cmd)))
    (t (bformat t "Unknown command %s\n" cmd))))

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
(si:fset 'my-time
           #'(lambda (def env)
               (let ((form (cadr def)))
                 `(my-do-time #'(lambda () ,form))))
           t)
(export 'my-time)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Setup the build system for SICL
;;
(defun setup-cleavir ()
  (load "src;lisp;kernel;asdf;build;asdf.fasl")
  (load "src;lisp;cleavir;ccmp-all.lsp"))

(export 'setup-sicl)

(defun load-cleavir-system ()
  (let* ((fin (open "src;lisp;kernel;cleavir-system.lsp")))
    (read fin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Start everything up
;;

(export 'core:top-level)
(defun run-repl ()
  (if (fboundp 'core:top-level)
      (progn
	(maybe-load-clasprc)
	(core:top-level))
      (core:low-level-repl)))

#-(or aclasp bclasp cclasp)
(eval-when (:execute)
  (process-command-line-load-eval-sequence)
  (bformat t "Low level repl - in init.lsp\n")
  (core:low-level-repl))

#-(or bclasp cclasp)
(eval-when (:execute :load-top-level)
  (bformat t "init.lsp  \n!\n!\n! Hello from the bottom of init.lsp - for some reason execution is passing through here\n!\n!\n"))
