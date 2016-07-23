;; Set features :ecl-min for minimal system without CLOS
;; :clos to compile with CLOS
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (core:select-package "CORE"))

#+(or)(setq *features* (cons :dbg-print *features*))
(SYS:*MAKE-SPECIAL '*echo-repl-tpl-read*)
(export '(*echo-repl-tpl-read* 
          run-repl 
          *load-current-source-file-info* 
          *load-current-linenumber*
          cons-car
          cons-cdr))
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

;;; Turn on compiler warnings for missing features
#+(or)(setq *features* (cons :verbose-compiler *features*))
(setq *features* (cons :compile-mcjit *features*))


;; When boostrapping in stages, set this feature,
;; it guarantees that everything that is declared at compile/eval time
;; gets declared at load-time
;; Turn this off and recompile everything once the system has
;; been bootstrapped
(setq *features* (cons :clasp-boot *features*)) ;; When bootstrapping in stages

;; Set up a few things for the CLOS package
(eval-when (:execute :compile-toplevel :load-toplevel)
  (core::select-package :clos))
(export '(standard-class))

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
(export '(llvm-link link-bitcode-modules))
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
          assume-right-type))
(core:*make-special '*register-with-pde-hook*)
(core:*make-special '*module-provider-functions*)
(export '*module-provider-functions*)
(setq *register-with-pde-hook* ())
(core:*make-special '*source-location*)
(setq *source-location* nil)
(export '*register-with-pde-hook*)
(core:fset 'register-with-pde
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


(si:fset 'core::defvar #'(lambda (whole env)
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
				     (SETQ ,var ,form))))
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

;;; A temporary definition of defun - the real one is in evalmacros
(si:fset 'defun
         #'(lambda (def env)
             (let ((name (second def))          ;cadr
                   (lambda-list (third def))	; caddr
                   (lambda-body (cdddr def)))   ; cdddr
               (multiple-value-call
                   (function (lambda (&optional (decl) (body) (doc) &rest rest)
                     (declare (ignore rest))
                     (if decl (setq decl (list (cons 'declare decl))))
                     (let ((func `#'(lambda ,lambda-list ,@decl ,@doc (block ,name ,@body))))
                       ;;(bformat t "PRIMITIVE DEFUN defun --> %s\n" func )
                       (ext::register-with-pde def `(si:fset ',name ,func nil nil ',lambda-list)))))
                 (si::process-declarations lambda-body nil #| No documentation until the real DEFUN is defined |#))))
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

(defun build-configuration ()
  (let ((gc (cond
              ((member :use-mps *features*) "mps")
              ((member :use-boehmdc *features*) "boehmdc")
              ((member :use-boehm *features*) "boehm")
              (t (error "Unknown clasp configuration")))))
    (bformat nil "%s-%s" (lisp-implementation-type) gc)))

(defun build-intrinsics-bitcode-pathname (link-type)
  (cond
    ((eq link-type :fasl)
     (translate-logical-pathname (bformat nil "lib:%s-intrinsics-cxx.a" +bitcode-name+)))
    ((eq link-type :executable)
     (translate-logical-pathname (bformat nil "lib:%s-all-cxx.a" +bitcode-name+)))
    (t (error "Provide a bitcode file for the link-type ~a" link-type))))

(defun build-common-lisp-bitcode-pathname ()
  (translate-logical-pathname (pathname (bformat nil "lib:%sclasp-%s-common-lisp.bc" (default-target-stage) +variant-name+))))
(export '(build-intrinsics-bitcode-pathname build-common-lisp-bitcode-pathname))
#+(or)
(progn
  (defconstant +image-pathname+ (make-pathname :directory '(:relative) :name "image" :type "fasl"))
  (export '(+image-pathname+ )))

(defun default-target-stage ()
  (if (member :cclasp *features*)
      "c"
      (if (member :bclasp *features*)
          "b"
          "a")))

(defun build-target-dir (type &optional stage)
  (let* ((stage (if stage 
                    stage
                    (default-target-stage)))
         (type-modified-host-suffix (build-configuration))
         (bitcode-host (bformat nil "%s%s" stage type-modified-host-suffix)))
    bitcode-host))


(defun strip-root (l)
  "Search for the string 'kernel', 'module', or 'generated' and return the rest of the list that starts with that"
  (or (member "kernel" l :test #'string=)
      (member "modules" l :test #'string=)
      (member "generated" l :test #'string=)
      (error "Could not find \"kernel\", \"modules\", or \"generated\" in ~a" l)))

(defun ensure-relative-pathname (input)
  "If the input pathname is absolute then search for kernel, module, or generated and return
a relative path from there."
  #+(or)(bformat t "ensure-relative-pathname input = %s   sys-pn = %s\n" input sys-pn)
  (let ((result
         (cond
           ((eq :relative (car (pathname-directory input)))
            (make-pathname :directory (pathname-directory input)
                           :name (pathname-name input)))
           ((eq :absolute (car (pathname-directory input)))
            (make-pathname :directory (cons :relative (strip-root (cdr (pathname-directory input))))
                           :name (pathname-name input)))
           (t (error "ensure-relative-pathname could not handle ~a" input)))))
    #+(or)(bformat t "ensure-relative-pathname result = %s\n" result)
    result))


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
                     (find-lisp-source module (translate-logical-pathname "LISP-SOURCE:"))))))
               ((and partial-pathname (or (eq type :fasl) (eq type :bc)))
                (merge-pathnames (merge-pathnames (ensure-relative-pathname partial-pathname)
                                                  (make-pathname :directory (list :relative target-dir) :type (string-downcase (string type))))
                                 (translate-logical-pathname (make-pathname :host target-host))))
               ((and (null partial-pathname) (eq type :fasl))
                (let* ((stage-char (default-target-stage))
                       (filename (bformat nil "%s%s-%s-image" stage-char +application-name+ +bitcode-name+))
                       (exec-pathname (merge-pathnames (make-pathname :name filename :type "fasl") (translate-logical-pathname "app-executable:"))))
                  exec-pathname))
               ((eq type :executable)
                (let* ((stage-char (default-target-stage))
                       (filename (bformat nil "%s%s-%s" stage-char +application-name+ +bitcode-name+))
                       (exec-pathname (merge-pathnames (make-pathname :name filename :type nil) (translate-logical-pathname "app-executable:") )))
                  exec-pathname))
               (t (error "Add support for build-pathname type: ~a" type)))))
        #+dbg-print(bformat t "DBG-PRINT build-pathname   result: %s\n" result)
        result))))
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



(eval-when (:execute)
  (load (build-pathname #P"kernel/cmp/jit-setup"))
  (load (build-pathname #P"kernel/clsymbols")))

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
	 (bc-path (build-pathname fn :bc))
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
	  (llvm-sys:load-bitcode bc-path))
	(if (probe-file-case lsp-path)
	    (progn
              (if cmp:*implicit-compile-hook*
                  (bformat t "Loading/compiling source: %s\n" lsp-path)
                  (bformat t "Loading/interpreting source: %s\n" lsp-path))
	      (load (probe-file lsp-path)))
	    (bformat t "No interpreted or bitcode file for %s could be found\n" lsp-path)))))

(defun delete-init-file (entry &key (really-delete t) stage)
  (let* ((module (entry-filename entry))
         (bitcode-path (build-pathname module :bc stage))) 
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




#+(or)
(progn
  (defun target-backend-pathname (pathname &key (target-backend *target-backend*) &allow-other-keys)
    ;;  (if target-backend nil (error "target-backend is nil"))
    (let ((relative (ensure-relative-pathname-to-sys pathname)))
      (merge-pathnames relative (translate-logical-pathname (make-pathname :host target-backend)))))
  (export 'target-backend-pathname))



(defun bitcode-exists-and-up-to-date (entry)
  (let* ((filename (entry-filename entry))
         (source-path (build-pathname filename))
         (bitcode-path (build-pathname filename :bc))
         (found-bitcode (probe-file bitcode-path)))
    (if found-bitcode
        (> (file-write-date bitcode-path)
           (file-write-date source-path))
        nil)))

(defun bitcode-pathnames (start end &key (system *system-files*))
  (let ((sources (select-source-files end :first-file start :system system)))
    (mapcar #'(lambda (f &aux (fn (entry-filename f))) (build-pathname fn :bc)) sources)))

(defun print-bitcode-file-names-one-line (start end)
  (mapc #'(lambda (f) (bformat t " %s" (namestring f)))
        (bitcode-pathnames start end))
  nil)

(defun out-of-date-bitcodes (start end &key (system *system-files*))
  (let ((sources (select-source-files end :first-file start :system system))
        out-of-dates)
    (mapc #'(lambda (f) (if out-of-dates
                            (setq out-of-dates (list* f out-of-dates))
                            (if (not (bitcode-exists-and-up-to-date f))
                                (setq out-of-dates (cons f nil)))))
          sources)
    (nreverse out-of-dates)))

(defun source-file-names (start end)
  (let ((sout (make-string-output-stream))
        (cur (select-source-files end :first-file :start))
        pn)
    (tagbody
     top
       (setq pn (car cur))
       (setq cur (cdr cur))
       (bformat sout "%s " (namestring (build-pathname pn :lisp)))
       (if cur (go top)))
    (get-output-stream-string sout)))
  
(defun out-of-date-image (image source-files)
  (let* ((last-source (car (reverse source-files)))
         (last-bitcode (build-pathname (entry-filename last-source) :bc))
         (image-file image))
    (format t "last-bitcode: ~a~%" last-bitcode)
    (format t "image-file: ~a~%" image-file)
    (if (probe-file image-file)
        (> (file-write-date last-bitcode)
           (file-write-date image-file))
        t)))

(defun out-of-date-target (target source-files)
  (let* ((last-source (car (reverse source-files)))
         (cxx-bitcode (build-intrinsics-bitcode-pathname :executable))
         (last-bitcode (build-pathname (entry-filename last-source) :bc))
         (target-file target))
    #+(or)(progn
            (format t "last-bitcode: ~a~%" last-bitcode)
            (format t "target-file: ~a~%" target-file))
    (if (probe-file target-file)
        (or (> (file-write-date last-bitcode)
               (file-write-date target-file))
            (> (file-write-date cxx-bitcode)
               (file-write-date target-file)))
        t)))

(defun compile-kernel-file (entry &key (reload nil) load-bitcode (force-recompile nil) counter total-files)
  #+dbg-print(bformat t "DBG-PRINT compile-kernel-file: %s\n" entry)
;;  (if *target-backend* nil (error "*target-backend* is undefined"))
  (let* ((filename (entry-filename entry))
         (source-path (build-pathname filename :lisp))
	 (bitcode-path (build-pathname filename :bc))
	 (load-bitcode (and (bitcode-exists-and-up-to-date filename) load-bitcode)))
    (if (and load-bitcode (not force-recompile))
	(progn
	  (bformat t "Skipping compilation of %s - its bitcode file %s is more recent\n" source-path bitcode-path)
	  ;;	  (bformat t "   Loading the compiled file: %s\n" (path-file-name bitcode-path))
	  ;;	  (load-bitcode (as-string bitcode-path))
	  )
	(progn
	  (bformat t "\n")
	  (if (and counter total-files)
              (bformat t "Compiling source [%d of %d] %s\n    to %s - will reload: %s\n" counter total-files source-path bitcode-path reload)
              (bformat t "Compiling source %s\n   to %s - will reload: %s\n" source-path bitcode-path reload))
	  (let ((cmp::*module-startup-prefix* "kernel"))
            #+dbg-print(bformat t "DBG-PRINT  source-path = %s\n" source-path)
            (apply #'compile-file (probe-file source-path) :output-file bitcode-path
                   :print t :verbose t :output-type :bitcode :type :kernel (entry-compile-file-options entry))
	    (if reload
		(progn
		  (bformat t "    Loading newly compiled file: %s\n" bitcode-path)
		  (llvm-sys:load-bitcode bitcode-path))))))
    bitcode-path))
(export 'compile-kernel-file)

(eval-when (:compile-toplevel :execute)
  (core:fset 'compile-execute-time-value
             #'(lambda (whole env)
                 (let* ((expression (second whole))
                        (result (eval expression)))
                   `',result))
             t))

(defun read-cleavir-system ()
  (let* ((fin (open (build-pathname #P"kernel/cleavir-system" :lisp))))
    (unwind-protect (read fin) (close fin))))

(defun add-cleavir-build-files ()
  (compile-execute-time-value (read-cleavir-system)))

(defun maybe-insert-epilogue-aclasp ()
  "Insert epilogue if we are compiling aclasp"
  (if (string= (default-target-stage) "a")
      (list (list #P"kernel/lsp/epilogue" (list :epilogue-module-p t)))
      nil))

(defun maybe-insert-epilogue-bclasp ()
  "Insert epilogue if we are compiling bclasp"
  (if (string= (default-target-stage) "b")
      (list (list #P"kernel/lsp/epilogue" (list :epilogue-module-p t)))
      nil))

(defun maybe-insert-epilogue-cclasp ()
  "Insert epilogue if we are compiling cclasp"
  (if (string= (default-target-stage) "c")
      (list (list #P"kernel/lsp/epilogue" (list :epilogue-module-p t)))
      nil))

(defvar *build-files*
  (list
   :init
   #P"kernel/lsp/prologue"
   #P"kernel/lsp/direct-calls"
   #P"generated/cl-wrappers"
   :min-start
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
   #P"kernel/lsp/logging"
   #P"kernel/lsp/trace"
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
   #P"kernel/cmp/lambdalistva"
   #P"kernel/cmp/cmpvars"
   #P"kernel/cmp/cmpquote"
   #P"kernel/cmp/cmpobj"
   ;;    #P"kernel/cmp/mincomp"
   #P"kernel/cmp/compiler"
   #P"kernel/cmp/compilefile"
   #P"kernel/cmp/external-clang"
   #P"kernel/cmp/cmpbundle"
   :pre-repl
   #P"kernel/cmp/cmprepl"
   :cmp-pre-epilogue
   #'maybe-insert-epilogue-aclasp
   :cmp
   :min
   :cmprepl
   #P"kernel/cmp/cmpwalk"
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
    #P"modules/sockets/sockets"
    ;;    asdf/build/asdf
    :front
    #P"kernel/lsp/top"
    #'maybe-insert-epilogue-bclasp
    :all
    :bclasp
    #'add-cleavir-build-files
    #'maybe-insert-epilogue-cclasp
    :cclasp
;;    lsp;pprint
    ))

(defun expand-build-file-list (sources)
  "Copy the list of symbols and pathnames into files
and if S-exps are encountered, funcall them with
no arguments and splice the resulting list into files.
Return files."
  (let* (files
         (cur sources))
    (tagbody
     top
       (if (null cur) (go done))
       (let ((entry (car cur)))
         (if (or (pathnamep entry) (keywordp entry))
             (setq files (cons entry files))
             (if (functionp entry)
                 (let* ((ecur (funcall entry)))
                   (tagbody
                    inner-top
                      (if (null ecur) (go inner-done))
                      (setq files (cons (car ecur) files))
                      (setq ecur (cdr ecur))
                      (go inner-top)
                    inner-done))
                 (error "I don't know what to do with ~a" entry))))
       (setq cur (cdr cur))
       (go top)
     done)
    (nreverse files)))

(defvar *system-files* (expand-build-file-list *build-files*))
(export '(*system-files*))

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
     done)))


(defun compile-system (files &key reload (system *system-files*))
  #+dbg-print(bformat t "DBG-PRINT compile-system files: %s\n" files)
  (with-compilation-unit ()
    (let* ((cur files)
           (counter 1)
           (total (length files)))
      (tagbody
       top
         (if (endp cur) (go done))
         (compile-kernel-file (car cur) :reload reload :counter counter :total-files total )
         (setq cur (cdr cur))
         (setq counter (+ 1 counter))
         (go top)
       done))))
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


#+(or)
(progn
  (defun link-system (start end &key (system *system-files*))
    #+dbg-print(bformat t "DBG-PRINT About to link-system\n")
    (let ((bitcode-files (mapcar #'(lambda (x) (build-pathname (entry-filename x) :bc)) (select-source-files end :first-file start :system system))))
      (cmp:llvm-link (build-pathname +image-pathname+ :executable)
                     :lisp-bitcode-files bitcode-files
                     :link-type :executable)))
  (export '(link-system)))
        
(export '(compile-aclasp source-files-aclasp bitcode-files-aclasp))
(defun source-files-aclasp ()
  (bformat t "%s\n" (source-file-names :min-start :cmp)))
(defun bitcode-files-aclasp ()
  (with-aclasp-features (bformat t "%s\n" (namestring (build-common-lisp-bitcode-pathname)))))
(defun compile-aclasp (&key clean (link-type :bc) (target-backend (default-target-backend)) (system *system-files*))
  (aclasp-features)
  (if clean (clean-system :min-start :no-prompt t))
  (if (out-of-date-bitcodes :min-start :cmp)
      (progn
        (load-system :start :cmp-pre-epilogue :system system)
        (let* ((*target-backend* target-backend)
               (files (out-of-date-bitcodes :min-start :cmp-pre-epilogue :system system))
               (files-with-epilogue (out-of-date-bitcodes :min-start :cmp :system system)))
          (with-compilation-unit ()
            (compile-system files :reload t)
            (if files-with-epilogue (compile-system (bitcode-pathnames :cmp-pre-epilogue :cmp :system system) :reload nil)))
          (let ((cl-bitcode-pathname (build-common-lisp-bitcode-pathname))
                (all-bitcode (bitcode-pathnames :min-start :cmp)))
            (if (out-of-date-target cl-bitcode-pathname all-bitcode)
                (progn
                  (cmp:link-bitcode-modules cl-bitcode-pathname all-bitcode)
                  (if (not (eq link-type :bc))
                      (let ((exec-pathname (build-pathname nil link-type)))
                        (bformat t "Linking aclasp %s\n" (string-downcase (string link-type)))
                        (cmp:llvm-link exec-pathname
                                       :lisp-bitcode-files (list cl-bitcode-pathname)
                                       :link-type link-type))))))))))
                   
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
  (setq *features* (recursive-remove-from-list :aclasp *features*))
  (setq *features* (recursive-remove-from-list :bclasp *features*))
  (setq *features* (recursive-remove-from-list :cclasp *features*)))

(defvar *plugin-startup-loads* nil)
(export 'process-plugin-loads)
(defun process-plugin-loads ()
  (mapcar #'(lambda (entry)
              (if (eq (car entry) 'cl:load)
                  (load (cadr entry))
                  (eval (read-from-string (cdr entry)))))
          core:*plugin-startup-loads*))

(export 'process-command-line-load-eval-sequence)
(defun process-command-line-load-eval-sequence ()
  (mapcar #'(lambda (entry)
              (if (eq (car entry) :load)
                  (load (cdr entry))
                (eval (read-from-string (cdr entry)))))
          core::*command-line-load-eval-sequence*)) 

(defun load-clasprc ()
  "Load the users startup code"
  (let ((clasprc (make-pathname :name ""
			       :type "clasprc"
			       :defaults (user-homedir-pathname))))
    (if (probe-file clasprc)
	(load clasprc))))
(export 'load-clasprc)

(export '(aclasp-features with-aclasp-features))
(defun aclasp-features ()
  (remove-stage-features)
  (setq *features* (list* :aclasp :ecl-min *features*)))
(core:fset 'with-aclasp-features
            #'(lambda (whole env)
                (let* ((body (cdr whole)))
                  `(let ((*features* (list* :aclasp :ecl-min *features*)))
                     ,@body)))
            t)


(export '(compile-link-aclasp))
(defun compile-link-aclasp (&key clean)
  (with-aclasp-features
      (if clean (clean-system :init :no-prompt t))
    (compile-aclasp)))

(export '(bclasp-features with-bclasp-features))
(defun bclasp-features()
  (remove-stage-features)
  (setq *features* (list* :clos :bclasp *features*)))
(core:fset 'with-bclasp-features
            #'(lambda (whole env)
                (let* ((body (cdr whole)))
                  `(let ((*features* (list* :bclasp :clos *features*)))
                     ,@body)))
            t)

(export '(cclasp-features with-cclasp-features))
(defun cclasp-features ()
  (remove-stage-features)
  (setq *features* (list* :clos :cclasp *features*)))
(core:fset 'with-cclasp-features
            #'(lambda (whole env)
                (let* ((body (cdr whole)))
                  `(let ((*features* (list* :cclasp :clos *features*)))
                     ,@body)))
            t)

(export '(load-bclasp-source))
(defun load-bclasp-source ()
  (bclasp-features)
  (let ((*target-backend* (default-target-backend)))
    (load-system :start :all :interp t )))

(export '(compile-bclasp source-files-bclasp bitcode-files-bclasp))
(defun source-files-bclasp ()
  (bformat t "%s\n" (source-file-names :init :all)))
(defun bitcode-files-bclasp ()
  (with-bclasp-features (bformat t "%s\n" (namestring (build-common-lisp-bitcode-pathname)))))
(defun compile-bclasp (&key clean (link-type :bc))
  (bclasp-features)
  (setq *system-files* (expand-build-file-list *build-files*))
  (if clean (clean-system :init :no-prompt t))
  (let ((*target-backend* (default-target-backend)))
    (if (out-of-date-bitcodes :init :all)
        (progn
          (load-system :start :all :interp t )
          (let ((files (out-of-date-bitcodes :init :all)))
            (compile-system files)
            (let ((cl-bitcode-pathname (build-common-lisp-bitcode-pathname))
                  (all-bitcode (bitcode-pathnames :init :all)))
              (if (out-of-date-target cl-bitcode-pathname all-bitcode)
                  (progn
                    (cmp:link-bitcode-modules cl-bitcode-pathname all-bitcode)
                    (if (not (eq link-type :bc))
                        (let ((exec-pathname (build-pathname nil link-type)))
                          (bformat t "Linking bclasp %s\n" (string-downcase (string link-type)))
                          (cmp:llvm-link exec-pathname
                                         :lisp-bitcode-files (list cl-bitcode-pathname)
                                         :link-type link-type)))))))))))

(export '(compile-cclasp recompile-cclasp source-files-cclasp bitcode-files-cclasp))
(defun source-files-cclasp ()
  (bformat t "%s\n" (source-file-names :init :cclasp)))
(defun bitcode-files-cclasp ()
  (with-cclasp-features (bformat t "%s\n" (namestring (build-common-lisp-bitcode-pathname)))))
(defun recompile-cclasp (&key clean (link-type :executable))
  (if clean (clean-system :init :no-prompt t))
  (let ((files (out-of-date-bitcodes :init :cclasp)))
    (compile-system files)
    (let ((cl-bitcode-pathname (build-common-lisp-bitcode-pathname))
          (all-bitcode (bitcode-pathnames :init :cclasp)))
      (if (out-of-date-target cl-bitcode-pathname all-bitcode)
          (let ((exec-pathname (build-pathname nil link-type)))
            (cmp:link-bitcode-modules cl-bitcode-pathname all-bitcode)
            (if (not (eq link-type :bc))
                (progn
                  (bformat t "Linking cclasp %s\n" (string-downcase (string link-type))
                           (cmp:llvm-link exec-pathname
                                          :lisp-bitcode-files (list cl-bitcode-pathname)
                                          :link-type link-type)))))))))

(defun compile-cclasp (&key clean (link-type :executable))
  (cclasp-features)
  (setq *system-files* (expand-build-file-list *build-files*))
  (if clean (clean-system :init :no-prompt t))
  (let ((*target-backend* (default-target-backend)))
    (if (out-of-date-bitcodes :init :cclasp)
        (time
         (progn
           (load-system :bclasp :cclasp :interp t )
           (let ((files (out-of-date-bitcodes :init :cclasp)))
             (compile-system files)
             (let ((cl-bitcode-pathname (build-common-lisp-bitcode-pathname))
                   (all-bitcode (bitcode-pathnames :init :cclasp)))
               (if (out-of-date-target cl-bitcode-pathname all-bitcode)
                   (let ((exec-pathname (build-pathname nil link-type)))
                     (cmp:link-bitcode-modules cl-bitcode-pathname all-bitcode)
                     (if (not (eq link-type :bc))
                         (progn
                           (bformat t "Linking cclasp %s\n" (string link-type))
                           (cmp:llvm-link exec-pathname
                                          :lisp-bitcode-files (list cl-bitcode-pathname)
                                          :link-type link-type))))))))))))

(defun tpl-default-pathname-defaults-command ()
  (print *default-pathname-defaults*))

(defun tpl-change-default-pathname-defaults-dir-command (raw-dir)
  (let* ((corrected-dir (format nil "~a/" (string-right-trim "/" (string raw-dir))))
	 (dir (pathname-directory (parse-namestring corrected-dir)))
	 (pn-dir (mapcar #'(lambda (x) (if (eq x :up) :back x)) dir))
	 (new-pathname (merge-pathnames (make-pathname :directory pn-dir) *default-pathname-defaults*))
	 )
    (setq *default-pathname-defaults* new-pathname)))


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


(defun load-pos ()
  (declare (special core:*load-current-source-file-info* core:*load-current-linenumber*))
  (bformat t "Load pos: %s %s\n" core:*load-current-source-file-info* core:*load-current-linenumber*))
(export 'load-pos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Setup the build system for ASDF
;;
;;

(defun compile-addons ()
  ;; Build serve-event and asdf
  (core:compile-kernel-file #P"modules/serve-event/serve-event" :force-recompile t)
  (core:compile-kernel-file #P"modules/asdf/build/asdf" :force-recompile t))

(defun link-addons ()
  (cmp:llvm-link (core:build-pathname #P"modules/serve-event/serve-event" :fasl)
                 :lisp-bitcode-files (list (core:build-pathname #P"modules/serve-event/serve-event" :bc)))
  (cmp:llvm-link (core:build-pathname #P"modules/asdf/asdf" :fasl)
                 :lisp-bitcode-files (list (core:build-pathname #P"modules/asdf/build/asdf" :bc))))
(export '(compile-addons link-addons))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Setup the build system for SICL
;;
(defun setup-cleavir ()
  (load "kernel;asdf;build;asdf.fasl")
  (load "kernel;cleavir;ccmp-all.lsp"))

(export 'setup-sicl)

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
	(load-clasprc)
	(core:top-level))
      (core:low-level-repl)))
  
#-(or aclasp bclasp cclasp)
(eval-when (:execute)
  (process-command-line-load-eval-sequence)
  (bformat t "Low level repl\n")
  (core:low-level-repl))
