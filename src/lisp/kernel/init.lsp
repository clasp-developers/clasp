;; Set features :clasp-min for minimal system without CLOS
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
#+(or)(use-package '(:compiler :ext))

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

;;; Setup a few things for the EXT package
;;; EXT exports
(eval-when (:execute :compile-toplevel :load-toplevel)
  (select-package :ext))
(export '(*module-provider-functions*
          *source-location-kinds*
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
(core:*make-special '*register-with-pde-hook*)
(core:*make-special '*module-provider-functions*)
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


(defun strip-root (pn-dir)
  "Remove the SOURCE-DIR: part of the path in l and then
search for the string 'src', or 'generated' and return the rest of the list that starts with that"
  (let ((rel (cdr (pathname-directory (enough-namestring (make-pathname :directory pn-dir) (translate-logical-pathname #P"SOURCE-DIR:"))))))
    (or (member "src" rel :test #'string=)
        (member "generated" rel :test #'string=)
        (error "Could not find \"src\" or \"generated\" in ~a" rel))))

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
  (let* ((fin (open (build-pathname #P"src/lisp/kernel/cleavir-system" :lisp))))
    (unwind-protect (read fin) (close fin))))

(defun add-cleavir-build-files ()
  (compile-execute-time-value (read-cleavir-system)))

(defvar *build-files*
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
    #P"src/lisp/kernel/asdf/build/asdf"
    :end))
(export '*asdf-files*)


(defun select-source-files (last-file &key first-file (system *system-files*))
  (or first-file (error "You must provide first-file to select-source-files"))
  (or system (error "You must provide system to select-source-files"))
  (let ((cur (member first-file system :test #'equal))
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


(defun compile-system (files &key reload)
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


(export '*extension-startup-loads*) ;; ADDED: frgo, 2016-08-10
(defvar *extension-startup-loads* nil)

(export 'process-extension-loads)
(defun process-extension-loads ()
  (if (not (member :ignore-extensions *features*))
      (mapcar #'(lambda (entry)
                  (if (eq (car entry) 'cl:load)
                      (load (cadr entry))
                      (eval (read-from-string (cdr entry)))))
              core:*extension-startup-loads*)))

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


(defun load-pos ()
  (declare (special core:*load-current-source-file-info* core:*load-current-linenumber*))
  (bformat t "Load pos: %s %s\n" core:*load-current-source-file-info* core:*load-current-linenumber*))
(export 'load-pos)

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
	(load-clasprc)
	(core:top-level))
      (core:low-level-repl)))

#-(or aclasp bclasp cclasp)
(eval-when (:execute)
  (process-command-line-load-eval-sequence)
  (bformat t "Low level repl\n")
  (core:low-level-repl))
