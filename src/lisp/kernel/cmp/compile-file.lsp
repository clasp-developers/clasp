(in-package #:cmp)

;;;; Top level interface: COMPILE-FILE, etc.

;;; Use the *cleavir-compile-file-hook* to determine which compiler to use
;;; if nil == bclasp. Code for the bclasp compiler is in codegen-toplevel.lsp;
;;; look for t1expr.

(defvar *compile-verbose* nil)
(defvar *compile-print* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Source database

(defparameter *active-compilation-source-database* nil)
(defun do-one-source-database (closure)
  (if (null *active-compilation-source-database*)
      (let ((*active-compilation-source-database* t)
            (core:*source-database* (core:make-source-manager)))
        (do-one-source-database closure))
      (unwind-protect
           (funcall closure)
        (core:source-manager-empty core:*source-database*))))
(defmacro with-one-source-database (&rest body)
  `(do-one-source-database #'(lambda () ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Describing top level forms (for compile-verbose)

(defun describe-form (form)
  ;; We could be smarter about this. For example, for (progn ...) nothing very interesting
  ;; will print, even though subforms are just as toplevel.
  ;; But it's just aesthetic, so cheaping out a little is okay.
  (write-string ";   ")
  (write form :length 2 :level 2 :lines 1 :pretty nil)
  (terpri)
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compilation units

;;; ------------------------------------------------------------
;;;
;;; BE VERY CAREFUL WHAT YOU DO HERE!!!
;;; This function must return the result* of evaluating the closure.
;;; I have put things in the wrong place (following the UNWIND-PROTECT)
;;; and it introduced subtle bugs that were very difficult to track down.
(defun do-compilation-unit (closure &key override)
  (cond (override
	 (let* ((*active-protection* nil))
	   (do-compilation-unit closure)))
	((null *active-protection*)
	 (let* ((*active-protection* t)
		(*pending-actions* nil)
                (*compilation-unit-module-index* 0)
                (*compilation-messages* nil)
                (*global-function-defs* (make-hash-table))
                (*global-function-refs* (make-hash-table :test #'equal)))
	   (unwind-protect
                (do-compilation-unit closure) ; --> result*
             (progn
               (dolist (action *pending-actions*)
                 (funcall action))
               (compilation-unit-finished *compilation-messages*))) ; --> result*
           ))
	(t
	 (funcall closure))))

(export 'do-compilation-unit)

(defmacro with-compilation-unit ((&rest options) &body body)
  `(do-compilation-unit #'(lambda () ,@body) ,@options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile-file pathnames

(defun cfp-output-extension (output-type)
  (cond
    ((eq output-type :bitcode) (if *use-human-readable-bitcode* "ll" "bc"))
    ((eq output-type :object) "o")
    ((eq output-type :fasl) "fasl")
    ((eq output-type :executable) #-windows "" #+windows "exe")
    (t (error "unsupported output-type ~a" output-type))))

(defun cfp-output-file-default (input-file output-type &key target-backend)
  (let* ((defaults (merge-pathnames input-file *default-pathname-defaults*)))
    (when target-backend
      (setq defaults (make-pathname :host target-backend :defaults defaults)))
    (make-pathname :type (cfp-output-extension output-type)
                   :defaults defaults)))


;;; Copied from sbcl sb!xc:compile-file-pathname
;;;   If INPUT-FILE is a logical pathname and OUTPUT-FILE is unsupplied,
;;;   the result is a logical pathname. If INPUT-FILE is a logical
;;;   pathname, it is translated into a physical pathname as if by
;;;   calling TRANSLATE-LOGICAL-PATHNAME.
;;; So I haven't really tried to make this precisely ANSI-compatible
;;; at the level of e.g. whether it returns logical pathname or a
;;; physical pathname. Patches to make it more correct are welcome.
(defun compile-file-pathname (input-file &key (output-file nil output-file-p)
                                           (output-type :fasl)
					   type
					   target-backend
                                           &allow-other-keys)
  (when type (error "Clasp compile-file-pathname uses :output-type rather than :type"))
  (let* ((pn (if output-file-p
		 (merge-pathnames output-file (translate-logical-pathname (cfp-output-file-default input-file output-type :target-backend target-backend)))
		 (cfp-output-file-default input-file output-type :target-backend target-backend)))
         (ext (cfp-output-extension output-type)))
    (make-pathname :type ext :defaults pn)))

(defun cf-module-name (type pathname)
  "Create a module name from the TYPE (either :user or :kernel)
and the pathname of the source file - this will also be used as the module initialization function name"
  (string-downcase (bformat nil "___%s_%s" (string type) (pathname-name pathname))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile-file proper

(defun generate-obj-asm (module output-stream &key file-type (reloc-model 'llvm-sys:reloc-model-undefined))
  (let* ((triple-string (llvm-sys:get-target-triple module))
	 (normalized-triple-string (llvm-sys:triple-normalize triple-string))
	 (triple (llvm-sys:make-triple normalized-triple-string))
	 (target-options (llvm-sys:make-target-options)))
    (multiple-value-bind (target msg)
	(llvm-sys:target-registry-lookup-target "" triple)
      (unless target (error msg))
      (let* ((target-machine (llvm-sys:create-target-machine target
							     (llvm-sys:get-triple triple)
							     ""
							     ""
							     target-options
							     reloc-model
							     *default-code-model*
							     'llvm-sys:code-gen-opt-default
                                                             NIL ; JIT?
                                                             ))
	     (pm (llvm-sys:make-pass-manager))
	     (tli (llvm-sys:make-target-library-info-wrapper-pass triple #||LLVM3.7||#))
	     (data-layout (llvm-sys:create-data-layout target-machine)))
	(llvm-sys:set-data-layout module data-layout)
	(llvm-sys:pass-manager-add pm tli)
	(llvm-sys:add-passes-to-emit-file-and-run-pass-manager target-machine pm output-stream file-type module)))))

(defun compile-file-results (output-file conditions)
  (let (warnings-p failures-p)
    (dolist (cond conditions)
      (cond
        ((typep cond 'compiler-error)
         (setq failures-p t))
        ((typep cond 'compiler-warning)
         (setq warnings-p t))
        (t (error "Illegal condition ~a" cond))))
    (values output-file warnings-p failures-p)))

(defvar *debug-compile-file* nil)
(defvar *debug-compile-file-counter* 0)

(defvar *compile-file-output-pathname* nil)


(defun bclasp-loop-read-and-compile-file-forms (source-sin environment)
  (let ((eof-value (gensym)))
    (loop
      (let ((eof (peek-char t source-sin nil eof-value)))
        (unless (eq eof eof-value)
          (let ((pos (core:input-stream-source-pos-info source-sin)))
            (setq *current-form-lineno* (core:source-file-pos-lineno pos)))))
      ;; FIXME: if :environment is provided we should probably use a different read somehow
      (let ((core:*current-source-pos-info* (core:input-stream-source-pos-info source-sin))
            (form (read source-sin nil eof-value)))
        (if (eq form eof-value)
            (return nil)
            (progn
              (when *compile-print* (describe-form form))
              (when *debug-compile-file* (bformat t "compile-file: cf%d -> %s%N" (incf *debug-compile-file-counter*) form))
              (t1expr form)))))))

(defun loop-read-and-compile-file-forms (source-sin environment compile-file-hook)
  ;; If the Cleavir compiler hook is set up then use that
  ;; to generate code 
  (if compile-file-hook
      (funcall compile-file-hook source-sin environment)
      (bclasp-loop-read-and-compile-file-forms source-sin environment)))

(defun compile-file-to-module (given-input-pathname
                               &key
                                 compile-file-hook
                                 type
                                 output-type
                                 source-debug-namestring
                                 (source-debug-offset 0)
                                 (print *compile-print*)
                                 (verbose *compile-verbose*)
                                 environment
                                 (optimize t)
                                 (optimize-level :|-O3|))
  "* Arguments
- given-input-pathname :: A pathname.
- output-path :: A pathname.
- compile-file-hook :: A function that will do the compile-file
- type :: :kernel or :user (I'm not sure this is useful anymore)
- source-debug-namestring :: A namestring.
- source-debug-offset :: An integer.
- environment :: Arbitrary, passed only to hook
Compile a lisp source file into an LLVM module."
  ;; TODO: Save read-table and package with unwind-protect
  (let* ((*package* *package*)
         (*compile-print* print)
         (*compile-verbose* verbose)
         (clasp-source-root (translate-logical-pathname "source-dir:"))
         (clasp-source (merge-pathnames (make-pathname :directory '(:relative :wild-inferiors) :name :wild :type :wild) clasp-source-root))
         (source-location
           (if (pathname-match-p given-input-pathname clasp-source)
               (enough-namestring given-input-pathname clasp-source-root)
               given-input-pathname))
         (input-pathname (or (probe-file given-input-pathname)
                             (error "compile-file-to-module could not find the file ~a to open it" given-input-pathname)))
         (source-sin (open input-pathname :direction :input))
         (module (llvm-create-module (namestring input-pathname)))
	 (module-name (cf-module-name type given-input-pathname))
	 warnings-p failure-p)
    (or module (error "module is NIL"))
    (with-open-stream (sin source-sin)
      ;; If a truename is provided then spoof the file-system to treat input-pathname
      ;; as source-truename with the given offset
      (when source-debug-namestring
	(core:source-file-info (namestring input-pathname) source-debug-namestring source-debug-offset nil))
      (when *compile-verbose*
	(bformat t "; Compiling file: %s%N" (namestring input-pathname)))
      (with-one-source-database
	  (cmp-log "About to start with-compilation-unit%N")
        (with-compilation-unit ()
          (let* ((*compile-file-pathname* (pathname (merge-pathnames given-input-pathname)))
                 (*compile-file-truename* (translate-logical-pathname *compile-file-pathname*)))
            (with-module (:module module
                          :source-namestring (namestring source-location)
                          :source-debug-namestring source-debug-namestring
                          :source-debug-offset source-debug-offset
                          :optimize (when optimize #'optimize-module-for-compile-file)
                          :optimize-level optimize-level)
              (with-debug-info-generator (:module *the-module*
                                          :pathname *compile-file-truename*)
                (or *the-module* (error "*the-module* is NIL"))
                (with-make-new-run-all (run-all-function)
                  (with-run-all-body-codegen ;;(result)
                      (irc-intrinsic "ltvc_assign_source_file_info_handle"
                                     (irc-constant-string-ptr *gv-source-namestring*)
                                     (irc-constant-string-ptr *gv-source-debug-namestring*)
                                     (jit-constant-i64 *source-debug-offset*)
                                     (jit-constant-i32 (if *source-debug-use-lineno* 1 0))
                                     *gv-source-file-info-handle*))
                  (with-literal-table
                      (loop-read-and-compile-file-forms source-sin environment compile-file-hook))
                  (make-boot-function-global-variable *the-module* run-all-function)))
              (cmp-log "About to verify the module%N")
              (cmp-log-dump-module *the-module*)
              (irc-verify-module-safe *the-module*)
              (quick-module-dump *the-module* "preoptimize")
              ;; ALWAYS link the builtins in, inline them and then remove them.
              (link-inline-remove-builtins *the-module*))
            (quick-module-dump module "postoptimize")
            module))))))

(defun compile-file (input-file
                     &key
                       (output-file nil output-file-p)
                       (verbose *compile-verbose*)
                       (print *compile-print*)
                       (optimize t)
                       (optimize-level :|-O3|)
                       (system-p nil system-p-p)
                       (external-format :default)
                       ;; If we are spoofing the source-file system to treat given-input-name
                       ;; as a part of another file then use source-truename to provide the
                       ;; truename of the file we want to mimic
                       source-debug-namestring
                       ;; This is the offset we want to spoof
                       (source-debug-offset 0)
                       ;; output-type can be (or :fasl :bitcode :object)
                       (output-type :fasl)
                       ;; type can be either :kernel or :user
                       (type :user)
                       ;; ignored by bclasp
                       ;; but passed to hook functions
                       environment
                     &aux conditions)
  "See CLHS compile-file."
  (if system-p-p (error "I don't support system-p keyword argument - use output-type"))
  (if (not output-file-p) (setq output-file (cfp-output-file-default input-file output-type)))
  (with-compiler-env (conditions)
    ;; Do the different kind of compile-file here
    (let* ((*compile-print* print)
           (*compile-verbose* verbose)
           (*current-form-lineno* 0)
           (output-path (compile-file-pathname input-file :output-file output-file :output-type output-type ))
           (*compile-file-output-pathname* output-path)
           (module (compile-file-to-module input-file
                                           :type type
                                           :output-type output-type
                                           :source-debug-namestring source-debug-namestring
                                           :source-debug-offset source-debug-offset
                                           :compile-file-hook *cleavir-compile-file-hook*
                                           :environment environment
                                           :optimize optimize
                                           :optimize-level optimize-level)))
      (cond
        ((null output-path)
         (error "The output-path is nil for input filename ~a~%" input-file))
        ((eq output-type :object)
         (when verbose (bformat t "Writing object to %s%N" (core:coerce-to-filename output-path)))
         (ensure-directories-exist output-path)
         ;; Save the bitcode so we can take a look at it
         (write-bitcode module (core:coerce-to-filename (cfp-output-file-default output-path :bitcode)))
         (with-open-file (fout output-path :direction :output)
           (let ((reloc-model (cond
                                ((member :target-os-linux *features*) 'llvm-sys:reloc-model-pic-)
                                (t 'llvm-sys:reloc-model-undefined))))
             (generate-obj-asm module fout :file-type 'llvm-sys:code-gen-file-type-object-file :reloc-model reloc-model))))
        ((eq output-type :bitcode)
         (when verbose (bformat t "Writing bitcode to %s%N" (core:coerce-to-filename output-path)))
         (ensure-directories-exist output-path)
         (write-bitcode module (core:coerce-to-filename output-path)))
        ((eq output-type :fasl)
         (ensure-directories-exist output-path)
         (let ((temp-bitcode-file (compile-file-pathname input-file :output-file output-file :output-type :bitcode)))
           (ensure-directories-exist temp-bitcode-file)
           (bformat t "Writing temporary bitcode file to: %s%N" temp-bitcode-file)
           (write-bitcode module (core:coerce-to-filename temp-bitcode-file))
           (bformat t "Writing fasl file to: %s%N" output-file)
           (llvm-link output-file :input-files (list temp-bitcode-file) :input-type :bitcode)))
        (t ;; fasl
         (error "Add support to file of type: ~a" output-type)))
      (dolist (c conditions)
        (bformat t "conditions: %s%N" c))
      (llvm-sys:module-delete module)
      (compile-file-results output-path conditions))))

(export 'compile-file)


#+(or bclasp cclasp)
(progn
  (defun bclasp-compile-file (input-file &rest args)
    (let ((cmp:*cleavir-compile-hook* nil)
          (cmp:*cleavir-compile-file-hook* nil)
          (core:*use-cleavir-compiler* nil)
          (core:*eval-with-env-hook* #'core:eval-with-env-default))
      (apply 'compile-file input-file args)))
  (export 'bclasp-compile-file))
