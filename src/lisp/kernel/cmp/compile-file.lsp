(in-package #:cmp)

;;;; Top level interface: COMPILE-FILE, etc.

;;; Use the *cleavir-compile-file-hook* to determine which compiler to use
;;; if nil == bclasp. Code for the bclasp compiler is in codegen-toplevel.lsp;
;;; look for t1expr.

(defvar *compile-verbose* t)
(defvar *compile-print* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Describing top level forms (for compile-print)

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

(defvar *compiler-real-time*)
(defvar *compiler-run-time*)
(defvar *compiler-timer-protection* nil)

(defun do-compiler-timer (closure &rest args &key message report-link-time verbose override)
  (cond (override
	 (let* ((*compiler-timer-protection* nil))
	   (apply #'do-compiler-timer closure args)))
	((null *compiler-timer-protection*)
	 (let* ((*compiler-timer-protection* t)
                (llvm-sys:*accumulated-llvm-finalization-time* 0)
                (llvm-sys:*number-of-llvm-finalizations* 0)
                (*compiler-real-time* (get-internal-real-time))
                (*compiler-run-time* (get-internal-run-time))
                (llvm-sys:*accumulated-clang-link-time* 0)
                (llvm-sys:*number-of-clang-links* 0))
           (multiple-value-prog1
               (do-compiler-timer closure)
             (let ((llvm-finalization-time llvm-sys:*accumulated-llvm-finalization-time*)
                   (compiler-real-time (/ (- (get-internal-real-time) *compiler-real-time*) (float internal-time-units-per-second)))
                   (compiler-run-time (/ (- (get-internal-run-time) *compiler-run-time*) (float internal-time-units-per-second)))
                   (link-time llvm-sys:*accumulated-clang-link-time*))
               (when verbose
                 (let* ((link-string (if report-link-time
                                        (core:bformat nil " link(%.1f)" link-time)
                                        ""))
                       (total-llvm-time (+ llvm-finalization-time (if report-link-time
                                                                      link-time
                                                                      0.0)))
                       (percent-llvm-time (* 100.0 (/ total-llvm-time compiler-real-time )))
                       (percent-time-string (if report-link-time
                                                (core:bformat nil "(llvm+link)/real(%1.f%%)" percent-llvm-time)
                                                (core:bformat nil "llvm/real(%1.f%%)" percent-llvm-time))))
                   #+(or)(core:bformat t "   %s seconds real(%.1f) run(%.1f) llvm(%.1f)%s %s%N"
                                 message
                                 compiler-real-time
                                 compiler-run-time
                                 llvm-finalization-time
                                 link-string
                                 percent-time-string)
                   (finish-output)))))))
        (t (funcall closure))))

(defmacro with-compiler-timer ((&key message report-link-time verbose override) &rest body)
  `(do-compiler-timer (lambda () (progn ,@body)) :message ,message :report-link-time ,report-link-time :verbose ,verbose))

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
  (with-track-llvm-time
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
                 (target-pass-config (llvm-sys:create-pass-config target-machine pm))
                 (_ (llvm-sys:set-enable-tail-merge target-pass-config nil))
                 (tli (llvm-sys:make-target-library-info-wrapper-pass triple #||LLVM3.7||#))
                 (data-layout (llvm-sys:create-data-layout target-machine)))
            (llvm-sys:set-data-layout module data-layout)
            (llvm-sys:pass-manager-add pm tli)
            (llvm-sys:add-passes-to-emit-file-and-run-pass-manager target-machine pm output-stream file-type module))))))

(defvar *debug-compile-file* nil)
(defvar *debug-compile-file-counter* 0)

(defvar *compile-file-output-pathname* nil)


(defun bclasp-loop-read-and-compile-file-forms (source-sin environment)
  (let ((eof-value (gensym)))
    (loop
      ;; Required to update the source pos info. FIXME!?
      (peek-char t source-sin nil)
      ;; FIXME: if :environment is provided we should probably use a different read somehow
      (let ((core:*current-source-pos-info* (core:input-stream-source-pos-info source-sin))
            (form (read source-sin nil eof-value)))
        (if (eq form eof-value)
            (return nil)
            (progn
              (when *compile-print* (describe-form form))
              (when *debug-compile-file* (bformat t "compile-file: cf%d -> %s%N" (incf *debug-compile-file-counter*) form))
              (core:with-memory-ramp (:pattern 'gctools:ramp)
                (t1expr form))))))))

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
                                 source-debug-pathname
                                 (source-debug-offset 0)
                                 environment
                                 image-startup-position
                                 (optimize t)
                                 (optimize-level *optimization-level*))
  "* Arguments
- given-input-pathname :: A pathname.
- output-path :: A pathname.
- compile-file-hook :: A function that will do the compile-file
- type :: :kernel or :user (I'm not sure this is useful anymore)
- source-debug-pathname :: A pathname.
- source-debug-offset :: An integer.
- environment :: Arbitrary, passed only to hook
Compile a lisp source file into an LLVM module."
  ;; TODO: Save read-table and package with unwind-protect
  (let* ((*package* *package*)
         (clasp-source-root (translate-logical-pathname "source-dir:"))
         (clasp-source (merge-pathnames (make-pathname :directory '(:relative :wild-inferiors) :name :wild :type :wild) clasp-source-root))
         (source-location
           (if (pathname-match-p given-input-pathname clasp-source)
               (enough-namestring given-input-pathname clasp-source-root)
               given-input-pathname))
         (input-pathname (or (probe-file given-input-pathname)
			     (error 'core:simple-file-error
				    :pathname given-input-pathname
				    :format-control "compile-file-to-module could not find the file ~s to open it"
				    :format-arguments (list given-input-pathname))))
         (source-sin (open input-pathname :direction :input))
         (module (llvm-create-module (namestring input-pathname)))
	 (module-name (cf-module-name type given-input-pathname))
	 warnings-p failure-p)
    (or module (error "module is NIL"))
    (with-open-stream (sin source-sin)
      ;; If a truename is provided then spoof the file-system to treat input-pathname
      ;; as source-truename with the given offset
      (when source-debug-pathname
        (core:file-scope (namestring input-pathname) source-debug-pathname source-debug-offset nil))
      (when *compile-verbose*
	(bformat t "; Compiling file: %s%N" (namestring input-pathname)))
      (let* ((*compilation-module-index* 0) ; FIXME: necessary?
             (*compile-file-pathname* (pathname (merge-pathnames given-input-pathname)))
             (*compile-file-truename* (translate-logical-pathname *compile-file-pathname*))
             run-all-name)
        (with-module (:module module
                      :optimize (when optimize #'optimize-module-for-compile-file)
                      :optimize-level optimize-level)
          (with-source-pathnames (:source-pathname *compile-file-truename* ;(namestring source-location)
                                  :source-debug-pathname source-debug-pathname
                                  :source-debug-offset source-debug-offset)
            ;; (1) Generate the code
            (with-debug-info-generator (:module *the-module*
                                        :pathname *compile-file-truename*)
              (or module (error "module is NIL"))
              (with-make-new-run-all (run-all-function (namestring input-pathname))
                (with-literal-table
                    (loop-read-and-compile-file-forms source-sin environment compile-file-hook))
                (setf run-all-name (llvm-sys:get-name run-all-function))))
            (cmp-log "About to verify the module%N")
            (cmp-log-dump-module *the-module*)
            (irc-verify-module-safe *the-module*)
            (quick-module-dump *the-module* "preoptimize")
            ;; (2) Add the CTOR next
            (make-boot-function-global-variable module run-all-name
                                                :position image-startup-position
                                                :register-library t)
            ;; (3) If optimize ALWAYS link the builtins in, inline them and then remove them - then optimize.
            (if (> optimize-level 0)
                (link-inline-remove-builtins *the-module*)))
          ;; Now at the end of with-module another round of optimization is done
          ;; but the RUN-ALL is now referenced by the CTOR and so it won't be optimized away
          ;; ---- MOVE OPTIMIZATION in with-module to HERE ----
          )
        (quick-module-dump module "postoptimize")
        module))))

(defun compile-file-serial (input-file
                            &key
                              (output-file nil output-file-p)
                              (verbose *compile-verbose*)
                              (print *compile-print*)
                              (optimize t)
                              (optimize-level *optimization-level*)
                              (external-format :default)
                              ;; If we are spoofing the source-file system to treat given-input-name
                              ;; as a part of another file then use source-debug-pathname to provide the
                              ;; truename of the file we want to mimic
                              source-debug-pathname
                              ;; This is the offset we want to spoof
                              (source-debug-offset 0)
                              ;; output-type can be (or :fasl :bitcode :object)
                              (output-type :fasl)
                              ;; type can be either :kernel or :user
                              (type :user)
                              ;; A unique prefix for symbols of compile-file'd files that
                              ;; will be linked together
                              (unique-symbol-prefix "")
                              ;; Control the order of startup functions
                              (image-startup-position (core:next-startup-position))
                              ;; ignored by bclasp
                              ;; but passed to hook functions
                              environment)
  "See CLHS compile-file."
  #+debug-monitor(sys:monitor-message "compile-file ~a" input-file)
  (if (not output-file-p) (setq output-file (cfp-output-file-default input-file output-type)))
  (with-compiler-env ()
    ;; Do the different kind of compile-file here
    (let* ((*compile-print* print)
           (*compile-verbose* verbose)
           (output-path (compile-file-pathname input-file :output-file output-file :output-type output-type ))
           (*compile-file-output-pathname* output-path)
           (*compile-file-unique-symbol-prefix* unique-symbol-prefix))
      (with-compiler-timer (:message "Compile-file" :report-link-time t :verbose verbose)
        (with-compilation-results ()
          (let ((module (compile-file-to-module input-file
                                                :type type
                                                :output-type output-type
                                                :source-debug-pathname source-debug-pathname
                                                :source-debug-offset source-debug-offset
                                                :compile-file-hook *cleavir-compile-file-hook*
                                                :environment environment
                                                :image-startup-position image-startup-position
                                                :optimize optimize
                                                :optimize-level optimize-level)))
            (output-module module output-file output-type output-path input-file type)
            output-path))))))

(defun reloc-model ()
  (cond
    ((or (member :target-os-linux *features*) (member :target-os-freebsd *features*))
     'llvm-sys:reloc-model-pic-)
    (t 'llvm-sys:reloc-model-undefined)))

(defun output-bitcode (module file)
  (with-track-llvm-time
      (write-bitcode module file)))

(defun output-kernel-fasl (output-file input-file output-type)
  (let ((fasl-output-file (make-pathname :type "fasl" :defaults output-file)))
    (when *compile-verbose*
      (bformat t "Writing %s kernel fasl file to: %s%N" output-type fasl-output-file)
      (finish-output))
    (llvm-link fasl-output-file :input-files (list input-file) :input-type :bitcode)))

(defun output-module (module output-file output-type output-path input-file type)
  (when (null output-path)
    (error "The output-path is nil for input filename ~a~%" input-file))
  (ensure-directories-exist output-path)
  (cond ((eq output-type :object)
         (when *compile-verbose*
           (bformat t "Writing object to: %s%N" (core:coerce-to-filename output-path)))
         ;; save the bitcode so we can look at it.
         (let ((temp-bitcode-file
                 (compile-file-pathname input-file :output-file output-file :output-type :bitcode)))
           (ensure-directories-exist temp-bitcode-file)
           (output-bitcode module temp-bitcode-file)
           (prog1
               (with-open-file (fout output-path :direction :output)
                 (generate-obj-asm module fout
                                   :file-type 'llvm-sys:code-gen-file-type-object-file
                                   :reloc-model (reloc-model)))
             (when (eq type :kernel)
               (output-kernel-fasl output-file temp-bitcode-file :object)))))
        ((eq output-type :bitcode)
         (when *compile-verbose*
           (bformat t "Writing bitcode to: %s%N" (core:coerce-to-filename output-path)))
         (prog1 (output-bitcode module (core:coerce-to-filename output-path))
           (when (eq type :kernel)
             (output-kernel-fasl output-file output-path :bitcode))))
        ((eq output-type :fasl)
         (let ((temp-bitcode-file (compile-file-pathname input-file
                                                         :output-file output-file :output-type :bitcode)))
           (ensure-directories-exist temp-bitcode-file)
           (when *compile-verbose*
             (bformat t "Writing temporary bitcode file to: %s%N" temp-bitcode-file))
           (output-bitcode module (core:coerce-to-filename temp-bitcode-file))
           (when *compile-verbose*
             (bformat t "Writing fasl file to: %s%N" output-file)
             (finish-output))
           (llvm-link output-file :input-files (list temp-bitcode-file) :input-type :bitcode)))
        (t ;; Unknown
         (error "Add support to file of type: ~a" output-type)))
  (with-track-llvm-time
      (llvm-sys:module-delete module)))

(export 'compile-file)

(eval-when (:load-toplevel :execute)
  (setf (fdefinition 'compile-file) #'compile-file-serial))

#+(or bclasp cclasp)
(progn
  (defun bclasp-compile-file (input-file &rest args)
    (let ((cmp:*cleavir-compile-hook* nil)
          (cmp:*cleavir-compile-file-hook* nil)
          (core:*use-cleavir-compiler* nil)
          (core:*eval-with-env-hook* #'core:interpret-eval-with-env))
      (apply 'compile-file input-file args)))
  (export 'bclasp-compile-file))
