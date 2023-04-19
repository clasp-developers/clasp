(in-package #:cmp)

#+(or)
(eval-when (:execute)
  (setq core:*echo-repl-read* t))

;;;; Top level interface: COMPILE-FILE, etc.

;;; Use the *cleavir-compile-file-hook* to determine which compiler to use
;;; if nil == bclasp. Code for the bclasp compiler is in codegen-toplevel.lisp;
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
  (fresh-line)
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
  (declare (ignorable message report-link-time verbose))
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
             #+(or)
             (when verbose
               (let ((llvm-finalization-time llvm-sys:*accumulated-llvm-finalization-time*)
                     (compiler-real-time (/ (- (get-internal-real-time) *compiler-real-time*) (float internal-time-units-per-second)))
                     (compiler-run-time (/ (- (get-internal-run-time) *compiler-run-time*) (float internal-time-units-per-second)))
                     (link-time llvm-sys:*accumulated-clang-link-time*))
                 (let* ((link-string (if report-link-time
                                        (core:fmt nil " link({:.1f})" link-time)
                                        ""))
                        (total-llvm-time (+ llvm-finalization-time (if report-link-time
                                                                       link-time
                                                                       0.0)))
                        (percent-llvm-time (if (zerop compiler-real-time)
                                               0.0
                                               (* 100.0 (/ total-llvm-time compiler-real-time))))
                        (percent-time-string
                          (if report-link-time
                              (core:fmt nil "(llvm+link)/real({:1.0f}%)" percent-llvm-time)
                              (core:fmt nil "llvm/real({:2.0f}%)" percent-llvm-time))))
                   (core:fmt t "   {} seconds real({:.1f}) run({:.1f}) llvm({:.1f}){} {}%N"
                                 message
                                 compiler-real-time
                                 compiler-run-time
                                 llvm-finalization-time
                                 link-string
                                 percent-time-string)
                   (finish-output)))))))
        (t (funcall closure))))

(defmacro with-compiler-timer ((&key message report-link-time verbose)
                               &body body)
  `(do-compiler-timer (lambda () (progn ,@body))
     :message ,message :report-link-time ,report-link-time :verbose ,verbose))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile-file pathnames

;;; I wonder, why that doesn't take core:*clasp-build-mode* into account
(defun cfp-output-extension (output-type)
  (if (eq output-type :object)
      (core:build-extension *default-object-type*)
      (core:build-extension output-type)))

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
(defun compile-file-pathname (input-file
                              &key (output-file nil output-file-p)
                                   (output-type (default-library-type) output-type-p)
                                target-backend
                              &allow-other-keys)
  (let* ((output-type (if output-type-p
                          (fixup-output-type output-type)
                          output-type))
         (pn (if output-file-p
		 (merge-pathnames output-file (translate-logical-pathname (cfp-output-file-default input-file output-type :target-backend target-backend)))
		 (cfp-output-file-default input-file output-type :target-backend target-backend)))
         (ext (cfp-output-extension output-type)))
    (if (or output-type-p (not output-file-p))
        (make-pathname :type ext :defaults pn :version nil)
        pn)))

(defun cf-module-name (type pathname)
  "Create a module name from the TYPE (either :user or :kernel)
and the pathname of the source file - this will also be used as the module initialization function name"
  (string-downcase (core:fmt nil "___{}_{}" (string type) (pathname-name pathname))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile-file proper

(defun generate-obj-asm-stream (module output-stream file-type reloc-model &key (target-faso-file *default-object-type*))
  (with-track-llvm-time
      (progn
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
                                                                   (code-model :jit nil :target-faso-file target-faso-file)
                                                                   'llvm-sys:code-gen-opt-default
                                                                   NIL ; JIT?
                                                                   ))
                   (pm (llvm-sys:make-pass-manager))
                   (target-pass-config (llvm-sys:create-pass-config target-machine pm))
                   (_ (llvm-sys:set-enable-tail-merge target-pass-config nil))
                   (tli (llvm-sys:make-target-library-info-wrapper-pass triple #||LLVM3.7||#))
                   (data-layout (llvm-sys:create-data-layout target-machine)))
              (declare (ignore _))
              (llvm-sys:set-data-layout module data-layout)
              (llvm-sys:pass-manager-add pm tli)
              (llvm-sys:add-passes-to-emit-file-and-run-pass-manager target-machine pm output-stream nil #|<-dwo-stream|# file-type module)))))))


(defun compile-file-generate-obj-asm (module output-pathname &key file-type (reloc-model 'llvm-sys:reloc-model-undefined))
  (with-atomic-file-rename (temp-output-pathname output-pathname)
    (with-open-file (output-stream temp-output-pathname :direction :output)
      (generate-obj-asm-stream module output-stream file-type reloc-model :target-faso-file nil))))

(defvar *debug-compile-file* nil)
(defvar *debug-compile-file-counter* 0)

(defvar *compile-file-output-pathname* nil)

(defun compile-file-source-pos-info (stream)
  (core:input-stream-source-pos-info
   stream *compile-file-file-scope*
   *compile-file-source-debug-lineno* *compile-file-source-debug-offset*))

(defun loop-read-and-compile-file-forms (source-sin environment)
  ;; If the Cleavir compiler hook is set up then use that
  ;; to generate code
  (if *cleavir-compile-file-hook*
      (funcall *cleavir-compile-file-hook* source-sin environment)
      (error "BUG: No compiler in loop-read-and-compile-forms")))

(defun compile-stream-to-module (source-sin
                                 &key
                                   environment
                                   image-startup-position
                                   (optimize t)
                                   (optimize-level *optimization-level*))
  "* Arguments
- source-sin :: An input stream to read forms from.
- environment :: A compilation environment.
Compile a Lisp source stream and return a corresponding LLVM module."
  (let* ((name (namestring *compile-file-pathname*))
         (module (llvm-create-module name))
         run-all-name)
    (unless module (error "module is NIL"))
    (cmp-log "About to with-module%N")
    (with-module (:module module
                  :optimize (when optimize #'optimize-module-for-compile-file)
                  :optimize-level optimize-level)
      ;; (1) Generate the code
      (cmp-log "About to with-debug-info-generator%N")
      (with-debug-info-generator (:module *the-module*
                                  :pathname *compile-file-source-debug-pathname*)
        (cmp-log "About to with-make-new-run-all%N")
        (with-make-new-run-all (run-all-function name)
          (cmp-log "About to with-literal-table%N")
          (with-literal-table (:id 0)
            (cmp-log "About to loop-read-and-compile-file-forms%N")
            (loop-read-and-compile-file-forms source-sin environment))
          (setf run-all-name (llvm-sys:get-name run-all-function))))
      (cmp-log "About to verify the module%N")
      (cmp-log-dump-module *the-module*)
      (irc-verify-module-safe *the-module*)
      (quick-module-dump *the-module* "preoptimize")
      ;; (2) Add the CTOR next
      (make-boot-function-global-variable module run-all-name
                                          :position image-startup-position
                                          :register-library t))
    ;; Now at the end of with-module another round of optimization is done
    ;; but the RUN-ALL is now referenced by the CTOR and so it won't be optimized away
    ;; ---- MOVE OPTIMIZATION in with-module to HERE ----
    (quick-module-dump module "postoptimize")
    module))

(defun default-object-type ()
  *default-object-type*)

(defun default-library-type (&optional (output-type *default-object-type*))
  (case output-type
    (:faso :fasp)
    (:object :fasl)
    (:bytecode :faslbc)
    (:fasoll :faspll)
    (:fasobc :faspbc)
    (:faspll :faspll)
    (:faspbc :faspbc)
    (otherwise (error "Handle output-type for ~a" output-type))))

(defun fixup-output-type (output-type)
  (cond
    ((eq output-type :object) (default-object-type))
    ((eq output-type :fasl) (default-library-type))
    (t output-type)))

(defun enable-bytecode-file-compiler ()
  (setf *default-object-type* :bytecode))

(defun disable-bytecode-file-compiler ()
  (setf *default-object-type* :faso))

(defun compile-file (input-file
                     &rest args
                     &key
                       ;; Standard keywords
                       output-file
                       ((:verbose *compile-verbose*) *compile-verbose*)
                       ((:print *compile-print*) *compile-print*)
                       (external-format :default)
                       ;; Extensions
                       (execution (if *compile-file-parallel*
                                      :parallel
                                      :serial))
                       environment ; compilation environment
                       ;; output-type can be (or :fasl :bitcode :object)
                       (output-type (default-library-type) output-type-p)
                       ;; type can be either :kernel or :user
                       ;; FIXME: What does this do.
                       (type :user)
                       ;; A unique prefix for symbols of compile-file'd files that
                       ;; will be linked together
                       ;; FIXME: Only relevant for object files, I think.
                       ((:unique-symbol-prefix
                         *compile-file-unique-symbol-prefix*)
                        "")
                       ;; Control the order of startup functions (FIXME: ditto above)
                       (image-startup-position (core:next-startup-position))
                       (source-debug-pathname nil cfsdpp)
                       ((:source-debug-lineno
                         *compile-file-source-debug-lineno*)
                        0)
                       ((:source-debug-offset
                         *compile-file-source-debug-offset*)
                        0)
                       ;; these ought to be removed, or at least made
                       ;; to use lisp-level optimization policy rather
                       ;; than what they do now, which is LLVM stuff.
                       (optimize t)
                       (optimize-level *optimization-level*)
                     &allow-other-keys)
  ;; These are all just passed along to other functions.
  (declare (ignore output-file environment type
                   image-startup-position optimize optimize-level))
  "See CLHS compile-file."
  (with-compilation-unit ()
    (let* ((output-type (fixup-output-type output-type))
           (output-path (apply #'compile-file-pathname input-file args))
           (*compilation-module-index* 0) ; FIXME: necessary?
           (*readtable* *readtable*) (*package* *package*)
           (*optimize* *optimize*) (*policy* *policy*)
           (*compile-file-pathname*
             (pathname (merge-pathnames input-file)))
           (*compile-file-truename*
             (translate-logical-pathname *compile-file-pathname*))
           (*compile-file-source-debug-pathname*
             (if cfsdpp source-debug-pathname *compile-file-truename*))
           (*compile-file-file-scope*
             (core:file-scope *compile-file-source-debug-pathname*))
           ;; bytecode compilation can't be done in parallel at the moment.
           ;; we could possibly warn about it if execution was specified,
           ;; but practically speaking it would mostly be noise.
           (execution (if (eq output-type :bytecode)
                          :serial
                          execution)))
      (with-open-file (source-sin input-file
                                  :external-format external-format)
        (with-compilation-results ()
          (when *compile-verbose*
            (format t "~&; Compiling file: ~a~%"
                    (namestring input-file)))
          (ecase execution
            (:serial
             (apply #'compile-stream/serial source-sin output-path args))
            (:parallel
             ;; defined later in compile-file-parallel.lisp.
             (apply #'compile-stream/parallel source-sin output-path
                    args))))))))

(defun compile-stream/serial (input-stream output-path &rest args
                              &key
                                (optimize t)
                                (optimize-level *optimization-level*)
                                (output-type (default-library-type) output-type-p)
                                ;; type can be either :kernel or :user
                                (type :user)
                                ;; Control the order of startup functions
                                (image-startup-position (core:next-startup-position)) 
                                environment
                              &allow-other-keys)
  (let* ((*compile-file-parallel* nil)
         (output-type (if output-type-p
                          (fixup-output-type output-type)
                          output-type)))
    (if (eq output-type :bytecode)
        (apply #'cmpltv:bytecode-compile-stream input-stream output-path args)
        (with-compiler-env ()
          (with-compiler-timer (:message "Compile-file"
                                :report-link-time t
                                :verbose *compile-verbose*)
            (let ((module (compile-stream-to-module input-stream
                                                    :environment environment
                                                    :image-startup-position image-startup-position
                                                    :optimize optimize
                                                    :optimize-level optimize-level)))
              (compile-file-output-module module output-path output-type
                                          type
                                          :position image-startup-position))))))
  (truename output-path))

(defun reloc-model ()
  (cond
    ((or (member :linux *features*) (member :freebsd *features*))
     'llvm-sys:reloc-model-pic-)
    (t 'llvm-sys:reloc-model-undefined)))

(defun output-bitcode (module file &key output-type)
  (with-track-llvm-time
      (write-bitcode module file :output-type output-type)))

(defun output-kernel-fasl (output-file input-file output-type)
  (let ((fasl-output-file (make-pathname :type "fasl" :defaults output-file)))
    (when *compile-verbose*
      (core:fmt t "Writing {} kernel fasl file to: {}%N" output-type fasl-output-file)
      (finish-output))
    (llvm-link fasl-output-file :input-files (list input-file) :input-type :bitcode)))

(defun compile-file-output-module-to-faso (module output-file
                                           &key position (output-bitcode t))
  "Generate a faso file from the module"
  (when output-bitcode
    (let ((temp-bitcode-file
            (make-pathname :defaults output-file :version nil
                           :type (cfp-output-extension :bitcode))))
      (when *compile-verbose*
        (format t "~&; Writing temporary bitcode to: ~a~%"
                (namestring temp-bitcode-file)))
      (output-bitcode module (core:coerce-to-filename temp-bitcode-file)
                      :output-type :object)))
  (when *compile-verbose*
    (format t "~&; Writing faso to: ~a~%" (namestring output-file))
    (finish-output))
  (let ((stream (generate-obj-asm-stream module :simple-vector-byte8
                                         'llvm-sys:code-gen-file-type-object-file
                                         (reloc-model))))
    (core:write-faso output-file (list stream) :start-object-id position)))

(defun compile-file-output-module (module output-file output-type type
                                   &key position (output-bitcode t))
  (ensure-directories-exist output-file)
  (ecase output-type
    ((:object)
     (when *compile-verbose*
       (format t "~&; Writing object to: ~a~%"
               (core:coerce-to-filename output-file)))
     ;; save the bitcode so we can look at it.
     (let ((temp-bitcode-file
             (make-pathname :defaults output-file :version nil
                            :type (cfp-output-extension :bitcode))))
       (ensure-directories-exist temp-bitcode-file)
       (when *compile-verbose*
         (format t "~&; Writing temporary bitcode to: ~a~%"
                 (namestring temp-bitcode-file)))
       (output-bitcode module temp-bitcode-file
                       :output-type (default-library-type output-type))
       (prog1
           (compile-file-generate-obj-asm module output-file
                                          :file-type 'llvm-sys:code-gen-file-type-object-file
                                          :reloc-model (reloc-model))
         (when (eq type :kernel)
           (output-kernel-fasl output-file temp-bitcode-file :object)))))
    ((:faso)
     (compile-file-output-module-to-faso module output-file
                                         :position position :output-bitcode output-bitcode))
    ((:bitcode)
     (when *compile-verbose*
       (format t "~&; Writing bitcode to: ~a~%"
               (core:coerce-to-filename output-file)))
     (prog1 (output-bitcode module (core:coerce-to-filename output-file)
                            :output-type (default-library-type output-type))
       (when (eq type :kernel)
         (output-kernel-fasl output-file output-file :bitcode))))
    ((:fasp)
     (let ((temp-bitcode-file
             (make-pathname :defaults output-file :version nil
                            :type (cfp-output-extension :bitcode))))
       (ensure-directories-exist temp-bitcode-file)
       (when *compile-verbose*
         (format t "~&; Writing temporary bitcode to: ~a~%"
                 (namestring temp-bitcode-file)))
       (output-bitcode module (core:coerce-to-filename temp-bitcode-file)
                       :output-type :object)
       (when *compile-verbose*
         (format t "~&; Writing faso to: ~a~%" (namestring output-file))
         (finish-output))
       (let ((stream (generate-obj-asm-stream module :simple-vector-byte8
                                              'llvm-sys:code-gen-file-type-object-file
                                              (reloc-model))))
         (core:write-faso output-file (list stream) :start-object-id position))))
    ((:fasoll :faspll)
     (let ((filename
             (make-pathname :defaults output-file :version nil
                            :type (cfp-output-extension output-type))))
       (ensure-directories-exist filename)
       (when *compile-verbose*
         (format t "~&; Writing ~a file to: ~a~%" output-type filename))
       (with-atomic-file-rename (temp-pathname filename)
         (with-open-file (fout temp-pathname :direction :output
                                             :if-exists :supersede)
           (llvm-sys:dump-module module fout)))))
    ((:fasobc :faspbc)
     (let ((filename
             (make-pathname :defaults output-file :version nil
                            :type (cfp-output-extension output-type))))
       (ensure-directories-exist filename)
       (when *compile-verbose*
         (format t "~&; Writing ~a file to: ~a~%" output-type filename))
       (with-atomic-file-rename (temp-pathname filename)
         (llvm-sys:write-bitcode-to-file module (namestring temp-pathname)))))
    ((:fasl)
     (let ((temp-bitcode-file
             (make-pathname :defaults output-file :version nil
                            :type (cfp-output-extension :bitcode))))
       (ensure-directories-exist temp-bitcode-file)
       (when *compile-verbose*
         (format t "~&; Writing temporary bitcode to: ~a~%" (namestring temp-bitcode-file)))
       (output-bitcode module (core:coerce-to-filename temp-bitcode-file)
                       :output-type :object)
       (when *compile-verbose*
         (core:fmt t "~&; Writing fasl file to: ~a~%" output-file)
         (finish-output))
       (llvm-link output-file :input-files (list temp-bitcode-file) :input-type :bitcode))))
  (with-track-llvm-time (llvm-sys:module-delete module)))

(export 'compile-file)
