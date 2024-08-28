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

(defun cfp-output-file-default (input-file output-type &key target-backend)
  (let* ((defaults (merge-pathnames input-file *default-pathname-defaults*)))
    (when target-backend
      (setq defaults (make-pathname :host target-backend :defaults defaults)))
    (make-pathname :type (core:build-extension output-type)
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
                                   (output-type *default-output-type* output-type-p)
                                   target-backend
                              &allow-other-keys)
  (let ((pn (if output-file-p
		 (merge-pathnames output-file (translate-logical-pathname (cfp-output-file-default input-file output-type :target-backend target-backend)))
		 (cfp-output-file-default input-file output-type :target-backend target-backend)))
         (ext (core:build-extension output-type)))
    (if (or output-type-p (not output-file-p))
        (make-pathname :type ext :defaults pn :version nil)
        pn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile-file proper

(defun generate-obj-asm-stream (module output-stream file-type reloc-model &key (output-type *default-output-type*))
  (with-track-llvm-time
      (progn
        (let* ((triple-string (llvm-sys:get-target-triple module))
               (normalized-triple-string (llvm-sys:triple-normalize triple-string))
               (triple (llvm-sys:make-triple normalized-triple-string))
               (target-options (llvm-sys:make-target-options)))
          (multiple-value-bind (target msg)
              (llvm-sys:target-registry-lookup-target "" triple)
            (unless target
              (error msg))
            (llvm-sys:emit-module (llvm-sys:create-target-machine target
                                                                  (llvm-sys:get-triple triple)
                                                                  ""
                                                                  ""
                                                                  target-options
                                                                  reloc-model
                                                                  (code-model :jit nil :output-type output-type)
                                                                  'llvm-sys:code-gen-opt-default
                                                                  nil)
                                  output-stream
                                  nil ; dwo-stream for dwarf objects
                                  file-type module))))))

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
    (with-module (:module module
                  :optimize (when optimize #'llvm-sys:optimize-module)
                  :optimize-level optimize-level)
      ;; (1) Generate the code
      (with-debug-info-generator (:module *the-module*
                                  :pathname *compile-file-source-debug-pathname*)
        (with-make-new-run-all (run-all-function name)
          (with-literal-table (:id 0)
            (loop-read-and-compile-file-forms source-sin environment))
          (setf run-all-name (llvm-sys:get-name run-all-function))))
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

(defun enable-bytecode-file-compiler ()
  (setf *default-output-type* :bytecode))

(defun disable-bytecode-file-compiler ()
  (setf *default-output-type* :faso))

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
                       ;; output-type can be (or :faso :fasobc :fasoll :bytecode)
                       (output-type *default-output-type*)
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
    (let* ((output-path (apply #'compile-file-pathname input-file args))
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
                                (output-type *default-output-type*)
                                ;; type can be either :kernel or :user
                                (type :user)
                                ;; Control the order of startup functions
                                (image-startup-position (core:next-startup-position)) 
                                environment
                              &allow-other-keys)
  (let ((*compile-file-parallel* nil))
    (if (eq output-type :bytecode)
        (apply #'cmpltv:bytecode-compile-stream input-stream output-path args)
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
                                          :position image-startup-position)))))
  (truename output-path))

(defun compile-file-output-module (module output-file output-type type
                                   &key position)
  (declare (ignore type))
  (setq output-file (make-pathname :defaults output-file :version nil
                                   :type (core:build-extension output-type)))
  (ensure-directories-exist output-file)
  (when *compile-verbose*
    (format t "~&; Writing ~a file to: ~a~%" output-type output-file))
  (ecase output-type
    (:faso
     (core:write-faso output-file
                      (list (generate-obj-asm-stream
                             module :simple-vector-byte8
                             'llvm-sys:code-gen-file-type-object-file
                             *default-reloc-model*))
                      :start-object-id position))
    (:fasoll
     (with-atomic-file-rename (temp-pathname output-file)
       (with-open-file (fout temp-pathname :direction :output
                                           :if-exists :supersede)
         (llvm-sys:dump-module module fout))))
    (:fasobc
     (with-atomic-file-rename (temp-pathname output-file)
       (llvm-sys:write-bitcode-to-file module
                                       (namestring temp-pathname)))))
  (with-track-llvm-time (llvm-sys:module-delete module)))

(export 'compile-file)
