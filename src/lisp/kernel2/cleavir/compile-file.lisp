(in-package #:cmp)

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

(defun generate-obj-asm-stream (module output-stream file-type reloc-model &key (output-type *default-output-type*))
  (with-track-llvm-time
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
                                file-type module)))))

;;; Called from compile-stream/serial
(defun native-compile-stream (input-stream output-path
                              &key (optimize t)
                                (optimize-level *optimization-level*)
                                (output-type *default-output-type*)
                                (type :user)
                                (image-startup-position
                                 (core:next-startup-position))
                                environment
                                (reader-client *cst-client*)
                              &allow-other-keys)
  (with-compiler-timer (:message "Compile-file"
                        :report-link-time t
                        :verbose *compile-verbose*)
    (let ((module (compile-stream-to-module input-stream
                                            :environment environment
                                            :reader-client reader-client
                                            :image-startup-position image-startup-position
                                            :optimize optimize
                                            :optimize-level optimize-level)))
      (compile-file-output-module module output-path output-type
                                  type
                                  :position image-startup-position))))

(defun compile-stream-to-module (source-sin
                                 &key
                                   environment
                                   (reader-client *cst-client*)
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
          (literal:with-literal-table (:id 0)
            (clasp-cleavir::bir-loop-read-and-compile-file-forms
             source-sin environment reader-client))
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

(defun build-extension (type)
  (cond ((or (eq type :bytecode)
             (member :bytecode *features*))
         "fasl")
        ((eq type :faso)
         "faso")
        ((eq type :fasoll)
         "fasoll")
        ((eq type :fasobc)
         "fasobc")
        (t
         (error "Unsupported build-extension type ~a" type))))

(defun compile-file-output-module (module output-file output-type type
                                   &key position)
  (declare (ignore type))
  (setq output-file (make-pathname :defaults output-file :version nil
                                   :type (build-extension output-type)))
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
