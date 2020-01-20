
(in-package :cmp)

#+(or)
(defmacro cf2-log (fmt &rest args)
  `(format *error-output* ,fmt ,@args))
(defmacro cf2-log (fmt &rest args)
  nil)

#+(or)
(progn
  (defparameter *cfp-message-mutex* (mp:make-lock :name "message-mutex"))
  (defmacro cfp-log (fmt &rest args)
    `(unwind-protect
          (progn
            (mp:get-lock *cfp-message-mutex*)
            (format *error-output* ,fmt ,@args))
       (mp:giveup-lock *cfp-message-mutex*))))
;;;#+(or)
(defmacro cfp-log (fmt &rest args)
  nil)

(defstruct (ast-job (:type vector) :named)
  ast environment dynenv output-stream
  form-index   ; Uses (core:next-startup-position) to keep count
  form-counter ; Counts from zero
  error current-source-pos-info startup-function-name form-output-path)


(defun compile-from-ast (job &key
                               optimize
                               optimize-level
                               intermediate-output-type
                               write-bitcode)
  (handler-case
      (let ((module (cmp::llvm-create-module (format nil "module~a" (ast-job-form-index job))))
            (core:*current-source-pos-info* (ast-job-current-source-pos-info job)))
        (with-module (:module module
                      :optimize (when optimize #'optimize-module-for-compile-file)
                      :optimize-level optimize-level)
          (with-debug-info-generator (:module module
                                      :pathname *compile-file-truename*)
            (with-make-new-run-all (run-all-function (format nil "module~a" (ast-job-form-index job)))
              (with-literal-table
                  (core:with-memory-ramp (:pattern 'gctools:ramp)
                    (literal:with-top-level-form
                        (let ((hoisted-ast (clasp-cleavir::hoist-ast
                                            (ast-job-ast job)
                                            (ast-job-dynenv job))))
                          (clasp-cleavir::translate-hoisted-ast hoisted-ast :env (ast-job-environment job))))))
              (let ((startup-function (add-global-ctor-function module run-all-function
                                                                :position (ast-job-form-index job)
                                                                :linkage 'llvm-sys:external-linkage)))
;;;                (add-llvm.used module startup-function)
                (setf (ast-job-startup-function-name job) (llvm-sys:get-name startup-function))
                ;; The link-once-odrlinkage should keep the startup-function alive and that
                ;; should keep everything else alive as well.
                )
              #+(or)(make-boot-function-global-variable module run-all-function
                                                        :position (ast-job-form-index job)
                                                        :linkage 'llvm-sys:link-once-odrlinkage)))
          (cmp-log "About to verify the module%N")
          (cmp-log-dump-module module)
          (irc-verify-module-safe module)
          (quick-module-dump module (format nil "preoptimize~a" (ast-job-form-index job)))
          ;; ALWAYS link the builtins in, inline them and then remove them.
          #+(or)(link-inline-remove-builtins module))
        (cond
          ((member intermediate-output-type '(:object :in-memory-object))
           (let ((reloc-model (cond
                                ((or (member :target-os-linux *features*) (member :target-os-freebsd *features*))
                                 'llvm-sys:reloc-model-pic-)
                                (t 'llvm-sys:reloc-model-undefined))))
             (when write-bitcode
               (let ((bitcode-filename (core:coerce-to-filename (cfp-output-file-default (ast-job-form-output-path job) :bitcode))))
                 (write-bitcode module bitcode-filename)))
             (let ((output (generate-obj-asm-stream module (ast-job-output-stream job) 'llvm-sys:code-gen-file-type-object-file reloc-model)))
               (when output (setf (ast-job-output-stream job) output))
               )))
          #+(or)
          ((eq intermediate-output-type :object)
           (let ((object-file-path (make-pathname :type "o" :defaults (ast-job-form-output-path job))))
             (ensure-directories-exist object-file-path)
             ;; Save the bitcode so we can take a look at it
             (with-track-llvm-time
                 (let ((bitcode-file (core:coerce-to-filename (cfp-output-file-default object-file-path :bitcode))))
                   (write-bitcode module bitcode-file)))
             (let ((reloc-model (cond
                                  ((or (member :target-os-linux *features*) (member :target-os-freebsd *features*))
                                   'llvm-sys:reloc-model-pic-)
                                  (t 'llvm-sys:reloc-model-undefined))))
               (generate-obj-asm module object-file-path :file-type 'llvm-sys:code-gen-file-type-object-file :reloc-model reloc-model))))
          #+(or)
          ((eq intermediate-output-type :bitcode)
           (with-track-llvm-time
               (let ((bitcode-file (core:coerce-to-filename (cfp-output-file-default (ast-job-form-output-path job) :bitcode))))
                 (write-bitcode module bitcode-file))))
          (t ;; fasl
           (error "Only options for intermediate-output-type are :object or :bitcode - not ~a" intermediate-output-type))))
    (error (e) (setf (ast-job-error job) e))))

(defun wait-for-ast-job (queue &key optimize optimize-level intermediate-output-type write-bitcode)
  (unwind-protect
       (loop for ast-job = (core:dequeue queue :timeout 1.0 :timeout-val nil)
             until (eq ast-job :quit)
             do (if ast-job
                    (progn
                      (cfp-log "Thread ~a compiling form~%" (mp:process-name mp:*current-process*))
                      (compile-from-ast ast-job
                                        :optimize optimize
                                        :optimize-level optimize-level
                                        :intermediate-output-type intermediate-output-type
                                        :write-bitcode write-bitcode)
                      (cfp-log "Thread ~a done with form~%" (mp:process-name mp:*current-process*)))
                    (cfp-log "Thread ~a timed out during dequeue - trying again~%" (mp:process-name mp:*current-process*))))
    (cfp-log "Leaving thread ~a~%" (mp:process-name mp:*current-process*))))


(defun cclasp-loop2 (input-pathname
                     source-sin
                     environment
                     &key
                       dry-run
                       optimize
                       optimize-level
                       output-path
                       write-bitcode
                       (intermediate-output-type :in-memory-object) ; or :bitcode
                       ast-only)
  (let (result
        (form-index (core:next-startup-position))
        (form-counter 0)
        (eof-value (gensym))
        #+cclasp(cleavir-generate-ast:*compiler* 'cl:compile-file)
        #+cclasp(core:*use-cleavir-compiler* t)
        #+cclasp(eclector.reader:*client* clasp-cleavir::*cst-client*)
        ast-jobs)
    (cfp-log "Starting the pool of threads~%")
    (finish-output)
    (let* ((number-of-threads (core:num-logical-processors))
           (ast-queue (core:make-queue 'compile-file-parallel))
           ;; Setup a pool of threads
           (ast-threads
             (loop for thread-num below number-of-threads
                   collect
                   (mp:process-run-function
                    (format nil "compile-file-parallel-~a" thread-num)
                    (lambda ()
                      (wait-for-ast-job ast-queue :optimize optimize
                                                  :optimize-level optimize-level
                                                  :intermediate-output-type intermediate-output-type
                                                  :write-bitcode write-bitcode))
                    `((*compile-print* . ',*compile-print*)
                      (*compile-verbose* . ',*compile-verbose*)
                      (*compile-file-output-pathname* . ',*compile-file-output-pathname*)
                      (*package* . ',*package*)
                      (*compile-file-pathname* . ',*compile-file-pathname*)
                      (*compile-file-truename* . ',*compile-file-truename*)
                      #+cclasp(cleavir-generate-ast:*compiler* . ',cleavir-generate-ast:*compiler*)
                      #+cclasp(core:*use-cleavir-compiler* . ',core:*use-cleavir-compiler*)
                      (cmp::*global-function-refs* . ',cmp::*global-function-refs*))))))
      (unwind-protect
           (loop
             ;; Required to update the source pos info. FIXME!?
             (peek-char t source-sin nil)
             ;; FIXME: if :environment is provided we should probably use a different read somehow
             (let* ((current-source-pos-info (compile-file-source-pos-info source-sin))
                    (core:*current-source-pos-info* current-source-pos-info)
                    (form-output-path
                      (make-pathname
                       :name (format nil "~a_~d" (pathname-name output-path) form-counter)
                       :defaults output-path))
                    (dynenv (clasp-cleavir::make-dynenv environment))
                    #+cst
                    (cst (eclector.concrete-syntax-tree:cst-read source-sin nil eof-value))
                    #+cst
                    (_ (when (eq cst eof-value) (return nil)))
                    #+cst
                    (form (cst:raw cst))
                    #+cst
                    (ast (if cmp::*debug-compile-file*
                             (clasp-cleavir::compiler-time (clasp-cleavir::cst->ast cst dynenv))
                             (clasp-cleavir::cst->ast cst dynenv)))
                    #-cst
                    (form (read source-sin nil eof-value))
                    #-cst
                    (_ (when (eq form eof-value) (return nil)))
                    #-cst
                    (ast (if cmp::*debug-compile-file*
                             (clasp-cleavir::compiler-time (clasp-cleavir::generate-ast form dynenv))
                             (clasp-cleavir::generate-ast form dynenv))))
               (let ((ast-job (make-ast-job :ast ast
                                            :environment environment
                                            :dynenv dynenv
                                            :current-source-pos-info current-source-pos-info
                                            :form-output-path form-output-path
                                            :output-stream (when (eq intermediate-output-type :in-memory-object)
                                                             :simple-vector-byte8)
                                            :form-index form-index
                                            :form-counter form-counter)))
                 (when *compile-print* (cmp::describe-form form))
                 (unless ast-only
                   (push ast-job ast-jobs)
                   (core:atomic-enqueue ast-queue ast-job))
                 #+(or)
                 (compile-from-ast ast-job
                                   :optimize optimize
                                   :optimize-level optimize-level
                                   :intermediate-output-type intermediate-output-type))
               (incf form-counter)
               (setf form-index (core:next-startup-position)))))
      (progn
        ;; Now send :quit messages to all threads
        (loop for thread in ast-threads
              do (cfp-log "Sending two :quit (why not?) for thread ~a~%" (mp:process-name thread))
              do (core:atomic-enqueue ast-queue :quit)
                 (core:atomic-enqueue ast-queue :quit))
        ;; Now wait for all threads to join
        (loop for thread in ast-threads
              do (mp:process-join thread)
                 (cfp-log "Process-join of thread ~a~%" (mp:process-name thread)))))
    (dolist (job ast-jobs)
      (when (ast-job-error job)
        (error "Compile-error ~a ~%" (ast-job-error job))))
    ;; Now print the names of the startup ctor functions
    ;;     Next we need to compile a new module that declares these ctor functions and puts them in a ctor list
    ;;      then it should add this new module to the result list so it can be linked with the others.
    #+(or)
    (dolist (job ast-jobs)
      (format t "ast-job ctor: ~a~%" (ast-job-startup-function-name job)))
    ;; Now return the results
    (values (nreverse result) ast-jobs)))


(defun compile-file-to-result (given-input-pathname
                               &key
                                 compile-file-hook
                                 output-type
                                 output-path
                                 environment
                                 (optimize t)
                                 (optimize-level *optimization-level*)
                                 ast-only
                                 dry-run
                                 write-bitcode)
  "* Arguments
- given-input-pathname :: A pathname.
- output-path :: A pathname.
- compile-file-hook :: A function that will do the compile-file
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
         (source-sin (open given-input-pathname :direction :input))
         warnings-p failure-p)
    (with-open-stream (sin source-sin)
      (when *compile-verbose*
        (bformat t "; Compiling file parallel: %s%N" (namestring given-input-pathname)))
      (let ((intermediate-output-type (case output-type
                                        #+(or)(:fasl :object)
                                        (:fasl :in-memory-object)
                                        (:object :in-memory-object)
                                        (:bitcode :bitcode))))
        (cclasp-loop2 given-input-pathname source-sin environment
                      :dry-run dry-run
                      :optimize optimize
                      :optimize-level optimize-level
                      :output-path output-path
                      :intermediate-output-type intermediate-output-type
                      :ast-only ast-only
                      :write-bitcode write-bitcode)))))

(defun cfp-result-files (result extension)
  (mapcar (lambda (name)
            (make-pathname :type extension :defaults name))
          result))

(defun output-cfp-result (result ast-jobs output-path output-type)
  (ensure-directories-exist output-path)
  (cond
    #+(or)
    ((eq output-type :bitcode)
     (cf2-log "output-type :bitcode  result -> ~s~%" result)
     (link-modules output-path result))
    #+(or)
    ((eq output-type :fasl)
     (let ((output-path (make-pathname :type (bitcode-extension) :defaults output-path)))
       (llvm-link output-path :input-files (cfp-result-files result (bitcode-extension))
                              :link-type :bitcode))
     (llvm-link output-path :input-files (cfp-result-files result "o")
                            :input-type :object))
    ((member output-type '(:object :fasl))
     (let ((output-path (compile-file-pathname output-path :output-type output-type)))
       #+(or)(format t "Output the object files in ast-jobs to ~s~%" output-path)
       (let* ((object-files (loop for ast-job in ast-jobs
                                  for index = (ast-job-form-index ast-job)
                                  collect (cons index (ast-job-output-stream ast-job))))
              (sorted-object-files (sort object-files #'< :key #'car)))
         #+(or)(format t "sorted-object-files length ~d output-path: ~s~%" (length sorted-object-files) output-path)
         (core:write-faso output-path (mapcar #'cdr sorted-object-files)))))
    #+(or)
    ((eq output-type :object)
     (let ((output-path (make-pathname :type (bitcode-extension) :defaults output-path)))
       (llvm-link output-path :input-files (cfp-result-files result (bitcode-extension))
                              :link-type :bitcode))
     (let ((output-path (make-pathname :type "o" :defaults output-path)))
       (llvm-link output-path :input-files (cfp-result-files result "o")
                              :link-type :object)))
    (t ;; unknown
     (error "Add support for output-type: ~a" output-type))))

(defvar *compile-file-parallel-write-bitcode* nil
  "Force compile-file-parallel to write out bitcode for each module. 
Each bitcode filename will contain the form-index.")


(defun compile-file-parallel (input-file
                              &key
                                (output-file nil output-file-p)
                                ((:verbose *compile-verbose*) *compile-verbose*)
                                ((:print *compile-print*) *compile-print*)
                                (optimize t)
                                (optimize-level *optimization-level*)
                                (external-format :default)
                                ;; Used for C-c C-c in SLIME.
                                (source-debug-pathname nil cfsdpp)
                                ((:source-debug-offset *compile-file-source-debug-offset*) 0)
                                ((:source-debug-lineno *compile-file-source-debug-lineno*) 0)
                                ;; output-type can be (or :fasl :bitcode :object)
                                (output-type :fasl)
                                ;; type can be either :kernel or :user (FIXME? unused)
                                (type :user)
                                ;; ignored by bclasp
                                ;; but passed to hook functions
                                environment
                                ;; Use as little llvm as possible for timing
                                dry-run ast-only
                                ;; Cleanup temporary files
                                (cleanup nil)
                                (write-bitcode *compile-file-parallel-write-bitcode*))
  "See CLHS compile-file."
  (let ((*compile-file-parallel* t))
    (if (not output-file-p) (setq output-file (cfp-output-file-default input-file output-type)))
    (with-compiler-env ()
      (let* ((input-pathname (or (probe-file input-file)
                                 (error 'core:simple-file-error
                                        :pathname input-file
                                        :format-control "compile-file-to-module could not find the file ~s to open it"
                                        :format-arguments (list input-file))))
             (output-path (compile-file-pathname input-file :output-file output-file :output-type output-type))
             (*compilation-module-index* 0)
             (*compile-file-pathname* (pathname (merge-pathnames input-file)))
             (*compile-file-truename* (translate-logical-pathname *compile-file-pathname*))
             (*compile-file-source-debug-pathname*
               (if cfsdpp source-debug-pathname *compile-file-truename*))
             (*compile-file-file-scope*
               (core:file-scope *compile-file-source-debug-pathname*))
             (*compile-file-output-pathname* output-path))
        (with-compiler-timer (:message "Compile-file-parallel" :report-link-time t :verbose *compile-verbose*)
          (with-compilation-results ()
            (multiple-value-bind (result ast-jobs)
                (compile-file-to-result input-pathname
                                        :output-type output-type
                                        :output-path output-path
                                        :compile-file-hook *cleavir-compile-file-hook*
                                        :environment environment
                                        :optimize optimize
                                        :optimize-level optimize-level
                                        :ast-only ast-only
                                        :dry-run dry-run
                                        :write-bitcode write-bitcode)
              (cf2-log "Came out of compile-file-to-result with result: ~s~%" result)
;;;          (loop for one in result do (format t "Result: ~s~%" one))
              (cond (dry-run (format t "Doing nothing further~%"))
                    ((null output-path)
                     (error "The output-file is nil for input filename ~a~%" input-file))
                    (t (output-cfp-result result ast-jobs output-path output-type)))
              output-path)))))))

(defun cl:compile-file (input-file &rest args)
  (if *compile-file-parallel*
      (apply #'compile-file-parallel input-file args)
      (apply #'compile-file-serial input-file args)))

(eval-when (:load-toplevel)
  (setf *compile-file-parallel* cmp:*use-compile-file-parallel*))
