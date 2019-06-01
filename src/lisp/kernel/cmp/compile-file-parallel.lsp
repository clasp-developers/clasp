
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

(defstruct (ast-job (:type vector) :named) ast environment dynenv form-output-path form-index error current-source-pos-info)

(defun compile-from-ast (job &key
                               optimize
                               optimize-level
                               intermediate-output-type)
  (handler-case
      (let ((module (cmp::llvm-create-module (namestring (ast-job-form-output-path job))))
            (core:*current-source-pos-info* (ast-job-current-source-pos-info job)))
        (with-module (:module module
                      :optimize (when optimize #'optimize-module-for-compile-file)
                      :optimize-level optimize-level)
          (with-debug-info-generator (:module module
                                      :pathname *compile-file-truename*)
            (with-make-new-run-all (run-all-function (namestring (ast-job-form-output-path job)))
              (with-literal-table
                  (let ((clasp-cleavir::*llvm-metadata* (make-hash-table :test 'eql)))
                    (core:with-memory-ramp (:pattern 'gctools:ramp)
                      (literal:with-top-level-form
                          (let ((hoisted-ast (clasp-cleavir::hoist-ast
                                              (ast-job-ast job)
                                              (ast-job-dynenv job))))
                            (clasp-cleavir::translate-hoisted-ast hoisted-ast :env (ast-job-environment job)))))))
              (make-boot-function-global-variable module run-all-function :position (ast-job-form-index job))))
          (cmp-log "About to verify the module%N")
          (cmp-log-dump-module module)
          (irc-verify-module-safe module)
          (quick-module-dump module (format nil "preoptimize~a" (ast-job-form-index job)))
          ;; ALWAYS link the builtins in, inline them and then remove them.
          (link-inline-remove-builtins module))
        (cond
          ((eq intermediate-output-type :object)
           (let ((object-file-path (make-pathname :type "o" :defaults (ast-job-form-output-path job))))
             (ensure-directories-exist object-file-path)
             ;; Save the bitcode so we can take a look at it
             (with-track-llvm-time
                 (let ((bitcode-file (core:coerce-to-filename (cfp-output-file-default object-file-path :bitcode))))
                   (write-bitcode module bitcode-file)))
             (with-open-file (fout object-file-path :direction :output)
               (let ((reloc-model (cond
                                    ((or (member :target-os-linux *features*) (member :target-os-freebsd *features*))
                                     'llvm-sys:reloc-model-pic-)
                                    (t 'llvm-sys:reloc-model-undefined))))
                 (generate-obj-asm module fout :file-type 'llvm-sys:code-gen-file-type-object-file :reloc-model reloc-model)))))
          ((eq intermediate-output-type :bitcode)
           (with-track-llvm-time
               (let ((bitcode-file (core:coerce-to-filename (cfp-output-file-default (ast-job-form-output-path job) :bitcode))))
                 (write-bitcode module bitcode-file))))
          (t ;; fasl
           (error "Only options for intermediate-output-type are :object or :bitcode - not ~a" intermediate-output-type))))
    (error (e) (setf (ast-job-error job) e))))

(defun wait-for-ast-job (queue &key optimize optimize-level intermediate-output-type)
  (unwind-protect
       (loop for ast-job = (core:dequeue queue :timeout 1.0 :timeout-val nil)
             until (eq ast-job :quit)
             do (if ast-job
                    (progn
                      (cfp-log "Thread ~a compiling form~%" (mp:process-name mp:*current-process*))
                      (compile-from-ast ast-job
                                        :optimize optimize
                                        :optimize-level optimize-level
                                        :intermediate-output-type intermediate-output-type)
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
                       working-dir
                       (intermediate-output-type :object) ; or :bitcode
                       ast-only
                       verbose)
  (let (result
        (form-index (core:next-startup-position))
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
                                                  :intermediate-output-type intermediate-output-type))
                    `((*compile-print* . ',*compile-print*)
                      (*compile-verbose* . ',*compile-verbose*)
                      (*compile-file-output-pathname* . ',*compile-file-output-pathname*)
                      (*package* . ',*package*)
                      (*compile-file-pathname* . ',*compile-file-pathname*)
                      (*compile-file-truename* . ',*compile-file-truename*)
                      (*source-debug-pathname* . ',*source-debug-pathname*)
                      (*source-debug-offset* . ',*source-debug-offset*)
                      #+cclasp(cleavir-generate-ast:*compiler* . ',cleavir-generate-ast:*compiler*)
                      #+cclasp(core:*use-cleavir-compiler* . ',core:*use-cleavir-compiler*)
                      (cmp::*global-function-refs* . ',cmp::*global-function-refs*))))))
      (unwind-protect
           (loop
             ;; Required to update the source pos info. FIXME!?
             (peek-char t source-sin nil)
             ;; FIXME: if :environment is provided we should probably use a different read somehow
             (let* ((current-source-pos-info (core:input-stream-source-pos-info source-sin))
                    (core:*current-source-pos-info* current-source-pos-info)
                    (form-output-path
                      (make-pathname
                       :name (format nil "~a_~d" (pathname-name output-path) form-index)
                       :defaults working-dir))
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
               (push form-output-path result)
               (let ((ast-job (make-ast-job :ast ast
                                            :environment environment
                                            :dynenv dynenv
                                            :current-source-pos-info current-source-pos-info
                                            :form-output-path form-output-path
                                            :form-index form-index)))
                 (when *compile-print* (cmp::describe-form form))
                 (unless ast-only
                   (push ast-job ast-jobs)
                   (core:atomic-enqueue ast-queue ast-job))
                 #+(or)
                 (compile-from-ast ast-job
                                   :optimize optimize
                                   :optimize-level optimize-level
                                   :intermediate-output-type intermediate-output-type))
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
    ;; Now return the results
    (values (nreverse result))))


(defun compile-file-to-result (given-input-pathname
                               &key
                                 compile-file-hook
                                 output-type
                                 output-path
                                 working-dir
                                 source-debug-pathname
                                 (source-debug-offset 0)
                                 environment
                                 (optimize t)
                                 (optimize-level *optimization-level*)
                                 verbose
                                 ast-only
                                 dry-run)
  "* Arguments
- given-input-pathname :: A pathname.
- output-path :: A pathname.
- compile-file-hook :: A function that will do the compile-file
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
         (source-sin (open given-input-pathname :direction :input))
         warnings-p failure-p)
    (with-open-stream (sin source-sin)
      ;; If a truename is provided then spoof the file-system to treat input-pathname
      ;; as source-truename with the given offset
      (when source-debug-pathname
        (core:file-scope (namestring given-input-pathname) source-debug-pathname source-debug-offset nil))
      (when *compile-verbose*
        (bformat t "; Compiling file parallel: %s%N" (namestring given-input-pathname)))
      (let* ((*compilation-module-index* 0)
             (*compile-file-pathname* (pathname (merge-pathnames given-input-pathname)))
             (*compile-file-truename* (translate-logical-pathname *compile-file-pathname*)))
        (with-source-pathnames (:source-pathname *compile-file-truename* ;(namestring source-location)
                                :source-debug-pathname source-debug-pathname
                                :source-debug-offset source-debug-offset)
          (let ((intermediate-output-type (case output-type
                                            (:fasl :object)
                                            (:object :object)
                                            (:bitcode :bitcode))))
            (cclasp-loop2 given-input-pathname source-sin environment
                          :dry-run dry-run
                          :optimize optimize
                          :optimize-level optimize-level
                          :working-dir working-dir
                          :output-path output-path
                          :intermediate-output-type intermediate-output-type
                          :ast-only ast-only
                          :verbose verbose)))))))

(defun cfp-result-files (result extension)
  (mapcar (lambda (name)
            (make-pathname :type extension :defaults name))
          result))

(defun output-cfp-result (result output-path output-type)
  (ensure-directories-exist output-path)
  (cond
    #+(or)
    ((eq output-type :bitcode)
     (cf2-log "output-type :bitcode  result -> ~s~%" result)
     (link-modules output-path result))
    ((eq output-type :fasl)
     (let ((output-path (make-pathname :type (bitcode-extension) :defaults output-path)))
       (llvm-link output-path :input-files (cfp-result-files result (bitcode-extension))
                              :link-type :bitcode))
     (llvm-link output-path :input-files (cfp-result-files result "o")
                            :input-type :object))
    ((eq output-type :object)
     (let ((output-path (make-pathname :type (bitcode-extension) :defaults output-path)))
       (llvm-link output-path :input-files (cfp-result-files result (bitcode-extension))
                              :link-type :bitcode))
     (let ((output-path (make-pathname :type "o" :defaults output-path)))
       (llvm-link output-path :input-files (cfp-result-files result "o")
                              :link-type :object)))
    (t ;; unknown
     (error "Add support for output-type: ~a" output-type))))

(defun compile-file-parallel (input-file
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
                                ;; type can be either :kernel or :user (FIXME? unused)
                                (type :user)
                                ;; ignored by bclasp
                                ;; but passed to hook functions
                                environment
                                ;; Use as little llvm as possible for timing
                                dry-run ast-only)
  "See CLHS compile-file."
  (if (not output-file-p) (setq output-file (cfp-output-file-default input-file output-type)))
  (with-compiler-env ()
    ;; Do the different kind of compile-file here
    (let* ((*compile-print* print)
           (*compile-verbose* verbose)
           (input-pathname (or (probe-file input-file)
                               (error 'core:simple-file-error
                                      :pathname input-file
                                      :format-control "compile-file-to-module could not find the file ~s to open it"
                                      :format-arguments (list input-file))))
           (output-path (compile-file-pathname input-file :output-file output-file :output-type output-type ))
           (working-dir (core:mkdtemp (namestring output-path)))
           (*compile-file-output-pathname* output-path))
      (with-compiler-timer (:message "Compile-file-parallel" :report-link-time t :verbose verbose)
        (with-compilation-results ()
          (let ((result (compile-file-to-result input-pathname
                                                :output-type output-type
                                                :output-path output-path
                                                :source-debug-pathname source-debug-pathname
                                                :source-debug-offset source-debug-offset
                                                :working-dir working-dir
                                                :compile-file-hook *cleavir-compile-file-hook*
                                                :environment environment
                                                :optimize optimize
                                                :optimize-level optimize-level
                                                :verbose verbose
                                                :ast-only ast-only
                                                :dry-run dry-run)))
            (cf2-log "Came out of compile-file-to-result with result: ~s~%" result)
;;;          (loop for one in result do (format t "Result: ~s~%" one))
            (cond (dry-run (format t "Doing nothing further~%"))
                  ((null output-path)
                   (error "The output-file is nil for input filename ~a~%" input-file))
                  (t (output-cfp-result result output-path output-type)))
            output-path))))))

(defvar *compile-file-parallel* nil)
                              
(defun cl:compile-file (input-file &rest args)
  (if *compile-file-parallel*
      (apply #'compile-file-parallel input-file args)
      (apply #'compile-file-serial input-file args)))

(eval-when (:load-toplevel)
  (setf *compile-file-parallel* nil))
