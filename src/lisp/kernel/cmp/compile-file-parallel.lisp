
(in-package :cmp)

#+(or)
(defmacro cf2-log (fmt &rest args)
  `(format *error-output* ,fmt ,@args))
(defmacro cf2-log (fmt &rest args) (declare (ignore fmt args)))

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
(defmacro cfp-log (fmt &rest args) (declare (ignore fmt args)))

(defstruct (ast-job (:type vector) :named)
  form ast environment output-object
  form-index   ; Uses (core:next-startup-position) to keep count
  form-counter ; Counts from zero
  module
  (serious-condition nil) (warnings nil) (notes nil) (other-conditions nil)
  current-source-pos-info startup-function-name form-output-path)


(defun compile-from-module (job &key
                                  optimize
                                  optimize-level
                                  intermediate-output-type
                                  write-bitcode)
  (declare (ignore optimize optimize-level))
  (let ((module (ast-job-module job)))
    (cond
      ((member intermediate-output-type '(:object :in-memory-object))
       (let ((reloc-model (cond
                            ((or (member :target-os-linux *features*)
                                 (member :target-os-freebsd *features*))
                             'llvm-sys:reloc-model-pic-)
                            (t 'llvm-sys:reloc-model-undefined))))
         (when write-bitcode
           (let ((bitcode-filename (core:coerce-to-filename
                                    (cfp-output-file-default
                                     (ast-job-form-output-path job)
                                     :bitcode))))
             (write-bitcode module bitcode-filename :output-type :object)))
         (let ((output (generate-obj-asm-stream
                        module (ast-job-output-object job)
                        'llvm-sys:code-gen-file-type-object-file reloc-model)))
           (when output (setf (ast-job-output-object job) output)))))
      ((eq intermediate-output-type :in-memory-module)
       (let ((llvm-ir (with-output-to-string (sout)
                        (llvm-sys:dump-module module sout))))
       (setf (ast-job-output-object job) llvm-ir)))
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
       (error "Only options for intermediate-output-type are :object or :bitcode - not ~a" intermediate-output-type)))
    (gctools:thread-local-cleanup))
  (values))

(defun ast-job-to-module (job &key optimize optimize-level)
  (let ((module (llvm-create-module (format nil "module~a" (ast-job-form-index job))))
        (core:*current-source-pos-info* (ast-job-current-source-pos-info job)))
    (with-module (:module module
                  :optimize (when optimize #'optimize-module-for-compile-file)
                  :optimize-level optimize-level)
      (with-debug-info-generator (:module module
                                  :pathname *compile-file-truename*)
        (with-make-new-run-all (run-all-function (format nil "module~a" (ast-job-form-index job)))
          (with-literal-table (:id (ast-job-form-index job))
              (core:with-memory-ramp (:pattern 'gctools:ramp)
                (literal:arrange-thunk-as-top-level
                 (clasp-cleavir-translate-bir::translate-ast
                  (ast-job-ast job)))))
          (let ((startup-function (add-global-ctor-function module run-all-function
                                                            :position (ast-job-form-counter job))))
;;;                (add-llvm.used module startup-function)
            (add-llvm.global_ctors module 15360 startup-function)
            (setf (ast-job-startup-function-name job) (llvm-sys:get-name startup-function))
            ;; The link-once-odrlinkage should keep the startup-function alive and that
            ;; should keep everything else alive as well.
            )
          #+(or)
          (make-boot-function-global-variable module run-all-function
                                                    :position (ast-job-form-index job)
                                                    )))
      (cmp-log "About to verify the module%N")
      (cmp-log-dump-module module)
      (irc-verify-module-safe module)
      (quick-module-dump module (format nil "preoptimize~a" (ast-job-form-index job)))
      ;; ALWAYS link the builtins in, inline them and then remove them.
      #+(or)(link-inline-remove-builtins module)
      module)))

(defun compile-from-ast (job &key
                               optimize
                               optimize-level
                               intermediate-output-type
                               write-bitcode)
  (setf (ast-job-module job)
        (ast-job-to-module job :optimize optimize
                               :optimize-level optimize-level))
  (compile-from-module job :optimize optimize :optimize-level optimize-level
                           :intermediate-output-type intermediate-output-type
                           :write-bitcode write-bitcode))

(defparameter *ast-job* nil)
(defun wait-for-ast-job (queue &key compile-func optimize optimize-level intermediate-output-type write-bitcode)
  (unwind-protect
       (loop for ast-job = (core:dequeue queue :timeout 1.0 :timeout-val nil)
             until (eq ast-job :quit)
             do (if ast-job
                    (let ((*ast-job* ast-job))
                      (cfp-log "Thread ~a compiling form~%" (mp:process-name mp:*current-process*))
                      (block nil
                        (handler-bind
                            ((serious-condition
                               (lambda (e)
                                 (setf (ast-job-serious-condition ast-job) e)
                                 ;; Cannot continue with this job
                                 (return)))
                             (warning
                               (lambda (w)
                                 (push w (ast-job-warnings ast-job))
                                 ;; Will be reported in the main thread instead.
                                 (muffle-warning w)))
                             (ext:compiler-note
                               (lambda (n)
                                 (push n (ast-job-notes ast-job))
                                 (muffle-note n)))
                             ((not (or ext:compiler-note
                                       serious-condition warning))
                               (lambda (c)
                                 (push c (ast-job-other-conditions ast-job)))))
                          (funcall compile-func ast-job
                                   :optimize optimize
                                   :optimize-level optimize-level
                                   :intermediate-output-type intermediate-output-type
                                   :write-bitcode write-bitcode)))
                      (cfp-log "Thread ~a done with form~%" (mp:process-name mp:*current-process*)))
                    (cfp-log "Thread ~a timed out during dequeue - trying again~%" (mp:process-name mp:*current-process*))))
    (cfp-log "Leaving thread ~a~%" (mp:process-name mp:*current-process*))))


(defun cclasp-loop2 (source-sin
                     environment
                     &key
                       (compile-from-module nil) ; If nil - then compile from the ast in threads
                       optimize
                       optimize-level
                       output-path
                       write-bitcode
                       (intermediate-output-type :in-memory-object) ; or :bitcode

                       ast-only)
  "The loop that creates parallel jobs (ast-job).  The jobs can be setup
to build the source->AST->HIR->LLVM-IR in serial mode and then compile the Module in parallel threads.
This is controlled using the :compile-from-module option.   When it is T then 
AST->HIR->LLVM-IR is done in serial and in parallel it compiles LLVM-IR->Object files.
Or it can go from source->AST and then compile the AST->HIR->LLVM-IR->ObjectFiles
in parallel threads. The reason for the two methods is that the AST->HIR uses the 
garbage collector heavily and Boehm doesn't work well in multithreaded mode.
Boehm has a mutex and stack unwinding involves a mutex on linux.
So as an experiment I tried doing AST->HIR and HIR->LLVM-IR in serial and 
then leave the LLVM stuff to be done in parallel.   That slows down so much
that it's not worth it either.   It would be better to improve the garbage collector (MPS)
to work better in a multithreaded way.  There are also options for Boehm to improve
multithreaded performance that we should explore."
  (let ((form-index (core:next-startup-position))
        (form-counter 0)
        (eof-value (gensym))
        #+cclasp (cleavir-cst-to-ast:*compiler*
                  'cl:compile-file)
        #+cclasp(core:*use-cleavir-compiler* t)
        #+cclasp(eclector.reader:*client* clasp-cleavir::*cst-client*)
        #+cclasp(eclector.readtable:*readtable* cl:*readtable*)
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
                      (wait-for-ast-job ast-queue
                                        :compile-func (if compile-from-module
                                                          'compile-from-module
                                                          'compile-from-ast)
                                        :optimize optimize
                                        :optimize-level optimize-level
                                        :intermediate-output-type intermediate-output-type
                                        :write-bitcode write-bitcode))
                    `((*compile-print* . ',*compile-print*)
                      (*compile-file-parallel* . ',*compile-file-parallel*)
                      (*default-object-type* . ',*default-object-type*)
                      (*compile-verbose* . ',*compile-verbose*)
                      (*compile-file-output-pathname* . ',*compile-file-output-pathname*)
                      (*package* . ',*package*)
                      (*compile-file-pathname* . ',*compile-file-pathname*)
                      (*compile-file-truename* . ',*compile-file-truename*)
                      #+cclasp(cleavir-cst-to-ast:*compiler*
                               . ',cleavir-cst-to-ast:*compiler*)
                      #+cclasp(core:*use-cleavir-compiler* . ',core:*use-cleavir-compiler*)
                      (*global-function-refs* . ',*global-function-refs*))))))
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
                    (cst (eclector.concrete-syntax-tree:cst-read source-sin nil eof-value))
                    (_ (when (eq cst eof-value) (return nil)))
                    (form (cst:raw cst))
                    (pre-ast
                      (if *debug-compile-file*
                          (with-compiler-timer ()
                           (clasp-cleavir-translate-bir::cst->ast cst))
                          (clasp-cleavir-translate-bir::cst->ast cst)))
                    (ast (clasp-cleavir-translate-bir::wrap-ast pre-ast)))
               (declare (ignore _))
               (let ((ast-job (make-ast-job :form form
                                            :ast ast
                                            :environment environment
                                            :current-source-pos-info current-source-pos-info
                                            :form-output-path form-output-path
                                            :output-object (cond
                                                             ((eq intermediate-output-type :in-memory-object)
                                                              :simple-vector-byte8)
                                                             ((eq intermediate-output-type :in-memory-module)
                                                              nil)
                                                             (t
                                                              (error "Handle intermediate-output-type ~a" intermediate-output-type)))
                                            :form-index form-index
                                            :form-counter form-counter)))
                 (when compile-from-module
                   (let ((module (ast-job-to-module ast-job :optimize optimize :optimize-level optimize-level)))
                     (setf (ast-job-module ast-job) module)))
                 (when *compile-print* (describe-form form))
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
      ;; Now send :quit messages to all threads
      (loop for thread in ast-threads
            do (cfp-log "Sending two :quit (why not?) for thread ~a~%" (mp:process-name thread))
            do (core:atomic-enqueue ast-queue :quit)
               (core:atomic-enqueue ast-queue :quit))
      ;; Now wait for all threads to join
      (loop for thread in ast-threads
            do (mp:process-join thread)
               (cfp-log "Process-join of thread ~a~%" (mp:process-name thread))))
    (dolist (job ast-jobs)
      (let ((*default-condition-origin*
              (ignore-errors
               (loop for origin = (cleavir-ast:origin (ast-job-ast job))
                       then (cst:source origin)
                     while (typep origin 'cst:cst)
                     finally (return origin)))))
        (mapc #'signal (ast-job-other-conditions job))
        ;; The WARN calls here never actually print warnings - the
        ;; with-compilation-results handlers do, and then muffle the warnings
        ;; (which is why we use WARN and not SIGNAL). Kind of ugly.
        (mapc #'warn (ast-job-warnings job))
        (mapc #'cmp:note (ast-job-notes job))
        (when (ast-job-serious-condition job)
          ;; We use SIGNAL rather than ERROR although the condition is serious.
          ;; This is because the AST job has already exited and therefore there
          ;; is no way to debug the problem. with-compilation-results will
          ;; still understand that it's an error and report compilation failure.
          ;; It's possible we could save the original backtrace and so on, but
          ;; if you want to debug problems, it would probably be easier to
          ;; use the serial compiler and debug them as they appear.
          (signal (ast-job-serious-condition job)))))
    ;; Now print the names of the startup ctor functions
    ;;     Next we need to compile a new module that declares these ctor functions and puts them in a ctor list
    ;;      then it should add this new module to the result list so it can be linked with the others.
    #+(or)
    (dolist (job ast-jobs)
      (format t "ast-job ctor: ~a~%" (ast-job-startup-function-name job)))
    ;; Now return the results
    ast-jobs))


(defun compile-file-to-result (given-input-pathname
                               &key
                               output-type
                               output-path
                               environment
                               (optimize t)
                               (optimize-level *optimization-level*)
                               ast-only
                               write-bitcode
                               external-format)
  "* Arguments
- given-input-pathname :: A pathname.
- output-path :: A pathname.
- environment :: Arbitrary, passed only to hook
Compile a lisp source file into an LLVM module."
  (let* ((*package* *package*)
         (*readtable* *readtable*)
         (clasp-source-root (translate-logical-pathname "sys:"))
         (clasp-source (merge-pathnames (make-pathname :directory '(:relative :wild-inferiors) :name :wild :type :wild) clasp-source-root))
         (source-sin (open given-input-pathname :direction :input :external-format (or external-format :default))))
    (declare (ignore clasp-source))
    (with-open-stream (sin source-sin)
      (when *compile-verbose*
        (core:fmt t "; Compiling file parallel: {}%N" (namestring given-input-pathname)))
      (let ((intermediate-output-type (case output-type
                                        #+(or)(:fasl :object)
                                        (:fasl :in-memory-object)
                                        (:faso :in-memory-object)
                                        (:fasoll :in-memory-module)
                                        (:fasobc :in-memory-module)
                                        (:faspll :in-memory-module)
                                        (:faspbc :in-memory-module)
                                        (:fasp :in-memory-object)
                                        (:object :in-memory-object)
                                        (:bitcode :bitcode)
                                        (otherwise (error "Figure out intermediate-output-type for output-type ~s" output-type)))))
        (cclasp-loop2 source-sin environment
                      :optimize optimize
                      :optimize-level optimize-level
                      :output-path output-path
                      :intermediate-output-type intermediate-output-type
                      :ast-only ast-only
                      :write-bitcode write-bitcode)))))



(defun link-compile-file-parallel-modules (output-pathname parts)
  "Link a bunch of modules together, return the linked module"
  (let* ((link-module (llvm-create-module (pathname-name output-pathname))))
    ;; Don't enforce .bc extension for additional-bitcode-pathnames
    ;; This is where I used to link the additional-bitcode-pathnames
    (loop for part-llvm-ir in parts
          for module = (llvm-sys:parse-irstring part-llvm-ir (thread-local-llvm-context))
          do (multiple-value-bind (failure error-msg)
                 (llvm-sys:link-modules link-module module)
               (when failure
                 (format t "While linking part module encountered error: ~a~%" error-msg))))
    link-module))

(defun output-cfp-result (ast-jobs output-path output-type)
  (ensure-directories-exist output-path)
  (cond
    ((member output-type '(:object :fasl :faso :fasp))
     (let (#+(or)(output-path (compile-file-pathname output-path :output-type output-type)))
       #+(or)(format t "Output the object files in ast-jobs to ~s~%" output-path)
       (let* ((object-files (loop for ast-job in ast-jobs
                                  for index = (ast-job-form-index ast-job)
                                  for ostream = (ast-job-output-object ast-job)
                                  collect (cons index ostream)))
              (sorted-object-files (sort object-files #'< :key #'car)))
         #+(or)(format t "sorted-object-files length ~d output-path: ~s~%" (length sorted-object-files) output-path)
         (core:write-faso output-path (mapcar #'cdr sorted-object-files)))))
    ((member output-type '(:fasoll :fasobc :faspll :faspbc))
     (let ((module (link-compile-file-parallel-modules (namestring output-path)
                                                       (mapcar #'ast-job-output-object ast-jobs))))
       (cond
         ((member output-type '(:fasoll :faspll))
          (let ((fout (open output-path :direction :output :if-exists :supersede)))
            (llvm-sys:dump-module module fout)))
         ((member output-type '(:fasobc :faspbc))
          (llvm-sys:write-bitcode-to-file module (namestring output-path)))
         (t (error "Handle output-type for ~a" output-type)))))
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
                                (output-type (default-library-type) output-type-p)
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
  (declare (ignore type cleanup))
  (setf output-type (maybe-fixup-output-type output-type output-type-p))
  (let ((*compile-file-parallel* t))
    (if (not output-file-p) (setq output-file (cfp-output-file-default input-file output-type)))
    (with-compiler-env ()
      (let* ((input-pathname (or (probe-file input-file)
                                 (error 'core:simple-file-error
                                        :pathname input-file
                                        :format-control "compile-file-to-module could not find the file ~s to open it"
                                        :format-arguments (list input-file))))
             (output-path (if output-type-p
                              (compile-file-pathname input-file :output-file output-file :output-type output-type)
                              (compile-file-pathname input-file :output-file output-file)))
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
            (let ((ast-jobs
                    (compile-file-to-result input-pathname
                     :output-type output-type
                     :output-path output-path
                     :environment environment
                     :optimize optimize
                     :optimize-level optimize-level
                     :ast-only ast-only
                     :write-bitcode write-bitcode
                     :external-format external-format)))
              (cond (dry-run (format t "Doing nothing further~%") nil)
                    ((null output-path)
                     (error "The output-file is nil for input filename ~a~%" input-file))
                    ((some #'ast-job-serious-condition ast-jobs)
                     ;; There was an insurmountable error - stop.
                     nil)
                    ;; Usual result
                    (t (output-cfp-result ast-jobs output-path output-type)
                       (truename output-path))))))))))

(defun cl:compile-file (input-file &rest args &key (output-type (default-library-type) output-type-p)
                                                output-file (verbose *compile-verbose*) &allow-other-keys)
  (setf output-type (maybe-fixup-output-type output-type output-type-p))
  (flet ((do-compile-file ()
           (cond ((or output-type-p (null output-file))
                  (if *compile-file-parallel*
                      (apply #'compile-file-parallel input-file
                             :output-type output-type args)
                      (apply #'compile-file-serial input-file
                             :output-type output-type args)))
                 (t (if *compile-file-parallel*
                        (apply #'compile-file-parallel input-file args)
                        (apply #'compile-file-serial input-file args))))))
    (if verbose
        (time (do-compile-file))
        (do-compile-file))))

(eval-when (:load-toplevel)
  (setf *compile-file-parallel* *use-compile-file-parallel*))
