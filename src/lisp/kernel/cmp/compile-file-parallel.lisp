
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
            (format *error-output* ,fmt ,@args)
            (finish-output *error-output*))
       (mp:giveup-lock *cfp-message-mutex*))))
;;;#+(or)
(defmacro cfp-log (fmt &rest args) (declare (ignore fmt args)))

(defclass thread-pool ()
  ((%queue :initarg :queue :reader thread-pool-queue)
   (%threads :initarg :threads :reader thread-pool-threads)))

(defclass job ()
  ((%serious-condition :initform nil :accessor job-serious-condition
                       :type (or null serious-condition))
   (%warnings :initform nil :accessor job-warnings :type list)
   (%notes :initform nil :accessor job-notes :type list)
   (%other-conditions :initform nil :accessor job-other-conditions :type list)))

(defun thread-pool-jobber (queue function arguments)
  (lambda ()
    (unwind-protect
         (loop for job = (core:dequeue queue :timeout 1.0 :timeout-val nil)
               until (eq job :quit)
               when job
                 do (cfp-log "Thread ~a working on ~s~%"
                             (mp:process-name mp:*current-process*) job)
                    (block nil
                      (handler-bind
                          ((serious-condition
                             (lambda (e)
                               (setf (job-serious-condition job) e)
                               ;; Cannot continue with this job,
                               ;; so return to the loop to wait for more jobs.
                               (return)))
                           ;; Other conditions are suppressed and saved for the manager.
                           (warning
                             (lambda (w) (push w (job-warnings job)) (muffle-warning w)))
                           (ext:compiler-note
                             (lambda (n) (push n (job-notes job)) (muffle-note n)))
                           ((not (or ext:compiler-note serious-condition warning))
                             (lambda (c) (push c (job-other-conditions job)))))
                        (apply function job arguments)))
                    (cfp-log "Thread ~a done with job~%"
                             (mp:process-name mp:*current-process*)))
      (cfp-log "Leaving thread ~a~%" (mp:process-name mp:*current-process*)))))

(defgeneric report-job-conditions (job)
  (:method ((job job))
    (mapc #'signal (job-other-conditions job))
    ;; The WARN calls here never actually print warnings - the
    ;; with-compilation-results handlers do, and then muffle the warnings
    ;; (which is why we use WARN and not SIGNAL). Kind of ugly.
    (mapc #'warn (job-warnings job))
    (mapc #'cmp:note (job-notes job))
    (when (job-serious-condition job)
      ;; We use SIGNAL rather than ERROR although the condition is serious.
      ;; This is because the job has already exited and therefore there
      ;; is no way to debug the problem. with-compilation-results will
      ;; still understand that it's an error and report compilation failure.
      ;; It's possible we could save the original backtrace and so on, but
      ;; if you want to debug problems, it would probably be easier to
      ;; use the serial compiler and debug them as they appear.
      (signal (job-serious-condition job)))))

(defun make-thread-pool (function &key arguments (name 'thread-pool)
                                  (nthreads (core:num-logical-processors))
                                  special-bindings)
  (loop with queue = (core:make-queue name)
        with conc-name = (format nil "~(~a~)-" (symbol-name name))
        for thread-num below nthreads
        collect (mp:process-run-function
                 (format nil "~a-~d" conc-name thread-num)
                 (thread-pool-jobber queue function arguments)
                 special-bindings)
          into threads
        finally (return (make-instance 'thread-pool
                          :queue queue :threads threads))))

(defun thread-pool-enqueue (pool job)
  (core:atomic-enqueue (thread-pool-queue pool) job))

(defun thread-pool-quit (pool)
  (loop with queue = (thread-pool-queue pool)
        for thread in (thread-pool-threads pool)
        do (cfp-log "Sending two :quit (why not?) for thread ~a~%"
                    (mp:process-name thread))
           (core:atomic-enqueue queue :quit)
           (core:atomic-enqueue queue :quit)))

(defun thread-pool-join (pool)
  (loop for thread in (thread-pool-threads pool)
        do (mp:process-join thread)
           (cfp-log "Process-join of thread ~a~%" (mp:process-name thread))))

;;;

(defclass ast-job (job)
  ((%form :initarg :form :reader ast-job-form)
   (%ast :initarg :ast :reader ast-job-ast)
   (%output-object :initarg :output-object :accessor ast-job-output-object)
   (%form-index :initarg :form-index :reader ast-job-form-index)
   (%form-counter :initarg :form-counter :reader ast-job-form-counter)
   (%module :accessor ast-job-module)
   (%source-pos-info :initarg :source-pos-info :reader ast-job-source-pos-info)
   (%startup-function-name :accessor ast-job-startup-function-name)
   (%form-output-path :initarg :form-output-path :reader ast-job-form-output-path)))

(defmethod report-job-conditions :around ((job ast-job))
  (let ((*default-condition-origin*
          (ignore-errors
           (loop for origin = (cleavir-ast:origin (ast-job-ast job))
                   then (cst:source origin)
                 while (typep origin 'cst:cst)
                 finally (return origin)))))
    (call-next-method)))

;;;

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
                            ((or (member :linux *features*)
                                 (member :freebsd *features*))
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
                              ((or (member :linux *features*) (member :freebsd *features*))
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
        (core:*current-source-pos-info* (ast-job-source-pos-info job)))
    (with-module (:module module
                  :optimize (when optimize #'optimize-module-for-compile-file)
                  :optimize-level optimize-level)
      (with-debug-info-generator (:module module
                                  :pathname *compile-file-source-debug-pathname*)
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

(defun read-one-ast (source-sin environment eof-value)
  ;; Required to update the source pos info. FIXME!?
  (peek-char t source-sin nil)
  ;; FIXME: if :environment is provided,
  ;; we should probably use a different read somehow
  (let* ((current-source-pos-info (compile-file-source-pos-info source-sin))
         (core:*current-source-pos-info* current-source-pos-info)
         ;; since cst-read returns a cst normally, we can use eof = nil.
         ;; ...except that eclector.cst:read of "#+(or) 4 #.(or)" returns nil.
         ;; Not sure if bug.
         (cst (eclector.concrete-syntax-tree:read source-sin nil eof-value))
         (_ (when (eq cst eof-value)
              (return-from read-one-ast (values nil nil nil))))
         (form (cst:raw cst))
         (pre-ast
           (if *debug-compile-file*
               (with-compiler-timer ()
                 (clasp-cleavir-translate-bir::cst->ast cst environment))
               (clasp-cleavir-translate-bir::cst->ast cst environment))))
    (declare (ignore _))
    (when *compile-print* (describe-form form))
    (values (clasp-cleavir-translate-bir::wrap-ast pre-ast)
            form current-source-pos-info)))

(defun ast-job-special-bindings ()
  `((*compile-print* . ',*compile-print*)
    (*compile-file-parallel* . ',*compile-file-parallel*)
    (*default-object-type* . ',*default-object-type*)
    (*compile-verbose* . ',*compile-verbose*)
    (*compile-file-output-pathname* . ',*compile-file-output-pathname*)
    (*package* . ',*package*)
    (*compile-file-pathname* . ',*compile-file-pathname*)
    (*compile-file-truename* . ',*compile-file-truename*)
    (*compile-file-source-debug-pathname*
     . ',*compile-file-source-debug-pathname*)
    (*compile-file-source-debug-offset*
     . ',*compile-file-source-debug-offset*)
    (*compile-file-source-debug-lineno*
     . ',*compile-file-source-debug-lineno*)
    (*compile-file-file-scope* . ',*compile-file-file-scope*)
    #+(or cclasp eclasp)(cleavir-cst-to-ast:*compiler*
                         . ',cleavir-cst-to-ast:*compiler*)
    #+(or cclasp eclasp)(core:*use-cleavir-compiler* . ',core:*use-cleavir-compiler*)
    (*global-function-refs* . ',*global-function-refs*)))

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
  (let* ((output-object
           (cond
             ((eq intermediate-output-type :in-memory-object)
              :simple-vector-byte8)
             ((eq intermediate-output-type :in-memory-module)
              nil)
             (t
              (error "Handle intermediate-output-type ~a" intermediate-output-type))))
         #+(or cclasp eclasp) (cleavir-cst-to-ast:*compiler*
                                'cl:compile-file)
         #+(or cclasp eclasp)(core:*use-cleavir-compiler* t)
         #+(or cclasp eclasp)(eclector.reader:*client* clasp-cleavir::*cst-client*)
         #+(or cclasp eclasp)(eclector.readtable:*readtable* cl:*readtable*)
         ast-jobs
         (_ (cfp-log "Starting the pool of threads~%"))
         (job-args `(:optimize ,optimize :optimize-level ,optimize-level
                     :intermediate-output-type ,intermediate-output-type
                     :write-bitcode ,write-bitcode))
         (pool (make-thread-pool (if compile-from-module
                                     'compile-from-module
                                     'compile-from-ast)
                                 :arguments job-args
                                 :name 'compile-file-parallel
                                 :special-bindings (ast-job-special-bindings)))
         (output-path-name (pathname-name output-path)))
    (declare (ignore _))
    (unwind-protect
         (loop with eof-value = (gensym "EOF")
               for form-counter from 0
               for form-index = (core:next-startup-position)
               for form-output-path = (make-pathname
                                       :name (format nil "~a_~d" output-path-name
                                                     form-counter)
                                       :defaults output-path)
               do (multiple-value-bind (ast form cspi)
                      (read-one-ast source-sin environment eof-value)
                    (when (null ast) (return nil)) ; EOF
                    (let ((ast-job (make-instance 'ast-job
                                     :form form :ast ast
                                     :source-pos-info cspi
                                     :form-output-path form-output-path
                                     :output-object output-object
                                     :form-index form-index
                                     :form-counter form-counter)))
                      (when compile-from-module
                        (let ((module (ast-job-to-module ast-job :optimize optimize :optimize-level optimize-level)))
                          (setf (ast-job-module ast-job) module)))
                      (unless ast-only
                        (push ast-job ast-jobs)
                        (thread-pool-enqueue pool ast-job))
                      #+(or)
                      (compile-from-ast ast-job
                                        :optimize optimize
                                        :optimize-level optimize-level
                                        :intermediate-output-type intermediate-output-type))))
      ;; Send :quit messages to all threads.
      ;; It's important to do this in the unwind-protect cleanup,
      ;; so that if there is a read error we actually clean up the threads.
      (thread-pool-quit pool))
    ;; Now wait for all threads to join
    (thread-pool-join pool)
    (mapc #'report-job-conditions ast-jobs)
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
                    ((some #'job-serious-condition ast-jobs)
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
