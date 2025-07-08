
(in-package :cmp)

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
                 do (block nil
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
                        (apply function job arguments)))))))

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
        do (core:atomic-enqueue queue :quit)
           (core:atomic-enqueue queue :quit)))

(defun thread-pool-join (pool)
  (loop for thread in (thread-pool-threads pool)
        do (mp:process-join thread)))

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

(defun compile-from-module (job
                            &key optimize
                                 optimize-level
                                 intermediate-output-type)
  (declare (ignore optimize optimize-level))
  (let ((module (ast-job-module job)))
    (ecase intermediate-output-type
      (:in-memory-object
       (let ((output (generate-obj-asm-stream
                      module (ast-job-output-object job)
                      'llvm-sys:code-gen-file-type-object-file *default-reloc-model*)))
         (when output (setf (ast-job-output-object job) output))))
      (:in-memory-module
       (let ((llvm-ir (with-output-to-string (sout)
                        (llvm-sys:dump-module module sout))))
       (setf (ast-job-output-object job) llvm-ir))))
    (gctools:thread-local-cleanup))
  (values))

(defun ast-job-to-module (job &key optimize optimize-level)
  (let ((module (llvm-create-module (format nil "module~a" (ast-job-form-index job))))
        (core:*current-source-pos-info* (ast-job-source-pos-info job)))
    (with-module (:module module
                  :optimize (when optimize #'llvm-sys:optimize-module)
                  :optimize-level optimize-level)
      (with-debug-info-generator (:module module
                                  :pathname *compile-file-source-debug-pathname*)
        (with-make-new-run-all (run-all-function (format nil "module~a" (ast-job-form-index job)))
          (literal:with-literal-table (:id (ast-job-form-index job))
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
      (irc-verify-module-safe module)
      (quick-module-dump module (format nil "preoptimize~a" (ast-job-form-index job)))
      ;; ALWAYS link the builtins in, inline them and then remove them.
      #+(or)(link-inline-remove-builtins module)
      module)))

(defun compile-from-ast (job &key
                               optimize
                               optimize-level
                               intermediate-output-type)
  (setf (ast-job-module job)
        (ast-job-to-module job :optimize optimize
                               :optimize-level optimize-level))
  (compile-from-module job :optimize optimize :optimize-level optimize-level
                           :intermediate-output-type intermediate-output-type))

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
    (*default-output-type* . ',*default-output-type*)
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
    (*global-function-refs* . ',*global-function-refs*)))

(defun cclasp-loop2 (source-sin
                     environment
                     &key
                       (compile-from-module nil) ; If nil - then compile from the ast in threads
                       optimize
                       optimize-level
                       output-path
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
         #+(or cclasp eclasp)(eclector.reader:*client* cmp:*cst-client*)
         ast-jobs
         (job-args `(:optimize ,optimize :optimize-level ,optimize-level
                     :intermediate-output-type ,intermediate-output-type))
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


(defun compile-stream-to-result (input-stream
                                 &key
                                   output-type
                                   output-path
                                   environment
                                   (optimize t)
                                   (optimize-level *optimization-level*)
                                   ast-only)
  "* Arguments
- given-input-pathname :: A pathname.
- output-path :: A pathname.
- environment :: Arbitrary, passed only to hook
Compile a lisp source file into an LLVM module."
  (cclasp-loop2 input-stream environment
                :optimize optimize
                :optimize-level optimize-level
                :output-path output-path
                :intermediate-output-type (ecase output-type
                                            (:faso :in-memory-object)
                                            (:fasoll :in-memory-module)
                                            (:fasobc :in-memory-module)
                                            (:bytecode :in-memory-module))
                :ast-only ast-only))

(defun link-compile-file-parallel-modules (output-pathname parts)
  "Link a bunch of modules together, return the linked module"
  (let* ((link-module (llvm-create-module (pathname-name output-pathname))))
    ;; Don't enforce .bc extension for additional-bitcode-pathnames
    ;; This is where I used to link the additional-bitcode-pathnames
    (loop for part-llvm-ir in parts
          for module = (llvm-sys:parse-irstring part-llvm-ir (thread-local-llvm-context) "")
          do (multiple-value-bind (failure error-msg)
                 (llvm-sys:link-modules link-module module)
               (when failure
                 (format t "While linking part module encountered error: ~a~%" error-msg))))
    link-module))

(defun output-cfp-result (ast-jobs output-path output-type)
  (ensure-directories-exist output-path)
  (case output-type
    (:faso
     (core:write-faso output-path
                      (mapcar #'ast-job-output-object
                              (sort (copy-list ast-jobs) #'<
                                    :key #'ast-job-form-index))))
    (:fasoll
     (with-open-file (fout output-path :direction :output :if-exists :supersede)
       (llvm-sys:dump-module (link-compile-file-parallel-modules
                              (namestring output-path)
                              (mapcar #'ast-job-output-object ast-jobs))
                             fout)))
    (:fasobc
     (llvm-sys:write-bitcode-to-file
      (link-compile-file-parallel-modules (namestring output-path)
                                          (mapcar #'ast-job-output-object ast-jobs))
      (namestring (translate-logical-pathname output-path))))
    (otherwise ;; unknown
     (error "Add support for output-type: ~a" output-type))))

(defun compile-stream/parallel (input-stream output-path
                                &key (optimize t)
                                     (optimize-level *optimization-level*)
                                     (output-type *default-output-type*)
                                     environment
                                     ;; Use as little llvm as possible for timing
                                     dry-run ast-only
                                &allow-other-keys)
  (with-compiler-timer (:message "Compile-file-parallel" :report-link-time t :verbose *compile-verbose*)
    (let ((ast-jobs
            (compile-stream-to-result
             input-stream
             :output-type output-type
             :output-path output-path
             :environment environment
             :optimize optimize
             :optimize-level optimize-level
             :ast-only ast-only)))
      (cond (dry-run (format t "Doing nothing further~%") nil)
            ((some #'job-serious-condition ast-jobs)
             ;; There was an insurmountable error - fail.
             nil)
            ;; Usual result
            (t (output-cfp-result ast-jobs output-path output-type)
               (truename output-path))))))

(eval-when (:load-toplevel)
  (setf *compile-file-parallel* *use-compile-file-parallel*))
