
(in-package :cmp)

#+(or)
(defmacro cf2-log (fmt &rest args)
  `(format *debug-io* ,fmt ,@args))
(defmacro cf2-log (fmt &rest args)
  nil)

#+(or)
(defun link-modules-parallel (output-pathname modules
                     &key additional-bitcode-pathnames
                     &aux conditions)
  "Link a bunch of modules together, return the linked module"
  (with-compiler-env (conditions)
    (let* ((module (llvm-create-module (pathname-name output-pathname)))
           (*compile-file-pathname* (pathname (merge-pathnames output-pathname)))
           (*compile-file-truename* (translate-logical-pathname *compile-file-pathname*))
           (bcnum 0))
      (with-module ( :module module
                     :optimize nil)
        (with-source-pathnames (:source-pathname (pathname output-pathname))
          (with-debug-info-generator (:module module :pathname output-pathname)
            (let* ((linker (llvm-sys:make-linker *the-module*))
                   (part-index 1))
              ;; Don't enforce .bc extension for additional-bitcode-pathnames
              ;; This is where I used to link the additional-bitcode-pathnames
              (dolist (part-module modules)
                (incf part-index)
                (multiple-value-bind (failure error-msg)
                    (let ((global-ctor (find-global-ctor-function part-module))
                          (priority part-index))
                      (remove-llvm.global_ctors-if-exists part-module)
                      (add-llvm.global_ctors part-module priority global-ctor)
                      (llvm-sys:link-in-module linker part-module))
                  (when failure
                    (error "While linking part module ~a" part-module error-msg))))
              ;; The following links in additional-bitcode-pathnames
              (dolist (part-pn additional-bitcode-pathnames)
                (let* ((bc-file part-pn))
;;;                (bformat t "Linking %s%N" bc-file)
                  (let* ((part-module (llvm-sys:parse-bitcode-file (namestring (truename bc-file)) *llvm-context*)))
                    ;;                  (remove-main-function-if-exists part-module) ;; Remove the ClaspMain FN if it exists
                    (multiple-value-bind (failure error-msg)
                        (llvm-sys:link-in-module linker part-module)
                      (when failure
                        (error "While linking additional module: ~a  encountered error: ~a" bc-file error-msg))))))
              (write-bitcode *the-module* (core:coerce-to-filename (pathname (if output-pathname
                                                                                 output-pathname
                                                                                 (error "The output pathname is NIL")))))
              *the-module*)))))))

(defstruct (ast-job (:type vector) :named) ast environment form-output-path form-index)

(defun compile-from-ast (job &key
                               optimize
                               optimize-level
                               intermediate-output-type)
  (restart-case
      (let ((module (cmp::llvm-create-module (namestring (ast-job-form-output-path job)))))
        (with-module (:module module
                      :optimize (when optimize #'optimize-module-for-compile-file)
                      :optimize-level optimize-level)
          (with-debug-info-generator (:module module
                                      :pathname *compile-file-truename*)
            (with-make-new-run-all (run-all-function (core:bformat nil "-top-%d" (core:next-number)))
              (with-literal-table
                  (let ((clasp-cleavir::*llvm-metadata* (make-hash-table :test 'eql)))
                    (core:with-memory-ramp (:pattern 'gctools:ramp)
                      (literal:with-top-level-form
                          (let ((hoisted-ast (clasp-cleavir::hoist-ast (ast-job-ast job))))
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
    (abort ())))

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
        (form-index 0)
        (eof-value (gensym))
        #+cclasp(eclector.reader:*client* clasp-cleavir::*cst-client*)
        #+cclasp(read-function 'eclector.concrete-syntax-tree:cst-read)
        ast-jobs ast-threads)
    (loop
      ;; Required to update the source pos info. FIXME!?
      (peek-char t source-sin nil)
      ;; FIXME: if :environment is provided we should probably use a different read somehow
      (let* ((core:*current-source-pos-info* (core:input-stream-source-pos-info source-sin))
             (form-output-path (make-pathname :name (format nil "~a_~d" (pathname-name output-path) form-index ) :defaults working-dir))
             #+cclasp(cleavir-generate-ast:*compiler* 'cl:compile-file)
             #+cclasp(core:*use-cleavir-compiler* t)
             #+cst
             (cst (eclector.concrete-syntax-tree:cst-read source-sin nil eof-value))
             #+cst
             (form (cst:raw cst))
             #+cst
             (at-eof (eq cst eof-value))
             #+cst
             (ast (if cmp::*debug-compile-file*
                      (clasp-cleavir::compiler-time (clasp-cleavir::cst->ast cst))))
             #-cst
             (form (read source-sin nil eof-value))
             #-cst
             (at-eof (eq form eof-value))             
             #-cst
             (ast (if cmp::*debug-compile-file*
                      (clasp-cleavir::compiler-time (clasp-cleavir::generate-ast form))
                      (clasp-cleavir::generate-ast form))))
        (when at-eof (return nil))
        (push form-output-path result)
        (let ((ast-job (make-ast-job :ast ast
                                     :environment environment
                                     :form-output-path form-output-path
                                     :form-index form-index)))
          (when *compile-print* (cmp::describe-form form))
          (unless ast-only
            (push ast-job ast-jobs)
            #+(or)
            (progn
              (push (mp:process-run-function
                     (core:bformat nil "compile-file-%d" form-index)
                     (lambda ()
                       (progn
                         (compile-from-ast ast-job
                                           :optimize optimize
                                           :optimize-level optimize-level
                                           :intermediate-output-type intermediate-output-type)))
                     `((*compile-print* . ',*compile-print*)
                       (*compile-verbose* . ',*compile-verbose*)
                       (*compile-file-output-pathname* . ',*compile-file-output-pathname*)
                       (*package* . ',*package*)
                       (*compile-file-pathname* . ',*compile-file-pathname*)
                       (*compile-file-truename* . ',*compile-file-truename*)
                       (*source-debug-pathname* . ',*source-debug-pathname*)
                       (*source-debug-offset* . ',*source-debug-offset*)
                       (core:*current-source-pos-info* . ',core:*current-source-pos-info*)
                       (cleavir-generate-ast:*compiler* . ',cleavir-generate-ast:*compiler*)
                       (core:*use-cleavir-compiler* . ',core:*use-cleavir-compiler*)
                       (cmp::*global-function-refs* . ',cmp::*global-function-refs*)
                       ))
                    ast-threads)
              (mp:process-join (pop ast-threads)))
;;;            #+(or)
            (compile-from-ast ast-job
                              :optimize optimize
                              :optimize-level optimize-level
                              :intermediate-output-type intermediate-output-type))
          (incf form-index))))
    ;; Now wait for all threads to join
    #+(or)
    (loop for thread in ast-threads
          do (mp:process-join thread))
    ;; Now return the results
    (values (nreverse result))))


(defun compile-file-to-result (given-input-pathname
                               &key
                                 compile-file-hook
                                 type
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
         
         (source-sin (open given-input-pathname :direction :input))
         warnings-p failure-p)
    (with-open-stream (sin source-sin)
      ;; If a truename is provided then spoof the file-system to treat input-pathname
      ;; as source-truename with the given offset
      (when source-debug-pathname
        (core:source-file-info (namestring given-input-pathname) source-debug-pathname source-debug-offset nil))
      (when *compile-verbose*
        (bformat t "; Compiling file: %s%N" (namestring given-input-pathname)))
      (cmp-log "About to start with-compilation-unit%N")
      (with-compilation-unit ()
        (let* ((*compile-file-pathname* (pathname (merge-pathnames given-input-pathname)))
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
                            :verbose verbose))))))))



(defun compile-file-parallel (input-file
                              &key
                                (output-file nil output-file-p)
                                (verbose *compile-verbose*)
                                (print *compile-print*)
                                (optimize t)
                                (optimize-level *optimization-level*)
                                (system-p nil system-p-p)
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
                                ;; ignored by bclasp
                                ;; but passed to hook functions
                                environment
                                ;; Use as little llvm as possible for timing
                                dry-run ast-only
                              &aux conditions)
  "See CLHS compile-file."
  (if system-p-p (error "I don't support system-p keyword argument - use output-type"))
  (if (not output-file-p) (setq output-file (cfp-output-file-default input-file output-type)))
  (with-compiler-env (conditions)
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
      (with-compiler-timer (:message "Compile-file" :report-link-time t :verbose verbose)
        (let ((result (compile-file-to-result input-pathname
                                              :type type
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
          (loop for one in result do (format t "Result: ~s~%" one))
          (cond
            (dry-run
             (format t "Doing nothing further~%"))
            ((null output-path)
             (error "The output-file is nil for input filename ~a~%" input-file))
            #+(or)((eq output-type :object)
                   (error "Implement me")
                   (when verbose (bformat t "Writing object to %s%N" (core:coerce-to-filename output-path)))
                   (ensure-directories-exist output-path)
                   ;; Save the bitcode so we can take a look at it
                   (with-track-llvm-time
                       (write-bitcode module (core:coerce-to-filename (cfp-output-file-default output-path :bitcode))))
                   (with-open-file (fout output-path :direction :output)
                     (let ((reloc-model (cond
                                          ((or (member :target-os-linux *features*) (member :target-os-freebsd *features*))
                                           'llvm-sys:reloc-model-pic-)
                                          (t 'llvm-sys:reloc-model-undefined))))
                       (unless dry-run (generate-obj-asm module fout :file-type 'llvm-sys:code-gen-file-type-object-file :reloc-model reloc-model)))))
            #+(or)
            ((eq output-type :bitcode)
             (cf2-log "output-type :bitcode  result -> ~s~%" result)
             (link-modules output-path result))
            ((eq output-type :fasl)
             (ensure-directories-exist output-path)
             (let ((output-path (make-pathname :type (bitcode-extension) :defaults output-path)))
               (llvm-link output-path :input-files (mapcar (lambda (name)
                                                             (make-pathname :type (bitcode-extension) :defaults name))
                                                           result)
                                      :link-type :bitcode))
             (llvm-link output-path :input-files (mapcar (lambda (name)
                                                           (make-pathname :type "o" :defaults name))
                                                         result)
                                    :input-type :object))
            ((eq output-type :object)
             (ensure-directories-exist output-path)
             (let ((output-path (make-pathname :type (bitcode-extension) :defaults output-path)))
               (llvm-link output-path :input-files (mapcar (lambda (name)
                                                             (make-pathname :type (bitcode-extension) :defaults name))
                                                           result)
                                      :link-type :bitcode))
             (let ((output-path (make-pathname :type "o" :defaults output-path)))
               (llvm-link output-path :input-files (mapcar (lambda (name)
                                                             (make-pathname :type "o" :defaults name))
                                                           result)
                                      :link-type :object)))
            (t ;; fasl
             (error "Add support for output-type: ~a" output-type)))          
          (dolist (c conditions)
            (when verbose
              (bformat t "conditions: %s%N" c)))
          (compile-file-results output-path conditions))))))
