#+(or)
(eval-when (:compile-toplevel :execute)
  (setq *echo-repl-read* t))
;;
;; Clasp builder code
;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (core:select-package :core))

(defvar *number-of-jobs* 1)

(defun message (level control-string &rest args)
  (core:fmt t "%e[{:d}m"
            (cond
              ((eq level :err)  31)
              ((eq level :warn) 33)
              ((eq level :emph) 32)
              ((eq level :debug) 36)
              ((eq level :info) 37)
              (t 0)))
  (apply #'core:fmt t control-string args)
  (core:fmt t "%e[0m%n")
  (if (eq level :err)
      (core:exit 1)))

#+(or)
(progn
  (defparameter *log* (open "/tmp/clasp-builder-log.txt" :direction :output :if-exists :supersede))
  (si:fset 'core::mmsg #'(lambda (whole env)
                           (let ((fmt (cadr whole))
                                 (args (cddr whole)))
                             `(progn
                                (core:fmt *log* ,fmt ,@args)
                                (finish-output *log*))))
           t))

;;;#+(or)
(si:fset 'core::mmsg #'(lambda (whole env)
                         nil)
         t)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (mmsg "Starting up%N")
  )


#-(or bclasp cclasp eclasp)
(core:fset 'cmp::with-compiler-timer
           (let ((body (gensym)))
             #+(or)(core:fmt t "body = {}%N" body)
             #'(lambda (whole env)
                 (let ((body (cddr whole)))
                   `(progn
                      ,@body))))
           t)

(defun load-kernel-file (path &key (type core:*clasp-build-mode*) silent)
  (let ((filename (make-pathname :type (if (eq type :faso) "faso" "fasl")
                                 :defaults path)))
    (if (not (or (eq type :bitcode) (eq type :object)
                 (eq type :fasl) (eq type :faso)))
        (message :err "Illegal type {} for load-kernel-file {}" type (namestring path)))
    (if (or (eq type :bitcode)
            (and (or (eq type :object) (eq type :fasl))
                 (not (probe-file filename))))
        (cmp:load-bitcode path :print (not silent))
        (progn
          (if (not silent)
              (message nil "Loading {}" (namestring filename)))
          (load filename :print nil :verbose nil)))
    path))

(defun compile-kernel-file (entry &rest args
                                  &key reload count (output-type core:*clasp-build-mode*) verbose print silent)
  #+dbg-print (message :debug "DBG-PRINT compile-kernel-file: {}" entry)
  (let* ((filename (getf entry :source-path))
         (position (getf entry :position))
         (output-path (getf entry :bitcode-path))
         (cmp::*module-startup-prefix* "kernel")
         (compile-file-arguments (list* filename
                                        :source-debug-pathname filename
                                        :output-file output-path
                                        :output-type output-type
                                        :print print
                                        :verbose verbose
                                        :unique-symbol-prefix (format nil "~a~a" (pathname-name filename) position)
                                        :type :kernel ;; (if reload :kernel nil)
                                        :image-startup-position position
                                        (getf entry :compile-file-options))))
    (cond ((getf entry :out-of-date)
           (unless silent
             (message nil "Compiling [{} of {}] {}%N    to {} - will reload: {}"
                      (getf entry :index) count filename output-path reload))
           #+dbg-print (message :debug "filename = {}" filename)
           (if verbose
               (let ((before-ms (get-internal-run-time))
                     (before-bytes (gctools:bytes-allocated)))
                 (apply #'cmp::compile-file-serial compile-file-arguments)
                 (let ((after-ms (get-internal-run-time))
                       (after-bytes (gctools:bytes-allocated)))
                   (message :info "Compile time run({:.3f} secs) consed({} bytes)"
                            (float (/ (- after-ms before-ms) internal-time-units-per-second))
                            (- after-bytes before-bytes))))
             (apply #'cmp::compile-file-serial compile-file-arguments))
           (if reload
               (let ((reload-file (make-pathname :type "fasl" :defaults output-path)))
                 (load-kernel-file reload-file :silent silent))))
          ((not silent)
           (message nil "Skipping [{} of {}] {}"
                    (getf entry :index) count filename)))
    output-path))

(eval-when (:compile-toplevel :execute)
  (core:fset 'compile-execute-time-value
             #'(lambda (whole env)
                 (let* ((expression (second whole))
                        (result (eval expression)))
                   `',result))
             t))

(defun compile-system-serial (system &key reload (output-type core:*clasp-build-mode*) &allow-other-keys
                                     &aux (count (length system)))
  (message :emph "Compiling system serially...")
  #+dbg-print (message :debug "compile-system files: {}" system)
  (dolist (entry system)
    (compile-kernel-file entry :reload reload :output-type output-type :count count :print t :verbose t)))

(defconstant +pjob-slots+ 7)
(defconstant +done-of+ 0)
(defconstant +pid-of+ 1)
(defconstant +signals-of+ 2)
(defconstant +entries-of+ 3)
(defconstant +child-stdout-of+ 4)
(defconstant +child-stderr-of+ 5)
(defconstant +start-time-of+ 6)

(defun setf-pjob-done (pjob value) (setf-elt pjob +done-of+ value))
(defun setf-pjob-done (pjob value) (setf-elt pjob +done-of+ value))
(defun setf-pjob-pid (pjob value) (setf-elt pjob +pid-of+ value))
(defun setf-pjob-signals (pjob value) (setf-elt pjob +signals-of+ value))
(defun setf-pjob-entries (pjob value) (setf-elt pjob +entries-of+ value))
(defun setf-pjob-start-time (pjob value) (setf-elt pjob +start-time-of+ value))
(defun setf-pjob-child-stdout (pjob value) (setf-elt pjob +child-stdout-of+ value))
(defun setf-pjob-child-stderr (pjob value) (setf-elt pjob +child-stderr-of+ value))

(defun pjob-done (pjob) (elt pjob +done-of+))
(defun pjob-pid (pjob) (elt pjob +pid-of+)) 
(defun pjob-signals (pjob) (elt pjob +signals-of+)) 
(defun pjob-entries (pjob) (elt pjob +entries-of+)) 
(defun pjob-child-stdout (pjob) (elt pjob +child-stdout-of+)) 
(defun pjob-child-stderr (pjob) (elt pjob +child-stderr-of+)) 
(defun pjob-start-time (pjob) (elt pjob +start-time-of+)) 

(defun make-pjob (&key done pid signals entries child-stdout child-stderr start-time)
  (let ((pjob (make-array +pjob-slots+)))
    (setf-pjob-done pjob done)
    (setf-pjob-pid pjob pid)
    (setf-pjob-signals pjob signals)
    (setf-pjob-entries pjob entries)
    (setf-pjob-child-stdout pjob child-stdout)
    (setf-pjob-child-stderr pjob child-stderr)
    (setf-pjob-start-time pjob start-time)
    pjob))

(defvar *before-ms* 0)
(defvar *before-bytes* 0)

                            
(defun wait-for-child-to-exit (jobs)
  (mmsg "About to waitpid sigchld-count: {}%N"(core:sigchld-count))
  (multiple-value-bind (wpid status)
      (core:wait)
    (mmsg "Returned from waitpid with wpid: {} status:{}%N" wpid status)
    (if (not (= wpid 0))
        (progn
          (if (/= status 0)
              (message :warn "wpid -> {}  status -> {}" wpid status))
          (if (core:wifexited status)
              (progn
                (mmsg "A child exited wpid: {}  status: {}%N" wpid status)
                (return-from wait-for-child-to-exit (values wpid status nil))))
          (if (core:wifsignaled status)
              (let ((signal (core:wtermsig status)))
                (mmsg "Child process with pid {} got signal {}%N" wpid signal)
                (message :warn "Child process with pid {} got signal {}" wpid signal)))))
    ;; If we drop through to here the child died for some reason - return and inform the parent
    (values wpid status t)))

(defun compile-system-parallel (system
                                &key reload (output-type core:*clasp-build-mode*)
                                (parallel-jobs *number-of-jobs*) (batch-min 1) (batch-max 1) &allow-other-keys)
  #+dbg-print (message :debug "DBG-PRINT compile-system files: {}" system)
  (mmsg "compile-system-parallel files {}%N" system)
  (message :emph "Compiling system with {:d} parallel jobs..." parallel-jobs)
  (let ((total (length system))
        (job-counter 0)
        (batch-size 0)
        (child-count 0)
        child-died
        (jobs (make-hash-table :test #'eql)))
    (labels ((started-one (entry child-pid)
               (let* ((filename (getf entry :source-path))
                      (output-path (bitcode-pathname filename output-type)))
                 (message nil "Starting {: >3d} of {:d} [pid {:d}] {}"
                          (getf entry :index) total child-pid (namestring filename)))
               (when (ext:getenv "CLASP_PAUSE_FORKED_CHILD")
                 (format t "CLASP_PAUSE_FORKED_CHILD is set - will pause all children until they receive SIGUSR1~%")))
             (started-some (entries child-pid)
               (dolist (entry entries)
                 (started-one entry child-pid)))
             (finished-report-one (entry child-pid)
               (let ((filename (getf entry :source-path)))
                 (message :emph "Finished {: >3d} of {:d} [pid {:d}] {} output follows..."
                          (getf entry :index) total child-pid (namestring filename))))
             (read-fd-into-buffer (fd)
               (mmsg "in read-fd-into-buffer {}%N" fd)
               (let ((buffer (make-array 1024 :element-type 'base-char :adjustable nil))
                     (sout (make-string-output-stream)))
                 (core:lseek fd 0 :seek-set)
                 (block readloop
                   (tagbody
                    top
                      (mmsg "About to read-fd%N")
                      (multiple-value-bind (num-read errno)
                          (core:read-fd fd buffer)
                        (if (= num-read 0)
                            (return-from readloop))
                        (if (< num-read 0)
                            (progn
                              (mmsg "Three was an error reading the stream errno {}%N" errno)
                              (message :warn "There was an error reading the stream errno {}" errno)))
                        (if (> num-read 0)
                            (progn
                              (write-sequence buffer sout :start 0 :end num-read)
                              (mmsg "Wrote <{}>%N" (subseq buffer 0 num-read)))))
                      (go top)))
                 (mmsg "Returning with buffer%N")
                 (core:close-fd fd)
                 (get-output-stream-string sout)))
             (report-stream (fd level)
               (mmsg "In report-stream%N")
               (let* ((buffer (read-fd-into-buffer fd))
                      (sin (make-string-input-stream buffer)))
                 ;; We use DO* instead of DO because the latter uses psetq,
                 ;; which is not defined early.
                 (do* ((line (read-line sin nil nil) (read-line sin nil nil)))
                      ((null line))
                   (message level "  {}" line)
                   (finish-output))))
             (report-child-exited (child-pid child-stdout child-stderr)
               (mmsg "In report-child-exited: {} {}%N" child-stdout child-stderr)
               (report-stream child-stdout :info)
               (report-stream child-stderr :warn))
             (finished-some (child-pid pjob)
               (let* ((entries (pjob-entries pjob))
                      (child-stdout (pjob-child-stdout pjob))
                      (child-stderr (pjob-child-stderr pjob)))
                 (setf-pjob-done pjob t)
                 (dolist (entry entries)
                   (finished-report-one entry child-pid))
                 (report-child-exited child-pid child-stdout child-stderr)))
             (reload-one (entry)
               (let* ((filename (getf entry :source-path))
                      (output-path (bitcode-pathname filename output-type)))
                 (load-kernel-file output-path :silent nil)))
             (reload-some (entries)
               (dolist (entry entries)
                 (reload-one entry)))
             (one-compile-kernel-file (entry)
               ;; Don't reload in the child process - there is no point
               (compile-kernel-file entry :reload nil :output-type output-type :count total :silent t :verbose t))
             (some-compile-kernel-files (entries)
               (dolist (entry entries)
                 (one-compile-kernel-file entry))))
      (gctools:garbage-collect)
      (let (entries wpid status)
        (tagbody
         top
           (setq batch-size (let ((remaining (length system)))
                              (min remaining
                                   batch-max
                                   (max batch-min
                                        (ceiling remaining
                                                 parallel-jobs)))))
           (setq entries (subseq system 0 batch-size))
           (setq system (nthcdr batch-size system))
           (incf job-counter)
           (if (> job-counter parallel-jobs)
               (progn
                 (mmsg "Going into wait-for-child-to-exit%N")
                 (multiple-value-bind (vwpid vstatus vchild-died)
                     (wait-for-child-to-exit jobs)
                   (setq wpid vwpid status vstatus child-died vchild-died))
                 #+(or)
                 (multiple-value-setq (wpid status child-died)
                   (wait-for-child-to-exit jobs))
                 (mmsg "Exited from wait-for-child-to-exit%N")
                 (if (null child-died)
                     (if (and (numberp wpid) (>= wpid 0))
                         (let* ((pjob (gethash wpid jobs))
                                (finished-entries (pjob-entries pjob)))
                           (finished-some wpid pjob)
                           (if (and (numberp status)
                                    (not (zerop status)))
                               (message :err "wait returned for process {:d} status {:d}: exiting compile-system"
                                        wpid status))
                           (if reload (reload-some finished-entries))
                           (let ((after-ms (get-internal-run-time))
                                 (after-bytes (gctools:bytes-allocated)))
                             (message :info "Parent time run({:.3f} secs)"
                                      (float (/ (- after-ms (pjob-start-time pjob)) internal-time-units-per-second))))
                           (decf child-count))
                         (error "wait returned ~d  status ~d~%" wpid status))
                     (if (and (numberp wpid) (>= wpid 0))
                         (progn
                           (finished-some wpid (gethash wpid jobs))
                           (message :err "The child with wpid {} died with status {} - terminating build"
                                    wpid status))
                         (message :err "A child died with wpid {} status {}" wpid status)))))
           (mmsg "child-count: {}%N" child-count)
           (if entries
               (let ((child-stdout (core:mkstemp-fd "clasp-build-stdout"))
                     (child-stderr (core:mkstemp-fd "clasp-build-stderr")))
                 (setq *before-ms* (get-internal-run-time)
                       *before-bytes* (gctools:bytes-allocated))
                 (multiple-value-bind (maybe-error pid-or-error child-stream)
                     (core:fork-redirect child-stdout child-stderr)
                   (if maybe-error
                       (error "Could not fork when trying to build ~a" entries)
                       (let ((pid pid-or-error))
                         (if (= pid 0)
                             (progn
                               (when (ext:getenv "CLASP_PAUSE_FORKED_CHILD")
                                 (gctools:wait-for-user-signal (format nil "Child with pid ~a is waiting for SIGUSR1" (core:getpid))))
                               #+(or)(progn
                                       (message nil "A child started up with pid {} - sleeping for 10 seconds" (core:getpid))
                                       (sleep 10))
                               (llvm-sys:create-lljit-thread-pool)
                               (ext:disable-debugger)
                               (let ((new-sigset (core:make-cxx-object 'core:sigset))
                                     (old-sigset (core:make-cxx-object 'core:sigset)))
                                 (core:sigset-sigaddset new-sigset 'core:signal-sigint)
                                 (core:sigset-sigaddset new-sigset 'core:signal-sigchld)
                                 (multiple-value-bind (fail errno)
                                     (core:sigthreadmask :sig-setmask new-sigset old-sigset)
                                   (some-compile-kernel-files entries)
                                   (core:sigthreadmask :sig-setmask old-sigset nil)
                                   (if fail
                                       (error "sigthreadmask has an error errno = ~a" errno))
                                   (finish-output)
                                   (let ((after-ms (get-internal-run-time))
                                         (after-bytes (gctools:bytes-allocated)))
                                     (message :info "Child time run({:.3f} secs) consed({} bytes)"
                                              (float (/ (- after-ms *before-ms*) internal-time-units-per-second))
                                              (- after-bytes *before-bytes*)))
                                   (sys:c_exit))))
                             (let ((one-pjob (make-pjob
                                              :done nil
                                              :pid pid
                                              :signals nil
                                              :entries entries
                                              :child-stdout child-stdout
                                              :child-stderr child-stderr
                                              :start-time *before-ms*)))
                               (started-some entries pid)
                               (core:hash-table-setf-gethash jobs pid one-pjob)
                               (incf child-count))))))))
           (if (> child-count 0) (go top)))))))

(defun parallel-build-p ()
  (and core:*use-parallel-build* (> *number-of-jobs* 1)))

(defun compile-system (&rest args)
  (apply (if (parallel-build-p)
             'compile-system-parallel
             'compile-system-serial)
         args))

(defun command-line-arguments-as-list ()
  (let ((idx (- (length core:*command-line-arguments*) 1))
        result)
    (tagbody
     top
       (if (>= idx 0)
           (progn
             (setq result (cons (pathname (elt core:*command-line-arguments* idx)) result))
             (setq idx (- idx 1))
             (go top))))
    result))

(defun build-failure (condition)
  (message :warn "%nBuild aborted.%nReceived condition of type: {}%n{}"
           (type-of condition)
           condition)
  (if (parallel-build-p)
      (message :err "About to exit clasp")))

(defun prepare-metadata (system
                         &aux (make-create-file-args (find-symbol "MAKE-CREATE-FILE-ARGS" "CMP")))
  "Call make-create-file-args with each system path and the installed path so that when the
DIFile is actually created the argument list passed to llvm-sys:create-file will have already
been initialized with install path versus the build path of the source code file."
  (mapc #'(lambda (entry &aux (source-path (getf entry :source-path))
                              (install-path (getf entry :install-path)))
            (funcall make-create-file-args source-path (namestring source-path) install-path))
        system))

(defun generate-loader (output-file all-compiled-files)
  (let ((output-file (make-pathname :type "lfasl" :defaults output-file)))
    (with-open-file (fout output-file :direction :output :if-exists :supersede)
      (format fout ";;;; Generated in clasp-builder.lisp by generate-loader - do not edit - these fasls need to be loaded in the given order~%")
      (dolist (one-file all-compiled-files)
        (let* ((name (make-pathname :type "fasl" :defaults one-file))
               (relative-name (enough-namestring name (translate-logical-pathname (make-pathname :host "sys:lib;")))))
          (format fout "(load #P\"sys:lib;~a\")~%" (namestring relative-name)))))))

(defun link-modules (output-file all-modules)
  (format t "link-modules output-file: ~a  all-modules: ~a~%" output-file all-modules)
  (cond ((eq core:*clasp-build-mode* :bitcode)
         (cmp:link-bitcode-modules output-file all-modules))
        ((eq core:*clasp-build-mode* :object)) ; Do nothing - object files are the result
        ((eq core:*clasp-build-mode* :faso)
         ;; Do nothing - faso files are the result
         (core:link-faso-files output-file all-modules nil))
        ((eq core:*clasp-build-mode* :fasoll)
         (cmp::link-fasoll-modules output-file all-modules))
        ((eq core::*clasp-build-mode* :fasobc)
         (cmp::link-fasobc-modules output-file all-modules))
        ((eq core:*clasp-build-mode* :fasl)
         (generate-loader output-file all-modules))
        (t
         (error "Unsupported value for core:*clasp-build-mode* -> ~a" core:*clasp-build-mode*))))

(defun link-fasl (&key (output-file (build-common-lisp-bitcode-pathname))
                       (target-backend (default-target-backend))
                       (system (command-line-arguments-as-list)))
  (cond ((eq core:*clasp-build-mode* :bitcode)
         (cmp:link-bitcode-modules output-file system))
        ((eq core:*clasp-build-mode* :object)) ; Do nothing - object files are the result
        ((eq core:*clasp-build-mode* :faso)
         ;; Do nothing - faso files are the result
         (core:link-faso-files output-file system nil))
        ((eq core:*clasp-build-mode* :fasoll)
         (let* ((module (cmp:link-bitcode-modules-together output-file system :clasp-build-mode :fasoll))
                (fout (open output-file :direction :output :if-exists :supersede)))
           (llvm-sys:dump-module module fout)))
        ((eq core::*clasp-build-mode* :fasobc)
         (let* ((module (cmp:link-bitcode-modules-together output-file system :clasp-build-mode :fasobc)))
           (llvm-sys:write-bitcode-to-file module (namestring output-file))))
        (t
         (error "Unsupported value for core:*clasp-build-mode* -> ~a" core:*clasp-build-mode*))))

(defun construct-system (files reproducible
                         &aux source-path bitcode-path system last item
                              new-last (position 0) (*features* (list* :cclasp *features*)))
  (tagbody
   next
    (when files
      (setq source-path (car files)
            bitcode-path (bitcode-pathname source-path)
            item (list :source-path source-path
                       :position position
                       :bitcode-path bitcode-path)
            position (+ 1 position)
            files (cdr files))
      (when reproducible
        (setq item (list* :install-path (car files) item)
              files (cdr files)))
      (cond (last
             (setq new-last (list item))
             (rplacd last new-last)
             (setq last new-last))
            (t
             (setq last (list item)
                   system last)))
     (go next)))
  system)

(defun pprint-features (added-features removed-features)
  (when removed-features
    (message :info "Removed features {}" removed-features))
  (when added-features
    (message :info "Added features {}" added-features)))

(defvar +stage-features+ '(:clasp-min :clos :aclasp :bclasp :cclasp :eclasp))
(defvar +load-weight+ 0.5d0)

(defun load-stage (system stage extension load-verbose)
  (let ((start (make-pathname :host "sys"
                              :directory (list :absolute "src" "lisp"
                                               "kernel" "stage"                                                             
                                               (if extension "extension" "base"))
                              :name (core:fmt nil "{:d}-begin" stage)
                              :type "lisp"))
        (end (make-pathname :host "sys"
                            :directory (list :absolute "src" "lisp"
                                             "kernel" "stage"
                                             (if extension "extension" "base"))
                            :name (core:fmt nil "{:d}-end" stage)
                            :type "lisp"))
        (stage-keyword (intern (core:fmt nil "STAGE{:d}" stage) :keyword))
        (stage-time (get-internal-run-time))
        file-time entry file prev-file-time bytes)
    (setq *features* (cons stage-keyword *features*))
    (message :emph "Loading stage {:d}..." stage)
    (setq system (member start system
                         :test #'(lambda (path entry)
                                   (equal path (getf entry :source-path)))))
    (tagbody
     next
      (if system
          (progn
            (setq entry (car system)
                  file (getf entry :source-path)
                  file-time (get-internal-run-time)
                  bytes (gctools:bytes-allocated))
            (if (not load-verbose)
                (message nil ";;; Loading {}" file))
            (let ((*load-verbose* load-verbose))
              (load file))
            (setq file-time (- (get-internal-run-time) file-time)
                  prev-file-time (getf entry :load-time))
            (rplaca system
                    (sys:put-f entry
                               (if prev-file-time
                                   (+ (* +load-weight+ file-time)
                                      (* (- 1 +load-weight+) prev-file-time))
                                   file-time)
                               :load-time))
            (if (and load-verbose (>= file-time internal-time-units-per-second))
                (message nil ";;; Load time({:.1f} seconds) consed({} bytes)"
                         (float (/ file-time internal-time-units-per-second))
                         (- (gctools:bytes-allocated) bytes)))
            (if (not (equal end file))
                (progn
                  (setq system (cdr system))
                  (go next))))))
    (setq *features* (core:remove-equal stage-keyword *features*))
    (message :emph "Stage {:d} time({:.1f} seconds)"
             stage
             (float (/ (- (get-internal-run-time) stage-time) internal-time-units-per-second)))))

(defun stage-features (&rest new-features &aux added-features removed-features)
  (dolist (feature +stage-features+)
    (when (and (not (member feature new-features))
               (member feature *features*))
      (setq *features* (core:remove-equal feature *features*)
            removed-features (cons feature removed-features))))
  (dolist (feature new-features)
    (unless (member feature *features*)
      (setq *features* (cons feature *features*)
            added-features (cons feature added-features))))
  (pprint-features added-features removed-features))

(defun load-clasp (&key (bytecode t)
                        (clean (ext:getenv "CLASP_CLEAN"))
                        extension
                        (load-verbose (ext:getenv "CLASP_LOAD_VERBOSE"))
                        (output-file (build-common-lisp-bitcode-pathname))
                        reproducible
                        (system-sort (ext:getenv "CLASP_SYSTEM_SORT"))
                        (stage-count (if (ext:getenv "CLASP_STAGE_COUNT")
                                         (parse-integer (ext:getenv "CLASP_STAGE_COUNT"))))
                        (system (command-line-arguments-as-list)))
  (unless stage-count
    (setq stage-count (if extension 1 3)))
  (setq *features*
        (if bytecode
            (list* :bytecode :staging :bytecodelike *features*)
            (list* :mclasp :staging *features*)))
  (let ((*target-backend* (default-target-backend (if extension "e" "c")))
        (write-date 0))
    (setq system (construct-system system reproducible))
    (dotimes (stage stage-count)
      (load-stage system stage extension load-verbose))
    (setq *features* (core:remove-equal :staging *features*))
    (when reproducible
      (prepare-metadata system))
    (setq system (mapcar #'(lambda (entry &aux (bitcode-path (getf entry :bitcode-path))
                                          (source-path (getf entry :source-path)))
                             (setq write-date (max write-date (file-write-date source-path)))
                             (list* :out-of-date (or clean
                                                     (not (probe-file bitcode-path))
                                                     (< (file-write-date bitcode-path)
                                                        write-date))
                                    entry))
                         system))
    (if system-sort
        (sort system #'> :key #'(lambda (entry) (getf entry :load-time 0)))
        system)))

(defun compile-clasp (system &aux (index 0))
  ;; Inline ASTs refer to various classes etc that are not available while earlier files are loaded.
  ;; Therefore we can't have the compiler save inline definitions for files earlier than we're able
  ;; to load inline definitions. We wait for the source code to turn it back on.
  (setq core:*defun-inline-hook* nil
        system (mapcan #'(lambda (entry)
                           (when (getf entry :out-of-date)
                             (incf index)
                             (list (list* :index index entry))))
                       system))
  #-clasp-min
  (handler-bind
      ((error #'build-failure))
    (compile-system system))
  #+clasp-min
  (compile-system system))

(defun load-and-compile-clasp (&rest rest)
  (compile-clasp (apply #'load-clasp rest)))

(export '(command-line-arguments-as-list
          compile-kernel-file
          compile-system
          compile-system-parallel
          compile-system-serial
          compile-clasp
          link-fasl
          load-and-compile-clasp
          load-clasp
          pprint-features
          stage-features))
