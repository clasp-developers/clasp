#+(or)
(eval-when (:compile-toplevel :execute)
  (setq *echo-repl-read* t))
;;
;; Clasp builder code
;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (core:select-package :core))

(defvar *number-of-jobs* 1)

(defun ansi-control (&optional level)
  (core:fmt t "%e[{:d}m"
            (cond ((eq level :err)  31)
                  ((eq level :warn) 33)
                  ((eq level :emph) 32)
                  ((eq level :debug) 36)
                  ((eq level :info) 37)
                  (t 0))))

(defun message (level control-string &rest args)
  (ansi-control level)
  (apply #'core:fmt t control-string args)
  (ansi-control)
  (terpri)
  (when (eq level :err)
    (core:exit 1)))

(defun message-fd (level fd)
  (let ((buffer (make-array 1024 :element-type 'base-char :adjustable nil)))
    (ansi-control level)
    (core:lseek fd 0 :seek-set)
    (tagbody
     top
      (multiple-value-bind (num-read errno)
          (core:read-fd fd buffer)
        (when (> num-read 0)
          (write-sequence buffer t :start 0 :end num-read)
          (go top))))
    (core:close-fd fd)
    (ansi-control)))

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
(Si:fset 'core::mmsg #'(lambda (whole env)
                         nil)
         t)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (mmsg "Starting up%N"))

#+clasp-min
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
    (unless (or (eq type :bitcode) (eq type :object)
                (eq type :fasl) (eq type :faso))
      (message :err "Illegal type {} for load-kernel-file {}" type (namestring path)))
    (cond ((or (eq type :bitcode)
               (and (or (eq type :object) (eq type :fasl))
                    (not (probe-file filename))))
           (cmp:load-bitcode path :print (not silent)))
          (t
           (unless silent
             (message nil "Loading {}" (namestring filename)))
           (load filename :print nil :verbose nil)))
    path))

(defun compile-kernel-file (entry &rest args
                                  &key reload count (output-type core:*clasp-build-mode*) verbose print silent)
  (let* ((filename (getf entry :source-path))
         (position (getf entry :position))
         (output-path (getf entry :output-path))
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
    (unless silent
      (message nil "Compiling [{} of {}] {}%N    to {} - will reload: {}"
               (getf entry :index) count filename output-path reload))
    (if verbose
        (let ((before-ms (get-internal-run-time))
              (before-bytes (gctools:bytes-allocated)))
          (apply #'compile-file compile-file-arguments)
          (let ((after-ms (get-internal-run-time))
                (after-bytes (gctools:bytes-allocated)))
            (message :info "Compile time run({:.3f} secs) consed({} bytes)"
                     (float (/ (- after-ms before-ms) internal-time-units-per-second))
                     (- after-bytes before-bytes))))
        (apply #'compile-file compile-file-arguments))
    (when reload
      (load-kernel-file (make-pathname :type "fasl" :defaults output-path) :silent silent))
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
  (dolist (entry system)
    (compile-kernel-file entry :reload reload :output-type output-type :count count :print t :verbose t)))

(defun compile-system-parallel (system
                                &key reload (output-type core:*clasp-build-mode*)
                                     (parallel-jobs *number-of-jobs*)
                                &allow-other-keys)
  (message :emph "Compiling system with {:d} parallel jobs..." parallel-jobs)
  (let ((count (length system))
        (child-count 0)
        (jobs (make-hash-table :test #'eql))
        entry)
    (labels ((started-one (entry)
               (message nil "Starting {: >3d} of {:d} [pid {:d}] {}"
                        (getf entry :index) count (getf entry :pid) (namestring (getf entry :source-path)))
               (when (ext:getenv "CLASP_PAUSE_FORKED_CHILD")
                 (message :emph "CLASP_PAUSE_FORKED_CHILD is set - will pause all children until they receive SIGUSR1")))
             (finished-one (entry)
               (when entry
                 (message :emph "Finished {: >3d} of {:d} [pid {:d}] {} output follows..."
                          (getf entry :index) count (getf entry :pid) (namestring (getf entry :source-path)))
                 (message-fd :info (getf entry :child-stdout))
                 (message-fd :warn (getf entry :child-stderr)))))
      (gctools:garbage-collect)
      (tagbody
       top
        (when (or (and system (= child-count parallel-jobs))
                  (and (null system) (> child-count 0)))
          (multiple-value-bind (wpid status)
              (core:wait)
            (when (= -1 wpid)                 
              (message :err "No children left to wait on."))
            (unless (or (core:wifsignaled status)
                        (core:wifexited status))
              (go top))
            (let ((entry (gethash wpid jobs)))
              (finished-one entry)
              (when (core:wifsignaled status)
                (message :err "The child with wpid {} was terminated with signal {}."
                         wpid (core:wtermsig status)))
              (unless (zerop (core:wexitstatus status))
                (message :err "The child with wpid {} exited with status {}."
                         wpid (core:wexitstatus status)))
              (when reload
                (load-kernel-file (getf entry :output-path) :silent nil))
              (message :info "Parent time run({:.3f} secs)"
                       (float (/ (- (get-internal-run-time) (getf entry :start-time))
                                 internal-time-units-per-second)))
              (decf child-count))))
        (when system
          (setq entry (car system)
                system (cdr system))
          (let ((child-stdout (core:mkstemp-fd "clasp-build-stdout"))
                (child-stderr (core:mkstemp-fd "clasp-build-stderr")))
            (multiple-value-bind (maybe-error pid child-stream)
                (core:fork-redirect child-stdout child-stderr)
              (when maybe-error
                (message :err "Could not fork when trying to build {}" entry))
              (cond ((zerop pid)
                     (when (ext:getenv "CLASP_PAUSE_FORKED_CHILD")
                       (gctools:wait-for-user-signal (core:fmt nil "Child with pid {} is waiting for SIGUSR1"
                                                               (core:getpid))))
                     ;;;(llvm-sys:create-lljit-thread-pool) ;;; Done by fork
                     (ext:disable-debugger)
                     (let ((new-sigset (core:make-cxx-object 'core:sigset))
                           (old-sigset (core:make-cxx-object 'core:sigset))
                           (start-time (get-internal-run-time))
                           (start-bytes (gctools:bytes-allocated)))
                       (core:sigset-sigaddset new-sigset 'core:signal-sigint)
                       (core:sigset-sigaddset new-sigset 'core:signal-sigchld)
                       (multiple-value-bind (fail errno)
                           (core:sigthreadmask :sig-setmask new-sigset old-sigset)
                         (compile-kernel-file entry :output-type output-type :silent t :verbose t)
                         (core:sigthreadmask :sig-setmask old-sigset nil)
                         (when fail
                           (message :err "sigthreadmask has an error errno = {}" errno))
                         (message :info "Child time run({:.3f} secs) consed({} bytes)"
                                  (float (/ (- (get-internal-run-time) start-time)
                                            internal-time-units-per-second))
                                  (- (gctools:bytes-allocated) start-bytes))
                         (sys:c_exit))))
                    (t
                     (started-one (core:hash-table-setf-gethash jobs pid
                                                                (list* :pid pid
                                                                       :child-stdout child-stdout
                                                                       :child-stderr child-stderr
                                                                       :start-time (get-internal-run-time)
                                                                       entry)))
                     (incf child-count))))))
         (when (or system (> child-count 0))
           (go top))))))

(defun parallel-build-p ()
  (and core:*use-parallel-build* (> *number-of-jobs* 1)))

(defun compile-system (&rest args)
  (apply (if (parallel-build-p)
             'compile-system-parallel
             'compile-system-serial)
         args))

(defun build-failure (condition)
  (message :warn "%nBuild aborted.%nReceived condition of type: {}%n{}"
           (type-of condition)
           condition)
  (when (parallel-build-p)
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
        ((eq core:*clasp-build-mode* :bytecode)
         (funcall (find-symbol "LINK-BYTECODE-FASL-FILES" "CMPLTV") output-file all-modules))
        ((eq core:*clasp-build-mode* :object)) ; Do nothing - object files are the result
        ((eq core:*clasp-build-mode* :faso)
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
                       (system (command-line-paths)))
  (cond ((eq core:*clasp-build-mode* :bitcode)
         (cmp:link-bitcode-modules output-file system))
        ((eq core:*clasp-build-mode* :bytecode)
         (funcall (find-symbol "LINK-BYTECODE-FASL-FILES" "CMPLTV") output-file system))
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

(defun construct-system (files position reproducible
                         &aux source-path output-path system last item new-last)
  (tagbody
   next
    (when files
      (setq source-path (car files)
            output-path (bitcode-pathname source-path)
            item (list :source-path source-path
                       :position position
                       :output-path output-path)
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

(defun load-stage (system stage name load-verbose)
  (let ((start (make-pathname :host "sys"
                              :directory (list :absolute "src" "lisp"
                                               "kernel" "stage" name)                                                          
                              :name (core:fmt nil "{:d}-begin" stage)
                              :type "lisp"))
        (end (make-pathname :host "sys"
                            :directory (list :absolute "src" "lisp"
                                             "kernel" "stage" name)
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
      (when system
        (setq entry (car system)
              file (getf entry :source-path)
              file-time (get-internal-run-time)
              bytes (gctools:bytes-allocated))
        (unless load-verbose
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
        (when (and load-verbose (>= file-time internal-time-units-per-second))
          (message nil ";;; Load time({:.1f} seconds) consed({} bytes)"
                   (float (/ file-time internal-time-units-per-second))
                   (- (gctools:bytes-allocated) bytes)))
        (unless (equal end file)
          (setq system (cdr system))
          (go next))))
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

(defun stage-count (system name
                    &aux (last-stage -1) source-path
                         (stage-directory (list :absolute "SRC" "LISP" "KERNEL" "STAGE" name))) 
  (dolist (entry system (1+ last-stage))
    (setq source-path (getf entry :source-path))
    (when (equalp (pathname-directory source-path) stage-directory)
      (setq last-stage (max last-stage (parse-integer (pathname-name source-path) :junk-allowed t))))))

(defun load-clasp (&key (clean (ext:getenv "CLASP_CLEAN"))
                        (load-verbose (ext:getenv "CLASP_LOAD_VERBOSE"))
                        (name "base")
                        (position 0)
                        reproducible
                        (system-sort (ext:getenv "CLASP_SYSTEM_SORT"))
                        (stage-count (when (ext:getenv "CLASP_STAGE_COUNT")
                                       (parse-integer (ext:getenv "CLASP_STAGE_COUNT"))))
                        (system (command-line-paths)))
  (setq *features* (list* :staging *features*)
        system (construct-system system position reproducible))
  (let ((write-date 0)
        (max-stage-count (stage-count system name)))
    (cond ((null stage-count)
           (setq stage-count max-stage-count))
          ((or (< stage-count 0)
               (> stage-count max-stage-count))
           (message :err "Stage count of {} is out of bounds. Maximum stage count is {}."
                    stage-count max-stage-count)))
    (dotimes (stage stage-count)
      (load-stage system stage name load-verbose))
    (setq *features* (core:remove-equal :staging *features*))
    (when reproducible
      (prepare-metadata system))
    (setq system (mapcar #'(lambda (entry &aux (output-path (getf entry :output-path))
                                          (source-path (getf entry :source-path)))
                             (setq write-date (max write-date (file-write-date source-path)))
                             (list* :out-of-date (or clean
                                                     (not (probe-file output-path))
                                                     (< (file-write-date output-path)
                                                        write-date))
                                    entry))
                         system))
    (if system-sort
        (sort system #'> :key #'(lambda (entry)
                                  (getf entry :load-time most-positive-fixnum)))
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

(export '(command-line-paths
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
