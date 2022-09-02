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
            (case level
              (:err  31)
              (:warn 33)
              (:emph 32)
              (:debug 36)
              (:info 37)
              (otherwise 0)))
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

(defun strip-root (pn-dir)
  "Remove the SYS: part of the path in l and then
search for the string 'src', or 'generated' and return the rest of the list that starts with that"
  (let ((rel (cdr (pathname-directory (enough-namestring (make-pathname :directory pn-dir) (translate-logical-pathname #P"sys:"))))))
    (or (member "src" rel :test #'string=)
        (member "generated" rel :test #'string=)
        (error "Could not find \"src\" or \"generated\" in ~a" rel))))

(defun calculate-file-order (system)
  "Return a hash-table that maps names to an integer index in system"
  (message nil "Assigning build order:")
  (let ((file-order (make-hash-table :test #'equal))
        (index 0))
    (tagbody
     top
       (if (>= index (length system)) (go done))
       (core:hash-table-setf-gethash file-order (elt system index) index)
       (message :info "{: >3d}. {}"
                (+ 1 index) (namestring (elt system index)))
       (setq index (+ index 1))
       (go top)
     done
       )
    file-order))

(defun expand-build-file-list (sources)
  "Copy the list of symbols and pathnames into files
and if S-exps are encountered, funcall them with
no arguments and splice the resulting list into files.
Return files."
  (let* (files
         (cur sources))
    (tagbody
     top
       (if (null cur) (go done))
       (let ((entry (car cur)))
         (if (or (pathnamep entry) (keywordp entry))
             (setq files (cons entry files))
             (if (functionp entry)
                 (let* ((ecur (funcall entry)))
                   (tagbody
                    inner-top
                      (if (null ecur) (go inner-done))
                      (setq files (cons (car ecur) files))
                      (setq ecur (cdr ecur))
                      (go inner-top)
                    inner-done))
                 (error "I don't know what to do with ~a" entry))))
       (setq cur (cdr cur))
       (go top)
     done)
    (nreverse files)))

(defun select-source-files (first-file last-file &key system)
  (or first-file (error "You must provide first-file to select-source-files"))
  (or system (error "You must provide system to select-source-files"))
  (let ((cur (member first-file system :test #'equal))
        (last (if last-file
                  (let ((llast (member last-file system :test #'equal)))
                    (or llast (error "last-file ~a was not a member of ~a" last-file system))
                    llast)))
        files
        file)
    (or cur (error "first-file ~a was not a member of ~a" first-file system))
    (tagbody
     top
       (setq file (car cur))
       (if (endp cur) (go done))
       (if (not (keywordp file)) (setq files (cons file files)))
       (if (and last (eq cur last)) (go done))
       (setq cur (cdr cur))
       (go top)
     done)
    (nreverse files)))


(defun output-object-pathnames (start end &key system (output-type core:*clasp-build-mode*))
  (let ((sources (select-source-files start end :system system)))
    (mapcar #'(lambda (f &aux (fn (entry-filename f))) (bitcode-pathname fn output-type)) sources)))

(defun print-bitcode-file-names-one-line (start end)
  (mapc #'(lambda (f) (core:fmt t " {}" (namestring f)))
        (output-object-pathnames start end))
  nil)

(defun out-of-date-bitcodes (start end &key system)
  (let ((sources (select-source-files start end :system system))
        out-of-dates)
    (mapc #'(lambda (f) (if out-of-dates
                            (setq out-of-dates (list* f out-of-dates))
                            (if (not (bitcode-exists-and-up-to-date f))
                                (setq out-of-dates (cons f nil)))))
          sources)
    (nreverse out-of-dates)))

(defun source-file-names (start end)
  (let ((sout (make-string-output-stream))
        (cur (select-source-files :start end))
        pn)
    (tagbody
     top
       (setq pn (car cur))
       (setq cur (cdr cur))
       (core:fmt sout "{} " (namestring pn))
       (if cur (go top)))
    (get-output-stream-string sout)))

(defun out-of-date-image (image source-files)
  (let* ((last-source (car (reverse source-files)))
         (last-bitcode (bitcode-pathname (entry-filename last-source) :bitcode))
         (image-file image))
    (message nil "last-bitcode: {}%nimage-file: {}" last-bitcode image-file)
    (if (probe-file image-file)
        (> (file-write-date last-bitcode)
           (file-write-date image-file))
        t)))

(defun out-of-date-target (target source-files)
  (let* ((last-source (car (reverse source-files)))
         (intrinsics-bitcode (build-inline-bitcode-pathname :executable :intrinsics))
         (builtins-bitcode (build-inline-bitcode-pathname :executable :builtins))
         (last-bitcode (bitcode-pathname (entry-filename last-source) :bitcode))
         (target-file target))
    (if (probe-file target-file)
        (or (> (file-write-date last-bitcode)
               (file-write-date target-file))
            (> (file-write-date intrinsics-bitcode)
               (file-write-date target-file))
            (> (file-write-date builtins-bitcode)
               (file-write-date target-file)))
        t)))

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

(defun compile-kernel-file (entry &rest args &key (reload nil) load-bitcode (force-recompile nil) position total-files (output-type core:*clasp-build-mode*) verbose print silent)
  #+dbg-print (message :debug "DBG-PRINT compile-kernel-file: {}" entry)
  (let* ((filename (entry-filename entry))
         (output-path (bitcode-pathname filename output-type))
         (load-bitcode (and (bitcode-exists-and-up-to-date filename) load-bitcode)))
    (if (and load-bitcode (not force-recompile))
        (if (not silent)
            (message nil "Skipping compilation of {} - its bitcode file {} is more recent" filename output-path))
        (progn
          (if (not silent)
              (if (and position total-files)
                  (message nil "Compiling [{} of {}] {}%N    to {} - will reload: {}" (1+ position) total-files filename output-path reload)
                  (message nil "Compiling {}%N   to {} - will reload: {}" filename output-path reload)))
          (let ((cmp::*module-startup-prefix* "kernel")
                (compile-file-arguments (list* filename
                                               :source-debug-pathname filename
                                               :output-file output-path
                                               :output-type output-type
                                               :print print
                                               :verbose verbose
                                               :unique-symbol-prefix (format nil "~a~a" (pathname-name filename) position)
                                               :type :kernel ;; (if reload :kernel nil)
                                               (if position
                                                   (list* :image-startup-position position (entry-compile-file-options entry))
                                                   (entry-compile-file-options entry)))))
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
                  (load-kernel-file reload-file :silent silent))))))
    output-path))
(export 'compile-kernel-file)

(eval-when (:compile-toplevel :execute)
  (core:fset 'compile-execute-time-value
             #'(lambda (whole env)
                 (let* ((expression (second whole))
                        (result (eval expression)))
                   `',result))
             t))

(defun interpreter-iload (entry)
  (let ((filename (entry-filename entry)))
    (message nil "Loading/{} {}"
             (if cmp:*implicit-compile-hook*
                 "compiling"
                 "interpreting")
             (namestring filename))
    (cmp::with-compiler-timer (:message "Compiler" :verbose t)
      (load filename))))

(defun iload (entry &key load-bitcode )
  #+dbg-print (message :debug "iload fn: {}" fn)
  (let* ((fn (entry-filename entry))
         (bc-path (bitcode-pathname fn :bitcode))
         (load-bc (if (not (probe-file fn))
                      t
                      (if (not (probe-file bc-path))
                          nil
                          (if load-bitcode
                              t
                              (let ((bc-newer (> (file-write-date bc-path) (file-write-date fn))))
                                bc-newer))))))
    (if load-bc
        (progn
          (cmp::with-compiler-timer (:message "Loaded bitcode" :verbose t)
            (load-kernel-file bc-path :silent nil)))
        (if (probe-file fn)
            (progn
              (message nil "Loading/{} {}"
                       (if (eq cmp:*implicit-compile-hook* 'cmp:implicit-compile-hook-default)
                           "compiling"
                           "interpreting")
                       (namestring fn))
              (cmp::with-compiler-timer (:message "Compiler" :verbose t)
                (load fn)))
            (message :warn "No interpreted or bitcode file for {} could be found" fn)))))

(defun load-system (files &rest args &key interp load-bitcode (target-backend *target-backend*) system)
  #+dbg-print (message :debug "load-system: {}" args)
  (let* ((*target-backend* target-backend)
         (*compile-verbose* t)
         (*features* (cons :clasp-builder *features*))
         (cur files))
    (tagbody
     top
       (if (endp cur) (go done))
       (cmp::with-compiler-timer (:message "Compiler" :verbose t)
         (if (not interp)
             (if (bitcode-exists-and-up-to-date (car cur))
                 (iload (car cur) :load-bitcode load-bitcode)
                 (progn
                   (setq load-bitcode nil)
                   (interpreter-iload (car cur))))
             (interpreter-iload (car cur))))
       (gctools:cleanup)
       (setq cur (cdr cur))
       (go top)
     done)))

(defun compile-system-serial (files &key reload (output-type core:*clasp-build-mode*) total-files parallel-jobs batch-min batch-max file-order)
  (declare (ignore parallel-jobs))
  (message :emph "Compiling system serially...")
  #+dbg-print (message :debug "compile-system files: {}" files)
  (let* ((cur files)
         (total (or total-files (length files))))
      (tagbody
       top
         (if (endp cur) (go done))
         (let ((*before-ms* (get-internal-run-time))
               (*before-bytes* (gctools:bytes-allocated)))
             (compile-kernel-file (car cur)
                              :reload reload
                              :output-type output-type
                              :position (gethash (car cur) file-order)
                              :total-files total
                              :print t
                              :verbose t))
         (setq cur (cdr cur))
         (go top)
       done)))

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

(defun compile-system-parallel (files
                                &key reload (output-type core:*clasp-build-mode*) total-files
                                     (parallel-jobs *number-of-jobs*) (batch-min 1) (batch-max 1)
                                     file-order)
  #+dbg-print (message :debug "DBG-PRINT compile-system files: {}" files)
  (mmsg "compile-system-parallel files {}%N" files)
  (message :emph "Compiling system with {:d} parallel jobs..." parallel-jobs)
  (let ((total (or total-files (length files)))
        (job-counter 0)
        (batch-size 0)
        (child-count 0)
        child-died
        (jobs (make-hash-table :test #'eql)))
    (labels ((entry-position (entry)
               (let ((pos (gethash (entry-filename entry) file-order)))
                 (if pos
                     nil
                     (error "Could not get position of ~s in file-order" (entry-filename entry)))
                 pos))
             (started-one (entry child-pid)
               (let* ((filename (entry-filename entry))
                      (output-path (bitcode-pathname filename output-type)))
                 (message nil "Starting {: >3d} of {:d} [pid {:d}] {}"
                          (1+ (entry-position entry)) total child-pid (namestring filename)))
               (when (ext:getenv "CLASP_PAUSE_FORKED_CHILD")
                 (format t "CLASP_PAUSE_FORKED_CHILD is set - will pause all children until they receive SIGUSR1~%")))
             (started-some (entries child-pid)
               (dolist (entry entries)
                 (started-one entry child-pid)))
             (finished-report-one (entry child-pid)
               (let ((filename (entry-filename entry)))
                 (message :emph "Finished {: >3d} of {:d} [pid {:d}] {} output follows..."
                          (1+ (entry-position entry)) total child-pid (namestring filename))))
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
                 (do ((line (read-line sin nil nil) (read-line sin nil nil)))
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
               (let* ((filename (entry-filename entry))
                      (output-path (bitcode-pathname filename output-type)))
                 (load-kernel-file output-path :silent nil)))
             (reload-some (entries)
               (dolist (entry entries)
                 (reload-one entry)))
             (one-compile-kernel-file (entry)
               ;; Don't reload in the child process - there is no point
               (compile-kernel-file entry :reload nil :output-type output-type :position (entry-position entry) :total-files total :silent t :verbose t))
             (some-compile-kernel-files (entries)
               (dolist (entry entries)
                 (one-compile-kernel-file entry))))
      (gctools:garbage-collect)
      (let (entries wpid status)
        (tagbody
         top
           (setq batch-size (let ((remaining (length files)))
                              (min remaining
                                   batch-max
                                   (max batch-min
                                        (ceiling remaining
                                                 parallel-jobs)))))
           (setq entries (subseq files 0 batch-size))
           (setq files (nthcdr batch-size files))
           (incf job-counter)
           (if (> job-counter parallel-jobs)
               (progn
                 (mmsg "Going into wait-for-child-to-exit%N")
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
  core:*use-parallel-build*)

(defun compile-system (&rest args)
  (apply (if (parallel-build-p)
             'compile-system-parallel
             'compile-system-serial)
         args))

(export '(compile-system-serial compile-system compile-system-parallel))

(defun select-trailing-source-files (after-file &key system)
  (or system (error "You must provide :system to select-trailing-source-files"))
  (let ((cur (reverse system))
        files file)
    (tagbody
     top
       (setq file (car cur))
       (if (endp cur) (go done))
       (if after-file
           (if (eq after-file file)
               (go done)))
       (if (not (keywordp file))
           (setq files (cons file files)))
       (setq cur (cdr cur))
       (go top)
     done)
    files))

(export 'select-source-files)

;; Clean out the bitcode files.
;; passing :no-prompt t will not prompt the user
(defun clean-system (after-file &key no-prompt stage system)
  (let* ((files (select-trailing-source-files after-file :system system))
         (cur files))
    (message nil "Will remove modules: {}" files)
    (message nil "cur={}" cur)
    (let ((proceed (or no-prompt
                       (progn
                         (core:fmt *query-io* "Delete? (Y or N) ")
                         (string-equal (read *query-io*) "Y")))))
      (if proceed
          (tagbody
           top
             (if (endp cur) (go done))
             (delete-init-file (car cur) :really-delete t :stage stage)
             (setq cur (cdr cur))
             (go top)
           done)
          (message nil "Not deleting")))))

(export 'clean-system)

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
(export 'command-line-arguments-as-list)           

(defun command-line-paths (&optional (start 0)
                           &aux (index (length core:*command-line-arguments*))
                                paths)
  (tagbody
   next
    (if (> index start)
        (progn
          (setq index (- index 1)
                paths (cons (pathname (elt core:*command-line-arguments* index)) paths))
          (go next))))
  paths)

(defun remove-stage-features ()
  (setq *features* (core:remove-equal :clasp-min *features*))
  (setq *features* (core:remove-equal :clos *features*))
  (setq *features* (core:remove-equal :aclasp *features*))
  (setq *features* (core:remove-equal :bclasp *features*))
  (setq *features* (core:remove-equal :cclasp *features*))
  (setq *features* (core:remove-equal :eclasp *features*)))

(export '(aclasp-features with-aclasp-features))
(defun aclasp-features ()
  (remove-stage-features)
  (setq *features* (list* :aclasp :clasp-min *features*))
  (message :emph "Aclasp *features* -> {}" *features*)
  (setq *target-backend* (default-target-backend)))
(core:fset 'with-aclasp-features
            #'(lambda (whole env)
                (let* ((body (cdr whole)))
                  `(let ((*features* (list* :aclasp :clasp-min *features*)))
                     ,@body)))
            t)

(defun build-failure (condition)
  (message :warn "%nBuild aborted.%nReceived condition of type: {}%n{}"
           (type-of condition)
           condition)
  (if (parallel-build-p)
      (message :err "About to exit clasp")))

(defun extract-installed-system (system)
  "During a reproducible build the system source files are interleaved with the install
paths. This function returns the install paths and removes them from the system list."
  (let (installed-system)
    (tagbody
     next
      (if (not system)
          (go end))
      (setq installed-system (cons (cadr system) installed-system))
      (rplacd system (cddr system))
      (setq system (cdr system))
      (go next)
     end)
    (nreverse installed-system)))

(defun prepare-metadata (system installed-system
                         &aux (make-create-file-args (find-symbol "MAKE-CREATE-FILE-ARGS" "CMP")))
  "Call make-create-file-args with each system path and the installed path so that when the
DIFile is actually created the argument list passed to llvm-sys:create-file will have already
been initialized with install path versus the build path of the source code file."
  (tagbody
   next
    (if (or (null system) (null installed-system))
        (go end))
    (funcall make-create-file-args (car system) (namestring (car system))
             (car installed-system))
    (setq system (cdr system)
          installed-system (cdr installed-system))
    (go next)
   end))

(defun load-aclasp (&key clean
                      (system (command-line-arguments-as-list)))
  (aclasp-features)
  (if clean
      (clean-system #P"sys:src;lisp;kernel;tag;min-start.lisp"
                    :no-prompt t :system system))
  (if (out-of-date-bitcodes #P"sys:src;lisp;kernel;tag;min-start.lisp"
                            #P"sys:src;lisp;kernel;tag;min-end.lisp"
                            :system system)
    (load-system (select-source-files #P"sys:src;lisp;kernel;tag;after-init.lisp"
                                      #P"sys:src;lisp;kernel;tag;min-pre-epilogue.lisp"
                                      :system system))))

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
  (cond
    ((eq core:*clasp-build-mode* :bitcode)
     (cmp:link-bitcode-modules output-file all-modules))
    ((eq core:*clasp-build-mode* :object)
     ;; Do nothing - object files are the result
     )
    ((eq core:*clasp-build-mode* :faso)
     ;; Do nothing - faso files are the result
     (core:link-faso-files output-file all-modules nil))
    ((eq core:*clasp-build-mode* :fasoll)
     (cmp::link-fasoll-modules output-file all-modules))
    ((eq core::*clasp-build-mode* :fasobc)
     (cmp::link-fasobc-modules output-file all-modules))
    ((eq core:*clasp-build-mode* :fasl)
     (generate-loader output-file all-modules))
    (t (error "Unsupported value for core:*clasp-build-mode* -> ~a" core:*clasp-build-mode*))))

(export '(load-aclasp compile-aclasp link-fasl))
(defun compile-aclasp (&key clean reproducible
                            (output-file (build-common-lisp-bitcode-pathname))
                            (target-backend (default-target-backend))
                            (system (command-line-arguments-as-list))
                       &aux installed-system)
  (message :emph "compile-aclasp output-file: {}" output-file)
  (aclasp-features)
  (if reproducible
      (setq installed-system (extract-installed-system system)))
  (let ((file-order (calculate-file-order system)))
    (if clean
        (clean-system #P"sys:src;lisp;kernel;tag;min-start.lisp"
                      :no-prompt t :system system))
    (if (or (out-of-date-bitcodes #P"sys:src;lisp;kernel;tag;min-start.lisp"
                                  #P"sys:src;lisp;kernel;tag;min-end.lisp"
                                  :system system)
            (null (probe-file output-file)))
        (progn
          (message :emph "Loading system...")
          (load-system (butlast (select-source-files #P"sys:src;lisp;kernel;tag;after-init.lisp"
                                                     #P"sys:src;lisp;kernel;tag;min-pre-epilogue.lisp"
                                                     :system system)))
          (message :emph "Loaded system")
          ;; Break up the compilation into files we just want to compile and files that we want to compile and load to speed things up
          (let* ((*target-backend* target-backend)
                 (pre-files (butlast (out-of-date-bitcodes #P"sys:src;lisp;kernel;tag;start.lisp"
                                                           #P"sys:src;lisp;kernel;tag;min-start.lisp"
                                                           :system system)))
                 (files (out-of-date-bitcodes #P"sys:src;lisp;kernel;tag;min-start.lisp"
                                              #P"sys:src;lisp;kernel;tag;min-pre-epilogue.lisp"
                                              :system system))
                 (epilogue-files (out-of-date-bitcodes #P"sys:src;lisp;kernel;tag;min-pre-epilogue.lisp"
                                                       #P"sys:src;lisp;kernel;tag;min-end.lisp"
                                                       :system system)))
            (let ((cmp::*activation-frame-optimize* t)
                  (core:*cache-macroexpand* (make-hash-table :test #'equal)))
              ;(prepare-metadata system installed-system)
              (compile-system files :reload nil ; RELOAD USED T HERE
                              :total-files (length system) :file-order file-order)
              ;;  Just compile the following files and don't reload, they are needed to link
              (compile-system pre-files :reload nil :file-order file-order :total-files (length system))
              (format t "Done with compile-system of aclasp~%")
              (if epilogue-files (compile-system epilogue-files
                                                 :reload nil
                                                 :total-files (length system)
                                                 :file-order file-order))))))))

(defun link-fasl (&key clean
                    (output-file (build-common-lisp-bitcode-pathname))
                    (target-backend (default-target-backend))
                    (system (command-line-arguments-as-list)))
  (cond
    ((eq core:*clasp-build-mode* :bitcode)
     (cmp:link-bitcode-modules output-file system))
    ((eq core:*clasp-build-mode* :object)
     ;; Do nothing - object files are the result
     )
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
    (t (error "Unsupported value for core:*clasp-build-mode* -> ~a" core:*clasp-build-mode*))))

(export '(bclasp-features with-bclasp-features))
(defun bclasp-features()
  (remove-stage-features)
  (setq *features* (list* :optimize-bclasp :clos :bclasp *features*))
  (message :emph "Bclasp *features* -> {}" *features*)
  (setq *target-backend* (default-target-backend)))
(core:fset 'with-bclasp-features
            #'(lambda (whole env)
                (let* ((body (cdr whole)))
                  `(let ((*features* (list* :bclasp :clos *features*)))
                     ,@body)))
            t)

(export '(cclasp-features with-cclasp-features))
(defun cclasp-features (eclasp)
  (remove-stage-features)
  (setq *features* (list* :clos (if eclasp :eclasp :cclasp) *features*))
  (message :emph "{}clasp *features* -> {}"
           (if eclasp "E" "C") *features*)
  (setq *target-backend* (default-target-backend)))
(core:fset 'with-cclasp-features
            #'(lambda (whole env)
                (let* ((body (cdr whole)))
                  `(let ((*features* (list* :cclasp :clos *features*)))
                     ,@body)))
            t)

(defun load-bclasp (&key (system (command-line-arguments-as-list)))
  (load-system (select-source-files #P"sys:src;lisp;kernel;tag;start.lisp"
                                    #P"sys:src;lisp;kernel;tag;pre-epilogue-bclasp.lisp"
                                    :system system)))

(export '(load-bclasp compile-bclasp))
(defun compile-bclasp (&key reproducible clean (output-file (build-common-lisp-bitcode-pathname))
                            (system (command-line-arguments-as-list))
                       &aux installed-system)
  (bclasp-features)
  (if reproducible
      (setq installed-system (extract-installed-system system)))
  (if clean
      (clean-system #P"sys:src;lisp;kernel;tag;start.lisp" :no-prompt t :system system))
  (let* ((*target-backend* (default-target-backend))
         (*trace-output* *standard-output*)
         (file-order (calculate-file-order system))
         (files (out-of-date-bitcodes #P"sys:src;lisp;kernel;tag;start.lisp"
                                      #P"sys:src;lisp;kernel;tag;bclasp.lisp"
                                      :system system)))
    (if (or files (null (probe-file output-file)))
        (progn
          (load-system (select-source-files #P"sys:src;lisp;kernel;tag;start.lisp"
                                            #P"sys:src;lisp;kernel;tag;pre-epilogue-bclasp.lisp"
                                            :system system))
          (prepare-metadata system installed-system)
          (compile-system files :file-order file-order :total-files (length system))))))

(export '(compile-cclasp recompile-cclasp compile-eclasp))

(defun link-cclasp (&key (output-file (build-common-lisp-bitcode-pathname))
                         (system (command-line-arguments-as-list)))
  (link-modules output-file
                (output-object-pathnames #P"sys:src;lisp;kernel;tag;start.lisp"
                                         #P"sys:src;lisp;kernel;tag;cclasp.lisp"
                                         :system system)))

(defun maybe-move-to-front (files target)
  "Move the target from inside the files list to the very front"
  (if (member target files :test #'equalp)
      (let ((removed (remove target files :test #'equalp)))
        (cons target removed))
      files))

(defun compile-cclasp* (output-file system eclasp)
  "Compile the cclasp source code."
  (let* ((first #P"sys:src;lisp;kernel;cleavir;inline-prep.lisp")
         (second #P"sys:src;lisp;kernel;cleavir;auto-compile.lisp")
         (ensure-adjacent (select-source-files first second :system system)))
    (or (= (length ensure-adjacent) 2)
        (error "~a MUST immediately precede ~a - currently the order is: ~a"
               first second ensure-adjacent)))
  (let ((files (out-of-date-bitcodes #P"sys:src;lisp;kernel;tag;start.lisp"
                                     (if eclasp
                                         #P"sys:src;lisp;kernel;tag;eclasp.lisp"
                                         #P"sys:src;lisp;kernel;tag;cclasp.lisp")
                                     :system system))
        (file-order (calculate-file-order system)))
    ;; Inline ASTs refer to various classes etc that are not available while earlier files are loaded.
    ;; Therefore we can't have the compiler save inline definitions for files earlier than we're able
    ;; to load inline definitions. We wait for the source code to turn it back on.
    (setq core:*defun-inline-hook* nil)
    ;; Pull the inline.lisp and fli.lisp files forward because they take the longest to build
    ;; Only do this if we are doing a parallel build
    (if (parallel-build-p)
        (progn
          (setq files (maybe-move-to-front files #P"sys:src;lisp;kernel;lsp;fli.lisp"))
          (setq files (maybe-move-to-front files #P"sys:src;lisp;kernel;cleavir;inline.lisp"))))
    (compile-system files :reload nil :file-order file-order :total-files (length system))))
  
(defun recompile-cclasp (&key clean (output-file (build-common-lisp-bitcode-pathname))
                              (system (command-line-arguments-as-list)) eclasp)
  (if clean (clean-system #P"sys:src;lisp;kernel;tag;start.lisp" :no-prompt t))
  (compile-cclasp* output-file system eclasp))

(defun compile-cclasp (&key reproducible clean (output-file (build-common-lisp-bitcode-pathname))
                            (system (command-line-arguments-as-list))
                       &aux installed-system)
  (cclasp-features nil)
  (if reproducible
      (setq installed-system (extract-installed-system system)))
  (if clean
      (clean-system #P"sys:src;lisp;kernel;tag;start.lisp" :no-prompt t :system system))
  (let ((*target-backend* (default-target-backend))
        (*trace-output* *standard-output*))
    (time
     (progn
       (unwind-protect
            (progn
              (push :compiling-cleavir *features*)
              (let* ((files (select-source-files #P"sys:src;lisp;kernel;tag;bclasp.lisp"
                                                 #P"sys:src;lisp;kernel;tag;pre-epilogue-cclasp.lisp"
                                                 :system system)))
                ;; We recompile and load these Eclector files under
                ;; cclasp because the functions here are performance
                ;; critical for compilation and cclasp does a much
                ;; better job at reducing unwinding, thereby speeding
                ;; up bootstrapping quite a bit by recompiling and
                ;; loading these as soon as possible.
                (setq files (append files
                                    '(#P"sys:src;lisp;kernel;contrib;eclector;code;reader;tokens.lisp"
                                      #P"sys:src;lisp;kernel;contrib;eclector;code;reader;read-common.lisp"
                                      #P"sys:src;lisp;kernel;contrib;eclector;code;reader;macro-functions.lisp"
                                      #P"sys:src;lisp;kernel;contrib;eclector;code;reader;read.lisp")))
                (with-compilation-unit ()
                  (load-system files))))
         (pop *features*))
       (push :cleavir *features*)
       (prepare-metadata system installed-system)
       (handler-bind
           ((error #'build-failure))
         (compile-cclasp* output-file system nil))))))

(defun compile-eclasp (&key reproducible clean (output-file (build-common-lisp-bitcode-pathname))
                            (system (command-line-arguments-as-list))
                       &aux installed-system)
  (cclasp-features t)
  (if reproducible
      (setq installed-system (extract-installed-system system)))
  (if clean
      (clean-system #P"sys:src;lisp;kernel;tag;start.lisp" :no-prompt t :system system))
  (let ((*target-backend* (default-target-backend))
        (*trace-output* *standard-output*))
    (load-system (select-source-files #P"sys:src;lisp;kernel;tag;cclasp.lisp"
                                      #P"sys:src;lisp;kernel;tag;pre-epilogue-eclasp.lisp"
                                      :system system))
    (prepare-metadata system installed-system)
    (compile-cclasp* output-file system t)))

(defun pprint-features ()
  (message :info "Features {}" *features*))

(export 'pprint-features)

(defvar *system-load-times*)

(defun load-stage (system stage)
  (let ((files (select-source-files (make-pathname :host "sys"
                                                   :directory '(:absolute "src" "lisp" "kernel" "stage")
                                                   :name (core:fmt nil "{:d}-begin" stage)
                                                   :type "lisp")
                                    (make-pathname :host "sys"
                                                   :directory '(:absolute "src" "lisp" "kernel" "stage")
                                                   :name (core:fmt nil "{:d}-end" stage)
                                                   :type "lisp")
                                    :system system))
        (stage-keyword (intern (core:fmt nil "STAGE{:d}" stage) :keyword))
        (stage-time (get-internal-run-time))
        file-time file)
    (setq *features* (cons stage-keyword *features*))
    (message :emph "Loading stage {:d}..." stage)
    (tagbody
     next
      (if files
          (progn
            (setq file (car files)
                  file-time (get-internal-run-time)
                  files (cdr files))
            (load file)
            (setq file-time (- (get-internal-run-time) file-time))
            (core:hash-table-setf-gethash *system-load-times* file
                                          (cons file-time (gethash file *system-load-times*)))
            (message nil ";;; Load time {:.6f} seconds" (float (/ file-time internal-time-units-per-second)))
            (go next))))
    (setq *features* (core:remove-equal stage-keyword *features*))
    (message :emph "Stage {:d} elapsed time: {:.6f} seconds"
             stage
             (float (/ (- (get-internal-run-time) stage-time) internal-time-units-per-second)))))

(defvar +stage-features+ '(:clasp-min :clos :aclasp :bclasp :cclasp :eclasp))

(defun stage-features (&rest rest &aux (features +stage-features+))
  (tagbody
   next
   (if features
       (progn
         (setq *features* (core:remove-equal (car features) *features*)
               features (cdr features))
         (go next))))
  (setq *features* (append rest *features*))
  (pprint-features))

(defun get-load-time (file)
  (let ((times (gethash file *system-load-times*)))
    (if times
        (float (/ (apply #'+ times) (length times)))
        0 #+(or)most-positive-fixnum)))

(defun load-vclasp (&key reproducible clean (output-file (build-common-lisp-bitcode-pathname))
                         (system (command-line-arguments-as-list))
                         (stage-count (if (ext:getenv "CLASP_STAGE_COUNT")
                                          (parse-integer (ext:getenv "CLASP_STAGE_COUNT"))
                                        7))
                         sort-system
                    &aux installed-system
                         (*system-load-times* (make-hash-table :test #'eql)))     
  (if reproducible
      (setq installed-system (extract-installed-system system)))
  (let ((*target-backend* (default-target-backend))
        (*trace-output* *standard-output*)
        (*load-verbose* t)
        (stage 0))
    (setq *features* (list* :staging :bytecode :bytecodelike *features*))
    (tagbody
     next
      (if (< stage stage-count)
          (progn
            (load-stage system stage)
            (setq stage (1+ stage))
            (go next))))
    (setq *features* (core:remove-equal :staging *features*))
    (prepare-metadata system installed-system)
    (if sort-system
        (sort system #'> :key #'get-load-time)
        system)))

(defun compile-vclasp (&rest rest)
  (let ((system (apply #'load-vclasp rest)))
    (handler-bind
        ((error #'build-failure))
      (compile-system system :reload nil :file-order (calculate-file-order system) :total-files (length system)))))

(export '(load-vclasp compile-vclasp))
      
#+(or bclasp cclasp eclasp)
(defun bclasp-repl ()
  (let ((cmp:*cleavir-compile-hook* nil)
        (cmp:*cleavir-compile-file-hook* nil)
        (core:*use-cleavir-compiler* nil)
        (core:*eval-with-env-hook* #'core:interpret-eval-with-env))
    (core:low-level-repl)))
(export 'bclasp-repl)

(eval-when (:execute)
  (message :emph "Loaded clasp-builder%n*features* -> {}" *features*)
  (if (member :clasp-builder-repl *features*)
      (progn
        (message nil "Starting low-level repl")
        (unwind-protect
             (core:low-level-repl)
          (message nil "Exiting low-level-repl")))))
