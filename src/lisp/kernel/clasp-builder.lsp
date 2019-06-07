;;
;; Clasp builder code
;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (core:select-package :core)
  )

(defparameter *number-of-jobs* 1)

#+(or)
(progn
  (defparameter *log* (open "/tmp/clasp-builder-log.txt" :direction :output :if-exists :supersede))
  (si:fset 'core::mmsg #'(lambda (whole env)
                           (let ((fmt (cadr whole))
                                 (args (cddr whole)))
                             `(progn
                                (core:bformat *log* ,fmt ,@args)
                                (finish-output *log*))))
           t))

;;;#+(or)
(si:fset 'core::mmsg #'(lambda (whole env)
                         nil)
         t)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (mmsg "Starting up%N")
  )



#+(not bclasp cclasp)
(core:fset 'cmp::with-compiler-timer
           (let ((body (gensym)))
             (core:bformat t "body = %s%N" body)
             #'(lambda (whole env)
                 (let ((body (cddr whole)))
                   `(progn
                      ,@body))))
           t)

(defun strip-root (pn-dir)
  "Remove the SOURCE-DIR: part of the path in l and then
search for the string 'src', or 'generated' and return the rest of the list that starts with that"
  (let ((rel (cdr (pathname-directory (enough-namestring (make-pathname :directory pn-dir) (translate-logical-pathname #P"SOURCE-DIR:"))))))
    (or (member "src" rel :test #'string=)
        (member "generated" rel :test #'string=)
        (error "Could not find \"src\" or \"generated\" in ~a" rel))))

(defun get-pathname-with-type (module &optional (type "lsp"))
  (error "Depreciated get-pathname-with-type")
  (cond
    ((pathnamep module)
     (merge-pathnames module
                      (make-pathname
                       :type type
                       :defaults (translate-logical-pathname
                                  (make-pathname :host "sys")))))
    ((symbolp module)
     (merge-pathnames (pathname (string module))
                      (make-pathname :host "sys" :directory '(:absolute) :type type)))
    (t (error "bad module name: ~s" module))))


(defun calculate-file-order (system)
  "Return a hash-table that maps names to an integer index in system"
  (let ((file-order (make-hash-table :test #'equal))
        (index 0))
    (tagbody
     top
       (if (>= index (length system)) (go done))
       (core:hash-table-setf-gethash file-order (elt system index) index)
       (format t "Assigned build order ~a to ~s~%" index (elt system index))
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
    (mapcar #'(lambda (f &aux (fn (entry-filename f))) (build-pathname fn output-type)) sources)))

(defun print-bitcode-file-names-one-line (start end)
  (mapc #'(lambda (f) (bformat t " %s" (namestring f)))
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
       (bformat sout "%s " (namestring (build-pathname pn :lisp)))
       (if cur (go top)))
    (get-output-stream-string sout)))

(defun out-of-date-image (image source-files)
  (let* ((last-source (car (reverse source-files)))
         (last-bitcode (build-pathname (entry-filename last-source) :bitcode))
         (image-file image))
    (format t "last-bitcode: ~a~%" last-bitcode)
    (format t "image-file: ~a~%" image-file)
    (if (probe-file image-file)
        (> (file-write-date last-bitcode)
           (file-write-date image-file))
        t)))

(defun out-of-date-target (target source-files)
  (let* ((last-source (car (reverse source-files)))
         (intrinsics-bitcode (build-inline-bitcode-pathname :executable :intrinsics))
         (builtins-bitcode (build-inline-bitcode-pathname :executable :builtins))
         (last-bitcode (build-pathname (entry-filename last-source) :bitcode))
         (target-file target))
    #+(or)(progn
            (format t "last-bitcode: ~a~%" last-bitcode)
            (format t "target-file: ~a~%" target-file))
    (if (probe-file target-file)
        (or (> (file-write-date last-bitcode)
               (file-write-date target-file))
            (> (file-write-date intrinsics-bitcode)
               (file-write-date target-file))
            (> (file-write-date builtins-bitcode)
               (file-write-date target-file)))
        t)))

(defun load-kernel-file (path type)
  (if (or (eq type :object) (eq type :bitcode))
      (progn
        (cmp:load-bitcode path))
      (if (eq type :fasl)
          (if (probe-file (make-pathname :type "fasl" :defaults path))
              (load (make-pathname :type "fasl" :defaults path))
              (cmp:load-bitcode path))
          (error "Illegal type ~a for load-kernel-file ~a" type path)))
  path)


(defun compile-kernel-file (entry &rest args &key (reload nil) load-bitcode (force-recompile nil) position total-files (output-type core:*clasp-build-mode*) verbose print silent)
  #+dbg-print(bformat t "DBG-PRINT compile-kernel-file: %s%N" entry)
  ;;  (if *target-backend* nil (error "*target-backend* is undefined"))
  (let* ((filename (entry-filename entry))
         (source-path (build-pathname filename :lisp))
         (output-path (build-pathname filename output-type))
         (load-bitcode (and (bitcode-exists-and-up-to-date filename) load-bitcode)))
    (if (and load-bitcode (not force-recompile))
        (progn
          (unless silent
            (bformat t "Skipping compilation of %s - its bitcode file %s is more recent%N" source-path output-path))
          ;;      (bformat t "   Loading the compiled file: %s%N" (path-file-name output-path))
          ;;      (load-bitcode (as-string output-path))
          )
        (progn
          (unless silent
            (bformat t "%N")
            (if (and position total-files)
                (bformat t "Compiling [%d of %d] %s%N    to %s - will reload: %s%N" position total-files source-path output-path reload)
                (bformat t "Compiling %s%N   to %s - will reload: %s%N" source-path output-path reload)))
          (let ((cmp::*module-startup-prefix* "kernel")
                (compile-file-arguments (list* (probe-file source-path)
                                               :output-file output-path
                                               :output-type output-type
                                               :print print
                                               :verbose verbose
                                               :unique-symbol-prefix (format nil "~a~a" (pathname-name source-path) position)
                                               :type :kernel ;; (if reload :kernel nil)
                                               (if position
                                                   (list* :image-startup-position position (entry-compile-file-options entry))
                                                   (entry-compile-file-options entry)))))
            #+dbg-print(bformat t "DBG-PRINT  source-path = %s%N" source-path)
            (apply #'cmp::compile-file-serial compile-file-arguments)
            (if reload
                (let ((reload-file (make-pathname :type "fasl" :defaults output-path)))
		  (unless silent
		    (bformat t "    Loading newly compiled file: %s%N" reload-file))
                  (load-kernel-file reload-file :fasl))))))
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
  (let* ((filename (entry-filename entry))
         (pathname (probe-file (build-pathname filename :lisp)))
         (name (namestring pathname)))
    (if cmp:*implicit-compile-hook*
        (bformat t "Loading/compiling source: %s%N" (namestring name))
        (bformat t "Loading/interpreting source: %s%N" (namestring name)))
    (cmp::with-compiler-timer (:message "Compiler" :verbose t)
     (load pathname))))

(defun iload (entry &key load-bitcode )
  #+dbg-print(bformat t "DBG-PRINT iload fn: %s%N" fn)
  (let* ((fn (entry-filename entry))
         (lsp-path (build-pathname fn))
         (bc-path (build-pathname fn :bitcode))
         (load-bc (if (not (probe-file lsp-path))
                      t
                      (if (not (probe-file bc-path))
                          nil
                          (if load-bitcode
                              t
                              (let ((bc-newer (> (file-write-date bc-path) (file-write-date lsp-path))))
                                bc-newer))))))
    (if load-bc
        (progn
          (bformat t "Loading fasl bitcode file: %s%N" bc-path)
          (cmp::with-compiler-timer (:message "Loaded bitcode" :verbose t)
            (load-kernel-file bc-path :fasl)))
        (if (probe-file lsp-path)
            (progn
              (if cmp:*implicit-compile-hook*
                  (bformat t "Loading/compiling source: %s%N" lsp-path)
                  (bformat t "Loading/interpreting source: %s%N" lsp-path))
              (cmp::with-compiler-timer (:message "Compiler" :verbose t)
               (load lsp-path)))
            (bformat t "No interpreted or bitcode file for %s could be found%N" lsp-path)))))

(defun load-system (files &rest args &key interp load-bitcode (target-backend *target-backend*) system)
  #+dbg-print(bformat t "DBG-PRINT  load-system: %s%N" args)
  (let* ((*target-backend* target-backend)
         (*compile-verbose* t)
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
  #+dbg-print(bformat t "DBG-PRINT compile-system files: %s%N" files)
  (let* ((cur files)
         (total (or total-files (length files))))
      (tagbody
       top
         (if (endp cur) (go done))
         (compile-kernel-file (car cur)
                              :reload reload
                              :output-type output-type
                              :position (gethash (car cur) file-order)
                              :total-files total
                              :print t
                              :verbose t)
         (setq cur (cdr cur))
         (go top)
       done)))

(defconstant +done-of+ 0)
(defconstant +pid-of+ 1)
(defconstant +signals-of+ 2)
(defconstant +entries-of+ 3)
(defconstant +child-stdout-of+ 4)
(defconstant +child-stderr-of+ 5)

(defun setf-pjob-done (pjob value) (setf-elt pjob +done-of+ value))
(defun setf-pjob-done (pjob value) (setf-elt pjob +done-of+ value))
(defun setf-pjob-pid (pjob value) (setf-elt pjob +pid-of+ value))
(defun setf-pjob-signals (pjob value) (setf-elt pjob +signals-of+ value))
(defun setf-pjob-entries (pjob value) (setf-elt pjob +entries-of+ value))
(defun setf-pjob-child-stdout (pjob value) (setf-elt pjob +child-stdout-of+ value))
(defun setf-pjob-child-stderr (pjob value) (setf-elt pjob +child-stderr-of+ value))

(defun pjob-done (pjob) (elt pjob +done-of+))
(defun pjob-pid (pjob) (elt pjob +pid-of+)) 
(defun pjob-signals (pjob) (elt pjob +signals-of+)) 
(defun pjob-entries (pjob) (elt pjob +entries-of+)) 
(defun pjob-child-stdout (pjob) (elt pjob +child-stdout-of+)) 
(defun pjob-child-stderr (pjob) (elt pjob +child-stderr-of+)) 

(defun make-pjob (&key done pid signals entries child-stdout child-stderr)
  (let ((pjob (make-array 6)))
    (setf-pjob-done pjob done)
    (setf-pjob-pid pjob pid)
    (setf-pjob-signals pjob signals)
    (setf-pjob-entries pjob entries)
    (setf-pjob-child-stdout pjob child-stdout)
    (setf-pjob-child-stderr pjob child-stderr)
    pjob))


                            
(defun wait-for-child-to-exit (jobs)
  (mmsg "About to waitpid sigchld-count: %s%N"(core:sigchld-count))
  (multiple-value-bind (wpid status)
      (core:wait)
    (mmsg "Returned from waitpid with wpid: %s status:%s%N" wpid status)
    (if (not (= wpid 0))
        (progn
          (unless (= status 0)
            (core:bformat t "wpid -> %s  status -> %s%N" wpid status))
          (when (core:wifexited status)
            (mmsg "A child exited wpid: %s  status: %s%N" wpid status)
            (return-from wait-for-child-to-exit (values wpid status nil)))
          (when (core:wifsignaled status)
            (let ((signal (core:wtermsig status)))
              (mmsg "Child process with pid %s got signal %s%N" wpid signal)
              (warn "Child process with pid ~a got signal ~a" wpid signal)))))
    ;; If we drop through to here the child died for some reason - return and inform the parent
    (values wpid status t)))

(defun compile-system-parallel (files &key reload (output-type core:*clasp-build-mode*) total-files (parallel-jobs *number-of-jobs*) (batch-min 1) (batch-max 1) file-order)
  #+dbg-print(bformat t "DBG-PRINT compile-system files: %s\n" files)
  (mmsg "compile-system-parallel files %s%N" files)
  (let ((total (or total-files (length files)))
        (job-counter 0)
        (batch-size 0)
        (child-count 0)
        child-died
        (jobs (make-hash-table :test #'eql)))
    (labels ((entry-position (entry)
               (let ((pos (gethash (entry-filename entry) file-order)))
                 (unless pos (error "Could not get position of ~s in file-order" (entry-filename entry)))
                 pos))
             (started-one (entry child-pid)
               (let* ((filename (entry-filename entry))
                      (source-path (build-pathname filename :lisp))
                      (output-path (build-pathname filename output-type)))
                 (format t "Compiling [~d of ~d (child-pid: ~d)] ~a~%    to ~a~%" (entry-position entry) total child-pid source-path output-path)))
             (started-some (entries child-pid)
               (dolist (entry entries)
                 (started-one entry child-pid)))
             (finished-report-one (entry child-pid)
               (let* ((filename (entry-filename entry))
                      (source-path (build-pathname filename :lisp)))
                 (format t "Finished [~d of ~d (child pid: ~d)] ~a output follows...~%" (entry-position entry) total child-pid source-path)))
             (read-fd-into-buffer (fd)
               (mmsg "in read-fd-into-buffer %s%N" fd)
               (let ((buffer (make-array 1024 :element-type 'base-char :adjustable nil))
                     (sout (make-string-output-stream)))
                 (core:lseek fd 0 :seek-set)
                 (block readloop
                   (tagbody
                    top
                      (mmsg "About to read-fd%N")
                      (multiple-value-bind (num-read errno)
                          (core:read-fd fd buffer)
                        (when (= num-read 0)
                          (return-from readloop))
                        (when (< num-read 0)
                          (mmsg "Three was an error reading the stream errno %d%N" errno)
                          (core:bformat t "There was an error reading the stream errno %d%N" errno))
                        (when (> num-read 0)
                          (write-sequence buffer sout :start 0 :end num-read)
                          (mmsg "Wrote <%s>%N" (subseq buffer 0 num-read))))
                      (go top)))
                 (mmsg "Returning with buffer%N")
                 (core:close-fd fd)
                 (get-output-stream-string sout)))
             (report-stream (pid fd name)
               (mmsg "In report-stream%N")
               (let* ((buffer (read-fd-into-buffer fd))
                      (sin (make-string-input-stream buffer)))
                 (do ((line (read-line sin nil nil) (read-line sin nil nil)))
                     ((null line))
                   (core:bformat t "--%d(%s)--> %s%N" pid name line)
                   (finish-output))))
             (report-child-exited (child-pid child-stdout child-stderr)
               (mmsg "In report-child-exited: %s %s%N" child-stdout child-stderr)
               (report-stream child-pid child-stdout "out")
               (report-stream child-pid child-stderr "err"))
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
                      (source-path (build-pathname filename :lisp))
                      (output-path (build-pathname filename output-type)))
                 (format t "Loading ~a~%" output-path)
                 (load-kernel-file output-path :fasl)))
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
      (let ((build-dir (pathname (core:mkdtemp "/tmp/clasp-builder")))
            entries wpid status)
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
           (when (> job-counter parallel-jobs)
             (mmsg "Going into wait-for-child-to-exit%N")
             (multiple-value-setq (wpid status child-died)
               (wait-for-child-to-exit jobs))
             (mmsg "Exited from wait-for-child-to-exit%N")
             (if (null child-died)
                 (if (and (numberp wpid) (>= wpid 0))
                     (let* ((pjob (gethash wpid jobs))
                            (finished-entries (pjob-entries pjob)))
                       (finished-some wpid pjob)
                       (when (and (numberp status)
                                  (not (zerop status)))
                         (format *error-output* "wait returned for process ~d status ~d: exiting compile-system~%" wpid status)
                         (core:exit 1))
                       (when reload (reload-some finished-entries))
                       (decf child-count))
                     (error "wait returned ~d  status ~d~%" wpid status))
                 (if (and (numberp wpid) (>= wpid 0))
                     (progn
                       (finished-some wpid (gethash wpid jobs))
                       (error "The child with wpid ~a died with status ~a - terminating build" wpid status)
                       (core:exit 1))
                     (progn
                       (error "A child died with wpid ~a status ~a" wpid status)
                       (core:exit 1)))))
           (mmsg "child-count: %s%N" child-count)
           (when entries
             (let ((child-stdout (core:mkstemp-fd "clasp-build-stdout"))
                   (child-stderr (core:mkstemp-fd "clasp-build-stderr")))
               (multiple-value-bind (maybe-error pid-or-error child-stream)
                   (core:fork-redirect child-stdout child-stderr)
                 (if maybe-error
                     (error "Could not fork when trying to build ~a" entries)
                     (let ((pid pid-or-error))
                       (if (= pid 0)
                           (progn
                             #+(or)(progn
                                     (core:bformat t "A child started up with pid %d - sleeping for 10 seconds%N" (core:getpid))
                                     (sleep 10))
                             ;; Turn off interactive mode so that errors cause clasp to die with backtrace
                             (core:set-interactive-lisp nil)
                             (let ((new-sigset (core:make-cxx-object 'core:sigset))
                                   (old-sigset (core:make-cxx-object 'core:sigset)))
                               (core:sigset-sigaddset new-sigset 'core:signal-sigint)
                               (core:sigset-sigaddset new-sigset 'core:signal-sigchld)
                               (multiple-value-bind (fail errno)
                                   (core:sigthreadmask :sig-setmask new-sigset old-sigset)
                                 (some-compile-kernel-files entries)
                                 (core:sigthreadmask :sig-setmask old-sigset nil)
                                 (when fail
                                   (error "sigthreadmask has an error errno = ~a" errno))
                                 (finish-output)
                                 (sleep 1)
                                 (core:exit))))
                           (let ((one-pjob (make-pjob
                                            :done nil
                                            :pid pid
                                            :signals nil
                                            :entries entries
                                            :child-stdout child-stdout
                                            :child-stderr child-stderr)))
                             (started-some entries pid)
                             (core:hash-table-setf-gethash jobs pid one-pjob)
                             (incf child-count))))))))
           (when (> child-count 0) (go top)))))))

(defun parallel-build-p ()
  (and core:*use-parallel-build* (> *number-of-jobs* 1)))

(defun compile-system (&rest args)
  (let ((compile-function (if (parallel-build-p)
                              'compile-system-parallel
                              'compile-system-serial)))
    (format t "Compiling with ~a / core:*use-parallel-build* -> ~a  core:*number-of-jobs* -> ~a~%" compile-function core:*use-parallel-build* *number-of-jobs*)
    
    (apply compile-function args)))

(export '(compile-system-serial compile-system compile-system-parallel))

;; Clean out the bitcode files.
;; passing :no-prompt t will not prompt the user
(export 'clean-system)
(defun clean-system (after-file &key no-prompt stage system)
  (let* ((files (select-trailing-source-files after-file :system system))
	 (cur files))
    (bformat t "Will remove modules: %s%N" files)
    (bformat t "cur=%s%N" cur)
    (let ((proceed (or no-prompt
		       (progn
			 (bformat *query-io* "Delete? (Y or N) ")
			 (string-equal (read *query-io*) "Y")))))
      (if proceed
	  (tagbody
	   top
	     (if (endp cur) (go done))
	     (delete-init-file (car cur) :really-delete t :stage stage )
	     (setq cur (cdr cur))
	     (go top)
	   done)
	  (bformat t "Not deleting%N")))))


(export 'select-source-files)

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
           

(defun recursive-remove-from-list (item list)
  (if list
      (if (equal item (car list))
          (recursive-remove-from-list item (cdr list))
          (list* (car list) (recursive-remove-from-list item (cdr list))))
      nil))
(export 'recursive-remove-from-list)


(defun remove-stage-features ()
  (setq *features* (recursive-remove-from-list :clasp-min *features*))
  (setq *features* (recursive-remove-from-list :clos *features*))
  (setq *features* (recursive-remove-from-list :aclasp *features*))
  (setq *features* (recursive-remove-from-list :bclasp *features*))
  (setq *features* (recursive-remove-from-list :cclasp *features*)))

(export '(aclasp-features with-aclasp-features))
(defun aclasp-features ()
  (remove-stage-features)
  (setq *features* (list* :aclasp :clasp-min *features*))
  (setq *target-backend* (default-target-backend)))
(core:fset 'with-aclasp-features
            #'(lambda (whole env)
                (let* ((body (cdr whole)))
                  `(let ((*features* (list* :aclasp :clasp-min *features*)))
                     ,@body)))
            t)




(defun build-failure (condition)
  (bformat *error-output* "\nBuild aborted.\n")
  (bformat *error-output* "Received condition of type: %s\n%s\n"
           (type-of condition)
           condition)
  (when (parallel-build-p)
    (bformat *error-output* "About to exit clasp")
    (core:exit 1)))

(defun load-aclasp (&key clean
                      (system (command-line-arguments-as-list)))
  (aclasp-features)
  (if clean (clean-system #P"src/lisp/kernel/tag/min-start" :no-prompt t :system system))
  (if (out-of-date-bitcodes #P"src/lisp/kernel/tag/min-start" #P"src/lisp/kernel/tag/min-end" :system system)
      (progn
        (load-system (select-source-files
                      #P"src/lisp/kernel/tag/after-init"
                      #P"src/lisp/kernel/tag/min-pre-epilogue" :system system)))))

(defun generate-loader (output-file all-compiled-files)
  (let ((output-file (make-pathname :type "lfasl" :defaults output-file)))
    (with-open-file (fout output-file :direction :output :if-exists :supersede)
      (format fout ";;;; Generated in clasp-builder.lsp by generate-loader - do not edit - these fasls need to be loaded in the given order~%")
      (dolist (one-file all-compiled-files)
        (let* ((name (make-pathname :type "fasl" :defaults one-file))
               (relative-name (enough-namestring name (translate-logical-pathname (make-pathname :host "app-fasl")))))
          (format fout "(load #P\"app-fasl:~a\")~%" (namestring relative-name)))))))

(defun link-modules (output-file all-modules)
  ;;(format t "link-modules output-file: ~a  all-modules: ~a~%" output-file all-modules)
  (cond
    ((eq core:*clasp-build-mode* :bitcode)
     (cmp:link-bitcode-modules output-file all-modules))
    ((eq core:*clasp-build-mode* :object)
     ;; Do nothing - object files are the result
     )
    ((eq core:*clasp-build-mode* :fasl)
     (generate-loader output-file all-modules))
    (t (error "Unsupported value for core:*clasp-build-mode* -> ~a" core:*clasp-build-mode*))))

    
(export '(compile-aclasp))
(defun compile-aclasp (&key clean
                         (output-file (build-common-lisp-bitcode-pathname))
                         (target-backend (default-target-backend))
                         (system (command-line-arguments-as-list)))
  (aclasp-features)
  (let ((file-order (calculate-file-order system)))
    (if clean (clean-system #P"src/lisp/kernel/tag/min-start" :no-prompt t :system system))
    (if (or (out-of-date-bitcodes #P"src/lisp/kernel/tag/min-start" #P"src/lisp/kernel/tag/min-end" :system system)
            (null (probe-file output-file)))
        (progn
          (format t "Loading system: ~a~%" system)
          (load-system
           (butlast (select-source-files #P"src/lisp/kernel/tag/after-init" #P"src/lisp/kernel/tag/min-pre-epilogue" :system system)))
          (format t "Loaded system~%")
          ;; Break up the compilation into files we just want to compile and files that we want to compile and load to speed things up
          (let* ((*target-backend* target-backend)
                 (pre-files (butlast (out-of-date-bitcodes #P"src/lisp/kernel/tag/start" #P"src/lisp/kernel/tag/min-start" :system system)))
                 (files (out-of-date-bitcodes #P"src/lisp/kernel/tag/min-start" #P"src/lisp/kernel/tag/min-pre-epilogue" :system system))
                 (epilogue-files (out-of-date-bitcodes #P"src/lisp/kernel/tag/min-pre-epilogue" #P"src/lisp/kernel/tag/min-end" :system system)))
            (let ((cmp::*activation-frame-optimize* t)
                  (core:*cache-macroexpand* (make-hash-table :test #'equal)))
              (compile-system files :reload t :parallel-jobs (min *number-of-jobs* 16) :total-files (length system) :file-order file-order)
              ;;  Just compile the following files and don't reload, they are needed to link
              (compile-system pre-files :reload nil :file-order file-order :total-files (length system))
              (if epilogue-files (compile-system epilogue-files
                                                 :reload nil
                                                 :total-files (length system)
                                                 :file-order file-order))
              (let ((all-output (output-object-pathnames #P"src/lisp/kernel/tag/min-start" #P"src/lisp/kernel/tag/min-end" :system system)))
                (if (out-of-date-target output-file all-output)
                    (link-modules output-file all-output)))))))))

(export '(bclasp-features with-bclasp-features))
(defun bclasp-features()
  (remove-stage-features)
  (setq *features* (list* :optimize-bclasp :clos :bclasp *features*))
  (setq *target-backend* (default-target-backend)))
(core:fset 'with-bclasp-features
            #'(lambda (whole env)
                (let* ((body (cdr whole)))
                  `(let ((*features* (list* :bclasp :clos *features*)))
                     ,@body)))
            t)

(export '(cclasp-features with-cclasp-features))
(defun cclasp-features ()
  (remove-stage-features)
  (setq *features* (list* :clos :cclasp *features*))
  (setq *target-backend* (default-target-backend)))
(core:fset 'with-cclasp-features
            #'(lambda (whole env)
                (let* ((body (cdr whole)))
                  `(let ((*features* (list* :cclasp :clos *features*)))
                     ,@body)))
            t)

(defun load-bclasp (&key (system (command-line-arguments-as-list)))
  (load-system (select-source-files #P"src/lisp/kernel/tag/start" #P"src/lisp/kernel/tag/pre-epilogue-bclasp" :system system)))

(export '(load-bclasp compile-bclasp))
(defun compile-bclasp (&key clean (output-file (build-common-lisp-bitcode-pathname)) (system (command-line-arguments-as-list)))
  (bclasp-features)
  (let ((file-order (calculate-file-order system)))
    (if clean (clean-system #P"src/lisp/kernel/tag/start" :no-prompt t :system system))
    (let ((*target-backend* (default-target-backend)))
      (if (or (out-of-date-bitcodes #P"src/lisp/kernel/tag/start" #P"src/lisp/kernel/tag/bclasp" :system system)
              (null (probe-file output-file)))
          (progn
            (load-system (select-source-files #P"src/lisp/kernel/tag/start" #P"src/lisp/kernel/tag/pre-epilogue-bclasp" :system system))
            (let ((files (out-of-date-bitcodes #P"src/lisp/kernel/tag/start" #P"src/lisp/kernel/tag/bclasp" :system system)))
              (compile-system files :file-order file-order :total-files (length system))
              (let ((all-output (output-object-pathnames #P"src/lisp/kernel/tag/start" #P"src/lisp/kernel/tag/bclasp" :system system)))
                (if (out-of-date-target output-file all-output)
                    (link-modules output-file all-output)))))))))

(export '(compile-cclasp recompile-cclasp))


(defun remove-files-for-boehmdc (system)
  "boehmdc doesn't do inlining and type inference properly - so we remove inline.lisp from the build system"
  #+(or)(let (keep)
          (dolist (file system)
            (if (string= "inline" (pathname-name file))
                nil
                (setq keep (cons file keep))))
          (nreverse keep))
  system)


(defun link-cclasp (&key (output-file (build-common-lisp-bitcode-pathname)) (system (command-line-arguments-as-list)))
  (let ((all-output (output-object-pathnames #P"src/lisp/kernel/tag/start" #P"src/lisp/kernel/tag/cclasp" :system system)))
    (link-modules output-file all-output)))

(defun maybe-move-to-front (files target)
  "Move the target from inside the files list to the very front"
  (if (member target files :test #'equalp)
      (let ((removed (remove target files :test #'equalp)))
        (cons target removed))
      files))

(defun compile-cclasp* (output-file system)
  "Compile the cclasp source code."
  (let ((ensure-adjacent (select-source-files #P"src/lisp/kernel/cleavir/inline-prep" #P"src/lisp/kernel/cleavir/auto-compile" :system system)))
    (or (= (length ensure-adjacent) 2) (error "src/lisp/kernel/inline-prep MUST immediately precede src/lisp/kernel/auto-compile - currently the order is: ~a" ensure-adjacent)))
  (let ((files #+(or)(append (out-of-date-bitcodes #P"src/lisp/kernel/tag/start" #P"src/lisp/kernel/cleavir/inline-prep" :system system)
                             (select-source-files #P"src/lisp/kernel/cleavir/auto-compile"
                                                  #P"src/lisp/kernel/tag/cclasp"
                                                  :system system))
               (out-of-date-bitcodes #P"src/lisp/kernel/tag/start" #P"src/lisp/kernel/tag/cclasp" :system system))
        (file-order (calculate-file-order system)))
    ;; Inline ASTs refer to various classes etc that are not available while earlier files are loaded.
    ;; Therefore we can't have the compiler save inline definitions for files earlier than we're able
    ;; to load inline definitions. We wait for the source code to turn it back on.
    (setf core:*defun-inline-hook* nil)
    ;; Pull the inline.lisp and fli.lsp files forward because they take the longest to build
    ;; Only do this if we are doing a parallel build
    (when (parallel-build-p)
      (setf files (maybe-move-to-front files #P"src/lisp/kernel/lsp/fli"))
      (setf files (maybe-move-to-front files #P"src/lisp/kernel/cleavir/inline")))
    (compile-system files :reload nil :file-order file-order :total-files (length system))
    (let ((all-output (output-object-pathnames #P"src/lisp/kernel/tag/start" #P"src/lisp/kernel/tag/cclasp" :system system)))
      (if (out-of-date-target output-file all-output)
          (link-modules output-file all-output)))))
  
(defun recompile-cclasp (&key clean (output-file (build-common-lisp-bitcode-pathname)) (system (command-line-arguments-as-list)))
  (if clean (clean-system #P"src/lisp/kernel/tag/start" :no-prompt t))
  (compile-cclasp* output-file system))

(defun compile-cclasp (&key clean (output-file (build-common-lisp-bitcode-pathname)) (system (command-line-arguments-as-list)))
  (handler-bind
      ((error #'build-failure))
    (cclasp-features)
    (if clean (clean-system #P"src/lisp/kernel/tag/start" :no-prompt t :system system))
    (let ((*target-backend* (default-target-backend))
          (*trace-output* *standard-output*))
      (time
       (progn
         (unwind-protect
              (progn
                (push :compiling-cleavir *features*)
                (let ((files (select-source-files #P"src/lisp/kernel/tag/bclasp"
                                                  #P"src/lisp/kernel/tag/pre-epilogue-cclasp"
                                                  :system system)))
                  (load-system files)))
           (pop *features*))
         (push :cleavir *features*)
         (compile-cclasp* output-file system))))))

#+(or bclasp cclasp)
(defun bclasp-repl ()
  (let ((cmp:*cleavir-compile-hook* nil)
        (cmp:*cleavir-compile-file-hook* nil)
        (core:*use-cleavir-compiler* nil)
        (core:*eval-with-env-hook* #'core:interpret-eval-with-env))
    (core:low-level-repl)))
(export 'bclasp-repl)


(eval-when (:execute)
  (bformat t "Loaded clasp-builder.lsp%N")
  (bformat t "*features* -> %s%N" *features*)
  (if (member :clasp-builder-repl *features*)
      (progn
        (core:bformat t "Starting low-level repl%N")
        (unwind-protect
             (core:low-level-repl)
          (core:bformat t "Exiting low-level-repl%N")))))
