;;
;; Clasp builder code
;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (core:select-package :core))

(defparameter *number-of-jobs* 1)

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
          (load path)
          (error "Illegal type ~a for load-kernel-file ~a" type path)))
  path)

(defun compile-kernel-file (entry &key (reload nil) load-bitcode (force-recompile nil) counter total-files (output-type core:*clasp-build-mode*) verbose print silent)
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
            (if (and counter total-files)
                (bformat t "Compiling [%d of %d] %s%N    to %s - will reload: %s%N" counter total-files source-path output-path reload)
                (bformat t "Compiling %s%N   to %s - will reload: %s%N" source-path output-path reload)))
          (let ((cmp::*module-startup-prefix* "kernel"))
            #+dbg-print(bformat t "DBG-PRINT  source-path = %s%N" source-path)
            (apply #'compile-file
                   (probe-file source-path)
                   :output-file output-path
                   :output-type output-type
                   :print print
                   :verbose verbose
                   :type :kernel (entry-compile-file-options entry))
            (if reload
                (let ((reload-file (if (eq output-type :object)
                                       (make-pathname :type (bitcode-extension) :defaults output-path)
                                       output-path)))
                  (bformat t "    Loading newly compiled file: %s%N" reload-file)
                  (load-kernel-file reload-file output-type))))))
    output-path))
(export 'compile-kernel-file)

(eval-when (:compile-toplevel :execute)
  (core:fset 'compile-execute-time-value
             #'(lambda (whole env)
                 (let* ((expression (second whole))
                        (result (eval expression)))
                   `',result))
             t))

(defun compile-file-and-load (entry)
  (let* ((filename (entry-filename entry))
         (pathname (probe-file (build-pathname filename :lisp)))
         (name (namestring pathname)))
    (bformat t "Compiling/loading source: %s%N" (namestring name))
    (let ((m (cmp::compile-file-to-module name :print nil)))
      (progn
        (cmp::link-builtins-module m)
        (cmp::optimize-module-for-compile m))
      (llvm-sys:load-module m))))

(defun load-system (files &key compile-file-load interp load-bitcode (target-backend *target-backend*) system)
  #+dbg-print(bformat t "DBG-PRINT  load-system: %s - %s%N" first-file last-file )
  (let* ((*target-backend* target-backend)
	 (cur files))
    (tagbody
     top
       (if (endp cur) (go done))
       (if (not interp)
	   (if (bitcode-exists-and-up-to-date (car cur))
               (iload (car cur) :load-bitcode load-bitcode)
               (if compile-file-load
                   (compile-file-and-load (car cur))
                   (progn
                     (setq load-bitcode nil)
                     (interpreter-iload (car cur)))))
	   (interpreter-iload (car cur)))
       (gctools:cleanup)
       (setq cur (cdr cur))
       (go top)
     done)))

(defun compile-system-serial (files &key reload (output-type core:*clasp-build-mode*) parallel-jobs)
  (declare (ignore parallel-jobs))
  #+dbg-print(bformat t "DBG-PRINT compile-system files: %s%N" files)
  (with-compilation-unit ()
    (let* ((cur files)
           (counter 1)
           (total (length files)))
      (tagbody
       top
         (if (endp cur) (go done))
         (compile-kernel-file (car cur) :reload reload :output-type output-type :counter counter :total-files total :print t :verbose t)
         (setq cur (cdr cur))
         (setq counter (+ 1 counter))
         (go top)
       done))))


(defun compile-system-parallel (files &key reload (output-type core:*clasp-build-mode*) (parallel-jobs *number-of-jobs*))
  #+dbg-print(bformat t "DBG-PRINT compile-system files: %s\n" files)
  (let ((total (length files))
        (counter 0)
        (child-count 0)
        (jobs (make-hash-table :test #'eql)))
    (flet ((started-one (entry counter child-pid)
             (let* ((filename (entry-filename entry))
                    (source-path (build-pathname filename :lisp))
                    (output-path (build-pathname filename output-type)))
               (format t "Compiling [~d of ~d (child-pid: ~d)] ~a~%    to ~a~%" counter total child-pid source-path output-path)))
           (finished-one (entry counter child-pid result-stream)
             (let* ((filename (entry-filename entry))
                    (source-path (build-pathname filename :lisp)))
               (format t "Finished [~d of ~d (child pid: ~d)] ~a output follows...~%" counter total child-pid source-path)
               (with-open-stream (sin result-stream)
                 (do ((line (read-line sin nil nil) (read-line sin nil nil)))
                     ((null line))
                   (princ "--> ")
                   (princ line)
                   (terpri)))))
           (reload-one (entry)
             (let* ((filename (entry-filename entry))
                    (source-path (build-pathname filename :lisp))
                    (output-path (build-pathname filename output-type)))
               (format t "Loading ~a~%" output-path)
               (load-kernel-file output-path output-type)))
           (one-compile-kernel-file (entry counter)
             (compile-kernel-file entry :reload reload :output-type output-type :counter counter :total-files total :silent t)))
      (let (entry job-counter wpid status)
        (tagbody
         top
           (setq entry (if files (pop files) nil))
           (setq job-counter (incf counter))
           (when (> counter parallel-jobs)
             (block wait-loop
               (tagbody
                top
                  (multiple-value-setq (wpid status) (core:wait))
                  (core:bformat t "wpid -> %s  status -> %s\n" wpid status)
                  (when (core:wifexited status) (go done))
                  (when (core:wifsignaled status)
                    (let ((signal (core:wtermsig status)))
                      (warn "Child process with pid ~a got signal ~a" signal)))
                  (go top)
                done
                  ))
             (if (>= wpid 0)
                 (let* ((finished-entry-triplet (gethash wpid jobs))
                        (finished-entry (first finished-entry-triplet))
                        (finished-job-counter (second finished-entry-triplet))
                        (result-stream (third finished-entry-triplet)))
                   (finished-one finished-entry finished-job-counter wpid result-stream)
                   (when reload (reload-one finished-entry))
                   (decf child-count))
                 (error "wait returned ~d  status ~d~%" wpid status)))
           (when entry
             (multiple-value-bind (maybe-error pid-or-error result-stream)
                 (core:fork t)
               (if maybe-error
                   (error "Could not fork when trying to build ~a" entry)
                   (let ((pid pid-or-error))
                     (if (= pid 0)
                         (progn
                           ;; Turn off interactive mode so that errors cause clasp to die with backtrace
                           (core:set-interactive-lisp nil)
                           (let ((new-sigset (core:make-cxx-object 'core:sigset))
                                 (old-sigset (core:make-cxx-object 'core:sigset)))
                             (core:sigset-sigaddset new-sigset 'core:signal-sigint)
                             (core:sigset-sigaddset new-sigset 'core:signal-sigchld)
                             (multiple-value-bind (fail errno)
                                 (core:sigthreadmask :sig-setmask new-sigset old-sigset)
                               (one-compile-kernel-file entry job-counter)
                               (core:sigthreadmask :sig-setmask old-sigset nil)
                               (when fail
                                 (error "sigthreadmask has an error errno = ~a" errno))
                               (core:exit))))
                         (progn
                           (started-one entry job-counter pid)
                           (setf (gethash pid jobs) (list entry job-counter result-stream))
                           (incf child-count)))))))
           (when (> child-count 0) (go top))))))
  (format t "Leaving compile-system-parallel~%"))

(defun compile-system (&rest args)
  (let ((compile-function (if (and core:*use-parallel-build* (> *number-of-jobs* 1))
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
  (bformat t "\nBuild aborted.\n")
  (bformat t "Received condition of type: %s\n%s\n"
           (type-of condition)
           condition)
  (bformat t "Entering repl\n"))

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
               (files-with-epilogue (out-of-date-bitcodes #P"src/lisp/kernel/tag/start" #P"src/lisp/kernel/tag/min-end" :system system)))
          (with-compilation-unit ()
            (let ((cmp::*activation-frame-optimize* nil))
              ;; It's better to compile aclasp with fewer cores because then more of the compilations
              ;; make use of compiled code.
              ;;  Compiling and reloading the following files will speed things up
              (compile-system files :reload t :parallel-jobs (min *number-of-jobs* 16))
              ;;  Just compile the following files and don't reload, they are needed to link
              (compile-system pre-files :reload nil :parallel-jobs (min *number-of-jobs* 16))
              (if files-with-epilogue (compile-system (output-object-pathnames #P"src/lisp/kernel/tag/min-pre-epilogue" #P"src/lisp/kernel/tag/min-end" :system system) :reload nil))))
          (let ((all-output (output-object-pathnames #P"src/lisp/kernel/tag/min-start" #P"src/lisp/kernel/tag/min-end" :system system)))
            (if (out-of-date-target output-file all-output)
                (link-modules output-file all-output)))))))

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
  (if clean (clean-system #P"src/lisp/kernel/tag/start" :no-prompt t :system system))
  (let ((*target-backend* (default-target-backend)))
    (if (or (out-of-date-bitcodes #P"src/lisp/kernel/tag/start" #P"src/lisp/kernel/tag/bclasp" :system system)
            (null (probe-file output-file)))
        (progn
          (load-system (select-source-files #P"src/lisp/kernel/tag/start" #P"src/lisp/kernel/tag/pre-epilogue-bclasp" :system system))
          (let ((files (out-of-date-bitcodes #P"src/lisp/kernel/tag/start" #P"src/lisp/kernel/tag/bclasp" :system system)))
            (compile-system files)
            (let ((all-output (output-object-pathnames #P"src/lisp/kernel/tag/start" #P"src/lisp/kernel/tag/bclasp" :system system)))
              (if (out-of-date-target output-file all-output)
                    (link-modules output-file all-output))))))))

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

(defun compile-cclasp* (output-file system)
  "Compile the cclasp source code."
  (let ((ensure-adjacent (select-source-files #P"src/lisp/kernel/cleavir/inline-prep" #P"src/lisp/kernel/cleavir/auto-compile" :system system)))
    (or (= (length ensure-adjacent) 2) (error "src/lisp/kernel/inline-prep MUST immediately preceed src/lisp/kernel/auto-compile - currently the order is: ~a" ensure-adjacent)))
  (let ((files (append (out-of-date-bitcodes #P"src/lisp/kernel/tag/start" #P"src/lisp/kernel/cleavir/inline-prep" :system system)
                       (select-source-files #P"src/lisp/kernel/cleavir/auto-compile"
                                            #P"src/lisp/kernel/tag/cclasp"
                                            :system system))))
    ;; Inline ASTs refer to various classes etc that are not available while earlier files are loaded.
    ;; Therefore we can't have the compiler save inline definitions for files earlier than we're able
    ;; to load inline definitions. We wait for the source code to turn it back on.
    (setf core:*defun-inline-hook* nil)
    (compile-system files :reload nil)
    (let ((all-output (output-object-pathnames #P"src/lisp/kernel/tag/start" #P"src/lisp/kernel/tag/cclasp" :system system)))
      (if (out-of-date-target output-file all-output)
          (link-modules output-file all-output)))))
  
(defun recompile-cclasp (&key clean (output-file (build-common-lisp-bitcode-pathname)) (system (command-line-arguments-as-list)))
  (if clean (clean-system #P"src/lisp/kernel/tag/start" :no-prompt t))
  (compile-cclasp* output-file system))

(defun load-cclasp (&key (system (command-line-arguments-as-list)))
  (progn
    (load-system (select-source-files #P"src/lisp/kernel/tag/bclasp" #P"src/lisp/kernel/cleavir/inline-prep" :system system) :compile-file-load t)
    (load-system (select-source-files #P"src/lisp/kernel/cleavir/auto-compile" #P"src/lisp/kernel/tag/cclasp" :system system) :compile-file-load nil )))
(export '(load-cclasp))

(defun compile-cclasp (&key clean (output-file (build-common-lisp-bitcode-pathname)) (system (command-line-arguments-as-list)))
  (handler-bind
      ((error #'build-failure))
    (cclasp-features)
    (if clean (clean-system #P"src/lisp/kernel/tag/start" :no-prompt t :system system))
    (let ((*target-backend* (default-target-backend)))
      (time
       (progn
         (unwind-protect
              (progn
                (push :compiling-cleavir *features*)
                (load-system (select-source-files #P"src/lisp/kernel/tag/bclasp"
                                                  #P"src/lisp/kernel/tag/pre-epilogue-cclasp"
                                                  :system system) :compile-file-load nil))
           (pop *features*))
         (push :cleavir *features*)
         (compile-cclasp* output-file system))))))

#+(or bclasp cclasp)
(defun bclasp-repl ()
  (let ((cmp:*cleavir-compile-hook* nil)
        (cmp:*cleavir-compile-file-hook* nil)
        (core:*use-cleavir-compiler* nil)
        (core:*eval-with-env-hook* #'core:eval-with-env-default))
    (core:low-level-repl)))
(export 'bclasp-repl)


(eval-when (:execute)
  (bformat t "Loaded clasp-builder.lsp%N")
  (if (member :clasp-builder-repl *features*)
      (progn
        (core:bformat t "Starting low-level repl%N")
        (unwind-protect
             (core:low-level-repl)
          (core:bformat t "Exiting low-level-repl%N")))))
