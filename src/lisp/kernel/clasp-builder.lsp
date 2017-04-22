;;
;; Clasp builder code
;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (core:select-package :core))



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
              files
              file)
          (or cur (error "first-file ~a was not a member of ~a" first-file system))
          (tagbody
           top
             (setq file (car cur))
             (if (endp cur) (go done))
             (if last-file
                 (if (equal last-file file)
                     (progn
                       (if (not (keywordp file))
                           (setq files (cons file files)))
                       (go done))))
             (if (not (keywordp file))
                 (setq files (cons file files)))
             (setq cur (cdr cur))
             (go top)
           done)
          (nreverse files)))


(defun bitcode-pathnames (start end &key system)
  (let ((sources (select-source-files start end :system system)))
    (mapcar #'(lambda (f &aux (fn (entry-filename f))) (build-pathname fn :bc)) sources)))

(defun print-bitcode-file-names-one-line (start end)
  (mapc #'(lambda (f) (bformat t " %s" (namestring f)))
        (bitcode-pathnames start end))
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
         (last-bitcode (build-pathname (entry-filename last-source) :bc))
         (image-file image))
    (format t "last-bitcode: ~a~%" last-bitcode)
    (format t "image-file: ~a~%" image-file)
    (if (probe-file image-file)
        (> (file-write-date last-bitcode)
           (file-write-date image-file))
        t)))

(defun out-of-date-target (target source-files)
  (let* ((last-source (car (reverse source-files)))
         (cxx-bitcode (build-intrinsics-bitcode-pathname :executable))
         (last-bitcode (build-pathname (entry-filename last-source) :bc))
         (target-file target))
    #+(or)(progn
            (format t "last-bitcode: ~a~%" last-bitcode)
            (format t "target-file: ~a~%" target-file))
    (if (probe-file target-file)
        (or (> (file-write-date last-bitcode)
               (file-write-date target-file))
            (> (file-write-date cxx-bitcode)
               (file-write-date target-file)))
        t)))

(defun compile-kernel-file (entry &key (reload nil) load-bitcode (force-recompile nil) counter total-files)
  #+dbg-print(bformat t "DBG-PRINT compile-kernel-file: %s\n" entry)
;;  (if *target-backend* nil (error "*target-backend* is undefined"))
  (let* ((filename (entry-filename entry))
         (source-path (build-pathname filename :lisp))
	 (bitcode-path (build-pathname filename :bc))
	 (load-bitcode (and (bitcode-exists-and-up-to-date filename) load-bitcode)))
    (if (and load-bitcode (not force-recompile))
	(progn
	  (bformat t "Skipping compilation of %s - its bitcode file %s is more recent\n" source-path bitcode-path)
	  ;;	  (bformat t "   Loading the compiled file: %s\n" (path-file-name bitcode-path))
	  ;;	  (load-bitcode (as-string bitcode-path))
	  )
	(progn
	  (bformat t "\n")
	  (if (and counter total-files)
              (bformat t "Compiling source [%d of %d] %s\n    to %s - will reload: %s\n" counter total-files source-path bitcode-path reload)
              (bformat t "Compiling source %s\n   to %s - will reload: %s\n" source-path bitcode-path reload))
	  (let ((cmp::*module-startup-prefix* "kernel"))
            #+dbg-print(bformat t "DBG-PRINT  source-path = %s\n" source-path)
            (apply #'compile-file (probe-file source-path) :output-file bitcode-path
                   #+build-print :print #+build-print t :verbose nil :output-type :bitcode :type :kernel (entry-compile-file-options entry))
	    (if reload
		(progn
		  (bformat t "    Loading newly compiled file: %s\n" bitcode-path)
		  (llvm-sys:load-bitcode bitcode-path))))))
    bitcode-path))
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
    (bformat t "Compiling/loading source: %s\n" (namestring name))
    (let ((m (cmp::compile-file-to-module name :print nil)))
      #+(or)
      (progn
        ;; Linking the intrinsics module is a bad idea - it includes way too much crap
        (cmp::link-intrinsics-module m)
        (cmp::optimize-module m))
      (llvm-sys:load-module m))))

(defun load-system (files &key compile-file-load interp load-bitcode (target-backend *target-backend*) system)
  #+dbg-print(bformat t "DBG-PRINT  load-system: %s - %s\n" first-file last-file )
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




(defun compile-system (files &key reload)
  #+dbg-print(bformat t "DBG-PRINT compile-system files: %s\n" files)
  (with-compilation-unit ()
    (let* ((cur files)
           (counter 1)
           (total (length files)))
      (tagbody
       top
         (if (endp cur) (go done))
         (compile-kernel-file (car cur) :reload reload :counter counter :total-files total )
         (setq cur (cdr cur))
         (setq counter (+ 1 counter))
         (go top)
       done))))
(export 'compile-system)

;; Clean out the bitcode files.
;; passing :no-prompt t will not prompt the user
(export 'clean-system)
(defun clean-system (after-file &key no-prompt stage system)
  (let* ((files (select-trailing-source-files after-file :system system))
	 (cur files))
    (bformat t "Will remove modules: %s\n" files)
    (bformat t "cur=%s\n" cur)
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
	  (bformat t "Not deleting\n")))))


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



(defun load-aclasp (&key clean
                      (output-file (build-common-lisp-bitcode-pathname))
                      (target-backend (default-target-backend))
                      (system (command-line-arguments-as-list)))
  (aclasp-features)
  (if clean (clean-system #P"src/lisp/kernel/tag/min-start" :no-prompt t :system system))
  (if (out-of-date-bitcodes #P"src/lisp/kernel/tag/min-start" #P"src/lisp/kernel/tag/min-end" :system system)
      (progn
        (load-system (select-source-files
                      #P"src/lisp/kernel/tag/after-init"
                      #P"src/lisp/kernel/tag/min-pre-epilogue" :system system)))))


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
        (load-system
         (butlast (select-source-files #P"src/lisp/kernel/tag/after-init" #P"src/lisp/kernel/tag/min-pre-epilogue" :system system)))
        (let* ((*target-backend* target-backend)
               (files (out-of-date-bitcodes #P"src/lisp/kernel/tag/min-start" #P"src/lisp/kernel/tag/min-pre-epilogue" :system system))
               (files-with-epilogue (out-of-date-bitcodes #P"src/lisp/kernel/tag/min-start" #P"src/lisp/kernel/tag/min-end" :system system)))
          (with-compilation-unit ()
            (compile-system files :reload t)
            (if files-with-epilogue (compile-system (bitcode-pathnames #P"src/lisp/kernel/tag/min-pre-epilogue" #P"src/lisp/kernel/tag/min-end" :system system) :reload nil)))
          (let ((all-bitcode (bitcode-pathnames #P"src/lisp/kernel/tag/min-start" #P"src/lisp/kernel/tag/min-end" :system system)))
            (if (out-of-date-target output-file all-bitcode)
                (cmp:link-bitcode-modules output-file all-bitcode)))))))

(export '(bclasp-features with-bclasp-features))
(defun bclasp-features()
  (remove-stage-features)
  (setq *features* (list* :clos :bclasp *features*))
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
          (load-system
           (butlast (select-source-files
                     #P"src/lisp/kernel/tag/start" #P"src/lisp/kernel/tag/bclasp" :system system)))
          (let ((files (out-of-date-bitcodes #P"src/lisp/kernel/tag/start" #P"src/lisp/kernel/tag/bclasp" :system system)))
            (compile-system files)
            (let ((all-bitcode (bitcode-pathnames #P"src/lisp/kernel/tag/start" #P"src/lisp/kernel/tag/bclasp" :system system)))
              (if (out-of-date-target output-file all-bitcode)
                    (cmp:link-bitcode-modules output-file all-bitcode))))))))

(export '(compile-cclasp recompile-cclasp))


(defun link-cclasp (&key (output-file (build-common-lisp-bitcode-pathname)) (system (command-line-arguments-as-list)))
  (let ((all-bitcode (bitcode-pathnames #P"src/lisp/kernel/tag/start" #P"src/lisp/kernel/tag/cclasp" :system system)))
    (cmp:link-bitcode-modules output-file all-bitcode)))

(defun compile-cclasp* (output-file system)
  "Turn off generation of inlining code until its turned back on by the source code.
Compile the cclasp source code."
  (let ((ensure-adjacent (select-source-files #P"src/lisp/kernel/cleavir/inline-prep" #P"src/lisp/kernel/cleavir/auto-compile" :system system)))
    (or (= (length ensure-adjacent) 2) (error "src/lisp/kernel/inline-prep MUST immediately preceed src/lisp/kernel/auto-compile - currently the order is: ~a" ensure-adjacent)))
  (let ((files (append (out-of-date-bitcodes #P"src/lisp/kernel/tag/start" #P"src/lisp/kernel/cleavir/inline-prep" :system system)
                       (select-source-files #P"src/lisp/kernel/cleavir/auto-compile"
                                            #P"src/lisp/kernel/tag/cclasp"
                                            :system system))))
    (format t "files: ~a~%" files)
    (setf core:*defun-inline-hook* nil)
    (compile-system files :reload nil)
    (let ((all-bitcode (bitcode-pathnames #P"src/lisp/kernel/tag/start" #P"src/lisp/kernel/tag/cclasp" :system system)))
      (if (out-of-date-target output-file all-bitcode)
          (cmp:link-bitcode-modules output-file all-bitcode)))))
  
(defun recompile-cclasp (&key clean (output-file (build-common-lisp-bitcode-pathname)) (system (command-line-arguments-as-list)))
  (if clean (clean-system #P"src/lisp/kernel/tag/start" :no-prompt t))
  (compile-cclasp* output-file system))

(defun load-cclasp (&key (system (command-line-arguments-as-list)))
  (load-system (select-source-files #P"src/lisp/kernel/tag/bclasp" #P"src/lisp/kernel/cleavir/inline-prep" :system system) :compile-file-load t)
  (load-system (select-source-files #P"src/lisp/kernel/cleavir/auto-compile" #P"src/lisp/kernel/tag/cclasp" :system system) :compile-file-load nil ))
(export '(load-cclasp))

(defun compile-cclasp (&key clean (output-file (build-common-lisp-bitcode-pathname)) (system (command-line-arguments-as-list)))
  (cclasp-features)
  (if clean (clean-system #P"src/lisp/kernel/tag/start" :no-prompt t :system system))
  (let ((*target-backend* (default-target-backend)))
    (time
     (progn
       (progn ;; Use load-cclasp?
         (load-system (select-source-files #P"src/lisp/kernel/tag/bclasp" #P"src/lisp/kernel/cleavir/inline-prep" :system system) :compile-file-load t )
         (load-system (select-source-files #P"src/lisp/kernel/cleavir/auto-compile" #P"src/lisp/kernel/tag/pre-epilogue-cclasp" :system system) :compile-file-load nil ))
       (compile-cclasp* output-file system)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Setup the build system for ASDF
;;
;;

(defun compile-addons ()
  ;; Build serve-event and asdf
  (core:compile-kernel-file #P"src/lisp/modules/serve-event/serve-event" :force-recompile t)
  (core:compile-kernel-file #P"src/lisp/modules/asdf/build/asdf" :force-recompile t))

(defun link-addons ()
  (cmp:llvm-link (core:build-pathname #P"src/lisp/modules/serve-event/serve-event" :fasl)
                 :lisp-bitcode-files (list (core:build-pathname #P"src/lisp/modules/serve-event/serve-event" :bc)))
  (cmp:llvm-link (core:build-pathname #P"src/lisp/modules/asdf/asdf" :fasl)
                 :lisp-bitcode-files (list (core:build-pathname #P"src/lisp/modules/asdf/build/asdf" :bc))))
(export '(compile-addons link-addons))

(eval-when (:execute)
  (bformat t "Loaded clasp-builder.lsp\n")
  (if (member :clasp-builder-repl *features*)
      (core:low-level-repl)))
