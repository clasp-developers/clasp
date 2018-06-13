(in-package #:clasp-sandbox)

(defun build-target-dir (type stage gc)
  (format nil "~a~a-~a" stage (lisp-implementation-type) gc))

(defun build-extension (type)
  (ecase type
    ((:fasl) "fasl")
    ((:bitcode) "bc")
    ((:ll) "ll")))

(defun strip-root (pn-dir)
  "Remove the SOURCE-DIR: part of the path in l and then
search for the string 'src', or 'generated' and return the rest of the list that starts with that"
  (let ((rel (cdr (pathname-directory (enough-namestring (make-pathname :directory pn-dir)
                                                         (translate-logical-pathname #P"SOURCE-DIR:"))))))
    (or (member "src" rel :test #'string=)
        (member "generated" rel :test #'string=)
        (error "Could not find \"src\" or \"generated\" in ~a" rel))))

(defun ensure-relative-pathname (input)
  "If the input pathname is absolute then search for src, or generated and return
a relative path from there."
  #+(or)(bformat t "ensure-relative-pathname input = %s   sys-pn = %s%N" input sys-pn)
  (cond
    ((eq :relative (car (pathname-directory input)))
     (make-pathname :directory (pathname-directory input)
                    :name (pathname-name input)))
    ((eq :absolute (car (pathname-directory input)))
     (make-pathname :directory (cons :relative (strip-root (pathname-directory input)))
                    :name (pathname-name input)))
    (t (error "ensure-relative-pathname could not handle ~a" input))))

(defun build-pathname (partial-pathname &key (type :lisp) (stage "s") (gc "boehm"))
  "If partial-pathname is nil and type is :fasl or :executable then construct the name using
the stage, the +application-name+ and the +bitcode-name+"
  (flet ((find-lisp-source (module root)
           (or
            (probe-file (merge-pathnames (merge-pathnames module (make-pathname :type "lsp")) root))
            (probe-file (merge-pathnames (merge-pathnames module (make-pathname :type "lisp")) root))
            (error "Could not find a lisp source file with root: ~a module: ~a" root module))))
    (let ((target-host "lib")
          (target-dir (build-target-dir type stage gc)))
      (cond
        ((eq type :lisp)
         (let ((module (ensure-relative-pathname partial-pathname)))
           (cond
             ((string= "generated" (second (pathname-directory module)))
              ;; Strip the "generated" part of the directory
              (find-lisp-source (make-pathname
                                 :directory (cons :relative (cddr (pathname-directory module)))
                                 :name (pathname-name module))
                                (translate-logical-pathname "GENERATED:")))
             (t
              (find-lisp-source module (translate-logical-pathname "SOURCE-DIR:"))))))
        ((and partial-pathname (or (eq type :fasl) (eq type :bitcode)))
         (merge-pathnames (merge-pathnames (ensure-relative-pathname partial-pathname)
                                           (make-pathname :directory (list :relative target-dir)
                                                          :type (build-extension type)))
                          (translate-logical-pathname (make-pathname :host target-host))))
        ((and (null partial-pathname) (eq type :fasl))
         (let ((filename (format nil "~a~a-~a-image" stage (lisp-implementation-type) gc)))
           (merge-pathnames (make-pathname :name filename :type "fasl")
                            (translate-logical-pathname "app-fasl:"))))
        ((eq type :executable)
         (let ((filename (format nil "~a~a-~a" stage (lisp-implementation-type) gc)))
           (merge-pathnames (make-pathname :name filename :type nil)
                            (translate-logical-pathname "app-executable:"))))
        (t (error "Add support for build-pathname type: ~a" type))))))

(defun entry-filename (entry)
  ;; Entries are either filenames or lists where the first element is a filename.
  (if (consp entry) (car entry) entry))
(defun entry-compile-file-options (entry)
  (if (consp entry) (second entry) nil))

(defun compile-kernel-file (entry &key counter environment total-files (output-type :bitcode))
  (let* ((filename (entry-filename entry))
         (source-path (build-pathname filename :type :lisp))
         (output-path (build-pathname filename :type output-type)))
    (if (and counter total-files)
        (format t "Compiling source [~d of ~d] ~a~%   to ~a~%" counter total-files source-path output-path)
        (format t "Compiling source ~a~%   to ~a~%" source-path output-path))
    (apply #'compile-file (probe-file source-path) :output-file output-path :environment environment
                                                   :output-type output-type :print t :verbose nil :type :kernel
                                                   (entry-compile-file-options entry))))

(defun bitcode-exists-and-up-to-date (entry)
  (let* ((filename (entry-filename entry))
         (source-path (build-pathname filename))
         (bitcode-path (build-pathname filename :type :bitcode))
         (found-bitcode (probe-file bitcode-path)))
    (if found-bitcode
        (> (file-write-date bitcode-path)
           (file-write-date source-path))
        nil)))

(defun out-of-date-bitcodes (start end &key system)
  (let ((sources (select-source-files start end :system system))
        out-of-dates)
    (mapc #'(lambda (f)
              (cond
                ;; if one thing is out of date, everything after it is as well
                (out-of-dates (setq out-of-dates (list* f out-of-dates)))
                ((not (bitcode-exists-and-up-to-date f))
                 (setq out-of-dates (cons f nil)))))
          sources)
    (nreverse out-of-dates)))

(defun select-source-files (first-file last-file &key system)
  (unless first-file (error "You must provide first-file to select-source-files"))
  (unless system (error "You must provide system to select-source-files"))
  (let ((cur (member first-file system :test #'equal)))
    (when last-file
      (unless (member last-file system :test #'equal)
        (error "last-file ~a was not a member of ~a" last-file system)))
    (unless cur (error "first-file ~a was not a member of ~a" first-file system))
    (loop for file in cur
          unless (keywordp file)
            collect file
          until (and last-file (eq cur last-file)))))

(defun compile-system (files environment &key (output-type :bitcode))
  (with-compilation-unit ()
    (loop with total = (length files)
          for cur in files
          for counter from 1
          do (compile-kernel-file cur :output-type output-type
                                      :environment environment
                                      :counter counter :total-files total))))


(defun compile-sclasp (environment system)
  "Compile the clasp source code."
  (let ((files (append (out-of-date-bitcodes #P"src/lisp/kernel/tag/start"
                                             #P"src/lisp/kernel/cleavir/inline-prep" :system system)
                       (select-source-files #P"src/lisp/kernel/cleavir/auto-compile"
                                            #P"src/lisp/kernel/tag/cclasp"
                                            :system system))))
    (format t "files: ~a~%" files)
    (compile-system files environment)
    (values)))

(defun bitcode-pathnames (start end &key system)
  (let ((sources (select-source-files start end :system system)))
    (mapcar #'(lambda (f &aux (fn (entry-filename f))) (build-pathname fn :type :bitcode)) sources)))

(defun link-sclasp (system &key (stage "s") (gc "boehm"))
  (let ((output-file
          (translate-logical-pathname (pathname (format nil "lib:~aclasp-~a-common-lisp.bc" stage gc))))
        (all-bitcode
          (bitcode-pathnames #P"src/lisp/kernel/tag/start" #P"src/lisp/kernel/tag/cclasp" :system system)))
    (cmp:link-bitcode-modules output-file all-bitcode)))
