(in-package #:koga)

;;;; Generally short Lisp scripts that make the command lines that Ninja uses to
;;;; execute rules simpler. These scripts do things like setup ASDF, load specific
;;;; systems and accept input and output files via the command line arguments
;;;; accessible via UIOP:COMMAND-LINE-ARGUMENTS or in iclasp as
;;;; CORE:*COMMAND-LINE-ARGUMENTS*. This is done to avoid shell escaping issues
;;;; that would present when using `--eval` flags.

(defun print-asdf-stub (output-stream host &rest systems)
  "Print the commands to initialize ASDF and load required systems."
  (format output-stream "#-asdf (require :asdf)~%")
  (pprint `(asdf:initialize-source-registry
             (list :source-registry
                (list :tree (merge-pathnames ,(root :code) (uiop:getcwd)))
                :inherit-configuration))
           output-stream)
  (pprint `(asdf:initialize-output-translations
             (list :output-translations
                (list t (list (merge-pathnames ,(if host
                                                    (make-pathname :directory '(:relative "host-fasl"))
                                                    (root :variant-lib))
                                               (uiop:getcwd))
                              :implementation))
                :inherit-configuration))
           output-stream)
  (loop for system in systems
        do (pprint `(asdf:load-system ,system) output-stream)))

(defmethod print-prologue (configuration (name (eql :update-unicode)) output-stream)
  (declare (ignore configuration))
  (print-asdf-stub output-stream t :unicode-data)
  (pprint '(apply #'uiop:symbol-call "UNICODE-DATA" "GENERATE" (uiop:command-line-arguments)) output-stream))

(defmethod print-prologue (configuration (name (eql :generate-sif)) output-stream)
  (declare (ignore configuration))
  (print-asdf-stub output-stream t :clasp-scraper)
  (pprint '(apply #'uiop:symbol-call "CSCRAPE" "GENERATE-SIF" (uiop:command-line-arguments)) output-stream))

(defmethod print-prologue (configuration (name (eql :generate-headers)) output-stream)
  (declare (ignore configuration))
  (print-asdf-stub output-stream t :clasp-scraper)
  (with-standard-io-syntax (pprint `(destructuring-bind (cl-user::precise cl-user::variant-path &rest cl-user::args)
               (uiop:command-line-arguments)
             (apply #'uiop:symbol-call "CSCRAPE" "GENERATE-HEADERS"
                    (equal "1" cl-user::precise)
                    cl-user::variant-path
                    (make-pathname :directory '(:relative :up))
                    ,(if (reproducible-build configuration)
                         (root :install-share)
                         '(truename (make-pathname :directory '(:relative :up))))
                    cl-user::args))
           output-stream)))

(defmethod print-prologue (configuration (name (eql :compile-module)) output-stream)
  (format output-stream
          "(compile-file (elt core:*command-line-arguments* 1)
              :source-debug-pathname (elt core:*command-line-arguments* 1)
              :output-file (elt core:*command-line-arguments* 0)
              :output-type ~s)"
          (case (build-mode configuration)
            (:faso :fasp)
            (:fasoll :faspll)
            (:fasobc :faspbc)
            (otherwise :fasl))))

(defmethod print-prologue (configuration (name (eql :compile-aclasp)) output-stream)
  (format output-stream "(setq *features* (cons :aclasp *features*))
(load #P\"sys:src;lisp;kernel;clasp-builder.lisp\")
(setq core::*number-of-jobs* ~a)
(core:compile-aclasp :reproducible ~s)
(core:quit)" (jobs configuration) (reproducible-build configuration)))

(defmethod print-prologue (configuration (name (eql :run-aclasp)) output-stream)
  (format output-stream "(load #P\"sys:src;lisp;kernel;clasp-builder.lisp\")
(setq core::*number-of-jobs* ~a)
(core:load-aclasp :reproducible ~s)" (jobs configuration) (reproducible-build configuration)))

(defmethod print-prologue (configuration (name (eql :compile-bclasp)) output-stream)
  (format output-stream "(setq *features* (cons :bclasp *features*))
(load #P\"sys:src;lisp;kernel;clasp-builder.lisp\")
(setq core::*number-of-jobs* ~a)
(core:compile-bclasp :reproducible ~s)
(core:quit)" (jobs configuration) (reproducible-build configuration)))

(defmethod print-prologue (configuration (name (eql :compile-cclasp)) output-stream)
  (format output-stream "(setq *features* (cons :cclasp *features*))
(load #P\"sys:src;lisp;kernel;clasp-builder.lisp\")
(setq core::*number-of-jobs* ~a)
(core:compile-cclasp :reproducible ~s)
(core:quit)" (jobs configuration) (reproducible-build configuration)))

(defmethod print-prologue (configuration (name (eql :compile-eclasp)) output-stream)
  (format output-stream "(setq *features* (cons :eclasp *features*))
(load #P\"sys:src;lisp;kernel;clasp-builder.lisp\")
(setq core::*number-of-jobs* ~a)
(core:compile-eclasp :reproducible ~s)
(core:quit)" (jobs configuration) (reproducible-build configuration)))

(defmethod print-prologue (configuration (name (eql :link-fasl)) output-stream)
  (write-string "(setq *features* (cons :aclasp *features*))
(load #P\"sys:src;lisp;kernel;clasp-builder.lisp\")
(load #P\"sys:src;lisp;kernel;cmp;jit-setup.lisp\")
(let ((args (core:command-line-arguments-as-list)))
  (core:link-fasl :output-file (car args)
                  :system (cdr args)))
(core:quit)" output-stream))

;;; TODO The parallel analyzer is disabled below. Enable it once it works.
(defmethod print-prologue (configuration (name (eql :static-analyzer)) output-stream)
  (declare (ignore configuration))
  (print-asdf-stub output-stream nil :clasp-analyzer)
  (let ((log-path (resolve-source (make-source "analyzer-logs/" :variant))))
    (format output-stream "
(setq core::*number-of-jobs* ~a)
(uiop:delete-directory-tree ~s :validate t :if-does-not-exist :ignore)
(clasp-analyzer:search-and-generate-code (pathname (elt core:*command-line-arguments* 0))
                                         (pathname (elt core:*command-line-arguments* 1))
                                         :log-path ~s :parallel ~s)"
            (jobs configuration) log-path log-path nil #+(or)(parallel-build configuration))))

(defmethod print-prologue (configuration (name (eql :snapshot)) output-stream)
  (when (jupyter configuration)
    (format output-stream "#+quicklisp (ql:quickload ~:[~; #-ignore-extensions :cando-jupyter #+ignore-extensions~] :common-lisp-jupyter)
#-quicklisp (asdf:load-system ~:[~; #-ignore-extensions :cando-jupyter #+ignore-extensions~] :common-lisp-jupyter)"
            (member :cando (extensions configuration))
            (member :cando (extensions configuration))))
  (format output-stream "(clos:compile-all-generic-functions)
(gctools:save-lisp-and-die (elt core:*command-line-arguments* 0) :executable t)
(core:quit)"
          (member :cando (extensions configuration))))

(defmethod print-prologue (configuration (name (eql :clasp-sh)) output-stream)
  (declare (ignore configuration))
  (format output-stream "#!/usr/bin/env bash
exec $(dirname \"$0\")/~a -f ignore-extensions -t c \"$@\""
          (build-name :iclasp)))

(defmethod print-prologue (configuration (name (eql :jupyter-kernel)) output-stream)
  (let ((candop (member :cando (extensions configuration))))
    (format output-stream "(let ((name (first (uiop:command-line-arguments)))
      (bin-path (second (uiop:command-line-arguments)))
      (load-system (equal \"1\" (third (uiop:command-line-arguments))))
      (system (equal \"1\" (fourth (uiop:command-line-arguments)))))
  (when load-system
    #+quicklisp (ql:quickload ~:[~; #-ignore-extensions :cando-jupyter #+ignore-extensions~] :common-lisp-jupyter)
    #-quicklisp (asdf:load-system ~:[~; #-ignore-extensions :cando-jupyter #+ignore-extensions~] :common-lisp-jupyter))
  (uiop/package:symbol-call ~:[~;#-ignore-extensions \"CANDO-JUPYTER\" #+ignore-extensions ~]\"CL-JUPYTER\" \"INSTALL\"
    :system system :local ~s :implementation name
    :bin-path (if system
                  bin-path
                  (merge-pathnames bin-path (uiop:getcwd)))
    :prefix (when system ~s) :jupyter (when system ~s) :load-system load-system))
(sys:quit)"
            candop
            candop
            candop
            (equal (bin-path configuration) #P"/usr/local/bin/")
            (package-path configuration)
            (jupyter-path configuration))))

(defmethod print-prologue (configuration (name (eql :clasprc)) output-stream)
  (declare (ignore configuration))
  (print-asdf-stub output-stream nil)
  (format output-stream "~%(load #P\"sys:quicklisp;setup.lisp\")
(unless (ql-dist:find-dist \"quickclasp\")
  (sleep 2) ; ensure that the sequence number if quickclasp is higher
  (ql-dist:install-dist \"http://thirdlaw.tech/quickclasp/quickclasp.txt\" :prompt nil))"))

(defun pprint-immutable-systems (stream object)
  (format stream "(in-package \"SYSTEM\")~%~%(defparameter *immutable-systems*~%")
  (pprint-logical-block (stream (sort (copy-seq object)
                                      (lambda (x y)
                                        (string-lessp (car x) (car y))))
                         :prefix "  '(" :suffix ")")
    (loop do (pprint-logical-block (stream (pprint-pop) :prefix "(" :suffix ")")
               (write (pprint-pop) :case :downcase :stream stream)
               (loop do (pprint-exit-if-list-exhausted)
                        (pprint-newline :mandatory stream)
                        (write (pprint-pop) :case :downcase :stream stream)
                        (pprint-exit-if-list-exhausted)
                        (write-char #\Space stream)
                        (write (pprint-pop) :stream stream)))
              (pprint-exit-if-list-exhausted)
              (pprint-newline :mandatory stream)))
  (write-line ")" stream))

(defun print-translations (output-stream targets sources)
  (format output-stream "(in-package \"SYSTEM\")~%
(let ((sys (translate-logical-pathname \"SYS:\"))
      (lib (translate-logical-pathname \"SYS:LIB;\"))
      (generated (translate-logical-pathname \"SYS:GENERATED;\")))
  (declare (ignorable sys lib generated))")
  (flet ((print-translation (translation root
                             &aux (prefix (root-to-prefix root)))
           (format output-stream "~%  (push (list ~s~%              (merge-pathnames ~s ~a))~%        (logical-pathname-translations \"SYS\"))"
                   (ninja:make-logical-pathname-representation "SYS" translation
                                                               :prefix prefix
                                                               :version :wild)
                   translation (or prefix "sys"))))
    (loop with translations = (make-hash-table :test #'equal)
          for source in sources
          for translation = (ninja:find-minimal-pathname-translation (source-path source))
          when (and translation
                    (not (member (source-root source) '(:code :variant-generated))))
            do (error "Found needed logical pathname translation for root ~s of ~s"
                      (source-root source)
                      translation)
          unless (or (null translation)
                     (gethash translation translations))
            do (setf (gethash translation translations) t)
               (print-translation translation :code)
               (loop for target in targets
                     do (print-translation (make-pathname :directory (list* :relative
                                                                            (format nil "~(~a~)-~a-bitcode" target *variant-name*)
                                                                            (cdr (pathname-directory translation)))
                                                          :name (pathname-name translation)
                                                          :type (pathname-type translation))
                                           :variant-lib))))
  (format output-stream ")~%"))

(defmethod print-variant-target-sources
    (configuration (name (eql :cclasp-translations)) output-stream
     (target (eql :cclasp)) sources
     &key &allow-other-keys)
  (print-translations output-stream (if (extensions configuration)
                                      '(:cclasp :eclasp)
                                      '(:cclasp))
                                    sources))

(defmethod print-variant-target-sources
    (configuration (name (eql :eclasp-translations)) output-stream
     (target (eql :eclasp-translations)) sources
     &key &allow-other-keys)
  (print-translations output-stream '(:eclasp) sources))

(defmethod print-prologue (configuration (name (eql :cclasp-immutable)) output-stream)
  (pprint-immutable-systems output-stream (gethash :cclasp (target-systems configuration))))

(defmethod print-prologue (configuration (name (eql :eclasp-immutable)) output-stream)
  (pprint-immutable-systems output-stream (gethash :eclasp (target-systems configuration))))

(defmethod print-prologue (configuration (name (eql :bench)) output-stream)
  (format output-stream "(require :asdf)
(asdf:load-system :cl-bench)
(asdf:load-system :cl-bench/report)
(asdf:load-system :split-sequence)
(loop with bench = (uiop:getenv \"CLASP_BENCH\")
      with negate = (and (position #\\^ bench :test #'char=) t)
      with names = (split-sequence:split-sequence-if (lambda (ch)
                                                       (position ch \",^\" :test #'char=))
                                                     bench
                                                     :remove-empty-subseqs t)
      for benchmark in cl-bench::*benchmarks*
      for name = (cl-bench::benchmark-name benchmark)
      for namesp = (find name names :test #'string-equal)
      when (or (and negate namesp)
               (and (not negate) names (not namesp)))
        do (pushnew :clasp (cl-bench::benchmark-disabled-for benchmark)))
(let ((cl-bench::*output-dir* (make-pathname :directory '(:relative :up \"bench\"))))
  (ensure-directories-exist cl-bench::*output-dir*)
  (cl-bench:bench-run)
  (cl-bench::bench-analysis-page))
(ext:quit)"))

