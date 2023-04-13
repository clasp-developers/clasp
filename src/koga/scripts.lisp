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
  (when host
    (pprint `(asdf:initialize-source-registry
               (list :source-registry
                     ,(when host '(list :also-exclude "asdf"))
                     (list :tree (merge-pathnames ,(root :code) (uiop:getcwd)))
                     :inherit-configuration))
            output-stream))
  (loop for system in systems
        do (pprint `(asdf:load-system ,system) output-stream)))

(defmethod print-prologue (configuration (name (eql :generate-vm-header)) output-stream)
  (declare (ignore configuration))
  (print-asdf-stub output-stream t :ninja)
  (format output-stream "(map nil #'load (cdr (uiop:command-line-arguments)))
(ninja:with-timestamp-preserving-stream (stream (first (uiop:command-line-arguments)))
  (cmpref:generate-virtual-machine-header stream))"))

(defmethod print-prologue (configuration (name (eql :update-unicode)) output-stream)
  (declare (ignore configuration))
  (print-asdf-stub output-stream t :unicode-data)
  (pprint '(apply #'uiop:symbol-call "UNICODE-DATA" "GENERATE" (uiop:command-line-arguments)) output-stream))

(defmethod print-prologue (configuration (name (eql :generate-sif)) output-stream)
  (declare (ignore configuration))
  (print-asdf-stub output-stream t :clasp-scraper)
  (pprint '(apply #'uiop:symbol-call "CSCRAPE" "GENERATE-SIF" (uiop:command-line-arguments)) output-stream))

(defmethod print-prologue (configuration (name (eql :compile-systems)) output-stream)
  (declare (ignore configuration))
  (format output-stream "#-asdf (require :asdf)~%
(loop for system across core:*command-line-arguments*
      for index from 0
      unless (zerop index)
        do (asdf:compile-system system))
(with-open-file (stream (elt core:*command-line-arguments* 0)
                 :direction :output :if-exists :supersede :if-does-not-exist :create)
  (pprint core:*command-line-arguments* stream))"))

(defmethod print-variant-target-sources
    (configuration (name (eql :generate-headers)) output-stream
     (target (eql :scraper)) sources
     &key &allow-other-keys)
  (declare (ignore configuration))
  (print-asdf-stub output-stream t :clasp-scraper)
  (with-standard-io-syntax
    (pprint `(destructuring-bind (cl-user::precise &rest cl-user::args)
               (uiop:command-line-arguments)
             (apply #'uiop:symbol-call "CSCRAPE" "GENERATE-HEADERS"
                    (equal "1" cl-user::precise)
                    ',(loop for source in sources
                            collect (intern (string-upcase (substitute #\_ #\- (substitute #\_ #\. (file-namestring (source-path source))))) 'keyword)
                            collect (resolve-source source))
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
            (:bytecode :faslbc)
            (:faso :fasp)
            (:fasoll :faspll)
            (:fasobc :faspbc)
            (otherwise :fasl))))

(defmethod print-prologue (configuration (name (eql :load-clasp)) output-stream)
  (format output-stream "(load #P\"sys:src;lisp;kernel;clasp-builder.lisp\")
(setq core::*number-of-jobs* ~a)
(defvar *system* (core:load-clasp :reproducible ~s
                                  :name (elt core:*command-line-arguments* 0)
                                  :position (parse-integer (elt core:*command-line-arguments* 1))
                                  :system (core:command-line-paths 2)))
(if (fboundp 'core:top-level)
    (core:top-level)
    (core:low-level-repl))" (jobs configuration) (reproducible-build configuration)))

(defmethod print-prologue (configuration (name (eql :snapshot-clasp)) output-stream)
  (format output-stream "(load #P\"sys:src;lisp;kernel;clasp-builder.lisp\")
(setq core::*number-of-jobs* ~a)
(defvar *system* (core:load-clasp :reproducible ~s
                                  :name (elt core:*command-line-arguments* 1)
                                  :position (parse-integer (elt core:*command-line-arguments* 2))
                                  :system (core:command-line-paths 3)))
(gctools:save-lisp-and-die (elt core:*command-line-arguments* 0) :executable t)
(core:quit)" (jobs configuration) (reproducible-build configuration)))

(defmethod print-prologue (configuration (name (eql :compile-clasp)) output-stream)
  (format output-stream "(load #P\"sys:src;lisp;kernel;clasp-builder.lisp\")
(setq core::*number-of-jobs* ~a)
(core:load-and-compile-clasp :reproducible ~s :system-sort ~s
                             :name (elt core:*command-line-arguments* 0)
                             :position (parse-integer (elt core:*command-line-arguments* 1))
                             :system (core:command-line-paths 2))
(core:quit)"
          (jobs configuration) (reproducible-build configuration)
          (and (> (jobs configuration) 1) (parallel-build configuration))))

(defmethod print-prologue (configuration (name (eql :link-fasl)) output-stream)
  (write-string "(setq *features* (cons :aclasp *features*))
(load #P\"sys:src;lisp;kernel;clasp-builder.lisp\")
(load #P\"sys:src;lisp;kernel;cmp;jit-setup.lisp\")
(core:link-fasl :output-file (pathname (elt core:*command-line-arguments* 0))
                :system (core:command-line-paths 1))
(core:quit)" output-stream))

(defmethod print-prologue (configuration (name (eql :analyze-generate)) output-stream)
  (declare (ignore configuration))
  (print-asdf-stub output-stream nil :clasp-analyzer)
  (format output-stream "
(clasp-analyzer:merge-and-generate-code (pathname (elt core:*command-line-arguments* 0))
                                        (core::command-line-paths 1))"))

(defmethod print-prologue (configuration (name (eql :analyze-file)) output-stream)
  (declare (ignore configuration))
  (print-asdf-stub output-stream nil :clasp-analyzer)
  (format output-stream "
(handler-case
    (clasp-analyzer:search-source-file (pathname (elt core:*command-line-arguments* 0))
                                       (elt core:*command-line-arguments* 1)
                                       (pathname (elt core:*command-line-arguments* 2))
                                       (pathname (elt core:*command-line-arguments* 3)))
  (error (condition)
    (format *error-output* \"~~a~~%\" condition)
    (sys:exit 1)))"))

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
exec $(dirname \"$0\")/iclasp -f ignore-extensions --base \"$@\""))

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

(defun pprint-immutable-systems (stream object &aux (*print-pretty* t))
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

(defun print-translations (output-stream sources)
  (format output-stream "(in-package \"SYSTEM\")~%
(let ((sys (translate-logical-pathname \"SYS:\"))
      (lib (translate-logical-pathname \"SYS:LIB;\"))
      (generated (translate-logical-pathname \"SYS:GENERATED;\")))
  (declare (ignorable sys lib generated))
  (setf (logical-pathname-translations \"SYS\")
        (list* ")
  (flet ((print-translation (translation root
                             &aux (prefix (root-to-prefix root)))
           (format output-stream "(list ~s
                      (merge-pathnames ~s ~a))
               "
                   (ninja:make-logical-pathname-representation "SYS" translation
                                                               :prefix prefix
                                                               :version :wild)
                   translation (or prefix "sys"))))
    (loop with translations = (make-hash-table :test #'equal)
          for source in (reverse sources)
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
               (print-translation (make-pathname :directory (list* :relative
                                                                            ;(format nil "~(~a~)-~a-bitcode" target *variant-name*)
                                                                            (cdr (pathname-directory translation)))
                                                          :name (pathname-name translation)
                                                          :type (pathname-type translation))
                                           :variant-lib)))
  (format output-stream "(logical-pathname-translations \"SYS\"))))~%"))

(defmethod print-variant-target-sources
    (configuration (name (eql :base-translations)) output-stream
     (target (eql :cclasp)) sources
     &key &allow-other-keys)
  (print-translations output-stream sources))

(defmethod print-variant-target-sources
    (configuration (name (eql :extension-translations)) output-stream
     (target (eql :extension-translations)) sources
     &key &allow-other-keys)
  (print-translations output-stream sources))

(defmethod print-prologue (configuration (name (eql :base-immutable)) output-stream)
  (pprint-immutable-systems output-stream (gethash :cclasp (target-systems configuration))))

(defmethod print-prologue (configuration (name (eql :extension-immutable)) output-stream)
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

(defmethod print-prologue (configuration (name (eql :ansi-test-subset)) output-stream)
  (format output-stream "(setf *default-pathname-defaults*
      (truename (make-pathname :directory '(:relative :up \"dependencies\" \"ansi-test\"))))
(load #P\"doit1.lsp\")
(load #P\"gclload2.lsp\")
(dolist (name (core:split (string-upcase (ext:getenv \"ANSI_TEST_SUBSET\")) \",\"))
  (write-line name)
  (rt:do-test (find-symbol name :cl-test) :catch-errors nil))
(ext:quit)"))
