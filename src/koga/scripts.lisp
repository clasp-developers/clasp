(in-package #:koga)

;;;; Generally short Lisp scripts that make the command lines that Ninja uses to
;;;; execute rules simpler. These scripts do things like setup ASDF, load specific
;;;; systems and accept input and output files via the command line arguments
;;;; accessible via UIOP:COMMAND-LINE-ARGUMENTS or in iclasp as
;;;; CORE:*COMMAND-LINE-ARGUMENTS*. This is done to avoid shell escaping issues
;;;; that would present when using `--eval` flags.

(defun print-asdf-stub (output-stream host &rest systems)
  "Print the commands to initialize ASDF and load required systems."
  (pprint '(require :asdf) output-stream)
  (pprint `(asdf:initialize-source-registry
             (list :source-registry
                (list :tree (merge-pathnames ,*code-path* (uiop:getcwd)))
                :inherit-configuration))
           output-stream)
  (pprint `(asdf:initialize-output-translations
             (list :output-translations
                (list t (list (merge-pathnames ,(if host
                                                    (make-pathname :directory '(:relative "host-fasl"))
                                                    (merge-pathnames (make-pathname :directory '(:relative "fasl"))
                                                                     *variant-path*))
                                               (uiop:getcwd))
                              :implementation))
                :inherit-configuration))
           output-stream)
  (loop for system in systems
        do (pprint `(asdf:load-system ,system) output-stream)))

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
                    ,*code-path*
                    cl-user::args))
           output-stream)))

(defmethod print-prologue (configuration (name (eql :compile-module)) output-stream)
  (format output-stream
          "(compile-file (merge-pathnames (elt core:*command-line-arguments* 1)
                               (ext:getcwd))
              :output-file (merge-pathnames (elt core:*command-line-arguments* 0)
                                            (ext:getcwd))
              :output-type ~s)"
          (case (build-mode configuration)
            (:faso :fasp)
            (:fasoll :faspll)
            (:fasobc :faspbc)
            (otherwise :fasl))))

(defmethod print-prologue (configuration (name (eql :compile-aclasp)) output-stream)
  (format output-stream "(setq *features* (cons :aclasp *features*))
(load #P\"sys:kernel;clasp-builder.lisp\")
(setq core::*number-of-jobs* ~a)
(core:compile-aclasp)
(core:quit)" (jobs configuration)))

(defmethod print-prologue (configuration (name (eql :compile-bclasp)) output-stream)
  (format output-stream "(setq *features* (cons :bclasp *features*))
(load #P\"sys:kernel;clasp-builder.lisp\")
(setq core::*number-of-jobs* ~a)
(core:compile-bclasp)
(core:quit)" (jobs configuration)))

(defmethod print-prologue (configuration (name (eql :compile-cclasp)) output-stream)
  (format output-stream "(setq *features* (cons :cclasp *features*))
(load #P\"sys:kernel;clasp-builder.lisp\")
(setq core::*number-of-jobs* ~a)
(core:compile-cclasp)
(core:quit)" (jobs configuration)))

(defmethod print-prologue (configuration (name (eql :link-fasl)) output-stream)
  (write-string "(setq *features* (cons :aclasp *features*))
(load #P\"sys:kernel;clasp-builder.lisp\")
(load #P\"sys:kernel;cmp;jit-setup.lisp\")
(let ((args (core:command-line-arguments-as-list)))
  (core:link-fasl :output-file (car args)
                  :system (cdr args)))
(core:quit)" output-stream))

(defmethod print-prologue (configuration (name (eql :static-analyzer)) output-stream)
  (declare (ignore configuration))
  (print-asdf-stub output-stream nil :clasp-analyzer)
  (format output-stream "~%(clasp-analyzer::serial-search/generate-code
  (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database
    (pathname (elt core:*command-line-arguments* 1)))
 :output-file (pathname (elt core:*command-line-arguments* 0)))"))

(defmethod print-prologue (configuration (name (eql :snapshot)) output-stream)
  (declare (ignore configuration))
  (when (member :cando (extensions configuration))
    (when (jupyter configuration)
      (write-line "#-ignore-extensions (ql:quickload :cando-jupyter)" output-stream))
    (write-line "#-ignore-extensions (setf ext:*snapshot-save-load-startup* 'cl-user:start-cando-user-from-snapshot)"
                output-stream))
  (write-line "(clos:compile-all-generic-functions)
(gctools:save-lisp-and-die (elt core:*command-line-arguments* 0))
(core:quit)" output-stream))

(defmethod print-prologue (configuration (name (eql :clasp-sh)) output-stream)
  (declare (ignore configuration))
  (format output-stream "#!/usr/bin/env bash
CLASP_FEATURES=ignore-extensions exec ~a \"$@\""
          (build-name :iclasp)))
