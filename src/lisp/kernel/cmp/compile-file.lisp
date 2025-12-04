(in-package #:cmp)

;;;; Top level interface: COMPILE-FILE, etc.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Describing top level forms (for compile-print)

(defun describe-form (form)
  ;; We could be smarter about this. For example, for (progn ...) nothing very interesting
  ;; will print, even though subforms are just as toplevel.
  ;; But it's just aesthetic, so cheaping out a little is okay.
  (fresh-line)
  (write-string ";   ")
  (write form :length 2 :level 2 :lines 1 :pretty nil)
  (terpri)
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile-file pathnames

(defun build-extension (type)
  (cond ((or (eq type :bytecode)
             (member :bytecode *features*))
         "fasl")
        ((eq type :faso)
         "faso")
        ((eq type :fasoll)
         "fasoll")
        ((eq type :fasobc)
         "fasobc")
        (t
         (error "Unsupported build-extension type ~a" type))))

(defun cfp-output-file-default (input-file output-type &key target-backend)
  (let* ((defaults (merge-pathnames input-file *default-pathname-defaults*)))
    (when target-backend
      (setq defaults (make-pathname :host target-backend :defaults defaults)))
    (make-pathname :type (build-extension output-type)
                   :defaults defaults)))


;;; Copied from sbcl sb!xc:compile-file-pathname
;;;   If INPUT-FILE is a logical pathname and OUTPUT-FILE is unsupplied,
;;;   the result is a logical pathname. If INPUT-FILE is a logical
;;;   pathname, it is translated into a physical pathname as if by
;;;   calling TRANSLATE-LOGICAL-PATHNAME.
;;; So I haven't really tried to make this precisely ANSI-compatible
;;; at the level of e.g. whether it returns logical pathname or a
;;; physical pathname. Patches to make it more correct are welcome.
(defun compile-file-pathname (input-file
                              &key (output-file nil output-file-p)
                                   (output-type *default-output-type* output-type-p)
                                   target-backend
                              &allow-other-keys)
  (declare (ignore output-type))
  (let* ((output-type :bytecode)
         (pn (if output-file-p
		 (merge-pathnames output-file (translate-logical-pathname (cfp-output-file-default input-file output-type :target-backend target-backend)))
		 (cfp-output-file-default input-file output-type :target-backend target-backend)))
         (ext (build-extension output-type)))
    (if (or output-type-p (not output-file-p))
        (make-pathname :type ext :defaults pn :version nil)
        pn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile-file proper

(defvar *debug-compile-file* nil)
(defvar *debug-compile-file-counter* 0)

(defvar *compile-file-output-pathname* nil)

(defun compile-file-source-pos-info (stream)
  (core:input-stream-source-pos-info
   stream *compile-file-file-scope*
   *compile-file-source-debug-lineno* *compile-file-source-debug-offset*))

(defun enable-bytecode-file-compiler ()
  (setf *default-output-type* :bytecode))

(defun disable-bytecode-file-compiler ()
  (setf *default-output-type* :faso))

(defmacro progv-env (environment symbols values &body forms)
  `(core:progv-env-function ,environment ,symbols ,values
                            (lambda ()
                              (declare (core:lambda-name core::progv-env-lambda))
                              (progn ,@forms))))

(defun compile-file (input-file
                     &rest args
                     &key
                       ;; Standard keywords
                       output-file
                       ((:verbose *compile-verbose*) *compile-verbose*)
                       ((:print *compile-print*) *compile-print*)
                       (external-format :default)
                       ;; Extensions
                       (execution (if *compile-file-parallel*
                                      :parallel
                                      :serial))
                       environment ; compilation environment
                       ;; output-type can be (or :faso :fasobc :fasoll :bytecode)
                       (output-type *default-output-type*)
                       ;; type can be either :kernel or :user
                       ;; FIXME: What does this do.
                       (type :user)
                       ;; Control the order of startup functions (FIXME: ditto above)
                       (image-startup-position (core:next-startup-position))
                       (source-debug-pathname nil cfsdpp)
                       ((:source-debug-lineno
                         *compile-file-source-debug-lineno*)
                        0)
                       ((:source-debug-offset
                         *compile-file-source-debug-offset*)
                        0)
                       ;; these ought to be removed, or at least made
                       ;; to use lisp-level optimization policy rather
                       ;; than what they do now, which is LLVM stuff.
                       (optimize t)
                       (optimize-level *optimization-level*)
                     &allow-other-keys)
  ;; These are all just passed along to other functions.
  (declare (ignore output-file type
                   image-startup-position optimize optimize-level))
  "See CLHS compile-file."
  (with-compilation-unit ()
    (let* ((output-path (apply #'compile-file-pathname input-file args))
           (*optimize* *optimize*) (*policy* *policy*)
           (compile-file-pathname
             (pathname (merge-pathnames input-file)))
           (compile-file-truename
             (translate-logical-pathname compile-file-pathname))
           (*compile-file-source-debug-pathname*
             (if cfsdpp source-debug-pathname compile-file-truename))
           (*compile-file-file-scope*
             (core:file-scope *compile-file-source-debug-pathname*))
           ;; bytecode compilation can't be done in parallel at the moment.
           ;; we could possibly warn about it if execution was specified,
           ;; but practically speaking it would mostly be noise.
           (execution (if (eq output-type :bytecode)
                          :serial
                          execution)))
      ;; Many of the special variables (e.g. *optimize*) are not expected to be
      ;; used by macroexpanders or other user code, so they can just always be
      ;; bound in the usual environment. But a few are user-accessible.
      (progv-env environment
          '(*readtable* *package* *compile-file-pathname* *compile-file-truename*)
          ;; FIXME: probably should read *readtable* and *package* from the env
          (list *readtable* *package* compile-file-pathname compile-file-truename)
        (with-open-file (source-sin input-file
                                    :external-format external-format)
          (with-compilation-results ()
            (when *compile-verbose*
              (format t "~&; Compiling file: ~a~%"
                      (namestring input-file)))
            (ecase execution
              ((:serial :parallel)
               (apply #'compile-stream/serial source-sin output-path args))
              #+(or)
              (:parallel
               ;; defined later in compile-file-parallel.lisp.
               (apply #'compile-stream/parallel source-sin output-path
                      args)))))))))

(defun compile-stream/serial (input-stream output-path &rest args
                              &key
                                (optimize t)
                                (optimize-level *optimization-level*)
                                (output-type *default-output-type*)
                                ;; type can be either :kernel or :user
                                (type :user)
                                ;; Control the order of startup functions
                                (image-startup-position (core:next-startup-position)) 
                                environment
                              &allow-other-keys)
  (declare (ignore environment image-startup-position type optimize-level optimize))
  (let ((*compile-file-parallel* nil))
    (if t;; (eq output-type :bytecode)
        (apply #'cmpltv:bytecode-compile-stream input-stream output-path args)
        ;; Defined later in cleavir/compile-file.lisp
        (apply #'native-compile-stream input-stream output-path args)))
  (truename output-path))
