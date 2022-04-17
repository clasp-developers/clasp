(in-package #:koga)

(defgeneric make-output-stream (configuration name path)
  (:method (configuration name path)
    (declare (ignore configuration name))
    (ninja:make-timestamp-preserving-stream path))
  (:documentation "Create an output stream for a configuration output."))

(defgeneric add-target-source (configuration target source)
  (:method (configuration target source)
    (unless (find source (gethash target (targets *configuration*))
                  :test (lambda (x y)
                          (and (equal (source-path x) (source-path y))
                               (eql (source-root x) (source-root y)))))
      (setf (gethash target (targets *configuration*))
            (nconc (gethash target (targets *configuration*))
                   (list source)))))
  (:documentation "Add a source file to a specific target."))

(defgeneric print-prologue (configuration name output-stream)
  (:method (configuration name output-stream)
    (declare (ignore configuration name output-stream)))
  (:documentation "Print the prologue of a configuration output. This
method is called for variant based outputs and build outputs."))

(defgeneric print-epilogue (configuration name output-stream)
  (:method (configuration name output-stream)
    (declare (ignore configuration name output-stream)))
  (:documentation "Print the epilogue of a configuration output. This
method is called for variant based outputs and build outputs."))

(defgeneric print-target-source (configuration name output-stream target source)
  (:method (configuration name output-stream target source)
    (declare (ignore configuration name output-stream target source)))
  (:documentation "Print the output associated with a specific target and source.
This method is called for variant based outputs and build outputs. The method
can return a plist whose values are sources or lists or source. The plists
from all sources associated with a target will accumulated and passed to
PRINT-TARGET-SOURCES. For example, suppose that a target has two sources A and
B. If (PRINT-TARGET-SOURCE configuration name output-stream target A) returns
(:fu BAR :wibble (QUUX GRONK)) and (PRINT-TARGET-SOURCE configuration name output-stream target B)
returns (:fu (BAZ FOO)) then the call to PRINT-TARGET-SOURCES will be
(PRINT-TARGET-SOURCES configuration name output-stream target (list A B)
                      :fu (list BAR BAZ FOO) :wibble (list QUUX GRONK))"))

(defgeneric print-target-sources (configuration name output-stream target sources &key &allow-other-keys)
  (:method (configuration name output-stream target sources &key &allow-other-keys)
    (declare (ignore configuration name output-stream target sources)))
  (:documentation "Print the output associated with a target and its sources. The
accumulated plists from each PRINT-TARGET-SOURCE is passed as keys."))

(defgeneric print-variant-target-source (configuration name output-stream target source)
  (:method (configuration name output-stream target source)
    (declare (ignore configuration name output-stream target source)))
  (:documentation "Print the output associated with a variant's target and a source.
The return value is handled the same as PRINT-TARGET-SOURCE."))

(defgeneric print-variant-target-sources
    (configuration name output-stream target sources &key &allow-other-keys)
  (:method (configuration name output-stream target sources &key &allow-other-keys)
    (declare (ignore configuration name output-stream target sources)))
  (:documentation "Print the output associated with a variant target and its sources. The
accumulated plists from each PRINT-VARIANT-TARGET-SOURCE is passed as keys."))

(defun map-variants (configuration func)
  "Apply func to all variants in configuration."
  (loop for variant in (variants configuration)
        for *variant-gc* = (variant-gc variant)
        for *variant-precise* = (variant-precise variant)
        for *variant-prep* = (variant-prep variant)
        for *variant-debug* = (variant-debug variant)
        for *variant-default* = (variant-default variant)
        for *variant-cflags* = (or (cflags variant) "")
        for *variant-cxxflags* = (or (cxxflags variant) "")
        for *variant-cppflags* = (or (cppflags variant) "")
        for *variant-ldflags* = (or (ldflags variant) "")
        for *variant-ldlibs* = (or (ldlibs variant) "")
        for *variant-name* = (variant-name variant)
        for *variant-bitcode-name* = (variant-bitcode-name variant)
        for *variant-path* = (merge-pathnames (make-pathname :directory (list :relative *variant-bitcode-name*))
                                              *build-path*)
        for *install-variant-path* = (merge-pathnames (make-pathname :directory (list :relative "build" *variant-bitcode-name*))
                                                      *install-clasp-path*)
        do (funcall func)))

(defun write-build-output (configuration name)
  "Write a configuration output"
  (destructuring-bind (path &rest targets)
      (gethash name (outputs configuration))
    (when (eq (source-root path) :build)
      (message :info "Writing output ~a" (resolve-source path))
      (ensure-directories-exist (resolve-source path))
      (let* ((output-path (resolve-source path))
             (stream (make-output-stream configuration name output-path))
             (*build-path* #P"")
             (*code-path* (make-pathname :directory '(:relative :up))))
        (unwind-protect
            (progn
              (print-prologue configuration name stream)
              (loop for target in targets
                    for sources = (gethash target (targets configuration))
                    do (apply #'print-target-sources configuration name
                              stream target sources
                              (join-plists (loop for source in sources
                                                 collect (print-target-source configuration name
                                                                              stream target source)))))
              (map-variants configuration
                            (lambda ()
                              (loop for target in targets
                                    for sources = (gethash target (targets configuration))
                                    do (apply #'print-variant-target-sources configuration name
                                              stream target sources
                                              (join-plists (loop for source in sources
                                                                 collect (print-variant-target-source configuration name
                                                                                                      stream target source)))))))
              (print-epilogue configuration name stream))
          (close stream))
        (when (shebangp output-path)
          (run-program (list "chmod" "a+x" (namestring output-path))))))))

(defun write-variant-output (configuration name)
  "Write a configuration variant output."
  (destructuring-bind (path &rest targets)
      (gethash name (outputs configuration))
    (when (eq (source-root path) :variant)
      (message :info "Writing output ~a" (resolve-source path))
      (ensure-directories-exist (resolve-source path))
      (let* ((output-path (resolve-source path))
             (stream (make-output-stream configuration name output-path))
             (*build-path* #P"")
             (*variant-path* (make-pathname :directory (list :relative *variant-bitcode-name*)))
             (*install-variant-path* (merge-pathnames (make-pathname :directory (list :relative
                                                                                      "build"
                                                                                      *variant-bitcode-name*))
                                                      *install-clasp-path*))
             (*code-path* (make-pathname :directory '(:relative :up))))
        (unwind-protect
            (progn
              (print-prologue configuration name stream)
              (loop for target in targets
                    for sources = (gethash target (targets configuration))
                    do (apply #'print-variant-target-sources configuration name
                              stream target sources
                              (join-plists (loop for source in sources
                                                     collect (print-variant-target-source configuration name
                                                                                          stream target source)))))
              (print-epilogue configuration name stream))
          (close stream))
        (when (shebangp output-path)
          (run-program (list "chmod" "a+x" (namestring output-path))))))))

(defun setup (&rest initargs)
  "Setup the build by configuring the units, looking for and loading scripts and
writing the build and variant outputs."
  (let* ((*configuration* (apply #'make-instance 'configuration initargs))
         (*code-path* #P"")
         (*build-path* (build-path *configuration*))
         (*script-path* #P"")
         (*install-path* (if (package-path *configuration*)
                             (merge-pathnames (uiop:relativize-pathname-directory (prefix *configuration*))
                                              (package-path *configuration*))
                             (prefix *configuration*)))
         (*install-bin-path* (merge-pathnames #P"bin/" *install-path*))
         (*install-clasp-path* (merge-pathnames #P"lib/clasp/" *install-path*)))
    (when (clean *configuration*)
      (message :emph "~%Cleaning up previous build")
      (uiop:delete-directory-tree (build-path *configuration*)
                                  :validate t
                                  :if-does-not-exist :ignore))
    (ensure-directories-exist (build-path *configuration*))
    (message :emph "~%Configuring the build")
    (loop for unit in (units *configuration*)
          do (configure-unit *configuration* unit))
    (message :emph "~%Evaluating cscript files")
    (loop for source in (uiop:read-file-form #P"repos.sexp")
          for name = (getf source :name)
          for directory = (getf source :directory)
          for extension = (getf source :extension)
          when (and extension (member name (extensions *configuration*)))
            do (recurse directory))
    (recurse #P"")
    (message :emph "~%Writing build files")
    (loop for name being the hash-keys in (outputs *configuration*)
          do (write-build-output *configuration* name))
    (map-variants *configuration*
                  (lambda ()
                    (loop for name being the hash-keys in (outputs *configuration*)
                          do (write-variant-output *configuration* name))))))


