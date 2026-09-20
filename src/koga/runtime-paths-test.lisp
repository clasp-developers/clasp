;; Run from the repository root with sbcl --script.
(require "asdf")
(asdf:initialize-source-registry
 `(:source-registry (:also-exclude "asdf") (:tree ,(uiop:getcwd)) :inherit-configuration))
(asdf:load-system :koga)
(in-package #:koga)

(assert (equal (linker-library-directories "-L'/tmp/one two' -L /tmp/three\\ four -pthread")
               '("/tmp/one two" "/tmp/three four")))
(assert (null (linker-library-directories nil)))
(assert (handler-case (progn (split-command-flags "-L'unclosed") nil)
          (error () t)))

;; Reproducible builds must not query or embed local toolchain paths.
(let ((config (make-instance 'configuration :reproducible-build t)))
  (configure-runtime-paths config)
  (assert (every (lambda (variant) (null (ldflags variant))) (variants config))))

;; Use the locally selected LLVM, including duplicate and quoted paths.
(let* ((config (apply #'make-instance 'configuration (uiop:read-file-form "config.sexp")))
       (variant (first (variants config))))
  (setf (cxx config) (merge-pathnames "clang++" (llvm-config config)))
  (setf (ldflags config) "-L'/tmp/extra lib' -L/tmp/extra\\ lib -L../relative")
  (configure-runtime-paths config)
  (let* ((args (split-command-flags (ldflags variant)))
         (paths (loop for tail on args when (equal (car tail) "-rpath") collect (third tail))))
    (assert (= 1 (count "/tmp/extra lib" paths :test #'equal)))
    (assert (member (normalize-directory (merge-pathnames "build/../relative/" (uiop:getcwd)))
                    paths :test #'equal))
    (assert (member (string-right-trim "/"
                     (run-program-capture (list (llvm-config config) "--libdir")))
                    paths :test #'equal))))
(format t "PASS: runtime discovery, quoting, deduplication, relative paths, reproducible builds~%")
