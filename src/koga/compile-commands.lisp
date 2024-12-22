(in-package #:koga)

;; This method creates a JSON Compilation Database that is compatible with
;; https://clang.llvm.org/docs/JSONCompilationDatabase.html
;; This file is used by boehm to create an AST and sif file to enable
;; the precise variants.
(defmethod print-variant-target-sources
    (configuration (name (eql :compile-commands)) output-stream (target (eql :libclasp)) sources
     &key &allow-other-keys)
  (shasht:write-json (loop with build-path = (merge-pathnames (build-path configuration)
                                                              (uiop:getcwd))
                           for source in sources
                           for out = (resolve-source (make-source-output source :type "o"))
                           for sa-out = (let ((*root-paths* (list* :variant (merge-pathnames (make-pathname :directory (list :relative "boehm"))
                                                                                             (root :build))
                                                                   *root-paths*)))
                                          (resolve-source (make-source-output source :type "sa")))
                           for cc-source = (typep source 'cc-source)
                           when (typep source 'c-source)
                             collect `(:object-plist "directory" ,build-path
                                                     "file" ,(resolve-source source)
                                                     "output" ,out
                                                     "command" ,(format nil "~a ~a ~a -c -MD -MF ~a.d -MT ~a -o~a ~a"
                                                                        (if cc-source
                                                                            (cxx configuration)
                                                                            (cc configuration))
                                                                        (if cc-source
                                                                            *variant-cxxflags*
                                                                            *variant-cflags*)
                                                                        (if cc-source
                                                                            (cxxflags configuration)
                                                                            (cflags configuration))
                                                                        sa-out
                                                                        sa-out
                                                                        out
                                                                        (resolve-source source))))
                     output-stream))

