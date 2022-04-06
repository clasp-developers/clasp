(in-package #:koga)

;; This method creates a JSON Compilation Database that is compatible with
;; https://clang.llvm.org/docs/JSONCompilationDatabase.html
;; This file is used by preciseprep to create an AST and sif file to enable
;; the precise variants.
(defmethod print-variant-target-sources
    (configuration (name (eql :compile-commands)) output-stream (target (eql :iclasp)) sources
     &key &allow-other-keys)
  (shasht:write-json (loop with build-path = (merge-pathnames (build-path configuration)
                                                              (uiop:getcwd))
                           for source in sources
                           for out = (resolve-source (make-source-output source :type "o"))
                           when (typep source 'cc-source)
                             collect `(:object-plist "directory" ,build-path
                                                     "file" ,(resolve-source source)
                                                     "output" ,out
                                                     "command" ,(format nil "~a ~a ~a -c -MD -MF ~a.d -o~a ~a"
                                                                        (cxx configuration)
                                                                        *variant-cxxflags*
                                                                        (cxxflags configuration)
                                                                        out
                                                                        out
                                                                        (resolve-source source))))
                     output-stream))

