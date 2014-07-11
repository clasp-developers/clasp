(defpackage :test-asdf/dll-user (:use)) ;; dummy, for package-inferred-system dependencies.

(in-package :test-package)

#+(and mkcl windows)
(ffi:clines "extern __declspec(dllimport) int sample_function(void);")
#-(and mkcl windows)
(ffi:clines "extern int sample_function(void);")

(ffi:def-function "sample_function" () :returning :int)
