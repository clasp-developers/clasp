;;; Remove compiled files
(let* ((fn (compile-file-pathname "doit.lsp"))
       (type (pathname-type fn))
       (dir-pathname (make-pathname :name :wild :type type))
       (files (union (directory "aux*.*") (directory dir-pathname) :test #'equal)))
  (assert type)
  (assert (not (string-equal type "lsp")))
  (mapc #'delete-file files))

(si::package-lock (find-package "COMMON-LISP") nil)
(require 'rt)

#+ecl (compile nil '(lambda () nil))
#+(and ecl (not ecl-bytecmp))
(setq c::*suppress-compiler-warnings* t c::*suppress-compiler-notes* t)

(setq *load-verbose* nil
      *load-print* nil
      *compile-verbose* nil
      *compile-print* nil)

(unless (find-package :cl-test)
  (make-package :cl-test))

(in-package :cl-test)
(use-package :sb-rt)

(load "tools.lsp")
(load "../ansi-tests/universe.lsp")
(load "../ansi-tests/ansi-aux.lsp")

(load "cl-001.lsp")

(load "int-001.lsp")

#-ecl-bytecmp
(load "cmp-001.lsp")

#+clos
(progn
 (load "mop-001.lsp")
 (load "mop-dispatch.lsp")
 (load "mop-dependents.lsp"))

#+(and ffi (not ecl-bytecmp))
(load "ffi-001.lsp")

#+threads
(progn
 (load "mp-tools.lsp")
 (load "mp-001.lsp")
 (load "mutex-001.lsp")
 (load "mailbox-001.lsp")
 )

#+unicode
(progn
 ;; In Windows SYSTEM does not fail with a nonzero code when it
 ;; fails to execute a command. Hence in that case we assume
 ;; we simply can not run these tests
 #-msvc
 (when (zerop (si::system "iconv -l >/dev/null 2>&1"))
  (load "eformat-002.lsp"))
 (load "eformat-001.lsp"))

(time (sb-rt:do-tests))

