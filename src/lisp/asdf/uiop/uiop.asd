;;; -*- mode: lisp -*-
(in-package :asdf)

(unless (or (member :asdf3 *features*)
            (and (member :asdf2 *features*) (version-satisfies (asdf:asdf-version) "2.11.4")))
  (error "UIOP requires ASDF 2.011.4 or later."))

(defun call-without-redefinition-warnings (thunk)
  (handler-bind (((or
                   #+allegro simple-warning
                   #+clozure ccl:compiler-warning
                   #+cmu kernel:simple-style-warning
                   #-(or allegro clozure cmu) warning)
                   #'muffle-warning))
    (funcall thunk)))

(defsystem :uiop
  :licence "MIT"
  :class #.(if (find-class 'package-system nil) 'package-system 'system)
  #+asdf3 :long-name #+asdf3 "Utilities for Implementation- and OS- Portability"
  :description "Runtime support for Common Lisp programs"
  :long-description "Basic general-purpose utilities that are in such a need
that you can't portably construct a complete program without using them.
UIOP is also known as ASDF/DRIVER or ASDF-UTILS,
being transcluded into asdf.lisp together with ASDF/DEFSYSTEM."
  #+asdf3 :version #+asdf3 (:read-file-form "version.lisp-expr")
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  #+asdf3 :around-compile #+asdf3 call-without-redefinition-warnings
  :components
  ((:static-file "version.lisp-expr")
   (:static-file "contrib/debug.lisp")
   (:file "package")
   (:file "common-lisp" :depends-on ("package"))
   (:file "utility" :depends-on ("common-lisp"))
   (:file "os" :depends-on ("utility"))
   (:file "pathname" :depends-on ("utility" "os"))
   (:file "filesystem" :depends-on ("os" "pathname"))
   (:file "stream" :depends-on ("filesystem"))
   (:file "image" :depends-on ("stream"))
   (:file "run-program" :depends-on ("stream"))
   (:file "lisp-build" :depends-on ("image"))
   (:file "configuration" :depends-on ("image"))
   (:file "backward-driver" :depends-on ("lisp-build" "run-program" "configuration"))
   (:file "driver" :depends-on ("backward-driver"))))
