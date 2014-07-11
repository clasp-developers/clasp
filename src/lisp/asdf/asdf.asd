;;; -*- mode: lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software available under an MIT-style license.              ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2001-2014 Daniel Barlow and contributors           ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :asdf)

#+asdf3
(defsystem :asdf/prelude
  ;; Note that it's polite to sort the defsystem forms in dependency order,
  ;; and compulsory to sort them in defsystem-depends-on order.
  :version (:read-file-form "version.lisp-expr")
  :around-compile call-without-redefinition-warnings ;; we need be the same as uiop
  :encoding :utf-8
  :components
  ((:file "header")))

#+asdf3
(defsystem :asdf/driver
  :depends-on (:uiop))

#+asdf3
(defsystem :asdf/defsystem
  :licence "MIT"
  :description "The defsystem part of ASDF"
  :long-name "Another System Definition Facility"
  :description "The portable defsystem for Common Lisp"
  :long-description "ASDF/DEFSYSTEM is the de facto standard DEFSYSTEM facility for Common Lisp,
   a successor to Dan Barlow's ASDF and Francois-Rene Rideau's ASDF2.
   For bootstrap purposes, it comes bundled with UIOP in a single file, asdf.lisp."
  :homepage "http://common-lisp.net/projects/asdf/"
  :bug-tracker "https://launchpad.net/asdf/"
  :mailto "asdf-devel@common-lisp.net"
  :source-control (:git "git://common-lisp.net/projects/asdf/asdf.git")
  :version (:read-file-form "version.lisp-expr")
  :build-operation monolithic-concatenate-source-op
  :build-pathname "build/asdf" ;; our target
  :around-compile call-without-redefinition-warnings ;; we need be the same as uiop
  :depends-on (:asdf/prelude :asdf/driver)
  :encoding :utf-8
  :components
  ((:file "upgrade")
   (:file "component" :depends-on ("upgrade"))
   (:file "system" :depends-on ("component"))
   (:file "cache" :depends-on ("upgrade"))
   (:file "find-system" :depends-on ("system" "cache"))
   (:file "find-component" :depends-on ("find-system"))
   (:file "operation" :depends-on ("find-system"))
   (:file "action" :depends-on ("find-component" "operation"))
   (:file "lisp-action" :depends-on ("action"))
   (:file "plan" :depends-on ("lisp-action"))
   (:file "operate" :depends-on ("plan"))
   (:file "output-translations" :depends-on ("operate"))
   (:file "source-registry" :depends-on ("find-system"))
   (:file "backward-internals" :depends-on ("lisp-action" "operate"))
   (:file "parse-defsystem" :depends-on ("backward-internals" "cache" "system"))
   (:file "bundle" :depends-on ("lisp-action" "operate" "parse-defsystem"))
   (:file "concatenate-source" :depends-on ("plan" "parse-defsystem" "bundle"))
   (:file "backward-interface" :depends-on ("operate" "output-translations"))
   (:file "package-inferred-system" :depends-on ("system" "find-system" "parse-defsystem"))
   (:file "interface" :depends-on
          ("parse-defsystem" "concatenate-source"
           "backward-interface" "backward-internals"
           "output-translations" "source-registry" "package-inferred-system"))
   (:file "user" :depends-on ("interface"))
   (:file "footer" :depends-on ("user"))))

(defsystem :asdf
  :author ("Daniel Barlow")
  :maintainer ("Robert Goldman")
  :licence "MIT"
  :description "Another System Definition Facility"
  :long-description "ASDF builds Common Lisp software organized into defined systems."
  :version "3.1.2.7" ;; to be automatically updated by make bump-version
  :depends-on ()
  #+asdf3 :encoding #+asdf3 :utf-8
  :class #.(if (find-class 'package-inferred-system nil) 'package-inferred-system 'system)
  ;; For most purposes, asdf itself specially counts as a builtin system.
  ;; If you want to link it or do something forbidden to builtin systems,
  ;; specify separate dependencies on uiop (aka asdf-driver) and asdf/defsystem.
  #+asdf3 :builtin-system-p #+asdf3 t
  :components ((:module "build" :components ((:file "asdf"))))
  :in-order-to (#+asdf3 (prepare-op (monolithic-concatenate-source-op :asdf/defsystem))))

