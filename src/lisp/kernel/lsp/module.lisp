;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;        module routines

;; This is taken from SBCL's code/module.lisp which is in the public
;; domain.

(in-package "SYSTEM")

;;;; exported specials

(defparameter *modules* ()
  "This is a list of module names that have been loaded into Lisp so far.
It is used by PROVIDE and REQUIRE.")

(defparameter ext:*module-provider-functions* nil
  "See function documentation for REQUIRE")

(defvar *immutable-systems* (list (list :asdf nil)
                                  (list :uiop nil)
                                  (list :asdf-package-system nil))
  "The immutable systems that should immediately be registered when ASDF
has been initially provided. Each element in this list should be a list
in which the first element is the name of the system and following
elements are key/values passed to ASDF:REGISTER-IMMUTABLE-SYSTEM. For
example, (:ASDF :VERSION \"3.0.0\") will register ASDF as immutable with
version number of 3.0.0")

;;;; PROVIDE and REQUIRE

(defun normalize-module-name (module-name)
  (let ((name (string module-name)))
    (or (find name '("asdf" "uiop" "sockets" "sb-bsd-sockets") :test #'string-equal)
        name)))

(defun provide (module-name)
  "Adds a new module name to *MODULES* indicating that it has been loaded.
Module-name is a string designator"
  (let ((name (normalize-module-name module-name)))
    (unless (member name *modules* :test #'string=)
      (push name *modules*)
      (when (and (find-package :asdf)
                 (string= "asdf" name))
        (let ((find-system (find-symbol "FIND-SYSTEM" :asdf))
              (register-immutable-system (find-symbol (if (ext:getenv "CLASP_MUTABLE_SYSTEMS")
                                                          "REGISTER-PRELOADED-SYSTEM"
                                                          "REGISTER-IMMUTABLE-SYSTEM")
                                                      :asdf)))
          (dolist (args *immutable-systems*)
            (when (second args)
              (funcall find-system (car args) nil))
            (apply register-immutable-system (car args) (cddr args)))))))
  t)

(defparameter *requiring* nil)

(defun require-error (control &rest arguments)
  (error "Module error: ~?" control arguments))

(defun require (module-name &optional pathnames)
  "Loads a module, unless it already has been loaded. PATHNAMES, if supplied,
is a designator for a list of pathnames to be loaded if the module
needs to be. If PATHNAMES is not supplied, functions from the list
ext:*MODULE-PROVIDER-FUNCTIONS* are called in order with MODULE-NAME
as an argument, until one of them returns non-NIL.  User code is
responsible for calling PROVIDE to indicate a successful load of the
module."
  (let ((name (normalize-module-name module-name)))
    (when (member name *requiring* :test #'string=)
      (require-error "~@<Could not ~S ~A: circularity detected. Please check ~
           your configuration.~:@>" 'require module-name))
    (let ((saved-modules (copy-list *modules*))
          (*requiring* (cons name *requiring*)))
      (unless (member name *modules* :test #'string=)
        (cond (pathnames
               (unless (listp pathnames) (setq pathnames (list pathnames)))
               ;; ambiguity in standard: should we try all pathnames in the
               ;; list, or should we stop as soon as one of them calls PROVIDE?
               (dolist (ele pathnames t)
                 (load ele)))
              (t
               (unless (some (lambda (p) (funcall p module-name))
                             ext:*module-provider-functions*)
                 (require-error "Don't know how to ~S ~A."
                                'require module-name)))))
      (set-difference *modules* saved-modules))))

(defparameter *fasl-extensions* (list "FASL" "FASO" "FASOLL" "FASOBC"))
(defparameter *lisp-extensions* (list "LSP" "LISP"))

(defun clasp-module-provider (module)
  (let* ((name (string module))
         (fasl-path (make-pathname :host "SYS"
                                   :directory '(:absolute "LIB" "MODULES")
                                   :name name))
         (lisp-path (make-pathname :host "SYS"
                                   :directory (list :absolute "SRC" "LISP" "MODULES" name)
                                   :name name)))
    (flet ((try-it (path)
             (when (member :debug-require *features*)
               (format t "REQUIRE is searching in modules: ~a~%" path))
             (when (load path :if-does-not-exist nil)
               (return-from clasp-module-provider t))))
      (dolist (type *fasl-extensions*)
        (try-it (merge-pathnames (make-pathname :type type) fasl-path)))
      (dolist (type *lisp-extensions*)
        (try-it (merge-pathnames (make-pathname :type type) lisp-path))))))

(pushnew 'clasp-module-provider ext:*module-provider-functions*)
