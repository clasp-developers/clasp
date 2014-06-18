;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;; CMPENV-DECLAIM -- Proclamations local to the current file
;;;;
;;;; One implementation of DECLAIM that uses the compiler environment
;;;; providing a "base" set of entries that all other environments
;;;; stem from.
;;;;
 
(in-package #-ecl-new "COMPILER" #+ecl-new "C-ENV")

(defun process-declaim-args (args)
  (flet ((add-variables (env types specials)
           (loop for name in specials
              unless (assoc name types)
              do (let ((v (c1make-global-variable name :kind 'special)))
                   (setf env (cmp-env-register-var v env nil))))
           (loop for (name . type) in types
              for specialp = (or (sys:specialp name) (member name specials))
              for kind = (if specialp 'SPECIAL 'GLOBAL)
              for v = (c1make-global-variable name :type type :kind kind)
              do (setf env (cmp-env-register-var v env nil)))
           env))
    (multiple-value-bind (body specials types ignored others doc all)
        (c1body `((DECLARE ,@args)) nil)
      (when ignored
        (cmpwarn-style "IGNORE/IGNORABLE declarations in DECLAIM are ignored"))
      (reduce #'add-one-declaration others
              :initial-value (add-variables *cmp-env* types specials))
      (reduce #'add-one-declaration others
              :initial-value (add-variables *cmp-env-root* types specials)))))

(defmacro declaim (&rest declarations)
  `(locally (declare (notinline mapc))
     (ext:with-backend
       :c/c++ (eval-when (:compile-toplevel)
                (c::process-declaim-args ',declarations))
       :bytecodes (eval-when (:compile-toplevel)
                    (mapc 'proclaim ',declarations)))
     (eval-when (:load-toplevel :execute)
       (mapc 'proclaim ',declarations))))

(defmacro ext::c-declaim (&rest declarations)
  `(ext:with-backend
       :c/c++ (eval-when (:compile-toplevel)
                (c::process-declaim-args ',declarations))))
