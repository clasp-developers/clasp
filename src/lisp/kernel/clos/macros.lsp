;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "CLOS")

(defmacro mapappend (fun &rest args)
  `(reduce #'append (mapcar ,fun ,@args)))

(defmacro ensure-up-to-date-instance (instance)
  ;; The up-to-date status of a class is determined by
  ;; instance.sig. This slot of the C structure contains a list of
  ;; slot definitions that was used to create the instance. When the
  ;; class is updated, the list is newly created. Structures are also
  ;; "instances" but keep ECL_UNBOUND instead of the list.
  `(let* ((i ,instance)
          (s (si::instance-sig i)))
     (declare (:read-only i s))
     (when (si:sl-boundp s)
       (unless (eq s (class-slots (si::instance-class i)))
         (update-instance i)))))



