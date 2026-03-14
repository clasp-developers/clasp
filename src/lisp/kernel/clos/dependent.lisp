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

(defgeneric add-dependent (metaobject dependent))
(defgeneric remove-dependent (metaobject dependent))
(defgeneric map-dependents (metaobject function))
(defgeneric update-dependent (metaobject dependent &rest initargs))

(defun update-dependents (object initargs)
  (map-dependents
   object
   #'(lambda (dep) (apply #'update-dependent object dep initargs))))
