;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;                              predicate routines

;;#-ecl(in-package "SYSTEM")

(eval-when (:compile-toplevel)
  (setq *low-level-trace* :print))

(defun push-type (type tag)
  (declare (si::c-local)
	   (ext:assume-no-errors))
  (dolist (i *member-types*)
    (declare (cons i))
    (when (typep (car i) type)
      (setq tag (logior tag (cdr i)))))
  (push (cons type tag) *elementary-types*)
  tag)

