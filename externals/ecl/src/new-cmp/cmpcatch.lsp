;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
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

;;;; CMPCATCH  Catch, Unwind-protect, and Throw.

(in-package "COMPILER")

(defun c1catch (destination args)
  (check-args-number 'CATCH args 1)
  ;; First we decide where to store the output
  (c1with-saved-output (values-prefix values-postfix new-destination destination)
    (let* ((cleanup-form (c1frame-pop))
	   (old-env *cmp-env*)
	   (*cmp-env* (cmp-env-register-cleanup cleanup-form (cmp-env-copy old-env)))
	   (normal (make-tag :name (gensym "CATCH-NORMAL") :label (next-label)))
	   (exit (make-tag :name (gensym "CATCH-EXIT") :label (next-label))))
      (nconc values-prefix
	     (c1with-saved-one-value (prefix postfix location (pop args))
                (nconc prefix
		       (c1frame-set location normal)
		       postfix))
	     (c1set-loc new-destination 'VALUES)
	     (c1jmp exit)
	     (list normal)
	     (c1translate new-destination `(progn ,@args))
	     (list exit)
	     cleanup-form
             (c1set-loc destination new-destination)
	     values-postfix))))

(defun c1unwind-protect (destination args)
  (check-args-number 'UNWIND-PROTECT args 1)
  (unless (rest args)
    (return-from c1unwind-protect (c1translate destination (first args))))
  (let* ((*cmp-env* (cmp-env-mark 'UNWIND-PROTECT (cmp-env-copy)))
         (exit (make-tag :name (gensym "UWP-EXIT") :label (next-label)))
         (main-form (make-tag :name (gensym "UWP-MAIN") :label (next-label)))
         (protect-form (make-tag :name (gensym "UWP-PROTECT") :label (next-label))))
    (c1with-temps (prefix postfix unwound frame)
      (cmp-env-register-cleanup (nconc (c1stack-frame-close frame)
                                       (c1frame-pop)))
      (nconc prefix
             ;; Create a point to stop when unwinding.
             (c1stack-frame-open frame)
             (c1set-loc unwound `(VV "OBJNULL"))
             (c1frame-set 'unwind-protect main-form)

             ;; We reach here when we intercepted a nonlocal control transfer.
             ;; UNWOUND is set to the original destination.
             (c1frame-save-next unwound)
             (c1jmp protect-form)

             ;; This is the form that is protected by UNWIND-PROTECT. We
             ;; store the output in the VALUES array.
             (list main-form)
             (c1translate 'VALUES (pop args))

             ;; These are the forms that have to be executed always. Note
             ;; that we save the values in the above created stack frame.
             (list protect-form)
             (c1stack-frame-push-values frame)
             (c1translate 'TRASH `(progn ,@args))
             (c1stack-frame-pop-values frame 'VALUES+VALUE0)
             (c1stack-frame-close frame)

             ;; After those forms, we decide whether we have to transfer control
             ;; elsewhere
             (c1jmp-false exit unwound)
             (c1frame-jmp-next unwound)

             ;; Otherwise we just send the values where they should.
             (list exit)
             (c1set-loc destination 'VALUES+VALUE0)
             postfix))))

(defun c1throw (destination args)
  (check-args-number 'THROW args 2 2)
  (c1with-temps (prefix postfix tag)
    (nconc prefix
           (c1translate tag (first args))
           (c1translate 'VALUES (second args))
           (c1throw-op tag)
           postfix)))
