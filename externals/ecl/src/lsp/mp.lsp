;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  MP.LSP  -- Multiprocessing capabilities.

;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

#-threads
(defpackage "MP"
  (:use "CL" "SI")
  (:export "WITH-LOCK"))

(in-package "MP")

(defmacro without-interrupts (&body body)
  #!+sb-doc
  "Executes BODY with all deferrable interrupts disabled. Deferrable
interrupts arriving during execution of the BODY take effect after BODY has
been executed.

Deferrable interrupts include most blockable POSIX signals, and
SB-THREAD:INTERRUPT-THREAD. Does not interfere with garbage collection, and
unlike in many traditional Lisps using userspace threads, in SBCL
WITHOUT-INTERRUPTS does not inhibit scheduling of other threads.

Binds ALLOW-WITH-INTERRUPTS, WITH-LOCAL-INTERRUPTS and WITH-RESTORED-INTERRUPTS
as a local macros.

WITH-RESTORED-INTERRUPTS executes the body with interrupts enabled if and only
if the WITHOUT-INTERRUPTS was in an environment in which interrupts were allowed.

ALLOW-WITH-INTERRUPTS allows the WITH-INTERRUPTS to take effect during the
dynamic scope of its body, unless there is an outer WITHOUT-INTERRUPTS without
a corresponding ALLOW-WITH-INTERRUPTS.

WITH-LOCAL-INTERRUPTS executes its body with interrupts enabled provided that
for there is an ALLOW-WITH-INTERRUPTS for every WITHOUT-INTERRUPTS surrounding
the current one. WITH-LOCAL-INTERRUPTS is equivalent to:

  (allow-with-interrupts (with-interrupts ...))

Care must be taken not to let either ALLOW-WITH-INTERRUPTS or
WITH-LOCAL-INTERRUPTS appear in a function that escapes from inside the
WITHOUT-INTERRUPTS in:

  (without-interrupts
    ;; The body of the lambda would be executed with WITH-INTERRUPTS allowed
    ;; regardless of the interrupt policy in effect when it is called.
    (lambda () (allow-with-interrupts ...)))

  (without-interrupts
    ;; The body of the lambda would be executed with interrupts enabled
    ;; regardless of the interrupt policy in effect when it is called.
    (lambda () (with-local-interrupts ...)))
"
  (ext:with-unique-names (outer-allow-with-interrupts outer-interrupts-enabled)
    `(multiple-value-prog1
         (macrolet ((allow-with-interrupts (&body allow-forms)
                      `(let ((si:*allow-with-interrupts* ,',outer-allow-with-interrupts))
                         ,@allow-forms))
                    (with-restored-interrupts (&body with-forms)
                      `(let ((si:*interrupts-enabled* ,',outer-interrupts-enabled))
                         ,@with-forms))
                    (with-local-interrupts (&body with-forms)
                      `(let* ((si:*allow-with-interrupts* ,',outer-allow-with-interrupts)
                              (si:*interrupts-enabled* ,',outer-allow-with-interrupts))
                         (when ,',outer-allow-with-interrupts
                           (si::check-pending-interrupts))
                         (locally ,@with-forms))))
           (let* ((,outer-interrupts-enabled si:*interrupts-enabled*)
                  (si:*interrupts-enabled* nil)
                  (,outer-allow-with-interrupts si:*allow-with-interrupts*)
                  (si:*allow-with-interrupts* nil))
             (declare (ignorable ,outer-allow-with-interrupts
                                 ,outer-interrupts-enabled))
             ,@body))
       (when si:*interrupts-enabled*
         (si::check-pending-interrupts)))))

(defmacro with-interrupts (&body body)
  "Executes BODY with deferrable interrupts conditionally enabled. If there
are pending interrupts they take effect prior to executing BODY.

As interrupts are normally allowed WITH-INTERRUPTS only makes sense if there
is an outer WITHOUT-INTERRUPTS with a corresponding ALLOW-WITH-INTERRUPTS:
interrupts are not enabled if any outer WITHOUT-INTERRUPTS is not accompanied
by ALLOW-WITH-INTERRUPTS."
  (ext:with-unique-names (allowp enablep)
    ;; We could manage without ENABLEP here, but that would require
    ;; taking extra care not to ever have *ALLOW-WITH-INTERRUPTS* NIL
    ;; and *INTERRUPTS-ENABLED* T -- instead of risking future breakage
    ;; we take the tiny hit here.
    `(let* ((,allowp si:*allow-with-interrupts*)
            (,enablep si:*interrupts-enabled*)
            (si:*interrupts-enabled* (or ,enablep ,allowp)))
       (when (and ,allowp (not ,enablep))
         (si::check-pending-interrupts))
       (locally ,@body))))

(defmacro with-lock ((lock-form &rest options) &body body)
  #-threads
  `(progn ,@body)
  ;; Why do we need %count? Even if get-lock succeeeds, an interrupt may
  ;; happen between the end of get-lock and when we save the output of
  ;; the function. That means we lose the information and ignore that
  ;; the lock was actually acquired. Furthermore, a lock can be recursive
  ;; and mp:lock-holder is also not reliable.
  ;;
  ;; Next notice how we need to disable interrupts around the body and
  ;; the get-lock statement, to ensure that the unlocking is done with
  ;; interrupts disabled.
  #+threads
  (ext:with-unique-names (lock owner count process)
    `(let* ((,lock ,lock-form)
            (,owner (mp:lock-owner ,lock))
	    (,count (mp:lock-count ,lock)))
       (declare (type fixnum ,count))
       (without-interrupts
           (unwind-protect
                (with-restored-interrupts
                    (mp::get-lock ,lock)
                  (locally ,@body))
	     (let ((,process mp:*current-process*))
	       (declare (optimize (speed 3) (safety 0) (debug 0)))
	       (when (and (eq ,process (mp:lock-owner ,lock))
			  (or (not (eq ,owner ,process))
			      (> (the fixnum (mp:lock-count ,lock))
				 (the fixnum ,count))))
		 (mp::giveup-lock ,lock))))))))
