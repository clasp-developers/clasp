;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: SYSTEM -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

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

#+threads
(defpackage "MP"
  (:use "CL")
  (:import-from :CORE "WITH-UNIQUE-NAMES")
  (:export "WITH-LOCK" "WITHOUT-INTERRUPTS" "WITH-INTERRUPTS"
           "WITH-LOCAL-INTERRUPTS" "WITH-RESTORED-INTERRUPTS" "ALLOW-WITH-INTERRUPTS"))

#+threads
(in-package "MP")

#+threads
(progn
  (defvar *debug-print-lock* (mp:make-lock :name :debug-print-lock))
  (defmacro debug-mp (fmt &rest args)
    `(unwind-protect
          (progn
            (get-lock *debug-print-lock*)
            (format t ,fmt ,@args))
       (giveup-lock *debug-print-lock*))))
                   

#+threads
(defmacro without-interrupts (&body body)
  #!+sb-doc
  "Executes BODY with all deferrable interrupts disabled. Deferrable
interrupts arriving during execution of the BODY take effect after BODY has
been executed.

Deferrable interrupts include most blockable POSIX signals, and
MP:INTERRUPT-THREAD. Does not interfere with garbage collection, and
unlike in many traditional Lisps using userspace threads, in ECL
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
  (with-unique-names (outer-allow-with-interrupts outer-interrupts-enabled)
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

#+threads
(defmacro with-interrupts (&body body)
  "Executes BODY with deferrable interrupts conditionally enabled. If there
are pending interrupts they take effect prior to executing BODY.

As interrupts are normally allowed WITH-INTERRUPTS only makes sense if there
is an outer WITHOUT-INTERRUPTS with a corresponding ALLOW-WITH-INTERRUPTS:
interrupts are not enabled if any outer WITHOUT-INTERRUPTS is not accompanied
by ALLOW-WITH-INTERRUPTS."
  (with-unique-names (allowp enablep)
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
  #+threads
  (with-unique-names (lock)
    `(let ((,lock ,lock-form))
       (unwind-protect
            (progn
              (mp:get-lock ,lock)
              (locally ,@body))
         (progn
           (mp:giveup-lock ,lock))))))

#+ecl-read-write-lock
(defmacro with-rwlock ((lock op) &body body)
  "Acquire rwlock for the dynamic scope of BODY for operation OP,
which is executed with the lock held by current thread, and
WITH-RWLOCK returns the values of body.

Valid values of argument OP are :READ or :WRITE
(for reader and writer access accordingly)."
    (assert (member op '(:read :write) :test #'eq))
    (let ((s-lock (gensym)))
      `(let ((,s-lock ,lock))
         (,(if (eq :read op)
               'mp:get-rwlock-read
             'mp:get-rwlock-write) ,s-lock t)
         (unwind-protect
             (progn
               ,@body)
           (,(if (eq :read op)
                 'mp:giveup-rwlock-read
               'mp:giveup-rwlock-write) ,s-lock)))))
