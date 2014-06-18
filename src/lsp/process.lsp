;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  PROCESS.LSP  -- External processes

;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "EXT")

(defstruct (external-process (:constructor make-external-process ()))
  pid
  input
  output
  (%status :running)
  (%code nil))

(defun external-process-status (external-process)
  (let ((status (external-process-%status external-process)))
    (if (eq status :running)
        (ext:external-process-wait external-process nil)
        (values status (external-process-%code external-process)))))

;;;
;;; Backwards compatible SI:SYSTEM call. We avoid ANSI C system()
;;; because we are consuming the process wait status using a SIGCHLD
;;; handler -- this breaks some C libraries out there (OS X 32 bit).
;;;
(defun system (cmd-string)
  (let ((shell "/bin/sh")
        (option "-c"))
    #+windows
    (let ((comspec (getenv "ComSpec")))
      (when comspec
        (setf shell comspec
              option "/c")))
    (nth-value 1 (run-program shell (list option cmd-string)
                              :wait t :output t :input t
                              :error t))))

