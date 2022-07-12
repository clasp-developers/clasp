(eval-when (:execute :compile-toplevel :load-toplevel)
  (core::select-package :core))

(defun entry-filename (filename-or-cons)
  "If filename-or-cons is a list then the first entry is a filename"
  (if (consp filename-or-cons)
      (car filename-or-cons)
      filename-or-cons))

(defun entry-compile-file-options (entry)
  (if (consp entry)
      (cadr entry)
      nil))

(defun delete-init-file (entry &key (really-delete t) stage)
  (let* ((module (entry-filename entry))
         (bitcode-path (build-pathname module :bitcode stage)))
    (if (probe-file bitcode-path)
        (if really-delete
            (progn
              (bformat t "     Deleting bitcode: %s%N" bitcode-path)
              (delete-file bitcode-path))))))


;; I need to search the list rather than using features because *features* may change at runtime
(defun default-target-backend (&optional given-stage)
  (let* ((stage (if given-stage
                    given-stage
                    (default-target-stage)))
         (garbage-collector (build-configuration))
         (target-backend (bformat nil "%s%s" stage garbage-collector)))
    target-backend))
(export 'default-target-backend)

(defvar *target-backend* (default-target-backend))
(export '*target-backend*)

(defun bitcode-exists-and-up-to-date (entry)
  (let* ((filename (entry-filename entry))
         (source-path (build-pathname filename))
         (bitcode-path (build-pathname filename cmp:*default-object-type*))
         (found-bitcode (probe-file bitcode-path)))
    (if found-bitcode
        (> (file-write-date bitcode-path)
           (file-write-date source-path))
        nil)))

(defun default-prologue-form (&optional features)
  `(progn
     ,@(mapcar #'(lambda (f) `(push ,f *features*)) features)
     (if (core:is-interactive-lisp)
         (bformat t "Starting %s ... loading image...%N" (lisp-implementation-version)))))

(export '*extension-startup-loads*) ;; ADDED: frgo, 2016-08-10
(defvar *extension-startup-loads* nil)

(export 'process-extension-loads)
(defun process-extension-loads ()
  (if (not (member :ignore-extensions *features*))
      (progn
        (mapcar #'(lambda (entry)
                    (if (eq (car entry) 'cl:load)
                        (load (cadr entry))
                        (let ((cmd (read-from-string (cdr entry))))
                          (apply (car cmd) (cdr cmd)))))
                core:*extension-startup-loads*)
        (mapcar #'(lambda (entry)
                    (funcall entry))
                core:*extension-startup-evals*))))

(export 'process-command-line-load-eval-sequence)
(defun process-command-line-load-eval-sequence ()
  (mapcar #'(lambda (entry)
              (if (eq (car entry) :load)
                  (load (cdr entry))
                  (let ((cmd (read-from-string (cdr entry))))
                    (eval cmd))))
          (core:command-line-load-eval-sequence)))

(export 'maybe-load-clasprc)
(defun maybe-load-clasprc ()
  "Maybe load the users startup code"
  (if (not (core:no-rc-p))
      (let ((clasprc (make-pathname :name (core:rc-file-name)
                                    :defaults (user-homedir-pathname))))
        (if (probe-file clasprc)
            (core:load-source clasprc)))))

(defun tpl-default-pathname-defaults-command ()
  (print *default-pathname-defaults*))

(defun tpl-change-default-pathname-defaults-dir-command (raw-dir)
  (let* ((corrected-dir (format nil "~a/" (string-right-trim "/" (string raw-dir))))
         (dir (pathname-directory (parse-namestring corrected-dir)))
         (pn-dir (mapcar #'(lambda (x) (if (eq x :up) :back x)) dir))
         (new-pathname (merge-pathnames (make-pathname :directory pn-dir) *default-pathname-defaults*))
         )
    (setq *default-pathname-defaults* new-pathname)))


;;; I moved the build system code out of init.lsp and
;;; put it in clasp-builder.lsp

(if (member :clasp-builder *features*)
    (load "source-dir:src;lisp;kernel;clasp-builder.lsp"))


(defun tpl-hook (cmd)
  (cond
    ((eq (car cmd) :pwd) (tpl-default-pathname-defaults-command))
    ((eq (car cmd) :cd) (tpl-change-default-pathname-defaults-dir-command (cadr cmd)))
    (t (bformat t "Unknown command %s%N" cmd))))

(setq *top-level-command-hook* #'tpl-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Start everything up
;;

(export 'core:top-level)

#-(or aclasp bclasp cclasp)
(eval-when (:execute)
  (process-command-line-load-eval-sequence)
  (if (core:noinform-p)
      nil
      (bformat t "Low level repl - in init.lsp%N"))
  (core:low-level-repl))

#-(or bclasp cclasp)
(eval-when (:execute :load-top-level)
  (bformat t "init.lsp  %N!\n!\n! Hello from the bottom of init.lsp - for some reason execution is passing through here\n!\n!\n"))
