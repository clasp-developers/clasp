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

;;;; CMPTEST  Functions for compiler test.

(in-package "COMPILER")

(defun self-compile ()
 (with-open-file (log "lsplog" :direction :output)
  (let ((*standard-output* (make-broadcast-stream *standard-output* log)))

;       (self-compile2 "cmpbind")
;       (self-compile2 "cmpblock")
;       (self-compile2 "cmpcall")
;       (self-compile2 "cmpcatch")
       (self-compile2 "cmpenv")
;       (self-compile2 "cmpeval")
;       (self-compile2 "cmpflet")
;       (self-compile2 "cmpfun")
;       (self-compile2 "cmpif")
;       (self-compile2 "cmpinline")
       (self-compile2 "cmplabel")
;       (self-compile2 "cmplam")
;       (self-compile2 "cmplet")
;       (self-compile2 "cmploc")
;       (self-compile2 "cmpmap")
;       (self-compile2 "cmpmulti")
;       (self-compile2 "cmpspecial")
;       (self-compile2 "cmptag")
;       (self-compile2 "cmptop")
;       (self-compile2 "cmptype")
       (self-compile2 "cmputil")
;       (self-compile2 "cmpvar")
;       (self-compile2 "cmpvs")
;       (self-compile2 "cmpwt")

       ))
 t)

(defun setup ()

;  (allocate 'cons 800)
;  (allocate 'string 256)
;  (allocate 'structure 32)
;  (allocate-relocatable-pages 128)

;  (load "cmpinline.lsp")
  (load "cmputil.lsp")
;  (load "cmptype.lsp")

;  (load "cmpbind.lsp")
;  (load "cmpblock.lsp")
  (load "cmpcall.lsp")
;  (load "cmpcatch.lsp")
;  (load "cmpenv.lsp")
;  (load "cmpeval.lsp")
  (load "cmpflet.lsp")
;  (load "cmpfun.lsp")
;  (load "cmpif.lsp")
  (load "cmplabel.lsp")
;  (load "cmplam.lsp")
;  (load "cmplet.lsp")
  (load "cmploc.lsp")
;  (load "cmpmain.lsp")
;  (load "cmpmap.lsp")
;  (load "cmpmulti.lsp")
;  (load "cmpspecial.lsp")
;  (load "cmptag.lsp")
  (load "cmptop.lsp")
;  (load "cmpvar.lsp")
;  (load "cmpvs.lsp")
;  (load "cmpwt.lsp")

;  (load "lfun_list")
;  (load "cmpopt.lsp")

  )

(defun cli () (process ":cli.pr"))

(defun load-fasl ()

  (load "cmpinline")
  (load "cmputil")
  (load "cmpbind")
  (load "cmpblock")
  (load "cmpcall")
  (load "cmpcatch")
  (load "cmpenv")
  (load "cmpeval")
  (load "cmpflet")
  (load "cmpfun")
  (load "cmpif")
  (load "cmplabel")
  (load "cmplam")
  (load "cmplet")
  (load "cmploc")
  (load "cmpmap")
  (load "cmpmulti")
  (load "cmpspecial")
  (load "cmptag")
  (load "cmptop")
  (load "cmptype")
  (load "cmpvar")
  (load "cmpvs")
  (load "cmpwt")

  (load "cmpmain.lsp")
  (load "lfun_list.lsp")
  (load "cmpopt.lsp")

  )

(setq *macroexpand-hook* 'funcall)

(defun self-compile1 (file)
  (prin1 file) (terpri)
  (compile-file1 file
    :fasl-file t :c-file t :h-file t :data-file t :ob-file t :system-p t))

(defun self-compile2 (file)
  (prin1 file) (terpri)
  (compile-file1 file
    :fasl-file t :c-file t :h-file t :data-file t :ob-file t :system-p t)
  (prin1 (load file)) (terpri))

(defvar *previous-form* nil)

(defun cmp (form)
  (setq *previous-form* form)
  (again))

(defun again ()
  (init-env)
  (print *previous-form*)
  (terpri)
  (setq *compiler-output1* *standard-output*)
  (setq *compiler-output2* *standard-output*)
  (t1expr *previous-form*)
  (catch *cmperr-tag* (ctop-write "test"))
  t)

;(defun make-cmpmain-for-unix ()
;       (print "unixmain")
;       (format t "~&The old value of *FEATURES* is ~s." *features*)
;       (let ((*features* '(:unix :common :ecl)))
;            (format t "~&The new value of *FEATURES* is ~s." *features*)
;            (init-env)
;            (compile-file1 "cmpmain.lsp"
;                           :output-file "unixmain"
;                           :c-file t
;                           :h-file t
;                           :data-file t
;                           :system-p t
;                           ))
;       (format t "~&The resumed value of *FEATURES* is ~s." *features*)
;       )

(defun compiler-make-ufun ()
  (make-ufun '(
  "cmpbind.lsp"
  "cmpblock.lsp"
  "cmpcall.lsp"
  "cmpcatch.lsp"
  "cmpenv.lsp"
  "cmpeval.lsp"
  "cmpflet.lsp"
  "cmpfun.lsp"
  "cmpif.lsp"
  "cmpinline.lsp"
  "cmplabel.lsp"
  "cmplam.lsp"
  "cmplet.lsp"
  "cmploc.lsp"
  "cmpmain.lsp"
  "cmpmap.lsp"
  "cmpmulti.lsp"
  "cmpspecial.lsp"
  "cmptag.lsp"
  "cmptop.lsp"
  "cmptype.lsp"
  "cmputil.lsp"
  "cmpvar.lsp"
  "cmpvs.lsp"
  "cmpwt.lsp"

  ))

  t)

(defun remrem ()
       (do-symbols (x (find-package 'lisp))
                   (rem-sysprop x ':inline-always)
                   (rem-sysprop x ':inline-safe)
                   (rem-sysprop x ':inline-unsafe))
       (do-symbols (x (find-package 'system))
                   (rem-sysprop x ':inline-always)
                   (rem-sysprop x ':inline-safe)
                   (rem-sysprop x ':inline-unsafe)))
(defun ckck ()
       (do-symbols (x (find-package 'lisp))
                   (when (or (get-sysprop x ':inline-always)
                             (get-sysprop x ':inline-safe)
                             (get-sysprop x ':inline-unsafe))
                         (print x)))
       (do-symbols (x (find-package 'si))
                   (when (or (get-sysprop x ':inline-always)
                             (get-sysprop x ':inline-safe)
                             (get-sysprop x ':inline-unsafe))
                         (print x))))

(defun make-cmpopt (&aux (eof (cons nil nil)))
  (with-open-file (in "cmpopt.db")
    (with-open-file (out "cmpopt.lsp" :direction :output)
      (print '(in-package "COMPILER") out)
      (terpri out) (terpri out)
      (do ((x (read in nil eof) (read in nil eof)))
          ((eq x eof))
          (apply #'(lambda (property return-type side-effectp new-object-p
                                     name arg-types body)
                     (when (stringp body)
                       (do ((i 0 (1+ i))
                            (l nil)
                            (l1 nil))
                           ((>= i (length body))
                            (when l1
                              (setq body
                                    (concatenate 'string
                                                 "@"
                                                 (nreverse l1)
                                                 ";"
                                                 body))))
                         (when (char= (aref body i) #\#)
                           (incf i)
                           (cond ((member (aref body i) l)
                                  (pushnew (aref body i) l1))
                                 (t (push (aref body i) l))))))
                     (print
                      `(push '(,arg-types ,return-type ,side-effectp
                                          ,new-object-p ,body)
                             (get-sysprop ',name ',property))
                      out))
                 (cdr x)))
      (terpri out))))
