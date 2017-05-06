(error "foo")

(defun gather 
(let ((CLOS::*TRAP* (cons t nil)))
  ;; Call c-a-m-u-c once to establish a call-history.  ECL bypasses it
  (print-object #'print-object *standard-output*)
  (print-object (core::make-restart) *standard-output*)
  (print-object (list 1 2 3) *standard-output*)
  (bformat t "*trap-recursive-dispatch-miss* -> %s\n" clos::*trap*)
  (print-address-of 'clos::*trap*)
  (defparameter *newgfs* (mapcar (lambda (gf) (cons gf (clos::calculate-strandh-dispatch-function gf))) (clos::all-generic-functions)))
  (dolist (ng *newgfs*) (clos:set-funcallable-instance-function (car ng) (cdr ng)))
  (push :strandh-dispatch *features*)
  (bformat t "------------done-------------\n"))

(let ((clos::*trap-recursive-dispatch* t))
  ;; Call c-a-m-u-c once to establish a call-history.  ECL bypasses it
  (clos::generic-function-compiled-dispatch-function #'print-object)
  (setf (clos::generic-function-compiled-dispatch-function #'print-object) t)
  (clos::compute-applicable-methods-using-classes #'print-object (list (find-class T) (find-class 'stream)))
  (clos:compute-effective-method #'clos:compute-effective-method (clos:generic-function-method-combination #'clos:compute-effective-method) (clos:generic-function-methods #'clos:compute-effective-method))
  (clos::generic-function-name #'print-object)
  (print-object #'print-object *standard-output*)
  (print-object (core::make-restart) *standard-output*)
  (defparameter *newgfs* (mapcar (lambda (gf) (cons gf (clos::calculate-strandh-dispatch-function gf))) (clos::all-generic-functions))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(push :enable-fastgf *features*)

;;; Running slime from bclasp+cleavir - don't load inline.lsp or auto-compile
;;;  --- Testing defun-inline-hook
(print "Starting")
(time (compile-file "sys:modules;asdf;build;asdf.lisp" :print t))
(progn ;; Set up everything for building cclasp from bclasp with auto-compile
  (format t "Loading ASDF system~%")
  (finish-output)
  (time (load "sys:modules;asdf;build;asdf.fasl"))
  #+(or)(time (require :asdf))
  (load "sys:local-asdf-config.lisp"))
(progn
  #+(or)(core::cclasp-features)
  (format t "Loading :clasp-cleavir system~%")
  (finish-output)
  (time (asdf:load-system "clasp-cleavir"))
  (format t "Done  pid = ~a~%"  (core:getpid)))

(apropos "dump-module")

(apropos "invalidated-dispatch-functions")

(clasp-cleavir:cleavir-compile 'foo '(lambda (x y) (+ x y)))ption
(time (clasp-cleavir:cleavir-compile-file "sys:kernel;lsp;predlib.lsp"))

(let ((clos::*monitor-dispatch* t)
      (clos::*dispatch-log* nil))
  (clasp-cleavir:cleavir-compile 'foo '(lambda (x y) (+ x y)))
  (print "------- dispatch-log----")
  (print (reverse clos::*dispatch-log*))
  nil)

(let ((clos::*monitor-dispatch* t)
      (clos::*dispatch-log* nil))
  (bar 1)
  (print "------- dispatch-log----")
  (print (reverse clos::*dispatch-log*))
  nil)

(cleavir-generate-ast:convert-special
(let ((clos::*monitor-dispatch* t)
      (clos::*dispatch-log* nil))
  (clasp-cleavir:cleavir-compile-file #P"sys:kernel;lsp;foundation.lsp" :print t)
  (print "------- dispatch-log----")
  (print clos::*dispatch-log*))





(time (clasp-cleavir:cleavir-compile 'myfoo '(lambda (x y) (+ x y))))



(dolist (gf (clos::all-generic-functions))
  (format t "~a  -> ~a~%" gf (clos::generic-function-compiled-dispatch-function gf)))




(apropos "header-stamp")
(apropos "get-instance-stamp")
(apropos "instance-class")
(core:get-instance-stamp (find-class 'cleavir-ir:dynamic-lexical-location))
(time (clasp-cleavir:cleavir-compile 'foo '(lambda (x y) (+ x y))))

(clos::generic-function-call-history #'cleavir-ir:name)
(clos::get-funcallable-instance-function #'cleavir-ir:name)



(clasp-cleavir:cleavir-compile-file "sys:kernel;lsp;pprint.lsp" :print t)

(clasp-cleavir:cleavir-compile-file "sys:kernel;lsp;pprint.lsp")

(clasp-cleavir:cleavir-compile-file "sys:tests;tc.lsp")

clasp-cleavir::*pvi*
(let ((clasp-cleavir:*debug-cleavir* t) (compiler:*compile-file-debug-dump-module* t)) (clasp-cleavir:cleavir-compile-file "sys:tests;td.lsp"))
(load "sys:tests;td.fasl")
(foo 0)


(clasp-cleavir:cleavir-compile 'foo '(lambda (priority) (declare (type (or fixnum cons) priority) (optimize (safety 2))) (identity priority)))


(clasp-cleavir:cleavir-compile
 'foo
 '(lambda (priority)
   (declare (type (or fixnum cons) priority) (optimize (safety 2)))
   (core:debug-message "Hi there")
   (identity priority)
   (core:debug-message "me again"))
 :debug t)



(apropos "compile-file-debug-dump")
(let ((cmp:*compile-file-debug-dump-module* t)) (clasp-cleavir:cleavir-compile-file "sys:tests;td.lsp"))
(load "sys:tests;td.fasl")

(foo 123)

(clasp-cleavir:cleavir-compile
 'foo '(lambda (&optional (priority 0))
        (declare (type real priority)
         (optimize (safety 2) (speed 1) (debug 1) (space 1)))
        (unless (typep priority 'real)
          (error "problem"))
        (print priority))
 :debug t)

(in-package :core)
(clasp-cleavir:cleavir-compile 'foo '(lambda (kind &optional stream)
                                      (declare (type (member :linear :miser :fill :mandatory) kind)
                                       (type (or stream (member t nil)) stream)
                                       (values null)
                                       (ext:check-arguments-type)
                                       #.+ecl-safe-declarations+)
                                      (let ((stream (case stream
                                                      ((t) *terminal-io*)
                                                      ((nil) *standard-output*)
                                                      (t stream))))
                                        (when (and (pretty-stream-p stream) *print-pretty*)
                                          (enqueue-newline stream kind)))
                                      nil)
                               :debug t)








(clasp-cleavir:cleavir-compile 'foo '(lambda (x) (declare (optimize (safety 1) (speed 0))) (car (the cons x))) :debug t)

(clasp-cleavir:cleavir-compile nil '(lambda (x) (declare (optimize (safety 0) (speed 1))) (car (the cons x))) :debug t)

(foo :x)

(time (asdf:load-system "clasp-cleavir"))

(clasp-cleavir:cleavir-compile-file "sys:tests;td.lsp")


(time (require :clasp-cleavir))

(load "sys:kernel;cleavir;auto-compile.lisp")
(load "sys:kernel;cleavir;inline.lisp")


(clasp-cleavir:cleavir-compile 'foo '(lambda (x y) (+ x y)))

(print #'clasp-cleavir:translate-simple-instruction)


(foo 1 2)



(foo 1 2)

(let ((cmp:*compile-file-debug-dump-module* t)) (clasp-cleavir:cleavir-compile-file "sys:tests;ta.lsp"))
(load "sys:tests;ta.fasl")
(foo)

(trace clasp-cleavir-ast:hoist-load-time-value)


(foo 0)
(let ((cmp:*compile-debug-dump-module* t)) (clasp-cleavir:cleavir-compile 'foo '(lambda (x) (declare (core:lambda-name foo)) (list x x))))
(disassemble 'foo)

(fdefinition 'foo)
(clasp-cleavir:cleavir-compile 'foo '(lambda (x y) (declare (fixnum x y)) (if (< x y) 1 2)))


*features*
(clasp-cleavir:cleavir-compile-file "sys:modules;asdf;build;asdf.lisp" :print t)
(clasp-cleavir:cleavir-compile-file "~/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Type-inference/type-descriptor.lisp" :print t)

(print "Hello there")

(defun my-hir-form (form)
  (let* ((ast (cleavir-generate-ast:generate-ast
               form
               clasp-cleavir:*clasp-env*
               clasp-cleavir:*clasp-system*))
         (hoisted-ast (clasp-cleavir-ast:hoist-load-time-value ast))
         (init-instr (cleavir-ast-to-hir:compile-toplevel hoisted-ast))
         (clasp-inst *clasp-system*))
    (clasp-cleavir:convert-funcalls init-instr)
    ;; my-hir-transformations
    (draw-hir init-instr #P"/tmp/hir-before.dot") ;; comment out
    (let ((types (cleavir-type-inference:infer-types init-instr)))
      (cleavir-type-inference::prune-typeqs init-instr types)
      (cleavir-type-inference::delete-the init-instr)
      (cleavir-hir-transformations:process-captured-variables init-instr)
      (draw-hir init-instr #P"/tmp/hir-after-ti.dot") ;; comment out
      #+(or)(clasp-cleavir::draw-hir init-instr))))

(clasp-cleavir:cleavir-compile
 'bar
 '(lambda (x)
   (if (cleavir-primop:typeq x fixnum)
       (if (cleavir-primop:typeq x fixnum)
           10
           20)
       25)) :debug t)

clasp-cleavir::*hir-types*
(clasp-cleavir:cleavir-compile
 'foo
 '(let ((a 1))
   (lambda (x)
     (declare (fixnum x))
     (if (cleavir-primop:typeq x fixnum)
         a
         20)))
 :debug t)

(require :cleavir-type-inference)
(defparameter *x*
  (my-hir-form '(let ((a 1))
                 (lambda (x)
                   (declare (fixnum x))
                   (if (cleavir-primop:typeq x fixnum)
                       a
                       20)))))


(clasp-cleavir:cleavir-compile-file "sys:tests;tt.lsp" :print t)





