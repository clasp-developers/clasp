
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;     cleavir tests
;;;
;;; Running slime from bclasp+cleavir - don't load inline.lsp or auto-compile
;;;  --- Testing defun-inline-hook
(print "Starting")
(progn
  (load "sys:kernel;clasp-builder.lsp")
  (core::compile-addons)
  (core::link-addons)
  (format t "Done building addons~%"))

(progn ;; Set up everything for building cclasp from bclasp with auto-compile
  (format t "Loading ASDF system~%")
  (finish-output)
  (time (require :asdf))
  (load "~/quicklisp/setup.lisp")
  (load "sys:local-asdf-config.lisp"))

(progn
  (format t "Loading :clasp-cleavir system~%")
  (finish-output)
  (time (asdf:load-system "clasp-cleavir"))
  (format t "Done  pid = ~a~%"  (core:getpid)))

;;; --- If you want inlining compile this
(let ((cmp:*cleavir-compile-hook* 'clasp-cleavir::cclasp-compile*)
      (cmp:*cleavir-compile-file-hook* 'clasp-cleavir::cclasp-loop-read-and-compile-file-forms)
      (core:*use-cleavir-compiler* t)
      (core:*eval-with-env-hook* 'clasp-cleavir::cclasp-eval))
  (setf *print-pretty* nil)
  (format t "Loading inline.lisp~%")
  (finish-output)
  (load "sys:kernel;cleavir;inline.lisp" :print t)
  (format t "Done loading inline.lisp~%"))


(clasp-cleavir:cleavir-compile-file "sys:kernel;clos;builtin.lsp" :print t)


;;; ------   Compile using forms
(clasp-cleavir:cleavir-compile-file "sys:tests;ta.lsp")

;;; ------   Compile using CSTs
(let ((clasp-cleavir::*interactive-debug* nil)
      (clasp-cleavir::*use-cst* t))
  (clasp-cleavir:cleavir-compile-file "sys:tests;ta.lsp"))


;;; ------   Compile using CSTs
(let ((clasp-cleavir::*interactive-debug* nil)
      (clasp-cleavir::*use-cst* t))
  (clasp-cleavir:cleavir-compile-file "sys:kernel;lsp;foundation.lsp"))

(let ((clasp-cleavir::*interactive-debug* nil)
      (clasp-cleavir::*use-cst* t))
  (clasp-cleavir:cleavir-compile-file "sys:tests;tc.lsp"))


(let ((clasp-cleavir::*interactive-debug* nil)
      (clasp-cleavir::*use-cst* t))
  (with-input-from-string (sin "(defun simple-program-error (e1 &rest args)
  (eval `(error ,e1 ,@args)))")
                           (clasp-cleavir:cleavir-compile-file sin :output-file "/tmp/out.fasl")))



(make-pathname :type "fasl" :defaults "~/Development/cst-clasp/src/lisp/kernel/tag/min-start.lsp")

(progn
  (defun compile-one (fn)
    (format t "Compiling ~a~%" fn)
    (clasp-cleavir:cleavir-compile-file fn :output-file (merge-pathnames #P"/tmp/temp/" (make-pathname :type "fasl" :defaults fn))))

  (time
   (let ((clasp-cleavir::*use-cst* t))
     (mapc (lambda (fn) (compile-one fn))
           '(
             "~/Development/cst-clasp/src/lisp/kernel/tag/min-start.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/init.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/tag/after-init.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/runtime-info.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/jit-setup.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/clsymbols.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/packages.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/foundation.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/export.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/defmacro.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/helpfile.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/evalmacros.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/claspmacros.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/source-transformations.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/testing.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/arraylib.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/setf.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/listlib.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/mislib.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/defstruct.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/predlib.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/cdr-5.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/seq.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/cmuutil.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/seqmacros.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/seqlib.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/iolib.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/logging.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/sharpmacros.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/trace.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/backtrace.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmpexports.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmpsetup.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmpglobals.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmputil.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmpintrinsics.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/primitives.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmpir.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmpeh.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/debuginfo.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/codegen-vars.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/arguments.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmplambda.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmprunall.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmpliteral.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/typeq.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmpfastgf.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/codegen-special-form.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/codegen.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/compile.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/codegen-toplevel.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/compile-file.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/disassemble.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/external-clang.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmpname.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmpbundle.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmprepl.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/tag/min-pre-epilogue.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/epilogue-aclasp.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/tag/min-end.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/tag/bclasp-start.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmpwalk.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/assert.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/numlib.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/describe.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/module.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/loop2.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/opt.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/opt-type.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/opt-sequence.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/opt-cons.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/shiftf-rotatef.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/assorted.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/packlib.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/defpackage.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/format.lsp" 
             "~/Development/cst-clasp/src/lisp/kernel/lsp/mp.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/clos/package.lsp"                             
             "~/Development/cst-clasp/src/lisp/kernel/clos/hierarchy.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/clos/cpl.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/clos/std-slot-value.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/clos/slot.lsp"
           ;;                             "~/Development/cst-clasp/src/lisp/kernel/clos/boot.lsp" ;
             "~/Development/cst-clasp/src/lisp/kernel/clos/kernel.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/clos/closfastgf.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/clos/satiation.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/clos/method.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/clos/combin.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/clos/std-accessors.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/clos/defclass.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/clos/slotvalue.lsp"

             "~/Development/cst-clasp/src/lisp/kernel/clos/standard.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/clos/builtin.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/clos/change.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/clos/stdmethod.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/clos/generic.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/clos/fixup.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/clos/extraclasses.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/source-location.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/defvirtual.lsp"
             ||#
             "~/Development/cst-clasp/src/lisp/kernel/clos/conditions.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/clos/print.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/clos/streams.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/pprint.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/clos/inspect.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/fli.lsp"
             "~/Development/cst-clasp/src/lisp/modules/sockets/sockets.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/top.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/cmp/export-to-cleavir.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/tag/pre-epilogue-bclasp.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/lsp/epilogue-bclasp.lsp"
             "~/Development/cst-clasp/src/lisp/kernel/tag/bclasp.lsp")))))



;;; ------------------------------------------------------------
;;; ------------------------------------------------------------
;;; ------------------------------------------------------------
;;; ------------------------------------------------------------
;;;   Lambda list parsing
;;;
(progn
  (defparameter *l* (cst:cst-from-expression '(a b c)))
  (defparameter *r* (cst:cst-from-expression '(&rest x)))
  (defparameter *vr* (cst:cst-from-expression '(core:&va-rest x)))
  (defparameter *wvr* (cst:cst-from-expression '(&rest y core:&va-rest x)))
  (defparameter *comp* (cst:cst-from-expression '(a b c core:&va-rest r &key x y)))
  (defparameter *original-llg*   (cl:cons '(cst:target cst::<- cst::ordinary-lambda-list) cst:*standard-grammar*))
  )

(cst:parse-ordinary-lambda-list clasp-cleavir::*clasp-system* *l* :error-p nil)

;;; Use the original rules and a simple &rest
(cst::parse-top-level clasp-cleavir:*clasp-system* *original-llg* 'cst:ordinary-lambda-list *r* :error-p nil)

;;; Use the new rules and a simple &rest
(cst::parse-top-level clasp-cleavir:*clasp-system* clasp-cleavir::*clasp-ordinary-lambda-list-grammar* 'cst::ordinary-lambda-list *r* :error-p nil)

;;; Use the new rules and a core:&va-rest
(cst::parse-top-level clasp-cleavir:*clasp-system* clasp-cleavir::*clasp-ordinary-lambda-list-grammar* 'cst::ordinary-lambda-list *vr* :error-p nil)

;;; Use the new rules and a broken core:&va-rest
(cst::parse-top-level clasp-cleavir:*clasp-system* clasp-cleavir::*clasp-ordinary-lambda-list-grammar* 'cst::ordinary-lambda-list *wvr* :error-p t)

;;; Use the new rules and a complicated ll
(cst::parse-top-level clasp-cleavir:*clasp-system* clasp-cleavir::*clasp-ordinary-lambda-list-grammar* 'cst::ordinary-lambda-list *comp* :error-p t)



clasp-cleavir::*use-cst*

(trace clasp-cleavir::layout-procedure)
(trace clasp-cleavir::layout-basic-block)
(trace clasp-cleavir::translate-simple-instruction)
(trace clasp-cleavir::translate-branch-instruction)


(defun read-one-file (pn)
  (with-open-file (fin pn)
    (loop for cst = (sicl-reader:cst-read fin nil :eof)
          until (eq cst :eof)
          do (format t "CST -> ~a~%" cst)
             do (finish-output))))
(let ((*package* (find-package :clos)))
  (read-one-file "sys:kernel;clos;hierarchy2.lsp"))




(trace sicl-reader::read-common)
(untrace sicl-reader::sharpsign-dot)

(apropos "sharpsign-dot")
(trace sicl-reader::sharpsign-dot)

(defun read-multiple-files (files)
  (dolist (f files)
    (format t "Reading file: ~a~%" f)
    (clasp-cleavir:cleavir-compile-file f)))

(time (read-multiple-files '("~/Development/cst-clasp/src/lisp/kernel/tag/min-start.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/init.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/tag/after-init.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/runtime-info.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/jit-setup.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/clsymbols.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/packages.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/foundation.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/export.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/defmacro.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/helpfile.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/evalmacros.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/claspmacros.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/source-transformations.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/testing.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/arraylib.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/setf.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/listlib.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/mislib.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/defstruct.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/predlib.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/cdr-5.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/seq.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/cmuutil.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/seqmacros.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/seqlib.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/iolib.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/logging.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/sharpmacros.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/trace.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/backtrace.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmpexports.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmpsetup.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmpglobals.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmputil.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmpintrinsics.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/primitives.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmpir.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmpeh.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/debuginfo.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/codegen-vars.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/arguments.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmplambda.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmprunall.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmpliteral.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/typeq.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmpfastgf.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/codegen-special-form.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/codegen.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/compile.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/codegen-toplevel.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/compile-file.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/disassemble.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/external-clang.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmpname.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmpbundle.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmprepl.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/tag/min-pre-epilogue.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/epilogue-aclasp.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/tag/min-end.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/tag/bclasp-start.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/cmpwalk.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/assert.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/numlib.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/describe.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/module.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/loop2.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/opt.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/opt-type.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/opt-sequence.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/opt-cons.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/shiftf-rotatef.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/assorted.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/packlib.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/defpackage.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/format.lsp" 
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/mp.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/clos/package.lsp"                             
                             "~/Development/cst-clasp/src/lisp/kernel/clos/hierarchy.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/clos/cpl.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/clos/std-slot-value.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/clos/slot.lsp"
;;                             "~/Development/cst-clasp/src/lisp/kernel/clos/boot.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/clos/kernel.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/clos/closfastgf.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/clos/satiation.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/clos/method.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/clos/combin.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/clos/std-accessors.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/clos/defclass.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/clos/slotvalue.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/clos/standard.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/clos/builtin.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/clos/change.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/clos/stdmethod.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/clos/generic.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/clos/fixup.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/clos/extraclasses.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/source-location.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/defvirtual.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/clos/conditions.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/clos/print.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/clos/streams.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/pprint.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/clos/inspect.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/fli.lsp"
                             "~/Development/cst-clasp/src/lisp/modules/sockets/sockets.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/top.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/cmp/export-to-cleavir.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/tag/pre-epilogue-bclasp.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/lsp/epilogue-bclasp.lsp"
                             "~/Development/cst-clasp/src/lisp/kernel/tag/bclasp.lsp")))

(with-input-from-string (sin "`(1 2 3 4)")
  (sicl-reader:cst-read sin nil nil))




(load "sys:tests;tt-00001-preoptimize.ll")
(core::foo)

(clasp-cleavir:cleavir-compile-file "sys:tests;ta.lsp")
(trace clasp-cleavir::layout-procedure)
(trace clasp-cleavir::layout-basic-block)
(trace clasp-cleavir::translate-simple-instruction)
(trace clasp-cleavir::translate-branch-instruction)

(trace clasp-cleavir::translate-datum)
(trace mapcar)


(cleavir-generate-ast:generate-ast '(lambda () 1)  *clasp-env* *clasp-system*)


(let ((cmp:*compile-file-debug-dump-module* t))
  (clasp-cleavir:cleavir-compile-file "sys:tests;tt.lsp"
                                      :optimize nil))


(clasp-cleavir:cleavir-compile-file "sys:kernel;lsp;mislib.lsp" :debug t)

(clasp-cleavir:cleavir-compile 'foo '(lambda (x y) (+ x y)))



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





