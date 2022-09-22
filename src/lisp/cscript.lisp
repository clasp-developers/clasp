(defun add-aclasp-sources (&rest rest &key target neo bytecode wrappers &allow-other-keys)
  (declare (ignore rest))
  (cond ((or neo bytecode)
         (k:sources target
                    #~"kernel/stage/0-begin.lisp"
                    #~"kernel/stage/4-begin.lisp"))
        (t
         (k:sources target
                    #~"kernel/tag/start.lisp")))
  (k:sources target
             #~"kernel/lsp/prologue.lisp")
  (when wrappers
    (k:sources target
               #~"kernel/lsp/direct-calls.lisp"
               #@"cl-wrappers.lisp"))
  (unless bytecode
    (k:sources target
               #~"kernel/tag/min-start.lisp"))
  (k:sources target
             #~"kernel/init.lisp")
  (unless bytecode
    (k:sources target
               #~"kernel/tag/after-init.lisp"))
  (k:sources target
             #~"kernel/cmp/runtime-info.lisp"
             #~"kernel/lsp/sharpmacros.lisp"
             #~"kernel/cmp/jit-setup.lisp"
             #~"kernel/clsymbols.lisp"
             #~"kernel/lsp/packages.lisp"
             #~"kernel/lsp/foundation.lisp"
             #~"kernel/lsp/export.lisp"
             #~"kernel/lsp/defmacro.lisp"
             #~"kernel/lsp/helpfile.lisp"
             #~"kernel/lsp/evalmacros.lisp"
             #~"kernel/lsp/claspmacros.lisp"
             #~"kernel/lsp/source-transformations.lisp"
             #~"kernel/lsp/arraylib.lisp"
             #~"kernel/lsp/setf.lisp"
             #~"kernel/lsp/listlib.lisp"
             #~"kernel/lsp/mislib.lisp"
             #~"kernel/lsp/defstruct.lisp"
             #~"kernel/lsp/predlib.lisp"
             #~"kernel/lsp/cdr-5.lisp"
             #~"kernel/lsp/cmuutil.lisp"
             #~"kernel/lsp/seqmacros.lisp"
             #~"kernel/lsp/seq.lisp"
             #~"kernel/lsp/seqlib.lisp"
             #~"kernel/lsp/iolib.lisp"
             #~"kernel/lsp/trace.lisp"
             #~"kernel/lsp/debug.lisp"
             #~"kernel/cmp/cmpexports.lisp"
             #~"kernel/cmp/cmpsetup.lisp"
             #~"kernel/cmp/cmpglobals.lisp"
             #~"kernel/cmp/cmputil.lisp"
             #~"kernel/cmp/cmpintrinsics.lisp"
             #~"kernel/cmp/primitives.lisp"
             #~"kernel/cmp/cmpir.lisp"
             #~"kernel/cmp/cmpeh.lisp"
             #~"kernel/cmp/debuginfo.lisp"
             #~"kernel/cmp/arguments.lisp"
             #~"kernel/cmp/cmplambda.lisp"
             #~"kernel/cmp/cmprunall.lisp"
             #~"kernel/cmp/cmpliteral.lisp"
             #~"kernel/cmp/typeq.lisp"
             #~"kernel/cmp/codegen-special-form.lisp")
  (unless nil;bytecode
    (k:sources target
               #~"kernel/cmp/codegen.lisp"
               #~"kernel/cmp/compile.lisp"))
  (k:sources target
             #~"kernel/cmp/compile-file.lisp"
             #~"kernel/cmp/external-clang.lisp"
             #~"kernel/cmp/cmpname.lisp"
             #~"kernel/cmp/cmpbundle.lisp")
  (cond ((or neo bytecode)
         (k:sources target
                    #~"kernel/stage/0-end.lisp"
                    #~"kernel/stage/1-begin.lisp"
                    #~"kernel/stage/3-begin.lisp"
                    )
         (when bytecode
           (k:sources target
                      #~"kernel/cmp/bytecode-compile.lisp"
                      #~"kernel/cmp/bytecode-reference.lisp"
                      ))
         (k:sources target
                    #~"kernel/stage/3-end.lisp"
                    #~"kernel/stage/1-end.lisp"
                    #~"kernel/stage/2-begin.lisp")
         (when bytecode
           (k:sources target
                      #~"kernel/cmp/cmprepl-bytecode.lisp"))
         (k:sources target
                    #~"kernel/stage/2-end.lisp"))
        (t
         (k:sources target
                    #~"kernel/tag/min-pre-epilogue.lisp"
                    #~"kernel/lsp/epilogue-aclasp.lisp"
                    #~"kernel/tag/min-end.lisp"))))
    
(defun add-bclasp-sources (&rest rest &key target neo bytecode &allow-other-keys)
  (apply #'add-aclasp-sources rest)
  (unless (or neo bytecode)
    (k:sources target
               #~"kernel/tag/bclasp-start.lisp"))
  (k:sources target
             #~"kernel/cmp/cmpwalk.lisp"
             #~"kernel/lsp/assert.lisp"
             #~"kernel/lsp/iolib.lisp"
             #~"kernel/lsp/numlib.lisp"
             #~"kernel/lsp/describe.lisp"
             #~"kernel/lsp/module.lisp"
             #~"kernel/lsp/loop2.lisp"
             #~"kernel/cmp/disassemble.lisp"
             #~"kernel/cmp/opt/opt.lisp" ; need loop
             #~"kernel/cmp/opt/opt-character.lisp"
             #~"kernel/cmp/opt/opt-number.lisp"
             #~"kernel/cmp/opt/opt-type.lisp"
             #~"kernel/cmp/opt/opt-control.lisp"
             #~"kernel/cmp/opt/opt-sequence.lisp"
             #~"kernel/cmp/opt/opt-cons.lisp"
             #~"kernel/cmp/opt/opt-array.lisp"
             #~"kernel/cmp/opt/opt-object.lisp"
             #~"kernel/cmp/opt/opt-condition.lisp"
             #~"kernel/cmp/opt/opt-print.lisp"
             #~"kernel/lsp/shiftf-rotatef.lisp"
             #~"kernel/lsp/assorted.lisp"
             #~"kernel/lsp/packlib.lisp"
             #~"kernel/lsp/defpackage.lisp"
             #~"kernel/lsp/format.lisp"
             #~"kernel/lsp/mp.lisp"
             #~"kernel/lsp/atomics.lisp"
             ;#~"kernel/stage/4-end.lisp"
             #~"kernel/clos/package.lisp"
             #~"kernel/clos/static-gfs/package.lisp"
             #~"kernel/clos/static-gfs/flag.lisp"
             #~"kernel/clos/static-gfs/constructor.lisp"
             #~"kernel/clos/static-gfs/reinitializer.lisp"
             #~"kernel/clos/static-gfs/changer.lisp"
             #~"kernel/clos/hierarchy.lisp"
             #~"kernel/clos/cpl.lisp"
             #~"kernel/clos/std-slot-value.lisp"
             #~"kernel/clos/slot.lisp"
             #~"kernel/clos/boot.lisp"
             #~"kernel/clos/kernel.lisp"
             #~"kernel/clos/outcome.lisp"
             #~"kernel/clos/discriminate.lisp"
             #~"kernel/clos/dtree.lisp"
             #~"kernel/clos/effective-accessor.lisp"
             #~"kernel/clos/closfastgf.lisp"
             #~"kernel/clos/satiation.lisp"
             #~"kernel/clos/method.lisp"
             #~"kernel/clos/combin.lisp"
             #~"kernel/clos/std-accessors.lisp"
             #~"kernel/clos/defclass.lisp"
             #~"kernel/clos/slotvalue.lisp"
             #~"kernel/clos/standard.lisp"
             #~"kernel/clos/builtin.lisp"
             #~"kernel/clos/change.lisp"
             #~"kernel/clos/stdmethod.lisp"
             #~"kernel/clos/generic.lisp"
             #~"kernel/clos/fixup.lisp"
             #~"kernel/clos/static-gfs/cell.lisp"
             #~"kernel/clos/static-gfs/effective-method.lisp"
             #~"kernel/clos/static-gfs/svuc.lisp"
             #~"kernel/clos/static-gfs/shared-initialize.lisp"
             #~"kernel/clos/static-gfs/initialize-instance.lisp"
             #~"kernel/clos/static-gfs/allocate-instance.lisp"
             #~"kernel/clos/static-gfs/make-instance.lisp"
             #~"kernel/clos/static-gfs/compute-constructor.lisp"
             #~"kernel/clos/static-gfs/dependents.lisp"
             #~"kernel/clos/static-gfs/compiler-macros.lisp"
             #~"kernel/clos/static-gfs/reinitialize-instance.lisp"
             #~"kernel/clos/static-gfs/update-instance-for-different-class.lisp"
             #~"kernel/clos/static-gfs/change-class.lisp"
             #~"kernel/lsp/source-location.lisp"
             #~"kernel/lsp/defvirtual.lisp"
             #~"kernel/clos/streams.lisp"
             #~"kernel/lsp/pprint.lisp"
             #~"kernel/lsp/format-pprint.lisp"
             #~"kernel/clos/conditions.lisp"
             #~"kernel/clos/print.lisp"
             #~"kernel/clos/sequences.lisp"
             #~"kernel/cmp/compiler-conditions.lisp"
             #~"kernel/lsp/packlib2.lisp"
             #~"kernel/clos/inspect.lisp"
             #~"kernel/lsp/fli.lisp"
             #~"kernel/lsp/posix.lisp"
             #~"modules/sockets/sockets.lisp"
             #~"kernel/lsp/top.lisp"
             #~"kernel/stage/4-end.lisp")
  (unless (or neo bytecode)
    (k:sources target
               #~"kernel/tag/pre-epilogue-bclasp.lisp"
               #~"kernel/lsp/epilogue-bclasp.lisp"
               #~"kernel/tag/bclasp.lisp")))

(defun add-cclasp-sources (&rest rest &key target neo bytecode &allow-other-keys)
  (apply #'add-bclasp-sources rest)
  (k:sources target
             #~"kernel/stage/5-begin.lisp"  
             (k:make-source "cclasp-translations.lisp" :variant-generated)
             :clasp-cleavir
             #~"kernel/lsp/queue.lisp" ;; cclasp sources
             #~"kernel/cleavir/auto-compile.lisp"
             #~"kernel/cleavir/bytecode-adaptor.lisp"
             ;; fixme2022 #~"kernel/cleavir/inline.lisp" ;; restore this
             #~"kernel/lsp/generated-encodings.lisp"
             #~"kernel/lsp/process.lisp"
             #~"kernel/lsp/encodings.lisp"
             #~"kernel/lsp/cltl2.lisp")
  (when (eq target :cclasp)
    (k:sources target
               (k:make-source "cclasp-immutable.lisp" :variant-generated)))
  (if (or neo bytecode)
      (k:sources target
                 #~"kernel/stage/5-end.lisp")
      (k:sources target
                 #~"kernel/tag/pre-epilogue-cclasp.lisp"))
  (k:sources target
             #~"kernel/cmp/compile-file-parallel.lisp"
             #~"kernel/lsp/epilogue-cclasp.lisp")
  (unless (or neo bytecode)
    (k:sources target
               #~"kernel/tag/cclasp.lisp")))

(defun add-eclasp-sources (&rest rest &key target &allow-other-keys)
  (apply #'add-cclasp-sources rest)
  (when (eq target :eclasp)
    (k:sources target
               (k:make-source "eclasp-immutable.lisp" :variant-generated)))
  (k:sources target
             (k:make-source "eclasp-translations.lisp" :variant-generated)
             #~"modules/asdf/build/asdf.lisp"
             :extension-systems)
  (k:sources target
             #~"kernel/tag/pre-epilogue-eclasp.lisp"
             #~"kernel/lsp/epilogue-eclasp.lisp"
             #~"kernel/tag/eclasp.lisp"))

(add-aclasp-sources :target :aclasp)

(add-bclasp-sources :target :bclasp :wrappers t)

(add-cclasp-sources :target :cclasp :wrappers t)

(add-eclasp-sources :target :eclasp :wrappers t)

(add-cclasp-sources :target :mclasp :wrappers nil :neo t)

(add-eclasp-sources :target :nclasp :wrappers t :neo t)

(add-cclasp-sources :target :vclasp :wrappers nil :bytecode t)

(add-eclasp-sources :target :wclasp :wrappers t :bytecode t)

(k:sources :eclasp-translations :extension-systems)

(k:sources :modules
           #~"modules/asdf/build/asdf.lisp"
           #~"modules/serve-event/serve-event.lisp")

(k:sources :install-code
           #~"modules/"
           #~"kernel/contrib/Acclimation/"
           #~"kernel/contrib/alexandria/"
           #~"kernel/contrib/Cleavir/"
           #~"kernel/contrib/closer-mop/"
           #~"kernel/contrib/Concrete-Syntax-Tree/"
           #~"kernel/contrib/Eclector/")

(k:sources :install-extension-code
           #~"kernel/contrib/anaphora/"
           #~"kernel/contrib/architecture.builder-protocol/"
           #~"kernel/contrib/array-utils/"
           #~"kernel/contrib/babel/"
           #~"kernel/contrib/bordeaux-threads/"
           #~"kernel/contrib/cffi/"
           #~"kernel/contrib/cl-markup/"
           #~"kernel/contrib/cl-ppcre/"
           #~"kernel/contrib/cl-svg/"
           #~"kernel/contrib/documentation-utils/"
           #~"kernel/contrib/esrap/"
           #~"kernel/contrib/global-vars/"
           #~"kernel/contrib/let-plus/"
           #~"kernel/contrib/cl-netcdf/"
           #~"kernel/contrib/lparallel/"
           #~"kernel/contrib/parser.common-rules/"
           #~"kernel/contrib/plump/"
           #~"kernel/contrib/split-sequence/"
           #~"kernel/contrib/static-vectors/"
           #~"kernel/contrib/trivial-features/"
           #~"kernel/contrib/trivial-garbage/"
           #~"kernel/contrib/trivial-http/"
           #~"kernel/contrib/trivial-indent/"
           #~"kernel/contrib/trivial-with-current-source-form/"
           #~"kernel/contrib/usocket/")
