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
;;;;
;;;;  CMPC-TABLES -- Dispatch tables for the C/C++ backend
;;;;

(in-package "C-BACKEND")

(defparameter +c2-dispatch-table+
 (make-dispatch-table
  '(
    (set . c2set)
    (set-mv . c2set-mv)
    (values . c2values-op)
    (bind . c2bind)
    (bind-special . c2bind-special)
    (progv . c2progv-op)
    (unbind . c2unbind)
    (progv-exit . c2progv-exit-op)
    (frame-pop . c2frame-pop)
    (frame-set . c2frame-set)
    (frame-save-next . c2frame-save-next)
    (frame-jmp-next . c2frame-jmp-next)
    (frame-id . c2frame-id)
    (jmp . c2jmp)

    (function-prologue . c2function-prologue)
    (function-epilogue . c2function-epilogue)
    
    (bind-requireds . c2bind-requireds)
    (varargs-bind . c2varargs-bind-op)
    (varargs-pop . c2varargs-pop-op)
    (varargs-rest . c2varargs-rest-op)
    (varargs-unbind . c2varargs-unbind-op)

    (stack-frame-open . c2stack-frame-open)
    (stack-frame-push . c2stack-frame-push)
    (stack-frame-push-values . c2stack-frame-push-values)
    (stack-frame-pop-values . c2stack-frame-pop-values)
    (stack-frame-apply . c2stack-frame-apply)
    (stack-frame-close . c2stack-frame-close)

    (throw . c2throw-op)
    (return-from . c2return-from-op)
    (go . c2go-op)
    (funcall . c2funcall-op)
    (call-local . c2call-local)
    (call-global . c2call-global)

    (debug-env-open . c2debug-env-open)
    (debug-env-close . c2debug-env-close)
    (debug-env-push-vars . c2debug-env-push-vars)
    (debug-env-pop-vars . c2debug-env-pop-vars)

    ;; cmpffi.lsp
    (ffi:c-inline . c2c-inline)
    
    ;; cmpflet.lsp
    (do-flet/labels . c2do-flet/labels)

    ;; cmpstructures.lsp
    ;; (sys:structure-ref . c2structure-ref)
    ;; (sys:structure-set . c2structure-set)

    ;; cmptop.lsp
    (si:fset . c2fset)
    )))

(defparameter +c2-wt-loc-table+
 (make-dispatch-table
  '(
    ;; cmploc.lsp
    (temp . wt-temp)
    (lcl . wt-lcl-loc)
    (vv . wt-vv)
    (vv-temp . wt-vv-temp)
    (car . wt-car)
    (cdr . wt-cdr)
    (cadr . wt-cadr)
    (fixnum-value . wt-number)
    (character-value . wt-character)
    (long-float-value . wt-number)
    (double-float-value . wt-number)
    (single-float-value . wt-number)
    (value . wt-value)
    (keyvars . wt-keyvars)
    (the . wt-the-loc)

    (nil . wt-nil-loc)
    (t . wt-t-loc)
    (value0 . wt-value0-loc)
    (return . wt-value0-loc)
    (values+value0 . wt-value0-loc)
    (values . wt-values-loc)
    (va-arg . wt-va-arg-loc)
    (cl-va-arg . wt-cl-va-arg-loc)

    ;; cmpbackend.lsp
    (call . wt-call)
    (call-normal . wt-call-normal)
    (call-indirect . wt-call-indirect)

    ;; cmpffi.lsp
    (ffi:c-inline . wt-c-inline-loc)
    (coerce-loc . wt-coerce-loc)

    ;; cmpspecial.ls
    (fdefinition . wt-fdefinition)
    (make-cclosure . wt-make-closure)

    ;; cmpstructures.lsp
    (sys:structure-ref . wt-structure-ref)
    )))

(defparameter +c2-set-loc-table+
 (make-dispatch-table
  '(
    ;; cmpbind.lsp
    (bind . bind)

    ;; cmploc.lsp
    (values . set-values-loc)
    (values+value0 . set-values+value0-loc)
    (value0 . set-value0-loc)
    (return . set-return-loc)
    (actual-return . set-actual-return-loc)
    (trash . set-trash-loc)
    (the . set-the-loc)

    ;; cmpbackend.lsp
    (jmp-true . set-loc-jmp-true)
    (jmp-false . set-loc-jmp-false)
    (jmp-zero . set-loc-jmp-zero)
    (jmp-nonzero . set-loc-jmp-nonzero)
    )))

