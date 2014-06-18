;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2009, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MAIN DRIVERS
;;;

(defun c1translate (destination value)
  (enforce-destination destination (c1expr destination value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SPECIALIZED OPERATIONS
;;;

;;;
;;; HANDLING OF TEMPORARIES
;;;

(defvar *c1-temps* '())

(defun make-c1-temp ()
  (let ((v (make-var :kind :object :type t :ref 1
                     :name (gensym "X")
                     :loc `(LCL ,(- (length *c1-temps*))))))
    (push v *c1-temps*)
    v))

;;;
;;; HIGHER ORDER TRANSLATION CONSTRUCTS
;;;

(defun enforce-destination (destination expr)
  (loop with last-form = nil
     for i in expr
     when (c1form-p last-form)
     do (setf last-form i)
     finally (let ((actual-dest destination))
               (unless (or (eq actual-dest 'TRASH)
                           (eq actual-dest destination))
                 (error "Wrong destination in forms list ~S" expr))))
  expr)

(labels ((recursively-process (function var-or-loc forms)
           (loop (cond ((var-p var-or-loc)
                        (return (funcall function var-or-loc forms)))
                       ((atom var-or-loc)
                        (return))
                       ((consp var-or-loc)
                        (recursively-process function (pop var-or-loc) forms))))))
  (defun maybe-add-to-read-nodes (var-or-loc forms)
    (recursively-process 'add-to-read-nodes var-or-loc forms)
    forms)
  (defun maybe-add-to-set-nodes (var-or-loc forms)
    (recursively-process 'add-to-set-nodes var-or-loc forms)
    forms))
(defun maybe-add-to-read/set-nodes (var-or-loc forms)
  (maybe-add-to-set-nodes var-or-loc forms)
  (maybe-add-to-read-nodes var-or-loc forms))

(defun c1save-one-value (value)
  (if (constantp value)
      (values nil (build-constant-value-loc (cmp-eval value) :always t))
      (let ((v (make-c1-temp)))
        (values (c1translate v value) v))))

(defmacro c1with-temps ((prefix postfix &rest temps) &rest body)
  `(let* ((*c1-temps* *c1-temps*)
          (*cmp-env* (cmp-env-copy))
          ,@(loop for name in temps
               collect `(,name (make-c1-temp)))
          (,prefix (c1bind (list ,@temps)))
          (,postfix (c1unbind (list ,@temps))))
     ,@(loop for name in temps
          collect `(cmp-env-register-var ,name *cmp-env*))
     ,@body))

(defmacro c1with-saved-one-value ((prefix postfix location expression) &rest body)
  `(let* ((*c1-temps* *c1-temps*)
          (,location (make-c1-temp))
          (,prefix (nconc
                    (c1bind (list ,location))
                    (c1translate ,location ,expression)))
          (,postfix (c1unbind (list ,location))))
     ,@body))

(defmacro c1with-saved-value ((prefix postfix temp value) &rest body)
  (let ((forms (gensym))
        (vars (gensym)))
    `(let* ((*c1-temps* *c1-temps*))
       (multiple-value-bind (,forms ,temp)
           (c1save-one-value ,value)
         (let* ((,vars (and ,forms (list ,temp)))
                (,prefix (nconc (c1bind ,vars) ,forms))
                (,postfix (c1unbind ,vars)))
           ,@body)))))

(defmacro c1with-saved-values ((prefix postfix temps values) &rest body)
  (let ((forms (gensym))
        (vars (gensym)))
    `(let* ((*c1-temps* *c1-temps*)
            (,temps '())
            (,vars '())
            (,prefix (loop for v in ,values
                        nconc (multiple-value-bind (forms temp)
                                  (c1save-one-value v)
                                (when forms
                                  (push temp ,vars))
                                (push temp ,temps)
                                forms)))
	    (,postfix (c1unbind (setf ,vars (nreverse ,vars)))))
       (setf prefix (nconc (c1bind ,vars) prefix)
             ,temps (nreverse ,temps))
       ,@body)))

(defun maybe-create-temp (destination)
  (case destination
    ((VALUES VALUE0 VALUES+VALUE0) destination)
    (RETURN 'VALUES+VALUE0)
    (ACTUAL-RETURN (baboon))
    (t (make-c1-temp))))

(defmacro c1with-saved-output ((prefix postfix new-destination old-destination)
                               &rest body)
  `(let* ((*c1-temps* *c1-temps*)
          (temp-var (maybe-create-temp ,old-destination))
          (,new-destination (or temp-var ,old-destination))
          (,prefix (and (var-p temp-var) (c1bind (list temp-var))))
          (,postfix (and (var-p temp-var)
                         (nconc (c1set-loc ,old-destination temp-var)
                                (c1unbind (list temp-var))))))
     ,@body))

;;;
;;; CLEANUP STATEMENTS
;;;

(defun c1cleanup-forms (env)
  (multiple-value-bind (specials other-statements)
      (cmp-env-cleanups env)
    (nconc (c1unbind specials nil) other-statements)))

;;;
;;; VARIABLE BINDINGS
;;;

(defun c1bind (vars)
  (and vars
       (make-c1form* 'BIND :args vars)))

(defun c1unbind (vars &optional (close-block t))
  (when vars
    (make-c1form* 'UNBIND :args vars close-block)))

(defun c1progv-op (ndx-loc vars-loc values-loc)
  (let ((form (make-c1form* 'PROGV :args ndx-loc vars-loc values-loc)))
    (setf (var-kind ndx-loc) :CL-INDEX
          (var-type ndx-loc) 'SI::INDEX)
    (maybe-add-to-set-nodes ndx-loc form)
    (maybe-add-to-read-nodes vars-loc form)
    (maybe-add-to-read-nodes values-loc form)))

(defun c1progv-exit-op (ndx-loc)
  (maybe-add-to-read-nodes ndx-loc (make-c1form* 'PROGV-EXIT :args ndx-loc)))

;;;
;;; ASSIGNMENTS
;;;

(defun update-destination-type (destination type)
  (cond ((not (var-p destination)))
        ((eq (setf type (values-type-primary-type type)) t))
        ((var-read-only-p destination)
         (setf (var-type destination) type))
        (t
         (let* ((type2 (var-type destination))
                (type1 (type-and type type2)))
           (unless type1
             (let* ((*print-length* 4)
                    (*print-level* 3))
               (cmpwarn "Variable ~A was declared to have type ~A~%and is assigned a value of type ~A"
                        (var-name destination) type2 type)))))))

(defun c1set-loc (dest value-loc)
  (unless (eq dest value-loc)
    (let* ((type (location-type value-loc))
           (form (make-c1form* 'SET :args dest value-loc)))
      (update-destination-type dest type)
      (maybe-add-to-set-nodes dest form)
      (maybe-add-to-read-nodes value-loc form))))

(defun c1bind-special-op (dest value-loc)
  (let* ((type (location-type value-loc))
         (form (make-c1form* 'BIND-SPECIAL :args dest value-loc)))
    (update-destination-type dest type)
    (maybe-add-to-set-nodes dest form)
    (maybe-add-to-read-nodes value-loc form)))

(defun c1maybe-bind-special (dest-var forms)
  (if (global-var-p dest-var)
      (c1bind-special dest-var forms)
      (c1translate dest-var forms)))

(defun c1maybe-bind-special-op (dest-var loc)
  (if (global-var-p dest-var)
      (c1bind-special-op dest-var loc)
      (c1set-loc dest-var loc)))

(defun c1bind-special (dest-var forms)
  (c1with-saved-value (prefix postfix temp forms)
    (nconc prefix
           (c1bind-special-op dest-var temp)
           postfix)))

(defun c1set-from-values (new-destination)
  (maybe-add-to-set-nodes new-destination
			  (c1set-loc new-destination 'VALUES)))

(defun c1set-mv (locations &optional (min-args 0) (max-args multiple-values-limit))
  (maybe-add-to-set-nodes locations
                          (make-c1form* 'SET-MV :args locations min-args max-args)))

(defun c1values-op (locations)
  (let ((form (make-c1form* 'VALUES
                            :args locations)))
    (maybe-add-to-read-nodes locations form)))

;;;
;;; FUNCTION ARGUMENTS AND BLOCKS
;;;

(defun c1function-prologue (fun)
  (make-c1form* 'FUNCTION-PROLOGUE :args fun))

(defun c1function-epilogue (fun)
  (make-c1form* 'FUNCTION-EPILOGUE :args fun))

(defun c1bind-requireds (var-loc-pairs)
  (make-c1form* 'BIND-REQUIREDS :args var-loc-pairs))

(defun c1varargs-bind-op (nargs-loc varargs-loc minargs maxargs nkeywords check)
  (make-c1form* 'VARARGS-BIND
                :args nargs-loc varargs-loc minargs maxargs nkeywords check))

(defun c1varargs-pop-op (dest nargs-loc varargs-loc)
  (if (global-var-p dest)
      (nconc (c1varargs-pop-op 'VALUE0 nargs-loc varargs-loc)
             (c1bind-special-op dest 'VALUE0))
      (let ((form (make-c1form* 'VARARGS-POP :args dest nargs-loc varargs-loc)))
        (maybe-add-to-read/set-nodes nargs-loc form)
        (maybe-add-to-read/set-nodes varargs-loc form)
        (maybe-add-to-set-nodes dest form))))

(defun c1varargs-rest-op (dest-loc nargs-loc varargs-loc nkeys
                          keywords-list allow-other-keys)
  (let ((form (make-c1form* 'VARARGS-REST :args dest-loc
                            nargs-loc varargs-loc
                            nkeys keywords-list allow-other-keys)))
    (update-destination-type dest-loc 'list)
    (maybe-add-to-read/set-nodes nargs-loc form)
    (maybe-add-to-read/set-nodes varargs-loc form)
    (maybe-add-to-set-nodes dest-loc form)))

(defun c1varargs-unbind-op (nargs-loc varargs-loc minargs maxargs nkeywords)
  (make-c1form* 'VARARGS-UNBIND
                :args nargs-loc varargs-loc minargs maxargs nkeywords))

;;;
;;; LOCATIONS, VARIABLES, ASSIGNMENTS
;;;

(defun c1values-ref (destination args)
  (maybe-add-to-set-nodes
   destination
   (make-c1form* 'SET :args destination `(VALUE ,(first args)))))

;;;
;;; JUMP FRAMES
;;;

(defun c1frame-set (id-loc no-label)
  (incf (tag-ref no-label))
  (maybe-add-to-read-nodes id-loc
                           (make-c1form* 'FRAME-SET :args id-loc no-label)))

(defun c1frame-pop (&optional var)
  (make-c1form* 'FRAME-POP :args var))

(defun c1frame-save-next (var)
  (make-c1form* 'FRAME-SAVE-NEXT :args var))

(defun c1frame-jmp-next (var)
  (make-c1form* 'FRAME-JMP-NEXT :args var))

(defun c1frame-id (var)
  (maybe-add-to-set-nodes var (make-c1form* 'FRAME-ID :args var)))

;;;
;;; STACK FRAMES
;;;

(defun c1stack-frame-open (var)
  (maybe-add-to-set-nodes var (make-c1form* 'STACK-FRAME-OPEN :args var)))

(defun c1stack-frame-push (frame-var value-loc)
  (maybe-add-to-read/set-nodes
   frame-var
   (make-c1form* 'STACK-FRAME-PUSH :args frame-var value-loc)))

(defun c1stack-frame-push-values (frame-var)
  (maybe-add-to-read/set-nodes
   frame-var
   (make-c1form* 'STACK-FRAME-PUSH-VALUES :args frame-var)))

(defun c1stack-frame-pop-values (frame-var &optional (dest 'trash))
  (maybe-add-to-read/set-nodes
   frame-var
   (make-c1form* 'STACK-FRAME-POP-VALUES :args frame-var dest)))

(defun c1stack-frame-apply (frame-var function-loc)
  (maybe-add-to-read-nodes
   frame-var
   (make-c1form* 'STACK-FRAME-APPLY :args frame-var function-loc)))

(defun c1stack-frame-close (frame-var)
  (maybe-add-to-read-nodes
   frame-var
   (make-c1form* 'STACK-FRAME-CLOSE :args frame-var)))

;;;
;;; LOCAL AND NONLOCAL CONTROL TRANSFER
;;;

(defun add-jmp-cleanups (tag forms)
  (let ((env (tag-env tag)))
    (if env
        (nconc (c1cleanup-forms env) forms)
        forms)))

(defun c1jmp (tag)
  (incf (tag-ref tag)) ;; Only local jumps, so no -ccb or -clb
  (add-jmp-cleanups tag (make-c1form* 'JMP :args tag)))

(defun c1jmp-true (tag loc)
  (incf (tag-ref tag))
  (add-jmp-cleanups tag (c1set-loc `(JMP-TRUE ,tag) loc)))

(defun c1jmp-false (tag loc)
  (incf (tag-ref tag))
  (add-jmp-cleanups tag (c1set-loc `(JMP-FALSE ,tag) loc)))

(defun c1jmp-zero (tag loc)
  (incf (tag-ref tag))
  (add-jmp-cleanups tag (c1set-loc `(JMP-ZERO ,tag) loc)))

(defun c1jmp-nonzero (tag loc)
  (incf (tag-ref tag))
  (add-jmp-cleanups tag (c1set-loc `(JMP-NONZERO ,tag) loc)))

(defun c1return-from-op (var name)
  (maybe-add-to-read-nodes var (make-c1form* 'RETURN-FROM :args var name)))

(defun c1throw-op (tag-loc)
  (maybe-add-to-read-nodes tag-loc (make-c1form* 'THROW :args tag-loc)))

(defun c1go-op (tag)
  (maybe-add-to-read-nodes tag (make-c1form* 'GO :args tag)))

;;;
;;; FUNCTION CALLS, CLOSURES AND THE LIKE
;;;

(defun c1do-flet/labels-op (function-list)
  (make-c1form* 'DO-FLET/LABELS :args function-list))

(defun c1funcall-op (destination arguments)
  (let ((form (make-c1form* 'FUNCALL :args destination arguments)))
    (maybe-add-to-read-nodes arguments form)
    (maybe-add-to-set-nodes destination form)))

(defun c1call-local-op (destination fun args-loc)
  (let* ((return-type (or (get-local-return-type fun) 'T))
         (arg-types (get-local-arg-types fun))
         (form (make-c1form* 'CALL-LOCAL
                             :args destination fun args-loc)))
    ;; Add type information to the arguments.
    (maybe-add-to-read-nodes args-loc form)
    (loop for arg in args-loc
       for type in arg-types
       do (and-form-type (car arg-types) form (car args)
                         :safe "In a call to ~a" fname))
    (update-destination-type destination return-type)
    (maybe-add-to-set-nodes destination form)))

(defun c1call-global-op (destination fname arguments)
  (let* ((return-type (propagate-types fname arguments))
         (form (make-c1form* 'CALL-GLOBAL
                             :sp-change (function-may-change-sp fname)
                             :args destination fname arguments
                             (values-type-primary-type return-type))))
    (maybe-add-to-read-nodes arguments form)
    (update-destination-type destination return-type)
    (maybe-add-to-set-nodes destination form)))

(defun c1c-inline-op (output-type destination temps arg-types
                      output-rep-type c-expression side-effects
                      one-liner)
  (let ((form (make-c1form* 'C-INLINE
                             :args destination temps arg-types
                             output-rep-type c-expression side-effects
                             one-liner)))
    (maybe-add-to-read-nodes temps form)
    (update-destination-type destination output-type)
    (maybe-add-to-set-nodes destination form)))

;;;
;;; DEBUG INFORMATION
;;;

(defun c1debug-env-open (fname)
  (make-c1form* 'DEBUG-ENV-OPEN :args fname))

(defun c1debug-env-push-vars (variables)
  (when variables
    (make-c1form* 'DEBUG-ENV-PUSH-VARS :args variables)))

(defun c1debug-env-pop-vars (variables &optional close-block)
  (when variables
    (make-c1form* 'DEBUG-ENV-POP-VARS :args variables close-block)))

(defun c1debug-env-close (fname)
  (make-c1form* 'DEBUG-ENV-CLOSE :args fname))
