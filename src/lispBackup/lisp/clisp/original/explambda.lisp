#|#########################################################################
# Copyright (C) 1997-2011 Ufasoft                                         #
# http://ufasoft.com   mailto:support@ufasoft.com                         #
#                                                                         #
# This program is free software; you can redistribute it and/or modify it #
# under the terms of the GNU General Public License as published by the   #
# Free Software Foundation; either version 3, or (at your option) any     #
# later version.                                                          #
#                                                                         #
# This program is distributed in the hope that it will be useful,         #
# but WITHOUT ANY WARRANTY; without even the implied warranty of          #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           #
# GNU General Public License for more details.                            #
#                                                                         #
# You should have received a copy of the GNU General Public License       #
# along with this program; If not, see <http://www.gnu.org/licenses/>     #
########################################################################=|#

(eval-when (compile load eval)
  (setq *package* (sys::_get-package "SYSTEM")))


(PROGN

(proclaim '(special *keyword-package*))
(setq *keyword-package* (find-package "KEYWORD"))
(defun symbol-to-keyword (s) (intern (symbol-name s) *keyword-package*))

(proclaim '(special *fenv*))
;; *FENV* = the current function environment during expansion of a form.
;; structure: NIL or a 2n+1-element vector
;;   (n1 f1 ... nn fn next),
;; where the ni are function-names,
;;       the fi are their functional meanings
;;           (closure or macro or function-macro or still NIL)
;; continued similarly at 'next'.

;; (fenv-assoc s fenv) searches symbol S in function-environment FENV.
;; the search routine uses EQUAL
(defun fenv-assoc (s fenv &optional (from-inside-macrolet nil))
  (if fenv
    (if (simple-vector-p fenv)
      #+COMPILER
      (do ((l (1- (length fenv)))
           (i 0 (+ i 2)))
          ((= i l) (fenv-assoc s (svref fenv i) from-inside-macrolet))
        (if (equal s (svref fenv i))
          (if (and from-inside-macrolet (not (macrop (svref fenv (1+ i)))))
            (error-of-type 'source-program-error
              :form (list 'FUNCTION s)
              :detail s
              (TEXT "Invalid access to the local function definition of ~S from within a ~S definition")
              s 'macrolet)
            (return (svref fenv (1+ i))))))
      #-COMPILER
      (let ((l (1- (length fenv)))
            (i 0))
        (block nil
          (tagbody
            1 (if (= i l) (return-from nil (fenv-assoc s (svref fenv i) from-inside-macrolet)))
              (if (equal s (svref fenv i))
                (if (and from-inside-macrolet (not (macrop (svref fenv (1+ i)))))
                  (error-of-type 'source-program-error
                    :form (list 'FUNCTION s)
                    :detail s
                    (TEXT "Invalid access to the local function definition of ~S from within a ~S definition")
                    s 'macrolet)
                  (return-from nil (svref fenv (1+ i)))))
              (setq i (+ i 2))
              (go 1))))
      (if (and (consp fenv) (eq (car fenv) 'MACROLET))
        (fenv-assoc s (cdr fenv) t)
        (error-of-type 'type-error
          :datum fenv :expected-type '(or null simple-vector (cons (eql macrolet) t))
          (TEXT "~S is an invalid function environment")
          fenv)))
    'T)) ; not found
;; Determines, if a function-name S in function-environment FENV is not
;; defined and thus refers to the global function.
(sys::%putd 'global-in-fenv-p
  (sys::make-preliminary
    (function global-in-fenv-p (lambda (s fenv) ; preliminary
      (eq (fenv-assoc s fenv) 'T)))))

(proclaim '(special *venv*))
;; *VENV* = the current variable-environment during the expansion of a form.
;; Structure: NIL or a 2n+1-element Vector
;;   (n1 v1 ... nn vn next),
;; where the ni are Symbols,
;;      the vi are their syntactic meanings (symbol-macro-object or sth. else)
;; continued similarly at 'next'.

;; (venv-assoc s venv) searches symbol S in variable-environment VENV.
;; Returns the value (or NIL if there's no value).
;; Caution: The value can be #<SPECDECL> or #<SYMBOL-MACRO ...> , thus
;; may not be temporarily saved in a variable in interpreted Code.
;; the search routine uses EQ
(defun venv-assoc (s venv &optional (from-inside-macrolet nil))
  (if venv
    (if (simple-vector-p venv)
      #+COMPILER
      (do ((l (1- (length venv)))
           (i 0 (+ i 2)))
          ((= i l) (venv-assoc s (svref venv i) from-inside-macrolet))
        (if (eq s (svref venv i))
          (if (and from-inside-macrolet
                   (not (eq (svref venv (1+ i)) compiler::specdecl))
                   (not (symbol-macro-p (svref venv (1+ i)))))
            (error-of-type 'source-program-error
              :form s
              :detail s
              (TEXT "Invalid access to the value of the lexical variable ~S from within a ~S definition")
              s 'macrolet)
            (return (svref venv (1+ i))))))
      #-COMPILER
      (let ((l (1- (length venv)))
            (i 0))
        (block nil
          (tagbody
            1 (if (= i l) (return-from nil (venv-assoc s (svref venv i) from-inside-macrolet)))
              (if (eq s (svref venv i))
                (if (and from-inside-macrolet
                         (not (symbol-macro-p (svref venv (1+ i)))))
                  (error-of-type 'source-program-error
                    :form s
                    :detail s
                    (TEXT "Invalid access to the value of the lexical variable ~S from within a ~S definition")
                    s 'macrolet)
                  (return-from nil (svref venv (1+ i)))))
              (setq i (+ i 2))
              (go 1))))
      (if (and (consp venv) (eq (car venv) 'MACROLET))
        (venv-assoc s (cdr venv) t)
        (error-of-type 'type-error
          :datum venv :expected-type '(or null simple-vector)
          (TEXT "~S is an invalid variable environment")
          venv)))
    ; not found
    (if (symbol-macro-expand s)
      (global-symbol-macro-definition (get s 'SYMBOLMACRO))
      (and (boundp s) (symbol-value s)))))

;; Most of the Expansion-functions return two values:
;;  (1) the expansion result,
;;  (2) (NIL or T) indicates, if something was changed within it.

(proclaim '(special %whole-form)) ; the whole form being expanded

;; (%expand-cons ...) composes a cons. returns 2 values.
;; form=old Form,
;; expf,flagf = expansion of the first-part,
;; expr,flagr = expansion of the rest-part.
(defun %expand-cons (form expf flagf expr flagr)
  (if (or flagf flagr)
    (values (cons expf expr) t)
    (values form nil)))
    
;; cons specs on top of *fenv*
(defun cons-*fenv* (specs) (apply #'vector (nreverse (cons *fenv* specs))))

(setq sys::*pr* nil)    

;; (%expand-form form) expands a whole Form. returns 2 values.
(defun %expand-form (form &aux (%whole-form form))
  (if (atom form)
    #+COMPILER
    (let (h)
      (if (and (symbolp form)
               (symbol-macro-p (setq h (venv-assoc form *venv*))))
        (values (%expand-form (sys::%record-ref h 0)) t)
        (values form nil)))
    #-COMPILER
    (if (and (symbolp form) (symbol-macro-p (venv-assoc form *venv*)))
      (values (%expand-form (sys::%record-ref (venv-assoc form *venv*) 0)) t)
      (values form nil))
      
    ;; form is a CONS
    (let ((f (first form)))
      (if (function-name-p f)
        (let ((h (fenv-assoc f *fenv*)))
          ;; f is in *fenv* associated to h
          (if (eq h 'T)
            ;; f has no local definition
            ;; Now the separate expanders for the special-forms:
            (case f
              ((RETURN-FROM THE) ; skip the 1st argument, expand the rest
               (multiple-value-call #'%expand-cons form
                  (first form) nil
                  (multiple-value-call #'%expand-cons (rest form)
                    (second form) nil
                    (%expand-list (cddr form)))))
              ((QUOTE GO DECLARE LOAD-TIME-VALUE) ; expand nothing
               (values form nil))
              ((FUNCTION)
               ;; if 1st or 2nd argument is a list,
               ;; expand as lambda expression.
               (multiple-value-call #'%expand-cons form
                 'FUNCTION nil
                 (if (atom (cddr form))
                   (if (function-name-p (second form))
                     (let ((h (fenv-assoc (second form) *fenv*)))
                       (cond ((or (eq h 'T) (closurep h)
                                  (function-macro-p h) (null h))
                              (values (rest form) nil))
                             ((macrop h)
                              (error-of-type 'source-program-error
                                :form form
                                :detail (second form)
                                (TEXT "~S: ~S is illegal since ~S is a local macro")
                                '%expand form (second form)))
                             (t (error-of-type 'error
                                  (TEXT "~S: invalid function environment ~S")
                                  '%expand *fenv*))))
                     (if (atom (second form))
                       (error-of-type 'source-program-error
                         :form form
                         :detail (second form)
                         (TEXT "~S: ~S is invalid since ~S is not a symbol")
                         '%expand form (second form))
                       (multiple-value-call #'%expand-cons (rest form)
                         (%expand-lambda (second form))
                         (cddr form) nil)))
                   (multiple-value-call #'%expand-cons (rest form)
                     (second form) nil
                     (multiple-value-call #'%expand-cons (cddr form)
                       (%expand-lambda (third form))
                       (cdddr form) nil)))))
              ((EVAL-WHEN)
               (let ((situations (second form)))
                 (if (or (member 'EVAL situations)
                         (member ':EXECUTE situations))
                   (multiple-value-call #'%expand-cons form
                     (first form) nil
                     (multiple-value-call #'%expand-cons (rest form)
                       (second form) nil
                       (%expand-list (cddr form))))
                   (values 'NIL t))))
              ((LET)            ; expand variable-list and body
               (let ((*venv* *venv*))
                 (%expand-special-declarations (cddr form))
                 (multiple-value-call #'%expand-cons form
                   (first form) nil
                   (multiple-value-call #'%expand-cons (rest form)
                     (%expand-varspez (second form))
                     (%expand-list (cddr form))))))
              ((LET*)           ; expand variable-list and body
               (let ((*venv* *venv*))
                 (%expand-special-declarations (cddr form))
                 (multiple-value-call #'%expand-cons form
                   (first form) nil
                   (multiple-value-call #'%expand-cons (rest form)
                     (%expand-varspez* (second form))
                     (%expand-list (cddr form))))))
              ((LOCALLY)        ; expand body
               (let ((*venv* *venv*))
                 (%expand-special-declarations (cdr form))
                 (multiple-value-call #'%expand-cons form
                   (first form) nil
                   (%expand-list (cdr form)))))
              ((MULTIPLE-VALUE-BIND) ; expand form and body, separately
               (let ((*venv* *venv*))
                 (%expand-special-declarations (cdddr form))
                 (multiple-value-call #'%expand-cons form
                   'MULTIPLE-VALUE-BIND nil
                   (multiple-value-call #'%expand-cons (rest form)
                     (second form) nil
                     (multiple-value-call #'%expand-cons (cddr form)
                       (%expand-form (third form))
                       (progn
                         (%expand-lexical-variables (second form))
                         (%expand-list (cdddr form))))))))
              ((COMPILER-LET) ; expand var-list in empty environment and body
               (progv
                   (mapcar #'%expand-varspec-var (second form))
                   (mapcar #'%expand-varspec-val (second form))
                 (values (%expand-form (cons 'PROGN (cddr form))) t)))
              ((COND) ; expand all Sub-Forms of the clauses:
               (multiple-value-call #'%expand-cons form
                 (first form) nil
                 (%expand-cond (rest form))))
              ((CASE) ; expand 1st argument and all sub-forms of the clauses
               (multiple-value-call #'%expand-cons form
                 (first form) nil
                 (multiple-value-call #'%expand-cons (rest form)
                   (%expand-form (second form))
                   (%expand-case (cddr form)))))
              ((BLOCK)
               ;; expand body. If there is a RETURN-FROM in this
               ;; block, keep BLOCK, else turn it into a PROGN.
               (multiple-value-bind (body flagb) (%expand-list (cddr form))
                 (if (%return-p (second form) body)
                   (multiple-value-call #'%expand-cons form
                     (first form) nil
                     (multiple-value-call #'%expand-cons (rest form)
                       (second form) nil
                       body flagb))
                    (values
                      (cond ((atom body) body)
                            ((null (cdr body)) (car body))
                            (t (cons 'progn body)))
                      t))))
              ((SETQ PSETQ) ; expand each second Argument
               (if (%expand-setqlist-macrop (rest form))
                 (let ((new (if (eq (first form) 'SETQ) 'SETF 'PSETF)))
                   (values
                    (%expand-form
                     (funcall (macro-function new) (cons new (rest form))
                              (vector *venv* *fenv*)))
                    t))
                 (multiple-value-call #'%expand-cons form
                   (first form) nil
                   (%expand-setqlist (rest form)))))
              ((MULTIPLE-VALUE-SETQ) ; skip 1st argument, expand the rest
               (if (%expand-varlist-macrop (second form))
                 (values (%expand-form (cons 'MULTIPLE-VALUE-SETF (rest form)))
                         t)
                 (multiple-value-call #'%expand-cons form
                   'MULTIPLE-VALUE-SETQ nil
                   (multiple-value-call #'%expand-cons (rest form)
                     (second form) nil
                     (%expand-list (cddr form))))))
              ((TAGBODY)
               ;; expand all arguments,
               ;; skip atoms that are created during expansion
               (multiple-value-call #'%expand-cons form
                 (first form) nil
                 (%expand-tagbody (rest form))))
              ((PROGN) ; expand all arguments, possibly simplify them.
               (if (null (rest form))
                 (values nil t)
                 (if (null (cddr form))
                   (values (%expand-form (second form)) t)
                   (multiple-value-call #'%expand-cons form
                     (first form) nil
                     (%expand-list (rest form))))))
              ((FLET) ; expand function definitions
               (if (null (second form))
                 (values (%expand-form (cons 'PROGN (cddr form))) t)
                 (let ((newfenv (%expand-fundefs-1 (second form))))
                   (multiple-value-call #'%expand-cons form
                     (first form) nil
                     (multiple-value-call #'%expand-cons (rest form)
                       (%expand-fundefs-2 (second form))
                       (let ((*fenv* (apply #'vector newfenv)))
                         (%expand-list (cddr form))))))))
              ((LABELS)
               ;; expand function definitions and body
               ;; in the extended environment
               (if (null (second form))
                 (values (%expand-form (cons 'PROGN (cddr form))) t)
                 (let ((newfenv (%expand-fundefs-1 (second form))))
                   (let ((*fenv* (apply #'vector newfenv)))
                     (multiple-value-call #'%expand-cons form
                       (first form) nil
                       (multiple-value-call #'%expand-cons (rest form)
                         (%expand-fundefs-2 (second form))
                         (%expand-list (cddr form))))))))
              ((MACROLET) ; expand the body in the extended environment
               (do ((L1 (second form) (cdr L1))
                    (L2 nil))
                   ((atom L1)
                    (if L1
                      (error-of-type 'source-program-error
                        :form form
                        :detail L1
                        (TEXT "code after MACROLET contains a dotted list, ending with ~S")
                        L1)
                      (let ((*fenv* (apply #'vector
                                           (nreverse (cons *fenv* L2)))))
                        (values (%expand-form (cons 'PROGN (cddr form))) t))))
                 (let ((macrodef (car L1)))
                   (if (and (consp macrodef)
                            (symbolp (car macrodef))
                            (consp (cdr macrodef)))
                     (setq L2 (list* (make-macro-expander macrodef form)
                                     (car macrodef) L2))
                     (error-of-type 'source-program-error
                       :form form
                       :detail macrodef
                       (TEXT "illegal syntax in MACROLET: ~S")
                       macrodef)))))
              ((FUNCTION-MACRO-LET)
               ;; expand function-definitions,
               ;; expand body in extended environment
               (if (null (second form))
                 (values (%expand-form (cons 'PROGN (cddr form))) t)
                 (let ((newfenv (%expand-funmacdefs-1 (second form))))
                   (multiple-value-call #'%expand-cons form
                     (first form) nil
                     (multiple-value-call #'%expand-cons (rest form)
                       (%expand-funmacdefs-2 (second form))
                       (let ((*fenv* (apply #'vector newfenv)))
                         (%expand-list (cddr form))))))))
              ((SYMBOL-MACROLET) ; expand body in extended environment
               (do ((L1 (second form) (cdr L1))
                    (L2 nil))
                   ((atom L1)
                    (if L1
                      (error-of-type 'source-program-error
                        :form form
                        :detail L1
                        (TEXT "code after SYMBOL-MACROLET contains a dotted list, ending with ~S")
                        L1)
                      (let ((*venv* (apply #'vector
                                           (nreverse (cons *venv* L2)))))
                        (let ((specials (%expand-special-declarations
                                         (cddr form))))
                          (do ((L3 (second form) (cdr L3)))
                              ((atom L3))
                            (if (memq (caar L3) specials)
                              (error-of-type 'source-program-error
                                :form form
                                :detail (caar L3)
                                (TEXT "~S: symbol ~S must not be declared SPECIAL and a macro at the same time")
                                'symbol-macrolet (caar L3)))))
                        (values (%expand-form (cons 'LOCALLY (cddr form)))
                                t))))
                 (let ((symdef (car L1)))
                   (if (and (consp symdef)
                            (symbolp (car symdef))
                            (consp (cdr symdef))
                            (null (cddr symdef)))
                     (let ((symbol (car symdef))
                           (expansion (cadr symdef)))
                       (if (special-variable-p symbol)
                         (error-of-type 'program-error
                           (TEXT "~S: symbol ~S is declared special and must not be declared a macro")
                           'symbol-macrolet symbol)
                         (setq L2 (list* (make-symbol-macro expansion)
                                         symbol L2))))
                     (error-of-type 'source-program-error
                       :form form
                       :detail symdef
                       (TEXT "illegal syntax in SYMBOL-MACROLET: ~S")
                       symdef)))))
              (t (cond ((and (symbolp f) (special-operator-p f))
                        ;; other Special-forms,
                        ;; e.g. IF, CATCH, THROW, PROGV, UNWIND-PROTECT, PROGN,
                        ;; PROG1, PROG2, WHEN, UNLESS, MULTIPLE-VALUE-LIST,
                        ;; MULTIPLE-VALUE-CALL, MULTIPLE-VALUE-PROG1, AND, OR:
                        (multiple-value-call #'%expand-cons form
                          f nil
                          (%expand-list (rest form))))
                       ((and (symbolp f) (setq h (macro-function f)))
                        ;; global Macro-Definition
                        (%expand-macro h form))
                       (t ; normal function-call
                        (multiple-value-call #'%expand-cons form
                          f nil
                          (%expand-list (rest form)))))))
            ;; f has a local definition
            (cond ((or (closurep h) (function-macro-p h) (null h))
                   ;; function to be called
                   (multiple-value-call #'%expand-cons form
                     f nil
                     (%expand-list (rest form))))
                  ((macrop h) ; macro to be expanded
                   (%expand-macro (macro-expander h) form)) ; call expander
                  (t (error-of-type 'error
                       (TEXT "bad function environment occurred in ~S: ~S")
                       '%expand-form *fenv*)))))
        (if (consp f)
          (multiple-value-call #'%expand-cons form
            (%expand-lambda f)
            (%expand-list (rest form)))
          (error-of-type 'source-program-error
            :form form
            :detail form
            (TEXT "~S: invalid form ~S")
            '%expand-form form))))))

;; Auxiliary functions for the the expansion:

;; call the macro-expander on the form (expanding after)
(defun %expand-macro (macro-expander form)
;  (_pr "-----%expand-macro-----------")
;  (_pr (sys::%record-ref macro-expander 1))

  (values (%expand-form (funcall macro-expander form (vector *venv* *fenv*)))
          t))

;; expands a list of forms. returns 2 values.
(defun %expand-list (l)
  (if (atom l)
    (if l
      (error-of-type 'source-program-error
        :form %whole-form
        :detail l
        (TEXT "code contains a dotted list, ending with ~S")
        l)
      (values nil nil))
    (multiple-value-call #'%expand-cons l
                         (%expand-form (first l))
                         (%expand-list (rest l)))))

;; Adds lexical variables to *venv* .
;; (only used for shadowing symbol-macros.)
(defun %expand-lexical-variables (vars)
  (if vars
    (setq *venv*
      (apply #'vector
        (nconc (mapcan #'(lambda (v) (declare (source nil)) (list v nil)) vars)
               (list *venv*))))))

;; Adds SPECIAL-Declarations at the beginning of a Body to *venv* .
(defun %expand-special-declarations (body)
  (multiple-value-bind (body-rest declarations) (sys::parse-body body)
    (declare (ignore body-rest)) ; do not throw away declarations!
    (let ((specials nil))
      (mapc #'(lambda (declspec)
                (declare (source nil))
                (if (and (consp declspec) (null (cdr (last declspec))))
                  (if (eq (car declspec) 'SPECIAL)
                    (mapc #'(lambda (x)
                              (declare (source nil))
                              (if (symbolp x)
                                (setq specials (cons x specials))))
                          (cdr declspec)))))
            (nreverse declarations))
      (setq specials (nreverse specials))
      (%expand-lexical-variables specials) ; specdecl doesn't matter here
      specials)))

;; expands a function-name, that is a Cons (that must be a
;; Lambda-Expression). returns 2 values.
(defun %expand-lambda (l)
  (unless (eq (first l) 'lambda)
    (error-of-type 'source-program-error
      :form %whole-form
      :detail l
      (TEXT "~S: ~S should be a lambda expression")
      '%expand-form l))
  (multiple-value-call #'%expand-cons l
      'lambda nil ; LAMBDA
      (%expand-lambdabody (rest l))))

;; expands the CDR of a Lambda-Expression, a (lambdalist . body).
;; returns 2 values.
(defun %expand-lambdabody (lambdabody &optional name blockp)
  (let ((body (rest lambdabody)))
    (if (and (consp body)
             (let ((form (car body)))
               (and (consp form)
                    (eq (car form) 'DECLARE)
                    (let ((declspecs (cdr form)))
                      (and (consp declspecs)
                           (let ((declspec (car declspecs)))
                             (and (consp declspec)
                                  (eq (car declspec) 'SOURCE))))))))
      (values lambdabody nil) ; already expanded -> leave untouched
      (let ((*venv* *venv*))
        (if blockp (setq lambdabody (add-implicit-block name lambdabody)))
        (values (list* (%expand-lambdalist (first lambdabody))
                       (list 'DECLARE (list 'SOURCE lambdabody))
                       (%expand-list (rest lambdabody)))
                t)))))

;; expands a Lambda-list. returns 2 values.
(defun %expand-lambdalist (ll)
  (if (atom ll)
    (if ll
      (error-of-type 'source-program-error
        :form %whole-form
        :detail ll
        (TEXT "lambda list must not end with the atom ~S")
        ll)
      (values nil nil))
    (multiple-value-call #'%expand-cons ll
        (%expand-parspez (first ll))
        (progn
          (let ((v (first ll)))
            (if (not (memq v lambda-list-keywords))
              (setq *venv* (vector (%expand-varspec-var v) nil *venv*))))
          (%expand-lambdalist (rest ll))))))

;; expands an element of a lambda-list. returns 2 values.
;; (expands only on lists, and then only the second element.)
(defun %expand-parspez (ps)
  (if (or (atom ps) (atom (rest ps)))
    (values ps nil)
    (multiple-value-call #'%expand-cons ps
        (first ps) nil
        (multiple-value-call #'%expand-cons (rest ps)
            (%expand-form (second ps))
            (cddr ps) nil))))

;; expand a Variable-list for LET. returns 2 values.
(defun %expand-varspez (vs &optional (nvenv nil))
  (if (atom vs)
    (if vs
      (error-of-type 'source-program-error
        :form %whole-form
        :detail vs
        (TEXT "~S: variable list ends with the atom ~S")
        'let vs)
      (progn
        (setq *venv* (apply #'vector (nreverse (cons *venv* nvenv))))
        (values nil nil)))
    (multiple-value-call #'%expand-cons vs
        (%expand-parspez (first vs)) ; For List: Expand 2nd Element
        (%expand-varspez (rest vs) (list* nil (%expand-varspec-var (first vs))
                                          nvenv)))))

;; expands a Variable-list for LET*. returns 2 values.
(defun %expand-varspez* (vs)
  (if (atom vs)
    (if vs
      (error-of-type 'source-program-error
        :form %whole-form
        :detail vs
        (TEXT "~S: variable list ends with the atom ~S")
        'let* vs)
      (values nil nil))
    (multiple-value-call #'%expand-cons vs
        (%expand-parspez (first vs)) ; for list: expand 2nd Element
        (progn
          (setq *venv* (vector (%expand-varspec-var (first vs)) nil *venv*))
          (%expand-varspez* (rest vs))))))

(defun %expand-varspec-var (varspec)
  (if (atom varspec) varspec (first varspec)))

(defun %expand-varspec-val (varspec)
  (if (atom varspec) nil (eval (second varspec))))

;; expands a cond-clause-list. returns 2 values.
(defun %expand-cond (clauses)
  (if (atom clauses)
    (values clauses nil)
    (multiple-value-call #'%expand-cons clauses
        (%expand-list (first clauses))
        (%expand-cond (rest clauses)))))

;; expands a case-clause-list. returns 2 values.
(defun %expand-case (clauses)
  (if (atom clauses)
    (values clauses nil)
    (multiple-value-call #'%expand-cons clauses
      (multiple-value-call #'%expand-cons (first clauses)
        (caar clauses) nil
        (%expand-list (cdar clauses)))
      (%expand-case (rest clauses)))))

;; Apply the following to the already expanded body:
;; (%return-p name list) determines, if the form-list list contains
;; a (RETURN-FROM name ...) somewhere.
(defun %return-p (name body)
  (block return-p
    (tagbody 1
      (if (atom body) (return-from return-p nil))
      (let ((form (car body)))
        (if ; determine, if form contains a (RETURN-FROM name ...) :
         (and (consp form)
              (not (eq (first form) 'quote))
              (or (and (eq (first form) 'return-from) ; (RETURN-FROM name ...)
                       (eq (second form) name))
                  (and (consp (first form))           ; lambda-list
                       (%return-p name (first form)))
                  (and (not ; no new definition of the same block ?
                        (and (eq (first form) 'block) (eq (second form) name)))
                       (%return-p name (rest form))))) ; function call
         (return-from return-p t)))
       (setq body (cdr body))
       (go 1))))

(defun %expand-varlist-macrop (l)
  (and (consp l)
       (or (and (symbolp (car l)) (symbol-macro-p (venv-assoc (car l) *venv*)))
           (%expand-varlist-macrop (cdr l)))))

(defun %expand-setqlist-macrop (l)
  (and (consp l) (consp (cdr l))
       (or (and (symbolp (car l)) (symbol-macro-p (venv-assoc (car l) *venv*)))
           (%expand-setqlist-macrop (cddr l)))))

(defun %expand-setqlist (l)
  (if (or (atom l) (atom (cdr l)))
    (values l nil)
    (multiple-value-call #'%expand-cons l
        (first l) nil
        (multiple-value-call #'%expand-cons (rest l)
            (%expand-form (second l))
            (%expand-setqlist (cddr l))))))

;; (%expand-tagbody list) expands the elements of a list
;; and leaves atoms, that are created meanwhile, untouched.
;; (thus no new tags are created that could hide other tags).
;; returns 2 values.
(defun %expand-tagbody (body)
  (cond ((atom body) (values body nil))
        ((atom (first body))
         (multiple-value-call #'%expand-cons body
             (first body) nil
             (%expand-tagbody (rest body))))
        (t (multiple-value-bind (exp flag) (%expand-form (first body))
             (if (atom exp)
               (values (%expand-tagbody (rest body)) t) ; omit
               (multiple-value-call #'%expand-cons body
                   exp flag
                   (%expand-tagbody (rest body))))))))
;; returns a list (name1 nil ... namek nil *fenv*)
(defun %expand-fundefs-1 (fundefs)
  (if (atom fundefs)
    (if fundefs
      (error-of-type 'source-program-error
        :form %whole-form
        :detail fundefs
        (TEXT "FLET/LABELS: code contains a dotted list, ending with ~S")
        fundefs)
      (list *fenv*))
    (let ((fundef (car fundefs)))
      (if (and (consp fundef) (function-name-p (car fundef))
               (consp (cdr fundef)))
        (list* (car fundef) nil (%expand-fundefs-1 (cdr fundefs)))
        (error-of-type 'source-program-error
          :form %whole-form
          :detail fundef
          (TEXT "illegal syntax in FLET/LABELS: ~S")
          fundef)))))
;; (%expand-fundefs-2 fundefs) expands a function-definition-list,
;; like in FLET, LABELS. returns 2 values.
(defun %expand-fundefs-2 (fundefs)
  (if (atom fundefs)
    (values fundefs nil)
    (let ((fundef (car fundefs)))
      (multiple-value-call #'%expand-cons fundefs
             (multiple-value-call #'%expand-cons fundef
                     (car fundef) nil
                     (%expand-lambdabody (cdr fundef) (car fundef) t))
             (%expand-fundefs-2 (rest fundefs))))))
;; returns a list (name1 nil ... namek nil *fenv*)
(defun %expand-funmacdefs-1 (funmacdefs)
  (if (atom funmacdefs)
    (if funmacdefs
      (error-of-type 'source-program-error
        :form %whole-form
        :detail funmacdefs
        (TEXT "FUNCTION-MACRO-LET: code contains a dotted list, ending with ~S")
        funmacdefs)
      (list *fenv*))
    (let ((funmacdef (car funmacdefs)))
      (if (and (consp funmacdef)
               (symbolp (car funmacdef))
               (consp (cdr funmacdef)) (consp (second funmacdef))
               (consp (cddr funmacdef)) (consp (third funmacdef))
               (null (cdddr funmacdef)))
        (list* (car funmacdef) nil (%expand-funmacdefs-1 (cdr funmacdefs)))
        (error-of-type 'source-program-error
          :form %whole-form
          :detail funmacdef
          (TEXT "illegal syntax in FUNCTION-MACRO-LET: ~S")
          funmacdef)))))
;; (%expand-funmacdefs-2 funmacdefs) expands a function-macro-
;; definition-list, like in FUNCTION-MACRO-LET. returns 2 values.
(defun %expand-funmacdefs-2 (funmacdefs)
  (if (atom funmacdefs)
    (values funmacdefs nil)
    (let ((funmacdef (car funmacdefs)))
      (multiple-value-call #'%expand-cons funmacdefs
        (multiple-value-call #'%expand-cons funmacdef
          (car funmacdef) nil
          (multiple-value-call #'%expand-cons (cdr funmacdef)
            (%expand-lambdabody (cadr funmacdef))
            (multiple-value-call #'%expand-cons (cddr funmacdef)
              (let ((*venv* nil) (*fenv* nil))
                (%expand-lambdabody (caddr funmacdef)))
              (cdddr funmacdef) nil)))
        (%expand-funmacdefs-2 (rest funmacdefs))))))
;; (%expand-handlers handlers) expands a Typ/Handler-List
;; like in %HANDLER-BIND. returns 2 values.
(defun %expand-handlers (handlers)
  (if (atom handlers)
    (values handlers nil)
    (let ((handler (car handlers)))
      (multiple-value-call #'%expand-cons handlers
        (multiple-value-call #'%expand-cons handler
          (car handler) nil
          (%expand-list (cdr handler)))
        (%expand-handlers (cdr handlers))))))

;; expands (lambdalist . body) in a given function-environment.
;; Is called by GET_CLOSURE.
(defun %expand-lambdabody-main (lambdabody *venv* *fenv*)
  (%expand-lambdabody lambdabody))

(defun expand-form (form &aux *fenv* *venv*)
  (%expand-form form))

(VALUES) )
