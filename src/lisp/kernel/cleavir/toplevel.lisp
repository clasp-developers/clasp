(in-package :clasp-cleavir)

(defun function-name-p (name)
  (or (symbolp name)
      (and (consp name)
           (eq (car name) 'setf)
           (consp (cdr name))
           (null (cddr name)))))

(defun augment-environment-with-declares (declares &optional env)
  (mapc (lambda (decl)
          (let ((head (car decl))
                (rest (cdr decl)))
            (case head
              (special
               (mapc (lambda (sym)
                       (setq env (cleavir-environment:add-special-variable env sym)))
                     rest))
              (optimize
               (mapc (lambda (quality)
                       (setq env (if (atom quality)
                                     (cleavir-environment:add-optimize env quality 3)
                                     (cleavir-environment:add-optimize env
                                                                       (car quality)
                                                                       (cadr quality)))))
                     rest))
              (inline
               (mapc (lambda (fn)
                       (setq env (cleavir-environment:add-inline env fn 'cl:inline)))
                     rest))
              (notinline
               (mapc (lambda (fn)
                       (setq env (cleavir-environment:add-inline env fn 'cl:notinline)))
                     rest))
              (t nil))))
        declares)
  env)

(defun augment-environment-with-macrolet (macros env)
  (mapc (lambda (macro)
          (setq env (cleavir-environment:add-local-macro env (car macro) (cdr macro))))
        macros)
  env)

(defun augment-environment-with-symbol-macrolet (macros env)
  (mapc (lambda (macro)
          (setq env (cleavir-environment:add-local-symbol-macro env (car macro) (cdr macro))))
        macros)
  env)

(defun cclasp-eval (form &optional env)
;;  (format t "cclasp-eval eval: ~a~%" form)
  (flet ((eval-progn (body &optional (penv env))
           (loop for (form . next) on body
              if next
              do (cclasp-eval form penv)
              else
              return (cclasp-eval form penv)))
         (eval-compile (form)
           (funcall (cclasp-compile-in-env nil `(lambda () ,form) env))))
    (let ((form (macroexpand form env)))
      (typecase form
        (symbol
         (symbol-value form))
        (atom
         form)
        (cons
         (let* ((name (car form))
                (body (cdr form))
                (arg-length (length body)))
           (if (and (fboundp name)
                    (not (special-operator-p name)))
               (apply name (loop for arg in body
                              collect (cclasp-eval arg env)))
               (case name
                 (quote
                  (assert (= arg-length 1))
                  (car body))
                 (function
                  (assert (= arg-length 1))
                  (let ((name (car body)))
                    (if (function-name-p name)
                        (fdefinition name)
                        (eval-compile form))))
                 (progn
                   (eval-progn body))
                 (eval-when
                     (assert (listp (car body)))
                   (when (or (member :execute (car body))
                             (member 'eval (car body)))
                     (eval-progn (cdr body))))
                 (locally
                     (multiple-value-bind (decls body) (core:process-declarations body nil)
                       (eval-progn body (augment-environment-with-declares decls env))))
                 (macrolet
                     (multiple-value-bind (macros declares macrolet-body)
                         (cmp:parse-macrolet body)
                       (let ((ml-env (augment-environment-with-macrolet macros env)))
                         (eval-progn macrolet-body (augment-environment-with-declares declares ml-env)))))
                 (symbol-macrolet
                     (multiple-value-bind (macros declares macrolet-body)
                         (cmp:parse-symbol-macrolet body)
                       (let ((ml-env (augment-environment-with-symbol-macrolet macros env)))
                         (eval-progn macrolet-body (augment-environment-with-declares declares ml-env)))))
                 (t (eval-compile form))))))))))
