(let ((stream
        (open "/tmp/interpreter-info.sexp" :direction :output :if-does-not-exist :create :if-exists :supersede))
      ;; make symbols print with prefixes.
      (*package* (find-package "KEYWORD")))
  (unwind-protect
       (mapc #'(lambda (s)
                 (if (keywordp s)
                     nil ; we assume no functions classes etc...
                     (let ((ls (list*
                                s
                                (let ((sm (core:lookup-symbol-macro s)))
                                  (if sm
                                      (list (funcall sm s nil))
                                      nil))
                                (core:specialp s)
                                (if (constantp s)
                                    (list (symbol-value s))
                                    nil)
                                (if (find-class s nil) t nil)
                                (if (fboundp s)
                                    (list t (if (macro-function s) t nil) (special-operator-p s))
                                    (list nil nil nil)))))
                       (if (equal (cdr ls) '(nil nil nil nil nil nil nil)) ; nothing interesting
                           nil
                           (progn
                             (prin1 ls stream)
                             (terpri stream))))))
             ;; list of all symbols
             (let (syms)
               (mapc #'(lambda (package)
                         (multiple-value-call #'(lambda (external internal &rest rest)
                                                  (maphash #'(lambda (sym value)
                                                               (setq syms (cons value syms)))
                                                           external)
                                                  (maphash #'(lambda (sym value)
                                                               (setq syms (cons value syms)))
                                                           internal))
                           (core:package-hash-tables package)))
                     (list-all-packages))
               syms))
    (close stream)))
