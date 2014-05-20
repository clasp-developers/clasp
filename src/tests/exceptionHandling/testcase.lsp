(load-boot :cmp)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defconstant +known-typep-predicates+
    '((ARRAY . ARRAYP)
      (ATOM . ATOM))))

(setq cmp:*debug-compiler* t)

(compile nil '(lambda () (dolist (l +known-typep-predicates+)
		       (put-sysprop (car l) 'TYPE-PREDICATE (cdr l)))))
