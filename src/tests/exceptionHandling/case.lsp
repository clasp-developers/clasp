(eval-when (:execute :load-toplevel :compile-toplevel)
  (defconstant +known-typep-predicates+
    '((ARRAY . ARRAYP)
      (ATOM . ATOM))))


(dolist (l +known-typep-predicates+)
  (put-sysprop (car l) 'TYPE-PREDICATE (cdr l)))
