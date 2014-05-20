(eval-when (:execute :load-toplevel :compile-toplevel)
  (defconstant +known-typep-predicates+
    '((ARRAY . ARRAYP)
      (ATOM . ATOM)
      (T . CONSTANTLY-T)
      (VECTOR . VECTORP))))
