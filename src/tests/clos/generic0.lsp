
(clos:defgeneric xx (a b))

(clos:defmethod xx ((a integer) (b integer))
  (bformat t "Integers %d,%d\n" a b))

(clos:defmethod xx ((a integer) (b symbol))
  (bformat t "Integer %d   Symbol %s\n" a b))

(xx 1 2)

(xx 1 'hello)
