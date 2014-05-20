(destructuring-bind (a b (c d) &aux (x 10) (y 20))
    (list 1 2 (list 3 4))
  (bformat t "a,b,c,d,x,y = %d,%d,%d,%d,%d,%d\n" a b c d x y))
