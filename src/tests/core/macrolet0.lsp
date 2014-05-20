(macrolet
    ((%x (y) `(+ 1 ,y))) (print (%x 10)))
