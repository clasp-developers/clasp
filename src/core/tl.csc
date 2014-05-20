
(defun closeOver ( b )
  (let ( ( cb b ) )
    return (lambda ( a ) (println [ "closed over value(%d) arg(%d)" % a cb ] ))))


(global co (closeOver 99))
(funcall co 20 )




(let ( ( j 1 ) )
  (defun closeOver ( a )
    (println [ " arg = %d    j = %d" % a j ])))

(closeOver 10)
(closeOver 20)

(println "First test")
(let ( ( zz 1 ) ) 
  (dumpEnvironment)
  (println [ "zz = %d" % zz ] ))
(println "global a - should be put in global Environment")
(global a 1)
(println "global b - should be put in global Environment")
(global b 2)
(println [ "before let a(%d) b(%d)" % a b ])
(let ( (a 100) (b 200 ) )
  (println [ "in let" ])
  (println [ "in let a(%d) b(%d)" % a b ])
  (local btemp 999)
  (println [ "Just set btemp = %d" % btemp ])
  (dumpEnvironment)
  (let ( ( a 1000 ) (c 5000 ) )
    (println [ "in second-let a(%d) b(%d) c(%d) btemp(%d)" % a b c btemp ] )
    (dumpEnvironment ) ) )

        
println [ "after lets a(%d) b(%d)" % a b ]
