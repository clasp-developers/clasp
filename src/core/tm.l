

(load "macros.lisp")


(print (macroexpand-1 '(defsdmethod a fixnum (x) a b c d))  )




;;(defsdmethod addstuff fixnum (x)
;;	     (+ 10 x))
