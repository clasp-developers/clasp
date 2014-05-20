#|  sbcl version

(make-package "ANSI-LEWP")
(make-package "EXT" :use '(:cl))
(cl:in-package :ext)
(cl:intern "EXT::ASSUME-RIGHT-TYPE")
(cl:export '(EXT::ASSUME-RIGHT-TYPE))
(cl:in-package :cl-user)
*package*
(declaim (declaration ext:assume-right-type))
(make-package "SI")
(make-package "SYS")
(defun cons-car (x) (car x))
(defun cons-cdr (x) (cdr x))

(macroexpand-1 '(setf (gethash 'a ht) 100))
|#

(load "lewp.lsp")


(trace
 lewp-standard-expansion
       lewp-translate
       lewp-iteration-driver
       lewp-bind-block
       lewp-lookup-keyword
       lewp-universe-keywords
       lewp-for-arithmetic
       lewp-check-data-type
       lewp-collect-prepositional-phrases
       lewp-sequencer
       lewp-declare-variable
       )

(lewp for i from 1.0 to 3.0 by 0.5 do (print i))



#|

(find :a '(:a :b :c))


(macroexpand-1 '(do-sequence (elt sequence start end :output output :index index :specialize t) (print "Hello")))

(defparameter args '(:a :b :c :d))
(macroexpand-1 '(remf args :c))
