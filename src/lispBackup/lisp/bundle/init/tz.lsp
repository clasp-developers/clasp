(defparameter lst1 (list "A" "b" "C" "d"))
(defparameter lst2 (list "a" "B" "C" "d"))

(set-difference lst1 lst2 :test 'eq) ;;  =>  ("d" "C" "b" "A")
(set-difference lst1 lst2 :test 'equal) ;;  ("b" "A")
(set-difference lst1 lst2 :test #'equalp) ;;  NIL 
(nset-difference lst1 lst2 :test #'string=) ;;  ("A" "b")


(member "A" '("A" "B" "C" 3 4))
