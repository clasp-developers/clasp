(make-package "IR" :use '("CL"))
(in-package :ir)

(defun %%uword (i)
  "Create an unsigned word constant - currently only i64 is supported"
  (jit-constant-i64 i))

(defun %sub (x y &key (label "") has-nuw has-nsw)
  (llvm-sys:create-sub cmp:*ir-builder* x y has-nuw has-nsw))

(defun %and (x y &key (label ""))
  (llvm-sys:create-and cmp:*ir-builder* x y label))

(defun %icmp-eq (x y &key (label ""))
  (llvm-sys:create-cmp-eq cmp:*ir-builder* x y label))

(defun %cond-br (val br-true br-false &key likely-true likely-false)
  (and likely-true likely-false (error "Only one of likely-true or likely-false can be true"))
  (llvm-sys:create-cond-br val br-true br-false nil))


#||
CONSP instruction
(let* ((xcons (car args)) (cmp (%icmp-eq (%and xcons (%%uword 7)) (%%uword 3)))) (%br cmp consp-true consp-false :likely-true t) ...)


CAR instruction

(if (consp x) 
    (gep 0 1 x)
    (if (null x)
        nil
        (error "Illegal car")))

||#
