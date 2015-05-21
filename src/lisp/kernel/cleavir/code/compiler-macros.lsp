
(defun pairwise-expand (op args)
  (if (= (length args) 2)
      `(,op ,(first args) ,(second args))
      `(,op ,(first args) ,(pairwise-expand op (cdr args)))))

(defmacro my+ (&rest args)
  (if (= (length args) 1)
      (car args)
      (pairwise-expand 'binary+ args)))


(defun binary+ (x y)
  (cond
    ((and (fixnump x) (fixnump y))
     (cleavir-primop:fixnum-add x y))
    ((and (single-float-p x) (single-float-p y))
     ;;unbox x&y, add them, box result
     )
    ;; Many more cases
    (t (contagen-add x y))))

     
