;; Bug 444
(defstruct defstruct-predicate.a)
(defstruct (defstruct-predicate.b (:include defstruct-predicate.a)))

(test defstruct-predicate
      (defstruct-predicate.a-p (make-defstruct-predicate.b)))

(setf (find-class 'defstruct-predicate.a) nil
      (find-class 'defstruct-predicate.b) nil)

;; Bug 445
(defstruct copy-structure.foo bar)

(test copy-structure
      (let* ((foo1 (make-copy-structure.foo :bar nil))
             (foo2 (copy-structure foo1)))
        (setf (copy-structure.foo-bar foo2) t)
        (not (eq (copy-structure.foo-bar foo1) (copy-structure.foo-bar foo2)))))

(setf (find-class 'copy-structure.foo) nil)
