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

;; Normal-case
(test structure-no-keyword-name-normal
      (let ((instance nil))
        (defstruct otto a b c)
        (setq instance (make-otto :a 1 :b 2 :c 3))
        (multiple-value-bind
              (copy a b c)
            (values (copy-otto instance) (otto-a instance)(otto-b instance)(otto-c instance))
          (and (otto-p copy)(eql a 1)(eql b 2)(eql c 3)))))

;; Following the pattern of above
(setf (find-class 'otto) nil)

(test structure-keyword-name-normal
      (let ((instance nil))
        (defstruct :otto1 a b c)
        (setq instance (make-otto1 :a 1 :b 2 :c 3))
        (multiple-value-bind
              (copy a b c)
            (values (copy-otto1 instance) (otto1-a instance)(otto1-b instance)(otto1-c instance))
          (and (otto1-p copy)(eql a 1)(eql b 2)(eql c 3)))))

(setf (find-class 'otto1) nil)


;; has no OTTO2-P by definition in ansi
(test structure-keyword-name-list
      (let ((instance nil))
        (defstruct (:otto2 (:type list)) a b c)
        (setq instance (make-otto2 :a 1 :b 2 :c 3))
        (multiple-value-bind
              (copy a b c)
            (values (copy-otto2 instance) (otto2-a instance)(otto2-b instance)(otto2-c instance))
          (and (eql a 1)(eql b 2)(eql c 3)))))

(setf (find-class 'otto2) nil)

;; has no OTTO3-P by definition in ansi
(test structure-keyword-name-vector
      (let ((instance nil))
        (defstruct (:otto3 (:type vector)) a b c)
        (setq instance (make-otto3 :a 1 :b 2 :c 3))
        (multiple-value-bind
              (copy a b c)
            (values (copy-otto3 instance) (otto3-a instance)(otto3-b instance)(otto3-c instance))
          (and (eql a 1)(eql b 2)(eql c 3)))))

(setf (find-class 'otto3) nil)

