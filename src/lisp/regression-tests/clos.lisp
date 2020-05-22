(in-package #:clasp-tests)

(defclass test ()((foo :initform :bar :accessor test-foo)))
(test-expect-error accessor-too-many-args-1
                   (test-foo (make-instance 'test) 23 24 25))

(test-expect-error defclass-1 (eval '(defclass foo ()(bar bar))) :type program-error)

(test JIT-FUNCTION-NAME-1
      (equal '(NIL (3 (3 . 2)))
             (multiple-value-list
              (let* ((sym (gensym))
                     (method
                      (eval `(defmethod (setf ,sym) ((x t) (y cons)) (setf (car y) x)))))
                (values
                 (fboundp sym)
                 (let ((x (cons 1 2))) (list (funcall (fdefinition `(setf ,sym)) 3 x) x)))))))

(test-expect-error defclass-2 (eval '(defclass xxx nil nil nil))  :type program-error)

(defmethod foo-bar ((vector vector)) vector)

(test issue-698
      (vectorp (foo-bar (make-array 23 :adjustable T))))

#+cst (test-expect-error make-instance.error.5 (let ()(make-instance)) :type program-error)

(defclass slot-missing-class-01 () (a))

(defmethod slot-missing ((class t) (obj slot-missing-class-01)
                         (slot-name t) (operation t)
                         &optional (new-value nil new-value-p))
  42)

(test slot-missing-2-simplified
      (eq 'bar
          (let ((obj (make-instance 'slot-missing-class-01)))
            (setf (slot-value obj 'foo) 'bar))))

(defstruct foo-make-load-form-saving-slots a b c)

(test make-load-form-saving-slots-defstruct
      (let ((object (make-foo-make-load-form-saving-slots :a 'foo :b '(1 2) :c 23)))
        (multiple-value-bind
              (form-allocation form-initialisation)
            (make-load-form-saving-slots object)
          (let ((new-object (eval form-allocation)))
            (eval (subst new-object object form-initialisation))
            (eq (class-of object) (class-of new-object))))))

(defclass foo-make-load-form-saving-slots-class ()((a :initform :a)))

(test make-load-form-saving-slots-class
      (let ((object (make-instance 'foo-make-load-form-saving-slots-class)))
        (multiple-value-bind
              (form-allocation form-initialisation)
            (make-load-form-saving-slots object)
          (let ((new-object (eval form-allocation)))
            (eval (subst new-object object form-initialisation))
            (eq (class-of object) (class-of new-object))))))
            
(test-expect-error
 defclass-error-options-1
 (eval '(defclass erroneous-class.13 ()
         (a b c)
         (#.(gensym))))
 :type program-error)

(test-expect-error
 defclass-error-options-2
 (eval '(defclass erroneous-class.13 ()
         (a b c)
         (:illegal-option nil)))
 :type program-error)

(test slot-exists-p-gives-single-value-slot-exists
      (null (cdr
             (multiple-value-list
              (slot-exists-p (make-instance 'test) 'foo)))))

(test slot-exists-p-gives-single-value-slot-not-exists
      (null (cdr
             (multiple-value-list
              (slot-exists-p (make-instance 'test) 'fooasdasdasd)))))

(test slot-exists-p-gives-single-value-slot-not-exists-build-in-class
      (null (cdr
             (multiple-value-list
              (slot-exists-p (find-class 'test) 'fooasdasdasd)))))

(test slot-exists-p-other-arguments
      (notany  #'(lambda(object)
                   (slot-exists-p object 'foo))
               (list 42 42.0 'c "324789" (code-char 65)
                     (vector 1 2 3) (make-hash-table))))
