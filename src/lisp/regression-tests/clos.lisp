(in-package #:clasp-tests)

(defclass test ()((foo :initform :bar :accessor test-foo)))
(test-expect-error accessor-too-many-args-1
                   (test-foo (make-instance 'test) 23 24 25))

(test-expect-error defclass-1 (eval '(defclass foo ()(bar bar))) :type program-error)

(test JIT-FUNCTION-NAME-1
      (let* ((sym (gensym))
             (method
               (eval `(defmethod (setf ,sym) ((x t) (y cons)) (setf (car y) x)))))
        (declare (ignore method))
        (values
         (fboundp sym)
         (let ((x (cons 1 2))) (list (funcall (fdefinition `(setf ,sym)) 3 x) x))))
      (NIL (3 (3 . 2))))

(test-expect-error defclass-2 (eval '(defclass xxx nil nil nil))  :type program-error)

(defmethod foo-bar ((vector vector)) vector)

;; if 698 is broken, this would signal an error
(test-true issue-698 (foo-bar (make-array 23 :adjustable T)))

(test-expect-error make-instance.error.5 (let ()(make-instance)) :type program-error)

(defclass slot-missing-class-01 () (a))

(defmethod slot-missing ((class t) (obj slot-missing-class-01)
                         (slot-name t) (operation t)
                         &optional (new-value nil new-value-p))
  (declare (ignore new-value))
  42)

(test slot-missing-2-simplified
      (let ((obj (make-instance 'slot-missing-class-01)))
        (setf (slot-value obj 'foo) 'bar))
      (bar))

(defstruct foo-make-load-form-saving-slots a b c)

(test-true make-load-form-saving-slots-defstruct
           (let ((object (make-foo-make-load-form-saving-slots :a 'foo :b '(1 2) :c 23)))
             (multiple-value-bind
                   (form-allocation form-initialisation)
                 (make-load-form-saving-slots object)
               (let ((new-object (eval form-allocation)))
                 (eval (subst new-object object form-initialisation))
                 (eq (class-of object) (class-of new-object))))))

(defclass foo-make-load-form-saving-slots-class ()((a :initform :a)))

(test-true make-load-form-saving-slots-class
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

(test-true slot-exists-p-gives-single-value-slot-exists
           (null (cdr
                  (multiple-value-list
                   (slot-exists-p (make-instance 'test) 'foo)))))

(test-true slot-exists-p-gives-single-value-slot-not-exists
           (null (cdr
                  (multiple-value-list
                   (slot-exists-p (make-instance 'test) 'fooasdasdasd)))))

(test-true slot-exists-p-gives-single-value-slot-not-exists-build-in-class
           (null (cdr
                  (multiple-value-list
                   (slot-exists-p (find-class 'test) 'fooasdasdasd)))))

(test-true slot-exists-p-other-arguments
           (notany #'(lambda (object) (slot-exists-p object 'foo))
                   (list 42 42.0 'c "324789" (code-char 65)
                         (vector 1 2 3) (make-hash-table))))

(defgeneric find-method-gf-02 (x))
(defmethod find-method-gf-02 ((x (eql 1234567890))) 'a)
(test-true find-method-eql
           (find-method #'find-method-gf-02 nil (list '(eql 1234567890))))


(defclass %foo-1 ()
  ((a :initform :a)))

(defmethod initialize-instance ((me  %foo-1) &rest initargs &key policy provider (hash-test 'eql) &allow-other-keys)
  (declare (ignore initargs policy provider hash-test))
  (call-next-method)
  23)

(test-type test-issue-1031 (make-instance '%foo-1 :a 1) %foo-1)

(defgeneric congruent-gf (a &key b &allow-other-keys))
(test-true clos-lambda-list-congruent-allow-other-keys
           (defmethod congruent-gf (a &key)
             a))

(defgeneric q (object))
(defmethod q ((list list)))
(defmethod q ((s symbol)))
(defmethod q ((true (eql t))) t)
(defmethod q ((n (eql nil))) t)
(test-true eql-specializers-from-irc (q t))

(test-true issue-1244 (typep (clos:ensure-class-using-class nil (gentemp)) 'standard-class))

(test class-slot-inheritance ; issue #1392
      (progn (setf (find-class 'testp) nil
                   (find-class 'testc1) nil
                   (find-class 'testc2) nil
                   (find-class 'testgc1) nil
                   (find-class 'testgc2) nil)
             (defclass testp ()
               ((%s :initform :parent :allocation :class :reader s)))
             (defclass testc1 (testp) ())
             (defclass testc2 (testp)
               ((%s :initform :child :allocation :class)))
             (defclass testgc1 (testc1 testc2) ())
             (defclass testgc2 (testc2 testc1) ())
             (values (s (make-instance 'testgc1))
                     (s (make-instance 'testgc2))))
      (:child :child))

;;; This test relates to a problem I hit where instances of certain
;;; classes, like SIMPLE-BASE-STRING, always made calls go through
;;; the slow path.
;;; Incidentally also tests the CLOS profiler.
(test dispatch-miss-perf
      (progn
        (fmakunbound 'dispatch-miss-perf.f)
        (defmethod dispatch-miss-perf.f (x) x)
        ;; Another method, to make the discriminator nontrivial.
        (defmethod dispatch-miss-perf.f ((x integer)) x)
        (let ((objects (list '(1) 'generic-function #() "hello"
                             13 3.2 4/3 #c(1 2) #\d (make-hash-table)
                             #'dispatch-miss-perf.f *readtable*
                             *package* *load-pathname*
                             *terminal-io* *random-state*)))
          ;; Call the function with each argument to fill the call history.
          (mapc #'dispatch-miss-perf.f objects)
          ;; Call it again, and this time it ought to never miss.
          (clos:with-profiling (dispatch-miss-perf.f) (:report nil)
            (mapc #'dispatch-miss-perf.f objects)
            (clos:profiling-data #'dispatch-miss-perf.f))))
      (0 0.0))
