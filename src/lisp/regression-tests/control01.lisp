(in-package #:clasp-tests)

;;; all from failed ansi-tests
(TEST MULTIPLE-VALUE-SETQ.9
      (multiple-value-bind (a b)
          (LET (X)
            (VALUES (MULTIPLE-VALUE-SETQ (X X) (VALUES 1 2)) X))
        (and (= a 1) (= b 2))))

(TEST MULTIPLE-VALUE-SETQ.10
      (multiple-value-bind (a b)
          (LET (X)
        (VALUES (MULTIPLE-VALUE-SETQ (X X) (VALUES 1)) X))
        (and (= a 1) (null b))))

(TEST MULTIPLE-VALUE-SETQ.13 (eq :good (MULTIPLE-VALUE-SETQ NIL :good)))

(TEST MULTIPLE-VALUE-SETQ.14 (null (MULTIPLE-VALUE-SETQ NIL (VALUES))))

(TEST MULTIPLE-VALUE-SETQ.15 (eq 'a (MULTIPLE-VALUE-SETQ NIL (VALUES 'A 'B))))

(test progv-1
      (PROGV
          '(X Y Z W)
          '(1)
        (and (BOUNDP 'X) (not (BOUNDP 'Y)) (not (BOUNDP 'Z)) (not (BOUNDP 'W)))))

;;; To fix this, I would have to "unbind" a or set its binding to undefined
(test-expect-error
 progv-2
 (let ((a 1))(declare (special a))(progv '(a) nil a))
 :type unbound-variable)

(test progv-3
       (= 1 (let ((a 1))(declare (special a))(progv '(a) nil) (symbol-value 'a))))

(test-expect-error FBOUNDP.ERROR.3 (FBOUNDP '(setf)) :type type-error)
(test-expect-error FBOUNDP.ERROR.4 (FBOUNDP '(SETF aref . BAR)) :type type-error)
(test-expect-error FBOUNDP.ERROR.5 (FBOUNDP '(SETF aref BAR)) :type type-error)
(test-expect-error FBOUNDP.ERROR.9 (FBOUNDP '(SETF . aref)) :type type-error)

(test-expect-error fmakunbound.1 (fmakunbound '(setf)) :type type-error)
(test-expect-error fmakunbound.2 (fmakunbound '(SETF aref . BAR)) :type type-error)
(test-expect-error fmakunbound.3 (fmakunbound '(SETF aref BAR)) :type type-error)
(test-expect-error fmakunbound.4 (fmakunbound '(SETF . aref)) :type type-error)

(test-expect-error fdefinition.1 (fdefinition '(setf)) :type type-error)
(test-expect-error fdefinition.2 (fdefinition '(SETF aref . BAR)) :type type-error)
(test-expect-error fdefinition.3 (fdefinition '(SETF aref BAR)) :type type-error)
(test-expect-error fdefinition.4 (fdefinition '(SETF . aref)) :type type-error)
(test              fdefinition.5 (fdefinition '(SETF aref)))
(test-expect-error fdefinition.6 (fdefinition '(SETF  %%%nada%%%)) :type undefined-function)
(test-expect-error fdefinition.7 (fdefinition '%%%nada%%%) :type undefined-function)

(test-expect-error setf-fdefinition.1 (setf (fdefinition '(SETF)) #'(lambda(&rest was) nil)) :type type-error)
(test-expect-error setf-fdefinition.2 (setf (fdefinition '(SETF aref . BAR)) #'(lambda(&rest was))) :type type-error)
(test-expect-error setf-fdefinition.3 (setf (fdefinition '(SETF aref BAR)) #'(lambda(&rest was))) :type type-error)
(test-expect-error setf-fdefinition.4 (setf (fdefinition '(SETF . aref)) #'(lambda(&rest was))) :type type-error)
(test              setf-fdefinition.5 (setf (fdefinition '%%%nada%%%) #'(lambda(&rest was))))

(test equalp-babel
      (equalp 
       (make-array 4 :element-type 'character :initial-contents (list #\a #\SUB #\b #\c))
       #(#\a #\Sub #\b #\c)))

(test notnot (eql t (let()
                      (not (not 3)))))

(test-expect-error flet-too-few (flet ((%f (a) a)) (%f)) :type program-error)
(test-expect-error flet-too-many (flet ((%f (a) a)) (%f 1 2)) :type program-error)

(test issue-930
      (let ()
        (eq :good
            (handler-case
                (progn
                  (FUNCALL 'PROGN 1)
                  :bad)
              (undefined-function (uf)
                (if (eq (cell-error-name uf) 'progn)
                    :good
                    :bad))))))


