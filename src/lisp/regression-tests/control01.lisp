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

(defun dummy (a) a)
(defun (setf dummy) (new old)
  "asdjhaskdhj"
  (list new old))

(test-expect-error FBOUNDP.ERROR.3 (FBOUNDP '(setf)) :type type-error)
(test-expect-error FBOUNDP.ERROR.4 (FBOUNDP '(SETF dummy . BAR)) :type type-error)
(test-expect-error FBOUNDP.ERROR.5 (FBOUNDP '(SETF dummy BAR)) :type type-error)
(test-expect-error FBOUNDP.ERROR.9 (FBOUNDP '(SETF . dummy)) :type type-error)

(test-expect-error fmakunbound.1 (fmakunbound '(setf)) :type type-error)
(test-expect-error fmakunbound.2 (fmakunbound '(SETF dummy . BAR)) :type type-error)
(test-expect-error fmakunbound.3 (fmakunbound '(SETF dummy BAR)) :type type-error)
(test-expect-error fmakunbound.4 (fmakunbound '(SETF . dummy)) :type type-error)

(test-expect-error fdefinition.1 (fdefinition '(setf)) :type type-error)
(test-expect-error fdefinition.2 (fdefinition '(SETF dummy . BAR)) :type type-error)
(test-expect-error fdefinition.3 (fdefinition '(SETF dummy BAR)) :type type-error)
(test-expect-error fdefinition.4 (fdefinition '(SETF . dummy)) :type type-error)
(test              fdefinition.5 (fdefinition '(SETF dummy)))
(test-expect-error fdefinition.6 (fdefinition '(SETF  %%%nada%%%)) :type undefined-function)
(test-expect-error fdefinition.7 (fdefinition '%%%nada%%%) :type undefined-function)

(test-expect-error setf-fdefinition.1 (setf (fdefinition '(SETF)) #'(lambda(&rest was) nil)) :type type-error)
(test-expect-error setf-fdefinition.2 (setf (fdefinition '(SETF dummy . BAR)) #'(lambda(&rest was))) :type type-error)
(test-expect-error setf-fdefinition.3 (setf (fdefinition '(SETF dummy BAR)) #'(lambda(&rest was))) :type type-error)
(test-expect-error setf-fdefinition.4 (setf (fdefinition '(SETF . dummy)) #'(lambda(&rest was))) :type type-error)
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

(test issue-780
      (equalp '(2 1)
              (macrolet ((%m (&key ((:a (b c)))) `(quote (,c ,b))))
    (%m :a (1 2)))))

;;; Test that setf expanders are shadowed by flet, labels, and macrolet.
(define-setf-expander issue-982-place (place &environment env)
  (get-setf-expansion `(cdr ,place) env))

(test issue-982-flet
      (flet ((issue-982-place (c) (car c))
             ((setf issue-982-place) (n c) (setf (car c) n)))
        (declare (ignore #'issue-982-place))
        (let ((c (cons nil nil)))
          (setf (issue-982-place c) t)
          (car c))))

(test issue-982-labels
      (labels ((issue-982-place (c) (car c))
               ((setf issue-982-place) (n c) (setf (car c) n)))
        (declare (ignore #'issue-982-place))
        (let ((c (cons nil nil)))
          (setf (issue-982-place c) t)
          (car c))))

(test issue-982-macrolet
      (macrolet ((issue-982-place (c) `(car ,c)))
        (let ((c (cons nil nil)))
          (setf (issue-982-place c) t)
          (car c))))

;;; Test that setf uses macroexpand-1, not macroexpand,
;;; and therefore can prefer setf expanders to macro expanders.
(define-setf-expander macro-place-1 (place &environment env)
  (get-setf-expansion `(cdr ,place) env))

(defmacro macro-place-1 (place) `(car ,place))

(test macro-place-1
      (macrolet ((macro-place-pre (place) `(macro-place-1 ,place)))
        (let ((c (cons nil nil)))
          (setf (macro-place-pre c) t)
          (cdr c))))
