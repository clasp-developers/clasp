(in-package #:clasp-tests)

;;; all from failed ansi-tests
(TEST MULTIPLE-VALUE-SETQ.9
      (LET (X)
        (VALUES (MULTIPLE-VALUE-SETQ (X X) (VALUES 1 2)) X))
      (1 2))

(TEST MULTIPLE-VALUE-SETQ.10
      (VALUES (MULTIPLE-VALUE-SETQ (X X) (VALUES 1)) X)
      (1 nil))

(TEST MULTIPLE-VALUE-SETQ.13 (MULTIPLE-VALUE-SETQ NIL :good) (:good))

(TEST MULTIPLE-VALUE-SETQ.14 (MULTIPLE-VALUE-SETQ NIL (VALUES)) (nil))

(TEST MULTIPLE-VALUE-SETQ.15 (MULTIPLE-VALUE-SETQ NIL (VALUES 'A 'B)) (a))

(test progv-1
      (PROGV '(X Y Z W) '(1)
        (values (boundp 'x) (boundp 'y) (boundp 'z) (boundp 'w)))
      (t nil nil nil))

;; Test for mv-call problem fixed in 69d0ed83
(test multiple-value-call.1
      (locally
          (declare (notinline values))
        (multiple-value-call #'values (values 4) 5 (values)))
      (4 5))

;;; To fix this, I would have to "unbind" a or set its binding to undefined
(test-expect-error progv-2
                   (let ((a 1)) (declare (special a)) (progv '(a) nil a))
                   :type unbound-variable)

(test progv-3
      (let ((a 1))(declare (special a))(progv '(a) nil) (symbol-value 'a))
      (1))

(defun dummy (a) a)
(defun (setf dummy) (new old)
  "asdjhaskdhj"
  (list new old))

(test-expect-error FBOUNDP.ERROR.3 (FBOUNDP '(setf)) :type type-error)
(test-expect-error FBOUNDP.ERROR.4 (FBOUNDP '(SETF dummy . BAR)) :type type-error)
(test-expect-error FBOUNDP.ERROR.5 (FBOUNDP '(SETF dummy BAR)) :type type-error)
(test-expect-error FBOUNDP.ERROR.9 (FBOUNDP '(SETF . dummy)) :type type-error)
(test-expect-error FBOUNDP.ERROR.10 (FBOUNDP '(SETF nil nil)) :type type-error)
(test FBOUNDP.11 (FBOUNDP '(SETF nil)) (nil))
(test FBOUNDP.12 (core:valid-function-name-p '(SETF nil)) (t))
(test FBOUNDP.13 (core:function-block-name '(SETF nil)) (nil))


(test-expect-error fmakunbound.1 (fmakunbound '(setf)) :type type-error)
(test-expect-error fmakunbound.2 (fmakunbound '(SETF dummy . BAR)) :type type-error)
(test-expect-error fmakunbound.3 (fmakunbound '(SETF dummy BAR)) :type type-error)
(test-expect-error fmakunbound.4 (fmakunbound '(SETF . dummy)) :type type-error)

(test-expect-error fdefinition.1 (fdefinition '(setf)) :type type-error)
(test-expect-error fdefinition.2 (fdefinition '(SETF dummy . BAR)) :type type-error)
(test-expect-error fdefinition.3 (fdefinition '(SETF dummy BAR)) :type type-error)
(test-expect-error fdefinition.4 (fdefinition '(SETF . dummy)) :type type-error)
(test-true         fdefinition.5 (fdefinition '(SETF dummy)))
(test-expect-error fdefinition.6 (fdefinition '(SETF  %%%nada%%%)) :type undefined-function)
(test-expect-error fdefinition.7 (fdefinition '%%%nada%%%) :type undefined-function)

(test-expect-error setf-fdefinition.1 (setf (fdefinition '(SETF)) #'(lambda(&rest was) (declare (ignore was)) nil)) :type type-error)
(test-expect-error setf-fdefinition.2 (setf (fdefinition '(SETF dummy . BAR)) #'(lambda(&rest was)(declare (ignore was)))) :type type-error)
(test-expect-error setf-fdefinition.3 (setf (fdefinition '(SETF dummy BAR)) #'(lambda(&rest was)(declare (ignore was)))) :type type-error)
(test-expect-error setf-fdefinition.4 (setf (fdefinition '(SETF . dummy)) #'(lambda(&rest was)(declare (ignore was)))) :type type-error)
(test-true              setf-fdefinition.5 (setf (fdefinition '%%%nada%%%) #'(lambda(&rest was)(declare (ignore was)))))

(test-true equalp-babel
      (equalp 
       (make-array 4 :element-type 'character :initial-contents (list #\a #\SUB #\b #\c))
       #(#\a #\Sub #\b #\c)))

(test notnot (let () (not (not 3))) (t))

(test-expect-error flet-too-few (flet ((%f (a) a)) (%f)) :type program-error)
(test-expect-error flet-too-many (flet ((%f (a) a)) (%f 1 2)) :type program-error)

(test issue-930
      (let ()
        (handler-case
            (progn
              (FUNCALL 'PROGN 1)
              :bad)
          (undefined-function (uf)
            (if (eq (cell-error-name uf) 'progn)
                :good
                :bad))))
      (:good))

(test issue-780
      (macrolet ((%m (&key ((:a (b c)))) `(quote (,c ,b))))
        (%m :a (1 2)))
      ((2 1)))

;;; Test that setf expanders are shadowed by flet, labels, and macrolet.
(define-setf-expander issue-982-place (place &environment env)
  (get-setf-expansion `(cdr ,place) env))

(test issue-982-flet
      (flet ((issue-982-place (c) (car c))
             ((setf issue-982-place) (n c) (setf (car c) n)))
        (declare (ignore #'issue-982-place))
        (let ((c (cons nil nil)))
          (setf (issue-982-place c) t)
          (car c)))
      (t))

(test issue-982-labels
      (labels ((issue-982-place (c) (car c))
               ((setf issue-982-place) (n c) (setf (car c) n)))
        (declare (ignore #'issue-982-place))
        (let ((c (cons nil nil)))
          (setf (issue-982-place c) t)
          (car c)))
      (t))

(test issue-982-macrolet
      (macrolet ((issue-982-place (c) `(car ,c)))
        (let ((c (cons nil nil)))
          (setf (issue-982-place c) t)
          (car c)))
      (t))

;;; Test that setf uses macroexpand-1, not macroexpand,
;;; and therefore can prefer setf expanders to macro expanders.
(define-setf-expander macro-place-1 (place &environment env)
  (get-setf-expansion `(cdr ,place) env))

(defmacro macro-place-1 (place) `(car ,place))

(test macro-place-1
      (macrolet ((macro-place-pre (place) `(macro-place-1 ,place)))
        (let ((c (cons nil nil)))
          (setf (macro-place-pre c) t)
          (cdr c)))
      (t))

;;; https://github.com/clasp-developers/clasp/commit/f276e6867a807845bea17e893ffc3723e4c5ac13
(test-true equalp-10
      (not (equalp 1.0s0 (1+ most-positive-fixnum))))

(test-true equalp-11
      (not (equalp 1.0s0 2.0d0)))
