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


