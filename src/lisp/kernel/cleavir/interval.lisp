(in-package #:clasp-cleavir)

(defstruct (interval (:constructor make-interval (low high)))
  ;; nil means unbounded. list means exclusive.
  (low nil :type (or null real (cons real null)))
  (high nil :type (or null real (cons real null))))

(defun bound-parts (finite-bound)
  (if (consp finite-bound)
      (values (car finite-bound) t)
      (values finite-bound nil)))

(defun finite-bound-binop (binop fb1 fb2)
  (multiple-value-bind (b1 bxp1) (bound-parts fb1)
    (multiple-value-bind (b2 bxp2) (bound-parts fb2)
      (let ((r (funcall binop b1 b2)))
        (if (or bxp1 bxp2) (list r) r)))))

(defun finite-bound-unop (unop fb)
  (multiple-value-bind (b bxp) (bound-parts fb)
    (let ((r (funcall unop b)))
      (if bxp (list r) r))))

(defun interval+ (i1 i2)
  (make-interval
   (let ((l1 (interval-low i1)) (l2 (interval-low i2)))
     (if (and l1 l2) (finite-bound-binop #'+ l1 l2) nil))
   (let ((h1 (interval-high i1)) (h2 (interval-high i2)))
     (if (and h1 h2) (finite-bound-binop #'+ h1 h2) nil))))

(defun interval-negate (interval)
  (make-interval
   (let ((high (interval-high interval)))
     (if high (finite-bound-unop #'- high) nil))
   (let ((low (interval-low interval)))
     (if low (finite-bound-unop #'- low) nil))))

;; Return -1 if the number is below the interval, 1 if above, 0 if in the interval.
(defun interval-num-compare (interval num)
  (let ((low (interval-low interval)) (high (interval-high interval)))
    (cond ((and low (if (consp low) (>= (car low) num) (> low num))) -1)
          ((and high (if (consp high) (<= (car high) num) (< high num))) 1)
          (t 0))))

(defun split-interval (interval where)
  (ecase (interval-num-compare interval where)
    ((-1) (values nil interval))
    ((1) (values interval nil))
    ((0)
     (values (make-interval (interval-low interval) where)
             (make-interval where (interval-high interval))))))

;; Multiply two intervals that are both positive, i.e. have lower bounds
;; that are at least zero.
(defun interval*-both-pos (i1 i2)
  (make-interval
   ;; the lower bounds are necessarily finite.
   (finite-bound-binop #'* (interval-low i1) (interval-low i2))
   (let ((h1 (interval-high i1)) (h2 (interval-high i2)))
     (if (and h1 h2)
         (finite-bound-binop #'* h1 h2)
         nil))))

(defun interval-merge (i1 i2)
  ;; Return the smallest interval including both input intervals.
  ;; If the inputs do not intersect, this will not be a strict join.
  (labels ((fb< (b1 b2)
             (multiple-value-bind (b1 xp1) (bound-parts b1)
               (multiple-value-bind (b2 xp2) (bound-parts b2)
                 (or (< b1 b2) (and (= b1 b2) xp1 (not xp2))))))
           (lbmin (b1 b2)
             (if (and b2 (or (not b1) (fb< b1 b2))) b1 b2))
           (hbmax (b1 b2)
             (if (and b1 (or (not b2) (fb< b1 b2))) b2 b1)))
    (make-interval (lbmin (interval-low i1) (interval-low i2))
                   (hbmax (interval-high i1) (interval-high i2)))))

;; Multiply a positive interval by any interval.
(defun interval*-1-pos (i1 i2)
  (multiple-value-bind (i2L i2H) (split-interval i2 0)
    (let ((rL (and i2L (interval-negate
                        (interval*-both-pos i1 (interval-negate i2L)))))
          (rH (and i2H (interval*-both-pos i1 i2H))))
      (if rL
          (if rH
              (interval-merge rL rH)
              rL)
          rH))))

(defun interval* (i1 i2)
  (multiple-value-bind (i1L i1H) (split-interval i1 0)
    (multiple-value-bind (i2L i2H) (split-interval i2 0)
      (let* ((i1L (and i1L (interval-negate i1L)))
             (i2L (and i2L (interval-negate i2L)))
             (iLL (and i1L i2L (interval*-both-pos i1L i2L)))
             (iLH (and i1L i2H (interval-negate (interval*-both-pos i1L i2H))))
             (iHL (and i1H i2L (interval-negate (interval*-both-pos i1H i2L))))
             (iHH (and i1H i2H (interval*-both-pos i1H i2H))))
        (labels ((pim (i1 i2)
                   (cond ((not i1) i2)
                         ((not i2) i1)
                         (t (interval-merge i1 i2)))))
          (pim iLL (pim iLH (pim iHL iHH))))))))

;;; Returns two values - one an all-negative interval and one all-positive.
;;; Either can be NIL if empty.
;;; This is necessary because the reciprocal of a zero-crossing interval has
;;; a hole in the middle, i.e. is not strictly an interval itself.
(defun interval-reciprocal (interval)
  (multiple-value-bind (low lxp) (bound-parts (interval-low interval))
    (multiple-value-bind (high hxp) (bound-parts (interval-high interval))
      (values
       (if (and low (> low 0))
           nil ; no interval
           (make-interval
            (cond ((or (not high) (>= high 0)) nil)
                  (hxp (list (/ high)))
                  (t (/ high)))
            (cond ((not low) '(0))
                  (lxp (list (/ low)))
                  (t (/ low)))))
       (if (and high (< high 0))
           nil
           (make-interval
            (cond ((not high) '(0))
                  (hxp (list (/ high)))
                  (t (/ high)))
            (cond ((or (not low) (<= low 0)) nil)
                  (lxp (list (/ low)))
                  (t (/ low)))))))))

;;; Compute the reciprocal of an all-positive interval, i.e. low must be finite
;;; and greater than zero (or 0 exclusive).
(defun interval-reciprocal-+ (interval)
  (multiple-value-bind (low lxp) (bound-parts (interval-low interval))
    (multiple-value-bind (high hxp) (bound-parts (interval-high interval))
      (make-interval (cond ((not high) '(0))
                           (hxp (list (/ high)))
                           (t (/ high)))
                     (cond ((not lxp) (/ low))
                           ((zerop low) nil)
                           (t (list (/ low))))))))

(defun approximate-interval-reciprocal (interval)
  (multiple-value-bind (minus plus) (interval-reciprocal interval)
    (if minus
        (if plus
            (interval-merge minus plus)
            minus)
        plus)))

(defun interval/ (i1 i2)
  (multiple-value-bind (m2 p2) (interval-reciprocal i2)
    (values (if m2 (interval* i1 m2) nil) (if p2 (interval* i1 p2) nil))))

(defun approximate-interval/ (i1 i2)
  (multiple-value-bind (m2 p2) (interval-reciprocal i2)
    (if m2
        (if p2
            ;; if the divisor has a zero crossing the approximation is everything
            (make-interval nil nil)
            (interval* i1 m2))
        (interval* i1 p2))))
