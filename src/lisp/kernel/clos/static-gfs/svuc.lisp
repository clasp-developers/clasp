(in-package #:static-gfs)

#|
Optimize (setf) slot-value-using-class for the case of both the class and
slotd being constant. Also slot-boundp-using-class.
A user will probably never write code like that, but it comes up both in
shared-initialize.lisp and fastgf.
Actually slot-value-using-class doesn't come up in shared-initialize.
TODO 1: Use this in fastgf.
TODO 2: Build a cell system like we do for make-instance.
 This would allow instance access (both in fastgf if used, and here) for
 nonstandard classes to be as fast as standard instance access if there
 are no user methods, or if there are, almost as fast.
|#

;;;; NOTE: The static-foo macros may evaluate the instance form more than once.
;;;; (Because we know here that it's always just a variable.)

(defun uncustomizable-slot-p (class slotd)
  (let ((metaclass (class-of class)))
    (and (or (eq metaclass #.(find-class 'standard-class))
             (eq metaclass #.(find-class 'clos:funcallable-standard-class)))
         (eq (class-of slotd) #.(find-class 'clos:standard-effective-slot-definition)))))

;;; I said "constant slotd", but slotds aren't dumpable, so to satisfy
;;; COMPILE-FILE what we'll actually get is (ltv-slotd class slotd),
;;; which expands into a load-time-value.
(defun ltv-slotd-form-p (slotdf)
  (and (consp slotdf)
       (eq (car slotdf) 'ltv-slotd)
       (consp (cdr slotdf))
       (consp (cddr slotdf))
       (null (cdddr slotdf))))

(defun extract-slotd (slotdf)
  (assert (ltv-slotd-form-p slotdf))
  (third slotdf))

;;; FIXME: Signal a real error (it should never happen though)
(defun slotd-form (class slotd)
  (let ((name (clos:slot-definition-name slotd)))
    `(or (find ',name (clos:class-slots ,class) :key #'clos:slot-definition-name)
         (error "BUG: Slotd disappeared"))))

;;; This exists because I'd like to make these forms acceptable to COMPILE-FILE.
;;; For regular COMPILE it should be redundant but harmless.
(defmacro ltv-slotd (class slotd)
  `(load-time-value ,(slotd-form class slotd)))

;;; Shared by slot-value and slot-boundp
(defun gen-fast-value-read (class instancef slotd)
  (let ((allocation (clos:slot-definition-allocation slotd)))
    (ecase allocation
      ((:instance)
       `(si:instance-ref ,instancef ,(clos:slot-definition-location slotd)))
      ((:class)
       `(car (load-time-value
              (clos:slot-definition-location ,(slotd-form class slotd))))))))

(defun gen-fast-svuc (class instancef slotd)
  (let ((instanceg (gensym "INSTANCE")))
    `(let ((,instanceg ,instancef)
           (sv ,(gen-fast-value-read class instanceg slotd)))
       (if (eq sv (load-time-value (core:unbound) t))
           (slot-unbound ,class ,instanceg ',(clos:slot-definition-name slotd))
           sv))))

(defmacro static-slot-value-using-class (class instance slotdf)
  (let ((slotd (extract-slotd slotdf)))
    (if (uncustomizable-slot-p class slotd)
        (gen-fast-svuc class instance slotd)
        `(clos:slot-value-using-class ,class ,instance ,slotdf))))

(defun gen-fast-sbuc (class instancef slotd)
  `(not (eq ,(gen-fast-value-read class instancef slotd)
            (load-time-value (core:unbound) t))))

(defmacro static-slot-boundp-using-class (class instance slotdf)
  (let ((slotd (extract-slotd slotdf)))
    (if (uncustomizable-slot-p class slotd)
        (gen-fast-sbuc class instance slotd)
        `(clos:slot-boundp-using-class ,class ,instance ,slotdf))))

;;; could be used by slot-makunbound-using-class, i guess.
(defun gen-fast-value-write (value class instancef slotd)
  (let ((allocation (clos:slot-definition-allocation slotd)))
    (ecase allocation
      ((:instance)
       `(si:instance-set ,instancef ,(clos:slot-definition-location slotd) ,value))
      ((:class)
       `(rplaca (load-time-value
                 (clos:slot-definition-location ,(slotd-form class slotd)))
                ,value)))))

(defun gen-fast-setf-svuc (value class instancef slotd)
  (gen-fast-value-write value class instancef slotd))

;;; can't use defsetf since it'll bind temps for the constants.
(define-setf-expander static-slot-value-using-class
    (&environment env class instancef slotdf)
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion instancef env)
    (let ((store (gensym "STORE")))
      (values vars vals (list store)
              (let ((slotd (extract-slotd slotdf)))
                (if (uncustomizable-slot-p class slotd)
                    `(progn ,(gen-fast-value-write store class access-form slotd)
                            ,store)
                    `(funcall #'(setf clos:slot-value-using-class)
                              ,store ,class ,access-form ,slotdf)))
              `(clos:slot-value-using-class ,class ,access-form ,slotdf)))))
