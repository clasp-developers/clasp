;;;; This file contains macro-like source transformations which
;;;; convert uses of certain functions into the canonical form desired
;;;; within the compiler. FIXME: and other IR1 transforms and stuff.

(in-package :core)

;; proper-list-p code from Robert Strandh's Cleavir code
(defun proper-list-p (object)
  (cond  ((null object) t)
         ((atom object) nil)
         (t (let ((slow object)
                  (fast (cdr object)))
              (declare (type cons slow))
              (tagbody
               again
                 (unless (consp fast)
                   (return-from proper-list-p
                     (if (null fast) t nil)))
                 (when (eq fast slow)
                   (return-from proper-list-p nil))
                 (setq fast (cdr fast))
                 (unless (consp fast)
                   (return-from proper-list-p
                     (if (null fast) t nil)))
                 (setq fast (cdr fast))
                 (setq slow (cdr slow))
                 (go again))))))

(declaim (inline singleton-p))
(defun singleton-p (list)
  (and (listp list) (null (rest list)) list))

;;;; In Clasp source-transforms are implemented as compiler macros
(defmacro define-source-transform (name lambda-list &body body)
  (multiple-value-bind (func pprint doc-string)
      (sys::expand-defmacro name lambda-list body 'cl:define-compiler-macro)
    `(setf (compiler-macro-function ',name)
           (lambda (whole env)
             (declare (core:lambda-name
                       ,(intern (format nil "SOURCE-TRANSFORM-~a" name) :core)))
             (or (proper-list-p whole)
               (error "Arguments for ~a must be a proper list" name))
             (multiple-value-bind (expansion done)
                 (funcall ,func whole env)
               (if (or expansion (not done))
                   expansion
                   whole))))))

(defmacro /show0 (&rest args) nil)

;;;; list hackery

;;; Translate CxR into CAR/CDR combos.
(defun source-transform-cxr (form env)
  (declare (ignore env))
  (if (not (singleton-p (cdr form)))
      (values nil t)
      (let* ((name (car form))
             (string (symbol-name
                      (etypecase name
                        (symbol name)
                        (leaf (leaf-source-name name))))))
        (do ((i (- (length string) 2) (1- i))
             (res (cadr form)
                  `(,(ecase (char string i)
                       (#\A 'car)
                       (#\D 'cdr))
                    ,res)))
            ((zerop i) res)))))

;;; Make source transforms to turn CxR forms into combinations of CAR
;;; and CDR. ANSI specifies that everything up to 4 A/D operations is
;;; defined.
;;; Don't transform CAD*R, they are treated specially for &more args
;;; optimizations

(/show0 "about to set CxR source transforms")
(loop for i of-type index from 2 upto 4 do
   ;; Iterate over BUF = all names CxR where x = an I-element
   ;; string of #\A or #\D characters.
     (let ((buf (make-string (+ 2 i))))
       (setf (aref buf 0) #\C
             (aref buf (1+ i)) #\R)
       (dotimes (j (ash 2 i))
         (declare (type index j))
         (dotimes (k i)
           (declare (type index k))
           (setf (aref buf (1+ k))
                 (if (logbitp k j) #\A #\D)))
         (unless (member buf '("CADR" "CADDR" "CADDDR")
                         :test #'equal)
           (setf (compiler-macro-function (intern buf)) #'source-transform-cxr)
           #+(or)(setf (info :function :source-transform (intern buf))
                       #'source-transform-cxr)
           ))))
(/show0 "done setting CxR source transforms")


;;; Turn FIRST..FOURTH and REST into the obvious synonym, assuming
;;; whatever is right for them is right for us. FIFTH..TENTH turn into
;;; Nth, which can be expanded into a CAR/CDR later on if policy
;;; favors it.
(define-source-transform rest (x) `(cdr ,x))
(define-source-transform first (x) `(car ,x))
(define-source-transform second (x) `(cadr ,x))
(define-source-transform third (x) `(caddr ,x))
(define-source-transform fourth (x) `(cadddr ,x))
(define-source-transform fifth (x) `(nth 4 ,x))
(define-source-transform sixth (x) `(nth 5 ,x))
(define-source-transform seventh (x) `(nth 6 ,x))
(define-source-transform eighth (x) `(nth 7 ,x))
(define-source-transform ninth (x) `(nth 8 ,x))
(define-source-transform tenth (x) `(nth 9 ,x))

;;; LIST with one arg is an extremely common operation (at least inside
;;; SBCL itself); translate it to CONS to take advantage of common
;;; allocation routines.
(define-source-transform list (&rest args)
  (case (length args)
    (1 `(cons ,(first args) nil))
    (t (values nil t))))

;;; And similarly for LIST*.
(define-source-transform list* (arg &rest others)
  (cond ((not others) arg)
        ((not (cdr others)) `(cons ,arg ,(car others)))
        (t (values nil t))))


(define-source-transform nconc (&rest args)
  (case (length args)
    (0 ())
    (1 (car args))
    (t (values nil t))))







;;;; converting N-arg arithmetic functions
;;;;
;;;; N-arg arithmetic and logic functions are associated into two-arg
;;;; versions, and degenerate cases are flushed.

;;; Left-associate FIRST-ARG and MORE-ARGS using FUNCTION.
(declaim (ftype (sfunction (symbol t list t) list) associate-args))
(defun associate-args (fun first-arg more-args)
  (assert more-args)
  (let ((next (rest more-args))
        (arg (first more-args)))
    (if (null next)
        `(,fun ,first-arg ,arg)
        (associate-args fun `(,fun ,first-arg ,arg) next))))

;;; Reduce constants in ARGS list.
(declaim (ftype (sfunction (symbol list symbol) list) reduce-constants))
(defun reduce-constants (fun args one-arg-result-type)
  (let ((one-arg-constant-p (ecase one-arg-result-type
                              (number #'numberp)
                              (integer #'integerp)))
        (reduced-value)
        (reduced-p nil))
    (core::collect ((not-constants))
      (dolist (arg args)
        (let ((value (if (constantp arg)
                         arg ;;(constant-form-value arg)
                         arg)))
          (cond ((not (funcall one-arg-constant-p value))
                 (not-constants arg))
                (reduced-value
                 (setf reduced-value (funcall fun reduced-value value)
                       reduced-p t))
                (t
                 (setf reduced-value value)))))
      ;; It is tempting to drop constants reduced to identity here,
      ;; but if X is SNaN in (* X 1), we cannot drop the 1.
      (if (not-constants)
          (if reduced-p
              `(,reduced-value ,@(not-constants))
              args)
          `(,reduced-value)))))

;;; Do source transformations for transitive functions such as +.
;;; One-arg cases are replaced with the arg and zero arg cases with
;;; the identity. ONE-ARG-RESULT-TYPE is the type to ensure (with THE)
;;; that the argument in one-argument calls is.
(declaim (ftype (function (symbol list t &optional symbol list)
                          (values t &optional (member nil t)))
                source-transform-transitive))
(defun source-transform-transitive (fun args identity
                                    &optional (one-arg-result-type 'number)
                                      (one-arg-prefixes '(values)))
  (let ((two-arg-fun (intern (format nil "TWO-ARG-~a" fun) :core)))
    (case (length args)
      (0 identity)
      (1 `(,@one-arg-prefixes (the ,one-arg-result-type ,(first args))))
      (2 (values `(,two-arg-fun ,@args) t))
      (t
       (let* ((reduced-args (reduce-constants fun args one-arg-result-type))
              (first (first reduced-args))
              (rest (rest reduced-args)))
         (if rest
             (associate-args two-arg-fun first rest)
             first))))))


(define-source-transform + (&rest args)
  (source-transform-transitive '+ args 0))
(define-source-transform * (&rest args)
  (source-transform-transitive '* args 1))
(define-source-transform logior (&rest args)
  (source-transform-transitive 'logior args 0 'integer))
(define-source-transform logxor (&rest args)
  (source-transform-transitive 'logxor args 0 'integer))
(define-source-transform logand (&rest args)
  (source-transform-transitive 'logand args -1 'integer))
(define-source-transform logeqv (&rest args)
  (source-transform-transitive 'logeqv args -1 'integer))
(define-source-transform gcd (&rest args)
  (source-transform-transitive 'gcd args 0 'integer '(abs)))
(define-source-transform lcm (&rest args)
  (source-transform-transitive 'lcm args 1 'integer '(abs)))

(compiler-macroexpand '(gcd  x 2 10 4 8))
