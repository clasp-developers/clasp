;;
;; Some assorted functions provided by Bike from #lisp
;;
;; Some of these he wrote and others were extracted from public domain source in SBCL
;;
;;

(in-package #:sys)


(declaim (ftype (function (list) (or (integer 0) null)) list-length))
(defun list-length (list)
  "Return the length of the given list, or NIL if the list is circular."
  ;; from sbcl (public domain)
  (do ((n 0 (+ n 2))
       (y list (cddr y))
       (z list (cdr z)))
      (nil)
    (declare (type fixnum n) ; fix if list lengths can be more than fixnums
	     (type list y z))
    (when (endp y) (return n))
    (when (endp (cdr y)) (return (1+ n)))
    (when (and (eq y z) (plusp n)) (return nil))))

(declaim (ftype (function (list list &optional list) list) pairlis))
(defun pairlis (keys data &optional alist)
  "Create an alist with keys KEYS paired with data DATA, appended to an existing ALIST."
  ;; bit inefficient but this is low-use
  (append (mapcar #'cons keys data) alist))

;; if not already defined
(deftype function-name () '(or symbol (cons (eql setf) (cons symbol null))))

(declaim (ftype (function ((or null pathname string function-name)) null) ed))
(defun ed (&optional x)
  "CLASP has no standard editor; this function is no-op."
  ;; "The consequences are undefined if the implementation
  ;;  does not provide a resident editor."
  (declare (ignore x))
  nil)

(declaim (ftype (function (list) list) copy-alist))
(defun copy-alist (alist)
  "Copy the list structure and conses of an alist."
  (loop for (a . b) in alist collecting (cons a b)))

#+(or) ;; numlib provides this
(progn
  (declaim (ftype (function (real) complex) cis))
  (defun cis (radians)
    "The common cos+i*sin operation."
    (complex (cos radians) (sin radians))))

;; may already be implemented
;; note that function designators do not include (setf ...) lists like names do
(declaim (inline coerce-fdesignator)
	 (ftype (function ((or function symbol)) function) fdesignator))
(defun coerce-fdesignator (fdesignator)
  "Take a CL function designator and spit out a function."
  (etypecase fdesignator
    (function fdesignator)
    (symbol (fdefinition fdesignator))))

;; partial evaluation from sbcl

(declaim (ftype (function (list list function) (or null (eql t)))
		tree-equal-TEST tree-equal-TEST-NOT))
(declaim (ftype (function (list list &key t t) (or null (eql t)))
		tree-equal))
(defun tree-equal-TEST (tree-1 tree-2 test)
  (cond ((consp tree-1)
	 (and (consp y)
	      (tree-equal-TEST (car tree-1) (car tree-2) test)
	      (tree-equal-TEST (cdr tree-1) (cdr tree-2) test)))
	((consp y) nil)
	((funcall test x y) t)
	(t nil)))

(defun tree-equal-TEST-NOT (tree-1 tree-2 test-not)
  (cond ((consp tree-1)
	 (and (consp y)
	      (tree-equal-TEST-NOT (car tree-1) (car tree-2) test-not)
	      (tree-equal-TEST-NOT (cdr tree-1) (cdr tree-2) test-not)))
	((consp y) nil)
	((not (funcall test-not x y) t))
	(t nil)))

(defun tree-equal (tree-1 tree-2 &key (test #'eql testp) (test-not nil notp))
  "Return T if X and Y are isomorphic trees with leaves equal under TEST/TEST-NOT."
  (when (and testp notp)
    (error ":TEST and :TEST-NOT both supplied to TREE-EQUAL."))
  (if test-not
      (tree-equal-TEST-NOT tree-1 tree-2 (coerce-fdesignator test-not))
      (tree-equal-TEST tree-1 tree-2 (coerce-fdesignator test))))

(declaim (ftype (function (list list) (values t t list)) get-properties))
(defun get-properties (place indicator-list)
  "Return the first key/value pair in PLACE where the key is eq to something in INDICATOR-LIST, as well as the plist tail at that point."
  (loop for plist on place by #'cddr
     when (atom (cdr plist))
     do (error 'simple-type-error
	       :format-control "Malformed property list; ~s."
	       :format-arguments (list place)
	       :datum (cdr plist)
	       :expected-type 'cons)
     when (member (car plist) indicator-list :test #'eq)
     return (values (first plist) (second plist) plist)
     finally (return (values nil nil nil))))

;; nobody uses these but this is conforming
(declaim (ftype (function () null) short-site-name long-site-name))
(defun short-site-name () nil)
(defun long-site-name () nil)


(defun digit-char (weight &optional radix)
  (if (>= weight radix)
      nil
      (code-char (+ weight (if (< weight 10) 48 55)))))


;; Donated by Shinmera in #clasp on April 2015 "free of charge"
(in-package :cl)
(defun string-capitalize (string)
  (with-output-to-string (stream)
    (loop with capitalize = T
          for char across (string string)
          do (cond ((alphanumericp char)
                    (cond (capitalize
                           (setf capitalize NIL)
                           (write-char (char-upcase char) stream))
                          (T
                           (write-char (char-downcase char) stream))))
                   (T
                    (setf capitalize T)
                    (write-char char stream))))))


(defun float-radix (arg)
  ;; Unless you are internally representing
  ;; floats in anything but base-2 this will do
  (etypecase arg
    (float 2)))

(defun remprop (symbol indicator)
  (remf (symbol-plist symbol) indicator))

(defun logcount (integer)
  ;; There's probably some C++ way to make this
  ;; much more efficient, but this should suffice.
  (let ((counting (if (plusp integer) 1 0)))
    (loop for i from 0 below (integer-length integer)
          count (= counting (ldb (byte 1 i) integer)))))

(defun logbitp (index integer)
  (ldb-test (byte 1 index) integer))
