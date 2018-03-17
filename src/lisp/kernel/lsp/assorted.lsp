;;
;; Some assorted functions provided by Bike from #lisp
;;
;; Some of these he wrote and others were extracted from public domain source in SBCL
;;
;;

(in-package :sys)

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
;;; Copied from SBCL
(defun pairlis (keys data &optional (alist '()))
  "Construct an association list from KEYS and DATA (adding to ALIST)."
  (do ((x keys (cdr x))
       (y data (cdr y)))
      ((and (endp x) (endp y)) alist)
    (if (or (endp x) (endp y))
        (error "The lists of keys and data are of unequal length."))
        (setq alist (acons (car x) (car y) alist))))

;; if not already defined
(deftype function-name () '(or symbol (cons (eql setf) (cons symbol null))))

(declaim (ftype (function ((or null pathname string function-name)) null) ed))
(defun ed (&optional x)
  "CLASP has no standard editor; this function is no-op."
  ;; "The consequences are undefined if the implementation
  ;;  does not provide a resident editor."
  (declare (ignore x))
  nil)

;;; Copied from SBCL
(declaim (ftype (function (list) list) copy-alist))
(defun copy-alist (alist)
  "Copy the list structure and conses of an alist."
  (if (endp alist)
      alist
      (let ((result
              (cons (if (atom (car alist))
                        (car alist)
                        (cons (caar alist) (cdar alist)))
                    nil)))
        (do ((x (cdr alist) (cdr x))
             (splice result
                     (cdr (rplacd splice
                                  (cons
                                   (if (atom (car x))
                                       (car x)
                                       (cons (caar x) (cdar x)))
                                   nil)))))
            ((endp x)))
        result)))

#+(or) ;; numlib provides this
(progn
  (declaim (ftype (function (real) complex) cis))
  (defun cis (radians)
    "The common cos+i*sin operation."
    (complex (cos radians) (sin radians))))

;; partial evaluation from sbcl

(declaim (ftype (function (list list function) (or null (eql t)))
		tree-equal-TEST tree-equal-TEST-NOT))
(declaim (ftype (function (list list &key t t) (or null (eql t)))
		tree-equal))
(defun tree-equal-TEST (tree-1 tree-2 test)
  (cond ((consp tree-1)
	 (and (consp tree-2)
	      (tree-equal-TEST (car tree-1) (car tree-2) test)
	      (tree-equal-TEST (cdr tree-1) (cdr tree-2) test)))
	((consp tree-2) nil)
	((funcall test tree-1 tree-2) t)
	(t nil)))

(defun tree-equal-TEST-NOT (tree-1 tree-2 test-not)
  (cond ((consp tree-1)
	 (and (consp tree-2)
	      (tree-equal-TEST-NOT (car tree-1) (car tree-2) test-not)
	      (tree-equal-TEST-NOT (cdr tree-1) (cdr tree-2) test-not)))
	((consp tree-2) nil)
	((not (funcall test-not tree-1 tree-2)))
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

(defun digit-char (weight &optional (radix 10))
  (check-type radix (integer 2 36))
  (when (< weight radix)
    (code-char (+ weight (if (< weight 10) 48 55)))))

;; Donated by Shinmera in #clasp on April 2015 "free of charge"
(in-package :cl)

(defun nstring-capitalize (string &key (start 0) end)
  (loop with capitalize = t
        for i from start below (or end (length string))
        for char = (char string i)
        do (cond ((not (alphanumericp char))
                  (setf capitalize t))
                 (capitalize
                  (setf capitalize nil)
                  (setf (char string i) (char-upcase char)))
                 (t
                  (setf (char string i) (char-downcase char)))))
  string)

(defun string-capitalize (string &key (start 0) end)
  (nstring-capitalize (copy-seq (string string)) :start start :end end))

;;; Begin from knpk 2018-03-17 Fix string-upcase, nstring-upcase, string-downcase, nstring-downcase
(defun %real-string (string-designator)
  (etypecase string-designator
    (string (copy-seq string-designator))
    (symbol
     (copy-seq (symbol-name string-designator)))
    (character
     (make-string 1 :initial-element string-designator))))

(defun %verify-string-args (start end real-string)
  (when (null end)
    (setq end (length real-string)))
  (unless (and (numberp start)
               (numberp end)
               (<= start end)
               (>= start 0)
               (<= end (length real-string)))
    (error "Bad parameters Start ~s End ~s for ~s" start end real-string))
  (values start end))

(defun %transform-string (real-string new-start new-end predicate transformation)
  (dotimes (x (- new-end new-start))
    (let* ((index (+ new-start x))
           (char (char real-string index)))
      (when (funcall predicate char)
        (setf (char real-string index)
              (funcall transformation char)))))
  real-string)

(defun string-upcase (string-designator &key (start 0) end)
  ;;;string-designator to string
  (let ((real-string (%real-string string-designator)))
    (multiple-value-bind
          (new-start new-end)
        (%verify-string-args start end real-string)
      (%transform-string real-string new-start new-end #'lower-case-p #'char-upcase))))

(defun string-downcase (string-designator &key (start 0) end)
  ;;;string-designator to string
  (let ((real-string (%real-string string-designator)))
    (multiple-value-bind
          (new-start new-end)
        (%verify-string-args start end real-string)
      (%transform-string real-string new-start new-end #'upper-case-p #'char-downcase))))

(defun nstring-upcase (real-string &key (start 0) end)
  (unless (stringp real-string)
    (error "Incorrect arguments in nstring-upcase ~s Start ~s End ~s" real-string start end))
  (multiple-value-bind
        (new-start new-end)
      (%verify-string-args start end real-string)
    (%transform-string real-string new-start new-end #'lower-case-p #'char-upcase)))

(defun nstring-downcase (real-string &key (start 0) end)
  (unless (stringp real-string)
    (error "Incorrect arguments in nstring-downcase ~s Start ~s End ~s" real-string start end))
  (multiple-value-bind
        (new-start new-end)
      (%verify-string-args start end real-string)
    (%transform-string real-string new-start new-end #'upper-case-p #'char-downcase)))
    
;;; End from knpk 2018-03-17 Fix string-upcase, nstring-upcase, string-downcase, nstring-downcase

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
