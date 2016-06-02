;; Run sbcl with sbcl --dynamic-space-size 4096

;;; Make sure you run slime from clasp/src/main


(progn
  (defparameter *clasp-home* #P"/Users/meister/Development/clasp/")
  (setf *default-pathname-defaults* (merge-pathnames "src/scraper/" *clasp-home*))
  (push :testing-scraper *features*)
  (load "scraper.lisp"))

(in-package :cscrape)


(generate-sif-file #P"~/Development/clasp/wbuild/src/core/cons.i" #P"~/Development/clasp/wbuild/src/core/cons.sif")
(progn
  (cscrape::do-scraping
    (list
     "/Users/meister/Development/externals-clasp/build/debug/bin/clang"
     (namestring (merge-pathnames "src/main/" cl-user::*clasp-home*))
     "bin/all-commands.txt"
     "bin/commands.txt")
  :run-preprocessor nil
  :regenerate-sifs t)
  (print "Done"))



(gethash :lisp-wrappers (cscrape::setup-application-config))
(cscrape::split-type-name "const string &name")


(substitute #\_ #\: "a::b")

(defparameter *a* (second cscrape::*functions*))
(print *a*)

(mapc (lambda (x)
        (format t "Function: ~a~%" x)
        (format t "types: ~a~%" (multiple-value-list (cscrape::parse-types-from-signature (cscrape::signature% x)))))
      cscrape::*functions*)

(cscrape::parse-types-from-signature (cscrape::signature% *a*))

(member #\space cscrape::+white-space+)


(position-if
 (lambda (c)
   (format t "Looking at char:~a:~%" c)
   (member c +white-space+))
 "abcd efg" :from-end t)
(split-cpps '(1 2 3 4 5 6 7) 2)
(apropos "wait")
(sb-posix:wait 
*packages-to-create*




(inherits-from* "core::Fixnum_dummy_O" "core::Integer_O" cscrape::*inheritance*)

(print "Done2")

(cscrape:extract-method-name-from-signature "inline T_sp Cons_O::setf_car(Cons_sp c)")
(untrace)
(trace cscrape::maybe-remove-one-prefix-from-start)
(gethash "llvmo::Type_O" cscrape::*classes*)
cscrape::*classes*
cscrape::*symbols*
cscrape::*functions*
(defparameter *sorted* (cscrape::sort-classes-by-inheritance *exposed-classes*))
*sorted*
*classes*
*functions*
(cscrape::inherits-from (car *classes*) (cadr *classes*) *inheritance*)q
*inheritance*
;;; Make sure you run slime from clasp/src/main


(progn
  (defparameter *clasp-home* #P"/Users/meister/Development/clasp/")
  (setf *default-pathname-defaults* (merge-pathnames "src/scraper/" *clasp-home*))
  (push :testing-scraper *features*)
  (load "scraper.lisp"))

(in-package :cscrape)

(cscrape::do-scraping
    (list
     "/Users/meister/Development/externals-clasp/build/release/bin/clang"
     (namestring (merge-pathnames "src/main/" cl-user::*clasp-home*))
     "bin/all-commands.txt"
     "bin/commands.txt")
  :run-preprocessor nil
  :regenerate-sifs t)



(trace tags::maybe-remove-one-prefix-from-start)
(untrace)
(tags::extract-method-name-from-signature "T_sp setf_car(Cons_sp x)")

(gethash "llvmo::Type_O" cscrape::*classes*)
cscrape::*tags*

(gethash "llvmo::Type_O" *exposed-classes*)
(extract-function-name-from-signature "llvm::Function *Module_O::getFunction(core::Str_sp dispatchName)" nil)

(trace tags::extract-method-name-from-signature)
(apropos "extract-method-name")
(mapc (lambda (x) (when (typep x 'tags:expose-method-tag) (print x))) *tags*)



(print "Done")

;;; ----------------------------------------------------------------------
;;;
;;; Profile the scraper
;;;
(require :sb-sprof)
(sb-sprof:with-profiling (:max-samples 10000
                                       :mode :alloc
                                       :report :flat)
  (do-scraping '("/Users/meister/Development/externals-clasp/build/release/bin/clang" "/Users/meister/Development/clasp/src/main/" "/Users/meister/Development/clasp/src/main/include/application.config" "/tmp/all-commands.txt" "/tmp/commands.txt") :run-preprocessor nil))


(print "Hello")

(tags::extract-function-name-from-signature "llvm::Function *Module_O::getFunction(core::Str_sp dispatchName)")

(trace tags::extract-method-name-from-signature)
(apropos "extract-method-name")
(mapc (lambda (x) (when (typep x 'tags:expose-method-tag) (print x))) *tags*)



(print "Done")

;;; ----------------------------------------------------------------------
;;;
;;; Profile the scraper
;;;
(require :sb-sprof)
(sb-sprof:with-profiling (:max-samples 10000
                                       :mode :alloc
                                       :report :flat)
  (do-scraping '("/Users/meister/Development/externals-clasp/build/release/bin/clang" "/Users/meister/Development/clasp/src/main/" "/Users/meister/Development/clasp/src/main/include/application.config" "/tmp/all-commands.txt" "/tmp/commands.txt") :run-preprocessor nil))

