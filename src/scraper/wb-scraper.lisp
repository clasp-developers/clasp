
;;; Make sure you run slime from clasp/src/main


(progn
  (defparameter *clasp-home* #P"/Users/meister/Development/clasp/")
  (setf *default-pathname-defaults* (merge-pathnames "src/scraper/" *clasp-home*))
  (push :testing-scraper *features*)
  (load "scraper.lisp"))

(in-package :cscrape)

(apropos "clasp-home")
(cscrape::do-scraping
    (list
     "/Users/meister/Development/externals-clasp/build/release/bin/clang"
     (namestring (merge-pathnames "src/main/" cl-user::*clasp-home*))
     "bin/all-commands.txt"
     "bin/commands.txt")
  :run-preprocessor nil)


(gethash "llvmo::Type_O" *exposed-classes*)
(defparameter *sorted* (cscrape::sort-classes-by-inheritance *exposed-classes*))
*sorted*
*classes*
(cscrape::inherits-from (car *classes*) (cadr *classes*) *inheritance*)q
*inheritance*
;;; Make sure you run slime from clasp/src/main


(progn
  (defparameter *clasp-home* #P"/Users/meister/Development/clasp/")
  (setf *default-pathname-defaults* (merge-pathnames "src/scraper/" *clasp-home*))
  (push :testing-scraper *features*)
  (load "scraper.lisp"))

(in-package :cscrape)

(apropos "clasp-home")
(cscrape::do-scraping
    (list
     "/Users/meister/Development/externals-clasp/build/release/bin/clang"
     (namestring (merge-pathnames "src/main/" cl-user::*clasp-home*))
     "bin/all-commands.txt"
     "bin/commands.txt")
  :run-preprocessor nil)

(gethash "llvmo::Type_O" *exposed-classes*)
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

