;; Run sbcl with sbcl --dynamic-space-size 4096

;;; Make sure you run slime from clasp/src/main


(require :asdf)
(progn
  (defparameter *clasp-home* #P"/Users/meister/Development/dev-clasp/")
  (asdf:initialize-source-registry `(:source-registry (:directory ,(merge-pathnames "src/scraper/" *clasp-home*)) :ignore-inherited-configuration))
  (load (merge-pathnames "src/scraper/dependencies/bundle.lisp" *clasp-home*))
  (asdf:load-system :clasp-scraper)
  (swank:set-default-directory (merge-pathnames "build/boehm/" *clasp-home*)))
(in-package :cscrape)

(progn
  (sb-ext:gc :full t)
  (sb-sprof:with-profiling ()
    (time (cscrape:generate-sif-files '("/usr/lib/llvm-5.0/bin/clang++" "-E" "-DSCRAPING" "-I./" "-I/usr/lib/llvm-5.0/include" "-fno-omit-frame-pointer"
                                        "-mno-omit-leaf-frame-pointer" "-std=c++11" "-flto=thin" "-Wno-macro-redefined" "-Wno-deprecated-register"
                                        "-Wno-expansion-to-defined" "-Wno-return-type-c-linkage" "-Wno-invalid-offsetof" "-Wno-#pragma-messages"
                                        "-Wno-inconsistent-missing-override" "-O3" "-g" "-I." "-I../.." "-I../../src/main" "-I../../include"
                                        "-Igenerated" "-I/usr/lib/llvm-5.0/include" "-I/usr/include")
                                      "/tmp/llvmoExpose.cc" "/tmp/llvmoExpose.sif"))))

(apropos "parse-lambda-list")


(parse-lambda-list-from-signature "APFloat_sp APFloat_O::makeAPFloatFloat(core::SingleFloat_sp value)")

(separate-type-pointer "&llvm::IRBuilderBase::SetInsertPoint")
(separate-type-pointer "(void (llvm::IRBuilderBase::*)(llvm::BasicBlock *))&llvm::IRBuilderBase::SetInsertPoint")


(progn
  (defun all-subdirectories (topdir)
    (cons topdir
          (loop for dir in (uiop:subdirectories topdir)
                append (all-subdirectories dir))))
  (defun all-sifs (topdir)
    (loop for subdir in (all-subdirectories topdir)
          append (remove-if-not (lambda (it)
                                  (search ".sif" (namestring it)))
                                (uiop:directory-files subdir)))))

(in-package :cscrape)


(cscrape::process-all-sif-files "/Users/meister/Development/dev-clasp/"
                                "/Users/meister/Development/dev-clasp/build/mps/"
                                (all-sifs "/Users/meister/Development/dev-clasp/build/mps/"))


(all-sifs "/Users/meister/Development/dev-clasp/build/mps/")



(progn
  (setq *default-pathname-defaults* #P"/Users/meister/Development/clasp/")
  (let* ((build-path (merge-pathnames #P"wbuild/boehmdc_o/"))
         (clasp-home-path *default-pathname-defaults*)
         (main-path #P"src/main/")
         (main-path (merge-pathnames main-path build-path))
         (sif-files *sif-files*))
    (process-all-sif-files clasp-home-path main-path sif-files)))

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



(gethash :lisp-wrappers (cscrape::read-application-config cscrape::*application-config*))
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

