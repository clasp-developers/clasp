(in-package :cscrape)

(do-scraping '("/Users/meister/Development/externals-clasp/build/release/bin/clang" "/Users/meister/Development/clasp/src/main/" "/Users/meister/Development/clasp/src/main/include/application.config" "/tmp/all-commands.txt" "/tmp/commands.txt") :run-preprocessor nil)

(print "Hello")

(length *tags*)
(mapc (lambda (ta)
        (when (typep ta 'tags:symbol-tag)
          (print ta)))
      *tags*)

(mapc (lambda (s)
        (print (tags:name% s)))
      (gethash "KeywordPkg" *symbols-by-package*))

(gethash "ClPkg" *symbols-by-package*)


(maphash (lambda (k v)
           (print k))
         *unique-symbols*)
(defparameter *sbp* (make-hash-table :test #'equal))
(maphash (lambda (key tag)
           (format t "key: ~s   value: ~a~%" key tag)
           (when (typep tag 'tags:symbol-tag)
             (format t "Pushing ~a to (gethash ~a *sbp*)~%" (tags:name% tag) (tags:package% tag))
             (push tag (gethash (tags:package% tag) *sbp*))))
         *unique-symbols*)

(gethash (

(print "Done")



(
(untrace)


(defparameter *ht* (make-hash-table))
(push 'c (gethash 'a *ht*))
*ht*


