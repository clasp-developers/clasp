(in-package :cscrape)

(setf *default-pathname-defaults* (pathname "/Users/meister/Development/clasp/src/scraper/"))
(push :testing-scraper *features*)
(load "scraper.lisp")

(do-scraping
    '("/Users/meister/Development/externals-clasp/build/release/bin/clang"
      "/Users/meister/Development/clasp/src/main/"
      "bin/all-commands.txt"
      "bin/commands.txt")
  :run-preprocessor t)

;;; ----------------------------------------------------------------------
;;;
;;; Profile the scraper
;;;
(require :sb-sprof)
(sb-sprof:with-profiling (:max-samples 10000
                                       :mode :alloc
                                       :report :flat)
  (do-scraping '("/Users/meister/Development/externals-clasp/build/release/bin/clang" "/Users/meister/Development/clasp/src/main/" "/Users/meister/Development/clasp/src/main/include/application.config" "/tmp/all-commands.txt" "/tmp/commands.txt") :run-preprocessor nil))

