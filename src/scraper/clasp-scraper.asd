(defsystem :clasp-scraper
  :description "A tool used by the clasp build process to scrape .cc files."
  :depends-on (:alexandria :esrap)
  :serial t
  :components
  ((:file "packages")
   (:file "foundation")
   (:file "parse")
   (:file "tags")
   (:file "serialize")
   (:file "extract-tags")
   (:file "sif-file")
   (:file "conditions")
   (:file "sourcepos")
   (:file "interpret-tags")
   (:file "format")
   (:file "csubst")
   (:file "code-generator")
   (:file "scraper")))
