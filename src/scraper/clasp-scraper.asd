(defsystem :clasp-scraper
  :description "A tool to scan C++ files for tags, and then generate binding code based on those tags that expose the C++ definitions to Clasp. It's used to bootstrap Clasp itself."
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
