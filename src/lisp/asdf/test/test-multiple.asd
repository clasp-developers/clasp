;;; -*- Lisp -*-
(asdf:defsystem test-multiple
  :components
  ((:file "file3")))

(asdf:defsystem test-multiple-too
  :components
  ((:file "file1")
   (:file "file2" :depends-on ("file1"))
   (:file "file3" :depends-on ("file1" "file2"))))

(asdf:defsystem test-multiple-free
  :depends-on (:test-multiple)
  :components ((:file "file4")))
