(require 'asdf)
(let ((*default-pathname-defaults* (make-pathname :name nil
                                                  :type nil
                                                  :defaults *load-truename*)))
  (push (merge-pathnames #p"foo-1/") asdf:*central-registry*)
  (asdf:load-system "foo")
  (push (merge-pathnames #p"foo-2/") asdf:*central-registry*)
  (asdf:load-system "foo"))
