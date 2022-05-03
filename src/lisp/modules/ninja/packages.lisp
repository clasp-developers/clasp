(defpackage #:ninja
  (:use #:common-lisp)
  (:documentation "Ninja writing interface along with utility streams for build files.")
  (:export #:escape
           #:*line-end*
           #:*line-start*
           #:*line-width*
           #:make-line-wrapping-stream
           #:make-timestamp-preserving-stream
           #:with-timestamp-preserving-stream
           #:write-bindings
           #:write-build
           #:write-comment
           #:write-default
           #:write-include
           #:write-pool
           #:write-rule))

