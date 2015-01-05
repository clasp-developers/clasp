
(format t "If things don't work it may be stale versions of compiled files - remember to erase the files in ~~/.slime/fasl~%")

(require :asdf)
(require :swank)
(setq swank:*communication-style* nil)

(defun start-swank ()
  (swank:create-server :port 4005 :dont-close nil))

(start-swank)
