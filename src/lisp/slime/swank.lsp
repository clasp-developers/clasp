
(format t "REQUIREing asdf~%")
(require :asdf "sys:kernel;asdf;build;asdf.bundle")

(format t "Pushing kernel;contrib;slime; to asdf:*central-registry*~%")
(push "sys:kernel;contrib;slime;" asdf:*central-registry*)
(format t "Loading swank system~%")
(asdf:load-system :swank)

(defun start-swank ()
  (swank:create-server :port 4005 :dont-close nil))

