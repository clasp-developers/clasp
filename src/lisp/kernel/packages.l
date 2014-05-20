
(defvar *available-packages* (Binder))
(defun declare-package (packageName sourcePath)
  (extend *available-packages packageName sourcePath))


(declare-package :db (path-append (scriptPath) "db"))
