(define-condition restartable-gethash-error (error)
  ((key  :initarg :key :accessor key)
   (hash :initarg :hash :accessor hash))
  (:report (lambda (condition stream)
	     (format stream "RESTARTABLE-GETHASH error getting ~A from ~A."
		     (key condition)
		     (hash condition)))))

(defun read-new-value (what)
  (format t "Enter a new ~A: " what)
  (multiple-value-list (eval (read)))) 

(defun restartable-gethash (key hash &optional default)
  (restart-case
      (multiple-value-bind (value present)
	  (gethash key hash default)
	(if present
	    (values value present)
	    (error 'restartable-gethash-error :key key :hash hash)))
    (continue ()
      :report "Return not having found the value."
      (values default nil))
    (try-again ()
      :report "Try getting the key from the hash again."
      (restartable-gethash key hash))
    (use-new-key (new-key)
      :report "Use a new key."
      :interactive (lambda () (read-new-value "key"))
      (restartable-gethash new-key hash))
    (use-new-hash (new-hash)
      :report "Use a new hash."
      :interactive (lambda () (read-new-value "hash"))
      (restartable-gethash key new-hash))))

(defun list-hash (&rest list)
  (let ((hash (make-hash-table)))
    (loop for (key val) on list by #'cddr
       do (setf (gethash key hash) val))
    hash))

