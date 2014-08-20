
(defparameter *ht* (make-hash-table :test 'eq))


(defun fill-ht (ht num)
  (let ((c 0))
    (tagbody
     top
       (let ((sym (intern (bformat nil "sym%s" c))))
         (hash-table-setf-gethash ht sym c)
         (bformat t "Adding %s %s\n" sym c))
       (setq c (+ c 1))
       (if (< c num)
           (go top)))))


(defun dump-ht (ht)
  (bformat t "Dumping ht\n")
  (maphash #'(lambda (k v) (bformat t "%s %s\n" k v)) ht))


(bformat t "Before fill %s\n" *ht*)
(fill-ht *ht* 100)
(bformat t "After fill %s\n" *ht*)
(gctools:garbage-collect)
(gctools:garbage-collect)
(gctools:garbage-collect)
(gctools:garbage-collect)
(gctools:garbage-collect)
(bformat t "After several garbage collects %s\n" *ht*)
(dump-ht *ht*)
