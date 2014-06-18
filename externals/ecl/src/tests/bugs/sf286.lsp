;; miscompilation - assumed that read-from-string returns a fixnum.

(deftest sf286-a
         (assert
           (let ((string ":cl-user"))
             (find-package 
               (let ((*package* (find-package :cl)))
                 (read-from-string string))))))

(deftest sf286-b
         (assert
           (let ((string ":cl-user"))
             (let ((*package* (find-package :cl)))
               (find-package 
                 (read-from-string string))))))
