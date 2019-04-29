(in-package #:static-gfs)

(defstruct (cell
            (:type vector) ; for annoying bootstrap reasons
            (:constructor make-cell (name keys function)))
  name keys function)

(defmacro ensure-gethash (key table &optional default)
  (let ((valueg (gensym "VALUE"))
        (presentpg (gensym "PRESENTP"))
        (keyo (gensym "KEY"))
        (tableo (gensym "TABLE")))
    `(let ((,keyo ,key) (,tableo ,table))
       (multiple-value-bind (,valueg ,presentpg)
           (gethash ,keyo ,tableo)
         (if ,presentpg
             ,valueg
             (setf (gethash ,keyo ,tableo) ,default))))))

(defmacro ensure-cell (name keys table default)
  `(ensure-gethash ,keys
                   (ensure-gethash ,name ,table
                                   (make-hash-table :test #'equal))
                   ,default))
