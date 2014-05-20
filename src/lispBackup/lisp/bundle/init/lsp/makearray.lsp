
(defun make-array (dimensions &key element-type initial-element initial-contents adjustable fill-pointer displaced-to displaced-index-offset)
  (and adjustable (error "Add support for adjustable arrays"))
  (and fill-pointer (error "Add support for fill-pointers in arrays"))
  (and displaced-to (error "Add support for displaced-to arrays"))
  (and displaced-index-offset (error "Add support for displaced-index-offset arrays"))
;;  (when element-type (inform "Add support for element-type in make-array\n"))
  (cond
    ((fixnump dimensions)
     (make-vector-objects initial-element initial-contents dimensions))
    ((and (consp dimensions) (eql 1 (length dimensions)))
     (make-vector-objects initial-element initial-contents (car dimensions)))
    ((consp dimensions)
     (and initial-contents (error "You passed initial-contents to make-array"))
     (make-array-objects dimensions element-type initial-element))
    (t (error "Illegal dimensions ~a for make-array" dimensions ))))

(export 'make-array)
