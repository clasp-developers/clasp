(in-package "SYSTEM")
(defun make-array (dimensions &key (element-type t)
                                initial-element initial-contents adjustable fill-pointer displaced-to displaced-index-offset)
  (and displaced-to (error "Add support for displaced-to arrays"))
  (and displaced-index-offset (error "Add support for displaced-index-offset arrays"))
;;  (when element-type (inform "Add support for element-type in make-array\n"))
  (cond
    ((or (fixnump dimensions) (and (consp dimensions) (eql 1 (length dimensions))))
     (let ((dim (if (fixnump dimensions)
		    dimensions
		    (car dimensions))))
       (make-vector element-type dim adjustable fill-pointer displaced-to displaced-index-offset initial-element initial-contents)))
    ((consp dimensions)
     (and initial-contents (error "You passed initial-contents to make-array"))
     (make-array-objects dimensions element-type initial-element adjustable))
    (t (error "Illegal dimensions ~a for make-array" dimensions ))))


(defun adjust-array (array dimensions &key element-type initial-element initial-contents fill-pointer displaced-to displaced-index-offset)
  (and fill-pointer (error "Add support for fill-pointers in arrays"))
  (and displaced-to (error "Add support for displaced-to arrays"))
  (and displaced-index-offset (error "Add support for displaced-index-offset arrays"))
;;  (when element-type (inform "Add support for element-type in make-array\n"))
  (cond
    ((vectorp array)
     (let ((dim (cond
		  ((fixnump dimensions) dimensions)
		  ((and (consp dimensions) (eql 1 (length dimensions))) (car dimensions))
		  (t (error "illegal dimensions for adjust-array: ~A" dimensions)))))
       (adjust-vector array dim initial-element initial-contents)))
    ((consp dimensions)
     (and initial-contents (error "Handle initial-contents to adjust-array"))
     (adjust-array-objects array dimensions element-type initial-element))
    (t (error "Illegal dimensions ~a for adjust-array" dimensions ))))
