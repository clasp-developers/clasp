(in-package "SYSTEM")
(defun make-array (dimensions &key (element-type t)
                                initial-element 
                                initial-contents 
                                adjustable 
                                fill-pointer 
                                displaced-to
                                (displaced-index-offset 0))
  ;;  (when element-type (inform "Add support for element-type in make-array\n"))
  (if (and (consp element-type)
	   (null initial-element)
	   (eq (car element-type) 'cl:unsigned-byte))
      (setq initial-element 0))
  (cond
    ((null dimensions)
     (make-array-objects dimensions (upgraded-array-element-type element-type) initial-element adjustable))
    ((or (fixnump dimensions) (and (consp dimensions) (eql 1 (length dimensions))))
     (let ((dim (if (fixnump dimensions)
		    dimensions
		    (car dimensions))))
       (if displaced-to
           (progn
             (and adjustable (error "Displaced arrays don't support adjustable"))
             (and fill-pointer (error "Displaced arrays don't support fill-pointer yet"))
             (if (eq (array-element-type displaced-to) element-type)
                 (make-vector-displaced dimensions element-type displaced-to displaced-index-offset)
                 (error "Cannot displace the array, because the element types don't match")))
           (make-vector (upgraded-array-element-type element-type) dim adjustable fill-pointer displaced-to displaced-index-offset initial-element initial-contents))))
    ((consp dimensions)
     (and initial-contents (error "You passed initial-contents to make-array"))
     (if displaced-to
         (if (eq (array-element-type displaced-to) element-type)
             (make-array-displaced dimensions element-type displaced-to displaced-index-offset)
             (error "Cannot displace the array, because the element types don't match"))
         (make-array-objects dimensions (upgraded-array-element-type element-type) initial-element adjustable)))
    (t (error "Illegal dimensions ~a for make-array" dimensions ))))


(defun adjust-array (array dimensions &key element-type initial-element initial-contents fill-pointer displaced-to displaced-index-offset)
  (and fill-pointer (error "Add support for fill-pointers in arrays"))
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
