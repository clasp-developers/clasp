
(progn
  (defstruct member
    class
    enum
    )

  (defstruct species
    name
    discriminator ;; Function - takes one argument, returns a species index
    index
    (members (make-hash-table :test #'eq))
    )
  
  (defstruct gckind-manager
    (species nil) ;; a single argument lambda that returns an integer index
    default-species 
    name-to-kind
    )


  (defun add-species (manager species &key default-species)
    (let ((number-of-species (length (gckind-manager-species manager))))
      (setf number-of-species (species-index species))
      (if default-species
          (setf (gckind-manager-default-species manager) species)
          (push species (gckind-manager-species manager)))
      ))


  (defun identify-species (manager aclass)
    (let (hits)
      (dolist (species (gckind-manager-species manager))
        (when (funcall (species-discriminator species) aclass)
          (push species hits)))
      (cond
        ((> (length hits) 1)
         (error "The class ~a could not be distinguished between the species: ~a" aclass hits))
        ((eql (length hits) 1)
         (car hits))
        (t (gckind-manager-default-species manager)))
      ))
  )



(defun analyze-gcobject (gco anal)
  (let ((manager (analysis-gckind-manager anal))
        (species (identify-species manager gco)))
    (setf (gethash gco (analysis-gcobjects-to-species anal)) species)
    (push gco (gcorganizer-species-to-objects anal)))
          (ash species )
  

(defun organize-all-classes (analysis &aux (project (analysis-project analysis)))
  (let ((project (analysis-project analysis))
        (manager (analysis-manager analysis)))
    (maphash (lambda (k v) (analyze-gcobject v analysis)) (project-gcobjects project))
    (maphash (lambda (k v) (analyze-gcobject v analysis)) (project-gccontainers project))
    ))
  





                           
