(defun setup-loop (loop-commands)
  (mapc #'(lambda (x)
	    (print (list " mapc---> x= " x))
	    (inner-mapc (car x))) loop-commands))

(defparameter *loop-commands*
  '( ((element elements) elt length sequence)
    ;;The following should be done by using ELEMENTS and type dcls...
    ((vector-element 
      vector-elements 
      array-element ;; Backwards compatibility -- DRM
      array-elements)
     aref length vector)
    ((simple-vector-element simple-vector-elements
      simple-general-vector-element simple-general-vector-elements)
     svref simple-vector-length simple-vector)
    ((bits bit bit-vector-element bit-vector-elements)
     bit bit-vector-length bit-vector bit)
    ((simple-bit-vector-element simple-bit-vector-elements)
     sbit simple-bit-vector-length simple-bit-vector bit)
    ((character characters string-element string-elements)
     char string-length string base-char)
    ((simple-string-element simple-string-elements)
     schar simple-string-length simple-string base-char)
    )
  )

;;(print "use: (setup-loop *loop-commands*) to setup the loop code")
(setup-loop *loop-commands*)



(trace loop-translate-1 loop-translate)
