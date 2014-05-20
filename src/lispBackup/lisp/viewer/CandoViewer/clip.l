(defun get-clip () (list (getNearClippingDistance *canvas*) (getFarClippingDistance *canvas*)))

(defun set-clip (near far) (setNearFarClippingDistances *canvas* (real near) (real far)))
(get-clip)

