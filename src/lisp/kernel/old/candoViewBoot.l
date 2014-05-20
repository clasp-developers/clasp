(load (path-append (scriptDir) "CANDO/init/common.l"))

(bformat t "Defining next and prev" )

(defun next ( )
  (let ((names (sliderNames))
	(firstSliderName ( car names )))
    (setSlider firstSliderName (+ (getSlider firstSliderName) 1))))


(defun prev ( )
    (let ((names (sliderNames))
	  (firstSliderName ( car names )))
      (setSlider firstSliderName (max 0 (- (getSlider firstSliderName) 1 )))))
