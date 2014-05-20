

(defClass SideView () (_Layer _Canvas _Camera _DisplayList _HitherClipLine
			      _YonClipLine _EyeTop _EyeBottom _WorldDepth
			      _FeatureScale _LeftMostFeature ))

(defMethod __init__ SideView (self canvas layer camera)
  (setq (slot self _Layer) layer)
  (setq (slot self _Canvas) canvas)
  (setq (slot self _Camera) camera)
  (connectListener camera self :nearFarChanged)
  (setq (slot self _DisplayList) (buildDisplayList self))
  (print "============ defining SideView")
  (bindTrigger camera :nearFarChanged (lambda (sender) (onCamera_nearFarChanged self sender)))
  (bindTrigger camera :focalChanged (lambda (sender) (onCamera_focalChanged self sender)))
  (bindTrigger camera :extentChanged (lambda (sender) (onCamera_extentChanged self sender)))
  )


(defMethod onCamera_nearFarChanged SideView (self sender)
  (rebuildGraphics self))


(defMethod onCamera_focalChanged SideView (self sender)
  (rebuildGraphics self))

(defMethod onCamera_extentChanged SideView (self sender)
  (rebuildGraphics self))



(defMethod manipulateFrontClip SideView (self)
  (log "manipulateFrontClip"))


(defMethod rebuildGraphics SideView (self)
  (let ((camera (getCamera (slot self _Canvas))))
    (let ((eyeZ (eyeZ camera))
	  (hither (hitherClip camera))
	  (yon (yonClip camera))
	  (worldBBox (worldBoundingBox (layerManager (slot self _Canvas)))))
      (log (% "    got yonClip = %f" yon ))
      (let* ((worldClosest (* 1.0 (getMaxZ worldBBox)))
	     (worldFarthest (* 1.0 (getMinZ worldBBox)))
	     (leftMostFeature eyeZ)
	     (rightMostFeature (min worldFarthest yon))
	     (featureDepth (- leftMostFeature rightMostFeature))
	     (featureScale (/ 0.8 featureDepth)))
	(setq (slot self _FeatureScale) featureScale)
	(setq (slot self _LeftMostFeature) leftMostFeature)
	(log "--------rebuildGraphics---------")
	(log (% "    rightMostFeature = %f" rightMostFeature ))
	(log (% "    featureDepth = %f" featureDepth ))
	(log (% "    _LeftMostFeature = %f" leftMostFeature ))
	(log (% "    _FeatureScale = %f" featureScale ))
	(log (% "    eyeZ = %f" eyeZ ))
	(log (% "    hither = %f" hither ))
	(log (% "    yon    = %f" yon ))
	(log (% "    bbox = %s - %s" (getMinZ worldBBox) (getMaxZ worldBBox) ))
	(log (% "    leftMostFeature[%f] rightMostFeature[%f] featureDepth[%f]" leftMostFeature rightMostFeature featureDepth ))
	(let ((ex 0.1)
	      (hx (sideViewPos self hither))
	      (yx (sideViewPos self yon))
	      (worldFrontX (sideViewPos self worldClosest))
	      (worldBackX (sideViewPos self worldFarthest)))
	  (set2DLine (slot self _HitherClipLine) (OVector2 hx 0.0) (OVector2 hx 1.0))
	  (set2DLine (slot self _YonClipLine) (OVector2 yx 0.0) (OVector2 yx 1.0))
	  (set2DLine (slot self _EyeTop) (OVector2 ex 0.2) (OVector2 (- ex 0.05) 0.5))
	  (set2DLine (slot self _EyeBottom) (OVector2 (- ex 0.05) 0.5) (OVector2 ex 0.8))
	  (set2DLine (slot self _WorldDepth) (OVector2 worldFrontX 0.5) (OVector2 worldBackX 0.5))
	  (Refresh (slot self _Canvas))
	  )))))


(defMethod sideViewPos SideView (self cameraPos)
  (+ 0.1 (* (- (slot self _LeftMostFeature) cameraPos) (slot self _FeatureScale))))

(defMethod cameraPos SideView (self sideViewPos)
  (log (% "Converting to cameraPos _LeftMostFeature[%f] _FeatureScale[%f]" (slot self _LeftMostFeature) (slot self _FeatureScale)))
  (let ((result  (- (slot self _LeftMostFeature)
		    (/ (- sideViewPos 0.1) (slot self _FeatureScale)) )))
    (log (% "Converted sideViewPos[%f] to cameraPos[%f]" sideViewPos result))
    result))

(defMethod sideViewDragObject SideView ( self moveCallback )
  (let (cameraMode viewIndex )
    (DragObject :start (lambda (event)
			 (let ((startx (GetX event))
			       (starty (- (getHeight (slot self _Canvas)) (GetY event))))
			   (setq cameraMode (mode (slot self _Camera)))
			   (setq viewIndex (viewContainingPoint cameraMode startx starty))))
		:drag (lambda (event)
			(progn
			  (let* ((vx (GetX event))
				 (vy (- (getHeight (slot self _Canvas)) (GetY event)))
				 (layerPos (positionInFixedLayer cameraMode (slot self _Layer) viewIndex vx vy )))
			    (log (% "Dragging mouse: %s" (repr layerPos) ))
			    (log (% "    layer = %s" (repr (slot self _Layer))))
			    (log (% "    self = %s" (repr self) ))
			    (apply moveCallback self (x layerPos))
			    (rebuildGraphics self))))
		:release (lambda (event) (log "Released")))))


(defMethod moveEye SideView (self delta)
  (let ((scale (* (+ 1.0 delta) (scaleFactor (slot self _Canvas)))))
    (setScaleFactor (slot self _Canvas) scale)
    (print (% "moveEye scale=%d" scale ))))


(defMethod moveHither SideView (self pos)
  (let ((cameraHither (cameraPos self pos))
	(cameraYon (yonClip (slot self _Camera))))
    (log (% "moveHither value=%s" cameraHither))
    (setHitherYon (slot self _Camera) cameraHither cameraYon )))

(defMethod moveYon SideView (self pos)
  (let ((cameraYon (cameraPos self pos))
	(cameraHither (hitherClip (slot self _Camera))))
    (log (% "moveYon value=%s" cameraYon))
    (setHitherYon (slot self _Camera) cameraHither cameraYon )))

  (defMethod buildDisplayList SideView (self)
    (let ((mainLine (Gr2DLine :v1 (OVector2 0.0 0.5)
			      :v2 (OVector2 1.0 0.5)
			      :color (lookup *colorTable* :white)
			      :width 2.0))
	  (dl (DisplayList :name :sideView))
	  (eyeMover (let (cameraMode viewIndex prevPos )
		      (DragObject :start (lambda (event)
					   (let ((startx (GetX event))
						 (starty (- (getHeight (slot self _Canvas)) (GetY event))))
					     (setq cameraMode (mode (slot self _Camera)))
					     (setq viewIndex (viewContainingPoint cameraMode startx starty))
					     (setq prevPos (x (positionInFixedLayer cameraMode (slot self _Layer) viewIndex startx starty)))))
				  :drag (lambda (event)
					  (progn
					    (let* ((vx (GetX event))
						   (vy (- (getHeight (slot self _Canvas)) (GetY event)))
						   (layerPoint (positionInFixedLayer cameraMode (slot self _Layer) viewIndex vx vy ))
						   (layerPos (x layerPoint)))
					      (log (% "Dragging mouse: %s" (repr layerPos) ))
					      (log (% "    layer = %s" (repr (slot self _Layer))))
					      (log (% "    self = %s" (repr self) ))
					      (moveEye self (- layerPos prevPos))
					      (setq prevPos layerPos))))
				  :release (lambda (event) (log "Released"))))))
      (setq (slot self _EyeTop) (GrPickable2DLine :color (lookup *colorTable* :red) :width 4.0 :object eyeMover ))
      (setq (slot self _EyeBottom) (GrPickable2DLine :color (lookup *colorTable* :red) :width 4.0 :object eyeMover ))
      (setq (slot self _HitherClipLine) (GrPickable2DLine :color (lookup *colorTable* :cyan) :width 4.0
							 :object (sideViewDragObject self (lambda (sideView xpos) (moveHither sideView xpos)))))
      (setq (slot self _YonClipLine) (GrPickable2DLine :color (lookup *colorTable* :blue) :width 4.0
							 :object (sideViewDragObject self (lambda (sideView xpos) (moveYon sideView xpos)))))
      (setq (slot self _WorldDepth ) (Gr2DLine :color (lookup *colorTable* :green) :width 4.0))
      (append dl mainLine)
      (append dl (slot self _EyeTop))
      (append dl (slot self _EyeBottom))
      (append dl (slot self _HitherClipLine))
      (append dl (slot self _YonClipLine))
      (append dl (slot self _WorldDepth))
      (rebuildGraphics self)
      dl))

  (defMethod getDisplayList SideView (self)
    (slot self _DisplayList))

  (defun setupDisplayList (canvas layer camera)
    (let ((sideView (SideView canvas layer camera)))
      (getDisplayList sideView)))



  (defun createSideViewFixedLayer (canvas camera)
    (log "Create sideView fixed layer")
    (let ((layer (FixedLayer :sideView :llx 0.1 :lly 0.0 :urx 0.9 :ury 0.05)))
      (setRenderController layer (RenderController (setupDisplayList canvas layer camera)))
      layer))
  