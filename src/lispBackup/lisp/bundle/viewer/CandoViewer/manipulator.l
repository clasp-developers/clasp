
(use-package :mwx)

(defvar *hither-yon-separation* 0.1)

(defClass Manipulator () ( _Canvas
		       _Mouse1Mode _Mouse1ShiftMode
		       _Mouse2Mode _MouseOverrideMode _InOverrideMode
		       _CurrentMode _SavedMode 
		       _Llx _Lly _Urx _Ury 
		       _MouseStartX _MouseStartY
		       _MouseStartXOglView _MouseStartYOglView
		       _MouseCurX _MouseCurY
		       _MouseCurXOglView _MouseCurYOglView
		       ))


(defMethod __init__ Manipulator (self canvas)
  (setq (slot self _Mouse1Mode) :select )
  (setq (slot self _Mouse2Mode) :idle)
  (setq (slot self _MouseOverrideMode) :idle)
  (setq (slot self _InOverrideMode) false)
  (setq (slot self _Canvas) canvas))


(defMethod convertToRelativeCoordinates Manipulator ( self x y )
  (list (- x (slot self _Llx)) (- y (slot self _Lly))))

(defMethod setMouse1Mode Manipulator (self mode &key shift )
  (setq (slot self _Mouse1Mode) mode)
  (if shift
      (setq (slot self _Mouse1ShiftMode) shift)
    (setq (slot self _Mouse1ShiftMode) mode)))

(defMethod setMouseOverrideMode Manipulator (self mode)
  (setq (slot self _MouseOverrideMode) mode))

(defMethod setMouse2Mode Manipulator (self mode)
  (setq (slot self _Mouse2Mode) mode))

(defMethod setMyViewport Manipulator (self x y)
  (let* ((camera (getCamera (slot self _Canvas)))
	 (vp (viewportContainingPoint camera x y)))
    (when vp
      (setq (slot self _Llx) (car vp))
      (setq (slot self _Lly) (cadr vp))
      (setq (slot self _Urx) (caddr vp))
      (setq (slot self _Ury) (cadddr vp)))))

(defMethod endSelectionMode Manipulator (self addToSelection)
  (let ((canvas (slot self _Canvas)))
    (setDrawSelectionRectangleOn canvas false)
    (select canvas addToSelection)
    (draw canvas)))


(defMethod endMode Manipulator (self mode)
  (cond 
   ((== mode :select) (endSelectionMode self false))
   ((== mode :selectAdd) (endSelectionMode self true))
   (true (log (% "switching out of mode: %s" (repr mode))))))

(defMethod beginMode Manipulator (self mode x y)
  (setq (slot self _CurrentMode) mode)
  (cond
   ((or (eq mode :select) (eq mode :selectAdd))
    (setDrawSelectionRectangleOn (slot self _Canvas) true)
    (setSelectionRectangleStart (slot self _Canvas) x y)
    (setSelectionRectangleEnd (slot self _Canvas) x y))
   (true
    (log (% "switching into mode: %s" (repr mode))))))

(defMethod switchToMode Manipulator (self mode &optional (x 0) (y 0))
  (endMode self (slot self _CurrentMode x y))
  (beginMode self mode x y))

(defMethod identifyMode Manipulator (self mouse event)
  (cond
   ((eq mouse :mouse1)
    (if (ShiftDown event)
	(slot self _Mouse1ShiftMode)
      (slot self _Mouse1Mode)))
   ((eq mouse :mouse2)
    (slot self _Mouse2Mode))
   (true :idle)))
      

(defMethod setMouseStart Manipulator (self x y)
  (setq (slot self _MouseStartX) x)
  (setq (slot self _MouseStartY) y))

(defMethod setMouseCur Manipulator (self x y)
  (setq (slot self _MouseCurX) x)
  (setq (slot self _MouseCurY) y))


(defMethod mouseDown Manipulator (self mouse event)
  (let ((x (mwx:GetX event))
	(y (mwx:GetY event)))
    (setMouseStart self x y)
    (setMouseCur self x y)
    (setMyViewport self x y)
    (switchToMode self (identifyMode self mouse event) x y)))

(defMethod mouse1Down Manipulator (self event)
  (mouseDown self :mouse1 event))

(defMethod mouse2Down Manipulator (self event)
  (mouseDown self :mouse2 event))

(defMethod relativeX Manipulator (self ox)
  (- ox (slot self _Llx)))

(defMethod relativeY Manipulator (self oy)
  (- oy (slot self _Lly)))

(defMethod getWidth Manipulator (self)
  (- (slot self _Urx) (slot self _Llx)))

(defMethod getHeight Manipulator (self)
  (- (slot self _Ury) (slot self _Lly)))


(defMethod manipulateVirtualSphere Manipulator (self vsMode fx fy tx ty)
  (let ((virtualSphere (virtualSphere (layerManager (slot self _Canvas)))))
    (setMouseStart virtualSphere vsMode fx fy (getWidth self) (getHeight self))
    (setMouseMove virtualSphere tx ty)
    (draw (slot self _Canvas))))

(defMethod mouseMotion Manipulator (self event)
  (when (not (or (LeftIsDown event) (MiddleIsDown event))) (return-from mouseMotion))
  (let ((canvas (slot self _Canvas))
	(fx (relativeX self (slot self _MouseCurX)))
	(fy (relativeY self (slot self _MouseCurY)))
	(tx (relativeX self (mwx:GetX event)))
	(ty (relativeY self (mwx:GetY event)))
	(mode (slot self _CurrentMode)))
    (cond
     ((eq mode :idle) (return-from mouseMotion))
     ((eq mode :rotate) (manipulateVirtualSphere self :rotate fx fy tx ty))
     ((eq mode :zoom) (manipulateVirtualSphere self :scale fx fy tx ty))
     ((eq mode :pan) (manipulateVirtualSphere self :translate fx fy tx ty))
     ((or (eq mode :select) (eq mode :selectAdd))
      (setSelectionRectangleEnd (slot self _Canvas) (GetX event) (GetY event))
      (draw canvas))
     ((eq mode :hitherClip)
      (let* ((camera (getCamera (slot self _Canvas)))
	     (delta (- ty fy))
	     (hither (hitherClip camera))
	     (yon (yonClip camera))
	     (eyeZ (eyeZ camera)))
	(setq hither (+ hither (* 0.01 delta)))
	(setq hither (max hither (+ yon *hither-yon-separation*)))
	(setq hither (min hither (+ 0.01 eyeZ)))
	(setHitherYon camera hither yon)
	(draw canvas)))
     ((eq mode :yonClip)
      (let* ((camera (getCamera (slot self _Canvas)))
	     (delta (- ty fy))
	     (hither (hitherClip camera))
	     (yon (yonClip camera)))
	(setq yon (+ yon (* 0.01 delta)))
	(setq yon (min (- hither *hither-yon-separation*) yon ))
	(setHitherYon camera hither yon)
	(draw canvas)))
     (true (print (% "Ignoring mouseMotion in mode: %s" (repr mode)))))
    (setMouseCur self (GetX event) (GetY event))
    ))


(defMethod mouseUp Manipulator (self event)
  (switchToMode self :idle))


