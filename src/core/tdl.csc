
local msg1 (GrText :text "Hi" :position (OVector3 0.0 0.0 0.0) )
local msg2 (GrText :text "There" :position (OVector3 100.0 0.0 0.0 ) )

local pnts (GrLineStrip (list 
		(OVector3 0.0 0.0 0.0)
		(OVector3 100.0 0.0 0.0)
		(OVector3 100.0 100.0 0.0)
		(OVector3 0.0 100.0 0.0)
		(OVector3 0.0 0.0 0.0)
		(OVector3 0.0 0.0 100.0)
		(OVector3 100.0 0.0 100.0)
		(OVector3 100.0 100.0 100.0)
		(OVector3 0.0 100.0 100.0)
		(OVector3 0.0 0.0 100.0)
		) )

local dl (RenderDisplayList)
[dl addGraphics (GrColor :name "red") ]
[dl addGraphics msg1 ]
[dl addGraphics msg2 ]
[dl addGraphics pnts ]


saveArchive dl "_dl.cxml"

