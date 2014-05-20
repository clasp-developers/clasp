
defClass MyFrame wx:wxFrame ()
    method __init__ ( self &rest allArgs )
        callAncestorMethod

    method OnIconize (self event userData)
        println ["Caught Iconize event"]

    method OnButton (self event userData)
        println ["Caught Button event"]

describe wx:wxFrame
describe MyFrame

global w1 (MyFrame () -1 "Frame1" (wx:wxPoint 600 300) (wx:wxSize 200 200) )
[ wx:wxXmlResource_Get Load "./viewerInterface.xrc" ]
[ wx:wxXmlResource_Get LoadFrame w1 () "mwxCandoViewBase" ]

global canvas [ w1 FindWindow_withName "GLCanvas" ]
println [ "canvas = %s" % (repr canvas) ]


;global b1 [w1 FindWindow_withName "m_button1" ]
;assert [b1 notNil] "Could not find m_button1"
;
;
;[b1 Bind wx:wxEVT_COMMAND_BUTTON_CLICKED (lambda (event data) [w1 OnButton event data] ) ]

[w1 Show true]


local fd ( wx:wxFileDialog () "Select the file to display" "./" "" "*.cxml" )
local result [fd ShowModal]
local fileName ()
cond
    [result == wx:wxID_OK ]
        setq fileName [ fd GetPath ]
    true
        println [ "Dialog cancelled" ]
	exit 1

println ["A file was selected = %s" % fileName ]
local contents ( loadArchive fileName )
local dl [ contents rendered () ]
local rc (RenderController dl)
[ canvas GraphicsChanged rc ]
