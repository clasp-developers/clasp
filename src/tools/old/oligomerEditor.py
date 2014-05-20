#! /bin/env python

import time
import traceback
import sys
from math import *

import  wx

from mbbCore import *

from dispatcher import *


OLIGOMER_CHANGED = "oligomer_changed"



#-------------------------------------------------------------
#
# Events
#
#

#
# candoApp events
#
ID_FILE_NEW			= 101
ID_FILE_OPEN_SEQUENCE		= 102
ID_FILE_OPEN_SEQUENCE_LIST 	= 103
ID_FILE_SAVE_SEQUENCE_AS	= 104
ID_FILE_SAVE_STRUCTURE_AS	= 105
ID_FILE_EDIT_IN_MOE		= 106
ID_FILE_PREFERENCES		= 107
ID_FILE_EXIT			= 108
ID_EDIT_COPY			= 150
ID_SEQUENCE_GET_FROM_SELECTION	= 201
ID_SEQUENCE_DUPLICATE_ALL	= 202
ID_SEQUENCE_DUPLICATE_TO_END	= 203
ID_VIEW_CENTER_ON_GEOMETRIC_CENTER = 300
ID_VIEW_AXES_TOGGLE		= 301
ID_VIEW_STRUCTURE_TOGGLE	= 302
ID_VIEW_BACKBONE_TOGGLE		= 303
ID_VIEW_CENTERS_TOGGLE		= 304
ID_VIEW_CHOICES_TOGGLE		= 305
ID_VIEW_SKELETON_TOGGLE		= 306
ID_VIEW_STEREO_MONO		= 307
ID_VIEW_STEREO_QUAD_BUFFERED	= 308
ID_VIEW_STEREO_CROSS_EYED	= 309
ID_VIEW_STEREO_WALL_EYED	= 310
ID_VIEW_HIT_DETAILS_TOGGLE	= 311
ID_VIEW_SEARCH_DETAILS_TOGGLE	= 312
ID_HELP_MOUSE_KEYBOARD		= 401

#
# viewSequence events
#
ID_SEQUENCE_LISTBOX = 1001
ID_DISPLAY_LISTS_LISTBOX = 1002


#
# viewDatabase events
#
ID_DATABASE_LISTBOX 	= 2001
ID_SHOW_SUBSET		= 2002
ID_SHOW_ALL		= 2003
ID_LISTBOX_DCLICK 	= 2004 




#---------------------------------------------------------------------------

class Log:

    def __init__(self):
	self.out = sys.stdout

    def write(self,str):
	self.out.write("LOG: ")
	self.out.write(str)
	self.out.write("\n")
	self.out.flush()
	sys.stdout.flush()
	sys.stderr.flush()







class	CandoEditorApp(wx.App):

    def OnInit(self):
	self.canvas = None

	self.log = Log()
	self.mainFrame = wx.Frame( None, -1, "CANDO" )
	menuBar = self.createMenuBar()
	self.mainFrame.SetMenuBar(menuBar)
	self.mainFrame.SetPosition((0,0))

	self.partsWindow = PartsWindow(self.mainFrame,
					"Parts", self, self.log)
	self.partsWindow.SetPosition((0,60))
	self.partsWindow.SetSize((200,600))

#
#	self.statusBar = self.oligomerEditorFrame.CreateStatusBar()
#
#	self.oligomerEditor = OligomerEditorPanel(self,self.oligomerEditorFrame,self.log)
	self.mainFrame.SetSize((800,60))
#	panel = wx.Panel(self.mainFrame,-1,wx.DefaultPosition,(100,1))
	self.mainFrame.Refresh()
	self.mainFrame.Show()


	self.oligomerEditors = []
	self.unnamed = 1

	self.appCurrentOligomerEditor = None
	self.appMonomer = None
	self.appCoupling = None

	return True

    def getCurrentMonomer(self):
	if ( self.appMonomer == None ):
	    name = "undef"
	else:
	    name = self.appMonomer
	return name

    def getCurrentCoupling(self):
	if ( self.appCoupling == None ):
	    return "?"
	return self.appCoupling

    def setDefaultCouplingName(self,nm):
	self.appCoupling = nm


    def OnMonomerSelected(self,obj):
	
	self.appMonomer = obj


    def OnMonomerActivated(self,obj):
	if ( self.appCurrentOligomerEditor != None ):
	    self.appCurrentOligomerEditor.OnMonomerActivated(obj)

    def OnOligomerEditorGotFocus(self,oligomerEditor):
	self.log.write("DEBUG:OnOligomerEditorGotFocus -->%s"%oligomerEditor)
	self.appCurrentOligomerEditor = oligomerEditor



    def createMenuBar(self):
		#
		# File menu
		#
	menu = wx.Menu()
	menu.Append(ID_FILE_NEW, "&New", "Start a new oligomer" )
	menu.Append(ID_FILE_OPEN_SEQUENCE, "&Open sequence...", \
					"Open a bisPeptide sequence" )
	menu.Append(ID_FILE_OPEN_SEQUENCE_LIST, "Open sequence &list...", \
					"Open a bisPeptide sequence list" )
	menu.AppendSeparator()
	menu.Append(ID_FILE_SAVE_SEQUENCE_AS, "&Save Sequence As...", \
					"Save the current bisPeptide sequence" )
	menu.Append(ID_FILE_SAVE_STRUCTURE_AS, "Save S&tructure As...", \
				"Save the current bisPeptide structure" )
	menu.AppendSeparator()
	menu.Append(ID_FILE_EDIT_IN_MOE, "&Edit in MOE", \
					"Edit the current structure in MOE" )
	menu.AppendSeparator()
	menu.Append(ID_FILE_PREFERENCES, "&Preferences...", \
				"Edit the preferences" )
	menu.AppendSeparator()
	menu.Append(ID_FILE_EXIT, "E&xit", "Exit" )
	self.fileMenu = menu

	wx.EVT_MENU( self, ID_FILE_NEW, self.menuFileNew )
#	wx.EVT_MENU( self, ID_FILE_OPEN_SEQUENCE, self.menuFileOpen )
#	EVT_MENU( self, ID_FILE_OPEN_SEQUENCE_LIST, self.menuFileOpenSequenceList)
#	EVT_MENU( self, ID_FILE_SAVE_SEQUENCE_AS, self.menuFileSaveSequenceAs)
#	EVT_MENU( self, ID_FILE_SAVE_STRUCTURE_AS, self.menuFileSaveStructureAs)
#	EVT_MENU( self, ID_FILE_EDIT_IN_MOE, self.menuFileEditInMoe)
#	EVT_MENU( self, ID_FILE_PREFERENCES, self.menuFilePreferences )
#	EVT_MENU( self, ID_FILE_EXIT, self.menuFileExit )

		#
		# Edit menu
		#
	menu = wx.Menu()
	menu.Append( ID_EDIT_COPY, "&Copy", "Copy structure" )
	self.editMenu = menu

#	EVT_MENU( self, ID_EDIT_COPY, self.menuEditCopy )


		#
		# Sequence menu
		#

	menu = wx.Menu()
	menu.Append(ID_SEQUENCE_DUPLICATE_ALL, "&Duplicate", "Duplicate the entire sequence" )
	menu.AppendSeparator()
	menu.Append(ID_SEQUENCE_DUPLICATE_TO_END, "Duplicate to &end", "Duplicate the rest of the sequence" )
	self.sequenceMenu = menu
#	EVT_MENU( self, ID_SEQUENCE_DUPLICATE_ALL, self.menuSequenceDuplicateAll)
#	EVT_MENU( self, ID_SEQUENCE_DUPLICATE_TO_END, self.menuSequenceDuplicateToEnd)

	menu = wx.Menu()
	menu.Append(ID_VIEW_CENTER_ON_GEOMETRIC_CENTER, "&Center view", "Center view on the geometric center" )
	menu.AppendSeparator()
#	menu.Append(ID_VIEW_AXES_TOGGLE, "Toggle A&xes", "Toggle the display of the coordinate axes" )
#	menu.Append(ID_VIEW_STRUCTURE_TOGGLE, "Toggle &structure", "Toggle the display of the bis-peptide structure" )
#	menu.Append(ID_VIEW_BACKBONE_TOGGLE, "Toggle &backbone", "Toggle the display of the bis-peptide backbone" )
#	menu.Append(ID_VIEW_CENTERS_TOGGLE, "Toggle &centers", "Toggle the display of the bis-amino acid centers" )
#	menu.Append(ID_VIEW_CHOICES_TOGGLE, "Toggle &choices", "Toggle the display of the bis-amino acid choices" )
#	menu.Append(ID_VIEW_HIT_DETAILS_TOGGLE, "Toggle &hit details", "Toggle the display of the hit details" )
#	menu.Append(ID_VIEW_SEARCH_DETAILS_TOGGLE, "Toggle &search details", "Toggle the display of the search details" )
	menu.AppendSeparator()
	menu.Append(ID_VIEW_STEREO_MONO, "Stereo off", "Turn off stereo" )
	menu.Append(ID_VIEW_STEREO_CROSS_EYED, "Stereo cross-eyed", "Turn on cross-eyed stereo" )
	menu.Append(ID_VIEW_STEREO_WALL_EYED, "Stereo wall-eyed", "Turn on wall-eyed stereo" )
	menu.Append(ID_VIEW_STEREO_QUAD_BUFFERED, "Stereo quad-buffered", "Turn on quad-buffered stereo" )
	self.viewMenu = menu
#	EVT_MENU( self, ID_VIEW_CENTER_ON_GEOMETRIC_CENTER, self.menuViewCenterOnGeometricCenter)
#	EVT_MENU( self, ID_VIEW_AXES_TOGGLE, self.menuViewAxesToggle)
#	EVT_MENU( self, ID_VIEW_STRUCTURE_TOGGLE, self.menuViewStructureToggle)
#	EVT_MENU( self, ID_VIEW_BACKBONE_TOGGLE, self.menuViewBackboneToggle)
#	EVT_MENU( self, ID_VIEW_HIT_DETAILS_TOGGLE, self.menuViewHitDetailsToggle)
#	EVT_MENU( self, ID_VIEW_SEARCH_DETAILS_TOGGLE, self.menuViewSearchDetailsToggle)
#	EVT_MENU( self, ID_VIEW_CENTERS_TOGGLE, self.menuViewCentersToggle)
#	EVT_MENU( self, ID_VIEW_CHOICES_TOGGLE, self.menuViewChoicesToggle)
#	EVT_MENU( self, ID_VIEW_STEREO_MONO, self.menuViewStereoMono)
#	EVT_MENU( self, ID_VIEW_STEREO_CROSS_EYED, self.menuViewStereoCrossEyed)
#	EVT_MENU( self, ID_VIEW_STEREO_WALL_EYED, self.menuViewStereoWallEyed)
#	EVT_MENU( self, ID_VIEW_STEREO_QUAD_BUFFERED, self.menuViewStereoQuadBuffered)


	menuBar = wx.MenuBar()
	menuBar.Append(self.fileMenu, "&File" )
	menuBar.Append(self.editMenu, "&Edit" )
	menuBar.Append(self.sequenceMenu, "&Sequence" )
	menuBar.Append(self.viewMenu, "&View" )
	return menuBar



    def setTopologyDatabase(self,topDb):
	self.topologyDatabase = topDb
	print "Setting topologyDatabase"
	self.partsWindow.setTopologyDatabase(topDb)

    def getTopologyDatabase(self):
	return self.topologyDatabase


#    def setOligomer(self,o):
#	self.oligomer = o
#	if (self.canvas != None):
#	    self.canvas.setOligomer(o)

    def menuFileNew(self,evt):
	self.newOligomer()

    def newOligomer(self):
	# Create an oligomer
	olig = GrOligomer(self.log)
	olig.setTopologyDatabase(self.topologyDatabase)
	mon1 = GrMonomer(olig,"gly")
	mon1.setRoot()
	mon2 = GrMonomer(olig,"gly")
	olig.addMonomer(mon1)
	olig.addMonomer(mon2)
	coup = GrCoupling(olig,"dkp")
	coup.couple(mon1,mon2)
	olig.addCoupling(coup)
	olig.layout()
	olig.setName("unnamed-%d"%self.unnamed)
	self.unnamed = self.unnamed+1
	oligEditor = OligomerEditorPanel(self,self.mainFrame,self.log)
	oligEditor.setOligomer(olig)
	self.oligomerEditors.append(oligEditor)
	self.appCurrentOligomerEditor = oligEditor


class OligomerEditorPanel(wx.Frame):

    drawMode = "draw"
    selectMode = "select"

    def __init__(self, application, parent, log ):
	wx.Frame.__init__(self,parent,-1,"",wx.DefaultPosition,(800,600))

	self.app = application
	self.log = log
	self.oligomer = None
	self.name = ""

	self.toolBar = self.CreateToolBar( wx.TB_HORIZONTAL 
				| wx.NO_BORDER 
				| wx.TB_FLAT 
				| wx.TB_TEXT )
	buttonId = wx.NewId()
	button = wx.Button(self.toolBar,buttonId,"Draw")
	self.toolBar.AddControl(button)
	button.Bind(wx.EVT_BUTTON, self.DrawOn)

	buttonId = wx.NewId()
	button = wx.Button(self.toolBar,buttonId,"Select")
	self.toolBar.AddControl(button)
	button.Bind(wx.EVT_BUTTON, self.SelectOn)

	cbID = wx.NewId()
	self.toolBar.AddControl(wx.ComboBox(self.toolBar,cbID,"",
			choices=["100","75","50","25","10"],
			size = (50,-1),style=wx.CB_DROPDOWN))
	self.Bind(wx.EVT_COMBOBOX, self.OnScaleCombo,id=cbID)

	
	self.toolBar.Realize()

	self.sequenceSash = wx.SashLayoutWindow(
                self, -1, wx.DefaultPosition, wx.DefaultSize, 
                wx.NO_BORDER|wx.SW_3D )
        self.sequenceSash.SetDefaultSize((200, 10000))
        self.sequenceSash.SetOrientation(wx.LAYOUT_VERTICAL)
        self.sequenceSash.SetAlignment(wx.LAYOUT_LEFT)
        self.sequenceSash.SetBackgroundColour(wx.Colour(255, 0, 0))
        self.sequenceSash.SetSashVisible(wx.SASH_RIGHT, True)

        self.Bind( wx.EVT_SASH_DRAGGED_RANGE, self.OnSashDrag,
            id=self.sequenceSash.GetId(), id2=self.sequenceSash.GetId() )
	self.canvas = OligomerLayoutCanvas(self.sequenceSash,self.app,self.log)

        self.structurePanel = wx.Panel(self, -1, 
					wx.DefaultPosition, (100,100),
					style=wx.SUNKEN_BORDER)

        self.structurePanel.SetBackgroundColour(wx.Colour(0, 0, 255))
#	siz = wx.BoxSizer( )
#	self.sequenceSash_remainder.SetSizer(siz)
#	siz.Add(app.canvas,wx.ALL,wx.EXPAND)

        self.Bind(wx.EVT_SIZE, self.OnSize )
        self.Bind(wx.EVT_SET_FOCUS, self.OnFocus )
        self.Bind(wx.EVT_ACTIVATE,self.OnActivate )
	self.Show()
	self.canvas.SelectOn(None)

    def OnMonomerActivated(self,obj):
	self.oligomer.mutateAllSelectedTo(obj)

    def OnScaleCombo(self,event):
	sc = float(event.GetString())/100.0
	if ( sc < 0.05 ):
	    sc = 0.05
	if ( sc > 2.0 ):
	    sc = 2.0
	self.canvas.setScale(sc)

    def OnFocus(self,event):
	self.log.write("Got focus")
	self.app.OnOligomerEditorGotFocus(self)

    def OnActivate(self,event):
	active = event.GetActive()
	if ( active ):
	    self.log.write("DEBUG:OnActivate got activated: %s"%self.name)
	else:
	    self.log.write("DEBUG:OnActivate got UN-activated: %s"%self.name)


    def SelectOn(self,event):
	self.canvas.SelectOn(event)

    def DrawOn(self,event):
	self.canvas.DrawOn(event)

    def OnSashDrag(self, event):
        if event.GetDragStatus() == wx.SASH_STATUS_OUT_OF_RANGE:
            self.log.write('drag is out of range')
            return
        eobj = event.GetEventObject()

        if eobj is self.sequenceSash:
            self.log.write('sequenceSash received drag event')
            self.sequenceSash.SetDefaultSize((event.GetDragRect().width, 
						1000))
        wx.LayoutAlgorithm().LayoutWindow(self, self.structurePanel)
        self.structurePanel.myRefresh()

    def OnSize(self, event):
	self.log.write("OnSize")
        wx.LayoutAlgorithm().LayoutWindow(self, self.structurePanel )
	pass


    def setOligomer(self, o):
	self.oligomer = o
	self.canvas.setOligomer(o)
	self.name = o.name
	self.SetTitle(self.name)

    def getOligomer(self):
	return self.oligomer	


#
#
# ToolboxWindow
#
# Allows selection of monomers and couplings
#
#

class PartsWindow(wx.MiniFrame):
    def __init__( self, parent, title, app, log, 
			pos=wx.DefaultPosition, 
			size=wx.DefaultSize,
			style=wx.THICK_FRAME|wx.CAPTION ):
	self.app = app
	self.log = log
	self.lastSelected = None
	wx.MiniFrame.__init__(self,parent,-1,title,pos,size,style)
#	panel = wx.Panel(self,-1)

	self.monomerTree = wx.TreeCtrl(self)
	self.topologyDatabase = None
	self.Bind(wx.EVT_CLOSE, self.OnCloseWindow )
	self.Bind(wx.EVT_TREE_SEL_CHANGED, self.OnSelectionChanged )
	self.Bind(wx.EVT_TREE_ITEM_ACTIVATED, self.OnSelectionActivated)
	self.Show()

    def OnSelectionChanged(self,event):
	tree = self.monomerTree
	item = event.GetItem()
	obj = self.monomerTree.GetItemText(item)	
	self.app.OnMonomerSelected(obj)

    def OnSelectionActivated(self,event):
	tree = self.monomerTree
	item = event.GetItem()
	obj = self.monomerTree.GetItemText(item)	
	self.app.OnMonomerActivated(obj)


    def setTopologyDatabase(self,top):
	self.fillMonomerSelector(top)


    def fillMonomerSelector(self,topDb):
	if ( self.monomerTree == None ):
	    return
	tree = self.monomerTree
	root = tree.AddRoot("Monomers")
	tree.SetPyData( root, None )
	layoutXml = topDb.getMonomerSelectorLayout()
	children = layoutXml.getChildren()
	for child in children:
	    self.fillBranch(tree,root,child)
	tree.Expand(root)

    def fillBranch(self, tree, parent, xml ):
	if ( xml.getLocalName() == "group" ):
	    name = xml.getAttribute("name")
	    branch = tree.AppendItem(parent,name)
	    tree.SetPyData( branch, None )	# no data for groups
	    for child in xml.getChildren():
		self.fillBranch(tree,branch,child)
	    tree.Expand(branch)
	elif (xml.getLocalName() == "monomer"):
	    name = xml.getAttribute("name")
	    leaf = tree.AppendItem(parent,name)
	    tree.SetPyData( leaf, name )


    def OnCloseWindow(self):
	pass
	





class OligomerLayoutCanvas(wx.ScrolledWindow):
    def __init__(self, parent, appl,log, id = -1, size = wx.DefaultSize):
        wx.ScrolledWindow.__init__(self, parent, id, (0, 0), size=size, style=wx.SUNKEN_BORDER|wx.CLIP_CHILDREN)
	self.app = appl
	self.log = log
        self.x = self.y = 0
        self.drawing = False
	self.rubberBand = False
	self.scale = 0.5

        self.SetBackgroundColour("WHITE")
        self.SetCursor(wx.StockCursor(wx.CURSOR_PENCIL))

        self.SetScrollRate(20,20)
        self.Bind(wx.EVT_LEFT_DOWN, self.OnLeftButtonEvent)
        self.Bind(wx.EVT_LEFT_UP,   self.OnLeftButtonEvent)
        self.Bind(wx.EVT_MOTION,    self.OnLeftButtonEvent)
        self.Bind(wx.EVT_PAINT, self.OnPaint)

	self.Bind(wx.EVT_RIGHT_UP, self.OnRightClick)
	self.createCouplingMenu()

	connect(self,OLIGOMER_CHANGED)


    def __call__(self,**args):
	signal = args['signal']
	if ( signal == OLIGOMER_CHANGED ):
	    self.myRefresh()

    def myRefresh(self):
	self.Refresh(eraseBackground=False)


    def setScale(self,s):
	self.scale = s
	self.myRefresh()
    
    def setOligomer(self,o):
	self.oligomer = o


    def createCouplingMenu(self):
	topDb = self.app.getTopologyDatabase()
	couplings = topDb.allCouplingRules()
	self.couplingNames = {}
	menu = wx.Menu()
	for x in couplings:
	    nid = wx.NewId()
	    self.couplingNames[nid] = x
	    self.Bind( wx.EVT_MENU, self.OnCouplingSet, id=nid)
	    menu.Append(nid,x)
	self.couplingMenu = menu

    def OnCouplingSet(self,event):
	nid = event.GetId()
	name = self.couplingNames[nid]
	self.log.write("Setting coupling to: %s"%name)
	self.oligomer.setSelectedCouplingsTo(name)
	self.app.setDefaultCouplingName(name)

    def SelectOn(self,event):
	self.mode = OligomerEditorPanel.selectMode
	self.SetCursor(wx.StockCursor(wx.CURSOR_ARROW))

    def DrawOn(self,event):
	self.mode = OligomerEditorPanel.drawMode
        self.SetCursor(wx.StockCursor(wx.CURSOR_PENCIL))

    def OnPaint(self, event):
	self.DoDrawing()


    def DoDrawing(self, printing=False):
	extents = self.oligomer.renderBoundingBox()
        self.SetVirtualSize((extents.getWidth(),extents.getHeight()))
        dc = wx.BufferedPaintDC(self,style=wx.BUFFER_CLIENT_AREA)
	vsx,vsy = self.GetViewStart()
	vpx,vpy = self.GetScrollPixelsPerUnit()
	x = vsx*vpx
	y = vsy*vpy
#        self.PrepareDC(dc)
#        dc = wx.PaintDC(self)
#xxxxxxxxxxxxxx
	dc.SetBackground(wx.Brush('WHITE'))
	dc.Clear()
	dc.SetDeviceOrigin(-x,-y)
	dc.SetUserScale(self.scale,self.scale)
	self.oligomer.render(dc)
	dc.SetUserScale(1.0,1.0)
        dc.EndDrawing()

    def ConvertEventCoords(self, event):
        xView, yView = self.GetViewStart()
        xDelta, yDelta = self.GetScrollPixelsPerUnit()
	x = event.GetX()
	y = event.GetY()
#	self.log.write("ViewStart = %f,%f"%(xView,yView))
#	self.log.write("delta = %f,%f"%(xDelta,yDelta))
#	self.log.write("x,y = %f,%f"%(x,y))
        return (event.GetX()/self.scale + (xView * xDelta/self.scale),
                event.GetY()/self.scale + (yView * yDelta/self.scale))

    def drawRubberBand(self):
	dc = wx.ClientDC(self)
	self.PrepareDC(dc)
	dc.BeginDrawing()
	dc.SetUserScale(self.scale,self.scale)
	dc.SetPen(wx.Pen('WHITE', 4))
	dc.SetBrush(wx.TRANSPARENT_BRUSH)
	dc.SetLogicalFunction(wx.INVERT)
	xWidth = self.xStop-self.xStart
	yWidth = self.yStop-self.yStart
	dc.DrawRectangle(self.xStart,self.yStart,xWidth,yWidth)
	dc.EndDrawing()
	
    def drawBranch(self):
	dc = wx.ClientDC(self)
	self.PrepareDC(dc)
	dc.BeginDrawing()
	dc.SetUserScale(self.scale,self.scale)
	dc.SetPen(wx.Pen('WHITE', 4))
	dc.SetLogicalFunction(wx.INVERT)
	dc.DrawLine(self.xStart,self.yStart,self.xStop,self.yStop)
	dc.EndDrawing()
	
    def OnLeftButtonEvent(self, event):
	self.log.write("Got event %s"%event )
	if ( self.mode == OligomerEditorPanel.selectMode ):
            if (event.LeftDown()):
		if ( not event.ShiftDown() ):
		    self.oligomer.clearSelection()
		x,y = self.ConvertEventCoords(event)
		obj = self.oligomer.pick(x,y)
		self.selectedObject = obj
		self.rubberBand = False
		if ( obj == None ):
		    self.rubberBand = True
		    self.SetFocus()
		    self.xStart, self.yStart = (x,y)
		    self.xStop, self.yStop = (x,y)
		    self.CaptureMouse()
		    self.drawRubberBand()
		else:
		    self.selectedObject.setSelected(True)
#		    self.selectedObject.setPosition(x,y)
		    self.myRefresh()
	    elif event.Dragging():
		if ( self.rubberBand ):
		    self.drawRubberBand()
		    self.xStop, self.yStop = self.ConvertEventCoords(event)
		    self.drawRubberBand()
		else:
		    x,y = self.ConvertEventCoords(event)
		    self.selectedObject.setPosition(x,y)
		    self.myRefresh()
	    elif event.LeftUp():
		if ( self.rubberBand ):
		    self.ReleaseMouse()
		    self.xStop, self.yStop = self.ConvertEventCoords(event)
		    self.drawRubberBand()
		    self.rubberBand = False
			    # do rectangle selection stuff
		    rect = Rectangle()
		    rect.set(self.xStart, self.yStart, self.xStop, self.yStop )
		    self.oligomer.selectWithin(rect)

		else:
		    x,y = self.ConvertEventCoords(event)
#		    self.selectedObject.setPosition(x,y)
		    self.myRefresh()
		    
		    
	elif ( self.mode == OligomerEditorPanel.drawMode ):
            if event.LeftDown():
		self.oligomer.clearSelection()
		x,y = self.ConvertEventCoords(event)
		obj = self.oligomer.pick(x,y)
		self.drawingFrom = obj
		if ( obj != None ):
		    if ( isinstance(obj,GrMonomer) ):
			x,y = obj.getPosition()
			self.xStart = x
			self.yStart = y
			self.xStop = x
			self.yStop = y
			self.drawing = True
			self.SetFocus()
			self.CaptureMouse()
			self.drawBranch()
	    elif event.Dragging():
		if ( self.drawing ):
		    self.drawBranch()
		    self.xStop, self.yStop = self.ConvertEventCoords(event)
		    self.drawBranch()
	    elif event.LeftUp():
		if ( self.drawing ):
		    self.ReleaseMouse()
		    self.xStop, self.yStop = self.ConvertEventCoords(event)
		    self.drawBranch()
		    self.drawing = False
			    # do drawing stuff
		    self.oligomer.addOutFrom(self.drawingFrom,(self.xStop,self.yStop),self.app.getCurrentCoupling(),self.app.getCurrentMonomer())
	

    def	OnRightClick(self,event):
        pos = (self.x, self.y) + self.ConvertEventCoords(event)
	print "Right click event at: ", pos
	obj = self.oligomer.pick(pos[2],pos[3])
	if ( obj != None ):
	    if ( isinstance(obj,GrCoupling) ):
		self.rightClickOnCoupling(event,obj)
	    elif ( isinstance(obj,GrMonomer) ):
	        print "Picked Monomer: ", obj.name
		self.rightClickOnMonomer(event,obj)
	    else:
		print "I don't know what you picked"
	else:
	    self.popUpID1 = wx.NewId()
	    self.popUpID2 = wx.NewId()
	    self.Bind( wx.EVT_MENU, self.OnLayout, id=self.popUpID1)
	    self.Bind( wx.EVT_MENU, self.OnDeleteSelected, id=self.popUpID2)
	    menu = wx.Menu()
	    menu.Append(self.popUpID1,"Layout")
	    menu.Append(self.popUpID2,"Delete selected")
	    self.PopupMenu(menu,event.GetPosition())
	    menu.Destroy()

    def	rightClickOnCoupling(self,event,coupling):
	coupling.setSelected(True)
	self.PopupMenu(self.couplingMenu,event.GetPosition())
	
    def	rightClickOnMonomer(self,event,coupling):
	self.popUpID_deleteMonomer = wx.NewId()
	self.popUpID2 = wx.NewId()
	self.Bind( wx.EVT_MENU, self.OnPopUp_deleteMonomer, \
					id=self.popUpID_deleteMonomer)
	self.Bind( wx.EVT_MENU, self.OnPopUp2, id=self.popUpID2)
	menu = wx.Menu()
	menu.Append(self.popUpID_deleteMonomer,"Delete")
	menu.Append(self.popUpID2,"Two Monomer")
	self.PopupMenu(menu,event.GetPosition())
	menu.Destroy()

	
    def	OnPopUp_deleteMonomer(self,event):
	print "clicked deleteMonomer:", event
	sys.stdout.flush()


    def	OnLayout(self,event):
	self.oligomer.layout()

    def	OnDeleteSelected(self,event):
	self.oligomer.deleteSelected()

#---------------------------------------------------------------------------



overview = """
<html>
<body>
The wx.ScrolledWindow class manages scrolling for its client area, transforming the 
coordinates according to the scrollbar positions, and setting the scroll positions, 
thumb sizes and ranges according to the area in view.
</body>
</html>
"""








class	SpanningTree:
    def __init__(self,olig):
	sys.stdout.flush()
	self.oligomer = olig
	self.spanningList = [self.oligomer.root()]
	spanIndex = 0
	while ( spanIndex < len(self.spanningList) ):
	    sys.stdout.flush()
	    spanNext = self.spanningList[spanIndex]
	    spanIndex = spanIndex + 1
	    outCouplings = spanNext.outCouplings
	    for i in outCouplings:
		outMon = i.toMonomer
		self.spanningList.append(outMon)
	self.spanIndex = 0

    def nextInSpanningTree(self):
	if ( self.spanIndex < len(self.spanningList) ):
	   r = self.spanningList[self.spanIndex]
	   self.spanIndex = self.spanIndex + 1
	   return r
	return None

    def lastInSpanningTree(self):
	return self.spanningList[-1]

    def longestRun(self):
	cur = self.lastInSpanningTree()
	longRun = []
	while ( cur != None ):
	    longRun.insert(0,cur)
	    cur = cur.getInMonomer()
	return longRun


class	GrMonomer:
    def __init__(self,olig,nm="Undef",rt=False):
	self.inCoupling = None
	self.outCouplings = []
	self.oligomer = olig
	self.name = nm
	self.selected = False
	self.positionBackup = (0,0)
	self.position = (0,0)
	self.extent = None
	self.inError = True
	self.errorReason = ""
	self.isRoot = rt
	self.spanningIndex = -1
	self.spanningBack = None
	self.spanningNext = None

    def getCouplingToPosition(self,posFrom,dev=None):
	if ( self.extent == None ):
	    self.calculateBoxExtent(dev)
	diff = (self.position[0]-posFrom[0],self.position[1]-posFrom[1])
	ext = self.extent
	halfX = ext[0]/2
	halfY = ext[1]/2
	xdiff = diff[0]
	ydiff = diff[1]
#	print "xdiff,ydiff=",xdiff," ",ydiff
#	sys.stdout.flush()
	if ( ydiff >= 0 ):
	    if ( abs(xdiff) <= ydiff ):
	        return (self.position[0],self.position[1]-halfY)
	    if ( (xdiff) > ydiff ):
	        return (self.position[0]-halfX,self.position[1])
	    return (self.position[0]+halfX,self.position[1])
	# ydiff < 0
	if ( abs(xdiff) <= abs(ydiff) ):
	    return (self.position[0],self.position[1]+halfY)
	if ( (xdiff) > abs(ydiff) ):
	    return (self.position[0]-halfX,self.position[1])
	return (self.position[0]+halfX,self.position[1])


    def setName(self,nm):
	self.name = nm
	self.extent = None	

    def getBoxExtent(self):
	if ( self.extent == None ):
	    raise "GrMonomer::getBoxExtent no extent defined"
	return self.extent

    def calculateBoxExtent(self,dc):
	if ( self.extent == None ):
	    if ( dc == None ):
	        raise "GrMonomer::calculateBoxExtent must have a device context the first time its called"
	    te = dc.GetTextExtent(self.name)
	    self.extent = (te[0]+20,te[1]+10)

    def render(self,dc):
	pos = self.position
	te = dc.GetTextExtent(self.name)
	if ( self.extent == None ):
	    self.calculateBoxExtent(dc)
	offsetX = self.extent[0]/2
	offsetY = self.extent[1]/2
	if ( self.inError ):
	    dc.SetBrush(wx.RED_BRUSH)
	    dc.DrawRoundedRectangle(pos[0]-offsetX-5,pos[1]-offsetY-5,\
				    self.extent[0]+10,self.extent[1]+10,10)
		#
		# if we have an in-coupling then draw a circle there
		#
	if ( self.inCoupling != None ):
	    inMonomer = self.getInMonomer()
	    fromPos = inMonomer.position
	    incoming = self.getCouplingToPosition(fromPos)
	    dc.SetBrush(wx.BLUE_BRUSH)
	    dc.DrawCircle(incoming[0],incoming[1],10)

		#
		# Draw the rectangle
		#
	if ( self.selected ):
            dc.SetBrush(wx.CYAN_BRUSH)
	else:
	    dc.SetBrush(wx.WHITE_BRUSH)
	dc.DrawRoundedRectangle(pos[0]-offsetX,pos[1]-offsetY,\
				    self.extent[0],self.extent[1],10)
	dc.SetFont(wx.Font(14, wx.SWISS, wx.NORMAL, wx.NORMAL))
	dc.SetTextForeground(wx.Colour(0,0,0))
	x = pos[0] - (te[0]/2)
	y = pos[1] - (te[1]/2)
	dc.DrawText(self.name,x,y)

		#
		# If its not the root monomer draw a close box
		#
	if ( not self.isRoot ):
	    dc.SetBrush(wx.RED_BRUSH)
	    sz = 10
	    delX = pos[0]+offsetX-sz
	    delY = pos[1]-offsetY+sz
	    dc.DrawCircle(delX,delY,10)

		#
		# Now draw the new button
		#
	dc.SetBrush(wx.GREEN_BRUSH)
	sz = 10
	newX = pos[0]+offsetX/2
	newY = pos[1]+offsetY-sz
	dc.DrawCircle(newX,newY,10)
	
	

    def renderBoundingBox(self):
	pos = self.position
	if ( self.extent == None ):
	    rect = Rectangle()
	    rect.set(pos[0],pos[1],pos[0],pos[1])
	    return rect
	offsetX = self.extent[0]/2
	offsetY = self.extent[1]/2
	rect = Rectangle()
	rect.set(pos[0]-offsetX,pos[1]-offsetY,pos[0]+offsetX,pos[1]+offsetY)
	return rect

    def pick(self,x,y):
	extent = self.getBoxExtent()
	xHalfWidth = extent[0]/2
	yHalfWidth = extent[1]/2
	xmin = self.position[0]-xHalfWidth
	ymin = self.position[1]-yHalfWidth
	xmax = self.position[0]+xHalfWidth
	ymax = self.position[1]+yHalfWidth
	if ( (x < xmin) or (xmax < x) ):
	    return False
	if ( (y < ymin) or (ymax < y) ):
	    return False
	return True

    def select(self,state):
	oldState = self.selected
	self.selected = state

    def isSelected(self):
	return self.selected

    def setIn(self,i):
	self.inCoupling = i

    def addOut(self,o):
	self.outCouplings.append(o)

    def removeOutCoupling(self,c):
	self.outCouplings.remove(c)

    def getInMonomer(self):
	if ( self.inCoupling==None ):
	    return None
	return self.inCoupling.fromMonomer


    def selectIfWithin(self,rect):
	if ( rect.containsPoint(self.position) ):
	    self.selected = True

    def setSelected(self,f):
	self.selected = f

    def lookForErrors(self,topDb,log):
	self.errorReason = ""
	if (not topDb.recognizesSubUnitWithNameOrPdb(self.name) ):
	    log.write("Found an error for name: %s"%self.name)
	    self.inError = True
	    self.errorReason = "Bad subUnit name"
	    log.write("Reason: %s"%self.errorReason)
	    return
	log.write("OK name: %s"%self.name)
	self.inError = False
	cm = CouplingMap()
	if ( self.inCoupling != None ):
	    cm.setIn(self.inCoupling.getName())
	outNames = []
	for x in self.outCouplings:
	    outNames.append(x.getName())
	log.write("outCouplings=%s"%outNames)
	cm.setOut(outNames)	
	mold = topDb.moldForSubUnitWithNameOrPdb(self.name)
	if ( mold.isCouplingMapValid(cm) ):
	    self.inError = False
	else:
	    self.inError = True
	    self.errorReason = "Bad couplings"
	    log.write("Reason: %s"%self.errorReason)


class	GrCoupling:
    def __init__(self,olig,nm=""):
	self.fromMonomer = None
	self.toMonomer = None
	self.oligomer = olig
	self.name = nm 
	self.selected = False

    def setName(self,nm):
	self.name = nm

    def getName(self):
	return self.name

    def getBoxExtent(self):
	return (60,30)

    def getDist(self):
	return 30

    def couple(self,f,t):
	self.fromMonomer = f
	self.toMonomer = t
	f.addOut(self)
	t.setIn(self)

    def isSelected(self):
	return self.selected

    def selectIfWithin(self,rect):
	pos1 = self.fromMonomer.position
	pos2 = self.toMonomer.getCouplingToPosition(pos1)
	pos = ((pos1[0]+pos2[0])/2,(pos1[1]+pos2[1])/2)
	if ( rect.containsPoint(pos) ):
	    self.selected = True

    def render(self, dc ):
	pos1 = self.fromMonomer.position
	pos2 = self.toMonomer.getCouplingToPosition(pos1,dev=dc)
	dc.DrawLine(pos1[0],pos1[1],pos2[0],pos2[1])

	dir = (pos2[0]-pos1[0],pos2[1]-pos1[1])
	midWayOffset = 0.6
	pos = (pos1[0]+dir[0]*midWayOffset,pos1[1]+dir[1]*midWayOffset)
	te = dc.GetTextExtent(self.name)
	extent = (te[0]+20,te[1]+20)
	offsetX = extent[0]/2
	offsetY = extent[1]/2
	if ( self.selected ):
            dc.SetBrush(wx.CYAN_BRUSH)
	else:
	    dc.SetBrush(wx.WHITE_BRUSH)
	dc.DrawEllipse(pos[0]-offsetX,pos[1]-offsetY,extent[0],extent[1])

	dc.SetFont(wx.Font(14, wx.SWISS, wx.NORMAL, wx.NORMAL))
	dc.SetTextForeground(wx.Colour(0,0,0))
	te = dc.GetTextExtent(self.name)
	x = pos[0] - (te[0]/2)
	y = pos[1] - (te[1]/2)
	dc.DrawText(self.name,x,y)

    def pick(self,x3,y3):
	fromPos = self.fromMonomer.position
	toPos = self.toMonomer.getCouplingToPosition(fromPos)
	x1 = fromPos[0]
	y1 = fromPos[1]
	x2 = toPos[0]
	y2 = toPos[1]
	xdiff = x2-x1
	ydiff = y2-y1
	dist2 = xdiff**2+ydiff**2
	dist = sqrt(dist2)
	u = float((x3-x1)*(x2-x1)+(y3-y1)*(y2-y1))/dist2
	sys.stdout.flush()
	if ( u<0 or u>1.0 ):
	    return False
	px = x1+u*(x2-x1)
	py = y1+u*(y2-y1)
	offset = sqrt((x3-px)**2+(y3-py)**2)
	if ( offset < self.getDist() ):
	    return True
	return False

    def setSelected(self,f):
	self.selected = f


class	GrOligomer:
    def __init__(self,l):
	self.topologyDatabase = topDb
	self.log = l
	self.monomers = []
	self.couplings = []
	self.name = ""

    def setTopologyDatabase(self,tdb):
	self.topologyDatabase = tdb

    def setName(self,nm):
	self.name = nm

    def getName(self):
	return self.name


    def root(self):
	m = self.monomers[0]
	while (m.inCoupling != None ):
	    c = m.inCoupling
	    m = c.fromMonomer
 	return m	

    def clearSelection(self):
	for m in self.monomers:
	    m.setSelected(False)
	for c in self.couplings:
	    c.setSelected(False)
	self.signalChanged()

    def removeMonomer(self,mon):
	inc = mon.inCoupling
	if ( inc==None ):
	    return		# never remove the root
	self.monomers.remove(mon)
	parentMon = inc.fromMonomer
	parentMon.removeOutCoupling(inc)
	self.couplings.remove(inc)
	for childCoup in mon.outCouplings:
	    parentMon.addOut(childCoup)
	    childCoup.fromMonomer = parentMon


    def lookForErrors(self):
	for m in self.monomers:
	    m.lookForErrors(self.topologyDatabase,self.log)


    def deleteSelected(self):
	sel = self.selectedMonomers()
	for m in sel:
	    self.removeMonomer(m)
	self.signalChanged()

    def selectedMonomers(self):
	sel = []
	for m in self.monomers:
	    if ( m.isSelected() ):
		sel.append(m)
	return sel


    def mutateAllSelectedTo(self,name):
	for m in self.monomers:
	    if m.isSelected():
		m.setName(name)
	self.signalChanged()

    def setSelectedCouplingsTo(self,name):
	for m in self.couplings:
	    if m.isSelected():
		m.setName(name)
	self.signalChanged()


    def layout(self):
	x = 100
	y = 100
	yinc = 75
	spanningTree = SpanningTree(self)
	verticalRun = spanningTree.longestRun()
	coupe = None
	for index in range(0,len(verticalRun)):
	    if ( index == 0 ):
		prev = None
	    else:
		prev = verticalRun[index-1] 
	    monomer = verticalRun[index]
	    if ( index < len(verticalRun) ):
		next = None
	    else:
		next = verticalRun[index+1]
	    couple = monomer.inCoupling
	    if ( couple != None ):
	        couple.position = (x,y)
	        y = y + yinc
	    monomer.position = (x,y)
	    y = y + yinc
#	    monomer.layoutOutCouplingsToRightExceptTo(next)
	self.signalChanged()


    def addMonomer(self,mon):
	self.monomers.append(mon)

    def addCoupling(self,coup):
	self.couplings.append(coup)

    def addOutFrom( self, fromMon, pos, couplingName, monomerName ):
	toMon = GrMonomer(self,monomerName)
	toCoup = GrCoupling(self,couplingName)
	toMon.position = pos
	self.addMonomer(toMon)
	self.addCoupling(toCoup)
	toCoup.couple(fromMon,toMon)
	self.signalChanged()
	
    def	pick(self,x,y):
	for m in self.monomers:
	    if ( m.pick(x,y) ):
		return m
	for c in self.couplings:
	    if ( c.pick(x,y) ):
		return c

    def render(self,dc):
        dc.SetPen(wx.Pen('BLUE'))
	for x in self.couplings:
	    x.render(dc)
	for m in self.monomers:
	    m.render(dc)


    def renderBoundingBox(self):
	x = self.monomers[0]
	bbox = x.renderBoundingBox()
	for m in self.monomers:
	    bbox.encompass(m.renderBoundingBox())
	bbox.inflate((200,200))
	return bbox

    def signalChanged(self):
	self.log.write("Looking for errors")
	self.lookForErrors()
	send(OLIGOMER_CHANGED,self)

    def selectWithin(self,rect):
	for x in self.couplings:
	    x.selectIfWithin(rect)
	for m in self.monomers:
	    m.selectIfWithin(rect)
	self.signalChanged()


class	Rectangle:
    def __init__(self):
	self.xmin = 0
	self.ymin = 0
	self.xmax = 0
	self.ymax = 0

    def containsPoint(self,pnt):
	if ( self.xmin < pnt[0] and pnt[0] < self.xmax ):
	    if ( self.ymin < pnt[1] and pnt[1] < self.ymax ):
		return True
	return False
	    

    def set(self,x1,y1,x2,y2):
	self.xmin = min(x1,x2)
	self.ymin = min(y1,y2)
	self.xmax = max(x1,x2)
	self.ymax = max(y1,y2)

    def encompass(self,rect):
	if ( rect.xmin < self.xmin ):
	    self.xmin = rect.xmin
	if ( rect.ymin < self.ymin ):
	    self.ymin = rect.ymin
	if ( rect.xmax > self.xmax ):
	    self.xmax = rect.xmax
	if ( rect.ymax > self.ymax ):
	    self.ymax = rect.ymax

    def getOrigin(self):
	return (self.xmin,self.ymin)

    def getWidth(self):
	return abs(self.xmax-self.xmin)

    def getHeight(self):
	return abs(self.ymax-self.ymin)

    def inflate(self,sz):
	self.xmin = self.xmin - sz[0]
	self.xmax = self.xmax + sz[0]
	self.ymin = self.ymin - sz[1]
	self.ymax = self.ymax + sz[1]



#
########################################################################
########################################################################
########################################################################
########################################################################
#
# main
#
#


#
# Load topologyDatabase
#

builderXml = QuickDomNode("topologyPhase01.xml")
builderDb = BuilderDatabase()
builderDb.parseFromXml(builderXml)

topDb = builderDb.getTopologyDatabase();
print "TopologyDatabase = ",topDb
sys.stdout.flush()

couplings = topDb.allCouplingRules()
for x in couplings:
    print "Coupling: ", x


# Create an oligomer

print "About to calculate layout"
sys.stdout.flush()
print "Done with calculate layout"
sys.stdout.flush()

#app = CandoEditorApp(redirect=True,clearSigInt=False)
app = CandoEditorApp(redirect=False,clearSigInt=False)
app.setTopologyDatabase(topDb)
app.newOligomer()
#app.MainLoop()


try:
    app.MainLoop()
except:
    print "CRASH!!!!!"
    sys.stdout.flush()
    time.sleep(2)
    sys.exit(1)

time.sleep(2)

