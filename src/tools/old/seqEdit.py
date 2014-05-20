#! /bin/env python

import copy

import time
import traceback
import sys
import math
from math import *

import  wx

from mbb import *

from dispatcher import *


OLIGOMER_CHANGED = "oligomer_changed"

RUBBER_BAND = "rubberBand"
SELECT = "select"
DRAG = "drag"
DELETE = "delete"
DRAW = "draw"
QUERY = "query"
INSERT_BEFORE = "insertBefore"
INSERT_AFTER = "insertAfter"


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

	self.mainFrame = wx.Frame( None, -1, "CANDO" )
	menuBar = self.createMenuBar()
	self.mainFrame.SetMenuBar(menuBar)
	self.mainFrame.SetPosition((0,0))

	self.partsWindow = PartsWindow(self.mainFrame, "Parts", self )
	self.partsWindow.SetPosition((0,60))

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

    def OnCouplingSelected(self,obj):
	self.appCoupling = obj


    def OnMonomerActivated(self,obj):
	if ( self.appCurrentOligomerEditor != None ):
	    self.appCurrentOligomerEditor.OnMonomerActivated(obj)

    def OnCouplingActivated(self,obj):
	if ( self.appCurrentOligomerEditor != None ):
	    self.appCurrentOligomerEditor.OnCouplingActivated(obj)


    def OnOligomerEditorGotFocus(self,oligomerEditor):
	global GrLog
	GrLog.write("DEBUG:OnOligomerEditorGotFocus -->%s"%oligomerEditor)
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
	menu.Append(ID_SEQUENCE_DUPLICATE_ALL, "&Duplicate", 
				"Duplicate the entire sequence" )
	menu.AppendSeparator()
	menu.Append(ID_SEQUENCE_DUPLICATE_TO_END, "Duplicate to &end", 
				"Duplicate the rest of the sequence" )
	self.sequenceMenu = menu
#	EVT_MENU( self, ID_SEQUENCE_DUPLICATE_ALL,
#				self.menuSequenceDuplicateAll)
#	EVT_MENU( self, ID_SEQUENCE_DUPLICATE_TO_END, 
#				self.menuSequenceDuplicateToEnd)

	menu = wx.Menu()
	menu.Append(ID_VIEW_CENTER_ON_GEOMETRIC_CENTER, "&Center view", 
				"Center view on the geometric center" )
	menu.AppendSeparator()
#	menu.Append(ID_VIEW_AXES_TOGGLE, "Toggle A&xes", 
#				"Toggle the display of the coordinate axes" )
#	menu.Append(ID_VIEW_STRUCTURE_TOGGLE, "Toggle &structure", 
#			"Toggle the display of the bis-peptide structure" )
#	menu.Append(ID_VIEW_BACKBONE_TOGGLE, "Toggle &backbone", 
#			"Toggle the display of the bis-peptide backbone" )
#	menu.Append(ID_VIEW_CENTERS_TOGGLE, "Toggle &centers", 
#			"Toggle the display of the bis-amino acid centers" )
#	menu.Append(ID_VIEW_CHOICES_TOGGLE, "Toggle &choices", 
#			"Toggle the display of the bis-amino acid choices" )
#	menu.Append(ID_VIEW_HIT_DETAILS_TOGGLE, "Toggle &hit details", 
#			"Toggle the display of the hit details" )
#	menu.Append(ID_VIEW_SEARCH_DETAILS_TOGGLE, "Toggle &search details", 
#			"Toggle the display of the search details" )
	menu.AppendSeparator()
	menu.Append(ID_VIEW_STEREO_MONO, "Stereo off", "Turn off stereo" )
	menu.Append(ID_VIEW_STEREO_CROSS_EYED, "Stereo cross-eyed", 
			"Turn on cross-eyed stereo" )
	menu.Append(ID_VIEW_STEREO_WALL_EYED, "Stereo wall-eyed", 
			"Turn on wall-eyed stereo" )
	menu.Append(ID_VIEW_STEREO_QUAD_BUFFERED, "Stereo quad-buffered", 
			"Turn on quad-buffered stereo" )
	self.viewMenu = menu
#	EVT_MENU( self, ID_VIEW_CENTER_ON_GEOMETRIC_CENTER, 
#			self.menuViewCenterOnGeometricCenter)
#	EVT_MENU( self, ID_VIEW_AXES_TOGGLE, self.menuViewAxesToggle)
#	EVT_MENU( self, ID_VIEW_STRUCTURE_TOGGLE, self.menuViewStructureToggle)
#	EVT_MENU( self, ID_VIEW_BACKBONE_TOGGLE, self.menuViewBackboneToggle)
#	EVT_MENU( self, ID_VIEW_HIT_DETAILS_TOGGLE, 
#			self.menuViewHitDetailsToggle)
#	EVT_MENU( self, ID_VIEW_SEARCH_DETAILS_TOGGLE, 
#			self.menuViewSearchDetailsToggle)
#	EVT_MENU( self, ID_VIEW_CENTERS_TOGGLE, self.menuViewCentersToggle)
#	EVT_MENU( self, ID_VIEW_CHOICES_TOGGLE, self.menuViewChoicesToggle)
#	EVT_MENU( self, ID_VIEW_STEREO_MONO, self.menuViewStereoMono)
#	EVT_MENU( self, ID_VIEW_STEREO_CROSS_EYED, self.menuViewStereoCrossEyed)
#	EVT_MENU( self, ID_VIEW_STEREO_WALL_EYED, self.menuViewStereoWallEyed)
#	EVT_MENU( self, ID_VIEW_STEREO_QUAD_BUFFERED, 
#			self.menuViewStereoQuadBuffered)


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
	global GrLog
	# Create an oligomer
	oligManager = OligomerManager(self,self.topologyDatabase)
	oligManager.newOligomer()
	oligManager.setName("unnamed-%d"%self.unnamed)
	oligManager.cmdLayout()
	oligManager.resetUndo()
	self.unnamed = self.unnamed+1
	oligEditor = OligomerEditorPanel(self,self.mainFrame)
	oligEditor.setOligomerManager(oligManager)
	self.oligomerEditors.append(oligEditor)
	self.appCurrentOligomerEditor = oligEditor


class OligomerEditorPanel(wx.Frame):

    drawMode = "draw"
    selectMode = "select"

    def __init__(self, application, parent ):
	wx.Frame.__init__(self,parent,-1,"",wx.DefaultPosition,(800,600))

	self.app = application
	self.oligomerManager = None
	self.name = ""

	self.toolBar = self.CreateToolBar( wx.TB_HORIZONTAL 
				| wx.NO_BORDER 
				| wx.TB_FLAT 
				| wx.TB_TEXT )
#	buttonId = wx.NewId()
#	button = wx.Button(self.toolBar,buttonId,"Draw")
#	self.toolBar.AddControl(button)
#	button.Bind(wx.EVT_BUTTON, self.DrawOn)

#	buttonId = wx.NewId()
#	button = wx.Button(self.toolBar,buttonId,"Select")
#	self.toolBar.AddControl(button)
#	button.Bind(wx.EVT_BUTTON, self.SelectOn)

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
	self.canvas = OligomerLayoutCanvas(self.sequenceSash,self.app)

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
#	self.canvas.SelectOn(None)

    def OnMonomerActivated(self,obj):
	self.oligomerManager.cmdMutateAllSelectedMonomersTo(obj)

    def OnCouplingActivated(self,obj):
	self.oligomerManager.cmdMutateAllSelectedCouplingsTo(obj)

    def OnScaleCombo(self,event):
	sc = float(event.GetString())/100.0
	if ( sc < 0.05 ):
	    sc = 0.05
	if ( sc > 2.0 ):
	    sc = 2.0
	self.canvas.setScale(sc)

    def OnFocus(self,event):
	global GrLog
	self.app.OnOligomerEditorGotFocus(self)

    def OnActivate(self,event):
	global GrLog
	active = event.GetActive()
	if ( active ):
	    GrLog.write("DEBUG:OnActivate got activated: %s"%self.name)
	else:
	    GrLog.write("DEBUG:OnActivate got UN-activated: %s"%self.name)


    def SelectOn(self,event):
	self.canvas.SelectOn(event)

    def DrawOn(self,event):
	self.canvas.DrawOn(event)

    def OnSashDrag(self, event):
	global GrLog
        if event.GetDragStatus() == wx.SASH_STATUS_OUT_OF_RANGE:
            return
        eobj = event.GetEventObject()

        if eobj is self.sequenceSash:
            self.sequenceSash.SetDefaultSize((event.GetDragRect().width, 
						1000))
        wx.LayoutAlgorithm().LayoutWindow(self, self.structurePanel)
#        self.structurePanel.myRefresh()

    def OnSize(self, event):
	global GrLog
        wx.LayoutAlgorithm().LayoutWindow(self, self.structurePanel )
	pass


    def setOligomerManager(self, om):
	self.oligomerManager = om
	self.canvas.setOligomerManager(om)
	self.name = om.getName()
	self.SetTitle(self.name)

    def getOligomerManager(self):
	return self.oligomerManager


#
#
# ToolboxWindow
#
# Allows selection of monomers and couplings
#
#

class PartsWindow(wx.MiniFrame):
    def __init__( self, parent, title, app,  pos=wx.DefaultPosition, 
			size=(400,400),
			style=wx.THICK_FRAME|wx.CAPTION ):
	wx.MiniFrame.__init__(self,parent,-1,title,pos,size,style)
	self.app = app
	self.lastSelected = None
	self.topologyDatabase = None
#	panel = wx.Panel(self,-1)

	winids = []
	monomerWin = wx.SashLayoutWindow(self,-1,wx.DefaultPosition,(300,200),
			wx.NO_BORDER|wx.SW_3D )
	monomerWin.SetDefaultSize((200,10000))
	monomerWin.SetOrientation(wx.LAYOUT_VERTICAL)
	monomerWin.SetAlignment(wx.LAYOUT_LEFT)
	monomerWin.SetBackgroundColour(wx.Colour(255,0,0))
	monomerWin.SetSashVisible(wx.SASH_RIGHT,True)
	self.monomerWin = monomerWin
	winids.append(monomerWin.GetId())

	self.couplingWin = wx.Panel(self,-1,style=wx.SUNKEN_BORDER )
	siz = wx.BoxSizer( )
	self.couplingWin.SetSizer(siz)
	self.Bind(wx.EVT_SASH_DRAGGED_RANGE, self.OnSashDrag,
			id=min(winids), id2=max(winids) )

	self.Bind(wx.EVT_SIZE,self.OnSize)


	self.monomerTree = wx.TreeCtrl(self.monomerWin)
	self.monomerTree.Bind(wx.EVT_TREE_SEL_CHANGED, self.OnMonomerSelectionChanged )
	self.monomerTree.Bind(wx.EVT_TREE_ITEM_ACTIVATED, self.OnMonomerSelectionActivated)

	self.couplingTree = wx.TreeCtrl(self.couplingWin)
	self.couplingTree.Bind(wx.EVT_TREE_SEL_CHANGED, self.OnCouplingSelectionChanged )
	self.couplingTree.Bind(wx.EVT_TREE_ITEM_ACTIVATED, self.OnCouplingSelectionActivated)

	siz.Add(self.couplingTree,wx.ALL,wx.EXPAND)

	self.Bind(wx.EVT_CLOSE, self.OnCloseWindow )
	self.Show()

    def OnSize(self,event):
	wx.LayoutAlgorithm().LayoutWindow(self,self.couplingWin)

    def OnSashDrag(self,event):
	global	GrLog
	if ( event.GetDragStatus() == wx.SASH_STATUS_OUT_OF_RANGE ):
	    return
	eobj = event.GetEventObject()
	if ( eobj is self.monomerWin ):
	    self.monomerWin.SetDefaultSize((event.GetDragRect().width,10000))
	wx.LayoutAlgorithm().LayoutWindow(self,self.couplingWin)
	self.remainingSpace.Refresh()

	
    def OnMonomerSelectionChanged(self,event):
	tree = self.monomerTree
	item = event.GetItem()
	obj = self.monomerTree.GetItemText(item)	
	self.app.OnMonomerSelected(obj)

    def OnMonomerSelectionActivated(self,event):
	tree = self.monomerTree
	item = event.GetItem()
	obj = self.monomerTree.GetItemText(item)	
	self.app.OnMonomerActivated(obj)


    def OnCouplingSelectionChanged(self,event):
	tree = self.couplingTree
	item = event.GetItem()
	obj = self.couplingTree.GetItemText(item)	
	self.app.OnCouplingSelected(obj)

    def OnCouplingSelectionActivated(self,event):
	tree = self.couplingTree
	item = event.GetItem()
	obj = self.couplingTree.GetItemText(item)	
	self.app.OnCouplingActivated(obj)


    def setTopologyDatabase(self,top):
	self.fillMonomerSelector(top)
	self.fillCouplingSelector(top)


    def fillMonomerSelector(self,topDb):
	if ( self.monomerTree == None ):
	    return
	tree = self.monomerTree
	root = tree.AddRoot("Monomers")
	tree.SetPyData( root, None )
	layoutXml = topDb.getMonomerLayout()
	children = layoutXml.getChildren()
	for child in children:
	    self.fillBranch(tree,root,child)
	tree.Expand(root)

    def fillBranch(self, tree, parent, xml ):
	if ( xml.getLocalName() == "branch" ):
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

    def fillCouplingSelector(self,topDb):
	if ( self.couplingTree == None ):
	    return
	tree = self.couplingTree
	root = tree.AddRoot("Coupling")
	tree.SetPyData( root, None )
	branch = tree.AppendItem(root,"branch")
	tree.SetPyData( branch, "branch" )
	couplings = topDb.allCouplingRules()
	for x in couplings:
	    leaf = tree.AppendItem(branch,x)
	    tree.SetPyData( leaf, x )
	tree.Expand(branch)
	ringBranch = tree.AppendItem(root,"ring")
	tree.SetPyData( ringBranch, "ring" )
	rings = []
	for x in rings:
	    leaf = tree.AppendItem(ringBranch,x)
	    tree.SetPyData( leaf, x )
	tree.Expand(ringBranch)
	tree.Expand(root)
	


    def OnCloseWindow(self):
	pass
	





class OligomerLayoutCanvas(wx.ScrolledWindow):

    def __init__(self, parent, appl, id = -1, size = wx.DefaultSize):
        wx.ScrolledWindow.__init__(self, parent, id, (0, 0), size=size, style=wx.SUNKEN_BORDER|wx.CLIP_CHILDREN)
	self.app = appl
        self.drawing = False
	self.mode = None
	self.xyStart = None
	self.xyStop = None
	self.scale = 0.5
	self.backgroundFlash = False
	self.bitmap = None
        self.SetBackgroundColour("WHITE")
#        self.SetCursor(wx.StockCursor(wx.CURSOR_PENCIL))

        self.SetScrollRate(20,20)
        self.Bind(wx.EVT_LEFT_DOWN, self.OnLeftButtonEvent)
        self.Bind(wx.EVT_LEFT_UP,   self.OnLeftButtonEvent)
        self.Bind(wx.EVT_MOTION,    self.OnLeftButtonEvent)
        self.Bind(wx.EVT_KEY_DOWN,    self.OnKeyDown)
        self.Bind(wx.EVT_PAINT, self.OnPaint)
	self.Bind(wx.EVT_SIZE, self.OnSize )
	self.Bind(wx.EVT_RIGHT_UP, self.OnRightClick)
	self.createCouplingMenu()
	connect(self,OLIGOMER_CHANGED)


    def __call__(self,**args):
	if ( args['signal'] == OLIGOMER_CHANGED ):
	    self.myRefresh()
	return

    def OnSize(self,event):
	self.bitmap = None
	
    def myRefresh(self):
	self.Refresh(eraseBackground=False)


    def setScale(self,s):
	self.scale = s
	self.myRefresh()
    
    def setOligomerManager(self,om):
	self.oligomerManager = om


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
	global GrLog
	nid = event.GetId()
	name = self.couplingNames[nid]
	self.oligomerManager.setSelectedCouplingsTo(name)
	self.app.setDefaultCouplingName(name)

    def OnPaint(self, event):
	self.DoDrawing()


    def DoDrawing(self, printing=False):
	extents = self.oligomerManager.renderBoundingBox()
        self.SetVirtualSize((extents.getWidth(),extents.getHeight()))
	if ( self.bitmap == None ):
	    width,height = self.GetClientSize()
	    self.bitmap = wx.EmptyBitmap(width,height)
        dc = wx.BufferedPaintDC(self,self.bitmap,style=wx.BUFFER_CLIENT_AREA)
#        dc = wx.PaintDC(self)
#	self.PrepareDC(dc)
	vsx,vsy = self.GetViewStart()
	vpx,vpy = self.GetScrollPixelsPerUnit()
	x = vsx*vpx
	y = vsy*vpy
	if ( self.backgroundFlash ):
	    dc.SetBackground(wx.Brush('WHITE'))
	else:
	    dc.SetBackground(wx.Brush('WHITE')) # GRAY
	self.backgroundFlash = not self.backgroundFlash
	dc.Clear()
	dc.SetDeviceOrigin(-x,-y)
	dc.SetUserScale(self.scale,self.scale)
	self.oligomerManager.render(dc)
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
	xStart,yStart = self.xyStart
	xStop,yStop = self.xyStop
	xWidth = xStop-xStart
	yWidth = yStop-yStart
	dc.DrawRectangle(xStart,yStart,xWidth,yWidth)
	dc.EndDrawing()
	
    def drawBranch(self):
	dc = wx.ClientDC(self)
	self.PrepareDC(dc)
	dc.BeginDrawing()
	dc.SetUserScale(self.scale,self.scale)
	dc.SetPen(wx.Pen('WHITE', 4))
	dc.SetLogicalFunction(wx.INVERT)
	xStart,yStart = self.xyStart
	xStop,yStop = self.xyStop
	dc.DrawLine(xStart,yStart,xStop,yStop)
	dc.EndDrawing()


    def OnKeyDown(self,event):
	global	GrLog
	if ( event.GetKeyCode() == wx.WXK_BACK ):
	    self.oligomerManager.undo()
	if ( event.GetKeyCode() == wx.WXK_TAB ):
	    self.oligomerManager.redo()




    def OnLeftButtonEvent(self, event):
	global	GrLog
	xy = self.ConvertEventCoords(event)
	if ( self.mode == None ):
	    self.oligomerManager.checkMouseOver(xy)
	if (event.LeftDown()):
	    self.SetFocus()
			#
			# Here handle scrolling
			#

			#
			# Now figure out if something was picked
			#
	    objCmd = self.oligomerManager.pick(xy)
	    if ( objCmd != None ):
	        obj = objCmd[0]
	        cmd = objCmd[1]

			#
			# If no objCmdect was picked then we are 
			# rubber band selecting
			#
	    if ( objCmd == None ):
		self.mode = RUBBER_BAND
		self.SetFocus()
		self.xyStart = xy
		self.xyStop = xy
	        if ( not event.ShiftDown() ):
		    self.oligomerManager.cmdClearSelection()
		self.CaptureMouse()
		self.drawRubberBand()
	
		
		#
		# If the user clicked the background then we are
		# starting to select with a rubber band
		#
		self.myRefresh()
	    else:
		if ( cmd == SELECT ):
		    if ( not event.ShiftDown() ):
		        self.oligomerManager.cmdClearSelectionThenSelectOnePart(obj)
		    else:
			self.oligomerManager.cmdToggleSelectionOnPart(obj)
		elif ( cmd == DRAG ):
		    self.mode = DRAG
		    self.xyStart = xy
		    self.CaptureMouse()
		elif ( cmd == DRAW ):
		    self.mode = DRAW
		    self.objectStart = obj
		    self.xyStart = obj.getPosition()
		    self.xyStop = xy
		    self.CaptureMouse()
		    self.drawBranch()
		elif ( cmd == DELETE ):
		    self.oligomerManager.cmdDeleteMonomer(obj)
		elif ( cmd == QUERY ):
#		    self.oligomerManager.cmdQueryMonomer(obj)
		    win = QueryPopup(self,obj,wx.SIMPLE_BORDER)
            	    dPos = event.GetEventObject().ClientToScreen(event.GetPosition())
		    win.SetPosition(dPos)
		    win.Show(True)
		elif ( cmd == INSERT_BEFORE ):
		    self.oligomerManager.cmdInsertBefore(obj)
		else:
		    GrLog.write("Unknown instruction: %s"%cmd)

	elif event.Dragging():
	    if ( self.mode == RUBBER_BAND ):
		self.drawRubberBand()
		self.xyStop = self.ConvertEventCoords(event)
		self.drawRubberBand()
	    elif ( self.mode == DRAG ):
		self.xyStop = self.ConvertEventCoords(event)
		xyDiff = (self.xyStop[0]-self.xyStart[0],
				self.xyStop[1]-self.xyStart[1])
		self.oligomerManager.temporarilyDragSelectedBy(xyDiff)
	    elif ( self.mode == DRAW ):
		self.drawBranch()
		self.xyStop = self.ConvertEventCoords(event)
		self.drawBranch()


	elif event.LeftUp():
	    if ( self.mode == RUBBER_BAND ):
		self.ReleaseMouse()
		self.xyStop = self.ConvertEventCoords(event)
		self.drawRubberBand()
		self.mode = None
			# do rectangle selection stuff
		rect = Rectangle()
		xStart,yStart = self.xyStart
		xStop,yStop = self.xyStop
		rect.set(xStart, yStart, xStop, yStop )
		self.oligomerManager.cmdSelectUnselectedWithinRectangle(rect)
	    elif ( self.mode == DRAG ):
		self.ReleaseMouse()
		self.xyStop = self.ConvertEventCoords(event)
		xyDiff = (self.xyStop[0]-self.xyStart[0],
				self.xyStop[1]-self.xyStart[1])
		self.mode = None
		self.oligomerManager.cmdDragSelectedBy(xyDiff)
	    elif ( self.mode == DRAW ):
		self.drawBranch()
		self.ReleaseMouse()
		self.xyStop = self.ConvertEventCoords(event)
			# Draw stuff here
		self.mode = None
		self.oligomerManager.cmdAddMonomer(self.objectStart,self.xyStop) #meister
		


#	if ( self.mode == OligomerEditorPanel.selectMode ):
#            if (event.LeftDown()):
#		if ( not event.ShiftDown() ):
#		    self.oligomerManager.clearSelection()
#		x,y = self.ConvertEventCoords(event)
#		obj = self.oligomerManager.pick(x,y)
#		self.selectedObject = obj
#		self.rubberBand = False
#		if ( obj == None ):
#		    self.rubberBand = True
#		    self.SetFocus()
#		    self.xStart, self.yStart = (x,y)
#		    self.xStop, self.yStop = (x,y)
#		    self.CaptureMouse()
#		    self.drawRubberBand()
#		else:
#		    self.selectedObject.setSelected(True)
##		    self.selectedObject.setPosition(x,y)
#		    self.myRefresh()
#	    elif event.Dragging():
#		if ( self.rubberBand ):
#		    self.drawRubberBand()
#		    self.xStop, self.yStop = self.ConvertEventCoords(event)
#		    self.drawRubberBand()
#		else:
#		    x,y = self.ConvertEventCoords(event)
#		    self.selectedObject.setPosition(x,y)
#		    self.myRefresh()
#	    elif event.LeftUp():
#		if ( self.rubberBand ):
#		    self.ReleaseMouse()
#		    self.xStop, self.yStop = self.ConvertEventCoords(event)
#		    self.drawRubberBand()
#		    self.rubberBand = False
#			    # do rectangle selection stuff
#		    rect = Rectangle()
#		    rect.set(self.xStart, self.yStart, self.xStop, self.yStop )
#		    self.oligomerManager.selectWithin(rect)
#
#		else:
#		    x,y = self.ConvertEventCoords(event)
##		    self.selectedObject.setPosition(x,y)
#		    self.myRefresh()
#		    
#		    
#	elif ( self.mode == OligomerEditorPanel.drawMode ):
#            if event.LeftDown():
#		self.oligomerManager.clearSelection()
#		x,y = self.ConvertEventCoords(event)
#		obj = self.oligomerManager.pick(xy)
#		self.drawingFrom = obj
#		if ( obj != None ):
#		    if ( isinstance(obj,GrMonomer) ):
#			x,y = obj.getPosition()
#			self.xStart = x
#			self.yStart = y
#			self.xStop = x
#			self.yStop = y
#			self.drawing = True
#			self.SetFocus()
#			self.CaptureMouse()
#			self.drawBranch()
#	    elif event.Dragging():
#		if ( self.drawing ):
#		    self.drawBranch()
#		    self.xStop, self.yStop = self.ConvertEventCoords(event)
#		    self.drawBranch()
#	    elif event.LeftUp():
#		if ( self.drawing ):
#		    self.ReleaseMouse()
#		    self.xStop, self.yStop = self.ConvertEventCoords(event)
#		    self.drawBranch()
#		    self.drawing = False
#			    # do drawing stuff
#		    self.oligomerManager.addOutFrom(self.drawingFrom,(self.xStop,self.yStop),self.app.getCurrentCoupling(),self.app.getCurrentMonomer())
#	

    def	OnRightClick(self,event):
        pos = (self.x, self.y) + self.ConvertEventCoords(event)
	print "Right click event at: ", pos
	obj = self.oligomerManager.pick((pos[2],pos[3]))
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
	self.oligomerManager.layout()

    def	OnDeleteSelected(self,event):
	self.oligomerManager.deleteSelected()



class	QueryPopup(wx.MiniFrame):
    def __init__(self,parent,obj,style):
	wx.MiniFrame.__init__(self,parent,style)
	self.SetBackgroundColour("WHEAT")
	st = wx.StaticText(self,-1,"Hi there",pos=(10,10))
	st.SetLabel(obj.getDescription())
	sz = st.GetBestSize()
	self.SetSize( (sz.width+20,sz.height+100) )
        st.Bind(wx.EVT_LEFT_DOWN, self.OnMouseLeftDown)
        st.Bind(wx.EVT_MOTION, self.OnMouseMotion)
        st.Bind(wx.EVT_LEFT_UP, self.OnMouseLeftUp)
        st.Bind(wx.EVT_RIGHT_UP, self.OnRightUp)
        wx.CallAfter(self.Refresh)

    def OnMouseLeftDown(self, evt):
        self.Refresh()
        self.ldPos = evt.GetEventObject().ClientToScreen(evt.GetPosition())
        self.wPos = self.ClientToScreen((0,0))
        self.CaptureMouse()

    def OnMouseMotion(self, evt):
        if evt.Dragging() and evt.LeftIsDown():
            dPos = evt.GetEventObject().ClientToScreen(evt.GetPosition())
            nPos = (self.wPos.x + (dPos.x - self.ldPos.x),
                    self.wPos.y + (dPos.y - self.ldPos.y))
            self.Move(nPos)

    def OnMouseLeftUp(self, evt):
        self.ReleaseMouse()

    def OnRightUp(self, evt):
        self.Show(False)
        self.Destroy()





#---------------------------------------------------------------------------



class	Rectangle:
    def __init__(self):
	self.xmin = 0
	self.ymin = 0
	self.xmax = 0
	self.ymax = 0

    def setCenterExtent(self,center,extent):
	self.xmin = center[0]-extent[0]/2
	self.xmax = center[0]+extent[0]/2
	self.ymin = center[1]-extent[1]/2
	self.ymax = center[1]+extent[1]/2

    def set(self,xmn,ymn,xmx,ymx):
	self.xmin = min(xmn,xmx)
	self.ymin = min(ymn,ymx)
	self.xmax = max(xmn,xmx)
	self.ymax = max(ymn,ymx)

    def centerOn(self,pos):
	self.xmin = self.xmin + pos[0]
	self.xmax = self.xmax + pos[0]
	self.ymin = self.ymin + pos[1]
	self.ymax = self.ymax + pos[1]

    def containsPoint(self,pnt):
	if ( self.xmin < pnt[0] and pnt[0] < self.xmax ):
	    if ( self.ymin < pnt[1] and pnt[1] < self.ymax ):
		return True
	return False
	    

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






class	SpanningTree:
    def __init__(self,olig,rt):
	sys.stdout.flush()
	self.oligomer = olig
	self.spanningList = [ rt ]
	spanIndex = 0
	while ( spanIndex < len(self.spanningList) ):
	    sys.stdout.flush()
	    spanNext = self.spanningList[spanIndex]
	    spanIndex = spanIndex + 1
	    outCouplings = spanNext.outCouplings
	    for i in outCouplings:
		outMon = i.getToMonomer()
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

    def allMonomers(self):
	return self.spanningList

class	GrButton:
    def __init__(self,offset,extent,cmd):
	self.centerOffset = offset
	self.extent = extent
	self.cmd = cmd

    def render(self,pos,dc):
	GrLog.write("GrButton subclass must define render")

    def pick(self,pos,xy):
	GrLog.write("GrButton subclass must define pick")

    def rectangle(self):
	r = Rectangle()
	r.setCenterExtent(self.centerOffset,self.extent)
	return r

class	GrCircleButton(GrButton):
    def __init__(self,offset,radius,cmd):
	GrButton.__init__(self,offset,(radius*2,radius*2),cmd)
	self.offset = offset
	self.radius = radius
	self.topLeftX = offset[0]-radius
	self.topLeftY = offset[1]-radius
	self.bottomRightX = offset[0]+radius
	self.bottomRightY = offset[1]+radius

    def render(self,dc,pos,brush):
	dc.SetBrush(brush)
	dc.DrawCircle(self.offset[0]+pos[0], self.offset[1]+pos[1], 
				self.radius )

    def pick(self,pos,xy):
	x,y = xy
	minX = self.topLeftX+pos[0]
	minY = self.topLeftY+pos[1]
	maxX = self.bottomRightX+pos[0]
	maxY = self.bottomRightY+pos[1]
	if ( x < minX ):
	    return None
	if ( x > maxX ):
	    return None
	if ( y < minY ):
	    return None
	if ( y > maxY ):
	    return None
	return self.cmd

class	GrColoredEllipseButton(GrButton):
    def __init__(self,offset,extent,cmd):
	GrButton.__init__(self,offset,extent,cmd)
	self.offset = offset
	self.extent = extent
	self.topLeftX = offset[0]-extent[0]/2
	self.topLeftY = offset[1]-extent[1]/2
	self.bottomRightX = offset[0]+extent[0]/2
	self.bottomRightY = offset[1]+extent[1]/2

    def render(self,dc,pos,brush):
	dc.SetBrush(brush)
	dc.DrawEllipse(self.topLeftX+pos[0], self.topLeftY+pos[1], 
				self.extent[0],self.extent[1])

    def pick(self,pos,xy):
	x,y = xy
	minX = self.topLeftX+pos[0]
	minY = self.topLeftY+pos[1]
	maxX = self.bottomRightX+pos[0]
	maxY = self.bottomRightY+pos[1]
	if ( x < minX ):
	    return None
	if ( x > maxX ):
	    return None
	if ( y < minY ):
	    return None
	if ( y > maxY ):
	    return None
	return self.cmd



class	GrRectangleButton(GrButton):
    def __init__(self,offset,extent,cmd):
	GrButton.__init__(self,offset,extent,cmd)
	self.topLeftX = offset[0]-extent[0]/2
	self.topLeftY = offset[1]-extent[1]/2
	self.bottomRightX = offset[0]+extent[0]/2
	self.bottomRightY = offset[1]+extent[1]/2

    def render(self,dc,pos):
	dc.DrawRoundedRectangle(self.topLeftX+pos[0], self.topLeftY+pos[1],
				self.extent[0],self.extent[1],10)

    def pick(self,pos,xy):
	x,y = xy
	minX = self.topLeftX+pos[0]
	minY = self.topLeftY+pos[1]
	maxX = self.bottomRightX+pos[0]
	maxY = self.bottomRightY+pos[1]
	if ( x < minX ):
	    return None
	if ( x > maxX ):
	    return None
	if ( y < minY ):
	    return None
	if ( y > maxY ):
	    return None
	return self.cmd

class	GrColoredRectangleButton(GrRectangleButton):
    def __init__(self,offset,extent,cmd):
	GrRectangleButton.__init__(self,offset,extent,cmd)

    def render(self,dc,pos,brush):
	dc.SetBrush(brush)
	GrRectangleButton.render(self,dc,pos)


class	GrMonomerButton(GrRectangleButton):
    def __init__(self,offset,extent,cmd):
	GrRectangleButton.__init__(self,offset,extent,cmd)
	errorBorder = 10
	self.errorTopLeftX = self.topLeftX-errorBorder
	self.errorTopLeftY = self.topLeftY-errorBorder
	self.errorExtent = (self.extent[0]+errorBorder*2,self.extent[1]+errorBorder*2)

    def render(self,dc,pos,incoming,inError,isSelected,name):
	if ( inError ):
	    dc.SetBrush(wx.Brush('RED',wx.SOLID))
	    dc.DrawRoundedRectangle(self.errorTopLeftX+pos[0], 
				    self.errorTopLeftY+pos[1],
				    self.errorExtent[0],
				    self.errorExtent[1],10)
	if ( incoming != None ):
	    dc.SetBrush(wx.BLUE_BRUSH)
	    dc.DrawCircle(incoming[0],incoming[1],10)
	if ( isSelected ):
	    dc.SetBrush(wx.CYAN_BRUSH)
	else:
	    dc.SetBrush(wx.WHITE_BRUSH)
	GrRectangleButton.render(self,dc,pos)
	te = dc.GetTextExtent(name)
#	dc.SetFont(wx.Font(14, wx.SWISS, wx.NORMAL, wx.NORMAL))
	dc.SetTextForeground(wx.Colour(0,0,0))
	x = pos[0]-te[0]/2
	y = pos[1]-te[1]/2
	dc.DrawText(name,x,y)

	
class	GrCouplingButton(GrColoredEllipseButton):
    def __init__(self,offset,extent,cmd):
	GrColoredEllipseButton.__init__(self,offset,extent,cmd)
	errWid = 14
	self.errorTopLeftX = self.topLeftX-errWid
	self.errorTopLeftY = self.topLeftY-errWid
	self.errorExtent = (self.extent[0]+errWid*2,self.extent[1]+errWid*2)

    def render(self,dc,pos,inError,isSelected,name):
	if ( inError ):
	    dc.SetBrush(wx.Brush('RED',wx.SOLID))
	    dc.DrawEllipse(self.errorTopLeftX+pos[0], 
				    self.errorTopLeftY+pos[1],
				    self.errorExtent[0],
				    self.errorExtent[1])
	if ( isSelected ):
	    brush = wx.CYAN_BRUSH
	else:
	    brush = wx.WHITE_BRUSH
	GrColoredEllipseButton.render(self,dc,pos,brush)
	te = dc.GetTextExtent(name)
#	dc.SetFont(wx.Font(14, wx.SWISS, wx.NORMAL, wx.NORMAL))
	dc.SetTextForeground(wx.Colour(0,0,0))
	x = pos[0]-te[0]/2
	y = pos[1]-te[1]/2
	dc.DrawText(name,x,y)


class	GrMonomer:
    selectButton = GrMonomerButton((0,0),(140,40),SELECT)
    drawButton = GrColoredRectangleButton((0,30),(140,20),DRAW)
    deleteButton = GrCircleButton((60,-34),14,DELETE)
    queryButton = GrCircleButton((-60,-34),14,QUERY)
    border = selectButton.rectangle()
    border.encompass(drawButton.rectangle())
    border.encompass(deleteButton.rectangle())
    border.encompass(queryButton.rectangle())

    def __init__(self,oligMan,nm="Undef",root=False):
		#
		# State variables
		#
	self.oligomerManager = oligMan
	self.isRoot = root
	self.name = nm
	self.inCoupling = None
	self.outCouplings = []
	self.mySelected = False
	self.myPosition = (0,0)
	self.reset_temp()

		#
		# Temporary state variables
		#	If these are None then they must be calculated
		#	when they are requested
		#
    def reset_temp(self):
	self.temp_spanningIndex = None
	self.temp_spanningBack = None
	self.temp_spanningNext = None
	self.temp_position = None
	self.temp_inError = None
	self.temp_errorReason = None
	self.mouseOver = False

    def _set_selected(self,s):
	self.mySelected = s

    def getSelected(self):
	return self.mySelected

    def _set_position(self,p):
	self.myPosition = p

    def _translateBy(self,o):
	xy = self.myPosition
	xy = ( xy[0]+o[0],xy[1]+o[1])
	self.myPosition = xy

    def getPosition(self):
	return self.myPosition

    def setTempPosition(self,p):
	self.temp_position = p

    def getTempPosition(self):
	if ( self.temp_position==None ):
	    return self.myPosition
	return self.temp_position

    def _set_name(self,nm):
	self.name = nm

    def getName(self):
	return self.name

    def _setIn(self,i):
	self.inCoupling = i

    def getIn(self):
	return self.inCoupling

    def _addOut(self,o):
	self.outCouplings.append(o)

    def getOut(self):
	return self.outCouplings

    def _removeOut(self,c):
	self.outCouplings.remove(c)

    def getInMonomer(self):
	if ( self.inCoupling==None ):
	    return None
	fm = self.inCoupling.get_fromMonomer()
	return self.inCoupling.get_fromMonomer()

    def getDescription(self):
	str = "Name: %s\n"%(self.getName())
	if ( self.temp_inError ):
	    str = str + "ERROR: %s\n"%self.temp_errorReason
	for x in self.outCouplings:
	    str = str + "out coupling: %s\n"%x.getName()
	return str

	
    def dump(self,log):
	log.write(" monomer name=%s"%self.name)
	inc = self.inCoupling
	if ( inc==None ):
	    log.write("    no inCoupling")
	else:
	    log.write("   inCoupling name: %s"%inc.getName())
	    f = inc.getFromMonomer()
	    if ( f==None):
	        log.write( "    inCoupling has no from monomer" )
	    else:
	        log.write("   inCoupling from monomer name: %s"%f.getName())
	for m in self.outCouplings:
	    if ( m==None ):
	        log.write("    no inCoupling")
	    else:
	        log.write(" outCoupling name: %s"%m.getName())
		t = m.getToMonomer()
	        if ( t==None):
	            log.write( "    outCoupling has no to monomer" )
	        else:
	            log.write(" outCoupling to monomer name: %s"%t.getName())

    def getCouplingToPosition(self,posFrom):
	pos = self.getTempPosition()
	diff = (pos[0]-posFrom[0],pos[1]-posFrom[1])
	ext = self.oligomerManager.getMonomerExtent()
	halfX = ext[0]/2
	halfY = ext[1]/2
	xdiff = diff[0]
	ydiff = diff[1]
#	print "xdiff,ydiff=",xdiff," ",ydiff
#	sys.stdout.flush()
	if ( ydiff >= 0 ):
	    if ( abs(xdiff) <= ydiff ):
	        return (pos[0],pos[1]-halfY)
	    if ( (xdiff) > ydiff ):
	        return (pos[0]-halfX,pos[1])
	    return (pos[0]+halfX,pos[1])
	# ydiff < 0
	if ( abs(xdiff) <= abs(ydiff) ):
	    return (pos[0],pos[1]+halfY)
	if ( (xdiff) > abs(ydiff) ):
	    return (pos[0]-halfX,pos[1])
	return (pos[0]+halfX,pos[1])


    def render(self,dc):
	center = self.getTempPosition()
		#
		# if we have an in-coupling then draw a circle there
		#
	incoming = None
	if ( self.inCoupling != None ):
	    inMonomer = self.getInMonomer()
	    fromPos = inMonomer.getTempPosition()
	    incoming = self.getCouplingToPosition(fromPos)
	GrMonomer.selectButton.render(dc,center,incoming,self.temp_inError,
			self.getSelected(),self.getName())

	if ( self.mouseOver ):
		    #
		    # If its not the root monomer draw a close box
		    #
	    if ( not self.isRoot ):
		GrMonomer.deleteButton.render(dc,center,wx.RED_BRUSH)
		    #
		    # Now draw the new button
		    #
	    GrMonomer.drawButton.render(dc,center,wx.GREEN_BRUSH)
		    #
		    # Now draw the query button
		    #
	    GrMonomer.queryButton.render(dc,center,wx.BLUE_BRUSH)


    def getDeleteQueryOffset(self):
	return 10	
	

    def renderBoundingBox(self):
	pos = self.getTempPosition()
	ext = self.oligomerManager.getMonomerExtent()
	if ( ext == None ):
	    rect = Rectangle()
	    rect.set(pos[0],pos[1],pos[0],pos[1])
	    return rect
	offsetX = ext[0]/2
	offsetY = ext[1]/2
	rect = Rectangle()
	rect.set(pos[0]-offsetX,pos[1]-offsetY,pos[0]+offsetX,pos[1]+offsetY)
	return rect

    def pick(self,xy):
	pos = self.getTempPosition()
	bbox = copy.copy(GrMonomer.border)
	bbox.centerOn(pos)
	if ( not bbox.containsPoint(xy) ):
	    return None
	if ( not self.isRoot ):
    	    pick = GrMonomer.queryButton.pick(pos,xy)
	    if ( pick != None ):
		return (self,pick)
	pick = GrMonomer.deleteButton.pick(pos,xy)
	if ( pick != None ):
	    return (self,pick)
	pick = GrMonomer.selectButton.pick(pos,xy)
	if ( pick != None ):
	    if ( self.getSelected() ):
		return (self,DRAG)
	    return (self,pick)
	pick = GrMonomer.drawButton.pick(pos,xy)
	if ( pick != None ):
	    return (self,pick)
	return None

    def inRectangle(self,rect):
	if ( rect.containsPoint(self.getTempPosition()) ):
	    return True
	return False


    def lookForErrors(self):
	topDb = self.oligomerManager.getTopologyDatabase()
	self.temp_errorReason = None
	self.temp_inError = None
	uniqueOuts = {}
	for x in self.outCouplings:
	    if ( uniqueOuts.has_key(x.getName()) ):
		self.temp_inError = True
		self.temp_errorReason = "Every out coupling must be unique\nThere are multiple couplings with name: %s"%x.getName()
		return
	    else:
		uniqueOuts[x.getName()] = True
	if (not topDb.recognizesNameOrPdb(self.name) ):
#	    GrLog.write("Found an error for name: %s"%self.name)
	    self.temp_inError = True
	    self.temp_errorReason = "Bad subUnit name"
#	    GrLog.write("Reason: %s"%self.temp_errorReason)
	    return
#	GrLog.write("OK name: %s"%self.getName())
	cm = CouplingMap()
	if ( self.inCoupling != None ):
	    cm.setIn(self.inCoupling.getName())
	outNames = []
	for x in self.outCouplings:
	    outNames.append(x.getName())
#	GrLog.write("outCouplings=%s"%outNames)
	cm.setOut(outNames)
	mold = topDb.moldForNameOrPdb(self.getName())
	if ( mold.isCouplingMapValid(cm) ):
	    self.temp_inError = False
	else:
	    self.temp_inError = True
	    self.temp_errorReason = "Bad couplings"
#	    GrLog.write("Reason: %s"%self.errorReason)


    def	getUpperExtension(self):
	return 20

    def getLowerExtension(self):
	return 30

    def checkMouseOver(self,xy):
	pos = self.getTempPosition()
	bbox = copy.copy(GrMonomer.border)
	bbox.centerOn(pos)
	if ( bbox.containsPoint(xy) ):
	    self.setMouseOver(True)
	else:
	    self.setMouseOver(False)

    def setMouseOver(self,st):
	if ( self.mouseOver!=st ):
	    self.mouseOver = st
	    self.oligomerManager.signalAnimationChanged()



class	GrCoupling:
    selectButton = GrCouplingButton((0,0),(120,40),SELECT)
    queryButton = GrCircleButton((-50,-15),10,QUERY)
    insertBeforeButton = GrColoredRectangleButton((0,-30),(60,20),INSERT_BEFORE)
    insertAfterButton = GrColoredRectangleButton((0,30),(60,20),INSERT_AFTER)
    border = selectButton.rectangle()
    border.encompass(queryButton.rectangle())
    border.encompass(insertBeforeButton.rectangle())
    border.encompass(insertAfterButton.rectangle())


    def __init__(self,oligMan,nm=""):
	self.oligomerManager = oligMan
	self.fromMonomer = None
	self.toMonomer = None
	self.name = nm 
	self.isSelected = False
	self.reset_temp()



    def reset_temp(self):
	self.mouseOver = False
	self.inError = None
	self.errorReason = None
	pass

    def getDescription(self):
	str = "Coupling: %s\n"%self.name
	if ( self.inError != None ):
	    str = str+"Error: %s\n"%self.errorReason
	return str

    def getFromMonomer(self):
	return self.fromMonomer

    def _setFromMonomer(self,f):
	self.fromMonomer = f

    def getToMonomer(self):
	return self.toMonomer

    def _setToMonomer(self,t):
	self.toMonomer = t

    def get_fromMonomer(self):
	return self.fromMonomer

    def getSelected(self):
	return self.isSelected

    def _set_selected(self,s):
	self.isSelected = s

    def _set_name(self,nm):
	self.name = nm

    def getName(self):
	return self.name

    def _couple(self,f,t):
	self._setFromMonomer(f)
	self._setToMonomer(t)
	f._addOut(self)
	t._setIn(self)

    def getLineStartCenterStop(self):
	start = self.getFromMonomer().getTempPosition()
	stop = self.getToMonomer().getCouplingToPosition(start)
	dir = (stop[0]-start[0],stop[1]-start[1])
	midWayOffset = 0.6
	center = (start[0]+dir[0]*midWayOffset,start[1]+dir[1]*midWayOffset)
	return (start,center,stop)


    def render(self, dc ):
	(start,center,stop) = self.getLineStartCenterStop()
	dc.DrawLine(start[0],start[1],stop[0],stop[1])

	GrCoupling.selectButton.render(dc,center,self.inError,
					self.isSelected,self.name)

	if ( self.mouseOver ):
			#
			# Draw query button
			#
	    GrCoupling.queryButton.render(dc,center,wx.BLUE_BRUSH)
	    brush = wx.Brush(wx.GREEN)
	    GrCoupling.insertBeforeButton.render(dc,center,brush)
	    GrCoupling.insertAfterButton.render(dc,center,brush)

    def getQueryOffset(self):
	return 10

    def getDrawOffset(self):
	return 10

    def pick(self,xy):
	(start,center,stop) = self.getLineStartCenterStop()
	bbox = copy.copy(GrCoupling.border)
	bbox.centerOn(center)
	if ( not bbox.containsPoint(xy) ):
	    return None
    	pick = GrCoupling.queryButton.pick(center,xy)
	if ( pick != None ):
	    return (self,pick)
    	pick = GrCoupling.insertBeforeButton.pick(center,xy)
	if ( pick != None ):
	    return (self,pick)
    	pick = GrCoupling.insertAfterButton.pick(center,xy)
	if ( pick != None ):
	    return (self,pick)
	pick = GrCoupling.selectButton.pick(center,xy)
	if ( pick != None ):
	    return (self,pick)
	return None

    def inRectangle(self,rect):
	(fromPos,center,toPos) = self.getLineStartCenterStop()
	if ( rect.containsPoint(center) ):
	    return True
	return False

    def checkMouseOver(self,xy):
	(fromPos,center,toPos) = self.getLineStartCenterStop()
	bbox = copy.copy(GrCoupling.border)
	bbox.centerOn(center)
	if ( bbox.containsPoint(xy) ):
	    self.setMouseOver(True)
	else:
	    self.setMouseOver(False)

    def setMouseOver(self,st):
	if ( self.mouseOver!=st ):
	    self.mouseOver = st
	    self.oligomerManager.signalAnimationChanged()

    def getEntireExtent(self):
	return (80,60)

    def getSelectButtonExtent(self):
	return ((0,0),(80,40))

    def getQueryButton(self):
	return ((-35,-20),(10,10))

    def getUpperAddButton(self):
	return ((0,-25),(10,10))

    def getLowerAddButton(self):
	return ((0,25),(10,10))


    def lookForErrors(self):
	self.inError = None
	topDb = self.oligomerManager.getTopologyDatabase()
	if (topDb.doesCouplingRuleExistBetween(self.name,
					self.fromMonomer.getName(),
					self.toMonomer.getName()) == False ):
	    self.inError = True
	    self.errorReason = "There is no coupling rule named: %s allowed between %s and %s"%(self.name,self.fromMonomer.getName(),self.toMonomer.getName())



class	GrOligomer:
    def __init__(self,oligMan):
	self.oligomerManager = oligMan
	self.monomers = []
	self.couplings = []
	self.name = ""

    def _setName(self,nm):
	self.name = nm

    def getName(self):
	return self.name

    def getMonomers(self):
	return self.monomers

    def getCouplings(self):
	return self.couplings

    def reset(self):
	for m in self.monomers:
	    m.reset_temp()
	for c in self.couplings:
	    c.reset_temp()

    def root(self):
	m = self.monomers[0]
	while (m.inCoupling != None ):
	    c = m.get_inCoupling()
	    m = c.get_fromMonomer()
 	return m	

    def selectedParts(self):
	parts = []
	for m in self.monomers:
	    if ( m.getSelected() ):
		parts.append(m)
	for c in self.couplings:
	    if ( c.getSelected() ):
		parts.append(c)
	return parts

	
    def lookForErrors(self):
	for m in self.monomers:
	    m.lookForErrors()
	for c in self.couplings:
	    c.lookForErrors()

    def	pick(self,xy):
	for m in self.monomers:
	    p = m.pick(xy)
	    if ( p!=None ):
		return p
	for c in self.couplings:
	    p = c.pick(xy)
	    if ( p!=None ):
		return p
	return None

    def render(self,dc):
        dc.SetPen(wx.Pen('BLUE'))
	for x in self.couplings:
	    x.render(dc)
	for m in self.monomers:
	    m.render(dc)

    def dump(self,log):
	log.write("DUMPING oligomer")
	for m in self.monomers:
	    m.dump(log)



    def partsInRectangle(self,rect):
	parts = []
	for m in self.monomers:
	    if ( m.inRectangle(rect) ):
		parts.append(m)
	for c in self.couplings:
	    if ( c.inRectangle(rect) ):
		parts.append(c)
	return parts


    def selectedMonomers(self):
	sel = []
	for m in self.monomers:
	    if ( m.isSelected() ):
		sel.append(m)
	return sel


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#
#WORKING WORKING WORKING 
#
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


    def _addMonomer(self,mon):
	self.monomers.append(mon)

    def _removeMonomer(self,mon):
	self.monomers.remove(mon)
	
    def _addCoupling(self,coup):
	self.couplings.append(coup)

    def _removeCoupling(self,coup):
	self.couplings.remove(coup)



class	OligomerManager:
    def __init__(self,app,topDb):
	self.app = app
	self.topologyDatabase = topDb
	self.namedOligomers = {}
	self.commandList = []
	self.commandIndex = 0
	self.name = ""

    def signalAnimationChanged(self):
	send(OLIGOMER_CHANGED,self)

    def signalStructureChanged(self):
	self.oligomer.lookForErrors()
	send(OLIGOMER_CHANGED,self)

    def checkMouseOver(self,xy):
	for m in self.oligomer.getMonomers():
	    m.checkMouseOver(xy)
	for c in self.oligomer.getCouplings():
	    c.checkMouseOver(xy)


    def setName(self,nm):
	self.name = nm

    def getName(self):
	return self.name

    def getTopologyDatabase(self):
	return self.topologyDatabase

    def resetUndo(self):
	self.commandList = []
	self.commandIndex = 0

    def setOligomer(self,olig):
	self.oligomer = olig
	self.signalStructureChanged()
	self.resetUndo()

    def getMonomerExtent(self):
	return (140,40)

    def renderBoundingBox(self):
	monomers = self.oligomer.getMonomers()
	x = monomers[0]
	bbox = x.renderBoundingBox()
	for m in monomers[1:]:
	    bbox.encompass(m.renderBoundingBox())
	bbox.inflate((200,200))
	return bbox

    def pick(self,xy):
	return self.oligomer.pick(xy)



    def render(self,dc):
	self.oligomer.render(dc)

    def newOligomer(self):
	global GrLog
	olig = GrOligomer(self)
	mon1 = GrMonomer(self,"gly",root=True)
	mon2 = GrMonomer(self,"gly")
	olig._addMonomer(mon1)
	olig._addMonomer(mon2)
	coup = GrCoupling(self,"dkp")
	coup._couple(mon1,mon2)
	olig._addCoupling(coup)
	self.setOligomer(olig)
	olig.dump(GrLog)


    def	executeCommand(self,cmd):
	if ( self.commandIndex < len(self.commandList) ):
	    self.commandList = self.commandList[:self.commandIndex]
	self.commandList.append(cmd)
	self.commandIndex = self.commandIndex + 1
	cmd.execute()
	self.signalStructureChanged()

    def undo(self):
	if ( self.commandIndex == 0 ):
	    return
	self.commandIndex = self.commandIndex - 1
	self.commandList[self.commandIndex].unExecute()
	self.signalStructureChanged()

    def redo(self):
	if ( self.commandIndex >= len(self.commandList) ):
	    return
	self.commandList[self.commandIndex].execute()
	self.commandIndex = self.commandIndex + 1
	self.signalStructureChanged()	


    def temporarilyDragSelectedBy(self,xyDiff):
	for m in self.oligomer.getMonomers():
	    if ( m.getSelected() ):
		p = m.getPosition()
		newPos = (p[0]+xyDiff[0],p[1]+xyDiff[1])
		m.setTempPosition(newPos)
	self.signalAnimationChanged()	
	


    def	cmdClearSelection(self):
	cmd = CmdClearSelection(self.oligomer)
	self.executeCommand(cmd)


    def cmdLayout(self):
	cmd = CmdLayout(self.oligomer)
	self.executeCommand(cmd)

    def cmdSelectUnselectedWithinRectangle(self,rect):
	cmd = CmdSelectUnselectedWithinRectangle(self.oligomer,rect)
	self.executeCommand(cmd)

    def cmdClearSelectionThenSelectOnePart(self,part):
	cmd = CmdClearSelectionThenSelectOnePart(self.oligomer,part)
	self.executeCommand(cmd)

    def cmdToggleSelectionOnPart(self,part):
	cmd = CmdToggleSelectionOnPart(self.oligomer,part)
	self.executeCommand(cmd)

    def cmdDragSelectedBy(self,xyDiff):
	cmd = CmdDragSelectedBy(self.oligomer,xyDiff)
	self.executeCommand(cmd)

    def cmdMutateAllSelectedMonomersTo(self,newName):
	cmd = CmdMutateAllSelectedMonomersTo(self.oligomer,newName)
	self.executeCommand(cmd)


    def cmdMutateAllSelectedCouplingsTo(self,newName):
	cmd = CmdMutateAllSelectedCouplingsTo(self.oligomer,newName)
	self.executeCommand(cmd)

    def cmdAddMonomer(self, objStart, xyStop ):
	newMonomer = GrMonomer(self,self.app.getCurrentMonomer())
	coup = GrCoupling(self,self.app.getCurrentCoupling())
	coup._setToMonomer(newMonomer)
	coup._setFromMonomer(objStart)
	newMonomer._setIn(coup)
	newMonomer._set_position(xyStop)
	cmd = CmdAddMonomer(self.oligomer,objStart,coup,newMonomer)
	self.executeCommand(cmd)

    def cmdDeleteMonomer(self, mon ):
	cmd = CmdDeleteMonomer(self.oligomer,mon)
	self.executeCommand(cmd)


    def cmdInsertBefore(self,coupling):
	objStart = coupling.getFromMonomer()
	newMonomer = GrMonomer(self,self.app.getCurrentMonomer())
	coup = GrCoupling(self,self.app.getCurrentCoupling())
	coup._setToMonomer(newMonomer)
	coup._setFromMonomer(objStart)
	newMonomer._setIn(coup)
	newMonomer._addOut(coupling)
	outPos = coupling.getToMonomer().getPosition()
	newMonomer._set_position(outPos)
	cmd = CmdInsertBeforeCoupling(self.oligomer,coupling,coup,newMonomer)
	self.executeCommand(cmd)


#    def cmdRemoveMonomer(self,mon):
#	inc = mon.inCoupling
#	if ( inc==None ):
#	    return		# never remove the root
#	self.monomers.remove(mon)
#	parentMon = inc.get_fromMonomer()
#	parentMon.removeOutCoupling(inc)
#	self.couplings.remove(inc)
#	for childCoup in mon.outCouplings:
#	    parentMon._addOut(childCoup)
#	    childCoup._setFromMonomer(parentMon)
#

class	CmdUndoable:
    def __init__(self,olig):
	self.oligomer = olig

    def execute(self):
	global GrLog
#	GrLog.write("++++++++++   Doing command: %s"%self.__class__)
	self.oligomer.reset()

    def unExecute(self):
	global GrLog
#	GrLog.write("++++++++++ UNDoing command: %s"%self.__class__)
	self.oligomer.reset()


	
class	CmdClearSelection(CmdUndoable):
    def __init__(self,olig):
	CmdUndoable.__init__(self,olig)
	self.selectedParts = olig.selectedParts()

    def execute(self):
	CmdUndoable.execute(self)
	for x in self.selectedParts:
	    x._set_selected(False)

    def unExecute(self):
	CmdUndoable.unExecute(self)
	for x in self.selectedParts:
	    x._set_selected(True)


class	CmdSelectUnselectedWithinRectangle(CmdUndoable):
    def __init__(self,olig,rect):
	CmdUndoable.__init__(self,olig)
	partsInRect = olig.partsInRectangle(rect)
	self.partsToSelect= []
	for x in partsInRect:
	    if ( not x.getSelected() ):
		self.partsToSelect.append(x)

    def execute(self):
	CmdUndoable.execute(self)
	for x in self.partsToSelect:
	    x._set_selected(True)

    def unExecute(self):
	CmdUndoable.unExecute(self)
	for x in self.partsToSelect:
	    x._set_selected(False)



class	CmdClearSelectionThenSelectOnePart(CmdUndoable):
    def __init__(self,olig,part):
	CmdUndoable.__init__(self,olig)
	self.clearSelection = CmdClearSelection(olig)
	self.part = part
	self.oldState = part.getSelected()

    def execute(self):
	CmdUndoable.execute(self)
	self.clearSelection.execute()	
	self.part._set_selected(True)

    def	unExecute(self):
	CmdUndoable.unExecute(self)
	self.part._set_selected(self.oldState)
	self.clearSelection.unExecute()



class	CmdToggleSelectionOnPart(CmdUndoable):
    def __init__(self,olig,part):
	CmdUndoable.__init__(self,olig)
	self.part = part
	self.oldState = part.getSelected()
	self.newState = not self.oldState

    def execute(self):
	CmdUndoable.execute(self)
	self.part._set_selected(self.newState)

    def	unExecute(self):
	CmdUndoable.unExecute(self)
	self.part._set_selected(self.oldState)


class	CmdDragSelectedBy(CmdUndoable):
    def __init__(self,olig,xyDiff):
	global GrLog
	CmdUndoable.__init__(self,olig)
#	GrLog.write("!!!!!!!!!! STARTING CmdDragSelectedBy __init__")
	self.dragMonomers = []
	for x in olig.getMonomers():
	    if ( x.getSelected() ):
		pos = x.getPosition()
		self.dragMonomers.append((x,pos))
		GrLog.write("saving monomer %s at position: %f, %f"%(x.getName(),pos[0],pos[1]))
	self.xyDiff = xyDiff

    def execute(self):
	CmdUndoable.execute(self)
	for m in self.dragMonomers:
	    p = m[0].getPosition()
	    x = self.xyDiff[0]
	    y = self.xyDiff[1]
	    newPos = (p[0]+x,p[1]+y)
	    m[0]._set_position(newPos)

    def unExecute(self):
	CmdUndoable.unExecute(self)
	for m in self.dragMonomers:
	    m[0]._set_position(m[1])



class	CmdLayout(CmdUndoable):
    def __init__(self,olig):
	global GrLog
	CmdUndoable.__init__(self,olig)
	GrLog.write("LAYOUT")
	
	self.originalPositions = []
	for x in olig.getMonomers():
	    self.originalPositions.append((x,x.getPosition()))

    def execute(self):
	CmdUndoable.execute(self)
	x = 100
	y = 100
	yinc = 200
	spanningTree = SpanningTree(self.oligomer,self.oligomer.root())
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
#	    if ( couple != None ):
#	        couple.position = (x,y)
#	        y = y + yinc
	    monomer._set_position((x,y))
	    y = y + yinc
#	    monomer.layoutOutCouplingsToRightExceptTo(next)

    def unExecute(self):
	CmdUndoable.unExecute(self)
	for (m,p) in self.originalPositions:
	    m._set_position(p)

class	CmdMutateAllSelectedMonomersTo(CmdUndoable):
    def __init__(self,olig,nm):
	CmdUndoable.__init__(self,olig)
	self.changeNames = []
	for m in olig.getMonomers():
	    if ( m.getSelected() ):
		self.changeNames.append((m,m.getName()))
	self.newName = nm

    def execute(self):
	CmdUndoable.execute(self)
	for m in self.changeNames:
	    m[0]._set_name(self.newName)

    def unExecute(self):
	CmdUndoable.unExecute(self)
	for m in self.changeNames:
	    m[0]._set_name(m[1])


class	CmdMutateAllSelectedCouplingsTo(CmdUndoable):
    def __init__(self,olig,nm):
	CmdUndoable.__init__(self,olig)
	self.changeNames = []
	for m in olig.getCouplings():
	    if ( m.getSelected() ):
		self.changeNames.append((m,m.getName()))
	self.newName = nm

    def execute(self):
	CmdUndoable.execute(self)
	for m in self.changeNames:
	    m[0]._set_name(self.newName)

    def unExecute(self):
	CmdUndoable.unExecute(self)
	for m in self.changeNames:
	    m[0]._set_name(m[1])


class	CmdAddMonomer(CmdUndoable):
    def __init__(self,olig,monomerFrom,couplingNew, monomerNew ):
	CmdUndoable.__init__(self,olig)
	self.monomerFrom = monomerFrom
	self.couplingNew = couplingNew
	self.monomerNew = monomerNew

    def execute(self):
	CmdUndoable.execute(self)
	self.monomerFrom._addOut(self.couplingNew)
	self.oligomer._addMonomer(self.monomerNew)
	self.oligomer._addCoupling(self.couplingNew)

    def unExecute(self):
	CmdUndoable.unExecute(self)
	self.monomerFrom._removeOut(self.couplingNew)
	self.oligomer._removeMonomer(self.monomerNew)
	self.oligomer._removeCoupling(self.couplingNew)


class	CmdDeleteMonomer(CmdUndoable):
    def __init__(self,olig,mon):
	CmdUndoable.__init__(self,olig)
	self.monomerToDelete = mon
	self.couplingToDelete = mon.getIn()
	self.couplingsToRewire = mon.getOut()
	self.monomerToRewireTo = mon.getInMonomer()


    def execute(self):
	CmdUndoable.execute(self)
	self.oligomer._removeMonomer(self.monomerToDelete)
	self.oligomer._removeCoupling(self.couplingToDelete)
	self.monomerToRewireTo._removeOut(self.couplingToDelete)
	for c in self.couplingsToRewire:
	    c._setFromMonomer(self.monomerToRewireTo)
	    self.monomerToRewireTo._addOut(c)

    def unExecute(self):
	CmdUndoable.unExecute(self)
	for c in self.couplingsToRewire:
	    self.monomerToRewireTo._removeOut(c)
	    c._setFromMonomer(self.monomerToDelete)
	self.monomerToRewireTo._addOut(self.couplingToDelete)
	self.oligomer._addMonomer(self.monomerToDelete)
	self.oligomer._addCoupling(self.couplingToDelete)



class	CmdInsertBeforeCoupling(CmdUndoable):
    def __init__(self,olig,coupling,newCoupling,newMonomer):
	CmdUndoable.__init__(self,olig)
		# first calculate how much to move everything below down
	start = coupling.getFromMonomer().getPosition()
	stop = coupling.getToMonomer().getPosition()
	self.offset = ( stop[0]-start[0],stop[1]-start[1])
	tree = SpanningTree(olig,coupling.getToMonomer())
	self.objectsToMove = tree.allMonomers()
	self.couplingToInsertBefore = coupling
	self.newMonomer = newMonomer
	self.newCoupling = newCoupling

    def execute(self):
	CmdUndoable.execute(self)
	for m in self.objectsToMove:
	    m._translateBy(self.offset)
	mon = self.couplingToInsertBefore.getFromMonomer()
	mon._removeOut(self.couplingToInsertBefore)
	mon._addOut(self.newCoupling)
	self.oligomer._addCoupling(self.newCoupling)
	self.oligomer._addMonomer(self.newMonomer)
	self.couplingToInsertBefore._setFromMonomer(self.newMonomer)

    def unExecute(self):
	CmdUndoable.unExecute(self)
	# add your stuff here
	putBack = (-self.offset[0],-self.offset[1])
	for m in self.objectsToMove:
	    m._translateBy(putBack)
	mon = self.newCoupling.getFromMonomer()
	self.oligomer._removeCoupling(self.newCoupling)
	self.oligomer._removeMonomer(self.newMonomer)
	self.couplingToInsertBefore._setFromMonomer(mon)
	mon._removeOut(self.newCoupling)
	mon._addOut(self.couplingToInsertBefore)
	
class	CmdInsertAfterCoupling(CmdUndoable):
    def __init__(self,olig,coupling,newCoupling,newMonomer):
	CmdUndoable.__init__(self,olig)
		# first calculate how much to move everything below down
	start = coupling.getFromMonomer().getPosition()
	stop = coupling.getToMonomer().getPosition()
	self.offset = ( stop[0]-start[0],stop[1]-start[1])
	tree = SpanningTree(olig,coupling.getToMonomer())
	self.objectsToMove = tree.allMonomers()
	self.couplingToInsertAfter = coupling
	self.newMonomer = newMonomer
	self.newCoupling = newCoupling

    def execute(self):
	CmdUndoable.execute(self)
	for m in self.objectsToMove:
	    m._translateBy(self.offset)
	mon = self.couplingToInsertAfter.getToMonomer()
	mon._setIn(self.couplingToInsertAfter)
	mon._addOut(self.newCoupling)
	self.oligomer._addCoupling(self.newCoupling)
	self.oligomer._addMonomer(self.newMonomer)
	self.couplingToInsertBefore._setFromMonomer(self.newMonomer)

    def unExecute(self):
	CmdUndoable.unExecute(self)
	# add your stuff here
	putBack = (-self.offset[0],-self.offset[1])
	for m in self.objectsToMove:
	    m._translateBy(putBack)
	mon = self.newCoupling.getFromMonomer()
	self.oligomer._removeCoupling(self.newCoupling)
	self.oligomer._removeMonomer(self.newMonomer)
	self.couplingToInsertBefore._setFromMonomer(mon)
	mon._removeOut(self.newCoupling)
	mon._addOut(self.couplingToInsertBefore)
	



class	CmdNoOperation(CmdUndoable):
    def __init__(self,olig):
	CmdUndoable.__init__(self,olig)
	# add your stuff here

    def execute(self):
	CmdUndoable.execute(self)
	# add your stuff here

    def unExecute(self):
	CmdUndoable.unExecute(self)
	# add your stuff here



   




#
########################################################################
########################################################################
########################################################################
########################################################################
#
# main
#
#

GrLog = Log()

#
# Load topologyDatabase
#

builderXml = QuickDomNode("extractorDb.xml")
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

