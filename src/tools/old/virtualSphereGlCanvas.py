from foundation import *

from mbbCore import *


from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *

from wxPython.wx import *
import wx

try:
    from wx import glcanvas
    haveGLCanvas = True
except ImportError:
    haveGLCanvas = False

if not haveGLCanvas:
#    print "The GLCanvas class could not be loaded"
    sys.exit(1)






class	VirtualSphereGlCanvas(glcanvas.GLCanvas):
    IDLE 	= 0
    ROTATE 	= 1
    TRANSLATE	= 2
    SCALE	= 3
    def __init__(self,parent,cnf={},**kw):
	glcanvas.GLCanvas.__init__(self,parent,-1)

	self.init = False
	self.shiftDown = False
	self.controlDown = False
	self.middleDown = False
	self.manipulateMode = VirtualSphereGlCanvas.IDLE

		# background color
	self.r_back = 0.0
	self.g_back = 0.0
	self.b_back = 0.0


		# field of view/near plane/far plane/eye position
	self.fovy = 30.0
	self.near = 0.1
	self.far = 1000.0
	self.distance = 30.0
	self.xtranslate = 0
	self.ytranslate = 0
	self.vTranslate = [0,0,0]
	self.virtualSphere = VirtualSphere()
	self.stereo = False
	self.crossEye = 1
	self.eyeDistanceDefault = 2
	self.pickCB = None
	self.redrawCB = None
	self.pickRedrawCB = None
	self.eyeDistance = self.eyeDistanceDefault

		# Figure out how many mouse buttons we have
	self.numberOfButtons = wx.SystemSettings_GetMetric(wx.SYS_MOUSE_BUTTONS)

	self.Bind( wx.EVT_PAINT, self.OnPaint )
	self.Bind( wx.EVT_SIZE, self.OnSize )
	self.Bind( wx.EVT_ERASE_BACKGROUND, self.OnEraseBackground )


	self.Bind(wx.EVT_LEFT_DOWN, self.OnPick )
	self.Bind(wx.EVT_MOTION, self.OnMouseMotion )





    def set_centerpoint(self,x,y,z):
	self.xcenter = x
	self.ycenter = y
	self.zcenter = z 
	self.virtualSphere.setCenter(x,y,z)
	self.virtualSphere.setTranslate(-x,-y,-z)
	self.Redraw()

    def HandlePick(self,event):
	pickx,upicky = event.GetPosition()
	picky = self.GetClientSize().height - upicky
	glSelectBuffer(1000)
# comment out the following code for debugging
	glRenderMode(GL_SELECT)
# comment in the following code for debugging
#	glRenderMode(GL_RENDER)		#DEBUG PICKING

	glPushMatrix()                  # Protect our matrix
	w,h = self.GetClientSize()
	glViewport(0, 0, w, h)

# comment out the following code for debugging
#	glClearColor(0,0,1,0)					#DEBUG
#	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)	#DEBUG

	glMatrixMode(GL_PROJECTION)
	glLoadIdentity()

	if ( not self.stereo ):
	    gluPickMatrix(pickx,picky,10,10)
	    self.SetupPerspective((w,h),0)
	else:
	    if ( pickx < w/2 ):
	    	gluPickMatrix(pickx*2,picky,20,20)
	    	self.SetupPerspective((w/2,h),1)
	    else:
	        gluPickMatrix((pickx-w/2)*2,picky,20,20)
	    	self.SetupPerspective((w/2,h),-1)

	self.pickRedrawCB(self)
	glFlush()                               # Tidy up
	glPopMatrix()                   # Restore the matrix
	buffer = glRenderMode(GL_RENDER)
	self.pickCB(buffer)

#comment in the following code for debugging
#	self.SwapBuffers()



    def OnPick(self,evt):
	self.HandlePick(evt)

#    def myIgnore(self,o):
#	return "break"
#
#    def setStereoOff(self):
#	self.stereo = False
#	self.tkRedraw()
#
#    def setStereoCrossEyed(self):
#	self.stereo = True
#	self.crossEye = 1
#	self.tkRedraw()
#
#    def setStereoWallEyed(self):
#    	self.stereo = True
#	self.crossEye = -1
#	self.tkRedraw()
#




    def OnEraseBackground(self,event):
	pass # do nothing

    def OnSize(self,event):
	self.size = self.GetClientSize()
	if ( self.GetContext() ):
	    self.SetCurrent()
	    glViewport(0,0,self.size.width,self.size.height )
	event.Skip()



    def OnPaint(self,event):

	"""Cause the opengl widget to redraw itself."""

	self.init = True
	dc = wx.PaintDC(self)
	self.SetCurrent()
#	wx.wxSafeYield()
	size = self.GetClientSize()
	glViewport(0, 0, size.width, size.height )
	glClearColor(self.r_back, self.g_back, self.b_back, 0.)
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

#	if ( self.stereo ): self.tkStereoRedraw(w,h,self.redrawCB)
#	else: self.tkMonoRedraw(w,h,self.redrawCB)
	self.MonoRedraw(size,self.redrawCB)
	self.SwapBuffers()


    def Redraw(self):
	if self.init:
	    self.Refresh()


    def MonoRedraw(self,size,redrawFn):
	glPushMatrix()
	glMatrixMode(GL_PROJECTION)
	glLoadIdentity()
	(w,h) = (size.width,size.height)
	self.SetupPerspective((w,h))
	# Call objects redraw method.
	if ( redrawFn ):
	    redrawFn(self)  # self.redraw()
	glFlush()				# Tidy up
	glPopMatrix()			# Restore the matrix


    def OnMouseMotion(self,event):
	# determine the current manipulation mode
	newMode = VirtualSphereGlCanvas.IDLE
	# Figure out how to handle the mouse
	doSomething = false
	if ( self.numberOfButtons == 2 ):
	   doSomething = event.RightIsDown()
	else:
	   doSomething = event.MiddleIsDown()
	if ( doSomething ):
	    if ( not event.ShiftDown() and not event.ControlDown() ):
		newMode = VirtualSphereGlCanvas.ROTATE
	    if ( event.ShiftDown() and not event.ControlDown() ):
		newMode = VirtualSphereGlCanvas.TRANSLATE
	    if ( not event.ShiftDown() and event.ControlDown() ):
		newMode = VirtualSphereGlCanvas.SCALE
	    if ( newMode != self.manipulateMode ):
		x = event.m_x
		y = event.m_y
		w,h = self.GetClientSize()
		if (newMode == VirtualSphereGlCanvas.ROTATE):
		    self.virtualSphere.setMouseStart(VS_ROTATE,x,y,w,h)
		if (newMode == VirtualSphereGlCanvas.TRANSLATE):
		    self.virtualSphere.setMouseStart(VS_TRANSLATE,x,y,w,h)
		if (newMode == VirtualSphereGlCanvas.SCALE):
		    self.virtualSphere.setMouseStart(VS_SCALE,x,y,w,h)
	    else:
		self.virtualSphere.setMouseMove(event.m_x,event.m_y)
		self.Redraw()
	self.manipulateMode = newMode



    def setPopUpMenu(self,popup):
#	print "Setting popup menu"
	pass




#	return "break"
#
#    def myContinueRotate(self,event):
#	self.virtualSphere.setMouseMove(event.x,event.y)
##	ma = self.virtualSphere.getOverallMatrix()
#	self.tkRedraw()
#	return "break"
#
		
	
		

#
#    def tkStereoRedraw(self,w,h,redrawFn):
#	glPushMatrix()			# Protect our matrix
#	glViewport(0, 0, w/2, h)
#	# Clear the background and depth buffer.
#	glMatrixMode(GL_PROJECTION)
#	glLoadIdentity()
#	self.tkSetupMatrix(w/2,h,1)
#	# Call objects redraw method.
#	redrawFn(self)  # self.redraw()
#	glFlush()				# Tidy up
#	glPopMatrix()			# Restore the matrix
#
#	glPushMatrix()			# Protect our matrix
#	glViewport(w/2+1, 0, w/2, h)
#	glMatrixMode(GL_PROJECTION)
#	glLoadIdentity()
#	self.tkSetupMatrix(w/2,h,-1)
#	# Call objects redraw method.
#	redrawFn(self)  # self.redraw()
#	glFlush()				# Tidy up
#	glPopMatrix()			# Restore the matrix
#

    def SetupPerspective(self,size,eye=0):
	glMatrixMode(GL_PROJECTION)
	w = size[0]
	h = size[1]
	gluPerspective(self.fovy, float(w)/float(h), self.near, self.far)
	gluLookAt(self.eyeDistance*self.crossEye*eye,0.0,self.distance,\
			0.0,0.0,0.0,0.0,1.0,0.0)
	glMatrixMode(GL_MODELVIEW)
	mat = self.virtualSphere.getOverallMatrix().asOpenGLMatrix()
	glLoadMatrixd(mat)
	glEnable(GL_DEPTH_TEST)
	glEnable(GL_LIGHTING)
	glEnable(GL_LIGHT0)
	glEnable(GL_NORMALIZE)


def draw(canvas):
    # clear color and depth buffers
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    # draw six faces of a cube
    glBegin(GL_QUADS)
    glNormal3f( 0.0, 0.0, 1.0)
    glVertex3f( 0.5, 0.5, 0.5)
    glVertex3f(-0.5, 0.5, 0.5)
    glVertex3f(-0.5,-0.5, 0.5)
    glVertex3f( 0.5,-0.5, 0.5)

    glNormal3f( 0.0, 0.0,-1.0)
    glVertex3f(-0.5,-0.5,-0.5)
    glVertex3f(-0.5, 0.5,-0.5)
    glVertex3f( 0.5, 0.5,-0.5)
    glVertex3f( 0.5,-0.5,-0.5)

    glNormal3f( 0.0, 1.0, 0.0)
    glVertex3f( 0.5, 0.5, 0.5)
    glVertex3f( 0.5, 0.5,-0.5)
    glVertex3f(-0.5, 0.5,-0.5)
    glVertex3f(-0.5, 0.5, 0.5)

    glNormal3f( 0.0,-1.0, 0.0)
    glVertex3f(-0.5,-0.5,-0.5)
    glVertex3f( 0.5,-0.5,-0.5)
    glVertex3f( 0.5,-0.5, 0.5)
    glVertex3f(-0.5,-0.5, 0.5)

    glNormal3f( 1.0, 0.0, 0.0)
    glVertex3f( 0.5, 0.5, 0.5)
    glVertex3f( 0.5,-0.5, 0.5)
    glVertex3f( 0.5,-0.5,-0.5)
    glVertex3f( 0.5, 0.5,-0.5)

    glNormal3f(-1.0, 0.0, 0.0)
    glVertex3f(-0.5,-0.5,-0.5)
    glVertex3f(-0.5,-0.5, 0.5)
    glVertex3f(-0.5, 0.5, 0.5)
    glVertex3f(-0.5, 0.5,-0.5)
    glEnd()   

class	VirtualSphereApp(wxApp):
    def OnInit(self):
	self.main = wxFrame( NULL, -1, "OpenGL viewer" )
	self.main.Show(1)
	self.opengl = VirtualSphereGlCanvas(self.main,-1)
	self.opengl.redrawCB = draw
	self.opengl.Show(1)
	return True




if ( __name__ == "__main__" ):
    app = VirtualSphereApp()
    app.MainLoop()
