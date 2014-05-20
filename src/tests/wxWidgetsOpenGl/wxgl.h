#ifndef _glpane_
#define _glpane_

#include	<stdio.h>
#include	<string>
#include	<vector>
#include	<set>

#include "wx/wx.h"
#include "wx/glcanvas.h"
 
class BasicGLPane : public wxGLCanvas
{
    
public:
	BasicGLPane(wxFrame* parent, int* args);
    
	void resized(wxSizeEvent& evt);
    
	int getWidth();
	int getHeight();
    
	void render(wxPaintEvent& evt);
	void prepare3DViewport(int topleft_x, int topleft_y, int bottomrigth_x, int bottomrigth_y);
	void prepare2DViewport(int topleft_x, int topleft_y, int bottomrigth_x, int bottomrigth_y);
    
	// events
	void mouseMoved(wxMouseEvent& event);
	void mouseDown(wxMouseEvent& event);
	void mouseWheelMoved(wxMouseEvent& event);
	void mouseReleased(wxMouseEvent& event);
	void rightClick(wxMouseEvent& event);
	void mouseLeftWindow(wxMouseEvent& event);
	void keyPressed(wxKeyEvent& event);
	void keyReleased(wxKeyEvent& event);
    
	DECLARE_EVENT_TABLE()
};
 
#endif 
