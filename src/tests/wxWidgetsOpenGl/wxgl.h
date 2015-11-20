/*
    File: wxgl.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
#ifndef _glpane_
#define _glpane_

#include <stdio.h>
#include <string>
#include <vector>
#include <set>

#include "wx/wx.h"
#include "wx/glcanvas.h"

class BasicGLPane : public wxGLCanvas {

public:
  BasicGLPane(wxFrame *parent, int *args);

  void resized(wxSizeEvent &evt);

  int getWidth();
  int getHeight();

  void render(wxPaintEvent &evt);
  void prepare3DViewport(int topleft_x, int topleft_y, int bottomrigth_x, int bottomrigth_y);
  void prepare2DViewport(int topleft_x, int topleft_y, int bottomrigth_x, int bottomrigth_y);

  // events
  void mouseMoved(wxMouseEvent &event);
  void mouseDown(wxMouseEvent &event);
  void mouseWheelMoved(wxMouseEvent &event);
  void mouseReleased(wxMouseEvent &event);
  void rightClick(wxMouseEvent &event);
  void mouseLeftWindow(wxMouseEvent &event);
  void keyPressed(wxKeyEvent &event);
  void keyReleased(wxKeyEvent &event);

  DECLARE_EVENT_TABLE()
};

#endif
