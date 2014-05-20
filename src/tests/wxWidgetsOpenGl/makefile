include	$(CANDO_HOME)/makefile.inc

ifeq ($(DEVELOPMENT_ENVIRONMENT),darwin)

export LIBS = -lwx_base_carbon-2.8
export LIBS += -lwx_base_carbon_net-2.8
export LIBS += -lwx_base_carbon_xml-2.8
export LIBS += -lwx_mac_adv-2.8
export LIBS += -lwx_mac_aui-2.8
export LIBS += -lwx_mac_core-2.8
export LIBS += -lwx_mac_gl-2.8
export LIBS += -lwx_mac_html-2.8
export LIBS += -lwx_mac_qa-2.8
export LIBS += -lwx_mac_richtext-2.8
export LIBS += -lwx_mac_xrc-2.8
export LIBS += -lwxjpeg-2.8
export LIBS += -lwxpng-2.8
export LIBS += -lwxtiff-2.8
export LIBS += -liconv

export WXGL_APP=$(OS_BUILDTOP)/wxgl.app

printenv:
	echo WXGL_APP=$(WXGL_APP)

sa subAll:
	make wxgl
	make subBundle

wxgl: wxgl.cc
	g++ -D__WXMAC__ \
		-DSOURCE_DEBUG -DEXPAT -Ddarwin \
		-I$(mbbCxx_INCLUDE) $(EXTERNAL_INCLUDES)\
		-I/usr/local/lib/wx/include/mac-ansi-release-static-2.8 \
		-I/usr/local/include/wx-2.8 \
		-I/System/Library/Frameworks/OpenGL.framework/Versions/A/Headers/ \
		-L/usr/local/lib $(LIBS) \
		-framework IOKit \
		-framework Carbon  \
		-framework Cocoa \
		-framework System \
		-framework QuickTime \
		-framework OpenGL \
		-framework Glut \
		-framework AGL \
		-o wxgl wxgl.cc

wxgl.pp: wxgl.cc
	g++ -D__WXMAC__ \
		-DSOURCE_DEBUG -DEXPAT -Ddarwin \
		-I$(mbbCxx_INCLUDE) $(EXTERNAL_INCLUDES)\
		-I/usr/local/lib/wx/include/mac-ansi-release-static-2.8 \
		-I/usr/local/include/wx-2.8 \
		-I/System/Library/Frameworks/OpenGL.framework/Versions/A/Headers/ \
		-dM -E wxgl.cc >wxgl.pp
subBundle sb:
	install -d $(WXGL_APP)/
	install -d $(WXGL_APP)/Contents
	install -d $(WXGL_APP)/Contents/MacOS
	install -d $(WXGL_APP)/Contents/Resources
	install -d $(WXGL_APP)/Contents/Resources/English.lproj
	install -c Info.plist $(WXGL_APP)/Contents
	install -c wxgl $(WXGL_APP)/Contents/MacOS/wxgl
#	install -c version.plist $(WXGL_APP)/Contents
#	install -c InfoPlist.strings $(WXGL_APP)/Contents/Resources/English.lproj
#	echo -n 'APPL????' > $(CANDO_APP_ROOT)/Contents/PkgInfo
#	install -c WxWidgetTest.icns AnotherResource.txt $(CANDO_APP_ROOT)/Contents/Resources/



endif





########## ########## ########## ########## ########## 
#
# Windows
#
#
ifeq ($(DEVELOPMENT_ENVIRONMENT),Windows_NT)



printenv:
	echo CANDOVIEW_APP=$(CANDOVIEW_APP)

sa subAll:
	make wxgl
	make subBundle

subClean sc:
	rm -f *.o wxgl.exe

subBundle:
	@echo subBundle - do Nothing


CXX	= cl
LINK	= link.exe
CXXFLAGS= -DWIN32 -DEXPAT /EHsc /GR /MD

.cc.i:
	$(CXX) /P -D__WXMSW__ $(CXXFLAGS) /c \
		"/I$(WIN_BOOST_INCLUDES)" \
		"/I$(WIN_EXPAT_INCLUDES)" \
		"/I$(WIN_WX_INCLUDES)" \
		"/I$(shell cygpath -w /cygdrive/c/wxWidgets-2.8.10/lib/vc_lib/msw)" \
		"/I$(shell cygpath -w /cygdrive/c/Program Files/Microsoft Visual Studio .NET 2003/Vc7/PlatformSDK/Include)" \
		/Fo$@ $<




#.cc.o:
#	$(CXX) /c /nologo /TP \
#		/Fo$@ \
#		/MD \
#		/DWIN32 \
#		/EHsc \
#		/D__WXMSW__ \
#		/GR \
#		/DwxNO_THREADS \
#		/DEBUG \
#		"/I$(WIN_WX_INCLUDES)" \
#		"/I$(shell cygpath -w /cygdrive/c/wxWidgets-2.8.10/lib/vc_lib/msw)" \
#		$< 

.cc.o:
	$(CXX) /c /nologo /TP \
		/Fo$@ \
		/MD \
		/DWIN32 \
		/EHsc \
		/D__WXMSW__ \
		/GR \
		"/Ic:\wxWidgets-2.8.10\include" \
		"/Ic:\wxWidgets-2.8.10\lib\vc_lib\msw" \
		$< 


##  WORKS!!!
#
#.cc.o:
#	cl /c /nologo /TP \
#		/Fo$@ \
#		/MD \
#		/DWIN32   \
#		/O2  \
#		/D__WXMSW__ \
#		/DNOPCH /GR \
#		/EHsc \
#		 /W4 \
#		/D_WINDOWS  \
#		"/Ic:\wxWidgets-2.8.10\lib\vc_lib\msw" \
#		"/Ic:\wxWidgets-2.8.10\include" \
#		/I. \
#		"/Ic:\wxWidgets-2.8.10\samples" \
#		$<


clhelp:
	$(CXX) /help


linkhelp:
	link /link

wxgl: wxgl.o
	link /OUT:wxgl.exe wxgl.o /DEBUG \
		/SUBSYSTEM:windows \
		"/LIBPATH:$(shell cygpath -w $(CANDO_APP_BIN_DIR))" \
		wxmsw28_gl.lib\
		opengl32.lib\
		glu32.lib\
		wxmsw28_core.lib\
		wxbase28.lib\
		wxtiff.lib\
		wxjpeg.lib\
		wxpng.lib\
		wxzlib.lib\
		wxregex.lib\
		wxexpat.lib\
		kernel32.lib\
		user32.lib\
		gdi32.lib\
		comdlg32.lib\
		winspool.lib\
		winmm.lib\
		shell32.lib\
		comctl32.lib\
		ole32.lib\
		oleaut32.lib\
		uuid.lib\
		rpcrt4.lib\
		advapi32.lib\
		wsock32.lib\
		odbc32.lib

clCubeWay:
	cl /c /nologo /TP \
		/Fowxgl.o \
		/MD /DWIN32   /O2  /D__WXMSW__ \
		"/Ic:\wxWidgets-2.8.10\lib\vc_lib\msw" \
		"/Ic:\wxWidgets-2.8.10\include" \
		 /W4 /I. /D_WINDOWS  \
		"/Ic:\wxWidgets-2.8.10\samples" \
		/DNOPCH /GR  /EHsc wxgl.cc

linkCubeWay:
	link /NOLOGO \
		"/OUT:wxgl.exe"   \
		"/LIBPATH:c:\wxWidgets-2.8.10\lib\vc_lib" \
		/SUBSYSTEM:WINDOWS  \
		"wxgl.o"  \
		wxmsw28_gl.lib\
		opengl32.lib\
		glu32.lib\
		wxmsw28_core.lib\
		wxbase28.lib\
		wxtiff.lib\
		wxjpeg.lib\
		wxpng.lib\
		wxzlib.lib\
		wxregex.lib\
		wxexpat.lib\
		kernel32.lib\
		user32.lib\
		gdi32.lib\
		comdlg32.lib\
		winspool.lib\
		winmm.lib\
		shell32.lib\
		comctl32.lib\
		ole32.lib\
		oleaut32.lib\
		uuid.lib\
		rpcrt4.lib\
		advapi32.lib\
		wsock32.lib\
		odbc32.lib
endif
