# Makefile for Windows NT.  Assumes Microsoft compiler.
# DLLs are included in the root set under NT, but not under win32S.
# Use "nmake nodebug=1 all" for optimized versions of library, gctest and editor.
# MY_CPU=i386 for 32 bits or AMD64 for 64 bits
CPU=$(MY_CPU)
#!include <ntwin32.mak>

srcdir=..\..\src\gc

!if "$(ECL_THREADS)" != ""
THREADS_OBJ= win32_threads.obj thread_local_alloc.obj
THREADS_FLAGS= -D_CRT_SECURE_NO_WARNINGS -DGC_DLL -DGC_BUILD -DGC_WIN32_THREADS -DTHREAD_LOCAL_ALLOC -DLARGE_CONFIG
!else
THREADS_OBJ=
THREADS_FLAGS= -D_CRT_SECURE_NO_WARNINGS -DGC_DLL -DGC_BUILD -DLARGE_CONFIG
!endif

# Make sure that .cc is not viewed as a suffix.  It is for VC++2005, but
# not earlier versions.  We can deal with either, but not inconsistency.
.SUFFIXES:
.SUFFIXES: .obj .cpp .c

# Atomic_ops installation directory.  For win32, the source directory
# should do, since we only need the headers.
# We assume this was manually unpacked, since I'm not sure there is
# a Windows standard command line tool to do this.
AO_VERSION=1.2
AO_SRC_DIR=$(srcdir)/libatomic_ops-$(AO_VERSION)/src
AO_INCLUDE_DIR=$(AO_SRC_DIR)

OBJS= alloc.obj reclaim.obj allchblk.obj misc.obj mach_dep.obj os_dep.obj mark_rts.obj headers.obj mark.obj obj_map.obj blacklst.obj finalize.obj new_hblk.obj dbg_mlc.obj malloc.obj stubborn.obj dyn_load.obj typd_mlc.obj ptr_chck.obj gc_cpp.obj mallocx.obj win32_threads.obj msvc_dbg.obj thread_local_alloc.obj

all: gc.lib

{$(srcdir)}.c{}.obj:
	$(CC) -c -DWIN32 -D_MT $(CFLAGS_CONFIG) -Iinclude -I$(AO_INCLUDE_DIR) $(THREADS_FLAGS) -I$(srcdir)\include $< /Fo$*.obj

{$(srcdir)}.cpp{}.obj:
	$(CC) -c -DWIN32 -D_MT $(CFLAGS_CONFIG) -Iinclude -I$(AO_INCLUDE_DIR) $(THREADS_FLAGS) -I$(srcdir)\include $< /Fo$*.obj

gc.lib: $(OBJS)
!if "$(CPU)" == "i386"
	lib /MACHINE:i386 /out:gc.lib $(OBJS)
!else
	lib /MACHINE:X64 /out:gc.lib $(OBJS)
!endif

gc_cpp.obj: $(srcdir)\include\gc_cpp.h $(srcdir)\include\gc.h

gc_cpp.cpp: $(srcdir)\gc_cpp.cc
	copy $? $@
