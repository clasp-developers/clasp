AM_CPPFLAGS = $(ZLIB_INCLUDE_LINE) $(ZOIDFS_INCLUDE_LINE)

include_HEADERS = otf.h \
	OTF_CopyHandler.h \
	OTF_CopyHandler_stream.h \
	OTF_Definitions.h \
	OTF_Errno.h \
	OTF_File.h \
	OTF_File_iofsl.h \
	OTF_FileManager.h \
	OTF_Filenames.h \
	OTF_HandlerArray.h \
	OTF_inttypes.h \
	OTF_KeyValue.h \
	OTF_MasterControl.h \
	OTF_RBuffer.h \
	OTF_RStream.h \
	OTF_Reader.h \
	OTF_Version.h \
	OTF_WBuffer.h \
	OTF_WStream.h \
	OTF_Writer.h

nodist_include_HEADERS = OTF_inttypes_unix.h

noinst_HEADERS = OTF_Handler.h \
	         OTF_Keywords.h \
	         OTF_Parse.h \
	         OTF_Platform.h \
	         OTF_Platform_unix.h

lib_LTLIBRARIES = libopen-trace-format.la

libopen_trace_format_la_CFLAGS = -prefer-pic
libopen_trace_format_la_LDFLAGS = -version-info @OTF_VERSION_LIBRARY@
libopen_trace_format_la_LIBADD = $(ZLIB_LIB_LINE) $(ZOIDFS_LIB_LINE)
libopen_trace_format_la_SOURCES = \
	OTF_CopyHandler.c \
	OTF_CopyHandler_stream.c \
	OTF_Errno.c \
	OTF_File.c \
	OTF_File_iofsl.c \
	OTF_FileManager.c \
	OTF_Filenames.c \
	OTF_HandlerArray.c \
	OTF_KeyValue.c \
	OTF_MasterControl.c \
	OTF_Parse.c \
	OTF_Platform.c \
	OTF_RBuffer.c \
	OTF_Reader.c \
	OTF_RStream.c \
	OTF_WBuffer.c \
	OTF_Writer.c \
	OTF_WStream.c \
	OTF_inttypes.c

EXTRA_DIST = \
	OTF_Platform_win.h \
	OTF_inttypes_win.h \
	OTF_Version.h \
	libotf_vs08.vcproj


.PHONY: doc
doc:
	doxygen
