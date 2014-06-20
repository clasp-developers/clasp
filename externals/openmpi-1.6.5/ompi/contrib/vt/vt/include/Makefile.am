if AMHAVEPTHREAD
PTHREADHEADERS = vt_wrap_pthread.h
else
PTHREADHEADERS =
endif

if AMBUILDLIBWRAP
LIBWRAPHEADERS = vt_libwrap.h
else
LIBWRAPHEADERS =
endif

if AMHAVEPLUGINCNTR
PLUGINCNTRHEADERS = vt_plugin_cntr.h
else
PLUGINCNTRHEADERS =
endif

include_HEADERS = \
	vt_user.h \
	vt_user.inc \
	vt_user_comment.h \
	vt_user_comment.inc \
	vt_user_control.h \
	vt_user_control.inc \
	vt_user_count.h \
	vt_user_count.inc \
	vt_user_marker.h \
	vt_user_marker.inc \
	vt_user_message.h \
	vt_user_message.inc \
	vt_user_region.h \
	vt_user_region.inc \
	$(PTHREADHEADERS) \
	$(LIBWRAPHEADERS) \
	$(PLUGINCNTRHEADERS)

nodist_include_HEADERS = \
        vt_inttypes.h

