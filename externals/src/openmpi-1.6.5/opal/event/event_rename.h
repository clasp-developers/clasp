/*
 *
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

/* buffer.c */
#define evbuffer_add opal_evbuffer_add
#define evbuffer_add_buffer opal_evbuffer_add_buffer
#define evbuffer_add_printf opal_evbuffer_add_printf
#define evbuffer_add_vprintf opal_evbuffer_add_vprintf
#define evbuffer_drain opal_evbuffer_drain
#define evbuffer_expand opal_evbuffer_expand
#define evbuffer_find opal_evbuffer_find
#define evbuffer_free opal_evbuffer_free
#define evbuffer_new opal_evbuffer_new
#define evbuffer_read opal_evbuffer_read
#define evbuffer_readline opal_evbuffer_readline
#define evbuffer_remove opal_evbuffer_remove
#define evbuffer_setcb opal_evbuffer_setcb
#define evbuffer_write opal_evbuffer_write

/* devpoll.c */
#define devpollops opal_devpollops

/* epoll_sub.c */
/* these symbols should *NOT* be renamed */


/* event.h */
#define OPAL_EV_TIMEOUT EV_TIMEOUT
#define OPAL_EV_READ EV_READ
#define OPAL_EV_WRITE EV_WRITE
#define OPAL_EV_SIGNAL EV_SIGNAL
#define OPAL_EV_PERSIST EV_PERSIST

#define OPAL_EVENT_SIGNAL(ev)	EVENT_SIGNAL(ev)

#define OPAL_EVLOOP_ONCE EVLOOP_ONCE
#define OPAL_EVLOOP_NONBLOCK EVLOOP_NONBLOCK
#define OPAL_EVLOOP_ONELOOP EVLOOP_ONELOOP


/* event.c */
#define current_base opal_current_base
#define event_base_loopexit opal_event_base_loopexit
#define event_get_method opal_event_get_method
#define event_get_version opal_event_get_version
#define event_gotsig opal_event_gotsig
#define event_sigcb opal_event_sigcb
#define event_add opal_event_add_i
#define event_del opal_event_del_i
#define event_active opal_event_active_i
#define event_base opal_event_base
#define event opal_event
#define event_base_new opal_event_base_new
#define event_init opal_event_init
#define event_reinit opal_event_reinit
#define event_dispatch opal_event_dispatch
#define event_base_dispatch opal_event_base_dispatch
#define event_base_get_method opal_event_base_get_method
#define event_base_free opal_event_base_free
#define event_set_log_callback opal_event_set_log_callback
#define event_base_set opal_event_base_set
#define event_loop opal_event_loop
#define event_base_loop opal_event_base_loop
#define event_loopexit opal_event_loopexit
#define event_loopbreak opal_event_loopbreak
#define event_base_loopbreak opal_event_base_loopbreak
#define event_set opal_event_set
#define event_once opal_event_once
#define event_base_once opal_event_base_once
#define event_pending opal_event_pending
#define event_get_version opal_event_get_version
#define event_get_method opal_event_get_method
#define event_priority_init opal_event_priority_init
#define event_base_priority_init opal_event_base_priority_init
#define event_priority_set opal_event_priority_set
#define evbuffer opal_evbuffer
#define bufferevent opal_bufferevent
#define event_watermark opal_event_watermark
#define bufferevent_new opal_bufferevent_new
#define bufferevent_priority_set opal_bufferevent_priority_set
#define bufferevent_free opal_bufferevent_free
#define bufferevent_write opal_bufferevent_write
#define bufferevent_write_buffer opal_bufferevent_write_buffer
#define bufferevent_read opal_bufferevent_read
#define bufferevent_enable opal_bufferevent_enable
#define bufferevent_disable opal_bufferevent_disable
#define bufferevent_settimeout opal_bufferevent_settimeout
#define evbuffer_new opal_evbuffer_new
#define evbuffer_free opal_evbuffer_free
#define evbuffer_expand opal_evbuffer_expand
#define evbuffer_add opal_evbuffer_add
#define evbuffer_remove opal_evbuffer_remove
#define evbuffer_readline opal_evbuffer_readline
#define evbuffer_add_buffer opal_evbuffer_add_buffer
#define evbuffer_add_printf opal_evbuffer_add_printf
#define evbuffer_add_vprintf opal_evbuffer_add_vprintf
#define evbuffer_drain opal_evbuffer_drain
#define evbuffer_write opal_evbuffer_write
#define evbuffer_read opal_evbuffer_read
#define evbuffer_find opal_evbuffer_find
#define evbuffer_setcb opal_evbuffer_setcb
#define evtag_init opal_evtag_init
#define evtag_marshal opal_evtag_marshal
#define encode_int opal_encode_int
#define evtag_marshal_int opal_evtag_marshal_int
#define evtag_marshal_string opal_evtag_marshal_string
#define evtag_marshal_timeval opal_evtag_marshal_timeval
#define evtag_unmarshal opal_evtag_unmarshal
#define evtag_peek opal_evtag_peek
#define evtag_peek_length opal_evtag_peek_length
#define evtag_payload_length opal_evtag_payload_length
#define evtag_consume opal_evtag_consume
#define evtag_unmarshal_fixed opal_evtag_unmarshal_fixed
#define evtag_unmarshal_string opal_evtag_unmarshal_string
#define evtag_unmarshal_timeval opal_evtag_unmarshal_timeval
#define evtag_unmarshal_int opal_evtag_unmarshal_int


/* log.c */
#define _event_debugx opal__event_debugx
#define event_err opal_event_err
#define event_errx opal_event_errx
#define event_msgx opal_event_msgx
#define event_set_log_callback opal_event_set_log_callback
#define event_warn opal_event_warn
#define event_warnx opal_event_warnx

/* poll.c */
#define pollop opal_pollop

/* evport.c */
#define evportops opal_evportops

/* event-internal.h */
#define eventop opal_eventop
#define event_base opal_event_base
#define _evsignal_set_handler _opal__evsignal_set_handler
#define _evsignal_restore_handler _opal__evsignal_restore_handler

/* evsignal.h */
#define evsignal_info opal_evsignal_info
#define evsignal_init opal_evsignal_init
#define evsignal_process opal_evsignal_process
#define evsignal_add opal_evsignal_add
#define evsignal_del opal_evsignal_del
#define evsignal_dealloc opal_evsignal_dealloc

/* evutil.c*/
#define evutil_socketpair opal_evutil_socketpair
#define evutil_make_socket_nonblocking opal_evutil_make_socket_nonblocking

/* kqueue.c */
#define kqop opal_kqop

/* min_heap.h */
#define min_heap_t opal_min_heap_t
#define min_heap_ctor opal_min_heap_ctor
#define min_heap_dtor opal_min_heap_dtor
#define min_heap_elem_init opal_min_heap_elem_init
#define min_heap_elem_greater opal_min_heap_elem_greater
#define min_heap_empty opal_min_heap_empty
#define min_heap_size opal_min_heap_size
#define min_heap_top opal_min_heap_top
#define min_heap_reserve opal_min_heap_reserve
#define min_heap_push opal_min_heap_push
#define min_heap_pop opal_min_heap_pop
#define min_heap_erase opal_min_heap_erase
#define min_heap_shift_up_ opal_min_heap_shift_up_
#define min_heap_shift_down_ opal_min_heap_shift_down_

/* select.c */
#define selectop opal_selectop



