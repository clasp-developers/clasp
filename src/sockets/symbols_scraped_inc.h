/*
    File: symbols_scraped_inc.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
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
// Symbol table
#include "sockets_scrape_flag.h"
// SYMBOL_TABLE_ENTRY   SocketsPkg    0 SO_REUSEADDR                   SO-REUSEADDR                   export ; cName=_sym_SO_REUSEADDR lispName=SO-REUSEADDR
// SYMBOL_TABLE_ENTRY   SocketsPkg    1 ll_socketConnect_inetSocket    LL-SOCKET-CONNECT-INET-SOCKET  export ; cName=_sym_ll_socketConnect_inetSocket lispName=LL-SOCKET-CONNECT-INET-SOCKET
// SYMBOL_TABLE_ENTRY   SocketsPkg    2 EADDRINUSE                     EADDRINUSE                     export ; cName=_sym_EADDRINUSE lispName=EADDRINUSE
// SYMBOL_TABLE_ENTRY   SocketsPkg    3 ll_get_name_service_h_errno    LL-GET-NAME-SERVICE-H-ERRNO    export ; cName=_sym_ll_get_name_service_h_errno lispName=LL-GET-NAME-SERVICE-H-ERRNO
// SYMBOL_TABLE_ENTRY   SocketsPkg    4 socketPeername_localSocket     SOCKET-PEERNAME-LOCAL-SOCKET   export ; cName=_sym_socketPeername_localSocket lispName=SOCKET-PEERNAME-LOCAL-SOCKET
// SYMBOL_TABLE_ENTRY   SocketsPkg    5 TRY_AGAIN                      TRY-AGAIN                      export ; cName=_sym_TRY_AGAIN lispName=TRY-AGAIN
// SYMBOL_TABLE_ENTRY   SocketsPkg    6 ll_getSockoptLinger            LL-GET-SOCKOPT-LINGER          export ; cName=_sym_ll_getSockoptLinger lispName=LL-GET-SOCKOPT-LINGER
// SYMBOL_TABLE_ENTRY   SocketsPkg    7 EPROTONOSUPPORT                EPROTONOSUPPORT                export ; cName=_sym_EPROTONOSUPPORT lispName=EPROTONOSUPPORT
// SYMBOL_TABLE_ENTRY   SocketsPkg    8 _PLUS_eintr_PLUS_              +EINTR+                        export ; cName=_sym__PLUS_eintr_PLUS_ lispName=+EINTR+
// SYMBOL_TABLE_ENTRY   SocketsPkg    9 _PLUS_sock_stream_PLUS_        +SOCK-STREAM+                  export ; cName=_sym__PLUS_sock_stream_PLUS_ lispName=+SOCK-STREAM+
// SYMBOL_TABLE_ENTRY   SocketsPkg   10 ENOBUFS                        ENOBUFS                        export ; cName=_sym_ENOBUFS lispName=ENOBUFS
// SYMBOL_TABLE_ENTRY   SocketsPkg   11 ll_getSockoptInt               LL-GET-SOCKOPT-INT             export ; cName=_sym_ll_getSockoptInt lispName=LL-GET-SOCKOPT-INT
// SYMBOL_TABLE_ENTRY   SocketsPkg   12 _PLUS_brcl_stream_mode_output_PLUS_ +BRCL-STREAM-MODE-OUTPUT+      export ; cName=_sym__PLUS_brcl_stream_mode_output_PLUS_ lispName=+BRCL-STREAM-MODE-OUTPUT+
// SYMBOL_TABLE_ENTRY   SocketsPkg   13 ll_socketSendAddress           LL-SOCKET-SEND-ADDRESS         export ; cName=_sym_ll_socketSendAddress lispName=LL-SOCKET-SEND-ADDRESS
// SYMBOL_TABLE_ENTRY   SocketsPkg   14 socketSendNoAddress            SOCKET-SEND-NO-ADDRESS         export ; cName=_sym_socketSendNoAddress lispName=SOCKET-SEND-NO-ADDRESS
// SYMBOL_TABLE_ENTRY   SocketsPkg   15 ll_setfNonBlockingMode         LL-SETF-NON-BLOCKING-MODE      export ; cName=_sym_ll_setfNonBlockingMode lispName=LL-SETF-NON-BLOCKING-MODE
// SYMBOL_TABLE_ENTRY   SocketsPkg   16 EPERM                          EPERM                          export ; cName=_sym_EPERM lispName=EPERM
// SYMBOL_TABLE_ENTRY   SocketsPkg   17 EINTR                          EINTR                          export ; cName=_sym_EINTR lispName=EINTR
// SYMBOL_TABLE_ENTRY   SocketsPkg   18 SOL_SOCKET                     SOL-SOCKET                     export ; cName=_sym_SOL_SOCKET lispName=SOL-SOCKET
// SYMBOL_TABLE_ENTRY   SocketsPkg   19 ll_socketConnect_localSocket   LL-SOCKET-CONNECT-LOCAL-SOCKET export ; cName=_sym_ll_socketConnect_localSocket lispName=LL-SOCKET-CONNECT-LOCAL-SOCKET
// SYMBOL_TABLE_ENTRY   SocketsPkg   20 SO_RCVTIMEO                    SO-RCVTIMEO                    export ; cName=_sym_SO_RCVTIMEO lispName=SO-RCVTIMEO
// SYMBOL_TABLE_ENTRY   SocketsPkg   21 SO_RCVBUF                      SO-RCVBUF                      export ; cName=_sym_SO_RCVBUF lispName=SO-RCVBUF
// SYMBOL_TABLE_ENTRY   SocketsPkg   22 _PLUS_af_local_PLUS_           +AF-LOCAL+                     export ; cName=_sym__PLUS_af_local_PLUS_ lispName=+AF-LOCAL+
// SYMBOL_TABLE_ENTRY   SocketsPkg   23 ENETUNREACH                    ENETUNREACH                    export ; cName=_sym_ENETUNREACH lispName=ENETUNREACH
// SYMBOL_TABLE_ENTRY   SocketsPkg   24 ll_strerror_errno              LL-STRERROR-ERRNO              export ; cName=_sym_ll_strerror_errno lispName=LL-STRERROR-ERRNO
// SYMBOL_TABLE_ENTRY   SocketsPkg   25 EOPNOTSUPP                     EOPNOTSUPP                     export ; cName=_sym_EOPNOTSUPP lispName=EOPNOTSUPP
// SYMBOL_TABLE_ENTRY   SocketsPkg   26 ll_setSockOptTimeval           LL-SET-SOCK-OPT-TIMEVAL        export ; cName=_sym_ll_setSockOptTimeval lispName=LL-SET-SOCK-OPT-TIMEVAL
// SYMBOL_TABLE_ENTRY   SocketsPkg   27 _PLUS_sock_dgram_PLUS_         +SOCK-DGRAM+                   export ; cName=_sym__PLUS_sock_dgram_PLUS_ lispName=+SOCK-DGRAM+
// SYMBOL_TABLE_ENTRY   SocketsPkg   28 ENOMEM                         ENOMEM                         export ; cName=_sym_ENOMEM lispName=ENOMEM
// SYMBOL_TABLE_ENTRY   SocketsPkg   29 NO_ADDRESS                     NO-ADDRESS                     export ; cName=_sym_NO_ADDRESS lispName=NO-ADDRESS
// SYMBOL_TABLE_ENTRY   SocketsPkg   30 ll_socketBind_localSocket      LL-SOCKET-BIND-LOCAL-SOCKET    export ; cName=_sym_ll_socketBind_localSocket lispName=LL-SOCKET-BIND-LOCAL-SOCKET
// SYMBOL_TABLE_ENTRY   SocketsPkg   31 ll_getNameServiceErrorMessage  LL-GET-NAME-SERVICE-ERROR-MESSAGE export ; cName=_sym_ll_getNameServiceErrorMessage lispName=LL-GET-NAME-SERVICE-ERROR-MESSAGE
// SYMBOL_TABLE_ENTRY   SocketsPkg   32 ll_getSockoptTimeval           LL-GET-SOCKOPT-TIMEVAL         export ; cName=_sym_ll_getSockoptTimeval lispName=LL-GET-SOCKOPT-TIMEVAL
// SYMBOL_TABLE_ENTRY   SocketsPkg   33 ll_autoCloseTwoWayStream       LL-AUTO-CLOSE-TWO-WAY-STREAM   export ; cName=_sym_ll_autoCloseTwoWayStream lispName=LL-AUTO-CLOSE-TWO-WAY-STREAM
// SYMBOL_TABLE_ENTRY   SocketsPkg   34 _PLUS_brcl_stream_mode_input_PLUS_ +BRCL-STREAM-MODE-INPUT+       export ; cName=_sym__PLUS_brcl_stream_mode_input_PLUS_ lispName=+BRCL-STREAM-MODE-INPUT+
// SYMBOL_TABLE_ENTRY   SocketsPkg   35 ESOCKTNOSUPPORT                ESOCKTNOSUPPORT                export ; cName=_sym_ESOCKTNOSUPPORT lispName=ESOCKTNOSUPPORT
// SYMBOL_TABLE_ENTRY   SocketsPkg   36 ECONNREFUSED                   ECONNREFUSED                   export ; cName=_sym_ECONNREFUSED lispName=ECONNREFUSED
// SYMBOL_TABLE_ENTRY   SocketsPkg   37 SO_TYPE                        SO-TYPE                        export ; cName=_sym_SO_TYPE lispName=SO-TYPE
// SYMBOL_TABLE_ENTRY   SocketsPkg   38 EINVAL                         EINVAL                         export ; cName=_sym_EINVAL lispName=EINVAL
// SYMBOL_TABLE_ENTRY   SocketsPkg   39 _PLUS_eagain_PLUS_             +EAGAIN+                       export ; cName=_sym__PLUS_eagain_PLUS_ lispName=+EAGAIN+
// SYMBOL_TABLE_ENTRY   SocketsPkg   40 ll_getProtocolByName           LL-GET-PROTOCOL-BY-NAME        export ; cName=_sym_ll_getProtocolByName lispName=LL-GET-PROTOCOL-BY-NAME
// SYMBOL_TABLE_ENTRY   SocketsPkg   41 ll_dup                         LL-DUP                         export ; cName=_sym_ll_dup lispName=LL-DUP
// SYMBOL_TABLE_ENTRY   SocketsPkg   42 ll_makeStreamFromFd            LL-MAKE-STREAM-FROM-FD         export ; cName=_sym_ll_makeStreamFromFd lispName=LL-MAKE-STREAM-FROM-FD
// SYMBOL_TABLE_ENTRY   SocketsPkg   43 NETDB_INTERNAL                 NETDB-INTERNAL                 export ; cName=_sym_NETDB_INTERNAL lispName=NETDB-INTERNAL
// SYMBOL_TABLE_ENTRY   SocketsPkg   44 EBADF                          EBADF                          export ; cName=_sym_EBADF lispName=EBADF
// SYMBOL_TABLE_ENTRY   SocketsPkg   45 ll_getHostByAddress            LL-GET-HOST-BY-ADDRESS         export ; cName=_sym_ll_getHostByAddress lispName=LL-GET-HOST-BY-ADDRESS
// SYMBOL_TABLE_ENTRY   SocketsPkg   46 ll_setSockOptBool              LL-SET-SOCK-OPT-BOOL           export ; cName=_sym_ll_setSockOptBool lispName=LL-SET-SOCK-OPT-BOOL
// SYMBOL_TABLE_ENTRY   SocketsPkg   47 IPPROTO_TCP                    IPPROTO-TCP                    export ; cName=_sym_IPPROTO_TCP lispName=IPPROTO-TCP
// SYMBOL_TABLE_ENTRY   SocketsPkg   48 ff_close                       FF-CLOSE                       export ; cName=_sym_ff_close lispName=FF-CLOSE
// SYMBOL_TABLE_ENTRY   SocketsPkg   49 ff_listen                      FF-LISTEN                      export ; cName=_sym_ff_listen lispName=FF-LISTEN
// SYMBOL_TABLE_ENTRY   SocketsPkg   50 ff_socket                      FF-SOCKET                      export ; cName=_sym_ff_socket lispName=FF-SOCKET
// SYMBOL_TABLE_ENTRY   SocketsPkg   51 ll_setSockoptInt               LL-SET-SOCKOPT-INT             export ; cName=_sym_ll_setSockoptInt lispName=LL-SET-SOCKOPT-INT
// SYMBOL_TABLE_ENTRY   SocketsPkg   52 ll_socketAccept_localSocket    LL-SOCKET-ACCEPT-LOCAL-SOCKET  export ; cName=_sym_ll_socketAccept_localSocket lispName=LL-SOCKET-ACCEPT-LOCAL-SOCKET
// SYMBOL_TABLE_ENTRY   SocketsPkg   53 ll_nonBlockingMode             LL-NON-BLOCKING-MODE           export ; cName=_sym_ll_nonBlockingMode lispName=LL-NON-BLOCKING-MODE
// SYMBOL_TABLE_ENTRY   SocketsPkg   54 SO_DONTROUTE                   SO-DONTROUTE                   export ; cName=_sym_SO_DONTROUTE lispName=SO-DONTROUTE
// SYMBOL_TABLE_ENTRY   SocketsPkg   55 ll_strerror                    LL-STRERROR                    export ; cName=_sym_ll_strerror lispName=LL-STRERROR
// SYMBOL_TABLE_ENTRY   SocketsPkg   56 ll_setSockOptLinger            LL-SET-SOCK-OPT-LINGER         export ; cName=_sym_ll_setSockOptLinger lispName=LL-SET-SOCK-OPT-LINGER
// SYMBOL_TABLE_ENTRY   SocketsPkg   57 SO_SNDTIMEO                    SO-SNDTIMEO                    export ; cName=_sym_SO_SNDTIMEO lispName=SO-SNDTIMEO
// SYMBOL_TABLE_ENTRY   SocketsPkg   58 ll_socketPeername_inetSocket   LL-SOCKET-PEERNAME-INET-SOCKET export ; cName=_sym_ll_socketPeername_inetSocket lispName=LL-SOCKET-PEERNAME-INET-SOCKET
// SYMBOL_TABLE_ENTRY   SocketsPkg   59 SO_REUSEPORT                   SO-REUSEPORT                   export ; cName=_sym_SO_REUSEPORT lispName=SO-REUSEPORT
// SYMBOL_TABLE_ENTRY   SocketsPkg   60 SO_KEEPALIVE                   SO-KEEPALIVE                   export ; cName=_sym_SO_KEEPALIVE lispName=SO-KEEPALIVE
// SYMBOL_TABLE_ENTRY   SocketsPkg   61 _PLUS_brcl_stream_mode_io_PLUS_ +BRCL-STREAM-MODE-IO+          export ; cName=_sym__PLUS_brcl_stream_mode_io_PLUS_ lispName=+BRCL-STREAM-MODE-IO+
// SYMBOL_TABLE_ENTRY   SocketsPkg   62 HOST_NOT_FOUND                 HOST-NOT-FOUND                 export ; cName=_sym_HOST_NOT_FOUND lispName=HOST-NOT-FOUND
// SYMBOL_TABLE_ENTRY   SocketsPkg   63 _PLUS_af_inet_PLUS_            +AF-INET+                      export ; cName=_sym__PLUS_af_inet_PLUS_ lispName=+AF-INET+
// SYMBOL_TABLE_ENTRY   SocketsPkg   64 ETIMEDOUT                      ETIMEDOUT                      export ; cName=_sym_ETIMEDOUT lispName=ETIMEDOUT
// SYMBOL_TABLE_ENTRY   SocketsPkg   65 ll_socketBind_inetSocket       LL-SOCKET-BIND-INET-SOCKET     export ; cName=_sym_ll_socketBind_inetSocket lispName=LL-SOCKET-BIND-INET-SOCKET
// SYMBOL_TABLE_ENTRY   SocketsPkg   66 ll_getSockoptBool              LL-GET-SOCKOPT-BOOL            export ; cName=_sym_ll_getSockoptBool lispName=LL-GET-SOCKOPT-BOOL
// SYMBOL_TABLE_ENTRY   SocketsPkg   67 ll_socketReceive               LL-SOCKET-RECEIVE              export ; cName=_sym_ll_socketReceive lispName=LL-SOCKET-RECEIVE
// SYMBOL_TABLE_ENTRY   SocketsPkg   68 SO_LINGER                      SO-LINGER                      export ; cName=_sym_SO_LINGER lispName=SO-LINGER
// SYMBOL_TABLE_ENTRY   SocketsPkg   69 NO_RECOVERY                    NO-RECOVERY                    export ; cName=_sym_NO_RECOVERY lispName=NO-RECOVERY
// SYMBOL_TABLE_ENTRY   SocketsPkg   70 ll_socket_errno                LL-SOCKET-ERRNO                export ; cName=_sym_ll_socket_errno lispName=LL-SOCKET-ERRNO
// SYMBOL_TABLE_ENTRY   SocketsPkg   71 ll_getHostByName               LL-GET-HOST-BY-NAME            export ; cName=_sym_ll_getHostByName lispName=LL-GET-HOST-BY-NAME
// SYMBOL_TABLE_ENTRY   SocketsPkg   72 ll_socketAccept_inetSocket     LL-SOCKET-ACCEPT-INET-SOCKET   export ; cName=_sym_ll_socketAccept_inetSocket lispName=LL-SOCKET-ACCEPT-INET-SOCKET
// SYMBOL_TABLE_ENTRY   SocketsPkg   73 NETDB_SUCCESS                  NETDB-SUCCESS                  export ; cName=_sym_NETDB_SUCCESS lispName=NETDB-SUCCESS
// SYMBOL_TABLE_ENTRY   SocketsPkg   74 ll_socketName                  LL-SOCKET-NAME                 export ; cName=_sym_ll_socketName lispName=LL-SOCKET-NAME
// SYMBOL_TABLE_ENTRY   SocketsPkg   75 TCP_NODELAY                    TCP-NODELAY                    export ; cName=_sym_TCP_NODELAY lispName=TCP-NODELAY
// SYMBOL_TABLE_ENTRY   SocketsPkg   76 EAGAIN                         EAGAIN                         export ; cName=_sym_EAGAIN lispName=EAGAIN
#ifdef SocketsPkg_SYMBOLS
DO_SYMBOL(_sym_SO_REUSEADDR,0,SocketsPkg,"SO-REUSEADDR",true);
DO_SYMBOL(_sym_ll_socketConnect_inetSocket,1,SocketsPkg,"LL-SOCKET-CONNECT-INET-SOCKET",true);
DO_SYMBOL(_sym_EADDRINUSE,2,SocketsPkg,"EADDRINUSE",true);
DO_SYMBOL(_sym_ll_get_name_service_h_errno,3,SocketsPkg,"LL-GET-NAME-SERVICE-H-ERRNO",true);
DO_SYMBOL(_sym_socketPeername_localSocket,4,SocketsPkg,"SOCKET-PEERNAME-LOCAL-SOCKET",true);
DO_SYMBOL(_sym_TRY_AGAIN,5,SocketsPkg,"TRY-AGAIN",true);
DO_SYMBOL(_sym_ll_getSockoptLinger,6,SocketsPkg,"LL-GET-SOCKOPT-LINGER",true);
DO_SYMBOL(_sym_EPROTONOSUPPORT,7,SocketsPkg,"EPROTONOSUPPORT",true);
DO_SYMBOL(_sym__PLUS_eintr_PLUS_,8,SocketsPkg,"+EINTR+",true);
DO_SYMBOL(_sym__PLUS_sock_stream_PLUS_,9,SocketsPkg,"+SOCK-STREAM+",true);
DO_SYMBOL(_sym_ENOBUFS,10,SocketsPkg,"ENOBUFS",true);
DO_SYMBOL(_sym_ll_getSockoptInt,11,SocketsPkg,"LL-GET-SOCKOPT-INT",true);
DO_SYMBOL(_sym__PLUS_brcl_stream_mode_output_PLUS_,12,SocketsPkg,"+BRCL-STREAM-MODE-OUTPUT+",true);
DO_SYMBOL(_sym_ll_socketSendAddress,13,SocketsPkg,"LL-SOCKET-SEND-ADDRESS",true);
DO_SYMBOL(_sym_socketSendNoAddress,14,SocketsPkg,"SOCKET-SEND-NO-ADDRESS",true);
DO_SYMBOL(_sym_ll_setfNonBlockingMode,15,SocketsPkg,"LL-SETF-NON-BLOCKING-MODE",true);
DO_SYMBOL(_sym_EPERM,16,SocketsPkg,"EPERM",true);
DO_SYMBOL(_sym_EINTR,17,SocketsPkg,"EINTR",true);
DO_SYMBOL(_sym_SOL_SOCKET,18,SocketsPkg,"SOL-SOCKET",true);
DO_SYMBOL(_sym_ll_socketConnect_localSocket,19,SocketsPkg,"LL-SOCKET-CONNECT-LOCAL-SOCKET",true);
DO_SYMBOL(_sym_SO_RCVTIMEO,20,SocketsPkg,"SO-RCVTIMEO",true);
DO_SYMBOL(_sym_SO_RCVBUF,21,SocketsPkg,"SO-RCVBUF",true);
DO_SYMBOL(_sym__PLUS_af_local_PLUS_,22,SocketsPkg,"+AF-LOCAL+",true);
DO_SYMBOL(_sym_ENETUNREACH,23,SocketsPkg,"ENETUNREACH",true);
DO_SYMBOL(_sym_ll_strerror_errno,24,SocketsPkg,"LL-STRERROR-ERRNO",true);
DO_SYMBOL(_sym_EOPNOTSUPP,25,SocketsPkg,"EOPNOTSUPP",true);
DO_SYMBOL(_sym_ll_setSockOptTimeval,26,SocketsPkg,"LL-SET-SOCK-OPT-TIMEVAL",true);
DO_SYMBOL(_sym__PLUS_sock_dgram_PLUS_,27,SocketsPkg,"+SOCK-DGRAM+",true);
DO_SYMBOL(_sym_ENOMEM,28,SocketsPkg,"ENOMEM",true);
DO_SYMBOL(_sym_NO_ADDRESS,29,SocketsPkg,"NO-ADDRESS",true);
DO_SYMBOL(_sym_ll_socketBind_localSocket,30,SocketsPkg,"LL-SOCKET-BIND-LOCAL-SOCKET",true);
DO_SYMBOL(_sym_ll_getNameServiceErrorMessage,31,SocketsPkg,"LL-GET-NAME-SERVICE-ERROR-MESSAGE",true);
DO_SYMBOL(_sym_ll_getSockoptTimeval,32,SocketsPkg,"LL-GET-SOCKOPT-TIMEVAL",true);
DO_SYMBOL(_sym_ll_autoCloseTwoWayStream,33,SocketsPkg,"LL-AUTO-CLOSE-TWO-WAY-STREAM",true);
DO_SYMBOL(_sym__PLUS_brcl_stream_mode_input_PLUS_,34,SocketsPkg,"+BRCL-STREAM-MODE-INPUT+",true);
DO_SYMBOL(_sym_ESOCKTNOSUPPORT,35,SocketsPkg,"ESOCKTNOSUPPORT",true);
DO_SYMBOL(_sym_ECONNREFUSED,36,SocketsPkg,"ECONNREFUSED",true);
DO_SYMBOL(_sym_SO_TYPE,37,SocketsPkg,"SO-TYPE",true);
DO_SYMBOL(_sym_EINVAL,38,SocketsPkg,"EINVAL",true);
DO_SYMBOL(_sym__PLUS_eagain_PLUS_,39,SocketsPkg,"+EAGAIN+",true);
DO_SYMBOL(_sym_ll_getProtocolByName,40,SocketsPkg,"LL-GET-PROTOCOL-BY-NAME",true);
DO_SYMBOL(_sym_ll_dup,41,SocketsPkg,"LL-DUP",true);
DO_SYMBOL(_sym_ll_makeStreamFromFd,42,SocketsPkg,"LL-MAKE-STREAM-FROM-FD",true);
DO_SYMBOL(_sym_NETDB_INTERNAL,43,SocketsPkg,"NETDB-INTERNAL",true);
DO_SYMBOL(_sym_EBADF,44,SocketsPkg,"EBADF",true);
DO_SYMBOL(_sym_ll_getHostByAddress,45,SocketsPkg,"LL-GET-HOST-BY-ADDRESS",true);
DO_SYMBOL(_sym_ll_setSockOptBool,46,SocketsPkg,"LL-SET-SOCK-OPT-BOOL",true);
DO_SYMBOL(_sym_IPPROTO_TCP,47,SocketsPkg,"IPPROTO-TCP",true);
DO_SYMBOL(_sym_ff_close,48,SocketsPkg,"FF-CLOSE",true);
DO_SYMBOL(_sym_ff_listen,49,SocketsPkg,"FF-LISTEN",true);
DO_SYMBOL(_sym_ff_socket,50,SocketsPkg,"FF-SOCKET",true);
DO_SYMBOL(_sym_ll_setSockoptInt,51,SocketsPkg,"LL-SET-SOCKOPT-INT",true);
DO_SYMBOL(_sym_ll_socketAccept_localSocket,52,SocketsPkg,"LL-SOCKET-ACCEPT-LOCAL-SOCKET",true);
DO_SYMBOL(_sym_ll_nonBlockingMode,53,SocketsPkg,"LL-NON-BLOCKING-MODE",true);
DO_SYMBOL(_sym_SO_DONTROUTE,54,SocketsPkg,"SO-DONTROUTE",true);
DO_SYMBOL(_sym_ll_strerror,55,SocketsPkg,"LL-STRERROR",true);
DO_SYMBOL(_sym_ll_setSockOptLinger,56,SocketsPkg,"LL-SET-SOCK-OPT-LINGER",true);
DO_SYMBOL(_sym_SO_SNDTIMEO,57,SocketsPkg,"SO-SNDTIMEO",true);
DO_SYMBOL(_sym_ll_socketPeername_inetSocket,58,SocketsPkg,"LL-SOCKET-PEERNAME-INET-SOCKET",true);
DO_SYMBOL(_sym_SO_REUSEPORT,59,SocketsPkg,"SO-REUSEPORT",true);
DO_SYMBOL(_sym_SO_KEEPALIVE,60,SocketsPkg,"SO-KEEPALIVE",true);
DO_SYMBOL(_sym__PLUS_brcl_stream_mode_io_PLUS_,61,SocketsPkg,"+BRCL-STREAM-MODE-IO+",true);
DO_SYMBOL(_sym_HOST_NOT_FOUND,62,SocketsPkg,"HOST-NOT-FOUND",true);
DO_SYMBOL(_sym__PLUS_af_inet_PLUS_,63,SocketsPkg,"+AF-INET+",true);
DO_SYMBOL(_sym_ETIMEDOUT,64,SocketsPkg,"ETIMEDOUT",true);
DO_SYMBOL(_sym_ll_socketBind_inetSocket,65,SocketsPkg,"LL-SOCKET-BIND-INET-SOCKET",true);
DO_SYMBOL(_sym_ll_getSockoptBool,66,SocketsPkg,"LL-GET-SOCKOPT-BOOL",true);
DO_SYMBOL(_sym_ll_socketReceive,67,SocketsPkg,"LL-SOCKET-RECEIVE",true);
DO_SYMBOL(_sym_SO_LINGER,68,SocketsPkg,"SO-LINGER",true);
DO_SYMBOL(_sym_NO_RECOVERY,69,SocketsPkg,"NO-RECOVERY",true);
DO_SYMBOL(_sym_ll_socket_errno,70,SocketsPkg,"LL-SOCKET-ERRNO",true);
DO_SYMBOL(_sym_ll_getHostByName,71,SocketsPkg,"LL-GET-HOST-BY-NAME",true);
DO_SYMBOL(_sym_ll_socketAccept_inetSocket,72,SocketsPkg,"LL-SOCKET-ACCEPT-INET-SOCKET",true);
DO_SYMBOL(_sym_NETDB_SUCCESS,73,SocketsPkg,"NETDB-SUCCESS",true);
DO_SYMBOL(_sym_ll_socketName,74,SocketsPkg,"LL-SOCKET-NAME",true);
DO_SYMBOL(_sym_TCP_NODELAY,75,SocketsPkg,"TCP-NODELAY",true);
DO_SYMBOL(_sym_EAGAIN,76,SocketsPkg,"EAGAIN",true);
#endif
