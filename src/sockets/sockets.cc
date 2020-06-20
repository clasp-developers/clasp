/*
    File: sockets.cc
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

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/time.h>
#include <netdb.h>
#include <string.h>
#include <unistd.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <errno.h>
#include <fcntl.h>
#ifndef MSG_CONFIRM
#define MSG_CONFIRM 0
#endif
#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL 0
#endif
#ifndef MSG_DONTWAIT
#define MSG_DONTWAIT 0
#endif
#ifndef MSG_EOR
#define MSG_EOR 0
#endif
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/array.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/evaluator.h>
#include <clasp/sockets/socketsPackage.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/wrappers.h>

namespace sockets {

#define REINTERPRET_CAST(t, c) reinterpret_cast<t>(c)

static void *
safe_buffer_pointer(core::T_sp x, uint size) {
  bool ok = false;
  void *address;
  if (core::Str8Ns_sp str = x.asOrNull<core::Str8Ns_O>()) {
    ok = (size <= str->arrayTotalSize());
    address = (void*)&(*str)[0]; // str->addressOfBuffer();
      } else if (core::SimpleBaseString_sp strb = x.asOrNull<core::SimpleBaseString_O>()) {
    ok = (size <= strb->arrayTotalSize());
    address = (void*)&(*strb)[0]; // str->addressOfBuffer();
  } else if (core::ComplexVector_T_sp vec = x.asOrNull<core::ComplexVector_T_O>()) {
    int divisor = vec->elementSizeInBytes();
    size = (size + divisor - 1) / divisor;
    ok = (size <= vec->arrayTotalSize());
    address = &(*vec)[0];
  } else {
    SIMPLE_ERROR(BF("Add support for buffer %s") % _rep_(x));
  }
  if (!ok) {
    SIMPLE_ERROR(BF("Lisp object does not have enough space to be a valid socket buffer: %s") % _rep_(x));
  }
  return address;
}

static void fill_inet_sockaddr(struct sockaddr_in *sockaddr, int port,
                               int a1, int a2, int a3, int a4) {
  bzero(sockaddr, sizeof(struct sockaddr_in));
  sockaddr->sin_family = AF_INET;
  sockaddr->sin_port = htons(port);
  sockaddr->sin_addr.s_addr = htonl((uint32_t)a1 << 24 | (uint32_t)a2 << 16 | (uint32_t)a3 << 8 | (uint32_t)a4);
}

CL_LAMBDA(domain type protocol);
CL_DECLARE();
CL_DOCSTRING("ff_socket");
CL_DEFUN int sockets_internal__ff_socket(int domain, int type, int protocol) {
  return ::socket(domain, type, protocol);
};

CL_LAMBDA(sockfd backlog);
CL_DECLARE();
CL_DOCSTRING("ff_listen");
CL_DEFUN int sockets_internal__ff_listen(int sockfd, int backlog) {
  return ::listen(sockfd, backlog);
};

CL_LAMBDA(filedes);
CL_DECLARE();
CL_DOCSTRING("ff_close");
CL_DEFUN int sockets_internal__ff_close(int filedes) {
  return ::close(filedes);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("funcName");
CL_DEFUN core::T_mv sockets_internal__funcName() {
  IMPLEMENT_MEF("Implement funcName");
};

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("ll_socket_errno");
CL_DEFUN int sockets_internal__ll_socket_errno() {
  /* Return the value of global variable errno */
  return errno;
};

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("ll_get_name_service_h_errno");
CL_DEFUN int sockets_internal__ll_get_name_service_h_errno() {
  /* Return the value of global variable h_errno */
  return h_errno;
};

CL_LAMBDA(num);
CL_DECLARE();
CL_DOCSTRING("ll_getNameServiceErrorMessage");
CL_DEFUN core::String_sp sockets_internal__ll_getNameServiceErrorMessage(int num) {
  return core::SimpleBaseString_O::make(strerror(num));
};

CL_LAMBDA(host-name host-ent setf-host-ent-name setf-host-ent-aliases setf-host-ent-address-type setf-host-ent-addresses);
CL_DECLARE();
CL_DOCSTRING("ll_getHostByName");
CL_DEFUN core::T_sp sockets_internal__ll_getHostByName(const string &hostName,                 // #0
                                             core::T_sp tHostEnt,                          // #1
                                             core::Function_sp setf_host_ent_name,         // #2
                                             core::Function_sp setf_host_ent_aliases,      // #3
                                             core::Function_sp setf_host_ent_address_type, // #4
                                             core::Function_sp setf_host_ent_addresses)    // #5
{
  struct hostent *hostent = gethostbyname(hostName.c_str());
  if (!hostent)
    return _Nil<core::T_O>();
  if (tHostEnt.notnilp()) {
    char **aliases;
    char **addrs;
    core::T_sp aliases_list = _Nil<core::T_O>();
    core::T_sp addr_list = _Nil<core::T_O>();
    int length = hostent->h_length;
    core::eval::funcall(setf_host_ent_name, core::SimpleBaseString_O::make(hostent->h_name), tHostEnt);
    core::eval::funcall(/*#4*/ setf_host_ent_address_type, core::Integer_O::create((gc::Fixnum)hostent->h_addrtype), tHostEnt);

    for (aliases = hostent->h_aliases; *aliases != NULL; aliases++) {
      aliases_list = core::Cons_O::create(core::SimpleBaseString_O::make(*aliases), aliases_list);
    }
    core::eval::funcall(/*#3*/ setf_host_ent_aliases, aliases_list, tHostEnt);

    for (addrs = hostent->h_addr_list; *addrs != NULL; addrs++) {
      int pos;
      core::Vector_sp vector = gc::As<core::Vector_sp>(core::eval::funcall(cl::_sym_makeArray, core::make_fixnum(length)));
      for (pos = 0; pos < length; pos++)
        vector->rowMajorAset(pos,core::make_fixnum((unsigned char)((*addrs)[pos])));
      addr_list = core::Cons_O::create(vector, addr_list);
    }
    core::eval::funcall(/*#5*/ setf_host_ent_addresses, addr_list, /*#1*/ tHostEnt);
    /* @(return) = #1; */
    return tHostEnt;
  } else {
    /* @(return) = _Nil<core::T_O>(); */
    return _Nil<core::T_O>();
  }
}

CL_LAMBDA(address host-ent stf-host-ent-name setf-host-ent-aliases setf-host-ent-address-type setf-host-ent-addresses);
CL_DECLARE();
CL_DOCSTRING("ll_getHostByAddress");
CL_DEFUN core::T_sp sockets_internal__ll_getHostByAddress(core::Vector_sp address,               //#0
                            core::T_sp tHostEnt,                   //#1
                            core::T_sp setf_host_ent_name,         //#2
                            core::T_sp setf_host_ent_aliases,      //#3
                            core::T_sp setf_host_ent_address_type, //#4
                            core::T_sp setf_host_ent_addresses)    //#5
{
  unsigned char vector[4];
  struct hostent *hostent;
  vector[0] = unbox_fixnum(gc::As<core::Fixnum_sp>(address->rowMajorAref(0)));
  vector[1] = unbox_fixnum(gc::As<core::Fixnum_sp>(address->rowMajorAref(1)));
  vector[2] = unbox_fixnum(gc::As<core::Fixnum_sp>(address->rowMajorAref(2)));
  vector[3] = unbox_fixnum(gc::As<core::Fixnum_sp>(address->rowMajorAref(3)));
  clasp_disable_interrupts();
  hostent = gethostbyaddr(REINTERPRET_CAST(const char *, vector), 4, AF_INET);
  clasp_enable_interrupts();
  if (hostent != NULL) {
    char **aliases;
    char **addrs;
    core::T_sp aliases_list = _Nil<core::T_O>();
    core::T_sp addr_list = _Nil<core::T_O>();
    int length = hostent->h_length;

    core::eval::funcall(/*#2*/ setf_host_ent_name, core::SimpleBaseString_O::make(hostent->h_name), tHostEnt);
    core::eval::funcall(/*#4*/ setf_host_ent_address_type, core::Integer_O::create((gc::Fixnum)hostent->h_addrtype), tHostEnt);

    for (aliases = hostent->h_aliases; *aliases != NULL; aliases++) {
      aliases_list = core::Cons_O::create(core::SimpleBaseString_O::make(*aliases), aliases_list);
    }
    core::eval::funcall(/*#3*/ setf_host_ent_aliases, aliases_list, tHostEnt);

    for (addrs = hostent->h_addr_list; *addrs != NULL; addrs++) {
      int pos;
      core::Vector_sp vector = gc::As<core::Vector_sp>(core::eval::funcall(cl::_sym_makeArray,core::make_fixnum(length)));
      for (pos = 0; pos < length; pos++)
        vector->rowMajorAset(pos,core::make_fixnum((unsigned char)((*addrs)[pos])));
      addr_list = core::Cons_O::create(vector, addr_list);
    }
    core::eval::funcall(/*#5*/ setf_host_ent_addresses, addr_list, tHostEnt);
    return tHostEnt;
    /* @(return) = #1; */
  } else {
    return _Nil<core::T_O>();
    /* @(return) = _Nil<core::T_O>(); */
  }
}

CL_LAMBDA(fd buffer length oob peek waitall);
CL_DECLARE();
CL_DOCSTRING("ll_socketReceive");
CL_DEFUN core::T_mv sockets_internal__ll_socketReceive(int fd,       // #0
                         core::T_sp buffer,  // #1
                         int length,   // #2
                         bool oob,     // #3
                         bool peek,    // #4
                         bool waitall) // #5
{
  int flags = (oob ? MSG_OOB : 0) |
              (peek ? MSG_PEEK : 0) |
              (waitall ? MSG_WAITALL : 0);
  cl_index len;
  clasp_disable_interrupts();
  len = recvfrom(fd, REINTERPRET_CAST(char *, safe_buffer_pointer(buffer, length)), length, flags, NULL, NULL);
  clasp_enable_interrupts();
  return Values(core::make_fixnum(len),core::make_fixnum(errno));
}

CL_LAMBDA(name);
CL_DECLARE();
CL_DOCSTRING("ll_getProtocolByName");
CL_DEFUN int sockets_internal__ll_getProtocolByName(const string &name) {
  return getprotobyname(name.c_str())->p_proto;
}

CL_LAMBDA(port ip0 ip1 ip2 ip3 socketFileDescriptor);
CL_DECLARE();
CL_DOCSTRING("ll_socketBind - returns output");
CL_DEFUN int sockets_internal__ll_socketBind_inetSocket(int port, int ip0, int ip1, int ip2, int ip3, int socketFileDescriptor) {
  struct sockaddr_in sockaddr;
  int output;
  clasp_disable_interrupts();
  fill_inet_sockaddr(&sockaddr, port, ip0, ip1, ip2, ip3);
  output = ::bind(socketFileDescriptor, (struct sockaddr *)&sockaddr, sizeof(struct sockaddr_in));
  clasp_enable_interrupts();
  return output;
}

CL_LAMBDA(sfd);
CL_DECLARE();
CL_DOCSTRING("ll_socketAccept_inetSocket");
CL_DEFUN core::T_mv sockets_internal__ll_socketAccept_inetSocket(int sfd) {
  struct sockaddr_in sockaddr;
  socklen_t addr_len = (socklen_t)sizeof(struct sockaddr_in);
  int new_fd;

  clasp_disable_interrupts();
  new_fd = accept(sfd, (struct sockaddr *)&sockaddr, &addr_len);
  clasp_enable_interrupts();

  int return0 = new_fd;
  core::T_sp return1 = _Nil<core::T_O>();
  int return2 = 0;
  if (new_fd != -1) {
    uint32_t ip = ntohl(sockaddr.sin_addr.s_addr);
    uint16_t port = ntohs(sockaddr.sin_port);
    core::Vector_sp vector = gc::As<core::Vector_sp>(core::eval::funcall(cl::_sym_makeArray,core::make_fixnum(4)));
    vector->rowMajorAset(0,core::make_fixnum(ip >> 24));
    vector->rowMajorAset(1,core::make_fixnum((ip >> 16) & 0xFF));
    vector->rowMajorAset(2,core::make_fixnum((ip >> 8) & 0xFF));
    vector->rowMajorAset(3,core::make_fixnum(ip & 0xFF));
    return1 = vector;
    return2 = port;
  }
  return Values(core::Integer_O::create((gc::Fixnum)return0), return1, core::Integer_O::create((gc::Fixnum)return2));
}

CL_LAMBDA(port ip0 ip1 ip2 ip3 socket-file-descriptor);
CL_DECLARE();
CL_DOCSTRING("ll_socketConnect_inetSocket");
CL_DEFUN int sockets_internal__ll_socketConnect_inetSocket(int port, int ip0, int ip1, int ip2, int ip3, int socket_file_descriptor) {
  struct sockaddr_in sockaddr;
  int output;
  clasp_disable_interrupts();
  fill_inet_sockaddr(&sockaddr, port, ip0, ip1, ip2, ip3);
  output = connect(socket_file_descriptor, (struct sockaddr *)&sockaddr, sizeof(struct sockaddr_in));
  clasp_enable_interrupts();
  return output;
}

CL_LAMBDA(fd vector);
CL_DECLARE();
CL_DOCSTRING("ll_socketPeername_inetSocket");
CL_DEFUN int sockets_internal__ll_socketPeername_inetSocket(int fd, core::Vector_sp vector) {
  /* @01; */
  struct sockaddr_in name;
  socklen_t len = sizeof(struct sockaddr_in);
  int ret;
  clasp_disable_interrupts();
  ret = getpeername(fd, (struct sockaddr *)&name, &len);
  clasp_enable_interrupts();

  if (ret == 0) {
    uint32_t ip = ntohl(name.sin_addr.s_addr);
    uint16_t port = ntohs(name.sin_port);

    vector->rowMajorAset(0,core::make_fixnum(ip >> 24));
    vector->rowMajorAset(1,core::make_fixnum((ip >> 16) & 0xFF));
    vector->rowMajorAset(2,core::make_fixnum((ip >> 8) & 0xFF));
    vector->rowMajorAset(3,core::make_fixnum(ip & 0xFF));
    return port;
  } else {
    return -1;
  }
}

CL_LAMBDA(fd vector);
CL_DECLARE();
CL_DOCSTRING("ll_socketName");
CL_DEFUN int sockets_internal__ll_socketName(int fd, core::Vector_sp vector) {
  /* @01 */
  struct sockaddr_in name;
  socklen_t len = sizeof(struct sockaddr_in);
  int ret;

  clasp_disable_interrupts();
  ret = getsockname(fd, (struct sockaddr *)&name, &len);
  clasp_enable_interrupts();

  if (ret == 0) {
    uint32_t ip = ntohl(name.sin_addr.s_addr);
    uint16_t port = ntohs(name.sin_port);

    vector->rowMajorAset(0,core::make_fixnum(ip >> 24));
    vector->rowMajorAset(1,core::make_fixnum((ip >> 16) & 0xFF));
    vector->rowMajorAset(2,core::make_fixnum((ip >> 8) & 0xFF));
    vector->rowMajorAset(3,core::make_fixnum(ip & 0xFF));

    return port;
  } else {
    return -1;
  }
}

CL_LAMBDA(fd vbuffer length secondAddress ip0 ip1 ip2 ip3 oob eor dontroute dontwait nosignal confirm);
CL_DECLARE();
CL_DOCSTRING("ll_socketSendAddress");
CL_DEFUN core::Integer_sp sockets_internal__ll_socketSendAddress(int fd,            //#0
                                   core::Vector_sp vbuffer, //#1
                                   int length,        //#2
                                   int secondAddress, //#3
                                   int ip0,           //#4
                                   int ip1,           //#5
                                   int ip2,           //#6
                                   int ip3,           //#7
                                   bool oob,          //#8
                                   bool eor,          //#9
                                   bool dontroute,    //#a
                                   bool dontwait,     //#b
                                   bool nosignal,     //#c
                                   bool confirm)      //#d
{
  /*		 (c-inline (fd buffer length
		 (second address)
		 (aref (first address) 0)
		 (aref (first address) 1)
		 (aref (first address) 2)
		 (aref (first address) 3)
		 oob eor dontroute dontwait nosignal confirm)
		 (:int :object :int
		 :int :int :int :int :int
		 :bool :bool :bool :bool :bool :bool)
		 :long
*/
  int sock = fd;
  void *buffer = safe_buffer_pointer(vbuffer, length);
  int flags = (oob ? MSG_OOB : 0) |
              (eor ? MSG_EOR : 0) |
              (dontroute ? MSG_DONTROUTE : 0) |
              (dontwait ? MSG_DONTWAIT : 0) |
              (nosignal ? MSG_NOSIGNAL : 0) |
              (confirm ? MSG_CONFIRM : 0);
  //    cl_type type = type_of(#1);
  struct sockaddr_in sockaddr;
  ssize_t len;

  clasp_disable_interrupts();
  fill_inet_sockaddr(&sockaddr, secondAddress, ip0, ip1, ip2, ip3);
#if (MSG_NOSIGNAL == 0) && defined(SO_NOSIGPIPE)
  {
    int sockopt = nosignal;
    setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE,
               REINTERPRET_CAST(char *, &sockopt),
               sizeof(int));
  }
#endif
  len = sendto(sock, REINTERPRET_CAST(char *, buffer),
               length, flags, (struct sockaddr *)&sockaddr,
               sizeof(struct sockaddr_in));
  clasp_enable_interrupts();
  return core::Integer_O::create((gc::Fixnum)(len));
}

CL_LAMBDA(fb buffer length oob eor dontroute dontwait nosignal confirm);
CL_DECLARE();
CL_DOCSTRING("ll_socketSendNoAddress");
CL_DEFUN core::Integer_sp sockets_internal__ll_socketSendNoAddress(int fb,            //#0
                                  core::Vector_sp vbuffer, //#1
                                  int length,        //#2
                                  bool oob,          //#3
                                  bool eor,          //#4
                                  bool dontroute,    //#5
                                  bool dontwait,     //#6
                                  bool nosignal,     //#7
                                  bool confirm       //#8
                                  ) {
  int sock = fb;
  void *buffer = safe_buffer_pointer(vbuffer, length);
  int flags = (oob ? MSG_OOB : 0) |
              (eor ? MSG_EOR : 0) |
              (dontroute ? MSG_DONTROUTE : 0) |
              (dontwait ? MSG_DONTWAIT : 0) |
              (nosignal ? MSG_NOSIGNAL : 0) |
              (confirm ? MSG_CONFIRM : 0);
  ssize_t len;
  clasp_disable_interrupts();
#if (MSG_NOSIGNAL == 0) && defined(SO_NOSIGPIPE)
  {
    int sockopt = nosignal;
    setsockopt(fb, SOL_SOCKET, SO_NOSIGPIPE,
               REINTERPRET_CAST(char *, &sockopt),
               sizeof(int));
  }
#endif
  len = send(sock, REINTERPRET_CAST(char *, buffer), length, flags);
  clasp_enable_interrupts();
  return core::Integer_O::create((gc::Fixnum)(len));
}

CL_LAMBDA(fd name family);
CL_DECLARE();
CL_DOCSTRING("ll_socketBind_localSocket");
CL_DEFUN int sockets_internal__ll_socketBind_localSocket(int fd, const string &name, int family) {
  struct sockaddr_un sockaddr;
  //	size_t size;
  int output;
#ifdef BSD
  sockaddr.sun_len = sizeof(struct sockaddr_un);
#endif
  sockaddr.sun_family = family;
  strncpy(sockaddr.sun_path, name.c_str(), sizeof(sockaddr.sun_path));
  sockaddr.sun_path[sizeof(sockaddr.sun_path) - 1] = '\0';

  clasp_disable_interrupts();
  output = ::bind(fd, (struct sockaddr *)&sockaddr, sizeof(struct sockaddr_un));
  clasp_enable_interrupts();
  return output;
}

CL_LAMBDA(socket-file-descriptor);
CL_DECLARE();
CL_DOCSTRING("ll_socketAccept_localSocket");
CL_DEFUN core::T_mv sockets_internal__ll_socketAccept_localSocket(int socketFileDescriptor) {
  struct sockaddr_un sockaddr;
  socklen_t addr_len = (socklen_t)sizeof(struct sockaddr_un);
  int new_fd;
  clasp_disable_interrupts();
  new_fd = accept(socketFileDescriptor, (struct sockaddr *)&sockaddr, &addr_len);
  clasp_enable_interrupts();
  core::T_sp second_ret = _Nil<core::T_O>();
  if (new_fd != -1) {
    second_ret = core::SimpleBaseString_O::make(sockaddr.sun_path);
  }
  return Values(core::Integer_O::create((gc::Fixnum)new_fd), second_ret);
}

CL_LAMBDA(fd family path);
CL_DECLARE();
CL_DOCSTRING("ll_socketConnect_localSocket");
CL_DEFUN int sockets_internal__ll_socketConnect_localSocket(int fd, int family, const string &path) {
  struct sockaddr_un sockaddr;
  int output;
#ifdef BSD
  sockaddr.sun_len = sizeof(struct sockaddr_un);
#endif
  sockaddr.sun_family = family;
  strncpy(sockaddr.sun_path, path.c_str(), sizeof(sockaddr.sun_path));
  sockaddr.sun_path[sizeof(sockaddr.sun_path) - 1] = '\0';

  clasp_disable_interrupts();
  output = connect(fd, (struct sockaddr *)&sockaddr, sizeof(struct sockaddr_un));
  clasp_enable_interrupts();

  return output;
}

CL_LAMBDA(fd);
CL_DECLARE();
CL_DOCSTRING("ll_socketPeername_localSocket");
CL_DEFUN core::T_sp sockets_internal__ll_socketPeername_localSocket(int fd) {
  struct sockaddr_un name;
  socklen_t len = sizeof(struct sockaddr_un);
  int ret;

  clasp_disable_interrupts();
  ret = getpeername(fd, (struct sockaddr *)&name, &len);
  clasp_enable_interrupts();

  if (ret == 0) {
    return core::SimpleBaseString_O::make(name.sun_path);
  } else {
    return _Nil<core::T_O>();
  }
}

CL_LAMBDA(fd);
CL_DECLARE();
CL_DOCSTRING("ll_nonBlockingMode");
CL_DEFUN int sockets_internal__ll_nonBlockingMode(int fd) {
  return fcntl(fd, F_GETFL, NULL) & O_NONBLOCK;
}

CL_LAMBDA(fd nblock);
CL_DECLARE();
CL_DOCSTRING("ll_setfNonBlockingMode");
CL_DEFUN int sockets_internal__ll_setfNonBlockingMode(int fd, int nblock) {
  int oldflags = fcntl(fd, F_GETFL, NULL);
  int newflags = (oldflags & ~O_NONBLOCK) |
                 (nblock ? O_NONBLOCK : 0);
  clasp_disable_interrupts();
  int ret = fcntl(fd, F_SETFL, newflags);
  clasp_enable_interrupts();
  return ret;
}

CL_LAMBDA(fd);
CL_DECLARE();
CL_DOCSTRING("ll_dup");
CL_DEFUN int sockets_internal__ll_dup(int fd) {
  return dup(fd);
}

CL_LAMBDA(name fd stream-mode element-type external-format);
CL_DECLARE();
CL_DOCSTRING("ll_makeStreamFromFd");
CL_DEFUN core::T_sp sockets_internal__ll_makeStreamFromFd(const string &name,  //#0
                            int fd,              //#1
                            int streamMode,      //#2
                            core::T_sp elementType,    //#3
                            core::T_sp externalFormat) //#4
{
  core::StreamMode direction;
  switch (streamMode) {
  case core::clasp_stream_mode_input:
      direction = core::clasp_smm_input;
    break;
  case core::clasp_stream_mode_output:
      direction = core::clasp_smm_output;
    break;
  case core::clasp_stream_mode_io:
      direction = core::clasp_smm_io;
    break;
  default: {
    SIMPLE_ERROR(BF("Illegal stream mode %d") % streamMode);
  }
  }
  core::Stream_sp stream = gc::As_unsafe<core::Stream_sp>(core::IOFileStream_O::make(name, fd, direction, elementType, externalFormat));
  return stream;
}

CL_LAMBDA(stream);
CL_DECLARE();
CL_DOCSTRING("ll_autoCloseTwoWayStream");
CL_DEFUN void sockets_internal__ll_autoCloseTwoWayStream(core::Stream_sp stream) {
#if 0
	IMPLEMENT_MEF("Handle ECL_STREAM_CLOSE_COMPONENTS");
	stream->stream.flags |= ECL_STREAM_CLOSE_COMPONENTS;
#endif
};

CL_LAMBDA(num);
CL_DECLARE();
CL_DOCSTRING("ll_strerror");
CL_DEFUN core::String_sp sockets_internal__ll_strerror(int num) {
  return core::SimpleBaseString_O::make(strerror(num));
}

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("ll_strerror_errno");
CL_DEFUN core::String_sp sockets_internal__ll_strerror_errno() {
  return core::SimpleBaseString_O::make(strerror(errno));
}

CL_LAMBDA(fd level constant);
CL_DECLARE();
CL_DOCSTRING("ll_getSockoptInt");
CL_DEFUN core::Integer_sp sockets_internal__ll_getSockoptInt(int fd, int level, int constant) {
  /* (c-inline (fd level const) (:int :int :int) t   ) */
  int sockopt, ret;
  socklen_t socklen = sizeof(int);

  clasp_disable_interrupts();
  ret = getsockopt(fd, level, constant, REINTERPRET_CAST(char *, &sockopt), &socklen);
  clasp_enable_interrupts();

  return (ret == 0) ? core::Integer_O::create((gc::Fixnum)sockopt) : _Nil<core::Integer_O>();
}

CL_LAMBDA(fd level constant);
CL_DECLARE();
CL_DOCSTRING("ll_getSockoptBool");
CL_DEFUN core::T_sp sockets_internal__ll_getSockoptBool(int fd, int level, int constant) {
  int sockopt, ret;
  socklen_t socklen = sizeof(int);

  clasp_disable_interrupts();
  ret = getsockopt(fd, level, constant, REINTERPRET_CAST(char *, &sockopt), &socklen);
  clasp_enable_interrupts();
  return (ret == 0) ? _lisp->_boolean(sockopt) : _Nil<core::T_O>();
}

CL_LAMBDA(fd level constant);
CL_DECLARE();
CL_DOCSTRING("ll_getSockoptTimeval");
CL_DEFUN core::DoubleFloat_sp sockets_internal__ll_getSockoptTimeval(int fd, int level, int constant) {
  struct timeval tv;
  socklen_t socklen = sizeof(struct timeval);
  int ret;
  clasp_disable_interrupts();
  ret = getsockopt(fd, level, constant, REINTERPRET_CAST(char *, &tv), &socklen);
  clasp_enable_interrupts();
  return (ret == 0) ? core::DoubleFloat_O::create((double)tv.tv_sec + ((double)tv.tv_usec) / 1000000.0) : _Nil<core::DoubleFloat_O>();
}

CL_LAMBDA(fd level constant);
CL_DECLARE();
CL_DOCSTRING("ll_getSockoptLinger");
CL_DEFUN core::Integer_sp sockets_internal__ll_getSockoptLinger(int fd, int level, int constant) {
  struct linger sockopt;
  socklen_t socklen = sizeof(struct linger);
  int ret;

  clasp_disable_interrupts();
  ret = getsockopt(fd, level, constant, REINTERPRET_CAST(char *, &sockopt), &socklen);
  clasp_enable_interrupts();

  return (ret == 0) ? core::Integer_O::create((gc::Fixnum)((sockopt.l_onoff != 0) ? sockopt.l_linger : 0)) : _Nil<core::Integer_O>();
}

CL_LAMBDA(fd level constant value);
CL_DECLARE();
CL_DOCSTRING("ll_setSockoptInt");
CL_DEFUN bool sockets_internal__ll_setSockoptInt(int fd, int level, int constant, int value) {
  int sockopt = value;
  int ret;

  clasp_disable_interrupts();
  ret = setsockopt(fd, level, constant, REINTERPRET_CAST(char *, &sockopt), sizeof(int));
  clasp_enable_interrupts();

  return (ret == 0) ? true : false;
}

CL_LAMBDA(fd level constant value);
CL_DECLARE();
CL_DOCSTRING("ll_setSockoptBool");
CL_DEFUN bool sockets_internal__ll_setSockoptBool(int fd, int level, int constant, bool value) {
  int sockopt = value ? 1 : 0;
  int ret;

  clasp_disable_interrupts();
  ret = setsockopt(fd, level, constant, REINTERPRET_CAST(char *, &sockopt), sizeof(int));
  clasp_enable_interrupts();

  return (ret == 0) ? true : false;
}

CL_LAMBDA(fd level constant value);
CL_DECLARE();
CL_DOCSTRING("ll_setSockoptTimeval");
CL_DEFUN bool sockets_internal__ll_setSockoptTimeval(int fd, int level, int constant, double value) {
  struct timeval tv;
  double tmp = value;
  int ret;

  clasp_disable_interrupts();
  tv.tv_sec = (int)tmp;
  tv.tv_usec = (int)((tmp - floor(tmp)) * 1000000.0);
  ret = setsockopt(fd, level, constant, &tv, sizeof(struct timeval));
  clasp_enable_interrupts();

  return (ret == 0) ? true : false;
}

CL_LAMBDA(fd level constant value);
CL_DECLARE();
CL_DOCSTRING("ll_setSockoptLinger");
CL_DEFUN bool sockets_internal__ll_setSockoptLinger(int fd, int level, int constant, int value) {
  struct linger sockopt = {0, 0};
  int ret;

  if (value > 0) {
    sockopt.l_onoff = 1;
    sockopt.l_linger = value;
  }

  clasp_disable_interrupts();
  ret = setsockopt(fd, level, constant, REINTERPRET_CAST(char *, &sockopt),
                   sizeof(struct linger));
  clasp_enable_interrupts();

  return (ret == 0) ? true : false;
}


CL_DEFUN int sockets_internal__shutdown(int sock_fd, int direction_flag) {
  return shutdown(sock_fd,direction_flag);
}

CL_DEFUN int sockets_internal__errno()
{
  return errno;
}

CL_DEFUN gc::Fixnum sockets_internal__fd_setsize() {
  return FD_SETSIZE;
}

CL_DEFUN void* sockets_internal__alloc_atomic_sizeof_fd_set() {
  return (void*)malloc(sizeof(fd_set));
}

CL_DEFUN void sockets_internal__fdset_zero(core::Pointer_sp p)
{
  FD_ZERO((fd_set*)p->ptr());
}

CL_DEFUN void sockets_internal__fdset_set(gc::Fixnum fd, core::Pointer_sp fdset) {
  FD_SET(fd,(fd_set*)fdset->ptr());
}

CL_DEFUN void sockets_internal__fdset_clr(gc::Fixnum fd, core::Pointer_sp fdset) {
  FD_CLR(fd,(fd_set*)fdset->ptr());
}

CL_DEFUN bool sockets_internal__fdset_isset(gc::Fixnum fd, core::Pointer_sp fdset) {
  return FD_ISSET(fd,(fd_set*)fdset->ptr());
}

CL_DEFUN core::T_sp sockets_internal__get_host_name() {
  char* buf = (char*)malloc(257);
  if (gethostname(buf,256) == 0 ) {
    core::T_sp result = core::SimpleBaseString_O::make(std::string(buf));
    free(buf);
    return result;
  }
  free(buf);
  return _Nil<core::T_O>();
}

CL_DEFUN int sockets_internal__do_select(core::T_sp to_secs,
                                         unsigned int to_musecs,
                                         core::Pointer_sp rfds,
                                         int max_fd) {
  int count;
  struct timeval tv;
  if (to_secs.fixnump()) {
    tv.tv_sec = to_secs.unsafe_fixnum();
    tv.tv_usec = to_musecs;
  }
  return select(max_fd + 1, (fd_set*)rfds->ptr(), NULL, NULL,
                (to_secs.fixnump()) ? &tv : NULL);
}


void initialize_sockets_globals() {
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_af_inet_PLUS_);
  _sym__PLUS_af_inet_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)AF_INET));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_af_local_PLUS_);
  _sym__PLUS_af_local_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)AF_UNIX));

  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_sock_dgram_PLUS_);
  _sym__PLUS_sock_dgram_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)SOCK_DGRAM));

  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_sock_stream_PLUS_);
  _sym__PLUS_sock_stream_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)SOCK_STREAM));

  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_clasp_stream_mode_input_PLUS_);
  _sym__PLUS_clasp_stream_mode_input_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)(core::clasp_stream_mode_input)));

  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_clasp_stream_mode_output_PLUS_);
  _sym__PLUS_clasp_stream_mode_output_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)core::clasp_stream_mode_output));

  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_clasp_stream_mode_io_PLUS_);
  _sym__PLUS_clasp_stream_mode_io_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)core::clasp_stream_mode_io));

  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_EADDRINUSE_PLUS_);
  _sym__PLUS_EADDRINUSE_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)EADDRINUSE));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_EAGAIN_PLUS_);
  _sym__PLUS_EAGAIN_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)EAGAIN));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_EBADF_PLUS_);
  _sym__PLUS_EBADF_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)EBADF));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_ECONNREFUSED_PLUS_);
  _sym__PLUS_ECONNREFUSED_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)ECONNREFUSED));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_ETIMEDOUT_PLUS_);
  _sym__PLUS_ETIMEDOUT_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)ETIMEDOUT));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_EINTR_PLUS_);
  _sym__PLUS_EINTR_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)EINTR));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_EINVAL_PLUS_);
  _sym__PLUS_EINVAL_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)EINVAL));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_ENOBUFS_PLUS_);
  _sym__PLUS_ENOBUFS_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)ENOBUFS));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_ENOMEM_PLUS_);
  _sym__PLUS_ENOMEM_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)ENOMEM));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_EOPNOTSUPP_PLUS_);
  _sym__PLUS_EOPNOTSUPP_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)EOPNOTSUPP));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_EPERM_PLUS_);
  _sym__PLUS_EPERM_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)EPERM));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_EPROTONOSUPPORT_PLUS_);
  _sym__PLUS_EPROTONOSUPPORT_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)EPROTONOSUPPORT));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_ESOCKTNOSUPPORT_PLUS_);
  _sym__PLUS_ESOCKTNOSUPPORT_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)ESOCKTNOSUPPORT));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_ENETUNREACH_PLUS_);
  _sym__PLUS_ENETUNREACH_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)ENETUNREACH));

  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_NETDB_INTERNAL_PLUS_);
  _sym__PLUS_NETDB_INTERNAL_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)NETDB_INTERNAL));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_NETDB_SUCCESS_PLUS_);
  _sym__PLUS_NETDB_SUCCESS_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)NETDB_SUCCESS));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_HOST_NOT_FOUND_PLUS_);
  _sym__PLUS_HOST_NOT_FOUND_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)HOST_NOT_FOUND));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_TRY_AGAIN_PLUS_);
  _sym__PLUS_TRY_AGAIN_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)TRY_AGAIN));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_NO_RECOVERY_PLUS_);
  _sym__PLUS_NO_RECOVERY_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)NO_RECOVERY));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_NO_ADDRESS_PLUS_);
  _sym__PLUS_NO_ADDRESS_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)NO_ADDRESS));

  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_SOL_SOCKET_PLUS_);
  _sym__PLUS_SOL_SOCKET_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)SOL_SOCKET));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_IPPROTO_TCP_PLUS_);
  _sym__PLUS_IPPROTO_TCP_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)IPPROTO_TCP));

  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_SO_TYPE_PLUS_);
  _sym__PLUS_SO_TYPE_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)SO_TYPE));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_SO_RCVBUF_PLUS_);
  _sym__PLUS_SO_RCVBUF_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)SO_RCVBUF));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_SO_RCVTIMEO_PLUS_);
  _sym__PLUS_SO_RCVTIMEO_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)SO_RCVTIMEO));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_SO_SNDTIMEO_PLUS_);
  _sym__PLUS_SO_SNDTIMEO_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)SO_SNDTIMEO));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_SO_REUSEADDR_PLUS_);
  _sym__PLUS_SO_REUSEADDR_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)SO_REUSEADDR));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_SO_KEEPALIVE_PLUS_);
  _sym__PLUS_SO_KEEPALIVE_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)SO_KEEPALIVE));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_SO_DONTROUTE_PLUS_);
  _sym__PLUS_SO_DONTROUTE_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)SO_DONTROUTE));
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_SO_LINGER_PLUS_);
  _sym__PLUS_SO_LINGER_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)SO_LINGER));
#ifndef _TARGET_OS_LINUX
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_SO_REUSEPORT_PLUS_);
  _sym__PLUS_SO_REUSEPORT_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)SO_REUSEPORT));
#endif
  SYMBOL_EXPORT_SC_(SocketsPkg, _PLUS_TCP_NODELAY_PLUS_);
  _sym__PLUS_TCP_NODELAY_PLUS_->defconstant(core::Integer_O::create((gc::Fixnum)TCP_NODELAY));
};

  SYMBOL_EXPORT_SC_(SocketsPkg, ff_socket);
  SYMBOL_EXPORT_SC_(SocketsPkg, ff_listen);
  SYMBOL_EXPORT_SC_(SocketsPkg, ff_close);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_get_name_service_h_errno);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_socket_errno);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_getNameServiceErrorMessage);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_getHostByName);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_getHostByAddress);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_socketReceive);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_getProtocolByName);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_socketBind_inetSocket);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_socketAccept_inetSocket);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_socketConnect_inetSocket);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_socketPeername_inetSocket);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_socketName);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_socketSendAddress);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_socketSendNoAddress);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_socketBind_localSocket);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_socketAccept_localSocket);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_socketConnect_localSocket);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_socketPeername_localSocket);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_nonBlockingMode);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_setfNonBlockingMode);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_dup);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_makeStreamFromFd);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_autoCloseTwoWayStream);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_strerror);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_strerror_errno);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_getSockoptInt);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_getSockoptBool);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_getSockoptTimeval);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_getSockoptLinger);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_setSockoptInt);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_setSockoptBool);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_setSockoptTimeval);
  SYMBOL_EXPORT_SC_(SocketsPkg, ll_setSockoptLinger);
};
