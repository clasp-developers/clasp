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
#include "core/foundation.h"
#include "core/object.h"
#include "core/str.h"
#include "core/symbolTable.h"
#include "core/lispStream.h"
#include "core/evaluator.h"
#include "core/lispVector.h"
#include "socketsPackage.h"
#include "symbolTable.h"
#include "core/wrappers.h"


namespace sockets {

#define REINTERPRET_CAST(t,c) reinterpret_cast<t>(c)

    using namespace core;

    static void *
    safe_buffer_pointer(core::T_sp x, uint size)
    {
	bool ok = false;
	void* address;
	if ( Str_sp str = x.asOrNull<Str_O>() )
	{
	    ok = (size <= str->dimension());
	    address = str->addressOfBuffer();
	} else if ( Vector_sp vec = x.asOrNull<Vector_O>() )
	{
	    int divisor = vec->elementSizeInBytes();
	    size = (size + divisor - 1) / divisor;
	    ok = (size <= vec->dimension());
	    address = vec->addressOfBuffer();
	}
	if (!ok) {
	    SIMPLE_ERROR(BF("Lisp object does not have enough space to be a valid socket buffer: %s") % _rep_(x));
	}
	return address;
    }



    static void fill_inet_sockaddr(struct sockaddr_in *sockaddr, int port,
				   int a1, int a2, int a3, int a4)
    {
	bzero(sockaddr,sizeof(struct sockaddr_in));
	sockaddr->sin_family = AF_INET;
	sockaddr->sin_port = htons(port);
	sockaddr->sin_addr.s_addr= htonl((uint32_t)a1<<24 | (uint32_t)a2<<16 | (uint32_t)a3<<8 | (uint32_t)a4) ;
    }


    
    
#define ARGS_af_ff_socket "(domain type protocol)"
#define DECL_af_ff_socket ""
#define DOCS_af_ff_socket "ff_socket"
    int af_ff_socket(int domain, int type, int protocol)
    {_G();
	return ::socket(domain,type,protocol);
    };


    
    
#define ARGS_af_ff_listen "(sockfd backlog)"
#define DECL_af_ff_listen ""
#define DOCS_af_ff_listen "ff_listen"
    int af_ff_listen(int sockfd, int backlog)
    {_G();
	return ::listen(sockfd,backlog);
    };




    
    
#define ARGS_af_ff_close "(filedes)"
#define DECL_af_ff_close ""
#define DOCS_af_ff_close "ff_close"
    int af_ff_close(int filedes)
    {_G();
	return ::close(filedes);
    };



    
    
#define ARGS_af_funcName "(arg)"
#define DECL_af_funcName ""
#define DOCS_af_funcName "funcName"
    T_mv af_funcName()
    {_G();
	IMPLEMENT_MEF(BF("Implement funcName"));
    };



#define ARGS_af_ll_socket_errno "()"
#define DECL_af_ll_socket_errno ""
#define DOCS_af_ll_socket_errno "ll_socket_errno"
    int af_ll_socket_errno()
    {_G();
	/* Return the value of global variable errno */
	return errno;
    };

#define ARGS_af_ll_get_name_service_h_errno "()"
#define DECL_af_ll_get_name_service_h_errno ""
#define DOCS_af_ll_get_name_service_h_errno "ll_get_name_service_h_errno"
    int af_ll_get_name_service_h_errno()
    {_G();
	/* Return the value of global variable h_errno */
	return h_errno;
    };





#define ARGS_af_ll_getNameServiceErrorMessage "(num)"
#define DECL_af_ll_getNameServiceErrorMessage ""
#define DOCS_af_ll_getNameServiceErrorMessage "ll_getNameServiceErrorMessage"
    Str_sp af_ll_getNameServiceErrorMessage(int num)
    {_G();
	return Str_O::create(strerror(num));
    };






#define ARGS_af_ll_getHostByName "(host-name host-ent setf_host_ent_name setf_host_ent_aliases setf_host_ent_address_type setf_host_ent_addresses)"
#define DECL_af_ll_getHostByName ""
#define DOCS_af_ll_getHostByName "ll_getHostByName"
    T_sp af_ll_getHostByName(const string& hostName, 		// #0
			     T_sp tHostEnt,				// #1
			     Function_sp setf_host_ent_name, 	// #2
			     Function_sp setf_host_ent_aliases, 	// #3
			     Function_sp setf_host_ent_address_type,// #4
			     Function_sp setf_host_ent_addresses ) 	// #5
    {_G();
	struct hostent *hostent = gethostbyname(hostName.c_str());
	if (tHostEnt.notnilp()) {
	    char **aliases;
	    char **addrs;
	    T_sp aliases_list = _Nil<core::T_O>();
	    T_sp addr_list = _Nil<core::T_O>();
	    int length = hostent->h_length;
	    eval::funcall(setf_host_ent_name,Str_O::create(hostent->h_name),tHostEnt);
	    eval::funcall(/*#4*/setf_host_ent_address_type,Integer_O::create(hostent->h_addrtype),tHostEnt);

	    for (aliases = hostent->h_aliases; *aliases != NULL; aliases++) {
		aliases_list = Cons_O::create(Str_O::create(*aliases),aliases_list);
	    }
	    eval::funcall(/*#3*/setf_host_ent_aliases,aliases_list,tHostEnt);

	    for (addrs = hostent->h_addr_list; *addrs != NULL; addrs++) {
		int pos;
		Vector_sp vector = eval::funcall(cl::_sym_makeArray,Fixnum_O::create(length)).as<Vector_O>();
		for (pos = 0; pos < length; pos++)
		    vector->operator[](pos) = Fixnum_O::create((unsigned char)((*addrs)[pos]));
		addr_list = Cons_O::create(vector, addr_list);
	    }
	    eval::funcall(/*#5*/ setf_host_ent_addresses,addr_list,/*#1*/ tHostEnt);
	    /* @(return) = #1; */
	    return tHostEnt;
	} else {
	    /* @(return) = _Nil<core::T_O>(); */
	    return _Nil<T_O>();
	}
    }







#define ARGS_af_ll_getHostByAddress "(address host-ent stf_host_ent_name setf_host_ent_aliases setf_host_ent_address_type setf_host_ent_addresses)"
#define DECL_af_ll_getHostByAddress ""
#define DOCS_af_ll_getHostByAddress "ll_getHostByAddress"
    T_sp af_ll_getHostByAddress(Vector_sp address,			//#0
				T_sp tHostEnt,			//#1
				T_sp setf_host_ent_name,		//#2
				T_sp setf_host_ent_aliases,		//#3
				T_sp setf_host_ent_address_type,	//#4
				T_sp setf_host_ent_addresses) 	//#5
    {_G();
	unsigned char vector[4];
	struct hostent *hostent;
	vector[0] = address->operator[](0).as<Fixnum_O>()->get();
	vector[1] = address->operator[](1).as<Fixnum_O>()->get();
	vector[2] = address->operator[](2).as<Fixnum_O>()->get();
	vector[3] = address->operator[](3).as<Fixnum_O>()->get();
	brcl_disable_interrupts();
	hostent = gethostbyaddr(REINTERPRET_CAST(const char *, vector),4,AF_INET);
	brcl_enable_interrupts();
	if (hostent != NULL) {
	    char **aliases;
	    char **addrs;
	    T_sp aliases_list = _Nil<core::T_O>();
	    T_sp addr_list = _Nil<core::T_O>();
	    int length = hostent->h_length;

	    eval::funcall(/*#2*/setf_host_ent_name,Str_O::create(hostent->h_name),tHostEnt);
	    eval::funcall(/*#4*/setf_host_ent_address_type,Integer_O::create(hostent->h_addrtype),tHostEnt);

	    for (aliases = hostent->h_aliases; *aliases != NULL; aliases++) {
		aliases_list = Cons_O::create(Str_O::create(*aliases),aliases_list);
	    }
	    eval::funcall(/*#3*/setf_host_ent_aliases,aliases_list,tHostEnt);

	    for (addrs = hostent->h_addr_list; *addrs != NULL; addrs++) {
		int pos;
		Vector_sp vector = eval::funcall(cl::_sym_makeArray,Fixnum_O::create(length)).as<Vector_O>();
		for (pos = 0; pos < length; pos++)
		    vector->setf_elt(pos, Fixnum_O::create((unsigned char)((*addrs)[pos])));
		addr_list = Cons_O::create(vector, addr_list);
	    }
	    eval::funcall(/*#5*/setf_host_ent_addresses,addr_list,tHostEnt);
	    return tHostEnt;
	    /* @(return) = #1; */
	} else {
	    return _Nil<core::T_O>();
	    /* @(return) = _Nil<core::T_O>(); */
	}
    }







      
      
#define ARGS_af_ll_socketReceive "(fd buffer length oob peek waitall)"
#define DECL_af_ll_socketReceive ""
#define DOCS_af_ll_socketReceive "ll_socketReceive"
    T_mv af_ll_socketReceive(int fd,	     	// #0
			  T_sp buffer,	// #1
			  int length,	// #2
			  bool oob,		// #3
			  bool peek,	// #4
			  bool waitall)	// #5
    {_G();
	int flags = ( oob ? MSG_OOB : 0 )  |
	    ( peek ? MSG_PEEK : 0 ) |
	    ( waitall ? MSG_WAITALL : 0 );
	ssize_t len;
	brcl_disable_interrupts();
	len = recvfrom(fd, REINTERPRET_CAST(char*, safe_buffer_pointer(buffer, length)), length, flags, NULL,NULL);
	brcl_enable_interrupts();
	if (len >= 0) {
	    if ( Vector_sp vec = buffer.as<Vector_O>() ) {
		vec->setFillPointer(len);
	    } else {
		SIMPLE_ERROR(BF("Vector must have fill pointer to be socket buffer: %s") % _rep_(vec));
	    }
	}
	return Values(Integer_O::create(static_cast<uint>(len)),Fixnum_O::create(errno));
    }



      
      
#define ARGS_af_ll_getProtocolByName "(name)"
#define DECL_af_ll_getProtocolByName ""
#define DOCS_af_ll_getProtocolByName "ll_getProtocolByName"
    int af_ll_getProtocolByName(const string& name)
    {
	return getprotobyname(name.c_str())->p_proto;
    }


	 
	 
#define ARGS_af_ll_socketBind_inetSocket "(port ip0 ip1 ip2 ip3 socketFileDescriptor)"
#define DECL_af_ll_socketBind_inetSocket ""
#define DOCS_af_ll_socketBind_inetSocket "ll_socketBind - returns output"
    int af_ll_socketBind_inetSocket(int port, int ip0, int ip1, int ip2, int ip3, int socketFileDescriptor)
    {_G();
	struct sockaddr_in sockaddr;
	int output;
	brcl_disable_interrupts();
	fill_inet_sockaddr(&sockaddr, port, ip0, ip1, ip2, ip3 );
	output = ::bind(socketFileDescriptor,(struct sockaddr*)&sockaddr, sizeof(struct sockaddr_in));
	brcl_enable_interrupts();
	return output;
    }




		     
		     
#define ARGS_af_ll_socketAccept_inetSocket "(sfd)"
#define DECL_af_ll_socketAccept_inetSocket ""
#define DOCS_af_ll_socketAccept_inetSocket "ll_socketAccept_inetSocket"
    T_mv af_ll_socketAccept_inetSocket(int sfd)
    {_G();
	struct sockaddr_in sockaddr;
	socklen_t addr_len = (socklen_t)sizeof(struct sockaddr_in);
	int new_fd;

	brcl_disable_interrupts();
	new_fd = accept(sfd, (struct sockaddr*)&sockaddr, &addr_len);
	brcl_enable_interrupts();

	int return0 = new_fd;
	core::T_sp return1 = _Nil<core::T_O>();
	int return2 = 0;
	if (new_fd != -1) {
	    uint32_t ip = ntohl(sockaddr.sin_addr.s_addr);
	    uint16_t port = ntohs(sockaddr.sin_port);
	    Vector_sp vector = eval::funcall(cl::_sym_makeArray,Fixnum_O::create(4)).as<Vector_O>();
	    vector->setf_elt(0, Fixnum_O::create( ip>>24 ));
	    vector->setf_elt(1, Fixnum_O::create( (ip>>16) & 0xFF));
	    vector->setf_elt(2, Fixnum_O::create( (ip>>8) & 0xFF));
	    vector->setf_elt(3, Fixnum_O::create( ip & 0xFF ));
	    return1 = vector;
	    return2 = port;
	}
	return Values(Integer_O::create(return0),return1,Integer_O::create(return2));
    }




      
#define ARGS_af_ll_socketConnect_inetSocket "(port ip0 ip1 ip2 ip3 socket-file-descriptor)"
#define DECL_af_ll_socketConnect_inetSocket ""
#define DOCS_af_ll_socketConnect_inetSocket "ll_socketConnect_inetSocket"
    int af_ll_socketConnect_inetSocket(int port, int ip0, int ip1, int ip2, int ip3, int socket_file_descriptor)
    {_G();
	struct sockaddr_in sockaddr;
	int output;
	brcl_disable_interrupts();
	fill_inet_sockaddr(&sockaddr, port, ip0, ip1, ip2, ip3 );
	output = connect(socket_file_descriptor,(struct sockaddr*)&sockaddr, sizeof(struct sockaddr_in));
	brcl_enable_interrupts();
	return output;
    }






#define ARGS_af_ll_socketPeername_inetSocket "(fd vector)"
#define DECL_af_ll_socketPeername_inetSocket ""
#define DOCS_af_ll_socketPeername_inetSocket "ll_socketPeername_inetSocket"
    int af_ll_socketPeername_inetSocket(int fd, Vector_sp vector)
    {_G();
	/* @01; */
	struct sockaddr_in name;
	socklen_t len = sizeof(struct sockaddr_in);
	int ret;
	brcl_disable_interrupts();
	ret = getpeername(fd,(struct sockaddr*)&name,&len);
	brcl_enable_interrupts();

	if (ret == 0) {
	    uint32_t ip = ntohl(name.sin_addr.s_addr);
	    uint16_t port = ntohs(name.sin_port);

	    vector->setf_elt(0, Fixnum_O::create( ip>>24 ));
	    vector->setf_elt(1, Fixnum_O::create( (ip>>16) & 0xFF));
	    vector->setf_elt(2, Fixnum_O::create( (ip>>8) & 0xFF));
	    vector->setf_elt(3, Fixnum_O::create( ip & 0xFF ));
	    return port;
	} else {
	    return -1;
	}
    }





#define ARGS_af_ll_socketName "(fd vector)"
#define DECL_af_ll_socketName ""
#define DOCS_af_ll_socketName "ll_socketName"
    int af_ll_socketName(int fd, Vector_sp vector)
    {_G();
	/* @01 */
	struct sockaddr_in name;
	socklen_t len = sizeof(struct sockaddr_in);
	int ret;
    
	brcl_disable_interrupts();
	ret = getsockname(fd,(struct sockaddr*)&name,&len);
	brcl_enable_interrupts();
    
	if (ret == 0) {
	    uint32_t ip = ntohl(name.sin_addr.s_addr);
	    uint16_t port = ntohs(name.sin_port);

	    vector->setf_elt(0, Fixnum_O::create( ip>>24 ));
	    vector->setf_elt(1, Fixnum_O::create( (ip>>16) & 0xFF));
	    vector->setf_elt(2, Fixnum_O::create( (ip>>8) & 0xFF));
	    vector->setf_elt(3, Fixnum_O::create( ip & 0xFF ));

	    return port;
	} else {
	    return -1;
	}
    }





#define ARGS_af_ll_socketSendAddress "(arg)"
#define DECL_af_ll_socketSendAddress ""
#define DOCS_af_ll_socketSendAddress "ll_socketSendAddress"
    Integer_sp af_ll_socketSendAddress(int fd,			//#0
				       Vector_sp vbuffer,	//#1
				       int length,		//#2
				       int secondAddress,	//#3
				       int ip0,		//#4
				       int ip1,		//#5
				       int ip2,		//#6
				       int ip3,		//#7
				       bool oob,		//#8
				       bool eor,		//#9
				       bool dontroute,		//#a
				       bool dontwait,		//#b
				       bool nosignal,		//#c
				       bool confirm )		//#d
    {_G();
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
	int flags = ( oob ? MSG_OOB : 0 )  |
	    ( eor ? MSG_EOR : 0 ) |
	    ( dontroute ? MSG_DONTROUTE : 0 ) |
	    ( dontwait ? MSG_DONTWAIT : 0 ) |
	    ( nosignal ? MSG_NOSIGNAL : 0 ) |
	    ( confirm ? MSG_CONFIRM : 0 );
//    cl_type type = type_of(#1);
	struct sockaddr_in sockaddr;
	ssize_t len;

	brcl_disable_interrupts();
	fill_inet_sockaddr(&sockaddr, secondAddress, ip0, ip1, ip2, ip3 );
#if (MSG_NOSIGNAL == 0) && defined(SO_NOSIGPIPE)
	{
	    int sockopt = nosignal;
	    setsockopt(fd,SOL_SOCKET,SO_NOSIGPIPE,
		       REINTERPRET_CAST(char *,&sockopt),
		       sizeof(int));
	}
#endif
	len = sendto(sock, REINTERPRET_CAST(char *,buffer),
		     length, flags,(struct sockaddr*)&sockaddr, 
		     sizeof(struct sockaddr_in));
	brcl_enable_interrupts();
	return Integer_O::create(static_cast<uint>(len));
    }




		 
		 
#define ARGS_af_socketSendNoAddress "(fb buffer length oob eor dontroute dontwait nosignal confirm)"
#define DECL_af_socketSendNoAddress ""
#define DOCS_af_socketSendNoAddress "socketSendNoAddress"
    Integer_sp af_socketSendNoAddress(int fb,		//#0
				      Vector_sp vbuffer,	//#1
				      int length,	//#2
				      bool oob,		//#3
				      bool eor,		//#4
				      bool dontroute,	//#5
				      bool dontwait,	//#6
				      bool nosignal,	//#7
				      bool confirm	//#8
	)
    {_G();
	int sock = fb;
	void *buffer = safe_buffer_pointer(vbuffer, length);
	int flags = ( oob ? MSG_OOB : 0 )  |
	    ( eor ? MSG_EOR : 0 ) |
	    ( dontroute ? MSG_DONTROUTE : 0 ) |
	    ( dontwait ? MSG_DONTWAIT : 0 ) |
	    ( nosignal ? MSG_NOSIGNAL : 0 ) |
	    ( confirm ? MSG_CONFIRM : 0 );
        ssize_t len;
	brcl_disable_interrupts();
#if (MSG_NOSIGNAL == 0) && defined(SO_NOSIGPIPE)
	{
	    int sockopt = nosignal;
	    setsockopt(fb,SOL_SOCKET,SO_NOSIGPIPE,
		       REINTERPRET_CAST(char *,&sockopt),
		       sizeof(int));
	}
#endif
	len = send(sock, REINTERPRET_CAST(char *, buffer), length, flags);
	brcl_enable_interrupts();
        return Integer_O::create(static_cast<uint>(len));
    }







#define ARGS_af_ll_socketBind_localSocket "(fd name family)"
#define DECL_af_ll_socketBind_localSocket ""
#define DOCS_af_ll_socketBind_localSocket "ll_socketBind_localSocket"
    int af_ll_socketBind_localSocket(int fd, const string& name, int family)
    {_G();
	struct sockaddr_un sockaddr;
//	size_t size;
	int output;
#ifdef BSD
	sockaddr.sun_len = sizeof(struct sockaddr_un);
#endif
        sockaddr.sun_family = family;
        strncpy(sockaddr.sun_path,name.c_str(),sizeof(sockaddr.sun_path));
	sockaddr.sun_path[sizeof(sockaddr.sun_path)-1] = '\0';

	brcl_disable_interrupts();
	output = ::bind(fd,(struct sockaddr*)&sockaddr, sizeof(struct sockaddr_un));
	brcl_enable_interrupts();
	return output;

    }




   
	   
#define ARGS_af_ll_socketAccept_localSocket "(socket-file-descriptor)"
#define DECL_af_ll_socketAccept_localSocket ""
#define DOCS_af_ll_socketAccept_localSocket "ll_socketAccept_localSocket"
    T_mv af_ll_socketAccept_localSocket(int socketFileDescriptor)
    {_G();
	struct sockaddr_un sockaddr;
	socklen_t addr_len = (socklen_t)sizeof(struct sockaddr_un);
	int new_fd;
	brcl_disable_interrupts();
	new_fd = accept(socketFileDescriptor, (struct sockaddr *)&sockaddr, &addr_len);
	brcl_enable_interrupts();
	return Values(Integer_O::create(new_fd),(new_fd==-1) ? _Nil<core::Str_O>() : Str_O::create(sockaddr.sun_path));
    }


      
      
#define ARGS_af_ll_socketConnect_localSocket "(fd family path)"
#define DECL_af_ll_socketConnect_localSocket ""
#define DOCS_af_ll_socketConnect_localSocket "ll_socketConnect_localSocket"
    int af_ll_socketConnect_localSocket(int fd, int family, const string& path)
    {_G();
	struct sockaddr_un sockaddr;
	int output;
#ifdef BSD
	sockaddr.sun_len = sizeof(struct sockaddr_un);
#endif
	sockaddr.sun_family = family;
	strncpy(sockaddr.sun_path,path.c_str(),sizeof(sockaddr.sun_path));
	sockaddr.sun_path[sizeof(sockaddr.sun_path)-1] = '\0';

	brcl_disable_interrupts();
	output = connect(fd,(struct sockaddr*)&sockaddr, sizeof(struct sockaddr_un));
	brcl_enable_interrupts();

	return output;
    }







#define ARGS_af_socketPeername_localSocket "(fd)"
#define DECL_af_socketPeername_localSocket ""
#define DOCS_af_socketPeername_localSocket "socketPeername_localSocket"
    Str_sp af_socketPeername_localSocket(int fd)
    {_G();
	struct sockaddr_un name;
	socklen_t len = sizeof(struct sockaddr_un);
	int ret;

	brcl_disable_interrupts();
	ret = getpeername(fd,(struct sockaddr*)&name,&len);
	brcl_enable_interrupts();

	if (ret == 0) {
	    return Str_O::create(name.sun_path);
	} else {
	    return _Nil<core::Str_O>();
	}
    }








#define ARGS_af_ll_nonBlockingMode "(fd)"
#define DECL_af_ll_nonBlockingMode ""
#define DOCS_af_ll_nonBlockingMode "ll_nonBlockingMode"
    int af_ll_nonBlockingMode(int fd)
    {_G();
	return fcntl(fd,F_GETFL,NULL)&O_NONBLOCK;
    }






#define ARGS_af_ll_setfNonBlockingMode "(fd nblock)"
#define DECL_af_ll_setfNonBlockingMode ""
#define DOCS_af_ll_setfNonBlockingMode "ll_setfNonBlockingMode"
    int af_ll_setfNonBlockingMode(int fd, int nblock)
    {_G();
	int oldflags = fcntl(fd,F_GETFL,NULL);
	int newflags = (oldflags & ~O_NONBLOCK) |
	    (nblock ? O_NONBLOCK : 0);
	brcl_disable_interrupts();
	int ret = fcntl(fd,F_SETFL,newflags);
	brcl_enable_interrupts();
	return ret;
    }







#define ARGS_af_ll_dup "(fd)"
#define DECL_af_ll_dup ""
#define DOCS_af_ll_dup "ll_dup"
    int af_ll_dup(int fd)
    {_G();
	return dup(fd);
    }




  
  
#define ARGS_af_ll_makeStreamFromFd "(name fd stream-mode element-type external-format)"
#define DECL_af_ll_makeStreamFromFd ""
#define DOCS_af_ll_makeStreamFromFd "ll_makeStreamFromFd"
    T_sp af_ll_makeStreamFromFd(const string& name,		//#0
				int fd,			//#1
				int streamMode,		//#2
				T_sp elementType,		//#3
				T_sp externalFormat )	//#4
    {_G();
        core::StreamMode direction;
	switch (streamMode) {
	case core::brcl_stream_mode_input:
	    direction = clasp_smm_input_file;
	    break;
	case core::brcl_stream_mode_output:
	    direction = clasp_smm_output_file;
	    break;
	case core::brcl_stream_mode_io:
	    direction = clasp_smm_io_file;
	    break;
	default: {
	    SIMPLE_ERROR(BF("Illegal stream mode %d") % streamMode );
	}}
	Stream_sp stream = core::IOFileStream_O::make(name,fd,direction,elementType,externalFormat);
	return stream;
    }




			       
			       
#define ARGS_af_ll_autoCloseTwoWayStream "(stream)"
#define DECL_af_ll_autoCloseTwoWayStream ""
#define DOCS_af_ll_autoCloseTwoWayStream "ll_autoCloseTwoWayStream"
    void af_ll_autoCloseTwoWayStream(Stream_sp stream)
    {_G();
#if 0
	IMPLEMENT_MEF(BF("Handle ECL_STREAM_CLOSE_COMPONENTS"));
	stream->stream.flags |= ECL_STREAM_CLOSE_COMPONENTS;
#endif
    };




#define ARGS_af_ll_strerror "(num)"
#define DECL_af_ll_strerror ""
#define DOCS_af_ll_strerror "ll_strerror"
    Str_sp af_ll_strerror(int num)
    {_G();
	return Str_O::create(strerror(num));
    }


#define ARGS_af_ll_strerror_errno "()"
#define DECL_af_ll_strerror_errno ""
#define DOCS_af_ll_strerror_errno "ll_strerror_errno"
    Str_sp af_ll_strerror_errno()
    {_G();
	return Str_O::create(strerror(errno));
    }


				 

  
  
#define ARGS_af_ll_getSockoptInt "(fd level constant)"
#define DECL_af_ll_getSockoptInt ""
#define DOCS_af_ll_getSockoptInt "ll_getSockoptInt"
    Integer_sp af_ll_getSockoptInt(int fd, int level, int constant)
    {_G();
	/* (c-inline (fd level const) (:int :int :int) t   ) */
	int sockopt, ret;
	socklen_t socklen = sizeof(int);

	brcl_disable_interrupts();
	ret = getsockopt(fd,level,constant,REINTERPRET_CAST(char*,&sockopt),&socklen);
	brcl_enable_interrupts();

	return (ret == 0) ? Integer_O::create(sockopt) : _Nil<core::Integer_O>();
    }


	
#define ARGS_af_ll_getSockoptBool "(fd level constant)"
#define DECL_af_ll_getSockoptBool ""
#define DOCS_af_ll_getSockoptBool "ll_getSockoptBool"
    T_sp af_ll_getSockoptBool(int fd, int level, int constant)
    {_G();
	int sockopt, ret;
	socklen_t socklen = sizeof(int);

	brcl_disable_interrupts();
	ret = getsockopt(fd,level,constant,REINTERPRET_CAST(char*,&sockopt),&socklen);
	brcl_enable_interrupts();
	return (ret == 0) ? _lisp->_boolean(sockopt) : _Nil<core::T_O>();
    }


#define ARGS_af_ll_getSockoptTimeval "(fd level constant)"
#define DECL_af_ll_getSockoptTimeval ""
#define DOCS_af_ll_getSockoptTimeval "ll_getSockoptTimeval"
    DoubleFloat_sp af_ll_getSockoptTimeval(int fd, int level, int constant)
    {
	struct timeval tv;
	socklen_t socklen = sizeof(struct timeval);
	int ret;
	brcl_disable_interrupts();
	ret = getsockopt(fd,level,constant,REINTERPRET_CAST(char*,&tv),&socklen);
	brcl_enable_interrupts();
	return (ret == 0) ? DoubleFloat_O::create((double)tv.tv_sec + ((double)tv.tv_usec) / 1000000.0) : _Nil<core::DoubleFloat_O>();
    }


   
   
#define ARGS_af_ll_getSockoptLinger "(fd level constant)"
#define DECL_af_ll_getSockoptLinger ""
#define DOCS_af_ll_getSockoptLinger "ll_getSockoptLinger"
    Integer_sp af_ll_getSockoptLinger(int fd, int level, int constant)
    {_G();
	struct linger sockopt;
	socklen_t socklen = sizeof(struct linger);
	int ret;

	brcl_disable_interrupts();
	ret = getsockopt(fd,level,constant,REINTERPRET_CAST(char*,&sockopt),&socklen);
	brcl_enable_interrupts();

	return (ret == 0) ? Integer_O::create((sockopt.l_onoff != 0) ? sockopt.l_linger : 0) : _Nil<core::Integer_O>();
    }




#define ARGS_af_ll_setSockoptInt "(fd level constant value)"
#define DECL_af_ll_setSockoptInt ""
#define DOCS_af_ll_setSockoptInt "ll_setSockoptInt"
    bool af_ll_setSockoptInt(int fd, int level, int constant, int value)
    {_G();
	int sockopt = value;
	int ret;

	brcl_disable_interrupts();
	ret = setsockopt(fd,level,constant,REINTERPRET_CAST(char *,&sockopt),sizeof(int));
	brcl_enable_interrupts();

	return (ret == 0) ? true : false;
    }


#define ARGS_af_ll_setSockOptBool "(fd level constant value)"
#define DECL_af_ll_setSockOptBool ""
#define DOCS_af_ll_setSockOptBool "ll_setSockOptBool"
    bool af_ll_setSockOptBool(int fd, int level, int constant, bool value)
    {_G();
	int sockopt = value ? 1 : 0;
	int ret;

	brcl_disable_interrupts();
	ret = setsockopt(fd,level,constant,REINTERPRET_CAST(char *,&sockopt),sizeof(int));
	brcl_enable_interrupts();

	return (ret == 0) ? true : false;
    }



   
   
#define ARGS_af_ll_setSockOptTimeval "(fd level constant value)"
#define DECL_af_ll_setSockOptTimeval ""
#define DOCS_af_ll_setSockOptTimeval "ll_setSockOptTimeval"
    bool af_ll_setSockOptTimeval(int fd, int level, int constant, double value)
    {
	struct timeval tv;
	double tmp = value;
	int ret;

	brcl_disable_interrupts();
	tv.tv_sec = (int)tmp;
	tv.tv_usec = (int)((tmp-floor(tmp))*1000000.0);
	ret = setsockopt(fd,level,constant,&tv,sizeof(struct timeval));
	brcl_enable_interrupts();

	return (ret == 0) ? true : false;
    }





   
   
#define ARGS_af_ll_setSockOptLinger "(fd level constant value)"
#define DECL_af_ll_setSockOptLinger ""
#define DOCS_af_ll_setSockOptLinger "ll_setSockOptLinger"
    bool af_ll_setSockOptLinger(int fd, int level, int constant, int value)
    {_G();
	struct linger sockopt = {0, 0};
	int ret;

	if (value > 0) {
	    sockopt.l_onoff = 1;
	    sockopt.l_linger = value;
	}

	brcl_disable_interrupts();
	ret = setsockopt(fd,level,constant,REINTERPRET_CAST(char *,&sockopt),
			 sizeof(struct linger));
	brcl_enable_interrupts();

	return (ret == 0) ? true : false;
    }



    void initialize_sockets_globals()
    {
	SYMBOL_EXPORT_SC_(SocketsPkg,_PLUS_af_inet_PLUS_);
	_sym__PLUS_af_inet_PLUS_->defconstant(Integer_O::create(AF_INET));
	SYMBOL_EXPORT_SC_(SocketsPkg,_PLUS_af_local_PLUS_);
	_sym__PLUS_af_local_PLUS_->defconstant(Integer_O::create(AF_UNIX));
	SYMBOL_EXPORT_SC_(SocketsPkg,_PLUS_eagain_PLUS_);
	_sym__PLUS_eagain_PLUS_->defconstant(Integer_O::create(EAGAIN));
	SYMBOL_EXPORT_SC_(SocketsPkg,_PLUS_eintr_PLUS_);
	_sym__PLUS_eintr_PLUS_->defconstant(Integer_O::create(EINTR));


	SYMBOL_EXPORT_SC_(SocketsPkg,_PLUS_sock_dgram_PLUS_);
	_sym__PLUS_sock_dgram_PLUS_->defconstant(Integer_O::create(SOCK_DGRAM));


	SYMBOL_EXPORT_SC_(SocketsPkg,_PLUS_sock_stream_PLUS_);
	_sym__PLUS_sock_stream_PLUS_->defconstant(Integer_O::create(SOCK_STREAM));

	SYMBOL_EXPORT_SC_(SocketsPkg,_PLUS_brcl_stream_mode_input_PLUS_);
	_sym__PLUS_brcl_stream_mode_input_PLUS_->defconstant(Integer_O::create(static_cast<int>(core::brcl_stream_mode_input)));

	SYMBOL_EXPORT_SC_(SocketsPkg,_PLUS_brcl_stream_mode_output_PLUS_);
	_sym__PLUS_brcl_stream_mode_output_PLUS_->defconstant(Integer_O::create(core::brcl_stream_mode_output));

	SYMBOL_EXPORT_SC_(SocketsPkg,_PLUS_brcl_stream_mode_io_PLUS_);
	_sym__PLUS_brcl_stream_mode_io_PLUS_->defconstant(Integer_O::create(brcl_stream_mode_io));



	SYMBOL_EXPORT_SC_(SocketsPkg,EADDRINUSE);
	_sym_EADDRINUSE->defconstant(Integer_O::create(EADDRINUSE));
	SYMBOL_EXPORT_SC_(SocketsPkg,EAGAIN);
	_sym_EAGAIN->defconstant(Integer_O::create(EAGAIN));
	SYMBOL_EXPORT_SC_(SocketsPkg,EBADF);
	_sym_EBADF->defconstant(Integer_O::create(EBADF));
	SYMBOL_EXPORT_SC_(SocketsPkg,ECONNREFUSED);
	_sym_ECONNREFUSED->defconstant(Integer_O::create(ECONNREFUSED));
	SYMBOL_EXPORT_SC_(SocketsPkg,ETIMEDOUT);
	_sym_ETIMEDOUT->defconstant(Integer_O::create(ETIMEDOUT));
	SYMBOL_EXPORT_SC_(SocketsPkg,EINTR);
	_sym_EINTR->defconstant(Integer_O::create(EINTR));
	SYMBOL_EXPORT_SC_(SocketsPkg,EINVAL);
	_sym_EINVAL->defconstant(Integer_O::create(EINVAL));
	SYMBOL_EXPORT_SC_(SocketsPkg,ENOBUFS);
	_sym_ENOBUFS->defconstant(Integer_O::create(ENOBUFS));
	SYMBOL_EXPORT_SC_(SocketsPkg,ENOMEM);
	_sym_ENOMEM->defconstant(Integer_O::create(ENOMEM));
	SYMBOL_EXPORT_SC_(SocketsPkg,EOPNOTSUPP);
	_sym_EOPNOTSUPP->defconstant(Integer_O::create(EOPNOTSUPP));
	SYMBOL_EXPORT_SC_(SocketsPkg,EPERM);
	_sym_EPERM->defconstant(Integer_O::create(EPERM));
	SYMBOL_EXPORT_SC_(SocketsPkg,EPROTONOSUPPORT);
	_sym_EPROTONOSUPPORT->defconstant(Integer_O::create(EPROTONOSUPPORT));
	SYMBOL_EXPORT_SC_(SocketsPkg,ESOCKTNOSUPPORT);
	_sym_ESOCKTNOSUPPORT->defconstant(Integer_O::create(ESOCKTNOSUPPORT));
	SYMBOL_EXPORT_SC_(SocketsPkg,ENETUNREACH);
	_sym_ENETUNREACH->defconstant(Integer_O::create(ENETUNREACH));

	SYMBOL_EXPORT_SC_(SocketsPkg,NETDB_INTERNAL);
	_sym_NETDB_INTERNAL->defconstant(Integer_O::create(NETDB_INTERNAL));
	SYMBOL_EXPORT_SC_(SocketsPkg,NETDB_SUCCESS);
	_sym_NETDB_SUCCESS->defconstant(Integer_O::create(NETDB_SUCCESS));
	SYMBOL_EXPORT_SC_(SocketsPkg,HOST_NOT_FOUND);
	_sym_HOST_NOT_FOUND->defconstant(Integer_O::create(HOST_NOT_FOUND));
	SYMBOL_EXPORT_SC_(SocketsPkg,TRY_AGAIN);
	_sym_TRY_AGAIN->defconstant(Integer_O::create(TRY_AGAIN));
	SYMBOL_EXPORT_SC_(SocketsPkg,NO_RECOVERY);
	_sym_NO_RECOVERY->defconstant(Integer_O::create(NO_RECOVERY));
	SYMBOL_EXPORT_SC_(SocketsPkg,NO_ADDRESS);
	_sym_NO_ADDRESS->defconstant(Integer_O::create(NO_ADDRESS));


	SYMBOL_EXPORT_SC_(SocketsPkg,SOL_SOCKET);
	_sym_SOL_SOCKET->defconstant(Integer_O::create(SOL_SOCKET));
	SYMBOL_EXPORT_SC_(SocketsPkg,IPPROTO_TCP);
	_sym_IPPROTO_TCP->defconstant(Integer_O::create(IPPROTO_TCP));




	SYMBOL_EXPORT_SC_(SocketsPkg,SO_TYPE);
	_sym_SO_TYPE->defconstant(Integer_O::create(SO_TYPE));
	SYMBOL_EXPORT_SC_(SocketsPkg,SO_RCVBUF);
	_sym_SO_RCVBUF->defconstant(Integer_O::create(SO_RCVBUF));
	SYMBOL_EXPORT_SC_(SocketsPkg,SO_RCVTIMEO);
	_sym_SO_RCVTIMEO->defconstant(Integer_O::create(SO_RCVTIMEO));
	SYMBOL_EXPORT_SC_(SocketsPkg,SO_SNDTIMEO);
	_sym_SO_SNDTIMEO->defconstant(Integer_O::create(SO_SNDTIMEO));
	SYMBOL_EXPORT_SC_(SocketsPkg,SO_REUSEADDR);
	_sym_SO_REUSEADDR->defconstant(Integer_O::create(SO_REUSEADDR));
	SYMBOL_EXPORT_SC_(SocketsPkg,SO_KEEPALIVE);
	_sym_SO_KEEPALIVE->defconstant(Integer_O::create(SO_KEEPALIVE));
	SYMBOL_EXPORT_SC_(SocketsPkg,SO_DONTROUTE);
	_sym_SO_DONTROUTE->defconstant(Integer_O::create(SO_DONTROUTE));
	SYMBOL_EXPORT_SC_(SocketsPkg,SO_LINGER);
	_sym_SO_LINGER->defconstant(Integer_O::create(SO_LINGER));
#ifndef _TARGET_OS_LINUX
	SYMBOL_EXPORT_SC_(SocketsPkg,SO_REUSEPORT);
	_sym_SO_REUSEPORT->defconstant(Integer_O::create(SO_REUSEPORT));
#endif
	SYMBOL_EXPORT_SC_(SocketsPkg,TCP_NODELAY);
	_sym_TCP_NODELAY->defconstant(Integer_O::create(TCP_NODELAY));
    };

    void initialize_sockets()
    {
	SYMBOL_EXPORT_SC_(SocketsPkg,ff_socket);
	Defun(ff_socket);
	SYMBOL_EXPORT_SC_(SocketsPkg,ff_listen);
	Defun(ff_listen);
	SYMBOL_EXPORT_SC_(SocketsPkg,ff_close);
	Defun(ff_close);

	SYMBOL_EXPORT_SC_(SocketsPkg,ll_get_name_service_h_errno);
	Defun(ll_get_name_service_h_errno);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_socket_errno);
	Defun(ll_socket_errno);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_getNameServiceErrorMessage);
	Defun(ll_getNameServiceErrorMessage);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_getHostByName);
	Defun(ll_getHostByName);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_getHostByAddress);
	Defun(ll_getHostByAddress);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_socketReceive);
	Defun(ll_socketReceive);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_getProtocolByName);
	Defun(ll_getProtocolByName);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_socketBind_inetSocket);
	Defun(ll_socketBind_inetSocket);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_socketAccept_inetSocket);
	Defun(ll_socketAccept_inetSocket);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_socketConnect_inetSocket);
	Defun(ll_socketConnect_inetSocket);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_socketPeername_inetSocket);
	Defun(ll_socketPeername_inetSocket);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_socketName);
	Defun(ll_socketName);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_socketSendAddress);
	Defun(ll_socketSendAddress);
	SYMBOL_EXPORT_SC_(SocketsPkg,socketSendNoAddress);
	Defun(socketSendNoAddress);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_socketBind_localSocket);
	Defun(ll_socketBind_localSocket);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_socketAccept_localSocket);
	Defun(ll_socketAccept_localSocket);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_socketConnect_localSocket);
	Defun(ll_socketConnect_localSocket);
	SYMBOL_EXPORT_SC_(SocketsPkg,socketPeername_localSocket);
	Defun(socketPeername_localSocket);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_nonBlockingMode);
	Defun(ll_nonBlockingMode);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_setfNonBlockingMode);
	Defun(ll_setfNonBlockingMode);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_dup);
	Defun(ll_dup);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_makeStreamFromFd);
	Defun(ll_makeStreamFromFd);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_autoCloseTwoWayStream);
	Defun(ll_autoCloseTwoWayStream);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_strerror);
	Defun(ll_strerror);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_strerror_errno);
	Defun(ll_strerror_errno);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_getSockoptInt);
	Defun(ll_getSockoptInt);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_getSockoptBool);
	Defun(ll_getSockoptBool);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_getSockoptTimeval);
	Defun(ll_getSockoptTimeval);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_getSockoptLinger);
	Defun(ll_getSockoptLinger);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_setSockoptInt);
	Defun(ll_setSockoptInt);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_setSockOptBool);
	Defun(ll_setSockOptBool);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_setSockOptTimeval);
	Defun(ll_setSockOptTimeval);
	SYMBOL_EXPORT_SC_(SocketsPkg,ll_setSockOptLinger);
	Defun(ll_setSockOptLinger);

    }


};
