/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <md.h>				/* get HAVE_'s */
#if defined(HAVE_SOCKET) || defined(HAVE_WINSOCK)

#include <sys/types.h>
#include <memory.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_WINSOCK
#include "mswinsock.h"
#include <io.h>
#include <fcntl.h>
#else /*HAVE_WINSOCK*/

#define HAVE_SYS_UN_H 1
#define UNIX_DOMAIN_SOCKETS 1
#include <sys/socket.h>			/* must be first to avoid send() */
#include <netdb.h>			/* conflict ... */
#include <netinet/in.h>
#include <errno.h>
extern int errno;

#define SOCKET int
#endif /*WINSOCK*/

#define create PCEcreate		/* avoid conflict */

#include <h/kernel.h>
#include <h/unix.h>
#include <h/interface.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif
#include <signal.h>

#define MAX_UN_ADDRESS_LEN (sizeof(struct sockaddr_un) - sizeof(short))

#ifndef INVALID_SOCKET
#define INVALID_SOCKET -1
#endif

static status	closeSocket(Socket);

#ifdef HAVE_WINSOCK

typedef struct
{ int	 id;
  char  *description;
} wsock_err;

static wsock_err wsock_err_list[] = {
{ WSAEINTR,		"WSAEINTR" },
{ WSAEBADF,		"WSAEBADF" },
{ WSAEACCES,		"WSAEACCES" },
{ WSAEFAULT,		"WSAEFAULT" },
{ WSAEINVAL,		"WSAEINVAL" },
{ WSAEMFILE,		"No more file descriptors" },
{ WSAEWOULDBLOCK,	"WSAEWOULDBLOCK" },
{ WSAEINPROGRESS,	"Blocking operation in progress" },
{ WSAEALREADY,		"WSAEALREADY" },
{ WSAENOTSOCK,		"WSAENOTSOCK" },
{ WSAEDESTADDRREQ,	"WSAEDESTADDRREQ" },
{ WSAEMSGSIZE,		"WSAEMSGSIZE" },
{ WSAEPROTOTYPE,	"Type/protocol mismatch" },
{ WSAENOPROTOOPT,	"WSAENOPROTOOPT" },
{ WSAEPROTONOSUPPORT,   "Protocol not supported" },
{ WSAESOCKTNOSUPPORT,   "Type not supported in address family" },
{ WSAEOPNOTSUPP,	"WSAEOPNOTSUPP" },
{ WSAEPFNOSUPPORT,	"WSAEPFNOSUPPORT" },
{ WSAEAFNOSUPPORT,	"Unsupported address family" },
{ WSAEADDRINUSE,	"WSAEADDRINUSE" },
{ WSAEADDRNOTAVAIL,	"WSAEADDRNOTAVAIL" },
{ WSAENETDOWN,		"Network is down" },
{ WSAENETUNREACH,	"WSAENETUNREACH" },
{ WSAENETRESET,		"WSAENETRESET" },
{ WSAECONNABORTED,	"WSAECONNABORTED" },
{ WSAECONNRESET,	"WSAECONNRESET" },
{ WSAENOBUFS,		"No more buffer space" },
{ WSAEISCONN,		"WSAEISCONN" },
{ WSAENOTCONN,		"WSAENOTCONN" },
{ WSAESHUTDOWN,		"WSAESHUTDOWN" },
{ WSAETOOMANYREFS,	"WSAETOOMANYREFS" },
{ WSAETIMEDOUT,		"WSAETIMEDOUT" },
{ WSAECONNREFUSED,	"Connection refused" },
{ WSAELOOP,		"WSAELOOP" },
{ WSAENAMETOOLONG,	"WSAENAMETOOLONG" },
{ WSAEHOSTDOWN,		"Remote host is down" },
{ WSAEHOSTUNREACH,	"Host unreachable" },
{ WSAENOTEMPTY,		"WSAENOTEMPTY" },
{ WSAEPROCLIM,		"WSAEPROCLIM" },
{ WSAEUSERS,		"WSAEUSERS" },
{ WSAEDQUOT,		"WSAEDQUOT" },
{ WSAESTALE,		"WSAESTALE" },
{ WSAEREMOTE,		"WSAEREMOTE" },
{ WSAEDISCON,		"WSAEDISCON" },
{ WSASYSNOTREADY,	"WSASYSNOTREADY" },
{ WSAVERNOTSUPPORTED,   "WSAVERNOTSUPPORTED" },
{ WSANOTINITIALISED,	"Unsuccessfull WSAStartup()" },
{ WSAHOST_NOT_FOUND,	"WSAHOST_NOT_FOUND" },
{ WSATRY_AGAIN,		"WSATRY_AGAIN" },
{ WSANO_RECOVERY,	"WSANO_RECOVERY" },
{ WSANO_DATA,		"WSANO_DATA" },
{ WSANO_ADDRESS,	"WSANO_ADDRESS" },
{ 0, NULL }
};


Name
SockError()
{ int err = WSAGetLastError();
  wsock_err *e = wsock_err_list;
  char buf[50];

  for( ; e->description; e++ )
  { if ( e->id == err )
      return CtoName(e->description);
  }

  sprintf(buf, "Unknown error: %d", err);
  return CtoName(buf);
}

#define SocketHandle(s) ((SOCKET)((s)->ws_ref))

#else /*HAVE_WINSOCK*/
#define SocketHandle(s) ((s->rdfd))
#define SockError() getOsErrorPce(PCE)
#endif /*HAVE_WINSOCK*/

static Chain	SocketChain;		/* Available open sockets */

static void
closeAllSockets(void)
{ Socket s;

  for_chain(SocketChain, s, closeSocket(s));

#ifdef WINSOCK
  WSACleanup();
#endif
}


static void
sigPipeSocket(void)
{ errorPce(PCE, NAME_brokenPipe, 0);
}


static void
setupSockets(void)
{ static int initialised = 0;

  if ( !initialised )
  { 
#ifdef HAVE_WINSOCK
    WSADATA data;
    WORD wversion = MAKEWORD(1, 1);
/*
    int optionValue = SO_SYNCHRONOUS_ALERT;
*/
    if ( WSAStartup(wversion, &data) != 0 )
      errorPce(NIL, NAME_socket, NAME_initialise, SockError());

    DEBUG(NAME_socket,
	  Cprintf("WSAStartup(): wVersion = %d.%d, wHighVersion = %d.%d\n",
		  data.wVersion >> 8, data.wVersion & 0xff,
		  data.wHighVersion >> 8, data.wHighVersion & 0xff);
	  Cprintf("Description: %s\n", data.szDescription);
	  Cprintf("Status:      %s\n", data.szSystemStatus);
	 );

/*
    if ( setsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE,
		    (char *)&optionValue, sizeof(optionValue)) != NO_ERROR )
      errorPce(NIL, NAME_socket, CtoName("setsockopt"), SockError());
*/
#endif

    at_pce_exit(closeAllSockets, ATEXIT_FIFO);
#ifdef SIGPIPE
    hostAction(HOST_SIGNAL, SIGPIPE, sigPipeSocket);
#endif
    initialised++;
  }
}


static status
initialiseSocket(Socket s, Any address, Name domain)
{ setupSockets();

  initialiseStream((Stream) s, NIL, NIL, NIL, DEFAULT);
#ifdef HAVE_WINSOCK
  s->ws_ref = (WsRef) INVALID_SOCKET;
#endif

  if ( isDefault(domain) )
  { if ( instanceOfObject(address, ClassFile) )
      domain = NAME_unix;
    else if ( instanceOfObject(address, ClassTuple) || isInteger(address) )
      domain = NAME_inet;
    else
      return errorPce(s, NAME_noDomain);
  }

  assign(s, domain,	    domain);
  assign(s, address,	    address);
  assign(s, status, 	    NAME_idle);

  succeed;
}


static status
unlinkSocket(Socket s)
{ closeSocket(s);			/* close output */

  succeed;
}


static status
cloneSocket(Socket s, Socket clone)
{ clonePceSlots(s, clone);

  clone->rdfd = clone->wrfd = -1;
#ifdef HAVE_WINSOCK
  clone->ws_ref = (WsRef)INVALID_SOCKET;
#else
  clone->ws_ref = 0;
#endif
  clone->input_buffer = NULL;
  clone->input_allocated = s->input_p = 0;

  succeed;
}

		 /*******************************
		 *	     PRINT NAME		*
		 *******************************/

static StringObj
getPrintNameSocket(Socket s)
{ char buf[256];
  Any av[3];
  Name fmt;
  int an;

  av[0] = getClassNameObject(s);

  if ( instanceOfObject(s->address, ClassTuple) )
  { Tuple t = (Tuple) s->address;

    av[1] = t->first;
    av[2] = t->second;
    an = 3;
    fmt = CtoName("%s(%s:%d)");
  } else
  { av[1] = get(s->address, NAME_printName, 0);
    an = 2;
    fmt = CtoName("%s(%s)");
  }

  swritefv(buf, (CharArray) fmt, an, av);

  answer(CtoString(buf));
}


		 /*******************************
		 *      CREATE/CONNECT/ETC	*
		 *******************************/

static status
createSocket(Socket s)
{ if ( SocketHandle(s) == INVALID_SOCKET )
  { int domain;
   
    closeSocket(s);

    if ( s->domain == NAME_unix )
    {
#ifdef UNIX_DOMAIN_SOCKETS
      domain = PF_UNIX;
#else
      return errorPce(s, NAME_noSocketDomain, NAME_unix);
#endif
    } else /*if ( s->domain == NAME_inet )*/
      domain = PF_INET;

#ifdef HAVE_WINSOCK
  { SOCKET mss;
    
    if ( (mss=socket(domain, SOCK_STREAM, 0)) == INVALID_SOCKET )
      return errorPce(s, NAME_socket, NAME_create, SockError());

    s->ws_ref = (WsRef) mss;
  }
#else
    if ( (s->rdfd = s->wrfd = socket(domain, SOCK_STREAM, 0)) < 0 )
      return errorPce(s, NAME_socket, NAME_create, SockError());
#endif
  }

  succeed;
}


#ifdef UNIX_DOMAIN_SOCKETS
static status
unix_address_socket(Socket s, struct sockaddr_un *address, int *len)
{ Name name = getOsNameFile((FileObj) s->address);
  char *path;

  if ( !name )
    fail;
  
  path = strName(name);
  address->sun_family = PF_UNIX;
  if ( (*len = strlen(path)) > MAX_UN_ADDRESS_LEN )
    return errorPce(s, NAME_socket, NAME_address, CtoName("Name too long"));

  strcpy(address->sun_path, path);
  *len += sizeof(address->sun_family);

  succeed;
}
#endif /*UNIX_DOMAIN_SOCKETS*/


static status
inet_address_socket(Socket s, struct sockaddr_in *address, int *len)
{ memset(address, 0, sizeof(*address));
  *len = sizeof(*address);
  address->sin_family = PF_INET;

  if ( instanceOfObject(s->address, ClassTuple) ) /* client */
  { CharArray hostname;
    Int port;
    struct hostent *hp;
    Tuple t = s->address;

    if ( !(hostname = checkType(t->first, TypeName, NIL)) )
      return errorPce(t->first, NAME_unexpectedType, TypeName);
    if ( !(port = checkType(t->second, TypeInt, NIL)) )
      return errorPce(t->second, NAME_unexpectedType, TypeInt);

    if ( !(hp = gethostbyname(strName(hostname))) )
      return errorPce(s, NAME_noHost, hostname);
	 
    address->sin_port = valInt(port);
    memcpy(&address->sin_addr, hp->h_addr, hp->h_length);
  } else if ( isInteger(s->address) )	/* server */
  { address->sin_port = valInt(s->address);
    address->sin_addr.s_addr = htonl(INADDR_ANY);
  } else
    return errorPce(s->address, NAME_unexpectedType, CtoType("tuple"));

  succeed;
}


static status
bindSocket(Socket s)
{ int rval;

  TRY(createSocket(s));

#ifdef UNIX_DOMAIN_SOCKETS
  if ( s->domain == NAME_unix )
  { struct sockaddr_un address;
    int len;
    TRY( unix_address_socket(s, &address, &len) );
    rval = bind(SocketHandle(s), (struct sockaddr *) &address, len);
  } else /*if ( s->domain == NAME_inet )*/
#endif
  { struct sockaddr_in address;
    int len;
    TRY( inet_address_socket(s, &address, &len) );
    if ( (rval = bind(SocketHandle(s), (struct sockaddr *) &address, len))==0 )
    { if ( s->address == ZERO ||
	   (instanceOfObject(s->address, ClassTuple) &&
	    ((Tuple)s->address)->second == ZERO) )
      { struct sockaddr_in addr;
	int len = sizeof(addr);

	if ( getsockname(SocketHandle(s), (struct sockaddr *) &addr, &len) )
	{ return errorPce(s, NAME_socket, NAME_getsockname, SockError());
	} else
	{ if ( s->address == ZERO )
	    assign(s, address, toInt(addr.sin_port));
	  else
	    assign((Tuple)s->address, second, toInt(address.sin_port));
	}
      }
    }
  }

  if ( rval != 0 )
    return errorPce(s, NAME_socket, NAME_bind, SockError());

  succeed;
}
  

status
acceptSocket(Socket s)
{ SOCKET id2;
  Socket s2;
  Any client_address;

#ifdef UNIX_DOMAIN_SOCKETS
  if ( s->domain == NAME_unix )
  { struct sockaddr_un address;
    int len = sizeof(address);

    if ( (id2=accept(SocketHandle(s), (struct sockaddr *) &address, &len))<0 )
      errorPce(s, NAME_socket, NAME_accept, SockError());
 
    client_address = s->address;
  } else /*if ( s->domain == NAME_inet )*/
#endif
  { struct sockaddr_in address;
    int len = sizeof(address);

    if ( (id2=accept(SocketHandle(s), (struct sockaddr *) &address, &len))<0 )
      errorPce(s, NAME_socket, NAME_accept, SockError());
 
    { struct hostent *hp;

      if ( (hp = gethostbyaddr((char *)&address.sin_addr,
			       sizeof(address.sin_addr),
			       AF_INET)) )
	client_address = newObject(ClassTuple,
				   CtoName(hp->h_name), 
				   toInt(address.sin_port),
				   0);
      else
	client_address = NIL;
    }
  }
  
  if ( !(s2 = get(s, NAME_clone, 0)) )
    return errorPce(s, NAME_failedToClone);

#ifdef HAVE_WINSOCK
  s2->ws_ref = (WsRef) id2;
  s2->rdfd = s2->wrfd = 0;		/* signal open status */
#else
  s2->rdfd = s2->wrfd = id2;
#endif
  assign(s2, input_message, s->input_message);
  assign(s2, status, NAME_accepted);
  assign(s2, master, s);
  appendChain(s->clients, s2);

  if ( notNil(s->accept_message) )
    forwardReceiverCode(s->accept_message, s, s2, 0);

  inputStream((Stream) s2, DEFAULT);

  succeed;
}


static status
listenSocket(Socket s, Code accept_message, Int backlog)
{ if ( isDefault(backlog) )
    backlog = toInt(5);

  TRY(bindSocket(s));

  if ( listen(SocketHandle(s), valInt(backlog)) )
    return errorPce(s, NAME_socket, NAME_listen, SockError());

  assign(s, status, NAME_listen);
  if ( notDefault(accept_message) )
    assign(s, accept_message, accept_message);
  assign(s, clients, newObject(ClassChain, 0));
  appendChain(SocketChain, s);

#ifndef RANDOM
#define RANDOM() rand()
#endif

  if ( notNil(s->authority) )
  { Int passwd = toInt(RANDOM());

    TRY(openFile(s->authority, NAME_write, DEFAULT, DEFAULT));

    if ( instanceOfObject(s->address, ClassFile) )
    { send(s->authority, NAME_format, CtoName("unix %s\n%d\n"),
	   getAbsolutePathFile(s->address));
    } else if ( instanceOfObject(s->address, ClassTuple) )
    { Tuple t = s->address;

      send(s->authority, NAME_format, CtoName("inet %s:%d\n%d\n"),
	   t->first, t->second, passwd, 0);
    } else
    { send(s->authority, NAME_format, CtoName("inet %s:%d\n%d\n"),
	   getHostnamePce(PCE), s->address, passwd, 0);
    }

    TRY(closeFile(s->authority));
  }

  ws_listen_socket(s);

  succeed;
}


static status
connectSocket(Socket s)
{ int rval;

  TRY(createSocket(s));

#ifdef UNIX_DOMAIN_SOCKETS
  if ( s->domain == NAME_unix )
  { struct sockaddr_un address;
    int len;
    TRY( unix_address_socket(s, &address, &len) );
    rval = connect(SocketHandle(s), (struct sockaddr *) &address, len);
  } else /*if ( s->domain == NAME_inet )*/
#endif
  { struct sockaddr_in address;
    int len;
    TRY( inet_address_socket(s, &address, &len) );
    rval = connect(SocketHandle(s), (struct sockaddr *) &address, len);
  }

  if ( rval )
    return errorPce(s, NAME_socket, NAME_connect, SockError());

#ifdef HAVE_WINSOCK
  s->wrfd = 0;				/* signal open status */
#endif

  assign(s, status, NAME_connected);
  appendChain(SocketChain, s);

  inputStream((Stream)s, DEFAULT);

  succeed;
}

		 /*******************************
		 *	    EXCEPTIONS		*
		 *******************************/

static status
eofSocket(Socket s)
{ if ( s->status == NAME_accepted )
    doneObject(s);

  succeed;
}


static status
brokenPipeSocket(Socket s)
{ succeed;
}


static status
closeSocket(Socket s)
{ deleteChain(SocketChain, s);

  closeInputStream((Stream) s);

  if ( notNil(s->clients) )		/* destroy clients */
  { Socket client;

    for_chain(s->clients, client, closeSocket(client));
  }

  if ( notNil(s->master) )		/* delete client <->master link */
  { if ( notNil(s->master->clients) )
    { addCodeReference(s);
      deleteChain(s->master->clients, s);
      delCodeReference(s);
    }
    assign(s, master, NIL);
  }

  if ( s->domain == NAME_unix &&	/* remove from file-system */
       s->status == NAME_listen )
    removeFile(s->address);

  assign(s, status, NAME_idle);

  succeed;
}


status
makeClassSocket(Class class)
{ sourceClass(class, makeClassSocket, __FILE__, "$Revision$");

  localClass(class, NAME_address, NAME_address, "file|tuple|int*", NAME_get,
	     "Address for the connection end-point");
  localClass(class, NAME_domain, NAME_address, "{unix,inet}", NAME_get,
	     "Domain for the connection");
  localClass(class, NAME_status, NAME_status,
	     "{idle,listen,accepted,connected}", NAME_get,
	     "Status of the associated socket");
  localClass(class, NAME_acceptMessage, NAME_connect, "code*", NAME_both,
	     "Message to accept new client");
  localClass(class, NAME_clients, NAME_server, "chain*", NAME_get,
	     "Chain with accepted connections");
  localClass(class, NAME_master, NAME_server, "socket*", NAME_get,
	     "Socket I accepted a connection for");
  localClass(class, NAME_authority, NAME_authority, "file*", NAME_get,
	     "Name of authority-file (if any)");

  termClass(class, "socket", 1, NAME_address);
  setCloneFunctionClass(class, cloneSocket);
  cloneStyleVariableClass(class, NAME_clients, NAME_nil);
  cloneStyleVariableClass(class, NAME_master, NAME_nil);

  sendMethod(class, NAME_initialise, DEFAULT, 2,
	     "address=file|tuple|int*", "domain=[{unix,inet}]",
	     "Create socket from address and domain",
	     initialiseSocket);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Cleanup socket",
	     unlinkSocket);
  sendMethod(class, NAME_listen, NAME_connect, 2, "[code]*", "[{1..5}]",
	     "Listen for connection requests]",
	     listenSocket);
  sendMethod(class, NAME_connect, NAME_connect, 0,
	     "Connect with server socket",
	     connectSocket);
  sendMethod(class, NAME_close, NAME_output, 0,
	     "Close communication to socket",
	     closeSocket);
  sendMethod(class, NAME_brokenPipe, NAME_control, 0,
	     "Attempt to write on broken connection",
	     brokenPipeSocket);
  sendMethod(class, NAME_endOfFile, NAME_input, 0,
	     "EOF read",
	     eofSocket);

  getMethod(class, NAME_printName, DEFAULT, "string", 0,
	    "returns <classname>(<address>)",
	    getPrintNameSocket);

#ifdef HAVE_SOCKET
  featureClass(class, NAME_unixDomain, ON);
#endif
  featureClass(class, NAME_inetDomain, ON);

  SocketChain = globalObject(NAME_openSockets, ClassChain, 0);

  succeed;
}

#endif /*HAVE_SOCKET*/
