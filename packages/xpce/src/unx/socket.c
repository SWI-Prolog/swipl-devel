/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <md.h>				/* get HAVE_'s */
#if HAVE_SOCKET

#include <sys/types.h>
#include <memory.h>
#include <unistd.h>
#include <sys/socket.h>			/* must be first to avoid send() */
#include <netdb.h>			/* conflict ... */
#include <netinet/in.h>
				
#define create PCEcreate		/* avoid conflict */

#include <h/kernel.h>
#include <h/unix.h>
#include <h/interface.h>
#include <sys/time.h>
#include <sys/un.h>
#include <signal.h>
#include <errno.h>

extern int errno;

#ifdef SOME_MISSING_LIB_PROTOTYPES
extern int socket(int domain, int type, int protocol);
extern int bind(int s, struct sockaddr *name, int namelen);
extern int accept(int s, struct sockaddr *addr, int *addrlen);
extern int listen(int s, int backlog);
extern int connect(int s, struct sockaddr *name, int namelen);
#endif

#define MAX_UN_ADDRESS_LEN (sizeof(struct sockaddr_un) - sizeof(short))

static status	closeSocket(Socket);

#define OsError() getOsErrorPce(PCE)

static Chain	SocketChain;		/* Available open sockets */

static void
closeAllSockets(void)
{ Socket s;

  for_chain(SocketChain, s, closeSocket(s));
}


static void
sigPipeSocket(void)
{ errorPce(PCE, NAME_brokenPipe, 0);
}


static void
setupSockets(void)
{ static int initialised = 0;

  if ( !initialised )
  { hostAction(HOST_ONEXIT, closeAllSockets, NULL);
    hostAction(HOST_SIGNAL, SIGPIPE, sigPipeSocket);
    initialised++;
  }
}


static status
initialiseSocket(Socket s, Any address, Name domain)
{ setupSockets();

  initialiseStream((Stream) s, NIL, NIL, NIL, DEFAULT);

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
  clone->ws_ref = 0;
  clone->input_buffer = NULL;
  clone->input_allocated = s->input_p = 0;

  succeed;
}

		 /*******************************
		 *      CREATE/CONNECT/ETC	*
		 *******************************/

static status
createSocket(Socket s)
{ if ( s->rdfd < 0 )
  { int domain;
   
    closeSocket(s);

    if ( s->domain == NAME_unix )
      domain = PF_UNIX;
    else /*if ( s->domain == NAME_inet )*/
      domain = PF_INET;

    if ( (s->rdfd = s->wrfd = socket(domain, SOCK_STREAM, 0)) < 0 )
      return errorPce(s, NAME_socket, NAME_create, OsError());
  }

  succeed;
}


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
    return errorPce(s->address, NAME_unexpectedType, CtoType("tupple"));

  succeed;
}


static status
bindSocket(Socket s)
{ int rval;

  TRY(createSocket(s));
  if ( s->domain == NAME_unix )
  { struct sockaddr_un address;
    int len;
    TRY( unix_address_socket(s, &address, &len) );
    rval = bind(s->rdfd, (struct sockaddr *) &address, len);
  } else /*if ( s->domain == NAME_inet )*/
  { struct sockaddr_in address;
    int len;
    TRY( inet_address_socket(s, &address, &len) );
    if ( (rval = bind(s->rdfd, (struct sockaddr *) &address, len)) == 0 )
    { if ( s->address == ZERO )
	assign(s, address, toInt(address.sin_port));
      else if ( instanceOfObject(s->address, ClassTuple) &&
		((Tuple)s->address)->second == ZERO )
	assign((Tuple)s->address, second, toInt(address.sin_port));
    }
  }

  if ( rval != 0 )
    return errorPce(s, NAME_socket, NAME_bind, OsError());

  succeed;
}
  

status
acceptSocket(Socket s)
{ int id2;
  Socket s2;
  Any client_address;

  if ( s->domain == NAME_unix )
  { struct sockaddr_un address;
    int len = sizeof(address);

    if ( (id2 = accept(s->rdfd, (struct sockaddr *) &address, &len)) < 0 )
      errorPce(s, NAME_socket, NAME_accept, OsError());
 
    client_address = s->address;
  } else /*if ( s->domain == NAME_inet )*/
  { struct sockaddr_in address;
    int len = sizeof(address);

    if ( (id2 = accept(s->rdfd, (struct sockaddr *) &address, &len)) < 0 )
      errorPce(s, NAME_socket, NAME_accept, OsError());
 
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

  s2->rdfd = s2->wrfd = id2;
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

  if ( listen(s->rdfd, valInt(backlog)) )
    return errorPce(s, NAME_socket, NAME_listen, OsError());

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
  if ( s->domain == NAME_unix )
  { struct sockaddr_un address;
    int len;
    TRY( unix_address_socket(s, &address, &len) );
    rval = connect(s->rdfd, (struct sockaddr *) &address, len);
  } else /*if ( s->domain == NAME_inet )*/
  { struct sockaddr_in address;
    int len;
    TRY( inet_address_socket(s, &address, &len) );
    rval = connect(s->rdfd, (struct sockaddr *) &address, len);
  }

  if ( rval )
    return errorPce(s, NAME_socket, NAME_connect, OsError());

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

  SocketChain = globalObject(NAME_openSockets, ClassChain, 0);

  succeed;
}

#else /*HAVE_SOCKET*/

#include <h/kernel.h>

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

  succeed;
}

#endif /*HAVE_SOCKET*/
