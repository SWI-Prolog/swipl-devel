/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#define O_DEBUG 1

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
History
=======

This module defines the bottom layer for dealing with TCP stream-sockets
from SWI-Prolog, both server and client side.

The basis of this module was implemented by Gordon Streeter. It has been
redesigned to make it a bit  easier   to  use  and handle problems using
Prolog exceptions instead of special return-values.

The   tcp_select()   call   has   been     replaced    by   SWI-Prolog's
wait_for_input/3.


Issues
======

This  module  is  designed  to  provide  relyable  and  portable  TCP/IP
connections and. In addition, if  the  system   is  embedded  in  an GUI
even-driven application we try  to  handle   events  on  behalf  of this
application while blocked in a socket operation.

Please  note  that  in  the  current  version  wait_for_input/3  doesn't
dispatch events. This needs to be fixed.

Windows issues
--------------

Winsock is hard to handle in blocking   mode  without blocking the whole
lot, notably (timeout) signals. We  therefore   have  a  seperate thread
dealing with I/O and  operating   the  sockets  through WSAAsyncSelect()
generated events. Requests are registered   with the plsocket structure,
handled and handled in the socket thread.  Upon completion, a message is
sent back to the waiting thread.

Unix issues
-----------

In the Unix version we simply call PL_dispatch() before doing recv() and
leave the details to this function.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef __CYGWIN__
#undef HAVE_H_ERRNO
#endif

#include <SWI-Stream.h>
#include "clib.h"

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#ifdef WIN32
#include <io.h>
#include <winsock2.h>
#else
#include <sys/types.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#ifdef HAVE_H_ERRNO
extern int h_errno;
#else
#define h_errno errno
#endif
#define closesocket(n) close((n))	/* same on Unix */
#endif
#include <assert.h>
#include <string.h>

#ifndef SD_SEND
#define SD_RECEIVE 0			/* shutdown() parameters */
#define SD_SEND    1
#define SD_BOTH    2
#endif

#ifndef SOCKET_ERROR
#define SOCKET_ERROR (-1)
#endif

#ifdef _REENTRANT
#include <pthread.h>

static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
#define LOCK() pthread_mutex_lock(&mutex)
#define UNLOCK() pthread_mutex_unlock(&mutex)
#else
#define LOCK()
#define UNLOCK()
#endif

static atom_t ATOM_reuseaddr;		/* "reuseaddr" */
static atom_t ATOM_dispatch;		/* "dispatch" */

#define SOCK_INSTREAM	0x01
#define SOCK_OUTSTREAM	0x02
#define SOCK_BIND	0x04		/* What have we done? */
#define SOCK_LISTEN	0x08
#define SOCK_CONNECT	0x10
#define SOCK_ACCEPT	0x20		/* Set on accepted sockets */
#define SOCK_NONBLOCK	0x40		/* Set to non-blocking mode */
#define SOCK_DISPATCH   0x80		/* do not dispatch events */
#define SOCK_CLOSE_SEEN	0x100		/* FD_CLOSE seen */
#define SOCK_EOF_SEEN   0x200		/* Seen end-of-file */

#define set(s, f)   ((s)->flags |= (f))
#define clear(s, f) ((s)->flags &= ~(f))
#define true(s, f)  ((s)->flags & (f))
#define false(s, f) (!true(s, f))

typedef enum
{ REQ_NONE = 0,				/* no request pending */
  REQ_ACCEPT,
  REQ_CONNECT,
  REQ_READ,
  REQ_WRITE
} tcp_request;

#define SOCK_MAGIC 0x38da3f2c

typedef struct _plsocket
{ int		    magic;		/* SOCK_MAGIC */
  struct _plsocket *next;		/* next in list */
  int		    socket;		/* The OS socket */
  int		    flags;		/* Misc flags */
  IOSTREAM *	    input;		/* input stream */
  IOSTREAM *	    output;		/* output stream */
#ifdef WIN32
  tcp_request	    request;		/* our request */
  DWORD		    thread;		/* waiting thread */
  DWORD		    error;		/* error while executing request */
  int		    done;		/* request completed */
  int		    w32_flags;		/* or of received FD_* */
  union
  { struct
    { struct sockaddr_in addr;		/* accepted address */
      int addrlen;			/* address length */
      int slave;			/* descriptor of slave */
    } accept;
    struct
    { struct sockaddr_in addr;		/* accepted address */
      int addrlen;			/* address length */
    } connect;
    struct
    { int bytes;			/* byte count */
      char *buffer;			/* the buffer */
      int size;				/* buffer size */
    } read;
    struct
    { int bytes;			/* byte count */
      char *buffer;			/* the buffer */
      int written;
      int size;				/* buffer size */
    } write;
  } rdata;
#endif
} plsocket;

static plsocket *lookupSocket(int socket);
#ifdef WIN32
static plsocket   *lookupExistingSocket(int socket);
static const char *WinSockError(unsigned long eno);
#endif

#ifdef O_DEBUG
static int debugging;
#define DEBUG(l, g) if ( debugging >= l ) g
#else
#define DEBUG(l, g) (void)0
#endif

		 /*******************************
		 *	  COMPATIBILITY		*
		 *******************************/

#ifdef WIN32
#define F_SETFL 0
#define O_NONBLOCK 0

#define WM_SOCKET	(WM_USER+20)
#define WM_REQUEST	(WM_USER+21)
#define WM_READY	(WM_USER+22)
#define WM_DONE		(WM_USER+23)

static int
fcntl(int fd, int op, int arg)
{ switch(op)
  { case F_SETFL:
      switch(arg)
      { case O_NONBLOCK:
	{ int rval;
	  int non_block;

	  non_block = 1;
	  rval = ioctlsocket(fd, FIONBIO, &non_block);
	  return rval ? -1 : 0;
	}
	default:
	  return -1;
      }
    break;
    default:
      return -1;
  }
}

static HINSTANCE hinstance;		/* hinstance */

typedef struct
{ HWND   hwnd;				/* our window */
  DWORD  tid;				/* thread id */
} local_state;

static local_state tcp_state;
#define State() (&tcp_state)

static int
doneRequest(plsocket *s)
{ s->done = TRUE;
  s->request = REQ_NONE;

  if ( s->thread )
  { DEBUG(2, Sdprintf("doneRequest(): posting %d\n", s->thread));
    PostThreadMessage(s->thread, WM_DONE, 0, (WPARAM)s);
  }

  return TRUE;
}


static int
waitRequest(plsocket *s)
{ DEBUG(2, Sdprintf("[%d] (%ld): Waiting ...",
		    PL_thread_self(), s->thread));

  for(;;)
  { MSG msg;
  
    if ( PL_handle_signals() < 0 )
    { DEBUG(1, Sdprintf("[%d]: Exception\n", PL_thread_self()));
      return FALSE;
    }
    if ( s->done )
    { DEBUG(2, Sdprintf("[%d]: Done\n", PL_thread_self()));
      return TRUE;
    }

    if ( false(s, SOCK_DISPATCH) )
    { if ( !GetMessage(&msg, NULL, WM_DONE, WM_DONE) )
	return FALSE;
    } else if ( GetMessage(&msg, NULL, 0, 0) )
    { TranslateMessage(&msg);
      DispatchMessage(&msg);
    } else
    { ExitProcess(0);			/* WM_QUIT received */
      return FALSE;			/* NOTREACHED */
    }
  }
}

static int
placeRequest(plsocket *s, tcp_request request)
{ s->error   = 0;
  s->done    = FALSE;
  s->thread  = GetCurrentThreadId();
  s->request = request;

  PostMessage(State()->hwnd, WM_REQUEST, 0, (LPARAM)s);
  DEBUG(2, Sdprintf("%d (%ld): Placed request %d\n",
		    PL_thread_self(), s->thread, request));

  return TRUE; 
}

static int
doRequest(plsocket *s)
{ switch(s->request)
  { case REQ_NONE:
      break;
    case REQ_CONNECT:
      if ( s->w32_flags & FD_CONNECT )
      { s->w32_flags &= ~FD_CONNECT;

	if ( connect(s->socket,
		     (struct sockaddr*)&s->rdata.connect.addr,
		     s->rdata.connect.addrlen) )
	{ s->error = WSAGetLastError();
	  
	  switch(s->error)
	  { case WSAEWOULDBLOCK:
	    case WSAEINVAL:
	    case WSAEALREADY:
	      break;
	    case WSAEISCONN:
	      s->error = 0;
	      doneRequest(s);
	      break;
	    default:
	      doneRequest(s);
	  }
	} else
	{ s->error = 0;
	  doneRequest(s);
	}
      }
      break;
    case REQ_ACCEPT:
      if ( s->w32_flags & FD_ACCEPT )
      { s->w32_flags &= ~FD_ACCEPT;

	s->rdata.accept.slave = accept(s->socket,
				       (struct sockaddr*)&s->rdata.accept.addr,
				       &s->rdata.accept.addrlen);

	DEBUG(2, Sdprintf("Accept() --> %d\n", s->rdata.accept.slave));

	if ( s->rdata.accept.slave == SOCKET_ERROR )
	{ s->error = WSAGetLastError();
	  
	  if ( s->error != WSAEWOULDBLOCK )
	    doneRequest(s);
	} else
	{ plsocket *s2;

	  s2 = lookupSocket(s->rdata.accept.slave);
	  s2->flags |= SOCK_ACCEPT;
	  s->error = 0;
	  doneRequest(s);
	}
      }
      break;
    case REQ_READ:
      if ( s->w32_flags & (FD_READ|FD_CLOSE) )
      { s->w32_flags &= ~FD_READ;

	s->rdata.read.bytes = recv(s->socket,
				   s->rdata.read.buffer,
				   s->rdata.read.size,
				   0);
	if ( s->rdata.read.bytes < 0 )
	{ s->error = WSAGetLastError();

	  if ( s->error != WSAEWOULDBLOCK )
	  { DEBUG(1, Sdprintf("Error reading from %d: %s\n",
			      s->socket, WinSockError(s->error)));
	    doneRequest(s);
	  }
	} else
	  doneRequest(s);
      }
      break;
    case REQ_WRITE:
      if ( s->w32_flags & FD_WRITE )
      { int n;
	s->w32_flags &= ~FD_WRITE;

	DEBUG(2, Sdprintf("send() %d bytes\n", s->rdata.write.size));
	n = send(s->socket,
		 s->rdata.write.buffer + s->rdata.write.written,
		 s->rdata.write.size - s->rdata.write.written,
		 0);
	DEBUG(2, Sdprintf("Wrote %d bytes\n", n));
	if ( n < 0 )
	{ s->error = WSAGetLastError();
	  if ( s->error == WSAEWOULDBLOCK )
	    break;
	  s->rdata.write.bytes = n;
	  doneRequest(s);
	} else
	  s->error = 0;
	  
	s->rdata.write.written += n;
	if ( s->rdata.write.written >= s->rdata.write.size )
	{ s->rdata.write.bytes = s->rdata.write.written;
	  doneRequest(s);
	}
      }
  }

  return TRUE;
}


static int WINAPI
socket_wnd_proc(HWND hwnd, UINT message, UINT wParam, LONG lParam)
{ switch( message )
  { case WM_REQUEST:
    { plsocket *s = (plsocket *)lParam;

      doRequest(s);

      return 0;
    }

    case WM_SOCKET:
    { SOCKET sock = (SOCKET) wParam;
      int err     = WSAGETSELECTERROR(lParam);
      int evt     = WSAGETSELECTEVENT(lParam);
      plsocket *s = lookupExistingSocket(sock);

      if ( s )
      { s->w32_flags |= evt;
	s->error = err;
	doRequest(s);
      } else
	DEBUG(2, Sdprintf("Socket %d is gone\n", sock));
    }
  }

  return DefWindowProc(hwnd, message, wParam, lParam);
}

static char *
HiddenFrameClass()
{ static char *name;
  static WNDCLASS wndClass;

  if ( !name )
  { char buf[50];

    hinstance = GetModuleHandle("socket");
    sprintf(buf, "PlSocketWin%d", (int)hinstance);
    name = strdup(buf);

    wndClass.style		= 0;
    wndClass.lpfnWndProc	= (LPVOID) socket_wnd_proc;
    wndClass.cbClsExtra		= 0;
    wndClass.cbWndExtra		= 0;
    wndClass.hInstance		= hinstance;
    wndClass.hIcon		= NULL;
    wndClass.hCursor		= NULL;
    wndClass.hbrBackground	= GetStockObject(WHITE_BRUSH);
    wndClass.lpszMenuName	= NULL;
    wndClass.lpszClassName	= name;

    RegisterClass(&wndClass);
  }

  return name;
}


static void
destroyHiddenWindow(int rval, void *closure)
{ local_state *s = State();

  if ( s->hwnd )
  { DestroyWindow(s->hwnd);
    s->hwnd = 0;
  }
}


static HWND
SocketHiddenWindow()
{ local_state *s = State();

  if ( !s->hwnd )
  { s->hwnd = CreateWindow(HiddenFrameClass(),
			   "SWI-Prolog socket window",
			   WS_POPUP,
			   0, 0, 32, 32,
			   NULL, NULL, hinstance, NULL);
    assert(s->hwnd);
    DEBUG(1, Sdprintf("%d created hidden window %p\n",
		   PL_thread_self(), s->hwnd));
  }

  return s->hwnd;
}


DWORD WINAPI
socket_thread(LPVOID arg)
{ DWORD parent = (DWORD)arg;

  SocketHiddenWindow();
  PostThreadMessage(parent, WM_READY, (WPARAM)0, (LPARAM)0);

  for(;;)
  { MSG msg;

    if ( GetMessage(&msg, NULL, 0, 0) )
    { TranslateMessage(&msg);
      DispatchMessage(&msg);
    }
  }

  return 0;
}


static int
startSocketThread()
{ DWORD me = GetCurrentThreadId();
  MSG msg;

  CreateThread(NULL,			/* security */
	       2048,			/* stack */
	       socket_thread, (LPVOID)me,	/* proc+arg */
	       0,			/* flags */
	       &State()->tid);

  GetMessage(&msg, NULL, WM_READY, WM_READY);
  DEBUG(1, Sdprintf("Socket thread started\n"));

  return TRUE;
}

#else /*WIN32*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
wait_socket() is the Unix way  to  wait   for  input  on  the socket. By
default event-dispatching on behalf of XPCE is performed. If this is not
desired, you can do tcp_setopt(Socket,   dispatch(false)), in which case
this call returns immediately, assuming the   actual TCP call will block
without dispatching if no input is available.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
wait_socket(plsocket *s, int fd)
{ if ( true(s, SOCK_DISPATCH) )
    return PL_dispatch(fd, PL_DISPATCH_WAIT);

  return TRUE;
}

#endif /*WIN32*/


		 /*******************************
		 *	 ADMINISTRATION		*
		 *******************************/

static functor_t FUNCTOR_socket1;
static functor_t FUNCTOR_module2;
static functor_t FUNCTOR_ip4;

static plsocket *sockets;
static int initialised = FALSE;		/* Windows only */

#ifdef WIN32
static plsocket *
lookupExistingSocket(int socket)
{ plsocket *p;

  LOCK();
  for(p=sockets; p; p = p->next)
  { if ( p->socket == socket )
    { UNLOCK();
      return p;
    }
  }
  UNLOCK();
  return NULL;
}
#endif


static plsocket *
lookupSocket(int socket)
{ plsocket *p;

  LOCK();
  for(p=sockets; p; p = p->next)
  { if ( p->socket == socket )
    { UNLOCK();
      return p;
    }
  }

  if ( !(p = PL_malloc(sizeof(plsocket))) )
  { pl_error(NULL, 0, NULL, ERR_ERRNO);
    UNLOCK();
    return NULL;
  }

  p->socket = socket;
  p->flags  = SOCK_DISPATCH;		/* by default, dispatch */
  p->magic  = SOCK_MAGIC;
#ifdef WIN32
  p->w32_flags = 0;
  p->request   = REQ_NONE;
#endif
  p->input = p->output = (IOSTREAM*)NULL;
  p->next   = sockets;
  sockets   = p;

  DEBUG(2, Sdprintf("[%d]: lookupSocket(%d): bound to %p\n",
		    PL_thread_self(), socket, p));

  UNLOCK();
  return p;
}


static int
freeSocket(int socket)
{ plsocket **p;
  int rval;

  DEBUG(2, Sdprintf("Closing %d\n", socket));

  LOCK();
  p = &sockets;

  for( ; *p; p = &(*p)->next)
  { if ( (*p)->socket == socket )
    { plsocket *s = *p;
      
      *p = s->next;
      PL_free(s);
      break;
    }
  }
  UNLOCK();

  again:
  if ( (rval=closesocket(socket)) == SOCKET_ERROR )
  { if ( errno == EINTR )
      goto again;
  }

  DEBUG(2, Sdprintf("freeSocket(%d) returned %d\n", socket, rval));

  return rval;
}


		 /*******************************
		 *	     CONVERSION		*
		 *******************************/

static int
tcp_get_socket(term_t Socket, int *id)
{ IOSTREAM *s;
  int socket;

  if ( PL_is_functor(Socket, FUNCTOR_socket1) )
  { term_t a = PL_new_term_ref();

    PL_get_arg(1, Socket, a);
    if ( PL_get_integer(a, id) )
      return TRUE;
  }
  
  if ( PL_get_stream_handle(Socket, &s) &&
       (socket = Sfileno(s)) >= 0 )
  { *id = socket;
    return TRUE;
  }

  return pl_error(NULL, 0, NULL, ERR_ARGTYPE, -1, Socket, "socket");
}


static int
tcp_unify_socket(term_t Socket, int id)
{ return PL_unify_term(Socket,
		       PL_FUNCTOR, FUNCTOR_socket1,
		         IntArg(id));
}


		 /*******************************
		 *	      ERRORS		*
		 *******************************/

#ifdef WIN32
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The code in BILLY_GETS_BETTER is, according to various documents the
right code, but it doesn't work, so we do it by hand.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef BILLY_GETS_BETTER

static const char *
WinSockError(unsigned long eno)
{ char buf[1024];
  static HMODULE netmsg = 0;
  static int netmsg_loaded = FALSE;
  unsigned long flags = (FORMAT_MESSAGE_FROM_SYSTEM|
			 FORMAT_MESSAGE_IGNORE_INSERTS); 

  if ( !netmsg_loaded )
  { netmsg_loaded = TRUE;
    netmsg = LoadLibraryEx("netmsg.dll", 0, LOAD_LIBRARY_AS_DATAFILE);
    if ( !netmsg )
      Sdprintf("failed to load netmsg.dll\n");
    else
      Sdprintf("Loaded netmsg.dll as %p\n", netmsg); 
  }

  if ( netmsg )
    flags |= FORMAT_MESSAGE_FROM_HMODULE;

  if ( !FormatMessage(flags,
		      netmsg,
		      eno,
		      GetUserDefaultLangID(),
		      /*MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),*/
		      buf, sizeof(buf),
		      0))
  { sprintf(buf, "Unknown socket error (%u)", eno);
  }

  buf[sizeof(buf)-1]='\0';

  return strdup(buf);
}

#else /*BILLY_GETS_BETTER*/

static const char *
WinSockError(unsigned long error)
{ struct
  { int index;
    const char *string;
  } *ep, edefs[] =
  { { WSAEACCES, "Permission denied" },
    { WSAEADDRINUSE, "Address already in use" },
    { WSAEADDRNOTAVAIL, "Cannot assign requested address" },
    { WSAEAFNOSUPPORT, "Address family not supported by protocol family" },
    { WSAEALREADY, "Operation already in progress" },
    { WSAECONNABORTED, "Software caused connection abort" },
    { WSAECONNREFUSED, "Connection refused" },
    { WSAECONNRESET, "Connection reset by peer" },
    { WSAEDESTADDRREQ, "Destination address required" },
    { WSAEFAULT, "Bad address" },
    { WSAEHOSTDOWN, "Host is down" },
    { WSAEHOSTUNREACH, "No route to host" },
    { WSAEINPROGRESS, "Operation now in progress" },
    { WSAEINTR, "Interrupted function call" },
    { WSAEINVAL, "Invalid argument" },
    { WSAEISCONN, "Socket is already connected" },
    { WSAEMFILE, "Too many open files" },
    { WSAEMSGSIZE, "Message too long" },
    { WSAENETDOWN, "Network is down" },
    { WSAENETRESET, "Network dropped connection on reset" },
    { WSAENETUNREACH, "Network is unreachable" },
    { WSAENOBUFS, "No buffer space available" },
    { WSAENOPROTOOPT, "Bad protocol option" },
    { WSAENOTCONN, "Socket is not connected" },
    { WSAENOTSOCK, "Socket operation on non-socket" },
    { WSAEOPNOTSUPP, "Operation not supported" },
    { WSAEPFNOSUPPORT, "Protocol family not supported" },
    { WSAEPROCLIM, "Too many processes" },
    { WSAEPROTONOSUPPORT, "Protocol not supported" },
    { WSAEPROTOTYPE, "Protocol wrong type for socket" },
    { WSAESHUTDOWN, "Cannot send after socket shutdown" },
    { WSAESOCKTNOSUPPORT, "Socket type not supported" },
    { WSAETIMEDOUT, "Connection timed out" },
    { WSAEWOULDBLOCK, "Resource temporarily unavailable" },
    { WSAEDISCON, "Graceful shutdown in progress" },
    { WSANOTINITIALISED, "Socket layer not initialised" },
					/* WinSock 2 errors */
    { WSAHOST_NOT_FOUND, "Host not found" },
    { 0, NULL }
  };
  char tmp[100];

  for(ep=edefs; ep->string; ep++)
  { if ( ep->index == (int)error )
      return ep->string;
  }

  sprintf(tmp, "Unknown error %ld", error);
  return strdup(tmp);			/* leaks memory */
}

#endif /*BILLY_GETS_BETTER*/
#endif /*WIN32*/

#ifdef HAVE_H_ERRNO
typedef struct
{ int code;
  const char *string;
} error_codes;

static error_codes h_errno_codes[] = {
#ifdef HOST_NOT_FOUND
    { HOST_NOT_FOUND, "Host not found" },
#endif
#ifdef TRY_AGAIN
    { TRY_AGAIN, "Try Again" },
#endif
#ifdef NO_RECOVERY
    { NO_RECOVERY, "No Recovery" },
#endif
#ifdef NO_DATA
    { NO_DATA, "No Data" },
#endif
#ifdef NO_ADDRESS
    { NO_ADDRESS, "No Address" },
#endif
    {0, NULL}
};

#else /*HAVE_H_ERRNO*/
#define h_errno_codes NULL
typedef void * error_codes;
#endif /*HAVE_H_ERRNO*/

static int
tcp_error(int code, error_codes *map)
{ const char *msg;
  term_t except = PL_new_term_ref();

#ifdef WIN32
  msg = WinSockError(code);
#else

#ifdef HAVE_H_ERRNO
  static char msgbuf[100];

  if ( map )
  { while( map->code && map->code != code )
      map++;
    if ( map->code )
      msg = map->string;
    else
    { sprintf(msgbuf, "Unknown error %d", code);
      msg = msgbuf;
    }
  } else
#endif
    msg = strerror(code);
#endif /*WIN32*/

  PL_unify_term(except,
		CompoundArg("error", 2),
		  CompoundArg("socket_error", 1),
		    AtomArg(msg),
		  PL_VARIABLE);

  return PL_raise_exception(except);
}

		 /*******************************
		 *	  INITIALISATION	*
		 *******************************/

static int
tcp_init()
{ LOCK();
  if ( initialised )
  { UNLOCK();
    return TRUE;
  }
  initialised = TRUE;

#ifdef WIN32
{ WSADATA WSAData;

  if ( WSAStartup(MAKEWORD(2,0), &WSAData) )
  { UNLOCK();
    return PL_warning("tcp_init() - WSAStartup failed.");
  }
  startSocketThread();
}
#endif /*WIN32*/

  UNLOCK();
  return TRUE;
}
	

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
socket(-Socket)
    Create a stream inet socket.  The socket is represented by a term of
    the format $socket(Id).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
tcp_socket(term_t Socket)
{ int sock;
  plsocket *s;
	
  if ( !tcp_init() )
    return FALSE;

  if ( (sock = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    return tcp_error(errno, NULL);

  if ( !(s=lookupSocket(sock)) )		/* register it */
  { closesocket(sock);
    return FALSE;
  }

#ifdef WIN32
  WSAAsyncSelect(sock, State()->hwnd, WM_SOCKET,
		 FD_READ|FD_WRITE|FD_ACCEPT|FD_CONNECT|FD_CLOSE);
#endif

  return tcp_unify_socket(Socket, sock);
}


static foreign_t
tcp_close_socket(term_t Socket)
{ int socket;
  plsocket *s;

  if ( !tcp_get_socket(Socket, &socket) )
    return FALSE;
  s = lookupSocket(socket);

  if ( true(s, SOCK_OUTSTREAM|SOCK_INSTREAM) )
  { int flags = s->flags;		/* may drop out! */

    if ( flags & SOCK_INSTREAM )
    { assert(s->input);
      Sclose(s->input);
    }
    if ( flags & SOCK_OUTSTREAM )
    { assert(s->output);
      Sclose(s->output);
    }
  } else
  {
#ifdef WIN32
    if ( true(s, SOCK_CONNECT) )
      shutdown(socket, SD_SEND);
#endif

    freeSocket(socket);
  }

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Translate a host + port-number into a sockaddr structure.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
tcp_get_port(term_t Port, int *port)
{ char *name;

  if ( PL_get_atom_chars(Port, &name) )
  { struct servent *service;
    
    if ( !(service = getservbyname(name, "tcp")) )
      return tcp_error(errno, NULL);

    *port = ntohs(service->s_port);
    return TRUE;
  }

  if ( PL_get_integer(Port, port) )
    return TRUE;

  return pl_error(NULL, 0, NULL, ERR_ARGTYPE, -1, Port, "port");
}


static int
tcp_get_sockaddr(term_t Address, struct sockaddr_in *addr)
{ struct hostent *host;
  char           *hostName = NULL;
  int		  port;
	
  addr->sin_family = AF_INET;

  if ( PL_is_functor(Address, FUNCTOR_module2) )
  { term_t arg = PL_new_term_ref();

    PL_get_arg(1, Address, arg);
    if ( !PL_get_atom_chars(arg, &hostName) )
      return pl_error(NULL, 0, NULL, ERR_ARGTYPE, 1, arg, "atom");

    PL_get_arg(2, Address, arg);
    if ( !tcp_get_port(arg, &port) )
      return FALSE;
  } else if ( PL_is_variable(Address) )
  { port = 0;
  } else if ( !tcp_get_port(Address, &port) )
    return FALSE;

  if ( hostName )
  { if( !(host = gethostbyname(hostName)) )
      return tcp_error(h_errno, h_errno_codes);
    if ( (int)sizeof(addr->sin_addr) < host->h_length )
      return PL_warning("Oops, host address too long!");
    memcpy(&addr->sin_addr, host->h_addr, host->h_length);
  } else
    addr->sin_addr.s_addr = INADDR_ANY;
	
  addr->sin_port = htons((short)port);

  return TRUE;
}


static int
tcp_get_ip(term_t Ip, struct in_addr *ip)
{ unsigned long hip = 0;

  if ( PL_is_functor(Ip, FUNCTOR_ip4) )
  { int i, ia;
    term_t a = PL_new_term_ref();

    for(i=1; i<=4; i++)
    { PL_get_arg(i, Ip, a);
      if ( PL_get_integer(a, &ia) )
	hip |= ia << ((4-i)*8);
      else
	return FALSE;
    }
    hip = htonl(hip);
    memcpy(ip, &hip, sizeof(hip));

    return TRUE;
  }

  return FALSE;
}


static int
tcp_unify_ip(term_t Ip, struct in_addr *ip, int netorder)
{ unsigned long hip;

  if ( netorder )
    hip = ntohl(ip->s_addr);
  else
    hip = ip->s_addr;

  return PL_unify_term(Ip,
		       PL_FUNCTOR, FUNCTOR_ip4,
		         IntArg((hip >> 24) & 0xff),
		         IntArg((hip >> 16) & 0xff),
		         IntArg((hip >>  8) & 0xff),
		         IntArg((hip >>  0) & 0xff));
}


static foreign_t
tcp_host_to_address(term_t Host, term_t Ip)
{ struct in_addr ip;
  struct hostent *host;
  char *host_name;

  if ( PL_get_atom_chars(Host, &host_name) )
  { if ( (host = gethostbyname(host_name)) )
    { if ( sizeof(ip) == host->h_length )
      { memcpy(&ip, host->h_addr, host->h_length);
	return tcp_unify_ip(Ip, &ip, TRUE);
      } else
	return PL_warning("tcp_host_to_address/2: length mismatch in address");
    } else
      return tcp_error(h_errno, h_errno_codes);
  } else if ( tcp_get_ip(Ip, &ip) )
  { if ( (host = gethostbyaddr((char *)&ip, sizeof(ip), AF_INET)) )
      return PL_unify_atom_chars(Host, host->h_name);
    else
      return tcp_error(h_errno, h_errno_codes);
  }

  return FALSE;
}


foreign_t
tcp_setopt(term_t Socket, term_t opt)
{ int socket;
  atom_t a;
  int arity;
       
  if ( !tcp_get_socket(Socket, &socket) )
    return FALSE;

  if ( PL_get_name_arity(opt, &a, &arity) )
  { if ( a == ATOM_reuseaddr && arity == 0 )
    { int val = 1;

      if( setsockopt(socket, SOL_SOCKET, SO_REUSEADDR,
		     (const char *)&val, sizeof(val)) == -1)
      { return tcp_error(h_errno, h_errno_codes);
      }

      return TRUE;
    } else if ( a == ATOM_dispatch && arity == 1 )
    { int val;
      term_t a1 = PL_new_term_ref();
      plsocket *s = lookupSocket(socket);

      if ( PL_get_arg(1, opt, a1) && PL_get_bool(a1, &val) )
      { if ( val )
	  set(s, SOCK_DISPATCH);
	else
	  clear(s, SOCK_DISPATCH);

	return TRUE;
      }
    }
  }
       
  return pl_error(NULL, 0, NULL, ERR_DOMAIN, opt, "socket_option");
}


foreign_t
tcp_bind(term_t Socket, term_t Address)
{ struct sockaddr_in sockaddr;
  int socket;
       
  if ( !tcp_get_socket(Socket, &socket) ||
       !tcp_get_sockaddr(Address, &sockaddr) )
    return FALSE;
	
  if ( bind(socket,
	    (struct sockaddr*)&sockaddr, sizeof(sockaddr)))
    return tcp_error(errno, NULL);

  if ( PL_is_variable(Address) )
  { struct sockaddr_in addr;
    int len = sizeof(addr);

    if ( getsockname(socket, (struct sockaddr *) &addr, &len) )
      return tcp_error(errno, NULL);
    PL_unify_integer(Address, ntohs(addr.sin_port));
  }

  lookupSocket(socket)->flags |= SOCK_BIND;

  return TRUE;
}


foreign_t
tcp_connect(term_t Socket, term_t Address)
{ struct sockaddr_in sockaddr;
  int socket;
  plsocket *s;
       
  if ( !tcp_get_socket(Socket, &socket) ||
       !tcp_get_sockaddr(Address, &sockaddr) )
    return FALSE;

  s = lookupSocket(socket);
  
#ifdef WIN32
  if ( connect(socket,
	       (struct sockaddr*)&sockaddr, sizeof(sockaddr)) )
  { s->error = WSAGetLastError();

    if ( s->error == WSAEWOULDBLOCK )
    { s->rdata.connect.addr = sockaddr;
      s->rdata.connect.addrlen = sizeof(sockaddr);
      placeRequest(s, REQ_CONNECT);
      waitRequest(s);
    }

    if ( s->error )
      return tcp_error(s->error, NULL);
  }
#else /*!WIN32*/
  for(;;)
  { if ( connect(socket,
		 (struct sockaddr*)&sockaddr, sizeof(sockaddr)))
    { if ( errno == EINTR )
      { if ( PL_handle_signals() < 0 )
	  return FALSE;
	continue;
      }
      return tcp_error(errno, NULL);
    } else
      break;
  }
#endif

  s->flags |= SOCK_CONNECT;

  return TRUE;
}


static foreign_t
tcp_accept(term_t Master, term_t Slave, term_t Peer)
{ int master, slave;
  struct sockaddr_in addr;
  int addrlen = sizeof(addr);
  plsocket *m;
	
  if ( !tcp_get_socket(Master, &master) )
    return FALSE;
  m = lookupSocket(master);

#ifdef WIN32
  m->rdata.accept.addrlen = sizeof(m->rdata.accept.addr);
  placeRequest(m, REQ_ACCEPT);
  if ( !waitRequest(m) )
    return FALSE;
  if ( m->error )
    return tcp_error(m->error, NULL);
  addr    = m->rdata.accept.addr;
  addrlen = m->rdata.accept.addrlen;
  slave   = m->rdata.accept.slave;
#else /*WIN32*/

  for(;;)
  { if ( !wait_socket(m, master) )
      return FALSE;

    slave = accept(master, (struct sockaddr*)&addr, &addrlen);

    if ( slave == SOCKET_ERROR )
    { if ( errno == EINTR )
      { if ( PL_handle_signals() < 0 )
	  return FALSE;

	continue;
      } else
	return tcp_error(errno, NULL);
    } else
      break;
  }

  { plsocket *s = lookupSocket(slave);
    s->flags |= SOCK_ACCEPT;
  }

#endif /*WIN32*/
  
  if ( tcp_unify_ip(Peer, &addr.sin_addr, TRUE) &&
       tcp_unify_socket(Slave, slave) )
    return TRUE;

  return FALSE;
}



foreign_t
tcp_listen(term_t Sock, term_t BackLog)
{ int socket;
  int backlog;

  if ( !tcp_get_socket(Sock, &socket) )
    return FALSE;

  if ( !PL_get_integer(BackLog, &backlog) ) 
    return pl_error(NULL, 0, NULL, ERR_ARGTYPE, -1, BackLog, "integer");

  if( listen(socket, backlog) == -1 )
    return tcp_error(errno, NULL);

  lookupSocket(socket)->flags |= SOCK_LISTEN;

  return TRUE;
}


		 /*******************************
		 *	  IO-STREAM STUFF	*
		 *******************************/

#define fdFromHandle(p) ((int)((long)(p)))

static int
tcp_read(void *handle, char *buf, int bufSize)
{ int socket = fdFromHandle(handle);
  plsocket *s = lookupSocket(socket);
  int n;

  if ( !s || s->magic != SOCK_MAGIC )
  { errno = EINVAL;
    DEBUG(1, Sdprintf("tcp_read(): Invalid handle: %p\n", handle));
    return -1;
  }

#ifdef WIN32

  s->rdata.read.buffer = buf;
  s->rdata.read.size   = bufSize;
  placeRequest(s, REQ_READ);
  if ( !waitRequest(s) )
  { errno = EPLEXCEPTION;
    return -1;
  }
  n = s->rdata.read.bytes;
  if ( n < 0 )
    errno = EIO;			/* TBD: map s->error to POSIX errno */

#else /*WIN32*/

  for(;;)
  { if ( !wait_socket(s, socket) )
    { errno = EPLEXCEPTION;
      return -1;
    }

    n = recv(socket, buf, bufSize, 0);

    if ( n == -1 && errno == EINTR )
    { if ( PL_handle_signals() < 0 )
      { errno = EPLEXCEPTION;
	return -1;
      }
      continue;
    }

    break;
  }

#endif /*WIN32*/

  if ( n == 0 )
    set(s, SOCK_EOF_SEEN);

  return n;
}

static int
tcp_write(void *handle, char *buf, int bufSize)
{ int socket = fdFromHandle(handle);
  int len = bufSize;
  char *str = buf;

#ifdef WIN32
  while( len > 0 )
  { int n;

    n = send(socket, str, len, 0);
    if ( n < 0 )
    { int error = WSAGetLastError();

      if ( error == WSAEWOULDBLOCK )
	break;

      return -1;
    }

    len -= n;
    str += n;
  }

  if ( len > 0 )			/* operation would block */
  { plsocket *s = lookupSocket(socket);

    s->rdata.write.buffer  = str;
    s->rdata.write.size    = len;
    s->rdata.write.written = 0;
    placeRequest(s, REQ_WRITE);
    if ( !waitRequest(s) )
    { errno = EPLEXCEPTION;
      return -1;
    }
    if ( s->rdata.write.bytes < 0 )
      return -1;
  }

#else /*WIN32*/

  while( len > 0 )
  { int n;

    n = send(socket, str, len, 0);
    if ( n < 0 )
    { if ( errno == EINTR )
      { if ( PL_handle_signals() < 0 )
	{ errno = EPLEXCEPTION;
	  return -1;
	}
	continue;
      }
      return -1;
    }

    len -= n;
    str += n;
  }

#endif /*WIN32*/

  return bufSize;
}


static long
tcp_seek_null(void *handle, long offset, int whence)
{ return -1;
}


static int
tcp_close_input(void *handle)
{ int socket = fdFromHandle(handle);
  plsocket *s = lookupSocket(socket);

  DEBUG(2, Sdprintf("[%d]: tcp_close_input(%d)\n", PL_thread_self(), socket));
  s->input = NULL;
  s->flags &= ~SOCK_INSTREAM;
#ifdef WIN32
  if ( false(s, SOCK_LISTEN) )
  { if ( shutdown(socket, SD_RECEIVE) == SOCKET_ERROR )
      Sdprintf("shutdown(%d, SD_RECEIVE) failed: %s\n",
	       socket,
	       WinSockError(WSAGetLastError()));
  }
#endif

  if ( !(s->flags & (SOCK_INSTREAM|SOCK_OUTSTREAM)) )
    return freeSocket(socket);

  return 0;
}


static int
tcp_close_output(void *handle)
{ int socket = fdFromHandle(handle);
  plsocket *s = lookupSocket(socket);

  DEBUG(2, Sdprintf("[%d]: tcp_close_output(%d)\n", PL_thread_self(), socket));
  if ( s->output )
  { s->output = NULL;
    s->flags &= ~SOCK_OUTSTREAM;
#if WIN32
    if ( shutdown(socket, SD_SEND) == SOCKET_ERROR )
    {
#ifdef O_DEBUG
      if ( debugging )
      { const char *msg;
#ifdef WIN32
	msg = WinSockError(WSAGetLastError());
#else
        msg = strerror(errno);
#endif
        Sdprintf("shutdown(%d, SD_SEND) failed: %s\n", socket, msg);
      }
#endif
    }
#endif
  }

  if ( !(s->flags & (SOCK_INSTREAM|SOCK_OUTSTREAM)) )
    return freeSocket(socket);

  return 0;
}


static IOFUNCTIONS readFunctions =
{ tcp_read,
  tcp_write,
  tcp_seek_null,
  tcp_close_input,
};


static IOFUNCTIONS writeFunctions =
{ tcp_read,
  tcp_write,
  tcp_seek_null,
  tcp_close_output,
};


foreign_t
tcp_open_socket(term_t Socket, term_t Read, term_t Write)
{ IOSTREAM *in, *out;
  int socket;
  plsocket *pls;
  void *handle;

  if ( !tcp_get_socket(Socket, &socket) )
    return FALSE;
  handle = (void *)(long)socket;
  
  pls = lookupSocket(socket);
  in  = Snew(handle, SIO_FILE|SIO_INPUT|SIO_RECORDPOS,  &readFunctions);
  if ( !PL_open_stream(Read, in) )
    return FALSE;
  pls->flags |= SOCK_INSTREAM;
  pls->input = in;

  if ( !(pls->flags & SOCK_LISTEN) )
  { out = Snew(handle, SIO_FILE|SIO_OUTPUT|SIO_RECORDPOS, &writeFunctions);
    if ( !PL_open_stream(Write, out) )
      return FALSE;
    pls->flags |= SOCK_OUTSTREAM;
    pls->output = out;
  }

  return TRUE;
}


		 /*******************************
		 *	   BLOCKING IO		*
		 *******************************/

static foreign_t
tcp_fcntl(term_t Socket, term_t Cmd, term_t Arg)
{ int socket;
  char *cmd;

  if ( !tcp_get_socket(Socket, &socket) )
    return FALSE;
  if ( !PL_get_atom_chars(Cmd, &cmd) )
    return pl_error(NULL, 0, NULL, ERR_ARGTYPE, 2, Cmd, "atom");

  if ( strcmp(cmd, "setfl") == 0 )
  { char *arg;

    if ( !PL_get_atom_chars(Arg, &arg) )
      return pl_error(NULL, 0, NULL, ERR_ARGTYPE, 3, Arg, "flag");
    if ( strcmp(arg, "nonblock") == 0 )
    { fcntl(socket, F_SETFL, O_NONBLOCK);
      lookupSocket(socket)->flags |= SOCK_NONBLOCK;
      return TRUE;
    }

    return pl_error(NULL, 0, NULL, ERR_ARGTYPE, 3, Arg, "flag");
  }

  return pl_error(NULL, 0, NULL, ERR_ARGTYPE, 3, Arg, "command");
}


static foreign_t
pl_gethostname(term_t name)
{ char buf[256];

  if ( !tcp_init() )
    return FALSE;

  if ( gethostname(buf, sizeof(buf)) == 0 )
  { struct hostent *he;

    if ( (he = gethostbyname(buf)) )
      return PL_unify_atom_chars(name, he->h_name);
    else
      return PL_unify_atom_chars(name, buf);
  }

  return tcp_error(h_errno, h_errno_codes);
}


#ifdef O_DEBUG
static foreign_t
tcp_debug(term_t val)
{ return PL_get_integer(val, &debugging);
}
#endif

install_t
install_socket()
{ ATOM_reuseaddr  = PL_new_atom("reuseaddr");
  ATOM_dispatch   = PL_new_atom("dispatch");

  FUNCTOR_socket1 = PL_new_functor(PL_new_atom("$socket"), 1);
  FUNCTOR_module2 = PL_new_functor(PL_new_atom(":"), 2);
  FUNCTOR_ip4     = PL_new_functor(PL_new_atom("ip"), 4);
  
#ifdef O_DEBUG
  PL_register_foreign("user:tcp_debug",	      1, tcp_debug,	      0);
#endif
  PL_register_foreign("tcp_accept",           3, tcp_accept,          0);
  PL_register_foreign("tcp_bind",             2, tcp_bind,            0);
  PL_register_foreign("tcp_connect",          2, tcp_connect,         0);
  PL_register_foreign("tcp_listen",           2, tcp_listen,          0);
  PL_register_foreign("tcp_open_socket",      3, tcp_open_socket,     0);
  PL_register_foreign("tcp_socket",           1, tcp_socket,          0);
  PL_register_foreign("tcp_close_socket",     1, tcp_close_socket,    0);
  PL_register_foreign("tcp_fcntl",            3, tcp_fcntl,           0);
  PL_register_foreign("tcp_setopt",           2, tcp_setopt,          0);
  PL_register_foreign("tcp_host_to_address",  2, tcp_host_to_address, 0);
  PL_register_foreign("gettcp_host_to_address",  2, tcp_host_to_address, 0);
  PL_register_foreign("gethostname",          1, pl_gethostname,      0);
}


install_t
uninstall_socket()
{ if ( initialised )
  {
#ifdef WIN32
    WSACleanup();
#endif
  }
}
