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
This module is extracted from socket.c to   provide  a common ground for
accessing sockets and  possibly  other   devices  in  non-blocking mode,
allowing  for  GUI  (XPCE)  event   dispatching,  timeout  handling  and
multi-threaded signal and timeout handling.

Besides dealing with nonblocking aspects,  an   important  facet of this
library is to hide OS differences.


API
---

The API is completely the same as for   blocking IO. It is however built
on top of sockets used in non-blocking   mode which enables the layer to
listen to Prolog events such  as   timeouts,  GUI  processing and thread
interaction. The functions are modelled  after   the  POSIX  socket API,
prefixed with nbio_*:

	nbio_socket()
	nbio_connect()
	nbio_bind()
	nbio_listen()
	nbio_accept()
	nbio_closesocket()

and IO is realised using

	nbio_read()		See also below
	nbio_write()

Overall control of the library:

	nbio_init()
	nbio_cleanup()
	nbio_debug()

Error handling

	nbio_error()		Raises a Prolog exception

Settings

	nbio_setopt()
	nbio_get_flags()

Address Converstion

	nbio_get_sockaddr()
	nbio_get_ip4()

Waiting

	nbio_select()

Alternative to nbio_read() and nbio_write(), the application program may
call  the  low-level  I/O  routines  in    non-blocking  mode  and  call
nbio_wait(int socket, nbio_request request). This  function returns 0 if
it thinks the call might  now  succeed   and  -1  if  an error occurred,
leaving the exception context in Prolog. On  receiving -1, the user must
return an I/O error as soon as possible.


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

#include "nonblockio.h"

#include <SWI-Stream.h>
#include "clib.h"

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <assert.h>
#include <string.h>
#ifdef WIN32
#include <malloc.h>
#endif

#ifndef WIN32
#define closesocket(n) close((n))	/* same on Unix */
#endif

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

#define SOCK_MAGIC 0x38da3f2c

typedef struct _plsocket
{ int		    magic;		/* SOCK_MAGIC */
  struct _plsocket *next;		/* next in list */
  int		    socket;		/* The OS socket */
  int		    flags;		/* Misc flags */
  IOSTREAM *	    input;		/* input stream */
  IOSTREAM *	    output;		/* output stream */
#ifdef WIN32
  nbio_request	    request;		/* our request */
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

static int
need_retry(int error)
{ if ( error == EINTR || error == EAGAIN || error == EWOULDBLOCK )
    return TRUE;

  return FALSE;
}

#ifdef O_DEBUG
static int debugging;

int
nbio_debug(int level)
{ int old = debugging;

  if ( level >= 0 )			/* -1 --> return current setting */
    debugging = level;

  return old;
}

#define DEBUG(l, g) if ( debugging >= l ) g
#else
#define DEBUG(l, g) (void)0

int
nbio_debug(int level)
{ return 0;
}

#endif

		 /*******************************
		 *	  COMPATIBILITY		*
		 *******************************/

#ifdef WIN32
#define WM_SOCKET	(WM_USER+20)
#define WM_REQUEST	(WM_USER+21)
#define WM_READY	(WM_USER+22)
#define WM_DONE		(WM_USER+23)

#define F_SETFL		0
#define O_NONBLOCK	0

static int
nbio_fcntl(int fd, int op, int arg)
{ switch(op)
  { case F_SETFL:
      switch(arg)
      { case O_NONBLOCK:
	{ int rval;
	  int non_block;

	  non_block = 1;
	  rval = ioctlsocket(fd, FIONBIO, &non_block);
	  if ( rval )
	  { lookupSocket(fd)->flags |= SOCK_NONBLOCK;
	    return 0;
	  } else
	    return -1;
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

static local_state nbio_state;
#define State() (&nbio_state)

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


int
nbio_wait(int socket, nbio_request request)
{ plsocket *s = lookupSocket(socket);

  s->flags  |= SOCK_WAITING;
  s->done    = FALSE;
  s->error   = 0;
  s->thread  = GetCurrentThreadId();
  s->request = request;

  PostMessage(State()->hwnd, WM_REQUEST, 1, (LPARAM)&s);

  DEBUG(2, Sdprintf("[%d] (%ld): Waiting ...",
		    PL_thread_self(), s->thread));

  for(;;)
  { MSG msg;
  
    if ( PL_handle_signals() < 0 )
    { DEBUG(1, Sdprintf("[%d]: Exception\n", PL_thread_self()));
      return -1;
    }
    if ( s->done )
    { DEBUG(2, Sdprintf("[%d]: Done\n", PL_thread_self()));
      return 0;
    }

    if ( false(s, SOCK_DISPATCH) )
    { if ( !GetMessage(&msg, NULL, WM_DONE, WM_DONE) )
	return FALSE;
    } else if ( GetMessage(&msg, NULL, 0, 0) )
    { TranslateMessage(&msg);
      DispatchMessage(&msg);
    } else
    { ExitProcess(0);			/* WM_QUIT received */
      return -1;			/* NOTREACHED */
    }
  }
}


int
nbio_select(int n,
	    fd_set *readfds, fd_set *writefds, fd_set *exceptfds,
	    struct timeval *timeout)
{ plsocket **sockets = alloca(n * sizeof(plsocket*));
  int i;
  DWORD t_end;

  if ( !sockets )
  { errno = ENOMEM;
    return -1;
  }
  for(i=0; i<n; i++)
    sockets[i] = NULL;

  if ( readfds )
  { for(i=0; i<n; i++)
    { if ( FD_ISSET(i, readfds) )
      { plsocket *s = lookupSocket(i);

	s->flags  |= SOCK_WAITING;
	s->done    = FALSE;
	s->error   = 0;
	s->thread  = GetCurrentThreadId();
	s->request = (s->flags & SOCK_LISTEN) ? REQ_ACCEPT : REQ_READ;
	sockets[i] = s;
      }
    }
  }
  if ( writefds )
    return -1;				/* not yet implemented */
  if ( exceptfds )
    return -1;				/* idem (might never be) */

  if ( timeout )
  { t_end = GetTickCount();
    t_end += timeout->tv_sec*1000;
    t_end += timeout->tv_usec/1000;
  }

  FD_ZERO(readfds);
  PostMessage(State()->hwnd, WM_REQUEST, n, (LPARAM)sockets);

  for(;;)
  { MSG msg;
    plsocket **s;
    int ready;

    if ( PL_handle_signals() < 0 )
    { DEBUG(1, Sdprintf("[%d]: Exception\n", PL_thread_self()));
      return -1;
    }
    
    for(ready=0, i=0, s=sockets; i<n; i++, s++)
    { if ( *s && (*s)->done )
      { ready++;
	FD_SET((unsigned)i, readfds);
      }
    }
    if ( ready > 0 )
      return ready;

    if ( timeout )
    { DWORD rc;
      DWORD t = GetTickCount();
      long msec = t_end - t;

      if ( msec < 0 )
	msec = -msec;			/* wrapped around */

      rc = MsgWaitForMultipleObjects(0, NULL, FALSE, msec, QS_ALLINPUT);
      if ( rc == WAIT_OBJECT_0 )
      { if ( GetMessage(&msg, NULL, 0, 0) )
	{ TranslateMessage(&msg);
	  DispatchMessage(&msg);
	} else
	{ ExitProcess(0);		/* WM_QUIT received */
	  return -1;			/* NOTREACHED */
	}
      } else if ( rc == WAIT_TIMEOUT )
      { return 0;
      } else
      { assert(0);
      }
    } else
    { if ( GetMessage(&msg, NULL, 0, 0) )
      { TranslateMessage(&msg);
	DispatchMessage(&msg);
      } else
      { ExitProcess(0);			/* WM_QUIT received */
	return -1;			/* NOTREACHED */
      }
    }
  }
}


static int
placeRequest(plsocket *s, nbio_request request)
{ s->error   = 0;
  s->done    = FALSE;
  s->thread  = GetCurrentThreadId();
  s->request = request;
  clear(s, SOCK_WAITING);

  PostMessage(State()->hwnd, WM_REQUEST, 1, (LPARAM)&s);
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

	if ( true(s, SOCK_WAITING) )
	{ doneRequest(s);
	  break;
	}

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

	if ( true(s, SOCK_WAITING) )
	{ doneRequest(s);
	  break;
	}

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

	if ( true(s, SOCK_WAITING) )
	{ doneRequest(s);
	  break;
	}

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

	if ( true(s, SOCK_WAITING) )
	{ doneRequest(s);
	  break;
	}

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
    { plsocket **s = (plsocket **)lParam;
      int i, n = (int)wParam;

      for(i=0; i<n; i++)
      { if ( s[i] )
	  doRequest(s[i]);
      }

      return 0;
    }

    case WM_SOCKET:
    { SOCKET sock = (SOCKET) wParam;
      int err     = WSAGETSELECTERROR(lParam);
      int evt     = WSAGETSELECTEVENT(lParam);
      plsocket *s = lookupExistingSocket(sock);

      if ( s )
      { s->w32_flags |= evt;
	if ( err )
	{ s->error = err;
	  
	  switch(s->request)
	  { case REQ_CONNECT:
	      break;
	    case REQ_ACCEPT:
	      s->rdata.accept.slave = SOCKET_ERROR;
	      break;
	    case REQ_READ:
	      s->rdata.read.bytes = -1;
	      break;
	    case REQ_WRITE:
	      s->rdata.write.bytes = -1;
	      break;
	  }
	  doneRequest(s);
	} else
	{ doRequest(s);
	}
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
desired, you can use tcp_setopt(Socket,  dispatch(false)), in which case
this call returns immediately, assuming the   actual TCP call will block
without dispatching if no input is available.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
wait_socket(plsocket *s, int fd)
{ if ( true(s, SOCK_DISPATCH) )
  { if ( true(s, SOCK_NONBLOCK) && !PL_dispatch(fd, PL_DISPATCH_INSTALLED) )
    { fd_set rfds;
      struct timeval tv;

      FD_ZERO(&rfds);
      FD_SET(fd, &rfds);
      tv.tv_sec = 0;
      tv.tv_usec = 250000;
      
      select(fd+1, &rfds, NULL, NULL, &tv) != 0;
      return TRUE;
    } else
    { return PL_dispatch(fd, PL_DISPATCH_WAIT);
    }
  }

  return TRUE;
}


int
nbio_wait(int socket, nbio_request request)
{ plsocket *s = lookupSocket(socket);

  return wait_socket(s, socket) ? 0 : -1;
}


static int
nbio_fcntl(int fd, int op, int arg)
{ int rc = fcntl(fd, op, arg);

  if ( rc == 0 )
  { if ( op == F_SETFL && arg == O_NONBLOCK )
      lookupSocket(fd)->flags |= SOCK_NONBLOCK;
  } else
    nbio_error(errno, TCP_ERRNO);

  return rc;
}

int
nbio_select(int n,
	    fd_set *readfds, fd_set *writefds, fd_set *exceptfds,
	    struct timeval *timeout)
{ return select(n, readfds, writefds, exceptfds, timeout); 
}

#endif /*WIN32*/


		 /*******************************
		 *	 ADMINISTRATION		*
		 *******************************/

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

int
nbio_error(int code, nbio_error_map mapid)
{ const char *msg;
  term_t except = PL_new_term_ref();
  error_codes *map;

  switch( mapid )
  { case TCP_HERRNO:
      map = h_errno_codes;
      break;
    default:
      map = NULL;
  }

  {
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
  }

  return PL_raise_exception(except);
}

		 /*******************************
		 *	  INITIALISATION	*
		 *******************************/

int
nbio_init(void)
{ LOCK();
  if ( initialised )
  { UNLOCK();
    return TRUE;
  }
  initialised = TRUE;

  FUNCTOR_module2 = PL_new_functor(PL_new_atom(":"), 2);
  FUNCTOR_ip4     = PL_new_functor(PL_new_atom("ip"), 4);

#ifdef WIN32
{ WSADATA WSAData;

  if ( WSAStartup(MAKEWORD(2,0), &WSAData) )
  { UNLOCK();
    return PL_warning("nbio_init() - WSAStartup failed.");
  }
  startSocketThread();
}
#endif /*WIN32*/

  UNLOCK();
  return TRUE;
}
	

int
nbio_cleanup(void)
{ if ( initialised )
  {
#ifdef WIN32
    WSACleanup();
#endif
  }

  return 0;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
socket(-Socket)
    Create a stream inet socket.  The socket is represented by a term of
    the format $socket(Id).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
nbio_socket(int domain, int type, int protocol)
{ int sock;
  plsocket *s;
	
  if ( !nbio_init() )
    return FALSE;

  if ( (sock = socket(domain, type , protocol)) < 0)
  { nbio_error(errno, TCP_ERRNO);
    return -1;
  }

  if ( !(s=lookupSocket(sock)) )		/* register it */
  { closesocket(sock);
    return -1;
  }

#ifdef WIN32
  WSAAsyncSelect(sock, State()->hwnd, WM_SOCKET,
		 FD_READ|FD_WRITE|FD_ACCEPT|FD_CONNECT|FD_CLOSE);
#endif

  return sock;
}


int
nbio_closesocket(int socket)
{ plsocket *s;

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

  return 0;
}


int
nbio_setopt(int socket, nbio_option opt, ...)
{ va_list args;
  int rc;

  va_start(args, opt);

  switch(opt)
  { case TCP_NONBLOCK:
      rc = nbio_fcntl(socket, F_SETFL, O_NONBLOCK);
      break;
    case TCP_REUSEADDR:
    { int val = va_arg(args, int);

      if( setsockopt(socket, SOL_SOCKET, SO_REUSEADDR,
		     (const char *)&val, sizeof(val)) == -1)
      { nbio_error(h_errno, TCP_HERRNO);
	rc = -1;
      } else
	rc = 0;

      break;
    }
    case TCP_DISPATCH:
    { int val = va_arg(args, int);
      plsocket *s = lookupSocket(socket);
      
      if ( val )
	set(s, SOCK_DISPATCH);
      else
	clear(s, SOCK_DISPATCH);

      rc = 0;

      break;
    }
    case TCP_INSTREAM:
    { IOSTREAM *in = va_arg(args, IOSTREAM*);
      plsocket *s = lookupSocket(socket);

      s->flags |= SOCK_INSTREAM;
      s->input = in;

      rc = 0;

      break;
    }
    case TCP_OUTSTREAM:
    { IOSTREAM *out = va_arg(args, IOSTREAM*);
      plsocket *s = lookupSocket(socket);

      s->flags |= SOCK_OUTSTREAM;
      s->output = out;

      rc = 0;

      break;
    }
    default:
      rc = -1;
      assert(0);
  }

  va_end(args);

  return rc;
}


int
nbio_get_flags(int socket)
{ plsocket *s = lookupSocket(socket);
  
  if ( s )
    return s->flags;

  return -1;
} 


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Translate a host + port-number into a sockaddr structure.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
nbio_get_port(term_t Port, int *port)
{ char *name;

  if ( PL_get_atom_chars(Port, &name) )
  { struct servent *service;
    
    if ( !(service = getservbyname(name, "tcp")) )
      return nbio_error(errno, TCP_ERRNO);

    *port = ntohs(service->s_port);
    return TRUE;
  }

  if ( PL_get_integer(Port, port) )
    return TRUE;

  return pl_error(NULL, 0, NULL, ERR_ARGTYPE, -1, Port, "port");
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Convert a term Host:Port to a socket address.  Port is either an integer
or the name of a registered port (e.g. 'smtp').
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
nbio_get_sockaddr(term_t Address, struct sockaddr_in *addr)
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
    if ( !nbio_get_port(arg, &port) )
      return FALSE;
  } else if ( PL_is_variable(Address) )
  { port = 0;
  } else if ( !nbio_get_port(Address, &port) )
    return FALSE;

  if ( hostName )
  { if( !(host = gethostbyname(hostName)) )
      return nbio_error(h_errno, TCP_HERRNO);
    if ( (int)sizeof(addr->sin_addr) < host->h_length )
      return PL_warning("Oops, host address too long!");
    memcpy(&addr->sin_addr, host->h_addr, host->h_length);
  } else
    addr->sin_addr.s_addr = INADDR_ANY;
	
  addr->sin_port = htons((short)port);

  return TRUE;
}


int
nbio_get_ip(term_t ip4, struct in_addr *ip)
{ unsigned long hip = 0;

  if ( PL_is_functor(ip4, FUNCTOR_ip4) )
  { int i, ia;
    term_t a = PL_new_term_ref();

    for(i=1; i<=4; i++)
    { PL_get_arg(i, ip4, a);
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


int
nbio_unify_ip4(term_t Ip, unsigned long hip)
{ return PL_unify_term(Ip,
		       PL_FUNCTOR, FUNCTOR_ip4,
		         IntArg((hip >> 24) & 0xff),
		         IntArg((hip >> 16) & 0xff),
		         IntArg((hip >>  8) & 0xff),
		         IntArg((hip >>  0) & 0xff));
}


int
nbio_bind(int socket, struct sockaddr *my_addr, size_t addrlen)
{ if ( bind(socket, my_addr, addrlen) )
  { nbio_error(errno, TCP_ERRNO);
    return -1;
  }

  lookupSocket(socket)->flags |= SOCK_BIND;

  return 0;
}


int
nbio_connect(int socket,
	    const struct sockaddr *serv_addr,
	    size_t addrlen)
{ plsocket *s;
       
  s = lookupSocket(socket);
  
#ifdef WIN32
  if ( connect(socket, serv_addr, addrlen) )
  { s->error = WSAGetLastError();

    if ( s->error == WSAEWOULDBLOCK )
    { s->rdata.connect.addrlen = addrlen;
      memcpy(&s->rdata.connect.addr, serv_addr, addrlen);
      placeRequest(s, REQ_CONNECT);
      waitRequest(s);
    }

    if ( s->error )
    { nbio_error(s->error, TCP_ERRNO);
      return -1;
    }
  }
#else /*!WIN32*/
  for(;;)
  { if ( connect(socket, serv_addr, addrlen) )
    { if ( need_retry(errno) )
      { if ( PL_handle_signals() < 0 )
	  return -1;
	continue;
      }
      nbio_error(errno, TCP_ERRNO);
      return -1;
    } else
      break;
  }
#endif

  s->flags |= SOCK_CONNECT;

  return 0;
}


int
nbio_accept(int master, struct sockaddr *addr, size_t *addrlen)
{ int slave;
  plsocket *m;
	
  m = lookupSocket(master);

#ifdef WIN32
  slave = accept(master, addr, addrlen);

  if ( slave == SOCKET_ERROR )
  { m->error = WSAGetLastError();

    if ( m->error == WSAEWOULDBLOCK )
    { m->rdata.accept.addrlen = sizeof(m->rdata.accept.addr);
      placeRequest(m, REQ_ACCEPT);
      if ( !waitRequest(m) )
	return FALSE;
      if ( m->error )
	return nbio_error(m->error, TCP_ERRNO);
      slave    = m->rdata.accept.slave;
      *addrlen = m->rdata.accept.addrlen;
      memcpy(addr, &m->rdata.accept.addr, m->rdata.accept.addrlen);
    } else
    { nbio_error(m->error, TCP_ERRNO);
      return -1;
    }
  }

#else /*WIN32*/

  for(;;)
  { if ( !wait_socket(m, master) )
      return -1;

    slave = accept(master, addr, addrlen);

    if ( slave == SOCKET_ERROR )
    { if ( need_retry(errno) )
      { if ( PL_handle_signals() < 0 )
	  return -1;

	continue;
      } else
      { nbio_error(errno, TCP_ERRNO);
	return -1;
      }
    } else
      break;
  }

  { plsocket *s = lookupSocket(slave);
    s->flags |= SOCK_ACCEPT;
    if ( true(s, SOCK_NONBLOCK) )
      nbio_setopt(slave, TCP_NONBLOCK);
  }

#endif /*WIN32*/
  
  return slave;
}


int
nbio_listen(int socket, int backlog)
{ if( listen(socket, backlog) == -1 )
  { nbio_error(errno, TCP_ERRNO);
    return -1;
  }

  lookupSocket(socket)->flags |= SOCK_LISTEN;

  return 0;
}


		 /*******************************
		 *	  IO-STREAM STUFF	*
		 *******************************/

#define fdFromHandle(p) ((int)((long)(p)))

int
nbio_read(int socket, char *buf, int bufSize)
{ plsocket *s = lookupSocket(socket);
  int n;

  if ( !s || s->magic != SOCK_MAGIC )
  { errno = EINVAL;
    DEBUG(1, Sdprintf("nbio_read(): Invalid socket: %d\n", socket));
    return -1;
  }

#ifdef WIN32

  n = recv(socket, buf, bufSize, 0);
  if ( n < 0 )
  { if ( WSAGetLastError() == WSAEWOULDBLOCK )
    { s->rdata.read.buffer = buf;
      s->rdata.read.size   = bufSize;
      placeRequest(s, REQ_READ);
      if ( !waitRequest(s) )
      { errno = EPLEXCEPTION;
	return -1;
      }
      n = s->rdata.read.bytes;
    }

    if ( n < 0 )
      errno = EIO;			/* TBD: map s->error to POSIX errno */
  }

#else /*WIN32*/

  for(;;)
  { if ( !wait_socket(s, socket) )
    { errno = EPLEXCEPTION;
      return -1;
    }

    n = recv(socket, buf, bufSize, 0);

    if ( n == -1 && need_retry(errno) )
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

int
nbio_write(int socket, char *buf, int bufSize)
{ int len = bufSize;
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
    { if ( need_retry(errno) )
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


int
nbio_close_input(int socket)
{ int rc = 0;
  plsocket *s = lookupSocket(socket);

  DEBUG(2, Sdprintf("[%d]: nbio_close_input(%d)\n", PL_thread_self(), socket));
  s->input = NULL;
  s->flags &= ~SOCK_INSTREAM;
#ifdef WIN32
  if ( false(s, SOCK_LISTEN) )
  { if ( shutdown(socket, SD_RECEIVE) == SOCKET_ERROR )
      Sdprintf("shutdown(%d, SD_RECEIVE) failed: %s\n",
	       socket,
	       WinSockError(WSAGetLastError()));
    rc = -1;
  }
#endif

  if ( !(s->flags & (SOCK_INSTREAM|SOCK_OUTSTREAM)) )
    return freeSocket(socket);

  return rc;
}


int
nbio_close_output(int socket)
{ int rc = 0;
  plsocket *s = lookupSocket(socket);

  DEBUG(2, Sdprintf("[%d]: nbio_close_output(%d)\n", PL_thread_self(), socket));
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
      rc = -1;
    }
#endif
  }

  DEBUG(3, Sdprintf("%d->flags = 0x%x\n", socket, s->flags));
  if ( !(s->flags & (SOCK_INSTREAM|SOCK_OUTSTREAM)) )
    return freeSocket(socket);

  return rc;
}

