/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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
#ifdef __WINDOWS__
#include <malloc.h>
#endif

#ifndef __WINDOWS__
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
#if __WINDOWS__
static CRITICAL_SECTION mutex;
static CRITICAL_SECTION mutex_free;

#define LOCK() EnterCriticalSection(&mutex)
#define UNLOCK() LeaveCriticalSection(&mutex)
#define LOCK_FREE() EnterCriticalSection(&mutex_free)
#define UNLOCK_FREE() LeaveCriticalSection(&mutex_free)
#define INITLOCK() (InitializeCriticalSection(&mutex), \
		    InitializeCriticalSection(&mutex_free))
#else
#include <pthread.h>

static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

#define LOCK() pthread_mutex_lock(&mutex)
#define UNLOCK() pthread_mutex_unlock(&mutex)
#define LOCK_FREE()
#define UNLOCK_FREE()
#define INITLOCK()
#endif
#else
#define LOCK()
#define UNLOCK()
#define LOCK_FREE()
#define UNLOCK_FREE()
#define INITLOCK()
#endif

#define SOCK_INSTREAM	0x01
#define SOCK_OUTSTREAM	0x02
#define SOCK_BIND	0x04		/* What have we done? */
#define SOCK_LISTEN	0x08
#define SOCK_CONNECT	0x10		/* Connected (client) socket */
#define SOCK_ACCEPT	0x20		/* Accepted (server) socket */
#define SOCK_NONBLOCK	0x40		/* Set to non-blocking mode */
#define SOCK_DISPATCH   0x80		/* do not dispatch events */

#define set(s, f)   ((s)->flags |= (f))
#define clear(s, f) ((s)->flags &= ~(f))
#define true(s, f)  ((s)->flags & (f))
#define false(s, f) (!true(s, f))

#define SOCK_MAGIC 0x38da3f2c

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NOTE: We must lock  the  structure   to  avoid  freeSocket() called from
Prolog deleting the socket while there are   still  pending events on it
that are concurrently executed in the socket thread.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct _plsocket
{ int		    magic;		/* SOCK_MAGIC */
  nbio_sock_t	    id;			/* Integer id */
  SOCKET	    socket;		/* The OS socket */
  int		    flags;		/* Misc flags */
  IOSTREAM *	    input;		/* input stream */
  IOSTREAM *	    output;		/* output stream */
#ifdef __WINDOWS__
  nbio_request	    request;		/* our request */
  DWORD		    thread;		/* waiting thread */
  DWORD		    error;		/* error while executing request */
  int		    done;		/* request completed */
  int		    w32_flags;		/* or of received FD_* */
  union
  { struct
    { struct sockaddr_in addr;		/* accepted address */
      int addrlen;			/* address length */
      nbio_sock_t slave;		/* descriptor of slave */
    } accept;
    struct
    { struct sockaddr_in addr;		/* accepted address */
      size_t addrlen;			/* address length */
    } connect;
    struct
    { int bytes;			/* byte count */
      char *buffer;			/* the buffer */
      size_t size;			/* buffer size */
    } read;
    struct
    { int bytes;			/* byte count */
      char *buffer;			/* the buffer */
      size_t written;
      size_t size;			/* buffer size */
    } write;
    struct
    { int bytes;			/* byte count */
      void *buffer;			/* the buffer */
      size_t size;			/* buffer size */
      int flags;
      struct sockaddr *from;
      socklen_t *fromlen;
    } recvfrom;
    struct
    { int bytes;			/* byte count */
      void *buffer;			/* the buffer */
      size_t size;			/* buffer size */
      int flags;
      const struct sockaddr *to;
      socklen_t tolen;
    } sendto;
  } rdata;
#endif
} plsocket;

static plsocket *allocSocket(SOCKET socket);
static plsocket *nbio_to_plsocket(nbio_sock_t socket);
#ifdef __WINDOWS__
static plsocket *lookupOSSocket(SOCKET socket);
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

void					/* allow debugger breakpoint */
tcp_debug()
{ Sdprintf("Trapping debugger\n");
}


		 /*******************************
		 *	  COMPATIBILITY		*
		 *******************************/

#ifdef __WINDOWS__
static UINT WM_SOCKET	= WM_APP+20;
static UINT WM_REQUEST  = WM_APP+21;
static UINT WM_READY	= WM_APP+22;
static UINT WM_DONE	= WM_APP+23;

#if O_DEBUG
static char *
request_name(nbio_request request)
{ switch(request)
  { case REQ_NONE:    return "req_none";
    case REQ_ACCEPT:  return "req_accept";
    case REQ_CONNECT: return "req_connect";
    case REQ_READ:    return "req_read";
    case REQ_WRITE:   return "req_write";
    case REQ_RECVFROM:return "req_recvfrom";
    case REQ_SENDTO:  return "req_sendto";
    default:	      return "req_???";
  }
}

static char *
event_name(int ev)
{ char buf[256];
  char *o = buf;

  o[0] = '\0';
  if ( (ev & FD_READ) )    strcat(o, "|FD_READ");
  if ( (ev & FD_WRITE) )   strcat(o, "|FD_WRITE");
  if ( (ev & FD_ACCEPT) )  strcat(o, "|FD_ACCEPT");
  if ( (ev & FD_CONNECT) ) strcat(o, "|FD_CONNECT");
  if ( (ev & FD_CLOSE) )   strcat(o, "|FD_CLOSE");
  if ( (ev & FD_OOB) )	   strcat(o, "|FD_OOB");
  if ( (ev & ~(FD_READ|FD_WRITE|FD_ACCEPT|FD_CONNECT|FD_CLOSE)) )
    strcat(o, "|FD_???");

  return strdup(buf);
}

#endif

#define F_SETFL		0
#define O_NONBLOCK	0

static int
nbio_fcntl(nbio_sock_t socket, int op, int arg)
{ plsocket *s;

  if ( !(s=nbio_to_plsocket(socket)) )
    return -1;

  switch(op)
  { case F_SETFL:
      switch(arg)
      { case O_NONBLOCK:
	{ int rval;
	  int non_block;

	  non_block = 1;
	  rval = ioctlsocket(s->socket, FIONBIO, &non_block);
	  if ( rval )
	  { s->flags |= SOCK_NONBLOCK;
	    return 0;
	  }

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
{ SOCKET sock;

  s->done = TRUE;
  s->request = REQ_NONE;

  if ( (s->w32_flags & FD_CLOSE) && (sock=s->socket) >= 0 )
  { s->socket = -1;
    closesocket(sock);
  }

  if ( s->thread )
  { DEBUG(2, Sdprintf("doneRequest(): posting %d\n", s->thread));
    PostThreadMessage(s->thread, WM_DONE, 0, (WPARAM)s);
  }

  return TRUE;
}


static int
waitRequest(plsocket *s)
{ assert(s->magic == SOCK_MAGIC);

  DEBUG(2, Sdprintf("[%d] (%ld): Waiting for %s on %d ...",
		    PL_thread_self(), s->thread,
		    request_name(s->request), (int)s->socket));

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
    { ExitThread(0);			/* WM_QUIT received */
      return FALSE;			/* NOTREACHED */
    }
  }
}


int
nbio_wait(nbio_sock_t socket, nbio_request request)
{ plsocket *s;

  if ( !(s=nbio_to_plsocket(socket)) )
    return -1;

  s->flags  |= SOCK_WAITING;
  s->done    = FALSE;
  s->error   = 0;
  s->thread  = GetCurrentThreadId();
  s->request = request;

  SendMessage(State()->hwnd, WM_REQUEST, 1, (LPARAM)&s);

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
    { ExitThread(0);			/* WM_QUIT received */
      return -1;			/* NOTREACHED */
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
nbio_select() selects using a set of socket streams. 

NOTE: The Windows versions uses our   nbio_sock_t abstraction, while the
other version uses  the  raw  Unix   file  descriptors  referencing  the
underlying socket.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

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
      { plsocket *s = nbio_to_plsocket(i);

	if ( s )
	{ s->flags  |= SOCK_WAITING;
	  s->done    = FALSE;
	  s->error   = 0;
	  s->thread  = GetCurrentThreadId();
	  s->request = (s->flags & SOCK_LISTEN) ? REQ_ACCEPT : REQ_READ;
	  sockets[i] = s;
	} else
	{ DEBUG(2, Sdprintf("nbio_select(): no socket for %d\n", i));
	}
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
  SendMessage(State()->hwnd, WM_REQUEST, n, (LPARAM)sockets);

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
	{ ExitThread(0);		/* WM_QUIT received */
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
      { ExitThread(0);			/* WM_QUIT received */
	return -1;			/* NOTREACHED */
      }
    }
  }
}


static int
placeRequest(plsocket *s, nbio_request request)
{ if ( s->magic != SOCK_MAGIC )
    Sdprintf("placeRequest: %p has bad magic\n", s);

  s->error   = 0;
  s->done    = FALSE;
  s->thread  = GetCurrentThreadId();
  s->request = request;
  clear(s, SOCK_WAITING);

  SendMessage(State()->hwnd, WM_REQUEST, 1, (LPARAM)&s);
  DEBUG(2, Sdprintf("%d (%ld): Placed %s request for %d\n",
		    PL_thread_self(), s->thread,
		    request_name(request), (int)s->socket));

  return TRUE; 
}

static int
doRequest(plsocket *s)
{ if ( s->magic != SOCK_MAGIC )
    Sdprintf("doRequest: %p has bad magic\n", s);

  switch(s->request)
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
		     (int)s->rdata.connect.addrlen) )
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
      { SOCKET slave;

	s->w32_flags &= ~FD_ACCEPT;
	if ( true(s, SOCK_WAITING) )
	{ doneRequest(s);
	  break;
	}

	slave = accept(s->socket,
		       (struct sockaddr*)&s->rdata.accept.addr,
		       &s->rdata.accept.addrlen);

	if ( slave == SOCKET_ERROR )
	{ s->error = WSAGetLastError();
	  
	  DEBUG(2, Sdprintf("Accept(%d): %s\n",
			    (int)s->socket, WinSockError(s->error)));

	  if ( s->error != WSAEWOULDBLOCK )
	  { s->rdata.accept.slave = (SOCKET)-1;
	    doneRequest(s);
	  }
	} else
	{ plsocket *pls;

	  DEBUG(2, Sdprintf("Accept(%d) --> %d\n",
			    (int)s->socket, (int)slave));
	  if ( (pls = allocSocket(slave)) )
	  { pls->flags |= SOCK_ACCEPT;	/* requests */

	    s->rdata.accept.slave = pls->id;
	    s->error = 0;
	    doneRequest(s);
	  } else
	  { DEBUG(1, Sdprintf("Socket %d already registered; "
			      "considering bogus\n", (int)slave));
	  }
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
				   (int)s->rdata.read.size,
				   0);
	if ( s->rdata.read.bytes < 0 )
	{ s->error = WSAGetLastError();

	  if ( s->error != WSAEWOULDBLOCK )
	  { DEBUG(1, Sdprintf("Error reading from %d: %s\n",
			      s->socket, WinSockError(s->error)));
	    doneRequest(s);
	  }
	} else
	{ doneRequest(s);
	}
      }
      break;
    case REQ_RECVFROM:
      if ( s->w32_flags & (FD_READ|FD_CLOSE) )
      { s->w32_flags &= ~FD_READ;

	if ( true(s, SOCK_WAITING) )
	{ doneRequest(s);
	  break;
	}

	s->rdata.recvfrom.bytes =
		recvfrom(s->socket,
			 s->rdata.recvfrom.buffer,
			 (int)s->rdata.recvfrom.size,
			 s->rdata.recvfrom.flags,
			 s->rdata.recvfrom.from,
			 s->rdata.recvfrom.fromlen);

	if ( s->rdata.recvfrom.bytes < 0 )
	{ s->error = WSAGetLastError();

	  if ( s->error != WSAEWOULDBLOCK )
	  { DEBUG(1, Sdprintf("Error recvfrom from %d: %s\n",
			      s->socket, WinSockError(s->error)));
	    doneRequest(s);
	  }
	} else
	{ doneRequest(s);
	}
      }
      break;
    case REQ_WRITE:
      if ( s->w32_flags & FD_WRITE )
      { int n;
	int len = (int)(s->rdata.write.size - s->rdata.write.written);

	s->w32_flags &= ~FD_WRITE;

	if ( true(s, SOCK_WAITING) )
	{ doneRequest(s);
	  break;
	}

	DEBUG(2, Sdprintf("send() %d bytes\n", s->rdata.write.size));
	n = send(s->socket,
		 s->rdata.write.buffer + s->rdata.write.written,
		 len, 0);
	DEBUG(2, Sdprintf("Wrote %d bytes\n", n));
	if ( n < 0 )
	{ s->error = WSAGetLastError();
	  if ( s->error == WSAEWOULDBLOCK )
	    break;
	  s->rdata.write.bytes = n;
	  DEBUG(1, Sdprintf("[%d]: send(%d, %d bytes): %s\n",
			    PL_thread_self(), s->socket, len,
			    WinSockError(s->error)));
	  doneRequest(s);
	} else
	  s->error = 0;
	  
	s->rdata.write.written += n;
	if ( s->rdata.write.written >= s->rdata.write.size )
	{ s->rdata.write.bytes = (int)s->rdata.write.written;
	  doneRequest(s);
	}
      }
    case REQ_SENDTO:
      if ( s->w32_flags & FD_WRITE )
      { int n;

	s->w32_flags &= ~FD_WRITE;

	if ( true(s, SOCK_WAITING) )
	{ doneRequest(s);
	  break;
	}

	DEBUG(2, Sdprintf("sendto() %d bytes\n", s->rdata.write.size));
	n = sendto(s->socket,
		   s->rdata.sendto.buffer,
		   s->rdata.sendto.size,
		   s->rdata.sendto.flags,
		   s->rdata.sendto.to,
		   s->rdata.sendto.tolen);
	DEBUG(2, Sdprintf("Wrote %d bytes\n", n));
	if ( n < 0 )
	{ s->error = WSAGetLastError();
	  s->rdata.write.bytes = n;
	  DEBUG(1, Sdprintf("[%d]: send(%d, %d bytes): %s\n",
			    PL_thread_self(), s->socket,
			    s->rdata.sendto.size,
			    WinSockError(s->error)));
	  doneRequest(s);
	} else
	  s->error = 0;
	  
	s->rdata.sendto.bytes = n;
	doneRequest(s);
      }
  }

  return TRUE;
}


static LRESULT WINAPI
socket_wnd_proc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{ if ( message == WM_REQUEST )
  { plsocket **s = (plsocket **)lParam;
    int i, n = (int)wParam;

    for(i=0; i<n; i++)
    { if ( s[i] )
      { __try
	{ if ( s[i]->magic != SOCK_MAGIC )
	  { goto nosocket;
	  }
	} __except(EXCEPTION_EXECUTE_HANDLER)
	{ goto nosocket;
	}

	doRequest(s[i]);
      }
    }

    return 0;

  nosocket:
    Sdprintf("%p@%d is not a socket!?\n", s[i], i);
    return 0;
  } else if ( message == WM_SOCKET )
  { SOCKET sock = (SOCKET) wParam;
    int err     = WSAGETSELECTERROR(lParam);
    int evt     = WSAGETSELECTEVENT(lParam);
    plsocket *s;

    if ( evt&FD_CLOSE )
    { DEBUG(1,
	    { char *nm = event_name(evt);
	      Sdprintf("WM_SOCKET on %d: ev=(%s); err=%s\n", 
		       (int)sock, nm, err ? WinSockError(err) : "none");
	      free(nm);
	    });
    } else
    { DEBUG(3,
	    { char *nm = event_name(evt);
	      Sdprintf("WM_SOCKET on %d: ev=(%s); err=%s\n", 
		       (int)sock, nm, err ? WinSockError(err) : "none");
	      free(nm);
	    });
    }

    LOCK_FREE();
    s = lookupOSSocket(sock);

    if ( s )
    { if ( (s->w32_flags & FD_CLOSE) )
      { DEBUG(1,
	      { char *nm = event_name(evt);
		Sdprintf("Got event %s (err=%s) on closed socket %d=%d\n",
			 nm, WinSockError(err), s->id, sock);
		free(nm);
	      })
      }

      s->w32_flags |= evt;
      if ( err )
      { SOCKET sock = s->socket;

	s->error = err;
	if ( sock )
	{ s->socket = -1;
	  closesocket(sock);
	}

	switch(s->request)
	{ case REQ_CONNECT:
	    break;
	  case REQ_ACCEPT:
	    s->rdata.accept.slave = SOCKET_ERROR;
	    break;
	  case REQ_READ:
	    s->rdata.read.bytes = -1;
	    break;
	  case REQ_RECVFROM:
	    s->rdata.recvfrom.bytes = -1;
	    break;
	  case REQ_WRITE:
	    s->rdata.write.bytes = -1;
	    break;
	  case REQ_SENDTO:
	    s->rdata.sendto.bytes = -1;
	    break;
	}
	doneRequest(s);
      } else if ( s->socket >= 0 )
      { doRequest(s);
      } else
      { doneRequest(s);
      }
    } else
    { DEBUG(1, Sdprintf("Socket %d is gone (error=%s)\n",
			sock, WinSockError(err)));
    }

    UNLOCK_FREE();
  }

  return DefWindowProc(hwnd, message, wParam, lParam);
}

static char *
HiddenFrameClass()
{ static char *name;
  static WNDCLASS wndClass;

  if ( !name )
  { char buf[50];

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

#else /*__WINDOWS__*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
wait_socket() is the Unix way  to  wait   for  input  on  the socket. By
default event-dispatching on behalf of XPCE is performed. If this is not
desired, you can use tcp_setopt(Socket,  dispatch(false)), in which case
this call returns immediately, assuming the   actual TCP call will block
without dispatching if no input is available.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
wait_socket(plsocket *s)
{ if ( true(s, SOCK_DISPATCH) )
  { int fd = s->socket;

    if ( true(s, SOCK_NONBLOCK) && !PL_dispatch(fd, PL_DISPATCH_INSTALLED) )
    { fd_set rfds;
      struct timeval tv;

      FD_ZERO(&rfds);
      FD_SET(fd, &rfds);
      tv.tv_sec = 0;
      tv.tv_usec = 250000;
      
      select(fd+1, &rfds, NULL, NULL, &tv);
      return TRUE;
    } else
    { return PL_dispatch(fd, PL_DISPATCH_WAIT);
    }
  }

  return TRUE;
}


int
nbio_wait(nbio_sock_t socket, nbio_request request)
{ plsocket *s;

  if ( !(s=nbio_to_plsocket(socket)) )
    return -1;

  return wait_socket(s) ? 0 : -1;
}


static int
nbio_fcntl(nbio_sock_t socket, int op, int arg)
{ plsocket *s;
  int rc;

  if ( !(s=nbio_to_plsocket(socket)) )
    return -1;

  rc = fcntl(s->socket, op, arg);

  if ( rc == 0 )
  { if ( op == F_SETFL && arg == O_NONBLOCK )
      s->flags |= SOCK_NONBLOCK;
  } else
    nbio_error(errno, TCP_ERRNO);

  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NOTE: when called througb wait_for_input/3, the  descriptors in the sets
are real underlying Unix socket descriptors   and  *not* the nbio_sock_t
psuedo descriptors.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
nbio_select(int n,
	    fd_set *readfds, fd_set *writefds, fd_set *exceptfds,
	    struct timeval *timeout)
{ return select(n, readfds, writefds, exceptfds, timeout); 
}

#endif /*__WINDOWS__*/


		 /*******************************
		 *	 ADMINISTRATION		*
		 *******************************/

static functor_t FUNCTOR_module2;
static functor_t FUNCTOR_ip4;
static functor_t FUNCTOR_ip1;
static atom_t ATOM_any;
static atom_t ATOM_broadcast;
static atom_t ATOM_loopback;

static plsocket **sockets = NULL;	/* id --> plsocket* */
static size_t	socks_count = 0;	/* #registered sockets */
static size_t	socks_allocated = 0;	/* #allocated entries */
static int initialised = FALSE;		/* Windows only */

#ifdef __WINDOWS__

static plsocket *
lookupOSSocket(SOCKET socket)
{ plsocket *p;
  size_t i;

  LOCK();
  for(i=0; i<socks_allocated; i++)
  { if ( (p=sockets[i]) && p->socket == socket )
    { UNLOCK();

      if ( p->magic != SOCK_MAGIC )
      { errno = EINVAL;
	DEBUG(1, Sdprintf("Invalid OS socket: %d\n", socket));
	return NULL;
      }

      return p;
    }
  }
  UNLOCK();

  return NULL;
}

#endif /*__WINDOWS__*/


static plsocket *
nbio_to_plsocket_raw(nbio_sock_t socket)
{ plsocket *p;

  if ( socket < 0 || (size_t)socket >= socks_allocated )
  { errno = EINVAL;
    return NULL;
  }

  LOCK();
  p = sockets[socket];
  UNLOCK();

  if ( !p || p->magic != SOCK_MAGIC )
  { DEBUG(1, Sdprintf("Invalid NBIO socket: %d\n", socket));
    errno = EINVAL;
    return NULL;
  }

  return p;
}


static plsocket *
nbio_to_plsocket(nbio_sock_t socket)
{ plsocket *p;

  if ( !(p=nbio_to_plsocket_raw(socket)) )
    return NULL;

#ifdef __WINDOWS__
  if ( p->socket < 0 )
  { p->error = WSAECONNRESET;
    return NULL;
  }
#endif

  return p;
}


SOCKET
nbio_fd(nbio_sock_t socket)
{ plsocket *p;

  if ( !(p=nbio_to_plsocket(socket)) )
    return -1;

  return p->socket;
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Allocate a wrapper for an OS  socket.   The  wrapper  is allocated in an
array of pointers, to keep small  integer   identifiers  we can use with
FD_SET, etc. for implementing a compatible nbio_select().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static plsocket *
allocSocket(SOCKET socket)
{ plsocket *p = NULL;
  size_t i;

#ifdef __WINDOWS__
  if ( (p=lookupOSSocket(socket)) )
  { DEBUG(1, Sdprintf("WinSock %d already registered on %d\n",
		      (int)socket, p->id));
    p->socket = (SOCKET)-1;
  }
#endif

  LOCK();
  if ( socks_count+1 > socks_allocated )
  { if ( socks_allocated )
    { size_t newa = socks_allocated*2;

      sockets = PL_realloc(sockets, sizeof(plsocket*)*newa);
      for(i=socks_allocated; i<newa; i++)
	sockets[i] = NULL;
      socks_allocated = newa;
    } else
    { socks_allocated = 32;
      sockets = PL_malloc(sizeof(plsocket*)*socks_allocated);
      memset(sockets, 0, sizeof(plsocket*)*socks_allocated);
    }
  }

  for(i=0; i<socks_allocated; i++)
  { if ( sockets[i] == NULL )
    { sockets[i] = p = PL_malloc(sizeof(*p));
      socks_count++;
      break;
    }
  }
  UNLOCK();

  assert(i<socks_allocated);
  
  memset(p, 0, sizeof(*p));
  p->id     = (int)i;			/* place in the array */
  p->socket = socket;
  p->flags  = SOCK_DISPATCH;		/* by default, dispatch */
  p->magic  = SOCK_MAGIC;
#ifdef __WINDOWS__
  p->w32_flags = 0;
  p->request   = REQ_NONE;
#endif
  p->input = p->output = (IOSTREAM*)NULL;

  DEBUG(2, Sdprintf("[%d]: allocSocket(%d): bound to %p\n",
		    PL_thread_self(), socket, p));

  return p;
}


static int
freeSocket(plsocket *s)
{ int rval;
  nbio_sock_t socket;
  SOCKET sock;

  DEBUG(2, Sdprintf("Closing %d\n", s->id));
  if ( !s || s->magic != SOCK_MAGIC )
  { errno = EINVAL;
    return -1;
  }

  LOCK_FREE();
  LOCK();
  sockets[s->id] = NULL;
  socks_count--;
  UNLOCK();

  sock = s->socket;
  socket = s->id;
  s->magic = 0;
  PL_free(s);
  UNLOCK_FREE();

  if ( sock >= 0 )
  { again:
    if ( (rval=closesocket(sock)) == SOCKET_ERROR )
    { if ( errno == EINTR )
	goto again;
    }
    DEBUG(2, Sdprintf("freeSocket(%d=%d) returned %d\n",
		      socket, (int)sock, rval));
  } else
  { rval = 0;				/* already closed.  Use s->error? */
  }

  return rval;
}


		 /*******************************
		 *	      ERRORS		*
		 *******************************/

#ifdef __WINDOWS__
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
  return strdup(tmp);			/* leaks memory on unknown errors */
}

#endif /*BILLY_GETS_BETTER*/
#endif /*__WINDOWS__*/

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
#ifdef __WINDOWS__
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
#endif /*__WINDOWS__*/

  PL_unify_term(except,
		CompoundArg("error", 2),
		  CompoundArg("socket_error", 1),
		    AtomArg(msg),
		  PL_VARIABLE);
  }

  return PL_raise_exception(except);
}


const char *
nbio_last_error(nbio_sock_t socket)
{
#ifdef __WINDOWS__
  plsocket *s;

  if ( !(s=nbio_to_plsocket_raw(socket)) )
    return NULL;

  if ( s->error )
    return WinSockError(s->error);
#endif

  return NULL;
}


		 /*******************************
		 *	  INITIALISATION	*
		 *******************************/

int
nbio_init(const char *module)
{ INITLOCK();				/* is this ok? */

  LOCK();
  if ( initialised )
  { UNLOCK();
    return TRUE;
  }
  initialised = TRUE;

  FUNCTOR_module2 = PL_new_functor(PL_new_atom(":"), 2);
  FUNCTOR_ip4     = PL_new_functor(PL_new_atom("ip"), 4);
  FUNCTOR_ip1     = PL_new_functor(PL_new_atom("ip"), 1);
  ATOM_any	  = PL_new_atom("any");
  ATOM_broadcast  = PL_new_atom("broadcast");
  ATOM_loopback   = PL_new_atom("loopback");

#ifdef __WINDOWS__
{ WSADATA WSAData;

  hinstance = GetModuleHandle(module);

#if 0
  WM_SOCKET  = RegisterWindowMessage("SWI-Prolog:nonblockio:WM_SOCKET");
  WM_REQUEST = RegisterWindowMessage("SWI-Prolog:nonblockio:WM_REQUEST");
  WM_READY   = RegisterWindowMessage("SWI-Prolog:nonblockio:WM_READY");
  WM_DONE    = RegisterWindowMessage("SWI-Prolog:nonblockio:WM_DONE");
#endif

  if ( WSAStartup(MAKEWORD(2,0), &WSAData) )
  { UNLOCK();
    return PL_warning("nbio_init() - WSAStartup failed.");
  }
  startSocketThread();
}
#endif /*__WINDOWS__*/

  UNLOCK();
  return TRUE;
}
	

int
nbio_cleanup(void)
{ if ( initialised )
  {
#ifdef __WINDOWS__
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

nbio_sock_t
nbio_socket(int domain, int type, int protocol)
{ SOCKET sock;
  plsocket *s;
	
  assert(initialised);

  if ( (sock = socket(domain, type , protocol)) < 0)
  { nbio_error(errno, TCP_ERRNO);
    return -1;
  }

  if ( !(s=allocSocket(sock)) )		/* register it */
  { closesocket(sock);
    return -1;
  }

#ifdef __WINDOWS__
  WSAAsyncSelect(sock, State()->hwnd, WM_SOCKET,
		 FD_READ|FD_WRITE|FD_ACCEPT|FD_CONNECT|FD_CLOSE);
#endif

  return s->id;
}


int
nbio_closesocket(nbio_sock_t socket)
{ plsocket *s;

  if ( !(s = nbio_to_plsocket_raw(socket)) )
  { DEBUG(1, Sdprintf("nbio_closesocket(%d): no plsocket\n", socket));
    return -1;
  }

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
#ifdef __WINDOWS__
    if ( true(s, SOCK_CONNECT) )
    { if ( s->socket >= 0 )
	shutdown(s->socket, SD_SEND);
    }
#endif

    freeSocket(s);
  }

  return 0;
}


int
nbio_setopt(nbio_sock_t socket, nbio_option opt, ...)
{ plsocket *s;
  va_list args;
  int rc;

  if ( !(s = nbio_to_plsocket(socket)) )
    return -1;

  va_start(args, opt);

  switch(opt)
  { case TCP_NONBLOCK:
      rc = nbio_fcntl(socket, F_SETFL, O_NONBLOCK);
      break;
    case TCP_REUSEADDR:
    { int val = va_arg(args, int);

      if( setsockopt(s->socket, SOL_SOCKET, SO_REUSEADDR,
		     (const char *)&val, sizeof(val)) == -1)
      { nbio_error(h_errno, TCP_HERRNO);
	rc = -1;
      } else
	rc = 0;

      break;
    }
    case UDP_BROADCAST:
    { int val = va_arg(args, int);

      if( setsockopt(s->socket, SOL_SOCKET, SO_BROADCAST,
		     (const char *)&val, sizeof(val)) == -1)
      { nbio_error(h_errno, TCP_HERRNO);
	rc = -1;
      } else
	rc = 0;

      break;
    }
    case TCP_DISPATCH:
    { int val = va_arg(args, int);
      
      if ( val )
	set(s, SOCK_DISPATCH);
      else
	clear(s, SOCK_DISPATCH);

      rc = 0;

      break;
    }
    case TCP_INSTREAM:
    { IOSTREAM *in = va_arg(args, IOSTREAM*);

      s->flags |= SOCK_INSTREAM;
      s->input = in;

      rc = 0;

      break;
    }
    case TCP_OUTSTREAM:
    { IOSTREAM *out = va_arg(args, IOSTREAM*);

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
nbio_get_flags(nbio_sock_t socket)
{ plsocket *s;
  
  if ( !(s = nbio_to_plsocket(socket)) )
    return -1;

  return s->flags;
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
      return pl_error(NULL, 0, NULL, ERR_EXISTENCE, "service", Port);

    *port = ntohs(service->s_port);
    DEBUG(1, Sdprintf("Service %s at port %d\n", name, *port));
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
{ int		  port;
	
  addr->sin_family = AF_INET;
  addr->sin_addr.s_addr = INADDR_ANY;

  if ( PL_is_functor(Address, FUNCTOR_module2) )
  { char *hostName;
    term_t arg = PL_new_term_ref();

    PL_get_arg(1, Address, arg);
    if ( PL_get_atom_chars(arg, &hostName) )
    { struct hostent *host;

      if( !(host = gethostbyname(hostName)) )
	return nbio_error(h_errno, TCP_HERRNO);
      if ( (int)sizeof(addr->sin_addr) < host->h_length )
	return PL_warning("Oops, host address too long!");
      memcpy(&addr->sin_addr, host->h_addr, host->h_length);
    } else if ( !nbio_get_ip(arg, &addr->sin_addr) )
    { return pl_error(NULL, 0, NULL, ERR_ARGTYPE, 1, arg, "atom|ip/4");
    }

    PL_get_arg(2, Address, arg);
    if ( !nbio_get_port(arg, &port) )
      return FALSE;
  } else if ( PL_is_variable(Address) )
  { port = 0;
  } else if ( !nbio_get_port(Address, &port) )
    return FALSE;

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
  } else if ( PL_is_functor(ip4, FUNCTOR_ip1) )
  { term_t a = PL_new_term_ref();
    atom_t id;

    PL_get_arg(1, ip4, a);
    if ( PL_get_atom(a, &id) )
    { if ( id == ATOM_any )
	ip->s_addr = INADDR_ANY;
      else if ( id == ATOM_broadcast )
	ip->s_addr = INADDR_BROADCAST;
      else if ( id == ATOM_loopback )
	ip->s_addr = INADDR_LOOPBACK;
      else
	return FALSE;

      return TRUE;
    }
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
nbio_bind(nbio_sock_t socket, struct sockaddr *my_addr, size_t addrlen)
{ plsocket *s;

  if ( !(s = nbio_to_plsocket(socket)) )
    return -1;

#ifdef __WINDOWS__
  if ( bind(s->socket, my_addr, (int)addrlen) )
#else
  if ( bind(s->socket, my_addr, addrlen) )
#endif
  { nbio_error(errno, TCP_ERRNO);
    return -1;
  }

  s->flags |= SOCK_BIND;

  return 0;
}


int
nbio_connect(nbio_sock_t socket,
	     const struct sockaddr *serv_addr,
	     size_t addrlen)
{ plsocket *s;
       
  if ( !(s = nbio_to_plsocket(socket)) )
    return -1;
  
#ifdef __WINDOWS__
  if ( connect(s->socket, serv_addr, (int)addrlen) )
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
#else /*!__WINDOWS__*/
  for(;;)
  { if ( connect(s->socket, serv_addr, addrlen) )
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


nbio_sock_t
nbio_accept(nbio_sock_t master, struct sockaddr *addr, socklen_t *addrlen)
{ SOCKET slave;
  plsocket *m, *s;
	
  if ( !(m = nbio_to_plsocket(master)) )
    return -1;

#ifdef __WINDOWS__
{ int alen = (int)*addrlen;
  slave = accept(m->socket, addr, &alen);
  *addrlen = alen;
}

  if ( slave == SOCKET_ERROR )
  { m->error = WSAGetLastError();

    if ( m->error == WSAEWOULDBLOCK )
    { m->rdata.accept.addrlen = sizeof(m->rdata.accept.addr);
      placeRequest(m, REQ_ACCEPT);
      if ( !waitRequest(m) )
	return -1;
      if ( m->error )
	return nbio_error(m->error, TCP_ERRNO);
      *addrlen = m->rdata.accept.addrlen;
      memcpy(addr, &m->rdata.accept.addr, m->rdata.accept.addrlen);

      return m->rdata.accept.slave;		/* already registered */
    } else
    { nbio_error(m->error, TCP_ERRNO);
      return -1;
    }
  }

#else /*__WINDOWS__*/

  for(;;)
  { if ( !wait_socket(m) )
      return -1;

    slave = accept(m->socket, addr, addrlen);

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

#endif /*__WINDOWS__*/
  
  s = allocSocket(slave);
  s->flags |= SOCK_ACCEPT;
#ifndef __WINDOWS__
  if ( true(s, SOCK_NONBLOCK) )
    nbio_setopt(slave, TCP_NONBLOCK);
#endif

  return s->id;
}


int
nbio_listen(nbio_sock_t socket, int backlog)
{ plsocket *s;

  if ( !(s = nbio_to_plsocket(socket)) )
    return -1;

  if( listen(s->socket, backlog) == -1 )
  { nbio_error(errno, TCP_ERRNO);
    return -1;
  }

  s->flags |= SOCK_LISTEN;

  return 0;
}


		 /*******************************
		 *	  IO-STREAM STUFF	*
		 *******************************/

#define fdFromHandle(p) ((int)((intptr_t)(p)))

ssize_t
nbio_read(int socket, char *buf, size_t bufSize)
{ plsocket *s;
  int n;

  if ( !(s = nbio_to_plsocket(socket)) )
    return -1;

#ifdef __WINDOWS__

  DEBUG(3, Sdprintf("[%d] reading from socket %d\n",
		    PL_thread_self(), socket));

  n = recv(s->socket, buf, (int)bufSize, 0);
  if ( n < 0 )
  { int wsaerrno;

    if ( (wsaerrno=WSAGetLastError()) == WSAEWOULDBLOCK )
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
    { s->error = wsaerrno;
      errno = EIO;
    }
  } else
  { s->error = 0;
  }

#else /*__WINDOWS__*/

  for(;;)
  { if ( !wait_socket(s) )
    { errno = EPLEXCEPTION;
      return -1;
    }

    n = recv(s->socket, buf, bufSize, 0);

    if ( n == -1 && need_retry(errno) )
    { if ( PL_handle_signals() < 0 )
      { errno = EPLEXCEPTION;
	return -1;
      }
      continue;
    }

    break;
  }

#endif /*__WINDOWS__*/

  return n;
}


ssize_t
nbio_write(nbio_sock_t socket, char *buf, size_t bufSize)
{ plsocket *s;
  size_t len = bufSize;
  char *str = buf;

  if ( !(s = nbio_to_plsocket(socket)) )
    return -1;

#ifdef __WINDOWS__
  while( len != 0 )
  { ssize_t n;

    DEBUG(3, Sdprintf("[%d] sending %d bytes using socket %d\n",
		      PL_thread_self(), (int)len, socket));

    n = send(s->socket, str, (int)len, 0);
    if ( n < 0 )
    { int error = WSAGetLastError();

      if ( error == WSAEWOULDBLOCK )
	break;

      DEBUG(1, Sdprintf("[%d]: send(%d, %d bytes): %s\n",
			PL_thread_self(), s->socket, (int)bufSize,
			WinSockError(error)));

      s->error = error;
      return -1;
    }

    len -= n;
    str += n;
  }

  if ( len > 0 )			/* operation would block */
  { s->rdata.write.buffer  = str;
    s->rdata.write.size    = len;
    s->rdata.write.written = 0;
    placeRequest(s, REQ_WRITE);
    if ( !waitRequest(s) )
    { errno = EPLEXCEPTION;		/* handled Prolog signal */
      return -1;
    }
    if ( s->rdata.write.bytes < 0 )
      return -1;			/* error in s->error */
  }

#else /*__WINDOWS__*/

  while( len > 0 )
  { int n;

    n = send(s->socket, str, len, 0);
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

#endif /*__WINDOWS__*/

  return bufSize;
}


int
nbio_close_input(nbio_sock_t socket)
{ int rc = 0;
  plsocket *s;

  if ( !(s = nbio_to_plsocket_raw(socket)) )
    return -1;

  DEBUG(2, Sdprintf("[%d]: nbio_close_input(%d, flags=0x%x)\n",
		    PL_thread_self(), socket, s->flags));
  s->input = NULL;
  s->flags &= ~SOCK_INSTREAM;
#ifdef __WINDOWS__
  if ( false(s, SOCK_LISTEN) )
  { SOCKET sock;

    if ( (sock=s->socket) < 0 )
    { s->error = WSAECONNRESET;
      rc = -1;
    } else if ( shutdown(sock, SD_RECEIVE) == SOCKET_ERROR )
    { DEBUG(1, Sdprintf("shutdown(%d=%d, SD_RECEIVE) failed: %s\n",
			socket, s->socket,
			WinSockError(WSAGetLastError())));
    }
    rc = -1;
  }
#endif

  if ( !(s->flags & (SOCK_INSTREAM|SOCK_OUTSTREAM)) )
    return freeSocket(s);

  return rc;
}


int
nbio_close_output(nbio_sock_t socket)
{ int rc = 0;
  plsocket *s;

  if ( !(s = nbio_to_plsocket_raw(socket)) )
    return -1;

  DEBUG(2, Sdprintf("[%d]: nbio_close_output(%d, flags=0x%x)\n",
		    PL_thread_self(), socket, s->flags));
  if ( s->output )
  {
#if __WINDOWS__
    SOCKET sock;
#endif

    s->output = NULL;
    s->flags &= ~SOCK_OUTSTREAM;
#if __WINDOWS__
    if ( (sock=s->socket) < 0 )
    { s->error = WSAECONNRESET;
      rc = -1;
    } else if ( shutdown(sock, SD_SEND) == SOCKET_ERROR )
    {
#ifdef O_DEBUG
      if ( debugging )
      { const char *msg;
#ifdef __WINDOWS__
	msg = WinSockError(WSAGetLastError());
#else
        msg = strerror(errno);
#endif
        Sdprintf("shutdown(%d=%d, SD_SEND) failed: %s\n",
		 socket, s->socket, msg);
      }
#endif
      rc = -1;
    }
#endif
  }

  DEBUG(3, Sdprintf("%d->flags = 0x%x\n", socket, s->flags));
  if ( !(s->flags & (SOCK_INSTREAM|SOCK_OUTSTREAM)) )
    return freeSocket(s);

  return rc;
}


		 /*******************************
		 *	    UDP SUPPORT		*
		 *******************************/

ssize_t
nbio_recvfrom(int socket, void *buf, size_t bufSize, int flags,
	     struct sockaddr *from, socklen_t *fromlen)
{ plsocket *s;
  int n;

  if ( !(s = nbio_to_plsocket(socket)) )
    return -1;

#ifdef __WINDOWS__

  DEBUG(3, Sdprintf("[%d] recvfrom from socket %d\n",
		    PL_thread_self(), socket));

  n = recvfrom(s->socket, buf, (int)bufSize, flags, from, fromlen);
  if ( n < 0 )
  { int wsaerrno;

    if ( (wsaerrno=WSAGetLastError()) == WSAEWOULDBLOCK )
    { s->rdata.recvfrom.buffer  = buf;
      s->rdata.recvfrom.size    = bufSize;
      s->rdata.recvfrom.flags   = flags;
      s->rdata.recvfrom.from    = from;
      s->rdata.recvfrom.fromlen = fromlen;
      placeRequest(s, REQ_RECVFROM);
      if ( !waitRequest(s) )
      { errno = EPLEXCEPTION;
	return -1;
      }
      n = s->rdata.recvfrom.bytes;
    }

    if ( n < 0 )
    { s->error = wsaerrno;
      errno = EIO;
    }
  } else
  { s->error = 0;
  }

#else /*__WINDOWS__*/

  for(;;)
  { if ( !wait_socket(s) )
    { errno = EPLEXCEPTION;
      return -1;
    }

    n = recvfrom(s->socket, buf, bufSize, flags, from, fromlen);

    if ( n == -1 && need_retry(errno) )
    { if ( PL_handle_signals() < 0 )
      { errno = EPLEXCEPTION;
	return -1;
      }
      continue;
    }

    break;
  }

#endif /*__WINDOWS__*/

  return n;
}


ssize_t
nbio_sendto(nbio_sock_t socket, void *buf, size_t bufSize, int flags,
	    const struct sockaddr *to, socklen_t tolen)
{ plsocket *s;
  ssize_t n;

  if ( !(s = nbio_to_plsocket(socket)) )
    return -1;

#ifdef __WINDOWS__
  DEBUG(3, Sdprintf("[%d] sending %d bytes using socket %d\n",
		    PL_thread_self(), (int)bufSize, socket));

  n = sendto(s->socket, buf, (int)bufSize, flags, to, tolen);
  if ( n < 0 )
  { int error = WSAGetLastError();
    
    if ( error == WSAEWOULDBLOCK )
      goto wouldblock;

    DEBUG(1, Sdprintf("[%d]: sendto(%d, %d bytes): %s\n",
		      PL_thread_self(), s->socket, (int)bufSize,
		      WinSockError(error)));
    
    s->error = error;
    return -1;
  } else
    return n;

wouldblock:
  s->rdata.sendto.buffer  = buf;
  s->rdata.sendto.size    = bufSize;
  s->rdata.sendto.bytes   = 0;
  s->rdata.sendto.flags   = flags;
  s->rdata.sendto.to      = to;
  s->rdata.sendto.tolen   = tolen;
  placeRequest(s, REQ_SENDTO);
  if ( !waitRequest(s) )
  { errno = EPLEXCEPTION;		/* handled Prolog signal */
    return -1;
  }

  return s->rdata.sendto.bytes;

#else /*__WINDOWS__*/

  return sendto(s->socket, buf, bufSize, flags, to, tolen);
#endif /*__WINDOWS__*/
}
