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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef __CYGWIN__
#undef HAVE_H_ERRNO
#endif

#include "nonblockio.h"

#include <SWI-Stream.h>
#include "clib.h"
#include "error.h"

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <assert.h>
#include <string.h>
#ifdef WIN32
#include <malloc.h>
#endif

static atom_t ATOM_reuseaddr;		/* "reuseaddr" */
static atom_t ATOM_dispatch;		/* "dispatch" */
static atom_t ATOM_nonblock;		/* "nonblock" */
static atom_t ATOM_infinite;		/* "infinite" */

static functor_t FUNCTOR_socket1;	/* $socket(Id) */


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


static foreign_t
pl_close_socket(term_t socket)
{ int sock;

  if ( !tcp_get_socket(socket, &sock) )
    return FALSE;

  if ( nbio_closesocket(sock) < 0 )
    return FALSE;

  return TRUE;
}


static foreign_t
pl_host_to_address(term_t Host, term_t Ip)
{ struct in_addr ip;
  struct hostent *host;
  char *host_name;

  if ( PL_get_atom_chars(Host, &host_name) )
  { if ( (host = gethostbyname(host_name)) )
    { if ( sizeof(ip) == host->h_length )
      { memcpy(&ip, host->h_addr, host->h_length);
	return nbio_unify_ip4(Ip, ntohl(ip.s_addr));
      } else
	return PL_warning("tcp_host_to_address/2: length mismatch in address");
    } else
      return nbio_error(h_errno, TCP_HERRNO);
  } else if ( nbio_get_ip(Ip, &ip) )
  { if ( (host = gethostbyaddr((char *)&ip, sizeof(ip), AF_INET)) )
      return PL_unify_atom_chars(Host, host->h_name);
    else
      return nbio_error(h_errno, TCP_HERRNO);
  }

  return FALSE;
}


static foreign_t
pl_setopt(term_t Socket, term_t opt)
{ int socket;
  atom_t a;
  int arity;
       
  if ( !tcp_get_socket(Socket, &socket) )
    return FALSE;

  if ( PL_get_name_arity(opt, &a, &arity) )
  { if ( a == ATOM_reuseaddr && arity == 0 )
    { if ( nbio_setopt(socket, TCP_REUSEADDR, TRUE) == 0 )
	return TRUE;

      return FALSE;
    } else if ( a == ATOM_dispatch && arity == 1 )
    { int val;
      term_t a1 = PL_new_term_ref();

      if ( PL_get_arg(1, opt, a1) && PL_get_bool(a1, &val) )
      { if ( nbio_setopt(socket, TCP_DISPATCH, val) == 0 )
	  return TRUE;
	return FALSE;
      }
    } else if ( a == ATOM_nonblock && arity == 0 )
    { if ( nbio_setopt(socket, TCP_NONBLOCK) == 0 )
	return TRUE;
      return FALSE;
    }
  }
       
  return pl_error(NULL, 0, NULL, ERR_DOMAIN, opt, "socket_option");
}


		 /*******************************
		 *	  IO-STREAM STUFF	*
		 *******************************/

#define fdFromHandle(p) ((int)((long)(p)))

static int
tcp_read_handle(void *handle, char *buf, int bufSize)
{ int sock = fdFromHandle(handle);

  return nbio_read(sock, buf, bufSize);
}


static int
tcp_write_handle(void *handle, char *buf, int bufSize)
{ int sock = fdFromHandle(handle);

  return nbio_write(sock, buf, bufSize);
}


static long
tcp_seek_null(void *handle, long offset, int whence)
{ return -1;
}


static int
tcp_close_input_handle(void *handle)
{ int socket = fdFromHandle(handle);

  return nbio_close_input(socket);
}


static int
tcp_close_output_handle(void *handle)
{ int socket = fdFromHandle(handle);

  return nbio_close_output(socket);
}


static IOFUNCTIONS readFunctions =
{ tcp_read_handle,
  tcp_write_handle,
  tcp_seek_null,
  tcp_close_input_handle,
};


static IOFUNCTIONS writeFunctions =
{ tcp_read_handle,
  tcp_write_handle,
  tcp_seek_null,
  tcp_close_output_handle,
};


static foreign_t
pl_open_socket(term_t Socket, term_t Read, term_t Write)
{ IOSTREAM *in, *out;
  int socket;
  void *handle;

  if ( !tcp_get_socket(Socket, &socket) )
    return FALSE;
  handle = (void *)(long)socket;
  
  in  = Snew(handle, SIO_FILE|SIO_INPUT|SIO_RECORDPOS,  &readFunctions);
  if ( !PL_open_stream(Read, in) )
    return FALSE;
  nbio_setopt(socket, TCP_INSTREAM, in);

  if ( !(nbio_get_flags(socket) & SOCK_LISTEN) )
  { out = Snew(handle, SIO_FILE|SIO_OUTPUT|SIO_RECORDPOS, &writeFunctions);
    if ( !PL_open_stream(Write, out) )
      return FALSE;
    nbio_setopt(socket, TCP_OUTSTREAM, out);
  }

  return TRUE;
}


		 /*******************************
		 *	PROLOG CONNECTION	*
		 *******************************/

static foreign_t
pl_socket(term_t socket)
{ int sock = nbio_socket(AF_INET, SOCK_STREAM, 0);

  if ( sock < 0 )
    return FALSE;

  return tcp_unify_socket(socket, sock);
}


static foreign_t
pl_connect(term_t Socket, term_t Address)
{ int sock;
  struct sockaddr_in sockaddr;

  if ( !tcp_get_socket(Socket, &sock) ||
       !nbio_get_sockaddr(Address, &sockaddr) )
    return FALSE;
  
  if ( nbio_connect(sock, (struct sockaddr*)&sockaddr, sizeof(sockaddr)) == 0 )
    return TRUE;

  return FALSE;
}


static foreign_t
pl_bind(term_t Socket, term_t Address)
{ struct sockaddr_in sockaddr;
  int socket;
       
  if ( !tcp_get_socket(Socket, &socket) ||
       !nbio_get_sockaddr(Address, &sockaddr) )
    return FALSE;

  if ( nbio_bind(socket, (struct sockaddr*)&sockaddr, sizeof(sockaddr)) < 0 )
    return FALSE;

  if ( PL_is_variable(Address) )
  { struct sockaddr_in addr;
    int len = sizeof(addr);

    if ( getsockname(socket, (struct sockaddr *) &addr, &len) )
      return nbio_error(errno, TCP_ERRNO);
    PL_unify_integer(Address, ntohs(addr.sin_port));
  }

  return TRUE;
}


static foreign_t
pl_listen(term_t Sock, term_t BackLog)
{ int socket;
  int backlog;

  if ( !tcp_get_socket(Sock, &socket) )
    return FALSE;

  if ( !PL_get_integer(BackLog, &backlog) ) 
    return pl_error(NULL, 0, NULL, ERR_ARGTYPE, -1, BackLog, "integer");

  if ( nbio_listen(socket, backlog) < 0 )
    return FALSE;

  return TRUE;
}


static foreign_t
pl_accept(term_t Master, term_t Slave, term_t Peer)
{ int master, slave;
  struct sockaddr_in addr;
  int addrlen = sizeof(addr);

  if ( !tcp_get_socket(Master, &master) )
    return FALSE;

  if ( (slave = nbio_accept(master, (struct sockaddr*)&addr, &addrlen)) < 0 )
    return FALSE;
					/* TBD: close on failure */
  if ( nbio_unify_ip4(Peer, ntohl(addr.sin_addr.s_addr)) &&
       tcp_unify_socket(Slave, slave) )
    return TRUE;

  return FALSE;
}



static foreign_t
pl_gethostname(term_t name)
{ char buf[256];

  if ( !nbio_init() )
    return FALSE;

  if ( gethostname(buf, sizeof(buf)) == 0 )
  { struct hostent *he;

    if ( (he = gethostbyname(buf)) )
      return PL_unify_atom_chars(name, he->h_name);
    else
      return PL_unify_atom_chars(name, buf);
  }

  return nbio_error(h_errno, TCP_HERRNO);
}


		 /*******************************
		 *	       SELECT		*
		 *******************************/

typedef struct fdentry
{ int fd;
  term_t stream;
  struct fdentry *next;
} fdentry;


static term_t
findmap(fdentry *map, int fd)
{ for( ; map; map = map->next )
  { if ( map->fd == fd )
      return map->stream;
  }
  assert(0);
  return 0;
}


static foreign_t
tcp_select(term_t Streams, term_t Available, term_t timeout)
{ fd_set fds;
  struct timeval t, *to;
  double time;
  int n, max = 0, ret, min = 1000000;
  fdentry *map     = NULL;
  term_t head      = PL_new_term_ref();
  term_t streams   = PL_copy_term_ref(Streams);
  term_t available = PL_copy_term_ref(Available);
  term_t ahead     = PL_new_term_ref();
  int from_buffer  = 0;
  atom_t a;

  FD_ZERO(&fds);
  while( PL_get_list(streams, head, streams) )
  { IOSTREAM *s;
    int fd;
    fdentry *e;

    if ( !PL_get_stream_handle(head, &s) )
      return FALSE;
    if ( (fd=Sfileno(s)) < 0 )
    { PL_release_stream(s);
      return pl_error("tcp_select", 3, NULL, ERR_DOMAIN,
		      head, "file_stream");
    }
    PL_release_stream(s);
					/* check for input in buffer */
    if ( s->bufp < s->limitp )
    { if ( !PL_unify_list(available, ahead, available) ||
	   !PL_unify(ahead, head) )
	return FALSE;
      from_buffer++;
    }

    e         = alloca(sizeof(*e));
    e->fd     = fd;
    e->stream = PL_copy_term_ref(head);
    e->next   = map;
    map       = e;

#ifdef WIN32
    FD_SET((SOCKET)fd, &fds);
#else
    FD_SET(fd, &fds);
#endif

    if ( fd > max )
      max = fd;
    if( fd < min )
      min = fd;
  }
  if ( !PL_get_nil(streams) )
    return pl_error("tcp_select", 3, NULL, ERR_TYPE, Streams, "list");

  if ( from_buffer > 0 )
    return PL_unify_nil(available);

  if ( PL_get_atom(timeout, &a) && a == ATOM_infinite )
  { to = NULL;
  } else
  { if ( !PL_get_float(timeout, &time) )
      return pl_error("tcp_select", 3, NULL,
		      ERR_TYPE, timeout, "number");
  
    if ( time >= 0.0 )
    { t.tv_sec  = (int)time;
      t.tv_usec = ((int)(time * 1000000) % 1000000);
    } else
    { t.tv_sec  = 0;
      t.tv_usec = 0;
    }
    to = &t;
  }

  while( (ret=nbio_select(max+1, &fds, NULL, NULL, to)) == -1 &&
	 errno == EINTR )
  { fdentry *e;

    if ( PL_handle_signals() < 0 )
      return FALSE;			/* exception */

    FD_ZERO(&fds);			/* EINTR may leave fds undefined */
    for(e=map; e; e=e->next)		/* so we rebuild it to be safe */
    {
#ifdef WIN32
      FD_SET((SOCKET)e->fd, &fds);
#else
      FD_SET(e->fd, &fds);
#endif
    }
  }

  switch(ret)
  { case -1:
      return pl_error("tcp_select", 3, ERR_ERRNO, errno);

    case 0: /* Timeout */
      break;

    default: /* Something happend -> check fds */
      for(n=min; n <= max; n++)
      { if ( FD_ISSET(n, &fds) )
	{ if ( !PL_unify_list(available, ahead, available) ||
	       !PL_unify(ahead, findmap(map, n)) )
	    return FALSE;
	}
      }
      break;
  }

  return PL_unify_nil(available);
}


#ifdef O_DEBUG
static foreign_t
pl_debug(term_t val)
{ int dbg;

  PL_get_integer(val, &dbg);
  nbio_debug(dbg);

  return TRUE;
}
#endif


install_t
install_socket()
{ ATOM_reuseaddr  = PL_new_atom("reuseaddr");
  ATOM_dispatch   = PL_new_atom("dispatch");
  ATOM_nonblock   = PL_new_atom("nonblock");
  ATOM_infinite   = PL_new_atom("infinite");

  FUNCTOR_socket1 = PL_new_functor(PL_new_atom("$socket"), 1);
  
#ifdef O_DEBUG
  PL_register_foreign("user:tcp_debug",	      1, pl_debug,	      0);
#endif
  PL_register_foreign("tcp_accept",           3, pl_accept,           0);
  PL_register_foreign("tcp_bind",             2, pl_bind,             0);
  PL_register_foreign("tcp_connect",          2, pl_connect,	      0);
  PL_register_foreign("tcp_listen",           2, pl_listen,           0);
  PL_register_foreign("tcp_open_socket",      3, pl_open_socket,      0);
  PL_register_foreign("tcp_socket",           1, pl_socket,           0);
  PL_register_foreign("tcp_close_socket",     1, pl_close_socket,     0);
  PL_register_foreign("tcp_setopt",           2, pl_setopt,           0);
  PL_register_foreign("tcp_host_to_address",  2, pl_host_to_address,  0);
  PL_register_foreign("gethostname",          1, pl_gethostname,      0);
  PL_register_foreign("tcp_select",           3, tcp_select,          0);
}


install_t
uninstall_socket()
{ nbio_cleanup();
}
