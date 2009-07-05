/*  $Id$

    Part of SWI-Prolog

    Author:        Jeffrey Rosenwald
    E-mail:        jeffrose@acm.org
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, Jeffrey Rosenwald

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

#ifdef HAVE_LINUX_TIPC_H
#include <linux/tipc.h>
#else
#error "Cannot find <tipc.h>"
#endif

static atom_t ATOM_scope;
static atom_t ATOM_no_scope;
static atom_t ATOM_node;
static atom_t ATOM_cluster;
static atom_t ATOM_zone;
static atom_t ATOM_all;

static atom_t ATOM_socket_type;
static atom_t ATOM_dgram;
static atom_t ATOM_rdm;
static atom_t ATOM_seqpacket;
static atom_t ATOM_stream;

static atom_t ATOM_nodelay;		/* "nodelay" */
static atom_t ATOM_nonblock;		/* "nonblock" */
static atom_t ATOM_as;			/* "as" */
static atom_t ATOM_atom;		/* "atom" */
static atom_t ATOM_string;		/* "string" */
static atom_t ATOM_codes;		/* "codes" */

static atom_t ATOM_importance;		/* "importance" */
static atom_t ATOM_low;			/* "low" */
static atom_t ATOM_medium;		/* "medium" */
static atom_t ATOM_high;		/* "high" */
static atom_t ATOM_critical;		/* "critical" */
static atom_t ATOM_src_droppable;	/* "src_droppable" */
static atom_t ATOM_dest_droppable;	/* "dest_droppable" */
static atom_t ATOM_conn_timeout;	/* "conn_timeout" */

static functor_t FUNCTOR_tipc_socket1;	/* $tipc_socket(Id) */
static functor_t FUNCTOR_port_id;
static functor_t FUNCTOR_name;
static functor_t FUNCTOR_name_seq;
static functor_t FUNCTOR_mcast;


		 /*******************************
		 *	     CONVERSION		*
		 *******************************/

static int
tipc_get_socket(term_t Socket, int *id)
{ IOSTREAM *s;
  int socket;

  if ( PL_is_functor(Socket, FUNCTOR_tipc_socket1) )
  { term_t a = PL_new_term_ref();

    PL_get_arg(1, Socket, a);
    if ( PL_get_integer(a, id) )
      return TRUE;
  }

  if ( PL_get_stream_handle(Socket, &s) )
  { socket = (int)(intptr_t)s->handle;

    *id = socket;
    return TRUE;
  }

  return pl_error(NULL, 0, NULL, ERR_ARGTYPE, -1, Socket, "socket");
}


static int
tipc_unify_socket(term_t Socket, int id)
{ return PL_unify_term(Socket,
		       PL_FUNCTOR, FUNCTOR_tipc_socket1,
		         IntArg(id));
}


#define pl_open_socket tipc_open_socket
#define pl_listen tipc_listen
#define pl_close_socket tipc_close_socket
#define tcp_get_socket(t, p) tipc_get_socket(t, p)
#include "sockcommon.c"

int
get_uint(term_t term, unsigned *value)
{ int64_t v0;

  if ( !PL_get_int64(term, &v0))
    return FALSE;

#if 0
	/* tipc users are somewhat cavalier about using -1 in place of 4294967295U */
  if(v0 < -1 || v0 > UINT_MAX)
	return FALSE;
#endif

  *value = (unsigned) v0 & 0xffffffff; 

  return TRUE;
}

static int
nbio_get_tipc(term_t tipc, struct sockaddr_tipc *sockaddr)
{ term_t a = PL_new_term_ref();
  sockaddr->family = AF_TIPC;

  do
  {
  if ( PL_is_functor(tipc, FUNCTOR_port_id) )
  {
    unsigned ref, node;

    PL_get_arg(1, tipc, a);
    if ( !get_uint(a, &ref) )
      break;

    PL_get_arg(2, tipc, a);
    if ( !get_uint(a, &node) )
      break;

    sockaddr->addrtype     = TIPC_ADDR_ID;
    sockaddr->addr.id.ref  = ref;
    sockaddr->addr.id.node = node;

    return TRUE;
  }

  if ( PL_is_functor(tipc, FUNCTOR_name) )
  {
    unsigned arg1, arg2, arg3;

    PL_get_arg(1, tipc, a);
    if ( !get_uint(a, &arg1) )
      break;

    PL_get_arg(2, tipc, a);
    if ( !get_uint(a, &arg2) )
      break;

    PL_get_arg(3, tipc, a);
    if ( !get_uint(a, &arg3) )
      break;

    sockaddr->addrtype                = TIPC_ADDR_NAME;
    sockaddr->addr.name.name.type     = arg1;
    sockaddr->addr.name.name.instance = arg2;
    sockaddr->addr.name.domain        = arg3;

    return TRUE;

  }

  if ( PL_is_functor(tipc, FUNCTOR_name_seq) ||
       PL_is_functor(tipc, FUNCTOR_mcast))
  {
    unsigned arg1, arg2, arg3;

    PL_get_arg(1, tipc, a);
    if ( !get_uint(a, &arg1) )
      break;

    PL_get_arg(2, tipc, a);
    if ( !get_uint(a, &arg2) )
      break;

    PL_get_arg(3, tipc, a);
    if ( !get_uint(a, &arg3) )
      break;

    sockaddr->addrtype           = TIPC_ADDR_NAMESEQ;
    sockaddr->addr.nameseq.type  = arg1;
    sockaddr->addr.nameseq.lower = arg2;
    sockaddr->addr.nameseq.upper = arg3;

    return TRUE;
  }

  } while(FALSE);

  return FALSE;
}


static int
nbio_get_tipc_sockaddr(term_t Address, struct sockaddr_tipc *addr)
{ if ( !nbio_get_tipc(Address, addr) )
    return pl_error(NULL, 0, NULL, ERR_ARGTYPE, 1, Address, "tipc address");

  return TRUE;
}


		 /*******************************
		 *	       SETOPT		*
		 *******************************/

typedef enum
{ NB_TIPC_IMPORTANCE,
  NB_TIPC_SRC_DROPPABLE,
  NB_TIPC_DEST_DROPPABLE,
  NB_TIPC_CONN_TIMEOUT,
} tipc_option;


static int
tipc_setopt(nbio_sock_t socket, tipc_option opt, ...)
{ plsocket_ptr s;
  va_list args;
  int rc;

  if ( !(s = nbio_to_plsocket(socket)) )
    return -1;

  va_start(args, opt);

  switch(opt)
  { case NB_TIPC_IMPORTANCE:
    { int val = va_arg(args, int);

      if ( setsockopt(plsocket_handle(s), SOL_TIPC, TIPC_IMPORTANCE,
		      (const void *)&val, sizeof(val)) == -1 )
      { nbio_error(h_errno, TCP_HERRNO);
	rc = -1;
      } else
	rc = 0;

      break;
    }
    case NB_TIPC_SRC_DROPPABLE:
    case NB_TIPC_DEST_DROPPABLE:
    { int val = va_arg(args, int);
      int level = (opt == NB_TIPC_SRC_DROPPABLE) ? TIPC_SRC_DROPPABLE
						 : TIPC_DEST_DROPPABLE;

      if ( setsockopt(plsocket_handle(s), SOL_TIPC, level,
		      (const void *) &val, sizeof(val)) == -1 )
      { nbio_error(h_errno, TCP_HERRNO);
	rc = -1;
      } else
	rc = 0;

      break;
    }
    case NB_TIPC_CONN_TIMEOUT:
    { int val = va_arg(args, int);

      if ( setsockopt(plsocket_handle(s), SOL_TIPC, TIPC_CONN_TIMEOUT,
		      (const void *) &val, sizeof(val)) == -1 )
      { nbio_error(h_errno, TCP_HERRNO);
	rc = -1;
      } else
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


static foreign_t
pl_tipc_setopt(term_t Socket, term_t opt)
{ int socket;
  atom_t a;
  int arity;

  if ( !tipc_get_socket(Socket, &socket) )
    return FALSE;

  if ( PL_get_name_arity(opt, &a, &arity) )
  { if ( a == ATOM_importance && arity == 1 )
    { atom_t val;
      term_t a1 = PL_new_term_ref();
      int ival = TIPC_LOW_IMPORTANCE;

      if (PL_get_arg(1, opt, a1))
      { if(!PL_get_atom(a1, &val) )
	  return pl_error(NULL, 0, NULL, ERR_DOMAIN, a1, "atom");

	if(val == ATOM_low)
	  ival = TIPC_LOW_IMPORTANCE;
	else if(val == ATOM_medium)
	  ival = TIPC_MEDIUM_IMPORTANCE;
	else if(val == ATOM_high)
	  ival = TIPC_HIGH_IMPORTANCE;
	else if(val == ATOM_critical)
	  ival = TIPC_CRITICAL_IMPORTANCE;
	else
	  return pl_error(NULL, 0, NULL, ERR_DOMAIN, a1, "low, medium, high, or critical");

	return((tipc_setopt(socket, NB_TIPC_IMPORTANCE, ival) == 0) ? TRUE : FALSE);
      }
    }

    if ( ((a == ATOM_dest_droppable) ||
	  (a == ATOM_src_droppable)) && arity == 1 )
    { int val;
      term_t a1 = PL_new_term_ref();
      int option = (a == ATOM_dest_droppable) ? NB_TIPC_DEST_DROPPABLE
					      : NB_TIPC_SRC_DROPPABLE;

      if (PL_get_arg(1, opt, a1))
      { if(!PL_get_bool(a1, &val) )
	  return pl_error(NULL, 0, NULL, ERR_DOMAIN, a1, "boolean");

	return((tipc_setopt(socket, option, val) == 0) ? TRUE : FALSE);
      }
    }
    if ( a == ATOM_conn_timeout && arity == 1 )
    { double val;
      int ival;
      term_t a1 = PL_new_term_ref();

      if (PL_get_arg(1, opt, a1))
      { if(!PL_get_float(a1, &val) || val < 0)
	  return pl_error(NULL, 0, NULL, ERR_DOMAIN, a1, "float");

        ival = val * 1000;  // time is in milliseconds

	return((tipc_setopt(socket, NB_TIPC_CONN_TIMEOUT, ival) == 0) ? TRUE : FALSE);
      }
    }

    if ( a == ATOM_nodelay && arity <= 1 )
    { int enable, rc;

      if ( arity == 0 )
      { enable = TRUE;
      } else /*if ( arity == 1 )*/
      { term_t a = PL_new_term_ref();

	PL_get_arg(1, opt, a);
	if ( !PL_get_bool(a, &enable) )
	  return pl_error(NULL, 0, NULL, ERR_DOMAIN, a, "boolean");
      }

      if ( (rc=nbio_setopt(socket, TCP_NO_DELAY, enable) == 0) )
	return TRUE;
      if ( rc == -2 )
  	return pl_error(NULL, 0, NULL, ERR_DOMAIN, opt, "socket_option");

    }

    if ( a == ATOM_nonblock && arity == 0 )
      return((nbio_setopt(socket, TCP_NONBLOCK) == 0) ? TRUE : FALSE );
  }

  return pl_error(NULL, 0, NULL, ERR_DOMAIN, opt, "socket_option");
}


static int
unify_tipc_address(term_t t, struct sockaddr_tipc *addr)
{ switch ( addr->addrtype )
  { case TIPC_ADDR_ID:
      return PL_unify_term(t, PL_FUNCTOR_CHARS, "port_id", 2,
			   IntArg(addr->addr.id.ref),
			   IntArg(addr->addr.id.node));
    case TIPC_ADDR_NAME:
      return PL_unify_term(t, PL_FUNCTOR_CHARS, "name", 3,
			   IntArg(addr->addr.name.name.type),
			   IntArg(addr->addr.name.name.instance),
			   IntArg(addr->addr.name.domain));
    case TIPC_ADDR_NAMESEQ:
      return PL_unify_term(t, PL_FUNCTOR_CHARS, "name_seq", 3,
			   IntArg(addr->addr.nameseq.type),
			   IntArg(addr->addr.nameseq.lower),
			   IntArg(addr->addr.nameseq.upper));
    default:
      return FALSE;
  }
}


static foreign_t
pl_tipc_get_name(term_t Socket, term_t t)
{ struct sockaddr_tipc addr;
  int socket;
  SOCKET fd;
#ifdef __WINDOWS__
  int alen = sizeof(addr);
#else
  socklen_t alen = sizeof(addr);
#endif

  if ( !tipc_get_socket(Socket, &socket))
    return FALSE;

  fd = nbio_fd(socket);

  if ( getsockname(fd, (struct sockaddr *) &addr, &alen) )
    return nbio_error(errno, TCP_ERRNO);
  else
    return unify_tipc_address(t, &addr);
}


#define TIPC_MAXDATA 4096

static foreign_t
pl_tipc_receive(term_t Socket, term_t Data, term_t From, term_t options)
{ struct sockaddr_tipc sockaddr;
#ifdef __WINDOWS__
  int alen = sizeof(sockaddr);
#else
  socklen_t alen = sizeof(sockaddr);
#endif
  int socket;
  int flags = 0;
  char buf[TIPC_MAXDATA];
  ssize_t n;
  int as = PL_STRING;

  if ( !PL_get_nil(options) )
  { term_t tail = PL_copy_term_ref(options);
    term_t head = PL_new_term_ref();
    term_t arg  = PL_new_term_ref();

    while(PL_get_list(tail, head, tail))
    { atom_t name;
      int arity;

      if ( PL_get_name_arity(head, &name, &arity) )
      {
	if ( name == ATOM_as && arity == 1)
	{ atom_t a;

          PL_get_arg(1, head, arg);

	  if ( !PL_get_atom(arg, &a) )
	    return pl_error(NULL, 0, NULL, ERR_TYPE, head, "atom");
	  if ( a == ATOM_atom )
	    as = PL_ATOM;
	  else if ( a == ATOM_codes )
	    as = PL_CODE_LIST;
	  else if ( a == ATOM_string )
	    as = PL_STRING;
	  else
	    return pl_error(NULL, 0, NULL, ERR_DOMAIN, arg, "as_option");
	}
        else if (name == ATOM_nonblock && arity == 0)
          flags |= MSG_DONTWAIT;
        else
	  return pl_error(NULL, 0, NULL, ERR_TYPE, head, "option");
      }
      else
	return pl_error(NULL, 0, NULL, ERR_TYPE, head, "option");
    }
    if ( !PL_get_nil(tail) )
      return pl_error(NULL, 0, NULL, ERR_TYPE, tail, "list");
  }


  if ( !tipc_get_socket(Socket, &socket))
    return FALSE;

  if ( (n=nbio_recvfrom(socket, buf, sizeof(buf), flags,
			(struct sockaddr*)&sockaddr, &alen)) == -1 )
    return nbio_error(errno, TCP_ERRNO);

  if ( !PL_unify_chars(Data, as, n, buf) )
    return FALSE;

  return unify_tipc_address(From, &sockaddr);
}


static foreign_t
pl_tipc_send(term_t Socket, term_t Data, term_t To, term_t Options)
{ struct sockaddr_tipc sockaddr;
#ifdef __WINDOWS__
  int alen = sizeof(sockaddr);
#else
  int alen = sizeof(sockaddr);
#endif
  int socket;
  int flags = 0L;
  char *data;
  size_t dlen;
  ssize_t n;

  if ( !PL_get_nchars(Data, &dlen, &data, CVT_ALL|CVT_EXCEPTION) )
    return FALSE;

  if ( !tipc_get_socket(Socket, &socket) ||
       !nbio_get_tipc_sockaddr(To, &sockaddr) )
    return FALSE;

  if ( (n=nbio_sendto(socket, data,
		      (int)dlen,
		      flags,
		      (struct sockaddr*)&sockaddr, alen)) == -1 )
    return nbio_error(errno, TCP_ERRNO);

  return TRUE;
}

#ifndef AF_TIPC
#define AF_TIPC 30
#endif

static foreign_t
create_tipc_socket(term_t socket, int type)
{ int sock;

  sock = nbio_socket(AF_TIPC, type, 0);
  if ( sock < 0 )
    return FALSE;

  return tipc_unify_socket(socket, sock);
}


static foreign_t
tipc_socket(term_t socket, term_t opt)
{ atom_t a;
  int arity;

  if ( PL_get_name_arity(opt, &a, &arity) && arity == 0)
    { int type;

    if ( a == ATOM_dgram )
      type = SOCK_DGRAM;
    else if ( a == ATOM_rdm )
      type = SOCK_RDM;
    else if ( a == ATOM_seqpacket )
      type = SOCK_SEQPACKET;
    else if ( a == ATOM_stream )
      type = SOCK_STREAM;
    else
      return pl_error(NULL, 0, NULL, ERR_DOMAIN, opt, "rdm, dgram, seqpacket, or stream");

      return create_tipc_socket(socket, type);
    }
    else return pl_error(NULL, 0, NULL, ERR_ARGTYPE, 1, opt, "atom");

  return FALSE;
}


static foreign_t
pl_tipc_accept(term_t Master, term_t Slave, term_t Peer)
{ int master, slave;
  struct sockaddr_tipc addr;
  socklen_t addrlen = sizeof(addr);

  if ( !tipc_get_socket(Master, &master) )
    return FALSE;

  if ( (slave = nbio_accept(master, (struct sockaddr*)&addr, &addrlen)) < 0 )
    return FALSE;
					/* TBD: close on failure */
  if ( unify_tipc_address(Peer, &addr) &&
       tipc_unify_socket(Slave, slave) )
    return TRUE;

  return FALSE;
}


static foreign_t
pl_tipc_connect(term_t Socket, term_t Address)
{ int sock;
  struct sockaddr_tipc sockaddr;

  if ( !tipc_get_socket(Socket, &sock) ||
       !nbio_get_tipc_sockaddr(Address, &sockaddr) )
    return FALSE;

  if ( nbio_connect(sock, (struct sockaddr*)&sockaddr, sizeof(sockaddr)) == 0 )
    return TRUE;

  return FALSE;
}


static foreign_t
pl_tipc_bind(term_t Socket, term_t Address, term_t opt)
{ struct sockaddr_tipc sockaddr;
  size_t addrlen = sizeof(sockaddr);
  int socket;
  atom_t a;
  int arity;

  memset(&sockaddr, 0, sizeof(sockaddr));

  if ( !tipc_get_socket(Socket, &socket) ||
       !nbio_get_tipc_sockaddr(Address, &sockaddr) )
    return FALSE;

  if ( PL_get_name_arity(opt, &a, &arity) )
  { if ( (a == ATOM_scope || a == ATOM_no_scope) && arity == 1 )
    { atom_t val;
      term_t a1 = PL_new_term_ref();

      if (PL_get_arg(1, opt, a1))
      { signed char ival = 0;

	if ( !PL_get_atom(a1, &val) )
	  return pl_error(NULL, 0, NULL, ERR_DOMAIN, a1, "atom");

	if ( val == ATOM_zone )
	  ival = TIPC_ZONE_SCOPE;
	else if ( val == ATOM_cluster )
	  ival = TIPC_CLUSTER_SCOPE;
	else if ( val == ATOM_node )
	  ival = TIPC_NODE_SCOPE;
	else if ( val == ATOM_all && a == ATOM_no_scope)
	  addrlen = 0;
	else
	  return pl_error(NULL, 0, NULL, ERR_DOMAIN, a1, "node, cluster, or zone");

	sockaddr.scope = (a == ATOM_scope) ? ival
                                           : -ival;

	if ( nbio_bind(socket, (struct sockaddr*)&sockaddr, addrlen) < 0 )
	  return FALSE;
      }
    } else
      return pl_error(NULL, 0, NULL, ERR_ARGTYPE, 1, opt, "scoping option");

      return TRUE;
  }

  return pl_error(NULL, 0, NULL, ERR_DOMAIN, a, "scope/1");
}

static foreign_t
pl_tipc_subscribe(term_t Socket, term_t Address,
		  term_t timeout, term_t filter, term_t usr_handle)
{ struct sockaddr_tipc sockaddr;
  struct tipc_subscr subscr;
  int socket;
  unsigned time, filt;
  char *handle;
  size_t handle_len;
  SOCKET fd;

  memset(&subscr, 0, sizeof(subscr));
  memset(&sockaddr, 0, sizeof(sockaddr));

  if ( !tipc_get_socket(Socket, &socket) ||
       !nbio_get_tipc_sockaddr(Address, &sockaddr))
    return FALSE;

  if(sockaddr.addrtype != TIPC_ADDR_NAMESEQ)
    return pl_error(NULL, 0, NULL, ERR_DOMAIN, Address, "name_seq/3");

  if( !get_uint(timeout, &time)) 
    return pl_error(NULL, 0, NULL, ERR_DOMAIN, timeout, "integer");

  if( !get_uint(filter, &filt))
    return pl_error(NULL, 0, NULL, ERR_DOMAIN, filter, "integer");

  if ( !PL_get_nchars(usr_handle, &handle_len, &handle, CVT_ALL|CVT_EXCEPTION) )
    return FALSE;

  memcpy(&subscr.seq, &sockaddr.addr.nameseq, sizeof(subscr.seq));
  subscr.timeout = time;
  subscr.filter = filt;
  memcpy(&subscr.usr_handle, handle,
	 (handle_len < sizeof(subscr.usr_handle)) ? handle_len
	 					  : sizeof(subscr.usr_handle));

  fd = nbio_fd(socket);

  if ( (send(fd, &subscr, sizeof(subscr), 0)) != sizeof(subscr) )
    return nbio_error(errno, TCP_ERRNO);
  else
    return TRUE;
}


install_t
install_tipc()
{ nbio_init("tipc");

  ATOM_scope	       = PL_new_atom("scope");
  ATOM_no_scope	       = PL_new_atom("no_scope");
  ATOM_node	       = PL_new_atom("node");
  ATOM_cluster	       = PL_new_atom("cluster");
  ATOM_zone	       = PL_new_atom("zone");
  ATOM_all	       = PL_new_atom("all");

  ATOM_importance      = PL_new_atom("importance");
  ATOM_low	       = PL_new_atom("low");
  ATOM_medium	       = PL_new_atom("medium");
  ATOM_high	       = PL_new_atom("high");
  ATOM_critical	       = PL_new_atom("critical");
  ATOM_src_droppable   = PL_new_atom("src_droppable");
  ATOM_dest_droppable  = PL_new_atom("dest_droppable");
  ATOM_conn_timeout    = PL_new_atom("conn_timeout");

  ATOM_socket_type     = PL_new_atom("socket_type");
  ATOM_dgram	       = PL_new_atom("dgram");
  ATOM_rdm	       = PL_new_atom("rdm");
  ATOM_seqpacket       = PL_new_atom("seqpacket");
  ATOM_stream	       = PL_new_atom("stream");

  ATOM_nodelay	       = PL_new_atom("nodelay");
  ATOM_nonblock	       = PL_new_atom("nonblock");
  ATOM_as	       = PL_new_atom("as");
  ATOM_atom	       = PL_new_atom("atom");
  ATOM_string	       = PL_new_atom("string");
  ATOM_codes	       = PL_new_atom("codes");

  FUNCTOR_tipc_socket1 = PL_new_functor(PL_new_atom("$tipc_socket"), 1);
  FUNCTOR_port_id      = PL_new_functor(PL_new_atom("port_id"), 2);
  FUNCTOR_name	       = PL_new_functor(PL_new_atom("name"), 3);
  FUNCTOR_name_seq     = PL_new_functor(PL_new_atom("name_seq"), 3);
  FUNCTOR_mcast	       = PL_new_functor(PL_new_atom("mcast"), 3);

  PL_register_foreign("tipc_socket",          2, tipc_socket,         0);
  PL_register_foreign("tipc_close_socket",    1, tipc_close_socket,   0);
  PL_register_foreign("tipc_setopt",          2, pl_tipc_setopt,      0);
  PL_register_foreign("tipc_bind",            3, pl_tipc_bind,        0);
  PL_register_foreign("tipc_listen",          2, tipc_listen,         0);
  PL_register_foreign("tipc_open_socket",     3, tipc_open_socket,    0);
  PL_register_foreign("tipc_accept",          3, pl_tipc_accept,      0);
  PL_register_foreign("tipc_connect",         2, pl_tipc_connect,     0);
  PL_register_foreign("tipc_get_name",        2, pl_tipc_get_name,    0);
  PL_register_foreign("tipc_receive",	      4, pl_tipc_receive,     0);
  PL_register_foreign("tipc_send",	      4, pl_tipc_send,	      0);
  PL_register_foreign("tipc_subscribe",	      5, pl_tipc_subscribe,   0);
}
