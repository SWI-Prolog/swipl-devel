/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2000-2008, University of Amsterdam

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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Shared stuff between the normal socket interface and the TIPC one.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	  IO-STREAM STUFF	*
		 *******************************/

#define fdFromHandle(p) ((nbio_sock_t)((long)(p)))

static ssize_t
tcp_read_handle(void *handle, char *buf, size_t bufSize)
{ nbio_sock_t sock = fdFromHandle(handle);

  return nbio_read(sock, buf, bufSize);
}


static ssize_t
tcp_write_handle(void *handle, char *buf, size_t bufSize)
{ nbio_sock_t sock = fdFromHandle(handle);

  return nbio_write(sock, buf, bufSize);
}


static long
tcp_seek_null(void *handle, long offset, int whence)
{ return -1;
}


static int
tcp_close_input_handle(void *handle)
{ nbio_sock_t socket = fdFromHandle(handle);

  return nbio_close_input(socket);
}


static int
tcp_close_output_handle(void *handle)
{ nbio_sock_t socket = fdFromHandle(handle);

  return nbio_close_output(socket);
}


static int
tcp_control(void *handle, int action, void *arg)
{ nbio_sock_t socket = fdFromHandle(handle);

  switch(action)
  { case SIO_GETFILENO:
    { SOCKET fd = nbio_fd(socket);
      SOCKET *fdp = arg;
      *fdp = fd;
      return 0;
    }
    case SIO_LASTERROR:
    { const char *s;
      
      if ( (s=nbio_last_error(socket)) )
      { const char **sp = arg;
	*sp = s;
	return 0;
      }

      return -1;
    }
    case SIO_SETENCODING:
    case SIO_FLUSHOUTPUT:
      return 0;
    default:
      return -1;
  }
}


static IOFUNCTIONS readFunctions =
{ tcp_read_handle,
  tcp_write_handle,
  tcp_seek_null,
  tcp_close_input_handle,
  tcp_control
};


static IOFUNCTIONS writeFunctions =
{ tcp_read_handle,
  tcp_write_handle,
  tcp_seek_null,
  tcp_close_output_handle,
  tcp_control
};


static foreign_t
pl_open_socket(term_t Socket, term_t Read, term_t Write)
{ IOSTREAM *in, *out;
  int socket;
  void *handle;

  if ( !tcp_get_socket(Socket, &socket) )
    return FALSE;
  handle = (void *)(long)socket;
  
  in  = Snew(handle, SIO_INPUT|SIO_RECORDPOS|SIO_FBUF,  &readFunctions);
  in->encoding = ENC_OCTET;
  if ( !PL_open_stream(Read, in) )
    return FALSE;
  nbio_setopt(socket, TCP_INSTREAM, in);

  if ( !(nbio_get_flags(socket) & SOCK_LISTEN) )
  { out = Snew(handle, SIO_OUTPUT|SIO_RECORDPOS|SIO_FBUF, &writeFunctions);
    out->encoding = ENC_OCTET;
    if ( !PL_open_stream(Write, out) )
      return FALSE;
    nbio_setopt(socket, TCP_OUTSTREAM, out);
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
pl_close_socket(term_t socket)
{ int sock;

  if ( !tcp_get_socket(socket, &sock) )
    return FALSE;

  if ( nbio_closesocket(sock) < 0 )
    return nbio_error(errno, TCP_ERRNO);;

  return TRUE;
}
