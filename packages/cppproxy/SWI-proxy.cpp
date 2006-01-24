/*  $Id$

    Part of SWI-Prolog C++ proxy package

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006, University of Amsterdam

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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "SWI-proxy.h"
#include <sys/types.h>
#include <string.h>
#include <stdio.h>


#ifdef WIN32

#include <io.h>
#include <winsock2.h>
typedef size_t socklen_t;

void
PlProxy::startSocketLib()
{ WSADATA WSAData;

  if ( WSAStartup(MAKEWORD(2,0), &WSAData) )
    throw(PlSocketException("WSAStartup", 0));
}

void
PlProxy::stopSocketLib()
{ WSACleanup();
}

#else /*WIN32*/

#include <sys/socket.h>
#include <sys/un.h>
#include <netdb.h>
#include <netinet/in.h>

#define closesocket(s) close(s)
#define startSocketLib()
#define stopSocketLib()

#endif /*WIN32*/


		 /*******************************
		 *	     OPEN/CLOSE		*
		 *******************************/

void
PlProxy::openProlog(const char *host, int port)
{ struct sockaddr_in a;
  struct sockaddr *addr = (struct sockaddr *) &a;
  struct hostent *hp;

  startSocketLib();

  memset(&a, 0, sizeof(a));
  a.sin_family = AF_INET;
  a.sin_port   = htons(port);

  if ( !(hp = gethostbyname(host)) )
    throw(PlSocketException("resolve hostname", errno));
  memcpy(&a.sin_addr, hp->h_addr, hp->h_length);

  if ( (sock = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
    throw(PlSocketException("create socket", errno));
  if ( connect(sock, addr, sizeof(a)) < 0 )
    throw(PlSocketException("connect", errno));

  if ( !(in = fdopen(sock, "r")) ||
       !(out = fdopen(sock, "w")) )
    throw(PlSocketException("create streams", errno));
}


void
PlProxy::closeProlog()
{ if ( in )
  { fclose(in);
    in = NULL;
  }
  if ( out )
  { fclose(out);
    out = NULL;
  }
  if ( sock >= 0 )
  { closesocket(sock);
    sock = -1;
  }

  stopSocketLib();
}


		 /*******************************
		 *	       DEBUG		*
		 *******************************/

void
PlProxy::debug(const char *s)
{ if ( debuglevel > 0 )
    cout << s << endl;
}


		 /*******************************
		 *	    SERVER QUERY	*
		 *******************************/

void
PlProxy::openQuery(const char *module, const char *predicate, int arity)
{ putc('q', out);
  send(module);
  send(predicate);
  send(arity);
  pushStatus(QSTAT_OPEN);
}


void
PlProxy::closeQuery()
{ if ( get_status() == QSTAT_MORE )
  { fputc('!', out);
    flush();
  }

  popStatus();
}


qstatus
PlProxy::readQueryReply()
{ flush();

  switch(int c = getc(in))
  { case 'f':				/* failure */
      debug("fail");
      set_status(QSTAT_FAIL);
      break;
    case 'l':				/* last (only) answer */
      debug("true (det)");
      set_status(QSTAT_TRUE);
      break;
    case 'm':				/* non-deterministic answer */
    { debug("true (nondet)");
      set_status(QSTAT_MORE);
      break;
    }
    case 'e':				/* Query execution error */
    { string s;
      debug("error");
      set_status(QSTAT_EXCEPT);

      receive_atom(s);
      closeQuery();
      throw(PlException(s.c_str()));
    }
    case 'E':				/* Communication/system error */
    { string s;
      debug("system error");
      set_status(QSTAT_COMMERROR);

      receive_atom(s);
      closeQuery();
      throw(PlException(s.c_str()));
    }
    default:
    { closeQuery();

      string s = "Unexpected query reply: ";
      s += c;
      throw(PlException(s.c_str()));
    }
  }

  return get_status();
}


int
PlProxy::runDetQuery()
{ qstatus stat = readQueryReply();

  closeQuery();

  switch(stat)
  { case QSTAT_FAIL:
      return FALSE;
    case QSTAT_TRUE:
    case QSTAT_MORE:
      return TRUE;
    default:
      throw(PlException("Internal error"));
  }
}


void
PlProxy::runVoidQuery()
{ if ( !runDetQuery() )
    throw(PlException("[one] query failed"));
}


int
PlProxy::runNonDetQuery()
{ switch(get_status())
  { case QSTAT_FAIL:
    case QSTAT_TRUE:
      return FALSE;
    case QSTAT_MORE:
      putc(';', out);
      break;
    default:
      break;
  }

  qstatus stat = readQueryReply();

  switch(stat)
  { case QSTAT_FAIL:
      return FALSE;
    case QSTAT_TRUE:
    case QSTAT_MORE:
      return TRUE;
    default:
      throw(PlException("Internal error"));
  }
}


		 /*******************************
		 *	    SEND TERMS		*
		 *******************************/

void
PlProxy::send_begin_term(const char *name, int arity)
{ putc('c', out);
  send(name);
  send(arity);
}

void
PlProxy::send_int(long i)
{ putc('i', out);
  send(i);
}

void
PlProxy::send_float(double f)
{ putc('f', out);
  send(f);
}

void
PlProxy::send_atom(const char *s)
{ putc('a', out);
  send(s);
}

void
PlProxy::flush()
{ if ( fflush(out) != 0 )
    throw(PlSocketException("write", errno));
}


		 /*******************************
		 *	    BYTE ORDER		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We deal with  the  byte-ordering  dynamically   to  avoid  the  need for
autoconf, complicating the re-use of this code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static const int double_byte_order_big_endian[]   = { 7,6,5,4,3,2,1,0 };
static const int double_byte_order_small_endian[] = { 0,1,2,3,4,5,6,7 };
static const int *double_byte_order = NULL;

static void
fix_byte_order()
{ if ( !double_byte_order )
  { int v = 0x0000ffff;
    char *c = (char *)&v;

    if ( *c == 0 )
      double_byte_order = double_byte_order_big_endian;
    else
      double_byte_order = double_byte_order_small_endian;
  }
}


		 /*******************************
		 *	       SEND		*
		 *******************************/

void
PlProxy::send(const char *name)
{ send(name, strlen(name));
}


void
PlProxy::send(const char *name, size_t len)
{ send(len);
  if ( ::fwrite(name, 1, len, out) != len )
    throw(PlSocketException("send", errno));
}


void
PlProxy::send(long val)
{ char buf[4];

  if ( debuglevel > 0 )
    cerr << "send: " << val << endl;

  buf[0] = val>>24&0xff;
  buf[1] = val>>16&0xff;
  buf[2] = val>>8 &0xff;
  buf[3] = val	  &0xff;

  if ( ::fwrite(buf, 1, 4, out) != 4 )
    throw(PlSocketException("send", errno));
}


void
PlProxy::send(double f)
{ unsigned char *cl = (unsigned char *)&f;
  unsigned long i;

  fix_byte_order();
  for(i=0; i<sizeof(double); i++)
  { if ( putc(cl[double_byte_order[i]], out) < 0 )
      throw(PlSocketException("send", errno));
  }
}



		 /*******************************
		 *	      RECEIVE		*
		 *******************************/

void
PlProxy::receive(long &v)
{ long val = 0;
  
  for(int i=0; i<4; i++)
  { int c = getc(in);

    if ( c == EOF )
      throw(PlSocketException("receive int", errno));
    val <<= 8;
    val |= c&0xff;
  }

  v = val;
}


void
PlProxy::receive(string &s)
{ long len;
  char buffer[512];
  char *buf;

  receive(len);
  if ( len < sizeof(buffer)-1 )

    buf = buffer;
  else if ( !(buf = (char *)malloc(len+1)) )
    throw(PlException("no memory"));

  if ( fread(buf, 1, len, in) != (size_t)len )
  { if ( buf != buffer )
      free(buf);
    throw(PlSocketException("receive atom", errno));
  }
  buf[len] = '\0';
  
  s = buf;
  if ( buf != buffer )
    free(buf);
}


void
PlProxy::receive(double &val)
{ double f;
  unsigned char *cl = (unsigned char *)&f;
  unsigned int i;

  fix_byte_order();
  for(i=0; i<sizeof(double); i++)
  { int c = getc(in);
    
    if ( c == -1 )
      throw(PlSocketException("receive float", errno));
    cl[double_byte_order[i]] = c;
  }

  val = f;
}


void
PlProxy::expect_chr(int c)
{ int got = getc(in);

  if ( got != c )
    throw(PlSerializationException(c, got));
}

void
PlProxy::expect_int(int c)
{ int got;

  receive(got);

  if ( got != c )
    throw(PlSerializationException(c, got));
}

void
PlProxy::receive_begin_term(const char *name, int arity)
{ string s;
  expect_chr('c');
  receive(s);
  if ( s != name )
    throw(PlSerializationException(name, s));
  expect_int(arity);
}


void
PlProxy::receive_int(long &v)
{ expect_chr('i');
  receive(v);
}


void
PlProxy::receive_float(double &f)
{ expect_chr('f');
  receive(f);
}


void
PlProxy::receive_atom(string &s)
{ expect_chr('a');

  receive(s);
}

