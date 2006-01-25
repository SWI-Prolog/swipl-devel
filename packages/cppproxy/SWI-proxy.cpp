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
#include <assert.h>


#ifdef WIN32

#include <io.h>
#include <winsock2.h>
typedef size_t socklen_t;
typedef unsigned int ssize_t;

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

  int sock;

  if ( (sock = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
    throw(PlSocketException("create socket", errno));
  if ( connect(sock, addr, sizeof(a)) < 0 )
    throw(PlSocketException("connect", errno));

  sockbuf *nb = new sockbuf(sock, 1024);
  ios = new iostream(nb);
}


void
PlProxy::closeProlog()
{ if ( ios )
  { delete ios;
    ios = NULL;
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
{ ios->put('q');
  send(module);
  send(predicate);
  send(arity);
  pushStatus(QSTAT_OPEN);
}


void
PlProxy::closeQuery()
{ if ( get_status() == QSTAT_MORE )
  { ios->put('!');
    flush();
  }

  popStatus();
}


qstatus
PlProxy::readQueryReply()
{ flush();

  switch(int c = ios->get())
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
      ios->put(';');
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
{ ios->put('c');
  send(name);
  send(arity);
}

void
PlProxy::send_int(long i)
{ ios->put('i');
  send(i);
}

void
PlProxy::send_float(double f)
{ ios->put('f');
  send(f);
}

void
PlProxy::send_atom(const char *s)
{ ios->put('a');
  send(s);
}

void
PlProxy::flush()
{ ios->flush();				/* TBD: error handling? */
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
  ios->write(name, len);		/* TBD: error handling */
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

  ios->write(buf, 4);			/* TBD: error handling */
}


void
PlProxy::send(double f)
{ unsigned char *cl = (unsigned char *)&f;
  unsigned long i;

  fix_byte_order();
  for(i=0; i<sizeof(double); i++)
  { ios->put(cl[double_byte_order[i]]);	/* TBD: error handling */
  }
}



		 /*******************************
		 *	      RECEIVE		*
		 *******************************/

void
PlProxy::receive(long &v)
{ long val = 0;
  
  for(int i=0; i<4; i++)
  { int c = ios->get();

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

  ios->read(buf, len);			/* TBD: error handling and free */
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
  { int c = ios->get();
    
    if ( c == -1 )
      throw(PlSocketException("receive float", errno));
    cl[double_byte_order[i]] = c;
  }

  val = f;
}


void
PlProxy::expect_chr(int c)
{ int got = ios->get();

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


		 /*******************************
		 *	  SOCKET BUFFER		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
See copyright claims in SWI-proxy.h with the header of this class.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


sockbuf::sockbuf(int fd, size_t size) : sock_fd(fd)
{ inbuf = new char_type[size];
  inbuf_size = size;
  setg(inbuf, inbuf, inbuf);

  outbuf = new char_type[size + 1];
  outbuf_size = size;
  setp(outbuf, outbuf + outbuf_size);
}

sockbuf::~sockbuf()
{ sync();
  close(sock_fd);
  delete [] inbuf;
  delete [] outbuf;
}


int
sockbuf::overflow(int_type c)
{ bool have_extra = (c != traits_type::eof());
  ssize_t count = pptr() - pbase();

  if( have_extra )
  { *(pptr()) = traits_type::to_char_type(c);
    count++;
  }

  if( count > 0 )
  { ssize_t written;
    char_type *wp = pbase();

    do
    { written = ::send(sock_fd, wp, count, 0);

      if( written > 0 )
      { count -= written;
	assert(count >= 0);
	wp += written;
      } else
      { return traits_type::eof();
      }
    } while(count);
    
    setp(outbuf, outbuf + outbuf_size);
  }

  return traits_type::not_eof(c);
}


int
sockbuf::sync()
{ if ( overflow(traits_type::eof()) == traits_type::eof() )
      return -1;
  return 0;
} 


int
sockbuf::underflow()
{ int_type ret = traits_type::eof();
  ssize_t count;

  if( (count = ::recv(sock_fd, inbuf, inbuf_size, 0)) > 0 )
  { setg(inbuf, inbuf, inbuf + count);

    ret = traits_type::to_int_type(*inbuf);
  }

  return ret;
}
