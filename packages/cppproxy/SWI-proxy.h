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

#ifndef SWI_SERVER_H_INCLUDED
#define SWI_SERVER_H_INCLUDED

#include <SWI-Prolog.h>
#include <string>
#include <iostream>
#include <sstream>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>

using namespace std;

#define MAX_QUERY_NESTING 10

typedef enum
{ QSTAT_CLOSED,				/* no query active */
  QSTAT_OPEN,				/* Sent data */
  QSTAT_FAIL,				/* Query failed */
  QSTAT_TRUE,				/* Query succeeded deterministic */
  QSTAT_MORE,				/* Query succeeded non-det */
  QSTAT_EXCEPT,				/* Query raised exception */
  QSTAT_COMMERROR			/* Communication problem */
} qstatus;


		 /*******************************
		 *	     EXCEPTIONS		*
		 *******************************/

class PlException : public string
{ 
public:
  PlException() : string() {}
  PlException(string &s)
  { append(s);
  }
  PlException(const char *s)
  { append(s);
  }
};

class PlSocketException : public PlException
{
public:

  PlSocketException(const char *id, int errid)
  { append(id);
    if ( errid )
    { append(": ");
      append(strerror(errid));
    }
  }
};

class PlSerializationException : public PlException
{
public:
  PlSerializationException(int ex, int got)
  { ostringstream oss;

    oss << "Expected: " << ex << "got: " << got;
    append(oss.str());
  }
  PlSerializationException(const char *ex, string got)
  { ostringstream oss;

    oss << "Expected: " << ex << "got: " << got;
    append(oss.str());
  }
};


		 /*******************************
		 *	       QUERY		*
		 *******************************/

class PlQuery
{
};


		 /*******************************
		 *	   PROLOG PROXY		*
		 *******************************/

class PlProxy
{ iostream *ios;
  int debuglevel;
  qstatus qstack[MAX_QUERY_NESTING];
  int     qnest;

  void initvars()
  { ios = NULL; 
    debuglevel = 0;
    qnest = -1;				/* no query in progress */
  }

  void pushStatus(qstatus stat)
  { if ( ++qnest >= MAX_QUERY_NESTING )
      throw PlException("Query stack overflow");
    qstack[qnest] = stat;
  }
  void popStatus()
  { if ( qnest < 0 )
      throw PlException("Query stack underflow");
    qnest--;
  }
  void set_status(qstatus stat) { qstack[qnest] = stat; }

  qstatus readQueryReply();
  void flush();
					/* send primitives */
  void send(const char *name);
  void send(const char *name, size_t len);
  void send(long i);
  void send(int i) { send((long)i); }
  void send(size_t size) { send((long)size); }
  void send(double f);
					/* receive primitives */
  void receive(long &i);
  void receive(int &i) { long v; receive(v); i = (int)v; }
  void receive(double &f);
  void receive(string& s);
					/* expect data from the stream */
  void expect_chr(int c);
  void expect_int(int c);

#ifdef WIN32
  void startSocketLib();
  void stopSocketLib();
#endif

public:
  PlProxy()
  { initvars();
  }
  PlProxy(const char *host, int port)
  { initvars();
    openProlog(host, port);
  }
  ~PlProxy()
  { closeProlog();
  }

  void debug(const char *);
  int setdebug(int level) { int r = debuglevel; debuglevel=level; return r; }

  void openProlog(const char *host, int port);
  void closeProlog();

  qstatus get_status() { return qstack[qnest]; }

  void openQuery(const char *module, const char *predicate, int arity);
  void closeQuery();

  void runVoidQuery();			/* [one] */
  int  runDetQuery();			/* [zero_or_one] */
  int  runNonDetQuery();		/* [zero_or_more] */

					/* send real data */
  void send_int(long i);
  void send_float(double f);
  void send_atom(const char *s);
  void send_begin_term(const char *name, int arity);
  void send_end_term() {}

					/* receive real data */
  void receive_atom(string& s);
  void receive_int(long &i);
  void receive_float(double &f);
  void receive_begin_term(const char *name, int arity);
  void receive_end_term() {}
};


		 /*******************************
		 *	  SOCKET BUFFERING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class sockbuf is derived  from  the  C++   class  streambuf  do  do  the
buffering  for  raw  streams.  Unlike  many    other  such  libraries  I
deliberately decided not  to  encapsulates   sockets  entirely  into  an
object. Creating the socket, connecting, etc. is   all left to the user.
This part only does the buffering.

This code is derived from code   containing  the copyright notice below.
The code is split into two parts,  things are renamed, slightly modified
and comments and layout has been  changed.   Don't  blame  Chris for any
problems.

// Copyright 2005, Chris Frey.  To God be the glory.
// You are free to use, modify, redistribute, and sublicense this code,
// as long as this copyright message is not removed from the source,
// and as long as the existence of any changes are noted in the source
// as well.  (i.e. You don't need a complete history, just don't claim
// that the modified code was written entirely by me -- include your own
// copyright notice as well.)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

class sockbuf : public streambuf
{ int sock_fd;
  char_type *inbuf, *outbuf;
  size_t inbuf_size;
  size_t outbuf_size;

public:
  sockbuf(int fd, size_t size);
  ~sockbuf();

  virtual int overflow(int_type c);
  virtual int sync();
  virtual int underflow();
};


#endif /*SWI_SERVER_H_INCLUDED*/
