/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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

#include <md.h>				/* get HAVE_'s */

#if defined(HAVE_SOCKET) || defined(HAVE_WINSOCK) || defined(HAVE_FORK)

#ifdef HAVE_WINSOCK
#include "mswinsock.h"
#define StreamError() SockError()
#else
#define StreamError() OsError()
#endif

#include <h/kernel.h>

#include <h/unix.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

static status recordSeparatorStream(Stream s, Any sep);

#define OsError() getOsErrorPce(PCE)

status
initialiseStream(Stream s, Int rfd, Int wfd, Code input, Any sep)
{ s->rdfd = s->wrfd = -1;
  s->ws_ref = 0;
  s->input_buffer = NULL;
  s->input_allocated = s->input_p = 0;

  if ( isDefault(rfd) )   rfd = NIL;
  if ( isDefault(wfd) )   wfd = NIL;
  if ( isDefault(input) ) input = NIL;
  if ( isDefault(sep) )   sep = newObject(ClassRegex, CtoName("\n"), EAV);

  if ( notNil(rfd) ) s->rdfd = valInt(rfd);
  if ( notNil(wfd) ) s->wrfd = valInt(wfd);

  assign(s, input_message, input);
  recordSeparatorStream(s, sep);

  succeed;
}


static status
unlinkStream(Stream s)
{ return closeStream(s);
}

		 /*******************************
		 *	    OPEN/CLOSE		*
		 *******************************/


status
closeStream(Stream s)
{ closeOutputStream(s);
  closeInputStream(s);

  ws_close_stream(s);

  succeed;
}


status
closeInputStream(Stream s)
{ if ( s->rdfd >= 0 )
  { DEBUG(NAME_stream, Cprintf("%s: Closing input\n", pp(s)));

    ws_close_input_stream(s);
    s->rdfd = -1;

    if ( s->input_buffer )
    { pceFree(s->input_buffer);
      s->input_buffer = NULL;
    }
  }

  succeed;
}


status
closeOutputStream(Stream s)
{ if ( s->wrfd >= 0 )
  { DEBUG(NAME_stream, Cprintf("%s: Closing output\n", pp(s)));

    ws_close_output_stream(s);
    s->wrfd = -1;
  }

  succeed;
}


status
inputStream(Stream s, Int fd)
{ if ( notDefault(fd) )
  { if ( isNil(fd) )
      closeInputStream(s);
    else
      s->rdfd = valInt(fd);		/* Unix only! */
  }

/*if ( notNil(s->input_message) )*/
    ws_input_stream(s);

  succeed;
}


		 /*******************************
		 *        HANDLE INPUT		*
		 *******************************/


#define BLOCKSIZE 1024
#define ALLOCSIZE 1024

#define Round(n, r) (((n) + (r) - 1) & ~((r)-1))

void
add_data_stream(Stream s, char *data, int len)
{ char *q;

  if ( !s->input_buffer )
  { s->input_allocated = Round(len+1, ALLOCSIZE);
    s->input_buffer = pceMalloc(s->input_allocated);
    s->input_p = 0;
  } else if ( s->input_p + len >= s->input_allocated )
  { s->input_allocated = Round(s->input_p + len + 1, ALLOCSIZE);
    s->input_buffer = pceRealloc(s->input_buffer, s->input_allocated);
  }

  q = (char *)&s->input_buffer[s->input_p];
  memcpy(q, data, len);
  s->input_p += len;
}


static void
write_byte(int byte)
{ if ( byte < 32 || (byte >= 127 && byte < 128+32) || byte == 255 )
  { char buf[10];
    char *prt = buf;

    switch(byte)
    { case '\t':
	prt = "\\t";
        break;
      case '\r':
	prt = "\\r";
	break;
      case '\n':
	prt = "\\n";
	break;
      case '\b':
	prt = "\\b";
	break;
      default:
	sprintf(buf, "<%d>", byte);
    }

    Cprintf("%s", prt);
  } else
    Cputchar(byte);
}


static void
write_buffer(char *buf, int size)
{ if ( size > 50 )
  { write_buffer(buf, 25);
    Cprintf(" ... ");
    write_buffer(buf + size - 25, 25);
  } else
  { int n;

    for(n=0; n<size; n++)
    { write_byte(buf[n]);
    }
  }
}


static void
dispatch_stream(Stream s, int size)
{ string q;
  AnswerMark mark;
  Any str;

  markAnswerStack(mark);
  str_inithdr(&q, ENC_ASCII);
  q.size = size;
  q.s_text8 = s->input_buffer;
  str = StringToString(&q);
  memcpy((char *)s->input_buffer,
	  (char *)&s->input_buffer[size],
	  s->input_p - size);
  s->input_p -= size;

  DEBUG(NAME_stream,
	{ int n = valInt(getSizeCharArray(str));

	  Cprintf("Sending: %d characters, `", n);
	  write_buffer(strName(str), n);
	  Cprintf("'\n\tLeft: %d characters, `", s->input_p);
	  write_buffer(s->input_buffer, s->input_p);
	  Cprintf("'\n");
	});
  
  if ( notNil(s->input_message) )
  { addCodeReference(s);
    forwardReceiverCodev(s->input_message, s, 1, &str);
    delCodeReference(s);
  }

  rewindAnswerStack(mark, NIL);
}


static void
dispatch_input_stream(Stream s)
{ while( !onFlag(s, F_FREED|F_FREEING) && s->input_buffer && s->input_p > 0 )
  { if ( isNil(s->record_separator) )
    { dispatch_stream(s, s->input_p);
      if ( s->input_buffer )
      { pceFree(s->input_buffer);
	s->input_buffer = NULL;
      }

      return;
    }

    if ( isInteger(s->record_separator) )
    { int bsize = valInt(s->record_separator);

      if ( bsize <= s->input_p )
      {	dispatch_stream(s, bsize);
	continue;
      }

      return;
    }

    if ( search_regex(s->record_separator,
		      (char *)s->input_buffer, s->input_p,
		      NULL, 0, 0, s->input_p) )
    { int size = valInt(getRegisterEndRegex(s->record_separator, ZERO));

      dispatch_stream(s, size);
      continue;
    }

    return;
  }
}



status
handleInputStream(Stream s)
{ char buf[BLOCKSIZE];
  int n;

  if ( onFlag(s, F_FREED|F_FREEING) )
    fail;

  if ( (n = ws_read_stream_data(s, buf, BLOCKSIZE, DEFAULT)) > 0 )
  { if ( isNil(s->input_message) )	/* modal */
      add_data_stream(s, buf, n);
    else if ( isNil(s->record_separator) && !s->input_buffer )
    { string q;
      Any str;
      AnswerMark mark;
      markAnswerStack(mark);

      DEBUG(NAME_stream,
	    { Cprintf("Read (%d chars, unbuffered): `", n);
	      write_buffer(buf, n);
	      Cprintf("'\n");
	    });

      str_inithdr(&q, ENC_ASCII);
      q.size = n;
      q.s_text8 = (unsigned char *)buf;
      str = StringToString(&q);
      addCodeReference(s);
      forwardReceiverCodev(s->input_message, s, 1, &str);
      delCodeReference(s);

      rewindAnswerStack(mark, NIL);
    } else
    { add_data_stream(s, buf, n);

      DEBUG(NAME_stream,
	    { Cprintf("Read (%d chars): `", n);
	      write_buffer(&s->input_buffer[s->input_p-n], n);
	      Cprintf("'\n");
	    });

      dispatch_input_stream(s);
    }
  } else
  { 
    DEBUG(NAME_stream,
	  if ( n < 0 )
	    Cprintf("Read failed: %s\n", strName(StreamError()));
	  else
	    Cprintf("%s: Got 0 characters: EOF\n", pp(s));
	 );
    send(s, NAME_closeInput, EAV);
    send(s, NAME_endOfFile, EAV);
  }

  succeed;
}


		 /*******************************
		 *       OUTPUT HANDLING	*
		 *******************************/

static status
appendStream(Stream s, CharArray data)
{ String str = &data->data;
  int l = str_datasize(str);

  return ws_write_stream_data(s, str->s_text, l);
}


static status
newlineStream(Stream s)
{ static char nl[] = "\n";

  return ws_write_stream_data(s, nl, 1);
}


static status
appendLineStream(Stream s, CharArray data)
{ if ( !appendStream(s, data) ||
       !newlineStream(s) )
    fail;

  succeed;
}


static status
formatStream(Stream s, CharArray fmt, int argc, Any *argv)
{ char buf[FORMATSIZE];
  int sz = sizeof(buf);

  swritefv(buf, &sz, fmt, argc, argv);
  if ( sz > FORMATSIZE-1 )
  { int len = ++sz;			/* 1 extra for EOS */
    char *abuf = alloc(len);
    status rval;

    swritefv(abuf, &sz, fmt, argc, argv);
    rval = ws_write_stream_data(s, abuf, sz);
    unalloc(len, abuf);

    return rval;
  } else
    return ws_write_stream_data(s, buf, sz);
}


static status
waitStream(Stream s)
{ while( s->rdfd >= 0 )
    dispatchDisplayManager(TheDisplayManager(), DEFAULT, DEFAULT);

  succeed;
}

		 /*******************************
		 *	  INPUT HANDLING	*
		 *******************************/

static StringObj
getReadLineStream(Stream s, Real timeout)
{ while(s->rdfd >= 0)
  { if ( s->input_buffer )
    { unsigned char *q;
      int n;

      DEBUG(NAME_stream, Cprintf("Scanning %d chars\n", s->input_p));
      for(n=s->input_p, q = s->input_buffer; n > 0; n--, q++)
      { if ( *q == '\n' )
	{ string str;
	  int len = (q-s->input_buffer)+1;
	  StringObj rval;
	  
	  str_set_n_ascii(&str, len, s->input_buffer);
	  rval = StringToString(&str);
	  strncpy(s->input_buffer, &s->input_buffer[len], s->input_p - len);
	  s->input_p -= len;

	  return rval;
	}
      }
      DEBUG(NAME_stream, Cprintf("No newline, reading\n"));
    }

    if ( !ws_dispatch(DEFAULT, timeout) )
      return (StringObj) NIL;
  }    

  fail;
}


static status
endOfFileStream(Stream s)
{ DEBUG(NAME_stream, Cprintf("Stream %s: end of output\n", pp(s)));

  succeed;
}


static status
recordSeparatorStream(Stream s, Any re)
{ if ( s->record_separator != re )
  { assign(s, record_separator, re);

    if ( instanceOfObject(re, ClassRegex) )
      compileRegex(re, ON);

    dispatch_input_stream(s);		/* handle possible pending data */
  }

  succeed;
}


static status
inputMessageStream(Stream s, Code msg)
{ if ( s->input_message != msg )
  { Code old = s->input_message;
    
    assign(s, input_message, msg);
    if ( isNil(old) && notNil(msg) )
    { ws_input_stream(s);
    } else if ( notNil(old) && isNil(msg) )
    { ws_no_input_stream(s);
    }
  }

  succeed;
}


		 /*******************************
		 *	      AS FILE		*
		 *******************************/

static status
writeAsFileStream(Stream s, Int where, CharArray txt)
{ if ( notDefault(where) )
    return errorPce(s, NAME_cannotSeekNonFile);

  return appendStream(s, txt);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_format[] =
        { "format=char_array", "argument=any ..." };
static char *T_initialise[] =
        { "rfd=[int]", "wfd=[int]",
	  "input_message=[code]", "record_separator=[regex|int]" };
static char *T_writeAsFile[] =
        { "at=[int]", "text=char_array" };

/* Instance Variables */

#define var_stream XPCE_var_stream	/* AIX 3.2.5 conflict */

static vardecl var_stream[] =
{ SV(NAME_inputMessage, "code*", IV_GET|IV_STORE,
     inputMessageStream,
     NAME_input, "Forwarded on input from the stream"),
  SV(NAME_recordSeparator, "regex|int*", IV_GET|IV_STORE,
     recordSeparatorStream,
     NAME_input, "Regex that describes the record separator"),
  IV(NAME_wrfd, "alien:int", IV_NONE,
     NAME_internal, "File-handle to write to stream"),
  IV(NAME_rdfd, "alien:int", IV_NONE,
     NAME_internal, "File-handle to read from stream"),
  IV(NAME_rdstream, "alien:FILE *", IV_NONE,
     NAME_internal, "Stream used for <-read_line"),
  IV(NAME_wsRef, "alien:WsRef", IV_NONE,
     NAME_internal, "Window system synchronisation"),
  IV(NAME_inputBuffer, "alien:char *", IV_NONE,
     NAME_internal, "Buffer for collecting input-data"),
  IV(NAME_inputAllocated, "alien:int", IV_NONE,
     NAME_internal, "Allocated size of input_buffer"),
  IV(NAME_inputP, "alien:int", IV_NONE,
     NAME_internal, "Number of characters in input_buffer")
};

/* Send Methods */

static senddecl send_stream[] =
{ SM(NAME_initialise, 4, T_initialise, initialiseStream,
     DEFAULT, "Create stream"),
  SM(NAME_unlink, 0, NULL, unlinkStream,
     DEFAULT, "Cleanup stream"),
  SM(NAME_wait, 0, NULL, waitStream,
     NAME_control, "Wait for the complete output"),
  SM(NAME_endOfFile, 0, NULL, endOfFileStream,
     NAME_input, "Send when end-of-file is reached"),
  SM(NAME_closeInput, 0, NULL, closeInputStream,
     NAME_open, "Close input section of stream"),
  SM(NAME_closeOutput, 0, NULL, closeOutputStream,
     NAME_open, "Close output section of stream"),
  SM(NAME_input, 1, "fd=[int]*", inputStream,
     NAME_open, "Enable input from file-descriptor"),
  SM(NAME_append, 1, "data=char_array", appendStream,
     NAME_output, "Send data to stream"),
  SM(NAME_appendLine, 1, "data=char_array", appendLineStream,
     NAME_output, "->append and ->newline"),
  SM(NAME_format, 2, T_format, formatStream,
     NAME_output, "Format arguments and send to stream"),
  SM(NAME_newline, 0, NULL, newlineStream,
     NAME_output, "Send a newline to the stream"),
  SM(NAME_writeAsFile, 2, T_writeAsFile, writeAsFileStream,
     NAME_stream, "Allow pce_open(Socket, append, Stream)")
};

/* Get Methods */

static getdecl get_stream[] =
{ GM(NAME_readLine, 1, "string*", "timeout=[real]", getReadLineStream,
     NAME_input, "Read line with optional timeout (seconds)")
};

/* Resources */

#define rc_stream NULL
/*
static classvardecl rc_stream[] =
{ 
};
*/

/* Class Declaration */

ClassDecl(stream_decls,
          var_stream, send_stream, get_stream, rc_stream,
          0, NULL,
          "$Rev$");

status
makeClassStream(Class class)
{ return declareClass(class, &stream_decls);
}

#else /*O_NO_PROCESS && O_NO_SOCKET*/

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static vardecl var_stream[] =
{ IV(NAME_inputMessage, "code*", IV_BOTH,
     NAME_input, "Forwarded on input from the stream"),
  IV(NAME_recordSeparator, "regex|int*", IV_GET,
     NAME_input, "Regex that describes the record separator"),
  IV(NAME_wrfd, "alien:int", IV_NONE,
     NAME_internal, "File-handle to write to stream"),
  IV(NAME_rdfd, "alien:int", IV_NONE,
     NAME_internal, "File-handle to read from stream"),
  IV(NAME_rdstream, "alien:FILE *", IV_NONE,
     NAME_internal, "Stream used for <-read_line"),
  IV(NAME_inputBuffer, "alien:char *", IV_NONE,
     NAME_internal, "Buffer for collecting input-data"),
  IV(NAME_inputAllocated, "alien:int", IV_NONE,
     NAME_internal, "Allocated size of input_buffer"),
  IV(NAME_inputP, "alien:int", IV_NONE,
     NAME_internal, "Number of characters in input_buffer"),
  IV(NAME_wsRef, "alien:WsRef", IV_NONE,
     NAME_internal, "Window System synchronisation")
};

/* Send Methods */

#define send_stream NULL
/*
static senddecl send_stream[] =
{ 
};
*/

/* Get Methods */

#define get_stream NULL
/*
static getdecl get_stream[] =
{ 
};
*/

/* Resources */

#define rc_stream NULL
/*
static classvardecl rc_stream[] =
{ 
};
*/

/* Class Declaration */

ClassDecl(stream_decls,
          var_stream, send_stream, get_stream, rc_stream,
          0, NULL,
          "$Rev$");

status
makeClassStream(Class class)
{ return declareClass(class, &stream_decls);
}

#endif /*O_NO_PROCESS && O_NO_SOCKET*/

