/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include <md.h>				/* get HAVE_'s */

#if defined(HAVE_SOCKET) || defined(HAVE_WINSOCK) || defined(HAVE_FORK)

#ifdef HAVE_WINSOCK
#include "mswinsock.h"
#endif

#include <h/kernel.h>

#include <h/unix.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifndef FD_ZERO
#include <sys/select.h>
#endif

static status recordSeparatorStream(Stream s, Regex re);
static status closeStream(Stream s);

#define OsError() getOsErrorPce(PCE)

status
initialiseStream(Stream s, Int rfd, Int wfd, Code input, Regex sep)
{ s->rdfd = s->wrfd = -1;
  s->ws_ref = 0;
  s->input_buffer = NULL;
  s->input_allocated = s->input_p = 0;

  if ( isDefault(rfd) )   rfd = NIL;
  if ( isDefault(wfd) )   wfd = NIL;
  if ( isDefault(input) ) input = NIL;
  if ( isDefault(sep) )   sep = newObject(ClassRegex, CtoName("\n"), 0);

  if ( notNil(rfd) ) s->rdfd = valInt(rfd);
  if ( notNil(wfd) ) s->wrfd = valInt(wfd);

  assign(s, input_message, input);
  recordSeparatorStream(s, sep);

  succeed;
}


static status
unlinkStream(Stream s)
{ closeStream(s);

  succeed;
}

		 /*******************************
		 *	    OPEN/CLOSE		*
		 *******************************/


static status
closeStream(Stream s)
{ closeOutputStream(s);
  closeInputStream(s);

  ws_close_stream(s);

  succeed;
}


status
closeInputStream(Stream s)
{ DEBUG(NAME_stream, Cprintf("%s: Closing input\n", pp(s)));

  ws_close_input_stream(s);
  s->rdfd = -1;

  if ( s->input_buffer )
  { free(s->input_buffer);
    s->input_buffer = NULL;
  }

  succeed;
}


status
closeOutputStream(Stream s)
{ DEBUG(NAME_stream, Cprintf("Closing output\n"));

  ws_close_output_stream(s);
  s->wrfd = -1;

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

  if ( notNil(s->input_message) )
    ws_input_stream(s);

  succeed;
}


		 /*******************************
		 *        HANDLE INPUT		*
		 *******************************/


#define BLOCKSIZE 1024
#define ALLOCSIZE 256

#define Round(n, r) ((((n) + (r) - 1)/(r)) * (r))

void
add_data_stream(Stream s, char *data, int len)
{ char *q;

  if ( !s->input_buffer )
  { s->input_allocated = Round(len+1, ALLOCSIZE);
    s->input_buffer = malloc(s->input_allocated);
    s->input_p = 0;
  } else if ( s->input_p + len >= s->input_allocated )
  { s->input_allocated = Round(s->input_p + len + 1, ALLOCSIZE);
    s->input_buffer = realloc(s->input_buffer, s->input_allocated);
  }

  q = &s->input_buffer[s->input_p];
  strncpy(q, data, len);
  s->input_p += len;
}


status
handleInputStream(Stream s)
{ char buf[BLOCKSIZE+1];
  int n;

  if ( onFlag(s, F_FREED|F_FREEING) )
    fail;

  if ( (n = ws_read_stream_data(s, buf, BLOCKSIZE)) > 0 )
  { if ( isNil(s->record_separator) )
    { string q;
      Any str;
      AnswerMark mark;
      markAnswerStack(mark);

      q.size = n;
      q.b16 = FALSE;
      q.encoding = ENC_ASCII;
      q.s_text8 = buf;
      str = StringToString(&q);
      addCodeReference(s);
      forwardReceiverCodev(s->input_message, s, 1, &str);
      delCodeReference(s);

      rewindAnswerStack(mark, NIL);
    } else
    { add_data_stream(s, buf, n);

      DEBUG(NAME_stream,
	    { s->input_buffer[s->input_p] = EOS;
	      Cprintf("Read (%d chars): `%s'\n",
		      n,
		      &s->input_buffer[s->input_p-n]);
	    });

      while ( !onFlag(s, F_FREED|F_FREEING) && /* may drop out! */
	      search_regex(s->record_separator,
			   s->input_buffer, s->input_p,
			   NULL, 0, 0, s->input_p) )
      { Any str;
	string q;
	int size;
	AnswerMark mark;

	markAnswerStack(mark);
	size = valInt(getRegisterEndRegex(s->record_separator, ZERO));
	q.encoding = ENC_ASCII;
	q.b16 = FALSE;
	q.size = size;
	q.s_text8 = s->input_buffer;
	str = StringToString(&q);
	strncpy(s->input_buffer, &s->input_buffer[size], s->input_p - size);
	s->input_p -= size;

	DEBUG(NAME_stream, Cprintf("Sending: `%s'\n", strName(str)));
	addCodeReference(s);
	forwardReceiverCodev(s->input_message, s, 1, &str);
	delCodeReference(s);
	rewindAnswerStack(mark, NIL);
      }

      DEBUG(NAME_stream,
	    if ( s->input_p )
	    { s->input_buffer[s->input_p] = EOS;
	      Cprintf("Left in buffer: `%s'\n", s->input_buffer);
	    });
    }
  } else
  { DEBUG(NAME_stream,
	  if ( n < 0 )
	    Cprintf("Read failed: %s\n", strName(OsError())));
    closeStream(s);
    send(s, NAME_endOfFile, 0);
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

  TRY(swritefv(buf, fmt, argc, argv));

  return ws_write_stream_data(s, buf, strlen(buf));
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
getReadLineStream(Stream s, Int timeout)
{ return ws_read_line_stream(s, timeout);
}


static status
endOfFileStream(Stream s)
{ DEBUG(NAME_stream, Cprintf("Stream %s: end of output\n", pp(s)));

  succeed;
}


static status
recordSeparatorStream(Stream s, Regex re)
{ assign(s, record_separator, re);
  if ( notNil(re) )
    compileRegex(re, ON);

  succeed;
}


status
makeClassStream(Class class)
{ sourceClass(class, makeClassStream, __FILE__, "$Revision$");

  localClass(class, NAME_inputMessage, NAME_input, "code*", NAME_both,
	     "Forwarded on input from the stream");
  localClass(class, NAME_recordSeparator, NAME_input, "regex*", NAME_get,
	     "Regex that describes the record separator");
  localClass(class, NAME_wrfd, NAME_internal, "alien:int", NAME_none,
	     "File-handle to write to stream");
  localClass(class, NAME_rdfd, NAME_internal, "alien:int", NAME_none,
	     "File-handle to read from stream");
  localClass(class, NAME_rdstream, NAME_internal, "alien:FILE *", NAME_none,
	     "Stream used for <-read_line");
  localClass(class, NAME_wsRef, NAME_internal, "alien:WsRef", NAME_none,
	     "Window system synchronisation");
  localClass(class, NAME_inputBuffer, NAME_internal, "alien:char *", NAME_none,
	     "Buffer for collecting input-data");
  localClass(class, NAME_inputAllocated, NAME_internal, "alien:int", NAME_none,
	     "Allocated size of input_buffer");
  localClass(class, NAME_inputP, NAME_internal, "alien:int", NAME_none,
	     "Number of characters in input_buffer");

  termClass(class, "stream", 0);

  storeMethod(class, NAME_recordSeparator, recordSeparatorStream);

  sendMethod(class, NAME_initialise, DEFAULT, 4,
	     "rfd=[int]", "wfd=[int]",
	     "input_message=[code]", "record_separator=[regex]",
	     "Create stream",
	     initialiseStream);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Cleanup stream",
	     unlinkStream);

  sendMethod(class, NAME_closeInput, NAME_open, 0,
	     "Close input section of stream",
	     closeInputStream);
  sendMethod(class, NAME_closeOutput, NAME_open, 0,
	     "Close output section of stream",
	     closeOutputStream);
  sendMethod(class, NAME_input, NAME_open, 1, "fd=[int]*",
	     "Enable input from file-descriptor",
	     inputStream);

  sendMethod(class, NAME_append, NAME_output, 1, "data=char_array",
	     "Send data to stream",
	     appendStream);
  sendMethod(class, NAME_appendLine, NAME_output, 1, "data=char_array",
	     "->append and ->newline",
	     appendLineStream);
  sendMethod(class, NAME_newline, NAME_output, 0,
	     "Send a newline to the stream",
	     newlineStream);
  sendMethod(class, NAME_format, NAME_output, 2,
	     "format=char_array", "argument=any ...",
	     "Format arguments and send to stream",
	     formatStream);
  sendMethod(class, NAME_wait, NAME_control, 0,
	     "Wait for the complete output",
	     waitStream);
  sendMethod(class, NAME_endOfFile, NAME_input, 0,
	     "Send when end-of-file is reached",
	     endOfFileStream);

  getMethod(class, NAME_readLine, NAME_input, "string", 1, "[int]",
	    "Read line with optional timeout (milliseconds)",
	    getReadLineStream);

  succeed;
}

#else /*O_NO_PROCESS && O_NO_SOCKET*/

status
makeClassStream(Class class)
{ sourceClass(class, makeClassStream, __FILE__, "$Revision$");

  localClass(class, NAME_inputMessage, NAME_input, "code*", NAME_both,
	     "Forwarded on input from the stream");
  localClass(class, NAME_recordSeparator, NAME_input, "regex*", NAME_get,
	     "Regex that describes the record separator");
  localClass(class, NAME_wrfd, NAME_internal, "alien:int", NAME_none,
	     "File-handle to write to stream");
  localClass(class, NAME_rdfd, NAME_internal, "alien:int", NAME_none,
	     "File-handle to read from stream");
  localClass(class, NAME_rdstream, NAME_internal, "alien:FILE *", NAME_none,
	     "Stream used for <-read_line");
  localClass(class, NAME_inputBuffer, NAME_internal, "alien:char *", NAME_none,
	     "Buffer for collecting input-data");
  localClass(class, NAME_inputAllocated, NAME_internal, "alien:int", NAME_none,
	     "Allocated size of input_buffer");
  localClass(class, NAME_inputP, NAME_internal, "alien:int", NAME_none,
	     "Number of characters in input_buffer");
  localClass(class, NAME_wsRef, NAME_internal, "alien:WsRef", NAME_none,
	     "Window System synchronisation");

  termClass(class, "stream", 0);

  succeed;
}

#endif /*O_NO_PROCESS && O_NO_SOCKET*/

