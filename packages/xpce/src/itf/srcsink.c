/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

status
initialiseSourceSink(SourceSink ss)
{ succeed;
}


static SourceSink
getConvertSourceSink(Class class, Name name)
{ answer(answerObject(ClassFile, name, EAV));
}

		 /*******************************
		 *	  GENERIC ACTIONS	*
		 *******************************/

static StringObj
getContentsSourceSink(SourceSink ss, Int from, Int len)
{ IOSTREAM *fd = Sopen_object(ss, "rbr");

  if ( fd )
  { long size = Ssize(fd);
    string s;
    status ok;

    if ( isDefault(from) )
      from = ZERO;

    if ( from != ZERO )
    { long pos = Sseek(fd, valInt(from), SIO_SEEK_SET);

      if ( pos != -1 )
	size -= pos;
    }

    if ( notDefault(len) )
      size = min(valInt(len), size);

    str_inithdr(&s, ENC_ASCII);
    s.size = size;
    str_alloc(&s);

    Sfread(s.s_text8, sizeof(char), size, fd);
    ok = checkErrorSourceSink(ss, fd);
    Sclose(fd);

    if ( ok )
    { StringObj str = answerObject(ClassString, EAV);

      str_unalloc(&str->data);
      str->data = s;

      answer(str);
    }
  }

  fail;
}


		 /*******************************
		 *	       UTIL		*
		 *******************************/

status
checkErrorSourceSink(SourceSink ss, IOSTREAM *fd)
{ if ( Sferror(fd) )
    return errorPce(ss, NAME_ioError, getOsErrorPce(PCE));

  succeed;
}

		/********************************
		*     OBJECT LOADING SAVING	*
		********************************/

static status
checkObjectSourceSink(SourceSink ss)
{ IOSTREAM *fd;

  if ( (fd = Sopen_object(ss, "rbr")) )
  { status rval = checkObjectMagic(fd);

    Sclose(fd);

    return rval;
  }

  fail;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_writeAsFile[] =
        { "at=[int]", "text=char_array" };
static char *T_readAsFile[] =
        { "from=int", "size=int" };
static char *T_contents[] =
        { "from=[int]", "size=[int]" };

/* Instance Variables */

#define var_source_sink NULL
/*
static vardecl var_source_sink[] =
{ 
};
*/

/* Send Methods */

static senddecl send_source_sink[] =
{ SM(NAME_initialise, 0, NULL, initialiseSourceSink,
     DEFAULT, "Initialise abstract class"),
  SM(NAME_truncateAsFile, 0, NULL, virtualObject,
     NAME_stream, "Truncate object (virtual method)"),
  SM(NAME_writeAsFile, 2, T_writeAsFile, virtualObject2,
     NAME_stream, "Write data to object (virtual method)"),
  SM(NAME_access, 1, "mode={read,write,append,execute}", virtualObject1,
     NAME_test, "Test if resource has access (virtual method)"),
  SM(NAME_checkObject, 0, NULL, checkObjectSourceSink,
     NAME_file, "Test if file contains a saved PCE object")
};

/* Get Methods */

static getdecl get_source_sink[] =
{ GM(NAME_convert, 1, "file", "path=name", getConvertSourceSink,
     DEFAULT, "Convert name to file"),
  GM(NAME_readAsFile, 2, "string", T_readAsFile, getVirtualObject2,
     NAME_stream, "Read data from object (virtual method)"),
  GM(NAME_sizeAsFile, 0, "characters=int", NULL, getVirtualObject,
     NAME_stream, "Get current size (virtual method)"),
  GM(NAME_contents, 2, "string", T_contents, getContentsSourceSink,
     NAME_read, "New string holding text (from, length)"),
  GM(NAME_object, 0, "object=any|function", NULL, getObjectSourceSink,
     NAME_file, "Reload object created with ->save_in_file")
};

/* Resources */

#define rc_source_sink NULL
/*
static classvardecl rc_source_sink[] =
{ 
};
*/

/* Class Declaration */

ClassDecl(source_sink_decls,
          var_source_sink, send_source_sink, get_source_sink, rc_source_sink,
          0, NULL,
          "$Rev$");


status
makeClassSourceSink(Class class)
{ return declareClass(class, &source_sink_decls);
}

