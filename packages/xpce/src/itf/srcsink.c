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

#include <h/kernel.h>

static Name encoding_to_name(IOENC enc);

status
initialiseSourceSink(SourceSink ss)
{ PceCValue val;

  if ( hostQuery(HOST_ENCODING, &val) )
  { Name en;
    IOENC enc;
  
    enc = (IOENC)val.integer;
    if ( (en = encoding_to_name(enc)) )
      assign(ss, encoding, en);
  }

  succeed;
}


static SourceSink
getConvertSourceSink(Class class, Name name)
{ answer(answerObject(ClassFile, name, EAV));
}


static status
encodingSourceSink(SourceSink ss, Name encoding)
{ ss->encoding = encoding;

  succeed;
}


typedef struct encname
{ Name  name;
  IOENC code;
} encname;


static const encname enc_names[] = 
{ { NAME_octet,		ENC_OCTET },
  { NAME_ascii,		ENC_ASCII },
  { NAME_iso_latin_1,   ENC_ISO_LATIN_1 },
  { NAME_text,   	ENC_ANSI },
  { NAME_utf8,		ENC_UTF8 },
  { NAME_unicodeBe,	ENC_UNICODE_BE },
  { NAME_unicodeLe,	ENC_UNICODE_LE },
  { NAME_wchar,		ENC_WCHAR },
  { 0, 0 }
};


static Name
encoding_to_name(IOENC enc)
{ const encname *en;

  for(en=enc_names; en->name; en++)
  { if ( en->code == enc )
    { return en->name;
    }
  }

  return NIL;
}



status
setStreamEncodingSourceSink(SourceSink ss, IOSTREAM *fd)
{ const encname *en;

  for(en=enc_names; en->name; en++)
  { if ( ss->encoding == en->name )
    { fd->encoding = en->code;
      succeed;
    }
  }

  return errorPce(ss, NAME_unknownEncoding, ss->encoding);
}



		 /*******************************
		 *	  GENERIC ACTIONS	*
		 *******************************/

static StringObj
getContentsSourceSink(SourceSink ss, Int from, Int len)
{ const char *mode = (ss->encoding == NAME_octet ? "rbr" : "rr");
  IOSTREAM *fd = Sopen_object(ss, mode);

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

    if ( size > STR_MAX_SIZE )
    { errorPce(ss, NAME_stringTooLong, toInt(size));
      fail;
    }

    if ( ss->encoding == NAME_octet ||
	 ss->encoding == NAME_iso_latin_1 )
    { str_inithdr(&s, ENC_ISOL1);
      s.size = size;
      str_alloc(&s);

      Sfread(s.s_textA, sizeof(char), size, fd);
      ok = checkErrorSourceSink(ss, fd);
      Sclose(fd);

      if ( ok )
      { StringObj str = answerObject(ClassString, EAV);
	
	str_unalloc(&str->data);
	str->data = s;

	answer(str);
      } else
      { str_unalloc(&s);
	fail;
      }
    } else
    { int c;
      long n = 0;

      str_inithdr(&s, ENC_ISOL1);
      s.size = 256;
      s.s_textA = pceMalloc(s.size);

      setStreamEncodingSourceSink(ss, fd);

      while( n < valInt(len) && (c=Sgetcode(fd)) != EOF )
      { if ( c > 0xff && !s.iswide )
	{ charW *w = pceMalloc(s.size*sizeof(charW));
	  charW *t = w;
	  const charA *f = s.s_textA;
	  const charA *e = &f[n];

	  while(f<e)
	    *t++ = *f++;

	  pceFree(s.s_textA);
	  s.s_textW = w;
	  s.iswide = TRUE;
	}
	if ( n >= s.size )
	{ s.size *= 2;

	  if ( isstrA(&s) )
	    s.s_textA = pceRealloc(s.s_textA, s.size);
	  else
	    s.s_textA = pceRealloc(s.s_textW, s.size*sizeof(charW));
	}
	if ( isstrA(&s) )
	  s.s_textA[n++] = c;
	else
	  s.s_textW[n++] = c;
      }

      ok = checkErrorSourceSink(ss, fd);
      Sclose(fd);
      s.size = n;

      if ( ok )
      { StringObj str = StringToString(&s);

	pceFree(s.s_textA);
	answer(str);
      } else
      { pceFree(s.s_textA);
	fail;
      }
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

static vardecl var_source_sink[] =
{ SV(NAME_encoding, "{octet,ascii,iso_latin_1,text,utf8,unicode_be,unicode_le,wchar}",
     IV_GET|IV_STORE, encodingSourceSink,
     NAME_encoding, "Encoding of the data-source"),
};

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

static classvardecl rc_source_sink[] =
{ RC(NAME_encoding, NULL, "iso_latin_1", NULL)
};

/* Class Declaration */

ClassDecl(source_sink_decls,
          var_source_sink, send_source_sink, get_source_sink, rc_source_sink,
          0, NULL,
          "$Rev$");


status
makeClassSourceSink(Class class)
{ return declareClass(class, &source_sink_decls);
}

