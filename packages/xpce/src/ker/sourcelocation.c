/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#if !defined(HAVE_SYS_PARAM_H) || HAVE_SYS_PARAM_H
#include <sys/param.h>
#else
#ifndef MAXPATHLEN
#define MAXPATHLEN 256
#endif
#endif

status
initialiseSourceLocation(SourceLocation loc, Name file, Int line)
{ if ( isDefault(line) )
    line = (Int) NIL;

  assign(loc, file_name, file);
  assign(loc, line_no,   line);

  succeed;
}


static status
copySourceLocation(SourceLocation loc1, SourceLocation loc2)
{ assign(loc1, file_name, loc2->file_name);
  assign(loc1, line_no,   loc2->line_no);

  succeed;
}


static SourceLocation
getConvertSourceLocation(SourceLocation loc, Any spec)
{ if ( instanceOfObject(spec, ClassFile) )
  { FileObj f = spec;
    Name abs = get(f, NAME_absolutePath, 0);

    if ( abs )
      answer(newObject(ClassSourceLocation, abs, 0));
  } else /* if ( instanceOfObject(spec, ClassCharArray) */
  { char buf[MAXPATHLEN];
    int line;
    CharArray ca = spec;
    String s = &ca->data;

    if ( isstr8(s) )
    { if ( sscanf(s->s_text, "%[^: ]:%d", buf, &line) == 2 )
	answer(newObject(ClassSourceLocation, CtoName(buf), toInt(line), 0));
      else
	answer(newObject(ClassSourceLocation, spec, 0));
    }
  }

  fail;
}


static Name
getPathSourceLocation(SourceLocation loc)
{ char *name = strName(loc->file_name);

  if ( *name == '/' || *name == '.' )
    answer(loc->file_name);
  else
  { Name home;
    char buf[LINESIZE];

    TRY(home = get(PCE, NAME_home, 0));
    sprintf(buf, "%s/src/%s", strName(home), name);

    answer(CtoName(buf));
  }
}


status
makeClassSourceLocation(Class class)
{ sourceClass(class, makeClassSourceLocation, __FILE__, "$Revision$");

  localClass(class, NAME_fileName, NAME_location, "name", NAME_both,
	     "Name of the file in which the source resides");
  localClass(class, NAME_lineNo, NAME_location, "int*", NAME_both,
	     "Starting line number of the source");

  termClass(class, "source_location", 2, NAME_fileName, NAME_lineNo);

  sendMethod(class, NAME_initialise, DEFAULT, 2, "file=name", "line=[int]*",
	     "Create from file_name and line_no",
	     initialiseSourceLocation);

  sendMethod(class, NAME_copy, NAME_copy, 1, "source_location",
	     "Copy file_name and line_no from argument",
	     copySourceLocation);

  getMethod(class, NAME_path, NAME_path, "name", 0,
	    "Get absolute path-name of source file",
	    getPathSourceLocation);
  getMethod(class, NAME_convert, DEFAULT, "source_location",
	    1, "char_array|file", 
	    "Convert `file', `path-name' and `path-name:line-no'",
	    getConvertSourceLocation);

  succeed;
}

