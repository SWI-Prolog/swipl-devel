/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#ifdef HAVE_SYS_PARAM_H
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

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "file=name", "line=[int]*" };

/* Instance Variables */

static vardecl var_sourceLocation[] =
{ IV(NAME_fileName, "name", IV_BOTH,
     NAME_location, "Name of the file in which the source resides"),
  IV(NAME_lineNo, "int*", IV_BOTH,
     NAME_location, "Starting line number of the source")
};

/* Send Methods */

static senddecl send_sourceLocation[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseSourceLocation,
     DEFAULT, "Create from file_name and line_no"),
  SM(NAME_copy, 1, "source_location", copySourceLocation,
     NAME_copy, "Copy file_name and line_no from argument")
};

/* Get Methods */

static getdecl get_sourceLocation[] =
{ GM(NAME_convert, 1, "source_location", "char_array|file", getConvertSourceLocation,
     DEFAULT, "Convert `file', `path-name' and `path-name:line-no'"),
  GM(NAME_path, 0, "name", NULL, getPathSourceLocation,
     NAME_path, "Get absolute path-name of source file")
};

/* Resources */

#define rc_sourceLocation NULL
/*
static resourcedecl rc_sourceLocation[] =
{ 
};
*/

/* Class Declaration */

static Name sourceLocation_termnames[] = { NAME_fileName, NAME_lineNo };

ClassDecl(sourceLocation_decls,
          var_sourceLocation, send_sourceLocation,
	  get_sourceLocation, rc_sourceLocation,
          2, sourceLocation_termnames,
          "$Rev$");

status
makeClassSourceLocation(Class class)
{ return declareClass(class, &sourceLocation_decls);
}

