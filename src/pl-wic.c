/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: load and save intermediate code files
*/

/*#define O_DEBUG 1*/
#include "pl-incl.h"

forwards char *	getString(FILE *);
forwards long	getNum(FILE *);
forwards real	getReal(FILE *);
forwards bool	loadWicFd(char *, FILE *, bool, bool);
forwards bool	loadPredicate(FILE *);
forwards bool	loadImport(FILE *);
forwards void	putString(char *, FILE *);
forwards void	putAtom(Atom, FILE *);
forwards void	putNum(long, FILE *);
forwards void	putReal(real, FILE *);
forwards void	saveWicClause(Clause, FILE *);
forwards void	closeProcedureWic(FILE *);
forwards bool	openWic(char *);
forwards bool	closeWic(void);
forwards bool	addClauseWic(Word, Atom);
forwards bool	addDirectiveWic(word, FILE *fd);
forwards bool	importWic(Procedure, FILE *fd);
forwards word	directiveClause(word, char *);
forwards bool	compileFile(char *);
forwards bool	putStates(FILE *);
forwards word	loadXR(FILE *);
forwards word   loadXRc(int c, FILE *fd);
forwards void	putstdw(word w, FILE *fd);
forwards word	getstdw(FILE *fd);
static bool	loadStatement(int c, FILE *fd);
static bool	loadPart(FILE *fd, Module *module);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SWI-Prolog can compile Prolog source files into intermediate code files, 
which can be loaded very  fast.   They  can  be  saved  as  stand  alone
executables using Unix #! magic number.

A wic file consists of the magic code and a version check code.  This is
followed by the command line option   defaults.  Then an optional series
of `include' statements follow.  Finally   the predicates and directives
are  described.   Predicates  are  described    close  to  the  internal
representation.  Directives are stored as  binary terms representing the
query.

The default options and include statements are written incrementally  in
each  wic  file.   In  the  normal  boot  cycle  first  the boot file is
determined.  Then the option structure is filled with the default option
found in this boot file.  Next the command line arguments are scanned to
obtain all options.  Then stacks, built  in's,  etc.   are  initialised.
The  the  boot  file is read again, but now only scanning for directives
and predicates.

IF YOU CHANGE ANYTHING TO THIS FILE, SO THAT OLD WIC-FILES CAN NO LONGER
BE READ, PLEASE DO NOT FORGET TO INCREMENT THE VERSION NUMBER!

Below is an informal description of the format of a `.qlf' file:

<wic-file>	::=	#!<path>
			<magic code>
			<version number>
			<localSize>			% a <word>
			<globalSize>			% a <word>
			<trailSize>			% a <word>
			<argumentSize>			% a <word>
			<lockSize>			% a <word>
			<goal>				% a <string>
			<topLevel>			% a <string>
			<initFile>			% a <string>
			{<statement>}
----------------------------------------------------------------
<qlf-file>	::=	<qlf-magic>
			<version-number>
			'Q' <qlf-part>
<qlf-magic>	::=	<string>
<qlf-module>	::=	<qlf-header>
			{<statement>}
			'X'
<qlf-header>	::=	'M' <XR/modulename>		% module name
			<source>			% file + time
			{<qlf-export>}
			'X'
			| <source>			% not a module
			<time>
<qlf-export>	::=	'E' <XR/functor>
<source>	::=	'F' <XR/name> <time> <system>
		      | '-'
----------------------------------------------------------------
<magic code>	::=	<string>			% normally #!<path>
<version number>::=	<num>
<statement>	::=	'W' <string>			% include wic file
		      | 'P' <XR/functor>
			    {<clause>} <pattern>	% predicate
		      | 'D' 
		        <lineno>			% source line number
			<term>				% directive
		      | 'E' <XR/functor>		% export predicate
		      | 'I' <XR/procedure>		% import predicate
		      | 'Q' <qlf-module>		% include module
<clause>	::=	'C' <line_no> <# var>
			    <#n subclause> <#codes> <codes>
		      | 'X' 				% end of list
<XR>		::=	XR_REF     <num>		% XR id from table
			XR_ATOM    <string>		% atom
			XR_INT     <num>		% number
			XR_FLOAT   <word>		% real (float)
			XR_STRING  <string>		% string
			XR_FUNCTOR <XR/name> <num>	% functor
			XR_PRED    <XR/fdef> <XR/module>% predicate
<term>		::=	<num>				% # variables in term
			<theterm>
<theterm>	::=	<XR/atomic>			% atomic data
		      | 'v' <num>			% variable
		      | 't' <XR/functor> {<theterm>}	% compound
<system>	::=	's'				% system source file
		      | 'u'				% user source file
<time>		::=	<word>				% time file was loaded
<pattern>	::=	<num>				% indexing pattern
<codes>		::=	<num> {<code>}
<string>	::=	{<non-zero byte>} <0>
<word>		::=	<4 byte entity>

Numbers are stored in  a  packed  format  to  reduce  the  size  of  the
intermediate  code  file  as  99%  of  them  is  normally  small, but in
principle not limited (virtual  machine  codes,  arities,  table  sizes,
etc).   The  upper  two  bits  of  the  first byte contain the number of
additional bytes.  the bytes represent the number `most-significant part
first'.  See the functions putNum() and getNum()  for  details.   Before
you  don't  agree  to  this  schema,  you  should  remember it makes the
intermediate code files about 30% smaller  and  avoids  the  differences
between  16  and  32  bits  machines (arities on 16 bits machines are 16
bits) as well as machines with different byte order.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define VERSION 17			/* save version number */

#define XR_REF     0			/* reference to previous */
#define XR_ATOM	   1			/* atom */
#define XR_FUNCTOR 2			/* functor */
#define XR_PRED	   3			/* procedure */
#define XR_INT     4			/* int */
#define XR_FLOAT   5			/* float */
#define XR_STRING   6			/* string */

static char saveMagic[] = "SWI-Prolog (c) 1990 Jan Wielemaker\n";
static char qlfMagic[]  = "SWI-Prolog .qlf file\n";
static char *wicFile;			/* name of output file */
static FILE *wicFd;			/* file descriptor of wic file */
static Procedure currentProc;		/* current procedure */
static SourceFile currentSource;	/* current source file */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
On tos, loading takes long; give the user  something  to  look  at.   On
workstations, it normally is so fast it is hardy noticable.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if tos
forwards void	notifyLoad(char *file);
forwards void	notifyLoaded(void);
forwards void	notifyPredicate(char *name, int arity);

static void
notifyLoad(file)
char *file;
{ printf("Loading %s ", file);
  fflush(stdout);
}

static void
notifyLoaded()
{ printf("\r\033K");
}

static void
notifyPredicate(name, arity)
char *name;
int arity;
{ static char cur[] = "|/-\\";
  static int  n = 0;

  printf("%c\b", cur[n++ & 0x3]);
}

#else /*!tos*/

#define notifyLoad(file)
#define notifyLoaded()
#define notifyPredicate(name, arity)

#endif /* tos */

		 /*******************************
		 *     LOADED XR ID HANDLING	*
		 *******************************/

typedef struct xr_table *XrTable;

struct xr_table
{ int		id;			/* next id to give out */
  Word	       *table;			/* main table */
  int   	tablesize;		/* # sub-arrays */
  XrTable	previous;		/* stack */
};

static XrTable loadedXrs;		/* head pointer */

#define loadedXRTableId		(loadedXrs->id)
#define loadedXRTable		(loadedXrs->table)
#define loadedXRTableArrays	(loadedXrs->tablesize)

#define SUBENTRIES ((ALLOCSIZE)/sizeof(word))

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
XR reference handling during loading.  This   is arranged as an array-of
arrays.  These arrays are of size ALLOCSIZE,   so they will be reused on
perfect-fit basis the pl-alloc.c.  With ALLOCSIZE   = 64K, this requires
minimal 128K memory.   Maximum  allowed  references   is  16K^2  or  32M
references.  That will normally overflow other system limits first.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
pushXrIdTable()
{ XrTable t = allocHeap(sizeof(struct xr_table));

  t->previous = loadedXrs;
  loadedXrs = t;

  loadedXRTable = malloc(ALLOCSIZE);
  loadedXRTableArrays = 0;
  loadedXRTableId = 0;
}


static void
popXrIdTable()
{ int i;
  XrTable prev = loadedXrs->previous;

  for(i=0; i<loadedXRTableArrays; i++)
    free(loadedXRTable[i]);

  free(loadedXRTable);
  freeHeap(loadedXrs, sizeof(struct xr_table));

  loadedXrs = prev;
}


static word
lookupXrId(long id)
{ Word array = loadedXRTable[id/SUBENTRIES];

  return array[id%SUBENTRIES];
}


static void
storeXrId(long id, word value)
{ int i = id/SUBENTRIES;

  while ( i >= loadedXRTableArrays )
    loadedXRTable[loadedXRTableArrays++] = malloc(ALLOCSIZE);
  
  loadedXRTable[i][id%SUBENTRIES] = value;
}


		 /*******************************
		 *	 PRIMITIVE LOADING	*
		 *******************************/

static bool
qlfLoadError(FILE *fd, char *ctx)
{ fatalError("%s: QLF format error at index = %ld", ctx, ftell(fd));

  fail;
}


static char *
getString(FILE *fd)
{ static char *tmp;
  static char *tmpend;
  static int  tmpsize = 512;
  char *s;
  Char c;

  if ( tmp == NULL )
  { tmp    = malloc(tmpsize);
    tmpend = &tmp[tmpsize-1];
  }

  for( s = tmp; (c = Getc(fd)) != EOS, *s = c; s++ )
  { if ( s == tmpend )
    { tmp = realloc(tmp, tmpsize+512);
      s = &tmp[tmpsize-1];
      tmpsize += 512;
      tmpend = &tmp[tmpsize-1];
    }
    if ( c == EOF )
      fatalError("Unexpected EOF on intermediate code file at offset %d",
		 ftell(fd));
  }

  return tmp;
}


static char *
getMagicString(FILE *fd, char *buf, int maxlen)
{ char *s;
  int c;

  for( s = buf; --maxlen >= 0 && (*s = (c = Getc(fd))); s++ )
    if ( c == EOF )
      return NULL;

  if ( maxlen > 0 )
    return buf;

  return NULL;
}


static long
getNum(FILE *fd)
{ long first = Getc(fd);
  int bytes, shift, b;

  if ( !(first & 0xc0) )
    return (first << 26) >> 26;		/* 99% of them: speed up a bit */    

  bytes = (int) ((first >> 6) & 0x3);
  first &= 0x3f;

  for( b = 0; b < bytes; b++ )
  { first <<= 8;
    first |= Getc(fd) & 0xff;
  }

  shift = (3-bytes)*8 + 2;

  return (first << shift) >> shift;
}


static word
getstdw(FILE *fd)
{
#ifndef WORDS_BIGENDIAN
  union
  { word         l;
    unsigned char c[4];
  } cvrt;
  long rval;

  cvrt.l = getw(fd);
  rval = (cvrt.c[0] << 24) |
         (cvrt.c[1] << 16) |
	 (cvrt.c[2] << 8) |
	  cvrt.c[3];
  return rval;
#else
  return getw(fd);
#endif
}


static real
getReal(FILE *fd)
{ real f;
  word *s = (word *) &f;

#ifndef WORDS_BIGENDIAN
  s[0] = getstdw(fd);
  s[1] = getstdw(fd);
#else
  s[1] = getstdw(fd);
  s[0] = getstdw(fd);
#endif

  DEBUG(3, printf("getReal() --> %f\n", f));

  return f;
}


static word
loadXRc(int c, FILE *fd)
{ word xr;
  int id = 0;				/* make gcc happy! */

  switch( c )
  { case XR_REF:
    { return lookupXrId(getNum(fd));
    }
    case XR_ATOM:
      id = ++loadedXRTableId;
      xr = (word) lookupAtom(getString(fd));
      DEBUG(3, Putf("XR(%d) = '%s'\n", id, stringAtom((Atom)xr)));
      break;
    case XR_FUNCTOR:
    { Atom name;
      int arity;

      id = ++loadedXRTableId;
      name = (Atom) loadXR(fd);
      arity = getNum(fd);
      xr = (word) lookupFunctorDef(name, arity);
      DEBUG(3, Putf("XR(%d) = %s/%d\n", id, stringAtom(name), arity));
      break;
    }
    case XR_PRED:
    { FunctorDef f;
      Atom mname;

      id = ++loadedXRTableId;
      f = (FunctorDef) loadXR(fd);
      mname = (Atom) loadXR(fd);
      xr = (word) lookupProcedure(f, lookupModule(mname));
      DEBUG(3, Putf("XR(%d) = proc %s\n", id, procedureName((Procedure)xr)));
      break;
    }
    case XR_INT:
      return consNum(getNum(fd));
    case XR_FLOAT:
      return heapReal(getReal(fd));	/* global ... */
#if O_STRING
    case XR_STRING:
      return heapString(getString(fd));
#endif
    default:
    { xr = 0;				/* make gcc happy */
      fatalError("Illegal XR entry at index %d: %c", ftell(fd)-1, c);
    }
  }

  storeXrId(id, xr);

  return xr;
}


static word
loadXR(FILE *fd)
{ return loadXRc(getc(fd), fd);
}


static void
do_load_qlf_term(FILE *fd, Word *vars, Word term)
{ int c = getc(fd);

  if ( c == 'v' )
  { int id = getNum(fd);
    
    *term = makeRef(&(*vars)[id]);
  } else if ( c == 't' )
  { FunctorDef f = (FunctorDef) loadXR(fd);
    int arity = f->arity;
    int n;

    *term = globalFunctor(f);
    for(n=0; n < arity; n++)
      do_load_qlf_term(fd, vars, argTermP(*term, n));
  } else
  { *term = loadXRc(c, fd);
  }
}


static word
loadQlfTerm(FILE *fd)
{ int nvars;
  Word vars;
  word term = 0;

  DEBUG(3, Putf("Loading from %d ...", ftell(fd)); fflush(stdout));
  if ( (nvars = getNum(fd)) )
  { int n;
    Word v;

    vars = allocGlobal(sizeof(word) * nvars);
    for(n=nvars, v=vars; n>0; n--, v++)
      setVar(*v);
  } else
    vars = NULL;

  lockp(&vars);
  lockw(&term);
  do_load_qlf_term(fd, &vars, &term);
  DEBUG(3, Putf("Loaded "); pl_write(&term); Putf(" to %d\n", ftell(fd)));
  unlockw(&term);
  unlockp(&vars);

  return term;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Load a complete `wic' file.  `toplevel' tells  us  whether  we  are  the
toplevel  file  opened,  and thus should include other `wic' files or we
should ignore the include statements.  `load_options' tells us  to  only
load the options of the toplevel file.

All wic files loaded are appended in the  right  order  to  a  chain  of
`states'.  They are written to a new toplevel wic file by openWic().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
loadWicFile(char *file, bool toplevel, bool load_options)
{ FILE *fd;
  bool rval = TRUE;
  bool tablealloced = FALSE;
  char *owf = wicFile;

  if ((fd = Fopen(file, STREAM_OPEN_BIN_READ)) == (FILE *) NULL)
  { fatalError("Can't open %s: %s", file, OsError());
    rval = FALSE;
    goto out;
  }

  wicFile = file;
  notifyLoad(file);

  if ( toplevel && !load_options )
  { pushXrIdTable();
    tablealloced    = TRUE;
  }

  if (loadWicFd(file, fd, toplevel, load_options) == FALSE)
  { rval = FALSE;
    goto out;
  }
  if (toplevel == TRUE && load_options == FALSE)
  { if (appendState(file) == FALSE)
    { rval = FALSE;
      goto out;
    }
  }

out:
  if (fd != (FILE *) NULL)
    fclose(fd);
  if ( tablealloced )
  { popXrIdTable();
  }

  wicFile = owf;
  notifyLoaded();

  return rval;
}


static bool
loadWicFd(char *file, FILE *fd, bool toplevel, bool load_options)
{ char *s;
  Char c;
  int n;
  char mbuf[100];

#if OS2
  for(n=0; n<5; n++)                    /* skip first five lines */
#else
  for(n=0; n<2; n++)			/* skip first two lines */
#endif
  { while( (c=(Char)Getc(fd)) != '\n' && c != EOF ) ;
    if ( c == EOF )
      return fatalError("%s is not a SWI-Prolog intermediate code file", file);
  }

  s = getMagicString(fd, mbuf, sizeof(mbuf));
  if ( !s || !streq(s, saveMagic) )
    return fatalError("%s is not a SWI-Prolog intermediate code file", file);

  if (getNum(fd) != VERSION)
  { fatalError("Intermediate code file %s has incompatible save version",
	       file);
    fail;
  }

  if (load_options && toplevel)
  { options.localSize    = getNum(fd);
    options.globalSize   = getNum(fd);
    options.trailSize    = getNum(fd);
    options.argumentSize = getNum(fd);
    options.lockSize	 = getNum(fd);
    DEBUG(2, printf("local=%ld, global=%ld, trail=%ld, argument=%ld\n",
		options.localSize, options.globalSize,
		options.trailSize, options.argumentSize));
    options.goal         = store_string(getString(fd) );
    options.topLevel     = store_string(getString(fd) );
    options.initFile     = store_string(getString(fd) );

    succeed;
  } else
  { int n;
    for(n=0; n<5; n++)   getNum(fd);
    for(n=0; n<3; n++)   getString(fd);
  }

  for(;;)
  { c = Getc(fd);

    switch( c )
    { case EOF:
	succeed;
      case 'W':
	{ char *name;

	  name = store_string(getString(fd) );
	  if (toplevel == TRUE)
	  { appendState(name);
	    pushXrIdTable();		/* has it's own id table! */
	    loadWicFile(name, FALSE, FALSE);
	    popXrIdTable();
	  }
	  continue;
	}
      default:
        { loadStatement(c, fd);
	  continue;
	}
    }
  }
}


static bool
loadStatement(int c, FILE *fd)
{ switch(c)
  { case 'P':
      return loadPredicate(fd);

    case 'I':
      return loadImport(fd);

    case 'D':
    { mark m;
      word goal;
      Atom osf = source_file_name;
      int  oln = source_line_no;

      source_file_name = currentSource->name;
      source_line_no   = getNum(fd);
      
      Mark(m);
      goal = loadQlfTerm(fd);
      if ( !callGoal(MODULE_user, goal, FALSE) )
      { printf("[WARNING: %s:%d: (loading %s) directive failed: ",
	       stringAtom(source_file_name), source_line_no, wicFile);
	pl_write(&goal);
	printf("]\n");
      }
      Undo(m);
      
      source_file_name = osf;
      source_line_no   = oln;

      succeed;
    }	  

    case 'Q':
      return loadPart(fd, NULL);

    default:
      return qlfLoadError(fd, "loadStatement()");
  }
}



static bool
loadPredicate(FILE *fd)
{ Procedure proc;
  Definition def;
  Clause clause;
  FunctorDef f = (FunctorDef) loadXR(fd);

  notifyPredicate(stringAtom(f->name), f->arity);
  proc = lookupProcedure(f, modules.source);
  DEBUG(3, Putf("Loading %s ", procedureName(proc)));
  def = proc->definition;
  if ( SYSTEM_MODE )
  { set(def, SYSTEM|HIDE_CHILDS|LOCKED);
  }
  addProcedureSourceFile(currentSource, proc);

  for(;;)
  { switch(Getc(fd) )
    { case 'X':
      { unsigned long pattern = getNum(fd);

	if ( def->indexPattern != pattern )
	{ def->indexPattern = pattern;
	  def->indexCardinality = cardinalityPattern(def->indexPattern);
	  if ( pattern != 0x1 )
	    reindexProcedure(proc);
	}

	DEBUG(3, Putf("ok\n"));
	succeed;
      }
      case 'C':
      { Code bp, ep;

	DEBUG(3, Putf("."); fflush(stdout));
	clause = (Clause) allocHeap(sizeof(struct clause));
	clause->line_no = getNum(fd);
	clause->next = (Clause) NULL;
	clause->references = 0;
	clearFlags(clause);
	clause->variables = getNum(fd);
	clause->subclauses = getNum(fd);
	clause->procedure = proc;
	clause->source_no = currentSource->index;
	clause->code_size = getNum(fd);
	statistics.codes += clause->code_size;
	clause->codes = (Code) allocHeap(clause->code_size * sizeof(code));

	bp = clause->codes;
	ep = bp + clause->code_size;

	while( bp < ep )
	{ code op = getNum(fd);
	  int n = 0;
	  int next = codeTable[op].externals;
	  int narg = codeTable[op].arguments;
	  
	  *bp++ = encode(op);
	  switch(op)
	  { case I_CALL:
	    case I_DEPART:
	    { FunctorDef f = (FunctorDef)loadXR(fd);
	      *bp++ = (word) lookupProcedure(f, modules.source);

	      break;
	    }
	    default:
	      for( ; n < next; n++ )
		*bp++ = loadXR(fd);
	      for( ; n < narg; n++ )
		*bp++ = getNum(fd);
	  }
	}

	assertProcedure(proc, clause, 'z');
	reindexClause(clause);
      }
    }
  }
}


static bool
loadImport(FILE *fd)
{ Procedure proc = (Procedure) loadXR(fd);
  FunctorDef functor = proc->functor;
  Procedure old;

  DEBUG(3, printf("loadImport(): %s into %s\n",
		  procedureName(proc), stringAtom(modules.source->name)));

  if ( (old = isCurrentProcedure(functor, modules.source)) )
  { if ( old->definition == proc->definition )
      succeed;			/* already done this! */

    if (!isDefinedProcedure(old) )
    { old->definition = proc->definition;
      succeed;
    }

    return warning("Failed to import %s into %s", 
		   procedureName(proc), 
		   stringAtom(modules.source->name) );
  }
  addHTable(modules.source->procedures, functor, proc);

  succeed;
}


static bool
qlfLoadSource(FILE *fd)
{ word fname = loadXR(fd);
  long time  = getstdw(fd);
  int issys  = (getc(fd) == 's') ? TRUE : FALSE;

  currentSource = lookupSourceFile((Atom) fname);
  currentSource->time = time;
  currentSource->system = issys;
  startConsult(currentSource);

  succeed;
}


static bool
loadPart(FILE *fd, Module *module)
{ Module om     = modules.source;
  SourceFile of = currentSource;
  int stchk     = debugstatus.styleCheck;

  switch(getc(fd))
  { case 'M':
    { word mname = loadXR(fd);

      switch( getc(fd) )
      { case '-':
	{ modules.source = lookupModule((Atom) mname);
					/* TBD: clear module? */
	  break;
	}
	case 'F':
	{ word fname;

	  qlfLoadSource(fd);
	  fname = (word) currentSource->name;

	  if ( !pl_declare_module(&mname, &fname) )
	    fail;
	  assert(currentSource == modules.source->file);

	  if ( module )
	    *module = modules.source;

	  for(;;)
	  { switch(getc(fd))
	    { case 'E':
	      { FunctorDef f = (FunctorDef) loadXR(fd);
		Procedure proc = lookupProcedure(f, modules.source);

		addHTable(modules.source->public, f, proc);
		continue;
	      }
	      case 'X':
		break;
	      default:
		return qlfLoadError(fd, "loadPart()");
	    }
	    break;
	  }
	  break;
	}
	default:
	  qlfLoadError(fd, "loadPart()");
	  break;
      }
      break;
    }
    case 'F':
    { qlfLoadSource(fd);

      if ( module )
	*module = NULL;

      break;
    }
    default:
      return qlfLoadError(fd, "loadPart()");
  }

  for(;;)
  { int c = getc(fd);

    switch(c)
    { case 'X':
      { modules.source = om;
	currentSource  = of;
	debugstatus.styleCheck = stchk;
	systemMode(debugstatus.styleCheck & DOLLAR_STYLE);

	succeed;
      }
      default:
	loadStatement(c, fd);
    }
  }

  succeed;
}


		 /*******************************
		 *	WRITING .QLF FILES	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The code below handles the creation of `wic' files.  It offers a  number
of  predicates  which  enables  us  to write the compilation toplevel in
Prolog.

Note that we keep track of the `current procedure' to keep  all  clauses
of a predicate together.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Table savedXRTable;		/* saved XR entries */
static int   savedXRTableId;		/* next id */

static void
putString(register char *s, FILE *fd)
{ while(*s)
  { Putc(*s, fd);
    s++;
  }

  Putc(EOS, fd);
}


static void
putAtom(Atom a, FILE *fd)
{ putString(a->name, fd);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Number encoding:

	0 <= n <= 2^6	Direct storage in byte
	


- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
putNum(long int n, FILE *fd)
{ if ( n > (1L << 28) || n < -((1L << 28) - 1) )
    sysError("Argument to putNum() out of range: %ld", n);

  n &= ~0xc0000000;

  if ( n < (1L << 5) )
  { Putc((char) (n & 0x3f), fd);
  } else if ( n < (1L << 13) )
  { Putc((char) (((n >> 8) & 0x3f) | (1 << 6)), fd);
    Putc((char) (n & 0xff), fd);
  } else if ( n < (1L << 21) )
  { Putc((char) (((n >> 16) & 0x3f) | (2 << 6)), fd);
    Putc((char) ((n >> 8) & 0xff), fd);
    Putc((char) (n & 0xff), fd);
  } else
  { Putc((char) (((n >> 24) & 0x3f) | (3 << 6)), fd);
    Putc((char) ((n >> 16) & 0xff), fd);
    Putc((char) ((n >> 8) & 0xff), fd);
    Putc((char) (n & 0xff), fd);
    return;
  }
}


static void
putstdw(word w, FILE *fd)
{
#ifndef WORDS_BIGENDIAN
  union
  { word         l;
    unsigned char c[4];
  } cvrt;
  word rval;

  cvrt.l = w;
  rval = (cvrt.c[0] << 24) |
         (cvrt.c[1] << 16) |
	 (cvrt.c[2] << 8) |
	  cvrt.c[3];
  putw(rval, fd);
#else
  putw(w, fd);
#endif
}


static void
putReal(real f, FILE *fd)
{ word *s = (word *)&f;

  DEBUG(3, printf("putReal(%f)\n", f));

#ifndef WORDS_BIGENDIAN
  putstdw(s[0], fd);
  putstdw(s[1], fd);
#else
  putstdw(s[1], fd);
  putstdw(s[0], fd);
#endif
}


static void
saveXR(word xr, FILE *fd)
{ Symbol s;
  int id;

  if (isInteger(xr) )
  { Putc(XR_INT, fd);
    putNum(valNum(xr), fd);
    return;
  } else if (isReal(xr) )
  { Putc(XR_FLOAT, fd);
    putReal(valReal(xr), fd);
    return;
#if O_STRING
  } else if ( isString(xr) )
  { Putc(XR_STRING, fd);
    putString(valString(xr), fd);
    return;
#endif /* O_STRING */
  }

  if ( (s = lookupHTable(savedXRTable, (void *)xr)) )
  { id = (int) s->value;
    Putc(XR_REF, fd);
    putNum(id, fd);
    return;
  }

  id = ++savedXRTableId;
  addHTable(savedXRTable, (void *)xr, (void *)id);

  if (isAtom(xr) )
  { Putc(XR_ATOM, fd);
    putAtom((Atom)xr, fd);
    DEBUG(3, Putf("XR(%d) = '%s'\n", id, stringAtom((Atom)xr)));
  } else if (((FunctorDef)xr)->type == FUNCTOR_TYPE)
  { FunctorDef f = (FunctorDef) xr;

    Putc(XR_FUNCTOR, fd);
    saveXR((word) f->name, fd);
    putNum(f->arity, fd);
    DEBUG(3, Putf("XR(%d) = %s/%d\n", id, stringAtom(f->name), f->arity));
  } else
  { Procedure p = (Procedure) xr;
    
    Putc(XR_PRED, fd);
    saveXR((word) p->functor, fd);
    saveXR((word) p->definition->module->name, fd);
    DEBUG(3, Putf("XR(%d) = proc %s\n", id, procedureName(p)));
  }
}


static void
do_save_qlf_term(Word t, FILE *fd)
{ deRef(t);

  if ( isTerm(*t) )
  { FunctorDef f = functorTerm(*t);

    if ( f == FUNCTOR_var1 )
    { int id = valNum(argTerm(*t, 0));

      Putc('v', fd);
      putNum(id, fd);
    } else
    { Word q = argTermP(*t, 0);
      int n;

      Putc('t', fd);
      saveXR((word) f, fd);
      for(n=0; n < f->arity; n++, q++)
	do_save_qlf_term(q, fd);
    }
  } else
  { assert(isAtomic(*t));
    saveXR(*t, fd);
  }
}


static void
saveQlfTerm(Word t, FILE *fd)
{ int nvars;
  mark m;

  DEBUG(3, Putf("Saving "); pl_write(t); Putf(" from %d ... ", ftell(fd)));
  Mark(m);
  nvars = numberVars(t, FUNCTOR_var1, 0);
  putNum(nvars, fd);
  do_save_qlf_term(t, fd);
  DEBUG(3, Putf("to %d\n", ftell(fd)));
  Undo(m);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
saveWicClause()  saves  a  clause  to  the  .qlf  file.   For  predicate
references of I_CALL and I_DEPART, we  cannot store the predicate itself
as this would lead to an inconsistency if   the .qlf file is loaded into
another context module.  Therefore we just   store the functor.  For now
this is ok as constructs of the   form  module:goal are translated using
the meta-call mechanism.  This needs consideration   if we optimise this
(which is not that likely as I   thin  module:goal, where `module' is an
atom,  should  be  restricted  to  very    special  cases  and  toplevel
interaction.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
saveWicClause(Clause clause, FILE *fd)
{ Code bp, ep;

  Putc('C', fd);
  putNum(clause->line_no, fd);
  putNum(clause->variables, fd);
  putNum(clause->subclauses, fd);
  putNum(clause->code_size, fd);

  bp = clause->codes;
  ep = bp + clause->code_size;

  while( bp < ep )
  { code op = decode(*bp++);
    int n = 0;

    putNum(op, fd);
    switch(op)
    { case I_CALL:
      case I_DEPART:
      { Procedure p = (Procedure) *bp++;

	saveXR((word)p->functor, fd);
	break;
      }
      default:
	for( ; n < codeTable[op].externals; n++ )
	  saveXR(*bp++, fd);
        for( ; n < codeTable[op].arguments; n++ )
	  putNum(*bp++, fd);
    }
  }
}


		/********************************
		*         COMPILATION           *
		*********************************/

static void
closeProcedureWic(FILE *fd)
{ if ( currentProc != (Procedure) NULL )
  { Putc('X', fd);
    putNum(currentProc->definition->indexPattern, fd);
    currentProc = (Procedure) NULL;
  }
}


static bool
openWic(char *file)
{ char *exec;

  wicFile = file;

  DEBUG(1, printf("Open compiler output file %s\n", file));
  if ( (wicFd = Fopen(file, STREAM_OPEN_BIN_WRITE)) == (FILE *)NULL )
    return warning("Can't open %s: %s", file, OsError());
  DEBUG(1, printf("Searching for executable\n"));
  if ( loaderstatus.restored_state )
  { exec = stringAtom(loaderstatus.restored_state);
  } else
  { TRY( getSymbols() );
    exec = stringAtom(loaderstatus.orgsymbolfile);
  }
  DEBUG(1, printf("Executable = %s\n", exec));
  if ( !(exec = OsPath(AbsoluteFile(exec))) )
    fail;
  DEBUG(1, printf("Expanded executable = %s\n", exec));
/*fprintf(wicFd, "#!%s -x\n", exec);*/
#if OS2
  fprintf(wicFd, "/* Compiled SWI-Prolog Program */\r\n'@ECHO OFF'\r\nparse source . . name\r\n\"%s -x \" name arg(1)\r\nexit\r\n", exec);
#else
  fprintf(wicFd, "#!/bin/sh\nexec %s -x $0 $*\n", exec);
#endif /* OS2 */
  DEBUG(2, printf("Magic  ...\n"));
  putString( saveMagic,            wicFd);
  DEBUG(2, printf("Numeric options ...\n"));
  putNum(    VERSION,              wicFd);
  putNum(    options.localSize,    wicFd);
  putNum(    options.globalSize,   wicFd);
  putNum(    options.trailSize,    wicFd);
  putNum(    options.argumentSize, wicFd);
  putNum(    options.lockSize,     wicFd);
  DEBUG(2, printf("String options ...\n"));
  putString(options.goal,          wicFd);
  putString(options.topLevel,      wicFd);
  putString(options.initFile, 	   wicFd);

  DEBUG(2, printf("States ...\n"));
  putStates(wicFd);

  currentProc    = (Procedure) NULL;
  currentSource  = (SourceFile) NULL;
  savedXRTable   = newHTable(256);
  savedXRTableId = 0;

  DEBUG(2, printf("Header complete ...\n"));
  succeed;
}  

static bool
closeWic()
{ if (wicFd == (FILE *) NULL)
    fail;
  closeProcedureWic(wicFd);
  destroyHTable(savedXRTable);
  savedXRTable = NULL;
  fclose(wicFd);
  wicFd = NULL;
  return MarkExecutable(wicFile);
}

static bool
addClauseWic(Word term, Atom file)
{ Clause clause;

  if ((clause = assert_term(term, 'z', file)) != (Clause)NULL)
  { if (clause->procedure != currentProc)
    { closeProcedureWic(wicFd);
      currentProc = clause->procedure;
      Putc('P', wicFd);
      saveXR((word) currentProc->functor, wicFd);
    }
    saveWicClause(clause, wicFd);
    succeed;
  }

  fail;
}

static bool
addDirectiveWic(word term, FILE *fd)
{ closeProcedureWic(fd);
  Putc('D', fd);
  putNum(source_line_no, fd);
  saveQlfTerm(&term, fd);

  succeed;
}  


static bool
importWic(Procedure proc, FILE *fd)
{ closeProcedureWic(fd);

  Putc('I', fd);
  saveXR((word) proc, fd);

  succeed;
}

		 /*******************************
		 *	NEW MODULE SUPPORT	*
		 *******************************/

static bool
qlfOpen(Atom name)
{ wicFile = stringAtom(name);

  if ( !(wicFd = Fopen(wicFile, STREAM_OPEN_BIN_WRITE)) )
    return warning("qlf_open/1: can't open %s: %s", wicFile, OsError());

  putString(qlfMagic, wicFd);
  putNum(VERSION, wicFd);

  currentProc    = (Procedure) NULL;
  currentSource  = (SourceFile) NULL;
  savedXRTable   = newHTable(256);
  savedXRTableId = 0;

  succeed;
}


static bool
qlfClose()
{ closeProcedureWic(wicFd);
  fclose(wicFd);
  wicFd = NULL;

  destroyHTable(savedXRTable);
  savedXRTable = NULL;
  
  succeed;
}


static bool
qlfLoad(char *file, Module *module)
{ FILE *fd;
  char *magic;
  bool rval;
  char mbuf[100];
  int lversion;

  wicFile = file;
  
  if ( !(fd = Fopen(file, STREAM_OPEN_BIN_READ)) )
    return warning("$qlf_load/1: can't open %s: %s", file, OsError());
  if ( !(magic = getMagicString(fd, mbuf, sizeof(mbuf))) ||
       !streq(magic, qlfMagic) )
  { fclose(fd);
    return warning("$qlf_load/1: %s is not a SWI-Prolog .qlf file", file);
  }
  if ( (lversion = getNum(fd)) != VERSION )
  { fclose(fd);
    warning("$qlf_load/1: %s bad version (file version = %d, prolog = %d)",
	    wicFile, lversion, VERSION);
    fail;
  }

  if ( fgetc(fd) != 'Q' )
    return qlfLoadError(fd, "qlfLoad()");

  pushXrIdTable();
  rval = loadPart(fd, module);
  popXrIdTable();

  fclose(fd);

  return rval;
}


static bool
qlfSaveSource(SourceFile f, FILE *fd)
{ Putc('F', fd);
  saveXR((word)f->name, fd);
  putstdw(f->time, fd);
  Putc(f->system ? 's' : 'u', fd);

  currentSource = f;

  succeed;
}


static bool
qlfStartModule(Module m, FILE *fd)
{ Symbol s;

  closeProcedureWic(fd);
  Putc('Q', fd);
  Putc('M', fd);
  saveXR((word) m->name, fd);
  if ( m->file )
    qlfSaveSource(m->file, fd);
  else
    Putc('-', fd);

  for_table(s, m->public)
  { FunctorDef f = (FunctorDef)s->name;

    Putc('E', fd);
    saveXR((word)f, fd);
  } 

  Putc('X', fd);

  succeed;
}


static bool
qlfStartFile(SourceFile f, FILE *fd)
{ closeProcedureWic(fd);
  Putc('Q', fd);
  qlfSaveSource(f, fd);

  succeed;
}


static bool
qlfEndPart(FILE  *fd)
{ closeProcedureWic(fd);
  Putc('X', fd);

  succeed;
}


word
pl_qlf_start_module(Word name)
{ if ( wicFd )
  { if ( !isAtom(*name) )
      return warning("qlf_start_module/1: argument must be an atom");
  
    return qlfStartModule(lookupModule((Atom)*name), wicFd);
  }

  succeed;
}


word
pl_qlf_start_file(Word name)
{ if ( wicFd )
  { if ( !isAtom(*name) )
      return warning("qlf_start_file/1: argument must be an atom");
  
    return qlfStartFile(lookupSourceFile((Atom)*name), wicFd);
  }

  succeed;
}


word
pl_qlf_end_part()
{ if ( wicFd )
  { return qlfEndPart(wicFd);
  }

  succeed;
}


word
pl_qlf_open(Word file)
{ if ( !isAtom(*file) )
    return warning("qlf_open/1: instantiation fault");

  return qlfOpen((Atom)*file);
}


word
pl_qlf_close()
{ return qlfClose();
}


word
pl_qlf_load(Word file, Word module)
{ Module m, target = NULL, oldsrc = modules.source;
  char *name;
  bool rval;

  if ( !(file = stripModule(file, &target)) )
    fail;
  if ( !(name = primitiveToString(*file, FALSE)) )
    return warning("$qlf_load/2: instantiation fault");
  if ( !(name = ExpandOneFile(name)) )
    fail;

  modules.source = target;
  rval = qlfLoad(name, &m);
  modules.source = oldsrc;

  if ( !rval )
    fail;

  return unifyAtomic(module, m ? (word) m->name : consNum(0));
}


		/********************************
		*        PROLOG SUPPORT         *
		*********************************/

word
pl_open_wic(Word name)
{ if (!isAtom(*name) )
    fail;

  return openWic(stringAtom(*name));
}

word
pl_close_wic(void)
{ return closeWic();
}

word
pl_add_directive_wic(Word term)
{ if ( wicFd )
  { if (isVar(*term) )
      return warning("$add_directive_wic/1: directive is a variable");

    return addDirectiveWic(*term, wicFd);
  }

  succeed;
}

word
pl_import_wic(Word module, Word head)
{ if ( wicFd )
  { Module m;
    FunctorDef f;

    if ( !isAtom(*module) ||
	 !(isAtom(*head) || isTerm(*head)) )
      return warning("$import_wic/3: instantiation fault");

    m = lookupModule((Atom)*module);

    if ( isAtom(*head) )
      f = lookupFunctorDef((Atom)*head, 0);
    else
      f = functorTerm(*head);

    return importWic(lookupProcedure(f, m), wicFd);
  }

  succeed;
}


word
pl_qlf_assert_clause(Word ref)
{ if ( wicFd )
  { Clause clause;

    if (!isInteger(*ref))
    { warning("$qlf_assert_clause/1: argument is not a clause reference");
      fail;
    }

    clause = (Clause) numToPointer(*ref);
  
    if ( !inCore(clause) || !isClause(clause) )
      return warning("$qlf_assert_claue/1: Invalid reference");

    if (clause->procedure != currentProc)
    { closeProcedureWic(wicFd);
      currentProc = clause->procedure;
      Putc('P', wicFd);
      saveXR((word) currentProc->functor, wicFd);
    }

    saveWicClause(clause, wicFd);
  }

  succeed;
}


		/********************************
		*     BOOTSTRAP COMPILATION     *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The code below offers a restricted compilation  toplevel  used  for  the
bootstrap  compilation  (-b  option).  It handles most things the Prolog
defined compiler handles as well, except:

  - Be carefull to define  a  predicate  first  before  using  it  as  a
    directive
  - It does not offer `consult', `ensure_loaded' or the  list  notation.
    (there is no way to include other files).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*  Check if a clause is of the for ":- directive". If not return NULL, 
    otherwise return the argument.

 ** Wed Jun  8 16:12:39 1988  jan@swivax.UUCP (Jan Wielemaker)  */

static word
directiveClause(word clause, char *functor)
{ if (!isTerm(clause) )
    return (word) NULL;
  if (functorTerm(clause)->arity == 1 &&
       streq(functorTerm(clause)->name->name, functor) )
  { word d;

    d = argTerm(clause, 0);
    if (isVar(d) )
      return (word) NULL;

    if ( !isTerm(d) || functorTerm(d) != FUNCTOR_module2 )
    { word directive;
      
      directive = globalFunctor(FUNCTOR_module2);
      argTerm(directive, 0) = (word) modules.source->name;
      argTerm(directive, 1) = d;

      return directive;
    }

    return d;
  } else
    return (word) NULL;
}

/*  Compile an entire file into intermediate code.

 ** Thu Apr 28 13:44:43 1988  jan@swivax.UUCP (Jan Wielemaker)  */

static bool
compileFile(char *file)
{ char *path;
  word f;
  Word term = newTerm();

  DEBUG(1, printf("Boot compilation of %s\n", file));
  if ((path = AbsoluteFile(file)) == (char *) NULL)
    fail;
  DEBUG(2, printf("Expanded to %s\n", path));

  f = (word) lookupAtom(path);
  DEBUG(2, printf("Opening\n"));
  if ( !pl_see(&f) )
    fail;
  DEBUG(2, printf("pl_start_consult()\n"));
  pl_start_consult(&f);
  qlfStartFile(lookupSourceFile((Atom)f), wicFd);
  
  for(;;)
  { word directive;
    mark m;
    
    Mark(m);
    DEBUG(2, printf("pl_read_clause() -> "));
    if (pl_read_clause(term) == FALSE)
      continue;
    DEBUG(2, pl_write(term); pl_nl());
    if (*term == (word) ATOM_end_of_file)
      break;
    if ((directive = directiveClause(*term, ":-")) != (word) NULL)
    { environment_frame = (LocalFrame) NULL;
      DEBUG(1, Putf(":- "); pl_write(&directive); Putf(".\n") );
      addDirectiveWic(directive, wicFd);
      callGoal(MODULE_user, directive, FALSE);
    } else if ((directive = directiveClause(*term, "$:-")) != (word) NULL)
    { environment_frame = (LocalFrame) NULL;
      DEBUG(1, Putf("$:- "); pl_write(&directive); Putf(".\n") );
      callGoal(MODULE_user, directive, FALSE);
    } else
      addClauseWic(term, (Atom)f);
    Undo(m);
  }

  qlfEndPart(wicFd);
  pl_seen();

  succeed;
}

bool
compileFileList(char *out, int argc, char **argv)
{ newOp("$:-", OP_FX, 1200);
  TRY(openWic(out) );
  
  systemMode(TRUE);

  for(;argc > 0; argc--, argv++)
  { if (streq(argv[0], "-c") )
      break;
    compileFile(argv[0]);
  }

  status.autoload = TRUE;
  systemMode(FALSE);

  callGoal(MODULE_user,
	   (word)lookupAtom("$load_additional_boot_files"),
	   FALSE);

  return closeWic();
}


		/********************************
		*         STATE LISTS           *
		*********************************/

/*  Add a new state to the chain of states this Prolog session is build
    from. The file name is made absolute to avoid directory problems
    with incremental loading.
*/

bool
appendState(char *name)
{ State state, st;
  char *absolute;

  if ((absolute = AbsoluteFile(name)) == (char *) NULL)
    return warning("invalid file specification: %s", name);

  state = (State) allocHeap(sizeof(struct state) );
  state->next = (State) NULL;
  state->name = store_string(absolute);

  if (stateList == (State) NULL)
  { stateList = state;
    succeed;
  }
  for(st = stateList; st->next; st = st->next) ;
  st->next = state;

  succeed;
}

/*  Add 'W' statements to the WIC file for each file in the state list.
*/

static bool
putStates(FILE *fd)
{ State st;

  for(st = stateList; st; st = st->next)
  { Putc('W', fd);
    putString(st->name, fd);
  }

  succeed;
}
