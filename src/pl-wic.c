/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: load and save intermediate code files
*/

/*#define O_DEBUG 1*/
#include "pl-incl.h"
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

forwards char *	getString(IOSTREAM *);
forwards long	getNum(IOSTREAM *);
forwards real	getReal(IOSTREAM *);
forwards bool	loadWicFd(char *, IOSTREAM *, int);
forwards bool	loadPredicate(IOSTREAM *, int skip);
forwards bool	loadImport(IOSTREAM *, int skip);
forwards void	putString(char *, IOSTREAM *);
forwards void	putAtom(atom_t, IOSTREAM *);
forwards void	putNum(long, IOSTREAM *);
forwards void	putReal(real, IOSTREAM *);
forwards void	saveWicClause(Clause, IOSTREAM *);
forwards void	closeProcedureWic(IOSTREAM *);
forwards bool	closeWic(void);
forwards bool	addDirectiveWic(term_t, IOSTREAM *fd);
forwards bool	importWic(Procedure, IOSTREAM *fd);
forwards bool	compileFile(char *);
forwards bool	putStates(IOSTREAM *);
forwards word	loadXR(IOSTREAM *);
forwards word   loadXRc(int c, IOSTREAM *fd);
forwards void	putstdw(word w, IOSTREAM *fd);
forwards word	getstdw(IOSTREAM *fd);
static bool	loadStatement(int c, IOSTREAM *fd, int skip);
static bool	loadPart(IOSTREAM *fd, Module *module, int skip);
static bool	loadInModule(IOSTREAM *fd, int skip);
static int	qlfVersion(IOSTREAM *s);
static bool	appendState(char *name);

#define Qgetc(s) Snpgetc(s)		/* ignore position recording */

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

IF YOU CHANGE ANYTHING TO THIS IOSTREAM, SO THAT OLD WIC-FILES CAN NO LONGER
BE READ, PLEASE DO NOT FORGET TO INCREMENT THE VERSION NUMBER!

Below is an informal description of the format of a `.qlf' file:

<wic-file>	::=	#!<path>
			<magic code>
			<version number>
			<localSize>			% a <word>
			<globalSize>			% a <word>
			<trailSize>			% a <word>
			<argumentSize>			% a <word>
			<heapSize>			% a <word>
			<goal>				% a <string>
			<topLevel>			% a <string>
			<initFile>			% a <string>
			<home>				% a <string>
			{<statement>}
			'T'
			<size>				% a stdword
			<QLFMAGICNUM>			% a stdword
----------------------------------------------------------------
<qlf-file>	::=	<qlf-magic>
			<version-number>
			'F' <string>			% path of qlf file
			'Q' <qlf-part>
<qlf-magic>	::=	<string>
<qlf-module>	::=	<qlf-header>
			<size>				% size in bytes
			{<statement>}
			'X'
<qlf-header>	::=	'M' <XR/modulename>		% module name
			<source>			% file + time
			{<qlf-export>}
			'X'
		      | <source>			% not a module
			<time>
<qlf-export>	::=	'E' <XR/functor>
<source>	::=	'F' <string> <time> <system>
		      | '-'
----------------------------------------------------------------
<magic code>	::=	<string>			% normally #!<path>
<version number>::=	<num>
<statement>	::=	'W' <string>			% include wic file
		      | 'P' <XR/functor>
			    {<clause>} <pattern>	% predicate
		      |	'O' <XR/modulename>
			    <XR/functor>		% pred out of module
			    {<clause>} <pattern>
		      | 'D' 
		        <lineno>			% source line number
			<term>				% directive
		      | 'E' <XR/functor>		% export predicate
		      | 'I' <XR/procedure>		% import predicate
		      | 'Q' <qlf-module>		% include module
		      | 'M' <XR/modulename>		% load-in-module
		            {<statement>}
			    'X'
<clause>	::=	'C' <line_no> <# var>
			    <#n subclause> <#codes> <codes>
		      | 'X' 				% end of list
<XR>		::=	XR_REF     <num>		% XR id from table
			XR_ATOM    <string>		% atom
			XR_INT     <num>		% number
			XR_BIGNUM  <word>		% big-number
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

#define LOADVERSION 30			/* load all versions later >= 30 */
#define VERSION 31			/* save version number */
#define QLFMAGICNUM 0x716c7374		/* "qlst" on little-endian machine */

#define XR_REF     0			/* reference to previous */
#define XR_ATOM	   1			/* atom */
#define XR_FUNCTOR 2			/* functor */
#define XR_PRED	   3			/* procedure */
#define XR_INT     4			/* int */
#define XR_BIGNUM  5			/* 32-bit integer */
#define XR_FLOAT   6			/* float */
#define XR_STRING  7			/* string */

static char saveMagic[] = "SWI-Prolog (c) 1990 Jan Wielemaker\n";
static char qlfMagic[]  = "SWI-Prolog .qlf file\n";
static char *wicFile;			/* name of output file */
static char *mkWicFile;			/* Wic file under construction */
static IOSTREAM *wicFd;			/* file descriptor of wic file */
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
{ Sfprintf(Soutput, "Loading %s ", file);
  Sflush(Soutput);
}

static void
notifyLoaded()
{ Sprintf(Soutput, "\r\033K");
}

static void
notifyPredicate(name, arity)
char *name;
int arity;
{ static char cur[] = "|/-\\";
  static int  n = 0;

  Sprintf(Soutput, "%c\b", cur[n++ & 0x3]);
}

#else /*!tos*/

#define notifyLoad(file)
#define notifyLoaded()
#define notifyPredicate(name, arity)

#endif /* tos */

		 /*******************************
		 *	     CLEANUP		*
		 *******************************/

void
qlfCleanup()
{ if ( mkWicFile )
  { warning("Removing incomplete Quick Load File %s", mkWicFile);
    RemoveFile(mkWicFile);
    mkWicFile = NULL;
  }
}


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

  if ( !(loadedXRTable = malloc(ALLOCSIZE)) )
    outOfCore();
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
  { if ( !(loadedXRTable[loadedXRTableArrays++] = malloc(ALLOCSIZE)) )
      outOfCore();
  }
  
  loadedXRTable[i][id%SUBENTRIES] = value;
}


		 /*******************************
		 *	 PRIMITIVE LOADING	*
		 *******************************/

static int	qlf_has_moved;		/* file has moved: be careful */
static char *   qlf_save_dir;		/* dir of saved .qlf file */
static char *	qlf_load_dir;		/* dir of .qlf file now */

static bool
qlfLoadError(IOSTREAM *fd, char *ctx)
{ fatalError("%s: QLF format error at index = %ld", ctx, Stell(fd));

  fail;
}


static char *
getString(IOSTREAM *fd)
{ static char *tmp;
  static char *tmpend;
  static int  tmpsize = 512;
  char *s;
  Char c;

  if ( tmp == NULL )
  { if ( !(tmp = malloc(tmpsize)) )
      outOfCore();
    tmpend = &tmp[tmpsize-1];
  }

  for( s = tmp; (*s = c = Getc(fd)) != EOS; s++ )
  { if ( s == tmpend )
    { if ( !(tmp = realloc(tmp, tmpsize+512)) )
	outOfCore();
      s = &tmp[tmpsize-1];
      tmpsize += 512;
      tmpend = &tmp[tmpsize-1];
    }
    if ( c == EOF )
      fatalError("Unexpected EOF on intermediate code file at offset %d",
		 Stell(fd));
  }

  return tmp;
}


static char *
getMagicString(IOSTREAM *fd, char *buf, int maxlen)
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
getNum(IOSTREAM *fd)
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
getstdw(IOSTREAM *fd)
{
#ifndef WORDS_BIGENDIAN
  union
  { word         l;
    unsigned char c[4];
  } cvrt;
  long rval;

  cvrt.l = Sgetw(fd);
  rval = (cvrt.c[0] << 24) |
         (cvrt.c[1] << 16) |
	 (cvrt.c[2] << 8) |
	  cvrt.c[3];
  return rval;
#else
  return Sgetw(fd);
#endif
}


static real
getReal(IOSTREAM *fd)
{ real f;
  word *s = (word *) &f;

#ifndef WORDS_BIGENDIAN
  s[0] = getstdw(fd);
  s[1] = getstdw(fd);
#else
  s[1] = getstdw(fd);
  s[0] = getstdw(fd);
#endif

  DEBUG(3, Sdprintf("getReal() --> %f\n", f));

  return f;
}


static word
loadXRc(int c, IOSTREAM *fd)
{ word xr;
  int id = 0;				/* make gcc happy! */

  switch( c )
  { case XR_REF:
    { return lookupXrId(getNum(fd));
    }
    case XR_ATOM:
      id = ++loadedXRTableId;
      xr = lookupAtom(getString(fd));
      DEBUG(3, Putf("XR(%d) = '%s'\n", id, stringAtom(xr)));
      break;
    case XR_FUNCTOR:
    { atom_t name;
      int arity;

      id = ++loadedXRTableId;
      name = loadXR(fd);
      arity = getNum(fd);
      xr = (word) lookupFunctorDef(name, arity);
      DEBUG(3, Putf("XR(%d) = %s/%d\n", id, stringAtom(name), arity));
      break;
    }
    case XR_PRED:
    { FunctorDef f;
      atom_t mname;

      id = ++loadedXRTableId;
      f = (FunctorDef) loadXR(fd);
      mname = loadXR(fd);
      xr = (word) lookupProcedure(f, lookupModule(mname));
      DEBUG(3, Putf("XR(%d) = proc %s\n", id, procedureName((Procedure)xr)));
      break;
    }
    case XR_INT:
      return consInt(getNum(fd));
    case XR_BIGNUM:
      return globalLong(getstdw(fd));
    case XR_FLOAT:
      return globalReal(getReal(fd));
#if O_STRING
    case XR_STRING:
      return globalString(getString(fd));
#endif
    default:
    { xr = 0;				/* make gcc happy */
      fatalError("Illegal XR entry at index %d: %c", Stell(fd)-1, c);
    }
  }

  storeXrId(id, xr);

  return xr;
}


static word
loadXR(IOSTREAM *fd)
{ return loadXRc(Qgetc(fd), fd);
}


static void
do_load_qlf_term(IOSTREAM *fd, term_t vars[], term_t term)
{ int c = Qgetc(fd);

  if ( c == 'v' )
  { int id = getNum(fd);
    
    if ( vars[id] )
      PL_unify(term, vars[id]);
    else
    { vars[id] = PL_new_term_ref();
      PL_put_term(vars[id], term);
    }
  } else if ( c == 't' )
  { FunctorDef f = (FunctorDef) loadXR(fd);
    term_t c2 = PL_new_term_ref();
    int arity = f->arity;
    int n;

    PL_unify_functor(term, f);
    for(n=0; n < arity; n++)
    { PL_get_arg(n+1, term, c2);
      do_load_qlf_term(fd, vars, c2);
    }
  } else
  { _PL_unify_atomic(term, loadXRc(c, fd));
  }
}


static void
loadQlfTerm(term_t term, IOSTREAM *fd)
{ int nvars;
  Word vars;

  DEBUG(3, Putf("Loading from %d ...", Stell(fd)));
  if ( (nvars = getNum(fd)) )
  { term_t *v;
    int n;

    vars = alloca(nvars * sizeof(term_t));
    for(n=nvars, v=vars; n>0; n--, v++)
      *v = 0L;
  } else
    vars = NULL;

  PL_put_variable(term);
  do_load_qlf_term(fd, vars, term);
  DEBUG(3, Putf("Loaded "); pl_write(term); Putf(" to %d\n", Stell(fd)));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Load a complete `wic' file.  `toplevel' tells  us  whether  we  are  the
toplevel  file  opened,  and thus should include other `wic' files or we
should ignore the include statements.  `load_options' tells us  to  only
load the options of the toplevel file.

All wic files loaded are appended in the  right  order  to  a  chain  of
`states'.  They are written to a new toplevel wic file by openWic().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
loadWicFile(char *file, int flags)
{ IOSTREAM *fd;
  bool rval = TRUE;
  bool tablealloced = FALSE;
  char *owf = wicFile;

  if ((fd = Sopen_file(file, "rbr")) == (IOSTREAM *) NULL)
  { if ( flags & QLF_EXESTATE )
      rval = -1;
    else
      fatalError("Can't open %s: %s", file, OsError());
    rval = FALSE;
    goto out;
  }

  if ( flags & QLF_EXESTATE )
  { if ( Sseek(fd, -2 * (long)sizeof(long), SIO_SEEK_END) > 0 )
    { long size, magic;

      size = getstdw(fd);
      magic = getstdw(fd);
      if ( magic == QLFMAGICNUM )
	Sseek(fd, -2 * (long)sizeof(long) - size, SIO_SEEK_END);
      else
	rval = -1;
    } else
      rval = -1;
  }

  if ( rval < 0 )
    goto out;

  wicFile = file;
  notifyLoad(file);

  if ( (flags & QLF_TOPLEVEL) && !(flags & QLF_OPTIONS) )
  { pushXrIdTable();
    tablealloced    = TRUE;
  }

  if ( loadWicFd(file, fd, flags) == FALSE )
  { rval = FALSE;
    goto out;
  }
  if ( (flags & QLF_TOPLEVEL) && !(flags & QLF_OPTIONS) )
  { if (appendState(file) == FALSE)
    { rval = FALSE;
      goto out;
    }
  }

out:
  if (fd != (IOSTREAM *) NULL)
    Sclose(fd);
  if ( tablealloced )
  { popXrIdTable();
  }

  wicFile = owf;
  notifyLoaded();

  return rval;
}

#define QLF_MAX_HEADER_LINES 100

static bool
loadWicFd(char *file, IOSTREAM *fd, int flags)
{ char *s;
  Char c;
  int n;
  char mbuf[100];
  char *savedhome;

  for(n=0; n<QLF_MAX_HEADER_LINES; n++)
  { char line[256];

    if ( Sfgets(line, sizeof(line), fd) == 0 )
      return fatalError("%s is not a SWI-Prolog intermediate code file", file);
    if ( streq(line, "# End Header\n") )
      break;
  }
  if ( n >= QLF_MAX_HEADER_LINES )
    return fatalError("%s: header script too long (> 100 lines)", file);

  s = getMagicString(fd, mbuf, sizeof(mbuf));
  if ( !s || !streq(s, saveMagic) )
    return fatalError("%s is not a SWI-Prolog intermediate code file", file);

  if ( getNum(fd) < LOADVERSION )
  { fatalError("Intermediate code file %s has incompatible save version",
	       file);
    fail;
  }

  if ( (flags & QLF_OPTIONS) && (flags & QLF_TOPLEVEL) )
  { options.localSize    = getNum(fd);
    options.globalSize   = getNum(fd);
    options.trailSize    = getNum(fd);
    options.argumentSize = getNum(fd);
    options.heapSize	 = getNum(fd);
    DEBUG(2,
	  Sdprintf("local=%ld, global=%ld, trail=%ld, arg=%ld, heap=%ld\n",
		   options.localSize, options.globalSize,
		   options.trailSize, options.argumentSize,
		   options.heapSize
		  ));
    options.goal         = store_string(getString(fd) );
    options.topLevel     = store_string(getString(fd) );
    options.initFile     = store_string(getString(fd) );

    succeed;
  } else
  { int n;
    for(n=0; n<5; n++)   getNum(fd);
    for(n=0; n<3; n++)   getString(fd);
  }

					/* fix paths for changed home */
  savedhome = getString(fd);
  if ( !systemDefaults.home || streq(savedhome, systemDefaults.home) )
  { qlf_has_moved = FALSE;
  } else
  { qlf_has_moved = TRUE;
    qlf_save_dir = store_string(savedhome);
    qlf_load_dir = systemDefaults.home;
  }

  for(;;)
  { c = Getc(fd);

    switch( c )
    { case EOF:
      case 'T':				/* trailer */
	succeed;
      case 'W':
	{ char *name;

	  name = store_string(getString(fd) );
	  if ( (flags & QLF_TOPLEVEL) )
	  { appendState(name);
	    pushXrIdTable();		/* has it's own id table! */
	    loadWicFile(name, 0);
	    popXrIdTable();
	  }
	  continue;
	}
      case 'X':
        break;
      default:
        { loadStatement(c, fd, FALSE);
	  continue;
	}
    }
  }
}


static bool
loadStatement(int c, IOSTREAM *fd, int skip)
{ switch(c)
  { case 'P':
      return loadPredicate(fd, skip);

    case 'O':
    { word mname = loadXR(fd);
      Module om = modules.source;
      bool rval;

      modules.source = lookupModule(mname);
      rval = loadPredicate(fd, skip);
      modules.source = om;

      return rval;
    }
    case 'I':
      return loadImport(fd, skip);

    case 'D':
    { fid_t       cid = PL_open_foreign_frame();
      term_t goal = PL_new_term_ref();
      atom_t osf         = source_file_name;
      int  oln         = source_line_no;

      source_file_name = (currentSource ? currentSource->name : NULL_ATOM);
      source_line_no   = getNum(fd);
      
      loadQlfTerm(goal, fd);
      DEBUG(1, Sdprintf("Directive: ");
	       pl_write(goal);
	       Sdprintf("\n"));
      if ( !skip )
      { if ( !callProlog(MODULE_user, goal, FALSE) )
	{ Sfprintf(Serror,
		   "[WARNING: %s:%d: (loading %s) directive failed: ",
		   stringAtom(source_file_name), source_line_no, wicFile);
	  pl_write(goal);
	  Sfprintf(Serror, "]\n");
	}
      }
      PL_discard_foreign_frame(cid);
      
      source_file_name = osf;
      source_line_no   = oln;

      succeed;
    }	  

    case 'Q':
      return loadPart(fd, NULL, skip);

    case 'M':
      return loadInModule(fd, skip);

    default:
      return qlfLoadError(fd, "loadStatement()");
  }
}



static bool
loadPredicate(IOSTREAM *fd, int skip)
{ Procedure proc;
  Definition def;
  Clause clause;
  FunctorDef f = (FunctorDef) loadXR(fd);

  notifyPredicate(stringAtom(f->name), f->arity);
  proc = lookupProcedure(f, modules.source);
  DEBUG(3, Putf("Loading %s ", procedureName(proc)));
  def = proc->definition;
  def->indexPattern |= NEED_REINDEX;
  if ( !skip )
  { if ( SYSTEM_MODE )
    { set(def, SYSTEM|HIDE_CHILDS|LOCKED);
    }
    if ( currentSource )
      addProcedureSourceFile(currentSource, proc);
  }

  for(;;)
  { switch(Getc(fd) )
    { case 'X':
      { unsigned long pattern = getNum(fd);

	def->indexPattern = (pattern | NEED_REINDEX);

	DEBUG(3, Putf("ok\n"));
	succeed;
      }
      case 'C':
      { Code bp, ep;

	DEBUG(3, Sdprintf("."));
	clause = (Clause) allocHeap(sizeof(struct clause));
	clause->line_no = (unsigned short) getNum(fd);
	clearFlags(clause);
	clause->prolog_vars = (short) getNum(fd);
	clause->variables = (short) getNum(fd);
	if ( getNum(fd) == 0 )		/* 0: fact */
	  set(clause, UNIT_CLAUSE);
	clause->procedure = proc;
	clause->source_no = (currentSource ? currentSource->index : 0);
	clause->code_size = (short) getNum(fd);
	statistics.codes += clause->code_size;
	clause->codes = (Code) allocHeap(clause->code_size * sizeof(code));

	bp = clause->codes;
	ep = bp + clause->code_size;

	while( bp < ep )
	{ code op = getNum(fd);
	  int n = 0;
	  int narg = codeTable[op].arguments;
	  
	  *bp++ = encode(op);
	  switch(codeTable[op].argtype)
	  { case CA1_PROC:
	    { switch(op)
	      { case I_CALL:
		case I_DEPART:
		{ FunctorDef f = (FunctorDef)loadXR(fd);
		  *bp++ = (word) lookupProcedure(f, modules.source);
		  break;
		}
		default:
		  *bp++ = loadXR(fd);
	      }
	      n++;
	      break;
	    }
	    case CA1_FUNC:
	    case CA1_DATA:
	      *bp++ = loadXR(fd);
	      n++;
	      break;
	    case CA1_INTEGER:
	      *bp++ = getstdw(fd);
	      n++;
	      break;
	    case CA1_FLOAT:
	    { union { word w[2]; double f; } v;
	      v.f = getReal(fd);
	      *bp++ = v.w[0];
	      *bp++ = v.w[1];
	      n += 2;
	      break;
	    }
	    case CA1_STRING:		/* <n> chars */
	    { int l = getNum(fd);
	      int lw = (l+sizeof(word))/sizeof(word);
	      int pad = (lw*sizeof(word) - l);
	      char *s = (char *)&bp[1];

	      DEBUG(3, Sdprintf("String of %ld bytes\n", l));
	      *bp = mkStrHdr(lw, pad);
	      bp += lw;
	      *bp++ = 0L;
	      while(--l >= 0)
		*s++ = Getc(fd);
	      n++;
	      break;
	    }
	  }
	  for( ; n < narg; n++ )
	    *bp++ = getNum(fd);
	}

	if ( skip )
	  freeClause(clause);
	else
	{ assertProcedure(proc, clause, CL_END);
	}
      }
    }
  }
}


static bool
loadImport(IOSTREAM *fd, int skip)
{ Procedure proc = (Procedure) loadXR(fd);
  FunctorDef functor = proc->definition->functor;
  Procedure old;

  if ( !skip )
  { DEBUG(3, Sdprintf("loadImport(): %s into %s\n",
		      procedureName(proc), stringAtom(modules.source->name)));

    if ( (old = isCurrentProcedure(functor, modules.source)) )
    { if ( old->definition == proc->definition )
	succeed;			/* already done this! */
      
      if ( !isDefinedProcedure(old) )
      { old->definition = proc->definition;
	succeed;
      }

      return warning("Failed to import %s into %s", 
		     procedureName(proc), 
		     stringAtom(modules.source->name) );
    }
    addHTable(modules.source->procedures, functor, proc);
  }

  succeed;
}


static bool
qlfLoadSource(IOSTREAM *fd)
{ char *str = getString(fd);
  long time = getstdw(fd);
  int issys = (Qgetc(fd) == 's') ? TRUE : FALSE;
  atom_t fname;

  if ( qlf_has_moved && strprefix(str, qlf_save_dir) )
  { char buf[MAXPATHLEN];
    char *s;

    strcpy(buf, qlf_load_dir);
    s = &buf[strlen(buf)];
    *s++ = '/';
    strcpy(s, &str[strlen(qlf_save_dir)]);
    fname = lookupAtom(canonisePath(buf));
  } else
    fname = lookupAtom(canonisePath(str));

  DEBUG(1, if ( !streq(stringAtom(fname), str) )
	     Sdprintf("Replaced path %s --> %s\n", str, stringAtom(fname)));

  currentSource = lookupSourceFile(fname);
  currentSource->time = time;
  currentSource->system = issys;
  startConsult(currentSource);

  succeed;
}


static bool
loadPart(IOSTREAM *fd, Module *module, int skip)
{ Module om     = modules.source;
  SourceFile of = currentSource;
  int stchk     = debugstatus.styleCheck;

  switch(Qgetc(fd))
  { case 'M':
    { atom_t mname = loadXR(fd);

      switch( Qgetc(fd) )
      { case '-':
	{ modules.source = lookupModule(mname);
					/* TBD: clear module? */
	  break;
	}
	case 'F':
	{ atom_t fname;
	  Module m;

	  qlfLoadSource(fd);
	  fname = currentSource->name;

	  m = lookupModule(mname);
	  if ( m->file && m->file != currentSource )
	  { warning("%s:\n\tmodule \"%s\" already loaded from \"%s\" (skipped)",
		    wicFile, stringAtom(m->name), stringAtom(m->file->name));
	    skip = TRUE;
	    modules.source = m;
	  } else
	  { if ( !declareModule(mname, currentSource) )
	      fail;
	  }

	  if ( module )
	    *module = modules.source;

	  for(;;)
	  { switch(Qgetc(fd))
	    { case 'E':
	      { FunctorDef f = (FunctorDef) loadXR(fd);

		if ( !skip )
		{ Procedure proc = lookupProcedure(f, modules.source);

		  addHTable(modules.source->public, f, proc);
		} else
		{ if ( !lookupHTable(m->public, f) )
		  { warning("%s: skipped module \"%s\" lacks %s/%d",
			    wicFile,
			    stringAtom(m->name),
			    stringAtom(f->name),
			    f->arity);
		  }
		}

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
  { int c = Qgetc(fd);

    switch(c)
    { case 'X':
      { modules.source = om;
	currentSource  = of;
	debugstatus.styleCheck = stchk;
	systemMode(debugstatus.styleCheck & DOLLAR_STYLE);

	succeed;
      }
      default:
	loadStatement(c, fd, skip);
    }
  }

  succeed;
}


static bool
loadInModule(IOSTREAM *fd, int skip)
{ word mname = loadXR(fd);
  Module om = modules.source;

  modules.source = lookupModule(mname);
  
  for(;;)
  { int c = Qgetc(fd);

    switch(c)
    { case 'X':
      { modules.source = om;
	succeed;
      }
      default:
	loadStatement(c, fd, skip);
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
static long  savedXRTableId;		/* next id */

static void
putString(register char *s, IOSTREAM *fd)
{ while(*s)
  { Putc(*s, fd);
    s++;
  }

  Putc(EOS, fd);
}


static void
putAtom(atom_t a, IOSTREAM *fd)
{ putString(stringAtom(a), fd);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Number encoding:

	0 <= n <= 2^6	Direct storage in byte
	


- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
putNum(long int n, IOSTREAM *fd)
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
putstdw(word w, IOSTREAM *fd)
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
  Sputw(rval, fd);
#else
  Sputw(w, fd);
#endif
}


static void
putReal(real f, IOSTREAM *fd)
{ word *s = (word *)&f;

  DEBUG(3, Sdprintf("putReal(%f)\n", f));

#ifndef WORDS_BIGENDIAN
  putstdw(s[0], fd);
  putstdw(s[1], fd);
#else
  putstdw(s[1], fd);
  putstdw(s[0], fd);
#endif
}


static void
saveXR(word xr, IOSTREAM *fd)
{ Symbol s;
  long id;

  if ( isTaggedInt(xr) )		/* TBD: switch */
  { Putc(XR_INT, fd);
    putNum(valInt(xr), fd);
    return;
  } else if ( isBignum(xr) )
  { Putc(XR_BIGNUM, fd);
    putstdw(valBignum(xr), fd);
    return;
  } else if ( isReal(xr) )
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

  if ( isAtom(xr) )
  { Putc(XR_ATOM, fd);
    putAtom(xr, fd);
    DEBUG(3, Putf("XR(%d) = '%s'\n", id, stringAtom(xr)));
    return;
  }

  assert(0);
}


static void
saveXRFunctor(FunctorDef f, IOSTREAM *fd)
{ Symbol s;
  long id;

  if ( (s = lookupHTable(savedXRTable, f)) )
  { id = (int) s->value;
    Putc(XR_REF, fd);
    putNum(id, fd);
    return;
  }

  id = ++savedXRTableId;
  addHTable(savedXRTable, f, (void *)id);

  Putc(XR_FUNCTOR, fd);
  saveXR(f->name, fd);
  putNum(f->arity, fd);
  DEBUG(3, Putf("XR(%d) = %s/%d\n", id, stringAtom(f->name), f->arity));
}


static void
saveXRProc(Procedure p, IOSTREAM *fd)
{ Symbol s;
  long id;

  if ( (s = lookupHTable(savedXRTable, p)) )
  { id = (int) s->value;
    Putc(XR_REF, fd);
    putNum(id, fd);
    return;
  }

  id = ++savedXRTableId;
  addHTable(savedXRTable, p, (void *)id);

  Putc(XR_PRED, fd);
  saveXRFunctor(p->definition->functor, fd);
  saveXR(p->definition->module->name, fd);
  DEBUG(3, Putf("XR(%d) = proc %s\n", id, procedureName(p)));
}


static void
do_save_qlf_term(Word t, IOSTREAM *fd)
{ deRef(t);

  if ( isTerm(*t) )
  { FunctorDef f = functorTerm(*t);

    if ( f == FUNCTOR_var1 )
    { int id = valInt(argTerm(*t, 0));

      Putc('v', fd);
      putNum(id, fd);
    } else
    { Word q = argTermP(*t, 0);
      int n;

      Putc('t', fd);
      saveXRFunctor(f, fd);
      for(n=0; n < f->arity; n++, q++)
	do_save_qlf_term(q, fd);
    }
  } else
  { assert(isAtomic(*t));
    saveXR(*t, fd);
  }
}


static void
saveQlfTerm(term_t t, IOSTREAM *fd)
{ int nvars;
  fid_t cid = PL_open_foreign_frame();

  DEBUG(3, Putf("Saving "); pl_write(t); Putf(" from %d ... ", Stell(fd)));
  nvars = numberVars(t, FUNCTOR_var1, 0);
  putNum(nvars, fd);
  do_save_qlf_term(valTermRef(t), fd);	/* TBD */
  DEBUG(3, Putf("to %d\n", Stell(fd)));

  PL_discard_foreign_frame(cid);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
saveWicClause()  saves  a  clause  to  the  .qlf  file.   For  predicate
references of I_CALL and I_DEPART, we  cannot store the predicate itself
as this would lead to an inconsistency if   the .qlf file is loaded into
another context module.  Therefore we just   store the functor.  For now
this is ok as constructs of the   form  module:goal are translated using
the meta-call mechanism.  This needs consideration   if we optimise this
(which is not that likely as I	think  module:goal, where `module' is an
atom,  should  be  restricted  to  very    special  cases  and  toplevel
interaction.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
saveWicClause(Clause clause, IOSTREAM *fd)
{ Code bp, ep;

  Putc('C', fd);
  putNum(clause->line_no, fd);
  putNum(clause->prolog_vars, fd);
  putNum(clause->variables, fd);
  putNum(true(clause, UNIT_CLAUSE) ? 0 : 1, fd);
  putNum(clause->code_size, fd);

  bp = clause->codes;
  ep = bp + clause->code_size;

  while( bp < ep )
  { code op = decode(*bp++);
    int n = 0;

    putNum(op, fd);
    switch(codeTable[op].argtype)
    { case CA1_PROC:
      { Procedure p = (Procedure) *bp++;
	n++;
	switch(op)
	{ case I_CALL:
	  case I_DEPART:
	    saveXRFunctor(p->definition->functor, fd);
	    break;
	  default:
	    saveXRProc(p, fd);
	}
	break;
      }
      case CA1_FUNC:
      { FunctorDef f = (FunctorDef) *bp++;
	n++;
	saveXRFunctor(f, fd);
	break;
      }
      case CA1_DATA:
      { word xr = (word) *bp++;
	n++;
	saveXR(xr, fd);
	break;
      }
      case CA1_INTEGER:
      { putstdw(*bp++, fd);
	n++;
	break;
      }
      case CA1_FLOAT:
      { union { word w[2]; double f; } v;
	v.w[0] = *bp++;
	v.w[1] = *bp++;
	n += 2;
	putReal(v.f, fd);
	break;
      }
      case CA1_STRING:
      { word m = *bp;
	char *s = (char *)++bp;
	int wn = wsizeofInd(m);
	int l = wn*sizeof(word) - padHdr(m);
	bp += wn;

	putNum(l, fd);
	while(--l >= 0)
	  Putc(*s++&0xff, fd);
	n++;
	break;
      }
    }
    for( ; n < codeTable[op].arguments; n++ )
      putNum(*bp++, fd);
  }
}


		/********************************
		*         COMPILATION           *
		*********************************/

static long emulator_size;

static void
closeProcedureWic(IOSTREAM *fd)
{ if ( currentProc != (Procedure) NULL )
  { Putc('X', fd);
    putNum(currentProc->definition->indexPattern & ~NEED_REINDEX, fd);
    currentProc = (Procedure) NULL;
  }
}


static int
copyEmulator(IOSTREAM *out, IOSTREAM *in)
{ long emsize = -1;
  long sizepos;
  int n = 0, c;

  if ( (sizepos = Sseek(in, -2 * (long)sizeof(long), SIO_SEEK_END)) >= 0 )
  { long size, magic;

    size = getstdw(in);
    magic = getstdw(in);
    if ( magic == QLFMAGICNUM )
      emsize = sizepos - size;
    Sseek(in, 0, SIO_SEEK_SET);
  }

  while((c=Sgetc(in)) != EOF && n++ != emsize)
    Sputc(c, out);

  emulator_size = n;

  succeed;
}


static bool
openWic(const char *file, term_t args)
{ char *exec;

  int   localSize    = options.localSize;
  int   globalSize   = options.globalSize;
  int   trailSize    = options.trailSize;
  int   argumentSize = options.argumentSize;
  int	heapSize     = options.heapSize;
  char *goal         = options.goal;
  char *topLevel     = options.topLevel;
  char *initFile     = options.initFile;
  bool  standalone   = FALSE;

  if ( args )
  { TRY(parseSaveProgramOptions(args,
				&localSize,
				&globalSize,
				&trailSize,
				&argumentSize,
				&goal,
				&topLevel,
				&initFile,
			        NULL,
			        &standalone));
  }

  wicFile = (char *) file;

  DEBUG(1, Sdprintf("Open compiler output file %s\n", file));
  if ( (wicFd = Sopen_file(file, "wbr")) == (IOSTREAM *)NULL )
    return warning("Can not open %s: %s", file, OsError());
  mkWicFile = wicFile;
  DEBUG(1, Sdprintf("Searching for executable\n"));
  if ( loaderstatus.restored_state )
  { exec = stringAtom(loaderstatus.restored_state);
  } else
  { TRY( getSymbols() );
    exec = stringAtom(loaderstatus.orgsymbolfile);
  }
  DEBUG(1, Sdprintf("Executable = %s\n", exec));
  if ( !(exec = OsPath(AbsoluteFile(exec))) )
    fail;
  emulator_size = 0;
  if ( standalone )
  { IOSTREAM *exefd;

    DEBUG(1, Sdprintf("Including executable\n", exec));
    if ( (exefd = Sopen_file(exec, "rbr")) != NULL )
    { copyEmulator(wicFd, exefd);
    } else
      warning("Can not read emulator %s --- ignoring stand_alone(on)", exec);
  }      

  DEBUG(1, Sdprintf("Expanded executable = %s\n", exec));
/*Sfprintf(wicFd, "#!%s -x\n", exec);*/
#if OS2
  Sfprintf(wicFd, "/* Compiled SWI-Prolog Program */\r\n'@ECHO OFF'\r\nparse source . . name\r\n\"%s -x \" name arg(1)\r\nexit\r\n", exec);
#else
  Sfprintf(wicFd, "#!/bin/sh\n");
  Sfprintf(wicFd, "# SWI-Prolog version: %d.%d.%d\n",
	   PLVERSION/10000,
	   (PLVERSION/100)%100,
	   PLVERSION%100);
  Sfprintf(wicFd, "# SWI-Prolog save-version: %d\n", VERSION);
  Sfprintf(wicFd, "exec ${SWIPL-%s} -x $0 \"$@\"\n", exec);
  Sfprintf(wicFd, "# End Header\n");
#endif /* OS2 */
  DEBUG(2, Sdprintf("Magic  ...\n"));
  putString( saveMagic,            wicFd);
  DEBUG(2, Sdprintf("Numeric options ...\n"));
  putNum(   VERSION,              wicFd);
  putNum(   localSize,    	  wicFd);
  putNum(   globalSize,   	  wicFd);
  putNum(   trailSize,    	  wicFd);
  putNum(   argumentSize, 	  wicFd);
  putNum(   heapSize,		  wicFd);
  DEBUG(2, Sdprintf("String options ...\n"));
  putString(goal,          	  wicFd);
  putString(topLevel,      	  wicFd);
  putString(initFile, 	   	  wicFd);
  if ( systemDefaults.home )
    putString(systemDefaults.home,  wicFd);
  else
    putString("<no home>",  wicFd);

  currentProc    = (Procedure) NULL;
  currentSource  = (SourceFile) NULL;
  savedXRTable   = newHTable(256);
  savedXRTableId = 0;

  DEBUG(2, Sdprintf("Header complete ...\n"));
  succeed;
}  


static void
writeTrailer(IOSTREAM *fd)
{ long size = Stell(fd) - emulator_size;

  Putc('T', fd);
  putstdw(size, fd);
  putstdw(QLFMAGICNUM, fd);
}


static bool
closeWic()
{ bool rval;

  if (wicFd == (IOSTREAM *) NULL)
    fail;

  closeProcedureWic(wicFd);
  Putc('X', wicFd);
  destroyHTable(savedXRTable);
  savedXRTable = NULL;
  writeTrailer(wicFd);
  Sclose(wicFd);
  rval = MarkExecutable(wicFile);

  wicFd = NULL;
  wicFile = NULL;
  mkWicFile = NULL;

  return rval;
}

static bool
addClauseWic(term_t term, atom_t file)
{ Clause clause;
  sourceloc loc;

  loc.file = file;
  loc.line = source_line_no;

  if ( (clause = assert_term(term, CL_END, &loc)) )
  { IOSTREAM *s = wicFd;

    DEBUG(3, Sdprintf("WAM code:\n");
	     wamListClause(clause));

    if (clause->procedure != currentProc)
    { closeProcedureWic(s);
      currentProc = clause->procedure;

      if ( clause->procedure->definition->module != modules.source )
      { Putc('O', s);
	saveXR(clause->procedure->definition->module->name, s);
      } else
      { Putc('P', s);
      }

      saveXRFunctor(currentProc->definition->functor, s);
    }
    saveWicClause(clause, s);
    succeed;
  }

  Sdprintf("Failed to compile: "); pl_write(term); Sdprintf("\n");
  fail;
}

static bool
addDirectiveWic(term_t term, IOSTREAM *fd)
{ closeProcedureWic(fd);
  Putc('D', fd);
  putNum(source_line_no, fd);
  saveQlfTerm(term, fd);

  succeed;
}  


static bool
importWic(Procedure proc, IOSTREAM *fd)
{ closeProcedureWic(fd);

  Putc('I', fd);
  saveXRProc(proc, fd);

  succeed;
}

		 /*******************************
		 *	    PART MARKS		*
		 *******************************/

typedef struct source_mark *SourceMark;

struct source_mark
{ long	   file_index;
  SourceMark next;
};

static SourceMark source_mark_head = NULL;
static SourceMark source_mark_tail = NULL;

static void
initSourceMarks()
{ source_mark_head = source_mark_tail = NULL;
}


static void
sourceMark(IOSTREAM *s)
{ SourceMark pm = allocHeap(sizeof(struct source_mark));

  pm->file_index = Stell(s);
  pm->next = NULL;
  if ( source_mark_tail )
  { source_mark_tail->next = pm;
    source_mark_tail = pm;
  } else
  { source_mark_tail = source_mark_head = pm;
  }
}


static int
writeSourceMarks(IOSTREAM *s)
{ int n = 0;
  SourceMark pn, pm = source_mark_head;

  DEBUG(1, Sdprintf("Writing source marks: "));

  for( ; pm; pm = pn )
  { pn = pm->next;

    DEBUG(1, Sdprintf(" %d", pm->file_index));
    putstdw(pm->file_index, s);
    freeHeap(pm, sizeof(*pm));
    n++;
  }
  
  DEBUG(1, Sdprintf("Written %d marks\n", n));
  putstdw(n, s);

  return 0;
}


static int
qlfSourceInfo(IOSTREAM *s, long offset, term_t list)
{ char *str;
  term_t head = PL_new_term_ref();

  if ( Sseek(s, offset, SIO_SEEK_SET) != offset )
    return warning("%s: seek failed: %s", wicFile, OsError());
  if ( Getc(s) != 'F' || !(str=getString(s)) )
    return warning("QLF format error");
  
  return PL_unify_list(list, head, list) &&
         PL_unify_atom_chars(head, str);
}


static word
qlfInfo(const char *file,
	term_t cversion, term_t version,
	term_t files0)
{ IOSTREAM *s = NULL;
  int lversion;
  int nqlf, i;
  long *qlfstart = NULL;
  word rval = TRUE;
  term_t files = PL_copy_term_ref(files0);

  TRY(PL_unify_integer(cversion, VERSION));

  wicFile = (char *)file;

  if ( !(s = Sopen_file(file, "rbr")) )
    return warning("Can't open %s: %s", file, OsError());

  if ( !(lversion = qlfVersion(s)) )
  { Sclose(s);
    fail;
  }
    
  TRY(PL_unify_integer(version, lversion));

  if ( Sseek(s, -(int)sizeof(long), SIO_SEEK_END) < 0 )
    return warning("qlf_info/3: seek failed: %s", OsError());
  nqlf = getstdw(s);
  DEBUG(1, Sdprintf("Found %d sources at %d starting at", nqlf, rval));
  qlfstart = (long *)allocHeap(sizeof(long) * nqlf);
  Sseek(s, -(int)sizeof(long) * (nqlf+1), SIO_SEEK_END);
  for(i=0; i<nqlf; i++)
  { qlfstart[i] = getstdw(s);
    DEBUG(1, Sdprintf(" %d", qlfstart[i]));
  }
  DEBUG(1, Sdprintf("\n"));

  for(i=0; i<nqlf; i++)
  { if ( !qlfSourceInfo(s, qlfstart[i], files) )
    { rval = FALSE;
      goto out;
    }
  }

  rval = PL_unify_nil(files);

out:
  if ( qlfstart )
    freeHeap(qlfstart, sizeof(long) * nqlf);
  if ( s )
    Sclose(s);

  return rval;
}



word
pl_qlf_info(term_t file,
	    term_t cversion, term_t version,
	    term_t files)
{ char *name;
  char buf[MAXPATHLEN];

  if ( !(name = PL_get_filename(file, buf, sizeof(buf))) )
    return warning("qlf_info/3: instantiation fault");

   return qlfInfo(name, cversion, version, files);
}



		 /*******************************
		 *	NEW MODULE SUPPORT	*
		 *******************************/

static bool
qlfOpen(atom_t name)
{ char *absname;

  wicFile = stringAtom(name);
  if ( !(absname = AbsoluteFile(wicFile)) )
    fail;

  if ( !(wicFd = Sopen_file(wicFile, "wbr")) )
    return warning("qlf_open/1: can't open %s: %s", wicFile, OsError());

  mkWicFile = wicFile;

  putString(qlfMagic, wicFd);
  putNum(VERSION, wicFd);
  putString(absname, wicFd);

  currentProc    = (Procedure) NULL;
  currentSource  = (SourceFile) NULL;
  savedXRTable   = newHTable(256);
  savedXRTableId = 0;
  initSourceMarks();

  succeed;
}


static bool
qlfClose()
{ IOSTREAM *fd = wicFd;

  closeProcedureWic(fd);
  writeSourceMarks(fd);
  Sclose(fd);
  wicFd = NULL;
  mkWicFile = NULL;

  destroyHTable(savedXRTable);
  savedXRTable = NULL;
  
  succeed;
}


static int
qlfVersion(IOSTREAM *s)
{ char mbuf[100];
  char *magic;

  if ( !(magic = getMagicString(s, mbuf, sizeof(mbuf))) ||
       !streq(magic, qlfMagic) )
  { Sclose(s);
    return warning("%s: not a SWI-Prolog .qlf file", wicFile);
  }

  return getNum(s);
}



static bool
qlfLoad(char *file, Module *module)
{ IOSTREAM *fd;
  bool rval;
  int lversion;
  char *absloadname;
  char *abssavename;

  wicFile = file;
  if ( !(absloadname = AbsoluteFile(wicFile)) )
    fail;
  
  if ( !(fd = Sopen_file(file, "rbr")) )
    return warning("$qlf_load/1: can't open %s: %s", file, OsError());
  if ( !(lversion = qlfVersion(fd)) || lversion < LOADVERSION )
  { Sclose(fd);
    if ( lversion )
      warning("$qlf_load/1: %s bad version (file version = %d, prolog = %d)",
	      wicFile, lversion, VERSION);
    fail;
  }

  abssavename = getString(fd);
  if ( streq(absloadname, abssavename) )
  { qlf_has_moved = FALSE;
    qlf_load_dir = qlf_save_dir = NULL;
  } else
  { qlf_has_moved = TRUE;
    qlf_load_dir = stringAtom(lookupAtom(DirName(absloadname)));
    qlf_save_dir = stringAtom(lookupAtom(DirName(abssavename)));
  }

  if ( Qgetc(fd) != 'Q' )
    return qlfLoadError(fd, "qlfLoad()");

  pushXrIdTable();
  rval = loadPart(fd, module, FALSE);
  popXrIdTable();

  Sclose(fd);

  return rval;
}


static bool
qlfSaveSource(SourceFile f, IOSTREAM *fd)
{ sourceMark(fd);
  Putc('F', fd);
  putAtom(f->name, fd);
  putstdw(f->time, fd);
  Putc(f->system ? 's' : 'u', fd);

  currentSource = f;

  succeed;
}


static bool
qlfStartModule(Module m, IOSTREAM *fd)
{ Symbol s;

  closeProcedureWic(fd);
  Putc('Q', fd);
  Putc('M', fd);
  saveXR(m->name, fd);
  if ( m->file )
    qlfSaveSource(m->file, fd);
  else
    Putc('-', fd);

  for_table(s, m->public)
  { FunctorDef f = (FunctorDef)s->name;

    Putc('E', fd);
    saveXRFunctor(f, fd);
  } 

  Putc('X', fd);

  succeed;
}


static bool
qlfStartSubModule(Module m, IOSTREAM *fd)
{ closeProcedureWic(fd);
  Putc('M', fd);
  saveXR(m->name, fd);

  succeed;
}


static bool
qlfStartFile(SourceFile f, IOSTREAM *fd)
{ closeProcedureWic(fd);
  Putc('Q', fd);
  qlfSaveSource(f, fd);

  succeed;
}


static bool
qlfEndPart(IOSTREAM  *fd)
{ closeProcedureWic(fd);
  Putc('X', fd);

  succeed;
}


word
pl_qlf_start_module(term_t name)
{ if ( wicFd )
  { Module m;

    if ( !PL_get_module(name, &m) )
      return warning("qlf_start_module/1: argument must be an atom");
  
    return qlfStartModule(m, wicFd);
  }

  succeed;
}


word
pl_qlf_start_sub_module(term_t name)
{ if ( wicFd )
  { Module m;

    if ( !PL_get_module(name, &m) )
      return warning("qlf_start_sub_module/1: argument must be an atom");
  
    return qlfStartSubModule(m, wicFd);
  }

  succeed;
}


word
pl_qlf_start_file(term_t name)
{ if ( wicFd )
  { atom_t a;

    if ( !PL_get_atom(name, &a) )
      return warning("qlf_start_file/1: argument must be an atom");
  
    return qlfStartFile(lookupSourceFile(a), wicFd);
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
pl_qlf_open(term_t file)
{ atom_t a;

  if ( PL_get_atom(file, &a) )
    return qlfOpen(a);

  return warning("qlf_open/1: instantiation fault");
}


word
pl_qlf_close()
{ return qlfClose();
}


word
pl_qlf_load(term_t file, term_t module)
{ Module m, oldsrc = modules.source;
  char fbuf[MAXPATHLEN];
  char *fn;
  bool rval;
  term_t name = PL_new_term_ref();

  if ( !PL_strip_module(file, &m, name) )
    fail;
  if ( !(fn = PL_get_filename(name, fbuf, sizeof(fbuf))) )
    return warning("$qlf_load/2: instantiation fault");

  rval = qlfLoad(fn, &m);
  modules.source = oldsrc;

  if ( !rval )
    fail;

  if ( m )
    return PL_unify_atom(module, m->name);
  else
    return PL_unify_integer(module, 0);
}


		/********************************
		*        PROLOG SUPPORT         *
		*********************************/

word
pl_open_wic(term_t name, term_t options)
{ char *file;
  atom_t fname;

  if ( !(file = PL_get_filename(name, NULL, 0)) )
    fail;
  fname = lookupAtom(file);	/* ensure persistency */

  return openWic(stringAtom(fname), options);
}

word
pl_qlf_put_states()
{ if ( wicFd )
    putStates(wicFd);

  succeed;
}


word
pl_close_wic()
{ return closeWic();
}


word
pl_add_directive_wic(term_t term)
{ if ( wicFd )
  { if ( PL_is_variable(term) )
      return warning("$add_directive_wic/1: directive is a variable");

    return addDirectiveWic(term, wicFd);
  }

  succeed;
}


word
pl_import_wic(term_t module, term_t head)
{ if ( wicFd )
  { Module m;
    FunctorDef f;

    if ( !PL_get_module(module, &m) ||
	 !PL_get_functor(head, &f) )
      return warning("$import_wic/3: instantiation fault");

    return importWic(lookupProcedure(f, m), wicFd);
  }

  succeed;
}


word
pl_qlf_assert_clause(term_t ref)
{ if ( wicFd )
  { Clause clause;
    IOSTREAM *s = wicFd;

    if ( !PL_get_pointer(ref, (void **)&clause) ||
	 !inCore(clause) || !isClause(clause) )
      return warning("$qlf_assert_clause/1: Invalid clause reference");

    if ( clause->procedure != currentProc )
    { closeProcedureWic(s);
      currentProc = clause->procedure;

      if ( clause->procedure->definition->module != modules.source )
      { Putc('O', s);
	saveXR(clause->procedure->definition->module->name, s);
      } else
      { Putc('P', s);
      }

      saveXRFunctor(currentProc->definition->functor, s);
    }

    saveWicClause(clause, s);
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Check whether clause is  of  the  form   :-  directive.  If  so, put the
directive in directive and succeed. If the   term has no explicit module
tag, add one from the current source-module.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
directiveClause(term_t directive, term_t clause, const char *functor)
{ atom_t name;
  int arity;
  term_t d0 = PL_new_term_ref();
  functor_t f;

  if ( !PL_get_name_arity(clause, &name, &arity) ||
       arity != 1 ||
       !streq(stringAtom(name), functor) )
    fail;

  PL_get_arg(1, clause, d0);
  if ( PL_get_functor(d0, &f) && f == FUNCTOR_module2 )
    PL_put_term(directive, d0);
  else
  { term_t m = PL_new_term_ref();

    PL_put_atom(m, modules.source->name);
    PL_cons_functor(directive, FUNCTOR_module2, m, d0);
  }

  succeed;
}

/*  Compile an entire file into intermediate code.

 ** Thu Apr 28 13:44:43 1988  jan@swivax.UUCP (Jan Wielemaker)  */

static bool
compileFile(char *file)
{ char *path;
  term_t f = PL_new_term_ref();
  atom_t nf;

  DEBUG(1, Sdprintf("Boot compilation of %s\n", file));
  if ( !(path = AbsoluteFile(file)) )
    fail;
  DEBUG(2, Sdprintf("Expanded to %s\n", path));

  nf = lookupAtom(path);
  PL_put_atom(f, nf);
  DEBUG(2, Sdprintf("Opening\n"));
  if ( !pl_see(f) )
    fail;
  DEBUG(2, Sdprintf("pl_start_consult()\n"));
  pl_start_consult(f);
  qlfStartFile(lookupSourceFile(nf), wicFd);
  
  for(;;)
  { fid_t            cid = PL_open_foreign_frame();
    term_t         t = PL_new_term_ref();
    term_t directive = PL_new_term_ref();
    atom_t eof;

    DEBUG(2, Sdprintf("pl_read_clause() -> "));
    PL_put_variable(t);
    if ( !pl_read_clause(t) )		/* syntax error */
      continue;
    if ( PL_get_atom(t, &eof) && eof == ATOM_end_of_file )
      break;

    DEBUG(2, pl_write(t); pl_nl());

    if ( directiveClause(directive, t, ":-") )
    { DEBUG(1, Putf(":- "); pl_write(directive); Putf(".\n") );
      addDirectiveWic(directive, wicFd);
      callProlog(MODULE_user, directive, FALSE);
    } else if ( directiveClause(directive, t, "$:-") )
    { DEBUG(1, Putf("$:- "); pl_write(directive); Putf(".\n") );
      callProlog(MODULE_user, directive, FALSE);
    } else
      addClauseWic(t, nf);

    PL_discard_foreign_frame(cid);
  }

  qlfEndPart(wicFd);
  pl_seen();

  succeed;
}

bool
compileFileList(char *out, int argc, char **argv)
{ newOp("$:-", OP_FX, 1200);
  TRY(openWic(out, 0) );
  
  systemMode(TRUE);

  for(;argc > 0; argc--, argv++)
  { if (streq(argv[0], "-c") )
      break;
    compileFile(argv[0]);
  }

  status.autoload = TRUE;
  systemMode(FALSE);

  { predicate_t pred = PL_predicate("$load_additional_boot_files", 0, "user");

    PL_call_predicate(MODULE_user, TRUE, pred, 0);
  }

  return closeWic();
}


		/********************************
		*         STATE LISTS           *
		*********************************/

/*  Add a new state to the chain of states this Prolog session is build
    from. The file name is made absolute to avoid directory problems
    with incremental loading.
*/

static bool
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
putStates(IOSTREAM *fd)
{ State st;

  for(st = stateList; st; st = st->next)
  { Putc('W', fd);
    putString(st->name, fd);
  }

  succeed;
}
