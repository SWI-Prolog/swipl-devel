/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

/*#define O_DEBUG 1*/
#include "pl-incl.h"
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

static char *	getString(IOSTREAM *);
static long	getNum(IOSTREAM *);
static real	getReal(IOSTREAM *);
static bool	loadWicFd(IOSTREAM *);
static bool	loadPredicate(IOSTREAM *, int skip);
static bool	loadImport(IOSTREAM *, int skip);
static void	putString(char *, IOSTREAM *);
static void	putAtom(atom_t, IOSTREAM *);
static void	putNum(long, IOSTREAM *);
static void	putReal(real, IOSTREAM *);
static void	saveWicClause(Clause, IOSTREAM *);
static void	closeProcedureWic(IOSTREAM *);
static bool	addDirectiveWic(term_t, IOSTREAM *fd);
static bool	importWic(Procedure, IOSTREAM *fd);
static bool	compileFile(char *);
static word	loadXR(IOSTREAM *);
static word	loadXRc(int c, IOSTREAM *fd);
static bool	loadStatement(int c, IOSTREAM *fd, int skip);
static bool	loadPart(IOSTREAM *fd, Module *module, int skip);
static bool	loadInModule(IOSTREAM *fd, int skip);
static int	qlfVersion(IOSTREAM *s);
static void	openProcedureWic(Procedure proc, IOSTREAM *fd, atom_t sclass);
static atom_t	qlfFixSourcePath(const char *raw);

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

IF YOU CHANGE ANYTHING TO THIS FILE, SO THAT OLD WIC-FILES CAN NO LONGER
BE READ, PLEASE DO NOT FORGET TO INCREMENT THE VERSION NUMBER!

Below is an informal description of the format of a `.qlf' file:

<wic-file>	::=	<magic code>
			<version number>
			<bits-per-word>
			<home>				% a <string>
			{<statement>}
			'T'
----------------------------------------------------------------
<qlf-file>	::=	<qlf-magic>
			<version-number>
			<bits-per-word>
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
		      | 'P' <XR/functor> 		% predicate
			    <flags>
			    {<clause>} <pattern>
		      |	'O' <XR/modulename>		% pred out of module
			    <XR/functor>
			    <flags>
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
<flags>		::=	<num>				% Bitwise or of PRED_*
<clause>	::=	'C' <line_no> <# var>
			    <#n subclause> <#codes> <codes>
		      | 'X' 				% end of list
<XR>		::=	XR_REF     <num>		% XR id from table
			XR_ATOM    <len><chars>		% atom
			XR_INT     <num>		% number
			XR_FLOAT   <word>*		% real (double)
			XR_STRING  <string>		% string
			XR_FUNCTOR <XR/name> <num>	% functor
			XR_PRED    <XR/fdef> <XR/module>% predicate
			XR_MODULE  <XR/name>		% module
			XR_FILE	   's'|'u' <XR/atom> <time>
				   '-'
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

#define LOADVERSION 36			/* load all versions later >= X */
#define VERSION 36			/* save version number */
#define QLFMAGICNUM 0x716c7374		/* "qlst" on little-endian machine */

#define XR_REF     0			/* reference to previous */
#define XR_ATOM	   1			/* atom */
#define XR_FUNCTOR 2			/* functor */
#define XR_PRED	   3			/* procedure */
#define XR_INT     4			/* int */
#define XR_FLOAT   5			/* float */
#define XR_STRING  6			/* string */
#define XR_FILE	   7			/* source file */
#define XR_MODULE  8			/* a module */

#define PRED_SYSTEM	 0x01		/* system predicate */
#define PRED_HIDE_CHILDS 0x02		/* hide my childs */

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
notifyPredicate(functor_t f)
{ static char cur[] = "|/-\\";
  static int  n = 0;

  Sprintf(Soutput, "%c\b", cur[n++ & 0x3]);
}

#else /*!tos*/

#define notifyLoad(file)
#define notifyLoaded()
#define notifyPredicate(f)

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
static int	qlf_saved_version;	/* version used for save */

static bool
qlfLoadError(IOSTREAM *fd, char *ctx)
{ fatalError("%s: QLF format error at index = %ld", ctx, Stell(fd));

  fail;
}


static char *getstr_buffer;
static char *getstr_buffer_end;
static int   getstr_buffer_size = 512;

static char *
getString(IOSTREAM *fd)
{ char *s;
  Char c;

  if ( getstr_buffer == NULL )
  { if ( !(getstr_buffer = malloc(getstr_buffer_size)) )
      outOfCore();
    getstr_buffer_end = &getstr_buffer[getstr_buffer_size-1];
  }

  for( s = getstr_buffer; (*s = c = Getc(fd)) != EOS; s++ )
  { if ( s == getstr_buffer_end )
    { if ( !(getstr_buffer = realloc(getstr_buffer, getstr_buffer_size+512)) )
	outOfCore();
      s = &getstr_buffer[getstr_buffer_size-1];
      getstr_buffer_size += 512;
      getstr_buffer_end = &getstr_buffer[getstr_buffer_size-1];
    }
    if ( c == EOF )
      fatalError("Unexpected EOF on intermediate code file at offset %d",
		 Stell(fd));
  }

  return getstr_buffer;
}


static atom_t
getAtom(IOSTREAM *fd)
{ char buf[1024];
  char *tmp, *s;
  int len = getNum(fd);
  int i;
  atom_t a;

  if ( len < sizeof(buf) )
    tmp = buf;
  else
    tmp = allocHeap(len);
  
  for(s=tmp, i=0; i<len; i++)
  { int c = Getc(fd);

    if ( c == EOF )
      fatalError("Unexpected EOF on intermediate code file at offset %d",
		 Stell(fd));
    *s++ = c;
  }
  a = lookupAtom(tmp, len);

  if ( tmp != buf )
    freeHeap(tmp, len);

  return a;
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

  if ( !(first & 0xc0) )		/* 99% of them: speed up a bit */    
  { first <<= (LONGBITSIZE-6);
    first >>= (LONGBITSIZE-6);

    DEBUG(4, Sdprintf("getNum() --> %ld\n", first));
    return first;
  }

  bytes = (int) ((first >> 6) & 0x3);
  first &= 0x3f;

  if ( bytes <= 2 )
  { for( b = 0; b < bytes; b++ )
    { first <<= 8;
      first |= Getc(fd) & 0xff;
    }

    shift = (sizeof(long)-1-bytes)*8 + 2;
  } else
  { int m;

    bytes = first;
    first = 0L;

    for(m=0; m<bytes; m++)
    { first <<= 8;
      first |= Getc(fd) & 0xff;
    }
    shift = (sizeof(long)-bytes)*8;
  }

  first <<= shift;
  first >>= shift;
  
  DEBUG(4, Sdprintf("getNum() --> %ld\n", first));
  return first;
}


#ifdef WORDS_BIGENDIAN
static const int double_byte_order[] = { 7,6,5,4,3,2,1,0 };
#else
static const int double_byte_order[] = { 0,1,2,3,4,5,6,7 };
#endif

#define BYTES_PER_DOUBLE (sizeof(double_byte_order)/sizeof(int))

static real
getReal(IOSTREAM *fd)
{ real f;
  unsigned char *cl = (unsigned char *)&f;
  int i;

  for(i=0; i<BYTES_PER_DOUBLE; i++)
  { int c = Sgetc(fd);
    
    if ( c == -1 )
      fatalError("Unexpected end-of-file in QLT file");
    cl[double_byte_order[i]] = c;
  }
  
  DEBUG(3, Sdprintf("getReal() --> %f\n", f));

  return f;
}


unsigned long
getLong(IOSTREAM *s)
{ unsigned long v;

  v  = (Sgetc(s) & 0xff) << 24;
  v |= (Sgetc(s) & 0xff) << 16;
  v |= (Sgetc(s) & 0xff) << 8;
  v |= (Sgetc(s) & 0xff);

  return v;
}


static word
loadXRc(int c, IOSTREAM *fd)
{ word xr;
  int id = 0;				/* make gcc happy! */

  switch( c )
  { case XR_REF:
    { long xr  = getNum(fd);
      word val = lookupXrId(xr);

      return val;
    }
    case XR_ATOM:
    { id = ++loadedXRTableId;
      xr = getAtom(fd);
      DEBUG(3, Sdprintf("XR(%d) = '%s'\n", id, stringAtom(xr)));
      break;
    }
    case XR_FUNCTOR:
    { atom_t name;
      int arity;

      id = ++loadedXRTableId;
      name = loadXR(fd);
      arity = getNum(fd);
      xr = (word) lookupFunctorDef(name, arity);
      DEBUG(3, Sdprintf("XR(%d) = %s/%d\n", id, stringAtom(name), arity));
      break;
    }
    case XR_PRED:
    { functor_t f;
      Module m;

      id = ++loadedXRTableId;
      f = (functor_t) loadXR(fd);
      m = (Module) loadXR(fd);
      xr = (word) lookupProcedure(f, m);
      DEBUG(3, Sdprintf("XR(%d) = proc %s\n", id, procedureName((Procedure)xr)));
      break;
    }
    case XR_MODULE:
    { atom_t name;
      id = ++loadedXRTableId;
      name = loadXR(fd);
      xr = (word) lookupModule(name);
      DEBUG(3, Sdprintf("XR(%d) = module %s\n", id, stringAtom(name)));
      break;
    }
    case XR_INT:
      return makeNum(getNum(fd));
    case XR_FLOAT:
      return globalReal(getReal(fd));
#if O_STRING
    case XR_STRING:
      return globalString(getString(fd));
#endif
    case XR_FILE:
    { int c;

      id = ++loadedXRTableId;

      switch( (c=Qgetc(fd)) )
      { case 'u':
	case 's':
	{ atom_t name   = loadXR(fd);
	  word   time   = getNum(fd);
	  const char *s = stringAtom(name);
	  SourceFile sf = lookupSourceFile(qlfFixSourcePath(s));

	  if ( !sf->time )
	  { sf->time   = time;
	    sf->system = (c == 's' ? TRUE : FALSE);
	  }
	  xr = (word)sf;
	  break;
	}
	case '-':
	  xr = 0;
	  break;
	default:
	  xr = 0;			/* make gcc happy */
	  fatalError("Illegal XR file index %d: %c", Stell(fd)-1, c);
      }

      break;
    }
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
  { functor_t f = (functor_t) loadXR(fd);
    term_t c2 = PL_new_term_ref();
    int arity = arityFunctor(f);
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

  DEBUG(3, Sdprintf("Loading from %d ...", Stell(fd)));

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
  DEBUG(3,
	Sdprintf("Loaded ");
	PL_write_term(Serror, term, 1200, 0);
	Sdprintf(" to %d\n", Stell(fd)));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Load intermediate code state from the specified stream.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
loadWicFromStream(IOSTREAM *fd)
{ pushXrIdTable();
  if ( !loadWicFd(fd) )
  { popXrIdTable();
    return FALSE;
  }
  popXrIdTable();

  return TRUE;
}


static int
loadWicFile(const char *file)
{ IOSTREAM *fd;
  int rval;

  if ( !(fd = Sopen_file(file, "rbr")) )
  { warning("Cannot open Quick Load File %s: %s", file, OsError());
    return FALSE;
  }

  rval = loadWicFromStream(fd);
  Sclose(fd);

  return rval;
}


#define QLF_MAX_HEADER_LINES 100

static bool
loadWicFd(IOSTREAM *fd)
{ char *s;
  Char c;
  char mbuf[100];
  char *savedhome;
  int saved_wsize;

  s = getMagicString(fd, mbuf, sizeof(mbuf));
  if ( !s || !streq(s, saveMagic) )
    return fatalError("Not a SWI-Prolog saved state");

  if ( (qlf_saved_version=getNum(fd)) < LOADVERSION )
  { fatalError("Saved state has incompatible save version");
    fail;
  }

  saved_wsize = getNum(fd);
  if ( saved_wsize != sizeof(word)*8 )
  { fatalError("Saved state has incompatible (%d) word-length", saved_wsize);
    fail;
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
	{ char *name = store_string(getString(fd) );

	  loadWicFile(name);
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
      Module om = LD->modules.source;
      bool rval;

      LD->modules.source = lookupModule(mname);
      rval = loadPredicate(fd, skip);
      LD->modules.source = om;

      return rval;
    }
    case 'I':
      return loadImport(fd, skip);

    case 'D':
    { fid_t   cid = PL_open_foreign_frame();
      term_t goal = PL_new_term_ref();
      atom_t  osf = source_file_name;
      int     oln = source_line_no;

      source_file_name = (currentSource ? currentSource->name : NULL_ATOM);
      source_line_no   = getNum(fd);
      
      loadQlfTerm(goal, fd);
      DEBUG(1, Sdprintf("Directive: ");
	       pl_write(goal);
	       Sdprintf("\n"));
      if ( !skip )
      { if ( !callProlog(MODULE_user, goal, PL_Q_NODEBUG, NULL) )
	{ Sfprintf(Serror,
		   "[WARNING: %s:%d: (loading %s) directive failed: ",
		   source_file_name ? stringAtom(source_file_name)
		                    : "<no file>",
		   source_line_no, wicFile);
	  PL_write_term(Serror, goal, 1200, 0);
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


static void
loadPredicateFlags(Definition def, IOSTREAM *fd, int skip)
{ if ( qlf_saved_version <= 31 )
  { if ( !skip && SYSTEM_MODE )
      set(def, SYSTEM|HIDE_CHILDS|LOCKED);
  } else
  { int	flags = getNum(fd);

    if ( !skip )
    { unsigned long lflags = 0L;

      if ( flags & PRED_SYSTEM )
	lflags |= SYSTEM;
      if ( flags & PRED_HIDE_CHILDS )
	lflags |= HIDE_CHILDS;
    
      set(def, lflags);
    }
  }
}


static bool
loadPredicate(IOSTREAM *fd, int skip)
{ Procedure proc;
  Definition def;
  Clause clause;
  functor_t f = (functor_t) loadXR(fd);

  notifyPredicate(f);
  proc = lookupProcedure(f, LD->modules.source);
  DEBUG(3, Sdprintf("Loading %s ", procedureName(proc)));
  def = proc->definition;
  if ( !skip && currentSource )
  { if ( def->definition.clauses )
      redefineProcedure(proc, currentSource);
    addProcedureSourceFile(currentSource, proc);
  }
  if ( def->references == 0 && !def->hash_info )
    def->indexPattern |= NEED_REINDEX;
  loadPredicateFlags(def, fd, skip);

  for(;;)
  { switch(Getc(fd) )
    { case 'X':
      { unsigned long pattern = getNum(fd);

	if ( (def->indexPattern & ~NEED_REINDEX) != pattern )
	{ if ( def->references == 0 && !def->hash_info )
	    def->indexPattern = (pattern | NEED_REINDEX);
	  else if ( false(def, MULTIFILE|DYNAMIC) )
	    Sdprintf("Cannot change indexing of %s\n", predicateName(def));
	}

	DEBUG(3, Sdprintf("ok\n"));
	succeed;
      }
      case 'C':
      { Code bp, ep;

	DEBUG(3, Sdprintf("."));
	clause = (Clause) allocHeap(sizeof(struct clause));
	clause->line_no = (unsigned short) getNum(fd);
	if ( qlf_saved_version < 32 )
	  clause->source_no = (currentSource ? currentSource->index : 0);
	else
	{ SourceFile sf = (void *) loadXR(fd);
	  int sno = (sf ? sf->index : 0);

	  clause->source_no = sno;
	}
	clearFlags(clause);
	clause->prolog_vars = (short) getNum(fd);
	clause->variables = (short) getNum(fd);
	if ( getNum(fd) == 0 )		/* 0: fact */
	  set(clause, UNIT_CLAUSE);
	clause->procedure = proc;
	clause->code_size = (short) getNum(fd);
	GD->statistics.codes += clause->code_size;
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
	    { *bp++ = loadXR(fd);
	      n++;
	      break;
	    }
	    case CA1_FUNC:
	    case CA1_DATA:
	    { word w = loadXR(fd);
	      if ( isAtom(w) )
		PL_register_atom(w);
	      *bp++ = w;
	      n++;
	      break;
	    }
	    case CA1_MODULE:
	      *bp++ = loadXR(fd);
	      n++;
	      break;
	    case CA1_INTEGER:
	      *bp++ = getNum(fd);
	      n++;
	      break;
	    case CA1_FLOAT:
	    { union
	      { word w[WORDS_PER_DOUBLE];
		double f;
	      } v;
	      Word p = v.w;
	      v.f = getReal(fd);
	      cpDoubleData(bp, p);
	      n += WORDS_PER_DOUBLE;
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
	{ if ( def->hash_info )
	  { reindexClause(clause);
	  }
	  assertProcedure(proc, clause, CL_END);
	}
      }
    }
  }
}


static bool
loadImport(IOSTREAM *fd, int skip)
{ Procedure proc = (Procedure) loadXR(fd);
  functor_t functor = proc->definition->functor->functor;
  Procedure old;

  if ( !skip )
  { DEBUG(3, Sdprintf("loadImport(): %s into %s\n",
		      procedureName(proc), stringAtom(LD->modules.source->name)));

    if ( (old = isCurrentProcedure(functor, LD->modules.source)) )
    { if ( old->definition == proc->definition )
	succeed;			/* already done this! */
      
      if ( !isDefinedProcedure(old) )
      { old->definition = proc->definition;
	succeed;
      }

      return warning("Failed to import %s into %s", 
		     procedureName(proc), 
		     stringAtom(LD->modules.source->name) );
    }
    addHTable(LD->modules.source->procedures, (void *)functor, proc);
  }

  succeed;
}


static atom_t
qlfFixSourcePath(const char *raw)
{ char buf[MAXPATHLEN];
    
  if ( qlf_has_moved && strprefix(raw, qlf_save_dir) )
  { char *s;
    int lensave = strlen(qlf_save_dir);
    const char *tail = &raw[lensave];

    if ( strlen(qlf_load_dir)+1+strlen(tail)+1 > MAXPATHLEN )
      fatalError("Path name too long: %s", raw);

    strcpy(buf, qlf_load_dir);
    s = &buf[strlen(buf)];
    *s++ = '/';
    strcpy(s, tail);
  } else
  { if ( strlen(raw)+1 > MAXPATHLEN )
      fatalError("Path name too long: %s", raw);
    strcpy(buf, raw);
  }

  return PL_new_atom(canonisePath(buf));
}


static bool
qlfLoadSource(IOSTREAM *fd)
{ char *str = getString(fd);
  long time = getNum(fd);
  int issys = (Qgetc(fd) == 's') ? TRUE : FALSE;
  atom_t fname;

  fname = qlfFixSourcePath(str);

  DEBUG(1, if ( !streq(stringAtom(fname), str) )
	     Sdprintf("Replaced path %s --> %s\n", str, stringAtom(fname)));

  currentSource = lookupSourceFile(fname);
  currentSource->time = time;
  currentSource->system = issys;
  startConsult(currentSource);
  PL_unregister_atom(fname);		/* locked with sourceFile */

  succeed;
}


static bool
loadPart(IOSTREAM *fd, Module *module, int skip)
{ Module om     = LD->modules.source;
  SourceFile of = currentSource;
  int stchk     = debugstatus.styleCheck;

  switch(Qgetc(fd))
  { case 'M':
    { atom_t mname = loadXR(fd);

      switch( Qgetc(fd) )
      { case '-':
	{ LD->modules.source = lookupModule(mname);
					/* TBD: clear module? */
	  break;
	}
	case 'F':
	{ Module m;

	  qlfLoadSource(fd);

	  m = lookupModule(mname);
	  if ( m->file && m->file != currentSource )
	  { warning("%s:\n\tmodule \"%s\" already loaded from \"%s\" (skipped)",
		    wicFile, stringAtom(m->name), stringAtom(m->file->name));
	    skip = TRUE;
	    LD->modules.source = m;
	  } else
	  { if ( !declareModule(mname, currentSource) )
	      fail;
	  }

	  if ( module )
	    *module = LD->modules.source;

	  for(;;)
	  { switch(Qgetc(fd))
	    { case 'E':
	      { functor_t f = (functor_t) loadXR(fd);

		if ( !skip )
		{ Procedure proc = lookupProcedure(f, LD->modules.source);

		  addHTable(LD->modules.source->public, (void *)f, proc);
		} else
		{ if ( !lookupHTable(m->public, (void *)f) )
		  { FunctorDef fd = valueFunctor(f);

		    warning("%s: skipped module \"%s\" lacks %s/%d",
			    wicFile,
			    stringAtom(m->name),
			    stringAtom(fd->name),
			    fd->arity);
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
      { LD->modules.source = om;
	currentSource  = of;
	debugstatus.styleCheck = stchk;
	systemMode(debugstatus.styleCheck & DOLLAR_STYLE);

	succeed;
      }
      default:
	loadStatement(c, fd, skip);
    }
  }
}


static bool
loadInModule(IOSTREAM *fd, int skip)
{ word mname = loadXR(fd);
  Module om = LD->modules.source;

  LD->modules.source = lookupModule(mname);
  
  for(;;)
  { int c = Qgetc(fd);

    switch(c)
    { case 'X':
      { LD->modules.source = om;
	succeed;
      }
      default:
	loadStatement(c, fd, skip);
    }
  }
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
putString(char *s, IOSTREAM *fd)
{ while(*s)
  { Sputc(*s, fd);
    s++;
  }

  Sputc(EOS, fd);
}


static void
putAtom(atom_t w, IOSTREAM *fd)
{ Atom a = atomValue(w);
  const char *s = a->name;
  const char *e = s+a->length;

  putNum(a->length, fd);
  for( ; s<e; s++ )
    Sputc(*s, fd);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Number encoding:

First byte:  bits 8&7  bits 1-6 (low order)

		0	6-bits signed value
		1      14-bits signed value
		2      22-bits signed value
		3      number of bytes following
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define PLMINLONG   ((long)(1L<<(LONGBITSIZE-1)))

static void
putNum(long n, IOSTREAM *fd)
{ int m;
  long absn = (n >= 0 ? n : -n);

  DEBUG(8, Sdprintf("0x%x at %ld\n", (unsigned long)n, Stell(fd)));

  if ( n != PLMINLONG )
  { if ( absn < (1L << 5) )
    { Sputc((n & 0x3f), fd);
      return;
    } else if ( absn < (1L << 13) )
    { Sputc((((n >> 8) & 0x3f) | (1 << 6)), fd);
      Sputc((n & 0xff), fd);
      return;
    } else if ( absn < (1L << 21) )
    { Sputc((((n >> 16) & 0x3f) | (2 << 6)), fd);
      Sputc(((n >> 8) & 0xff), fd);
      Sputc((n & 0xff), fd);
      return;
    }
  }

  for(m = sizeof(n); ; m--)
  { int b = (absn >> (((m-1)*8)-1)) & 0x1ff;

    if ( b == 0 )
      continue;
    break;
  }

  Sputc(m | (3 << 6), fd);

  for( ; m > 0; m--)
  { int b = (n >> ((m-1)*8)) & 0xff;
    
    Sputc(b, fd);
  }
}


static void
putReal(real f, IOSTREAM *fd)
{ unsigned char *cl = (unsigned char *)&f;
  int i;

  DEBUG(3, Sdprintf("putReal(%f)\n", f));

  for(i=0; i<BYTES_PER_DOUBLE; i++)
    Sputc(cl[double_byte_order[i]], fd);
}


static void
putLong(unsigned long v, IOSTREAM *fd)	/* always 4 bytes */
{ Sputc((v>>24)&0xff, fd);
  Sputc((v>>16)&0xff, fd);
  Sputc((v>>8)&0xff, fd);
  Sputc(v&0xff, fd);
}


static int
savedXR(void *xr, IOSTREAM *fd)
{ Symbol s;
  long id;

  if ( (s = lookupHTable(savedXRTable, xr)) )
  { id = (long) s->value;
    Sputc(XR_REF, fd);
    putNum(id, fd);
    
    succeed;
  } else
  { id = ++savedXRTableId;
    addHTable(savedXRTable, xr, (void *)id);
  }

  fail;
}


static void
saveXR(word xr, IOSTREAM *fd)
{ if ( isTaggedInt(xr) )		/* TBD: switch */
  { Sputc(XR_INT, fd);
    putNum(valInt(xr), fd);
    return;
  } else if ( isBignum(xr) )
  { Sputc(XR_INT, fd);
    putNum(valBignum(xr), fd);
    return;
  } else if ( isReal(xr) )
  { Sputc(XR_FLOAT, fd);
    putReal(valReal(xr), fd);
    return;
#if O_STRING
  } else if ( isString(xr) )
  { Sputc(XR_STRING, fd);
    putString(valString(xr), fd);
    return;
#endif /* O_STRING */
  }

  if ( savedXR((void *)xr, fd) )
    return;

  if ( isAtom(xr) )
  { DEBUG(3, Sdprintf("XR(%d) = '%s'\n", savedXRTableId, stringAtom(xr)));
    Sputc(XR_ATOM, fd);
    putAtom(xr, fd);
    return;
  }

  assert(0);
}


static void
saveXRModule(Module m, IOSTREAM *fd)
{ if ( savedXR((void *)m, fd) )
    return;

  Sputc(XR_MODULE, fd);
  DEBUG(3, Sdprintf("XR(%d) = module %s\n", savedXRTableId, stringAtom(m->name)));
  saveXR(m->name, fd);
}


static void
saveXRFunctor(functor_t f, IOSTREAM *fd)
{ FunctorDef fdef;

  if ( savedXR((void *)f, fd) )
    return;

  fdef = valueFunctor(f);

  DEBUG(3, Sdprintf("XR(%d) = %s/%d\n",
		savedXRTableId, stringAtom(fdef->name), fdef->arity));
  Sputc(XR_FUNCTOR, fd);
  saveXR(fdef->name, fd);
  putNum(fdef->arity, fd);
}


static void
saveXRProc(Procedure p, IOSTREAM *fd)
{ if ( savedXR(p, fd) )
    return;

  DEBUG(3, Sdprintf("XR(%d) = proc %s\n", savedXRTableId, procedureName(p)));
  Sputc(XR_PRED, fd);
  saveXRFunctor(p->definition->functor->functor, fd);
  saveXRModule(p->definition->module, fd);
}


static void
saveXRSourceFile(SourceFile f, IOSTREAM *fd)
{ if ( savedXR(f, fd) )
    return;

  Sputc(XR_FILE, fd);

  if ( f )
  { DEBUG(3, Sdprintf("XR(%d) = file %s\n", savedXRTableId, stringAtom(f->name)));
    Sputc(f->system ? 's' : 'u', fd);
    saveXR(f->name, fd);
    putNum(f->time, fd);
  } else
  { DEBUG(3, Sdprintf("XR(%d) = <no file>\n", savedXRTableId));
    Sputc('-', fd);  
  }
}



static void
do_save_qlf_term(Word t, IOSTREAM *fd)
{ deRef(t);

  if ( isTerm(*t) )
  { functor_t f = functorTerm(*t);

    if ( f == FUNCTOR_var1 )
    { int id = valInt(argTerm(*t, 0));

      Sputc('v', fd);
      putNum(id, fd);
    } else
    { Word q = argTermP(*t, 0);
      int n, arity = arityFunctor(f);

      Sputc('t', fd);
      saveXRFunctor(f, fd);
      for(n=0; n < arity; n++, q++)
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
  fid_t cid;

  cid = PL_open_foreign_frame();

  DEBUG(3,
	Sdprintf("Saving ");
	PL_write_term(Serror, t, 1200, 0);
	Sdprintf(" from %d ... ", Stell(fd)));
  nvars = numberVars(t, FUNCTOR_var1, 0);
  putNum(nvars, fd);
  do_save_qlf_term(valTermRef(t), fd);	/* TBD */
  DEBUG(3, Sdprintf("to %d\n", Stell(fd)));

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

  Sputc('C', fd);
  putNum(clause->line_no, fd);
  saveXRSourceFile(indexToSourceFile(clause->source_no), fd);
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
	saveXRProc(p, fd);
	break;
      }
      case CA1_MODULE:
      { Module m = (Module) *bp++;
	n++;
	saveXRModule(m, fd);
	break;
      }
      case CA1_FUNC:
      { functor_t f = (functor_t) *bp++;
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
      { putNum(*bp++, fd);
	n++;
	break;
      }
      case CA1_FLOAT:
      { union
	{ word w[WORDS_PER_DOUBLE];
	  double f;
	} v;
	Word p = v.w;
	cpDoubleData(p, bp);
	n += WORDS_PER_DOUBLE;
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
	  Sputc(*s++&0xff, fd);
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

static void
closeProcedureWic(IOSTREAM *fd)
{ if ( currentProc != (Procedure) NULL )
  { Sputc('X', fd);
    putNum(currentProc->definition->indexPattern & ~NEED_REINDEX, fd);
    currentProc = (Procedure) NULL;
  }
}


static int
predicateFlags(Definition def, atom_t sclass)
{ int flags = 0;
  
  if ( sclass == ATOM_kernel )
  { if ( true(def, SYSTEM) && false(def, HIDE_CHILDS) )
      return PRED_SYSTEM;
    return (PRED_SYSTEM|PRED_HIDE_CHILDS);
  }

  if ( true(def, SYSTEM) )
    flags |= PRED_SYSTEM;
  if ( true(def, HIDE_CHILDS) )
    flags |= PRED_HIDE_CHILDS;
  
  return flags;
}


static void
openProcedureWic(Procedure proc, IOSTREAM *fd, atom_t sclass)
{ if ( proc != currentProc)
  { Definition def = proc->definition;
    int mode = predicateFlags(def, sclass);

    closeProcedureWic(fd);
    currentProc = proc;

    if ( def->module != LD->modules.source )
    { Sputc('O', fd);
      saveXR(def->module->name, fd);
    } else
    { Sputc('P', fd);
    }

    saveXRFunctor(def->functor->functor, fd);
    putNum(mode, fd);
  }
}


static bool
writeWicHeader(IOSTREAM *fd)
{ wicFd = fd;

  putString(saveMagic, fd);
  putNum(VERSION, fd);
  putNum(sizeof(word)*8, fd);	/* bits-per-word */
  if ( systemDefaults.home )
    putString(systemDefaults.home,  fd);
  else
    putString("<no home>",  fd);

  currentProc    = (Procedure) NULL;
  currentSource  = (SourceFile) NULL;
  savedXRTable   = newHTable(256);
  savedXRTableId = 0;

  DEBUG(2, Sdprintf("Header complete ...\n"));
  succeed;
}  


static bool
writeWicTrailer(IOSTREAM *fd)
{ if ( !fd )
    fail;

  closeProcedureWic(fd);
  Sputc('X', fd);
  destroyHTable(savedXRTable);
  savedXRTable = NULL;
  Sputc('T', fd);

  wicFd = NULL;
  wicFile = NULL;

  succeed;
}

static bool
addClauseWic(term_t term, atom_t file)
{ Clause clause;
  sourceloc loc;

  loc.file = file;
  loc.line = source_line_no;

  if ( (clause = assert_term(term, CL_END, &loc)) )
  { IOSTREAM *s = wicFd;

#ifdef O_DEBUGGER
    DEBUG(3, Sdprintf("WAM code:\n");
	     wamListClause(clause));
#endif

    openProcedureWic(clause->procedure, s, ATOM_development);
    saveWicClause(clause, s);

    succeed;
  }

  Sdprintf("Failed to compile: "); pl_write(term); Sdprintf("\n");
  fail;
}

static bool
addDirectiveWic(term_t term, IOSTREAM *fd)
{ closeProcedureWic(fd);
  Sputc('D', fd);
  putNum(source_line_no, fd);
  saveQlfTerm(term, fd);

  succeed;
}  


static bool
importWic(Procedure proc, IOSTREAM *fd)
{ closeProcedureWic(fd);

  Sputc('I', fd);
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
{ unsigned int n = 0;
  SourceMark pn, pm = source_mark_head;

  DEBUG(1, Sdprintf("Writing source marks: "));

  for( ; pm; pm = pn )
  { pn = pm->next;

    DEBUG(1, Sdprintf(" %d", pm->file_index));
    putLong(pm->file_index, s);
    freeHeap(pm, sizeof(*pm));
    n++;
  }
  
  DEBUG(1, Sdprintf("Written %d marks\n", n));
  putLong(n, s);

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
  { term_t f = PL_new_term_ref();

    PL_put_atom_chars(f, file);
    return PL_error(NULL, 0, OsError(), ERR_FILE_OPERATION,
		    ATOM_open, ATOM_source_sink, f);
  }    

  if ( !(lversion = qlfVersion(s)) )
  { Sclose(s);
    fail;
  }
    
  TRY(PL_unify_integer(version, lversion));

  if ( Sseek(s, -4, SIO_SEEK_END) < 0 )	/* 4 bytes of putLong() */
    return warning("qlf_info/3: seek failed: %s", OsError());
  nqlf = getLong(s);
  DEBUG(1, Sdprintf("Found %d sources at %d starting at", nqlf, rval));
  qlfstart = (long *)allocHeap(sizeof(long) * nqlf);
  Sseek(s, -4 * (nqlf+1), SIO_SEEK_END);
  for(i=0; i<nqlf; i++)
  { qlfstart[i] = getLong(s);
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
    fail;

   return qlfInfo(name, cversion, version, files);
}



		 /*******************************
		 *	NEW MODULE SUPPORT	*
		 *******************************/

static bool
qlfOpen(atom_t name)
{ char *absname;
  char tmp[MAXPATHLEN];

  wicFile = stringAtom(name);
  if ( !(absname = AbsoluteFile(wicFile, tmp)) )
    fail;

  if ( !(wicFd = Sopen_file(wicFile, "wbr")) )
    return warning("qlf_open/1: can't open %s: %s", wicFile, OsError());

  mkWicFile = wicFile;

  putString(qlfMagic, wicFd);
  putNum(VERSION, wicFd);
  putNum(sizeof(word)*8, wicFd);

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
  char tmp[MAXPATHLEN];
  int saved_wsize;

  wicFile = file;
  if ( !(absloadname = AbsoluteFile(wicFile, tmp)) )
    fail;
  
  if ( !(fd = Sopen_file(file, "rbr")) )
  { term_t f = PL_new_term_ref();

    PL_put_atom_chars(f, file);
    return PL_error(NULL, 0, OsError(), ERR_FILE_OPERATION,
		    ATOM_open, ATOM_source_sink, f);
  }
  if ( !(lversion = qlfVersion(fd)) || lversion < LOADVERSION )
  { Sclose(fd);
    if ( lversion )
      warning("$qlf_load/1: %s bad version (file version = %d, prolog = %d)",
	      wicFile, lversion, VERSION);
    fail;
  }
  saved_wsize = getNum(fd);
  if ( saved_wsize != sizeof(word)*8 )
  { warning("QLF file %s has incompatible (%d) word-length",
	    file, saved_wsize);
    fail;
  }

  qlf_saved_version = lversion;
  abssavename = getString(fd);
  if ( streq(absloadname, abssavename) )
  { qlf_has_moved = FALSE;
    qlf_load_dir = qlf_save_dir = NULL;
  } else
  { char tmp[MAXPATHLEN];
    qlf_has_moved = TRUE;
    qlf_load_dir = stringAtom(PL_new_atom(DirName(absloadname, tmp)));
    qlf_save_dir = stringAtom(PL_new_atom(DirName(abssavename, tmp)));
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
  Sputc('F', fd);
  putString(stringAtom(f->name), fd);
  putNum(f->time, fd);
  Sputc(f->system ? 's' : 'u', fd);

  currentSource = f;

  succeed;
}


static bool
qlfStartModule(Module m, IOSTREAM *fd)
{ closeProcedureWic(fd);
  Sputc('Q', fd);
  Sputc('M', fd);
  saveXR(m->name, fd);
  if ( m->file )
    qlfSaveSource(m->file, fd);
  else
    Sputc('-', fd);

  DEBUG(2, Sdprintf("MODULE %s\n", stringAtom(m->name)));
  for_unlocked_table(m->public, s,
		     { functor_t f = (functor_t)s->name;

		       DEBUG(2, Sdprintf("Exported %s/%d\n",
					 stringAtom(nameFunctor(f)),
					 arityFunctor(f)));
		       Sputc('E', fd);
		       saveXRFunctor(f, fd);
		     })

  Sputc('X', fd);

  succeed;
}


static bool
qlfStartSubModule(Module m, IOSTREAM *fd)
{ closeProcedureWic(fd);
  Sputc('M', fd);
  saveXR(m->name, fd);

  succeed;
}


static bool
qlfStartFile(SourceFile f, IOSTREAM *fd)
{ closeProcedureWic(fd);
  Sputc('Q', fd);
  qlfSaveSource(f, fd);

  succeed;
}


static bool
qlfEndPart(IOSTREAM  *fd)
{ closeProcedureWic(fd);
  Sputc('X', fd);

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
{ Module m, oldsrc = LD->modules.source;
  char fbuf[MAXPATHLEN];
  char *fn;
  bool rval;
  term_t name = PL_new_term_ref();

  m = oldsrc;
  if ( !PL_strip_module(file, &m, name) )
    fail;
  if ( !(fn = PL_get_filename(name, fbuf, sizeof(fbuf))) )
    return warning("$qlf_load/2: instantiation fault");

  LD->modules.source = m;
  rval = qlfLoad(fn, &m);
  LD->modules.source = oldsrc;

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
pl_open_wic(term_t to)
{ IOSTREAM *fd;

  if ( PL_get_stream_handle(to, &fd) )
  { wicFd = fd;
    writeWicHeader(fd);
    succeed;
  }

  fail;					/* PL_get_stream_handle() */
					/* throws exception */
}

word
pl_close_wic()
{ IOSTREAM *fd = wicFd;

  if ( fd )
  { writeWicTrailer(fd);
    wicFd = NULL;

    succeed;
  }

  fail;
}


word
pl_add_directive_wic(term_t term)
{ if ( wicFd )
  { if ( !(PL_is_compound(term) || PL_is_atom(term)) )
      return PL_error("$add_directive_wic", 1, NULL, ERR_TYPE,
		      ATOM_callable, term);

    return addDirectiveWic(term, wicFd);
  }

  succeed;
}


word
pl_import_wic(term_t module, term_t head)
{ if ( wicFd )
  { Module m;
    functor_t f;

    if ( !PL_get_module(module, &m) ||
	 !PL_get_functor(head, &f) )
      return warning("$import_wic/3: instantiation fault");

    return importWic(lookupProcedure(f, m), wicFd);
  }

  succeed;
}


word
pl_qlf_assert_clause(term_t ref, term_t saveclass)
{ if ( wicFd )
  { Clause clause;
    IOSTREAM *s = wicFd;
    atom_t sclass;

    if ( !PL_get_pointer(ref, (void **)&clause) ||
	 !inCore(clause) || !isClause(clause) )
      return PL_error("qlf_assert_clause", 2, NULL,
		      ERR_DOMAIN, ATOM_clause_reference, 1);
    if ( !PL_get_atom(saveclass, &sclass) )
      return PL_error("qlf_assert_clause", 2, NULL,
		      ERR_DOMAIN, ATOM_save_class, 2);

    openProcedureWic(clause->procedure, s, sclass);
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

    PL_put_atom(m, LD->modules.source->name);
    PL_cons_functor(directive, FUNCTOR_module2, m, d0);
  }

  succeed;
}

/*  Compile an entire file into intermediate code.

 ** Thu Apr 28 13:44:43 1988  jan@swivax.UUCP (Jan Wielemaker)  */

static bool
compileFile(char *file)
{ char tmp[MAXPATHLEN];
  char *path;
  term_t f = PL_new_term_ref();
  atom_t nf;

  DEBUG(1, Sdprintf("Boot compilation of %s\n", file));
  if ( !(path = AbsoluteFile(file, tmp)) )
    fail;
  DEBUG(2, Sdprintf("Expanded to %s\n", path));

  nf = PL_new_atom(path);
  PL_put_atom(f, nf);
  DEBUG(2, Sdprintf("Opening\n"));
  if ( !pl_see(f) )
    fail;
  DEBUG(2, Sdprintf("pl_start_consult()\n"));
  pl_start_consult(f);
  qlfStartFile(lookupSourceFile(nf), wicFd);
  
  for(;;)
  { fid_t	 cid = PL_open_foreign_frame();
    term_t         t = PL_new_term_ref();
    term_t directive = PL_new_term_ref();
    atom_t eof;

    DEBUG(2, Sdprintf("pl_read_clause() -> "));
    PL_put_variable(t);
    if ( !pl_read_clause(t) )		/* syntax error */
    { Sdprintf("%s:%d: Syntax error\n",
	       PL_atom_chars(source_file_name),
	       source_line_no);
      continue;
    }
    if ( PL_get_atom(t, &eof) && eof == ATOM_end_of_file )
      break;

    DEBUG(2, PL_write_term(Serror, t, 1200, PL_WRT_NUMBERVARS); pl_nl());

    if ( directiveClause(directive, t, ":-") )
    { DEBUG(1,
	    Sdprintf(":- ");
	    PL_write_term(Serror, directive, 1200, 0);
	    Sdprintf(".\n") );
      addDirectiveWic(directive, wicFd);
      if ( !callProlog(MODULE_user, directive, PL_Q_NODEBUG, NULL) )
	Sdprintf("%s:%d: directive failed\n",
		 PL_atom_chars(source_file_name),
		 source_line_no);
    } else if ( directiveClause(directive, t, "$:-") )
    { DEBUG(1,
	    Sdprintf("$:- ");
	    PL_write_term(Serror, directive, 1200, 0);
	    Sdprintf(".\n"));
      callProlog(MODULE_user, directive, PL_Q_NODEBUG, NULL);
    } else
      addClauseWic(t, nf);

    PL_discard_foreign_frame(cid);
  }

  qlfEndPart(wicFd);
  pl_seen();

  succeed;
}

bool
compileFileList(IOSTREAM *fd, int argc, char **argv)
{ TRY(writeWicHeader(fd));
  
  systemMode(TRUE);
  defFeature("autoload", FT_BOOL, FALSE, 0);

  for(;argc > 0; argc--, argv++)
  { if ( streq(argv[0], "-c" ) )
      break;
    compileFile(argv[0]);
  }

  defFeature("autoload", FT_BOOL, TRUE, 0);
  systemMode(FALSE);

  { predicate_t pred = PL_predicate("$load_additional_boot_files", 0, "user");

    PL_call_predicate(MODULE_user, TRUE, pred, 0);
  }

  return writeWicTrailer(fd);
}

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

  if ( getstr_buffer )
  { free(getstr_buffer);
    getstr_buffer = NULL;
    getstr_buffer_size = 512;
  }
}


