/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam
			      VU University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

/*#define O_DEBUG 1*/
#include "pl-incl.h"
#include "pl-dbref.h"
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

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
			{'I' <include>}
			'Q' <qlf-part>
<qlf-magic>	::=	<string>
<qlf-module>	::=	<qlf-header>
			<size>				% size in bytes
			{<statement>}
			'X'
<qlf-header>	::=	'M' <XR/modulename>		% module name
			<source>			% file + time
			<line>
			{'S' <XR/supername>}
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
		      | 'P' <XR/functor>		% predicate
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
		      | 'I' <XR/procedure> <flags>	% import predicate
		      | 'Q' <qlf-module>		% include module
		      | 'M' <XR/modulename>		% load-in-module
		            {<statement>}
			    'X'
<flags>		::=	<num>				% Bitwise or of PRED_*
<clause>	::=	'C' <#codes>
			    <line_no>
			    <owner_file>
			    <source_file>
			    <# prolog vars> <# vars>
			    <is_fact>			% 0 or 1
			    <#n subclause> <codes>
		      | 'X'				% end of list
<XR>		::=	XR_REF     <num>		% XR id from table
			XR_ATOM    <len><chars>		% atom
			XR_BLOB	   <blob><private>	% typed atom (blob)
			XR_INT     <num>		% number
			XR_FLOAT   <word>*		% float (double)
			XR_STRING  <string>		% string
			XR_STRING_UTF8  <utf-8 string>	% wide string
			XR_FUNCTOR <XR/name> <num>	% functor
			XR_PRED    <XR/fdef> <XR/module>% predicate
			XR_MODULE  <XR/name>		% module
			XR_FILE	   's'|'u' <XR/atom> <time>
				   '-'
			XR_BLOB_TYPE <len><chars>	% blob type-name
<term>		::=	<num>				% # variables in term
			<theterm>
<theterm>	::=	<XR/atomic>			% atomic data
		      | 'v' <num>			% variable
		      | 't' <XR/functor> {<theterm>}	% compound
<system>	::=	's'				% system source file
		      | 'u'				% user source file
<time>		::=	<word>				% time file was loaded
<line>		::=	<num>
<codes>		::=	<num> {<code>}
<string>	::=	{<non-zero byte>} <0>
<word>		::=	<4 byte entity>
<include>	::=	<owner> <parent> <line> <file> <time>

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

#define LOADVERSION 64			/* load all versions later >= X */
#define VERSION 64			/* save version number */
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
#define XR_BLOB	   9			/* a typed atom (blob) */
#define XR_BLOB_TYPE 10			/* name of atom-type declaration */
#define XR_STRING_UTF8 11		/* Wide character string */
#define XR_NULL	  12			/* NULL pointer */

#define PRED_SYSTEM	 0x01		/* system predicate */
#define PRED_HIDE_CHILDS 0x02		/* hide my childs */

static char saveMagic[] = "SWI-Prolog state (www.swi-prolog.org)\n";
static char qlfMagic[]  = "SWI-Prolog .qlf file\n";

typedef struct source_mark
{ long		file_index;
  struct source_mark *next;
} source_mark, *SourceMark;


#define XR_ARRAYS 16000
typedef struct xr_table
{ int		id;			/* next id to give out */
  int		tablesize;		/* # sub-arrays */
  struct xr_table* previous;		/* stack */
  Word	        table[XR_ARRAYS];	/* main table */
} xr_table, *XrTable;


typedef struct qlf_state
{ char *save_dir;			/* Directory saved */
  char *load_dir;			/* Directory loading */
  int   saved_version;			/* Version saved */
  int	has_moved;			/* Paths must be translated */
  struct qlf_state *previous;		/* previous saved state (reentrance) */
} qlf_state;


typedef struct wic_state
{ char *wicFile;			/* name of output file */
  char *mkWicFile;			/* Wic file under construction */
  IOSTREAM *wicFd;			/* file descriptor of wic file */

  Procedure currentProc;		/* current procedure */
  SourceFile currentSource;		/* current source file */

  Table	savedXRTable;			/* saved XR entries */
  intptr_t savedXRTableId;		/* next id to hand out */

  SourceMark source_mark_head;		/* Locations of sources */
  SourceMark source_mark_tail;
  int	     has_source_marks;

  int	     load_nesting;		/* Nesting level of loadPart() */
  qlf_state *load_state;		/* current load-state */

  xr_table *XR;				/* external references */

  struct wic_state *parent;		/* parent state */
} wic_state;

static char *	getString(IOSTREAM *, size_t *len);
static int64_t	getInt64(IOSTREAM *);
static int	getInt32(IOSTREAM *s);
static long	getLong(IOSTREAM *);
static int	getInt(IOSTREAM *);
static double	getFloat(IOSTREAM *);
static bool	loadWicFd(wic_state *state);
static bool	loadPredicate(wic_state *state, int skip ARG_LD);
static bool	loadImport(wic_state *state, int skip ARG_LD);
static void	saveXRBlobType(wic_state *state, PL_blob_t *type);
static void	putString(const char *, size_t len, IOSTREAM *);
static void	putNum(int64_t, IOSTREAM *);
static void	putFloat(double, IOSTREAM *);
static void	saveWicClause(wic_state *state, Clause cl);
static void	closeProcedureWic(wic_state *state);
static word	loadXRc(wic_state *state, int c ARG_LD);
static atom_t   getBlob(wic_state *state ARG_LD);
static bool	loadStatement(wic_state *state, int c, int skip ARG_LD);
static bool	loadPart(wic_state *state, Module *module, int skip ARG_LD);
static bool	loadInModule(wic_state *state, int skip ARG_LD);
static int	qlfVersion(wic_state *state);
static atom_t	qlfFixSourcePath(wic_state *state, const char *raw);
static int	pushPathTranslation(wic_state *state, const char *loadname, int flags);
static void	popPathTranslation(wic_state *state);

#undef LD
#define LD LOCAL_LD


		 /*******************************
		 *     LOADED XR ID HANDLING	*
		 *******************************/

#define loadedXRTableId		(loadedXrs->id)

#define SUBENTRIES (8000)
#define ALLOCSIZE  (SUBENTRIES*sizeof(word))

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
XR reference handling during loading. This   is  arranged as an array-of
arrays. The main array has size  XR_ARRAYS. Each subarray has SUBENTRIES
constants, allowing for a total of 96M constants per QLF file or state.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
pushXrIdTable(wic_state *state)
{ XrTable t = allocHeapOrHalt(sizeof(struct xr_table));

  DEBUG(CHK_SECURE, memset(t->table, 0xbf, XR_ARRAYS*sizeof(Word)));
  t->tablesize = 0;
  t->id = 0;

  t->previous = state->XR;
  state->XR = t;
}


static void
popXrIdTable(wic_state *state)
{ int i;
  XrTable t = state->XR;

  state->XR = t->previous;		/* pop the stack */

  for(i=0; i<t->tablesize; i++)		/* destroy obsolete table */
    freeHeap(t->table[i], ALLOCSIZE);

  freeHeap(t, sizeof(*t));
}


static word
lookupXrId(wic_state *state, intptr_t id)
{ XrTable t = state->XR;
  Word array = t->table[id/SUBENTRIES];
  word value;

  DEBUG(CHK_SECURE, assert(array));
  value = array[id%SUBENTRIES];

  return value;
}


static void
storeXrId(wic_state *state, long id, word value)
{ XrTable t = state->XR;
  long i = id/SUBENTRIES;

  while ( i >= t->tablesize )
  { if ( t->tablesize < XR_ARRAYS-1 )
    { Word a = allocHeapOrHalt(ALLOCSIZE);

      DEBUG(CHK_SECURE, memset(a, 0xbf, ALLOCSIZE));
      t->table[t->tablesize++] = a;
    } else
    { fatalError("Too many constants in QLF file");
    }
  }

  t->table[i][id%SUBENTRIES] = value;
}


		 /*******************************
		 *	 PRIMITIVE LOADING	*
		 *******************************/

#define PATH_ISDIR	0x1		/* pushPathTranslation() flags */

static bool
qlfLoadError_ctx(wic_state *state, char *file, int line)
{ fatalError("%s: QLF format error at index = %ld (%s:%d)",
	     state->wicFile, Stell(state->wicFd), file, line);

  fail;
}

#define qlfLoadError(state) qlfLoadError_ctx(state, __FILE__, __LINE__)

static char *
getString(IOSTREAM *fd, size_t *length)
{ GET_LD
  char *s;
  size_t len = (size_t)getInt64(fd);
  size_t i;

  if ( LD->qlf.getstr_buffer_size < len+1 )
  { size_t size = ((len+1+1023)/1024)*1024;

    if ( LD->qlf.getstr_buffer )
      LD->qlf.getstr_buffer = realloc(LD->qlf.getstr_buffer, size);
    else
      LD->qlf.getstr_buffer = malloc(size);

    if ( LD->qlf.getstr_buffer )
      LD->qlf.getstr_buffer_size = size;
    else
      outOfCore();
  }

  for( i=0, s = LD->qlf.getstr_buffer; i<len; i++ )
  { int c = Sgetc(fd);

    if ( c == EOF )
      fatalError("Unexpected EOF on intermediate code file at offset %d",
		 Stell(fd));

    *s++ = c;
  }
  *s = EOS;

  if ( length )
    *length = (unsigned) len;

  return LD->qlf.getstr_buffer;
}


pl_wchar_t *
wicGetStringUTF8(IOSTREAM *fd, size_t *length,
		 pl_wchar_t *buf, size_t bufsize)
{ size_t i, len = (size_t)wicGetNum(fd);
  IOENC oenc = fd->encoding;
  pl_wchar_t *tmp, *o;

  if ( length )
    *length = len;

  if ( len < bufsize )
    tmp = buf;
  else
    tmp = PL_malloc(len*sizeof(pl_wchar_t));

  fd->encoding = ENC_UTF8;
  for(i=0, o=tmp; i<len; i++)
  { int c = Sgetcode(fd);

    if ( c < 0 )
      fatalError("Unexpected EOF in UCS atom");
    *o++ = c;
  }
  fd->encoding = oenc;

  return tmp;
}



static atom_t
getAtom(IOSTREAM *fd, PL_blob_t *type)
{ char buf[1024];
  char *tmp, *s;
  size_t len = getInt(fd);
  size_t i;
  atom_t a;

  if ( len < sizeof(buf) )
    tmp = buf;
  else
    tmp = allocHeapOrHalt(len);

  for(s=tmp, i=0; i<len; i++)
  { int c = Sgetc(fd);

    if ( c == EOF )
      fatalError("Unexpected EOF on intermediate code file at offset %d",
		 Stell(fd));
    *s++ = c;
  }
  if ( type )
  { int new;

    a = lookupBlob(tmp, len, type, &new);
  } else
  { a = lookupAtom(tmp, len);
  }

  if ( tmp != buf )
    freeHeap(tmp, len);

  return a;
}


static PL_blob_t *
getBlobType(IOSTREAM *fd)
{ const char *name = getString(fd, NULL);

  return PL_find_blob_type(name);
}


static char *
getMagicString(IOSTREAM *fd, char *buf, int maxlen)
{ char *s;
  int c;

  for( s = buf; --maxlen >= 0 && (*s = (c = Sgetc(fd))); s++ )
    if ( c == EOF )
      return NULL;

  if ( maxlen > 0 )
    return buf;

  return NULL;
}


static int64_t
getInt64(IOSTREAM *fd)
{ int64_t first;
  int bytes, shift, b;

  DEBUG(MSG_QLF_INTEGER, Sdprintf("getInt64() from %ld --> \n", Stell(fd)));

  first = Snpgetc(fd);
  if ( !(first & 0xc0) )		/* 99% of them: speed up a bit */
  { first <<= (INT64BITSIZE-6);
    first >>= (INT64BITSIZE-6);

    DEBUG(MSG_QLF_INTEGER, Sdprintf(INT64_FORMAT "\n", first));
    return first;
  }

  bytes = (int) ((first >> 6) & 0x3);
  first &= 0x3f;

  if ( bytes <= 2 )
  { for( b = 0; b < bytes; b++ )
    { first <<= 8;
      first |= Snpgetc(fd) & 0xff;
    }

    shift = (sizeof(first)-1-bytes)*8 + 2;
  } else
  { int m;

    bytes = (int)first;
    first = (int64_t)0;

    for(m=0; m<bytes; m++)
    { first <<= 8;
      first |= Snpgetc(fd) & 0xff;
    }
    shift = (sizeof(first)-bytes)*8;
  }

  first <<= shift;
  first >>= shift;

  DEBUG(MSG_QLF_INTEGER, Sdprintf(INT64_FORMAT "\n", first));
  return first;
}


static long
getLong(IOSTREAM *fd)
{ int64_t val = getInt64(fd);

  return (long)val;
}


static int
getInt(IOSTREAM *fd)
{ int64_t val = getInt64(fd);

  return (int)val;
}


#ifdef WORDS_BIGENDIAN
static const int double_byte_order[] = { 7,6,5,4,3,2,1,0 };
#else
static const int double_byte_order[] = { 0,1,2,3,4,5,6,7 };
#endif

#define BYTES_PER_DOUBLE (sizeof(double_byte_order)/sizeof(int))

static double
getFloat(IOSTREAM *fd)
{ double f;
  unsigned char *cl = (unsigned char *)&f;
  unsigned int i;

  for(i=0; i<BYTES_PER_DOUBLE; i++)
  { int c = Snpgetc(fd);

    if ( c == -1 )
      fatalError("Unexpected end-of-file in QLT file");
    cl[double_byte_order[i]] = c;
  }

  DEBUG(MSG_QLF_FLOAT, Sdprintf("getFloat() --> %f\n", f));

  return f;
}


static int
getInt32(IOSTREAM *s)
{ int v;

  v  = (Sgetc(s) & 0xff) << 24;
  v |= (Sgetc(s) & 0xff) << 16;
  v |= (Sgetc(s) & 0xff) << 8;
  v |= (Sgetc(s) & 0xff);

  return v;
}


static inline word
loadXR__LD(wic_state *state ARG_LD)
{ return loadXRc(state, Qgetc(state->wicFd) PASS_LD);
}
#define loadXR(s) loadXR__LD(s PASS_LD)


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
loadXRc(int c0, IOSTREAM *fd ARG_LD) loads   a constant from the stream.
Note that some constants (integers, floats and  strings) can cause GC or
stack-shifts.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static word
loadXRc(wic_state *state, int c ARG_LD)
{ IOSTREAM *fd = state->wicFd;
  word xr;
  int id = 0;				/* make gcc happy! */

  switch( c )
  { case XR_REF:
    { intptr_t xr  = getLong(fd);
      word val = lookupXrId(state, xr);

      return val;
    }
    case XR_ATOM:
    { id = ++state->XR->id;
      xr = getAtom(fd, NULL);
      DEBUG(MSG_QLF_XR, Sdprintf("XR(%d) = '%s'\n", id, stringAtom(xr)));
      break;
    }
    case XR_BLOB:
    { id = ++state->XR->id;
      xr = getBlob(state PASS_LD);
      DEBUG(MSG_QLF_XR, Sdprintf("XR(%d) = <blob>\n", id));
      break;
    }
    case XR_BLOB_TYPE:
    { id = ++state->XR->id;
      xr = (word)getBlobType(fd);
      DEBUG(MSG_QLF_XR,
	    Sdprintf("XR(%d) = <blob-type>%s", id, ((PL_blob_t*)xr)->name));
      break;
    }
    case XR_FUNCTOR:
    { atom_t name;
      int arity;

      id = ++state->XR->id;
      name = loadXR(state);
      arity = getInt(fd);
      xr = (word) lookupFunctorDef(name, arity);
      DEBUG(MSG_QLF_XR,
	    Sdprintf("XR(%d) = %s/%d\n", id, stringAtom(name), arity));
      break;
    }
    case XR_PRED:
    { functor_t f;
      Module m;

      id = ++state->XR->id;
      f = (functor_t) loadXR(state);
      m = (Module) loadXR(state);
      xr = (word) lookupProcedure(f, m);
      DEBUG(MSG_QLF_XR,
	    Sdprintf("XR(%d) = proc %s\n", id, procedureName((Procedure)xr)));
      break;
    }
    case XR_MODULE:
    { atom_t name;
      id = ++state->XR->id;
      name = loadXR(state);
      xr = (word) lookupModule(name);
      DEBUG(MSG_QLF_XR, Sdprintf("XR(%d) = module %s\n", id, stringAtom(name)));
      break;
    }
    case XR_INT:
    { int64_t i = getInt64(fd);
      word w;
      int rc;

      if ( (rc=put_int64(&w, i, ALLOW_GC PASS_LD)) != TRUE )
      { raiseStackOverflow(rc);
	return 0;
      }

      return w;
    }
    case XR_FLOAT:
    { word w;
      double f = getFloat(fd);
      int rc;

      if ( (rc=put_double(&w, f, ALLOW_GC PASS_LD)) != TRUE )
      { raiseStackOverflow(rc);
	return 0;
      }

      return w;
    }
#if O_STRING
    case XR_STRING:
    { char *s;
      size_t len;

      s = getString(fd, &len);

      return globalString(len, s);
    }
    case XR_STRING_UTF8:
    { pl_wchar_t *w;
      size_t len;
      pl_wchar_t buf[256];
      word s;

      w = wicGetStringUTF8(fd, &len, buf, sizeof(buf)/sizeof(pl_wchar_t));
      s = globalWString(len, w);
      if ( w != buf )
	PL_free(w);

      return s;
    }
#endif
    case XR_FILE:
    { int c;

      id = ++state->XR->id;

      switch( (c=Qgetc(fd)) )
      { case 'u':
	case 's':
	{ atom_t name   = loadXR(state);
	  double time   = getFloat(fd);
	  const char *s = stringAtom(name);
	  SourceFile sf = lookupSourceFile(qlfFixSourcePath(state, s), TRUE);

	  if ( sf->mtime == 0.0 )
	  { sf->mtime   = time;
	    sf->system = (c == 's' ? TRUE : FALSE);
	  }
	  sf->count++;
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
    case XR_NULL:
      return 0;
    default:
    { xr = 0;				/* make gcc happy */
      fatalError("Illegal XR entry at index %d: %c", Stell(fd)-1, c);
    }
  }

  storeXrId(state, id, xr);

  return xr;
}


static atom_t
getBlob(wic_state *state ARG_LD)
{ PL_blob_t *type = (PL_blob_t*)loadXR(state);

  if ( type->load )
  { return (*type->load)(state->wicFd);
  } else
  { return getAtom(state->wicFd, type);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Returns FALSE while leaving a resource exception if the term cannot be
allocated.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
do_load_qlf_term(wic_state *state, term_t vars[], term_t term ARG_LD)
{ IOSTREAM *fd = state->wicFd;
  int c = Qgetc(fd);

  if ( c == 'v' )
  { int id = getInt(fd);

    if ( vars[id] )
    { return PL_unify(term, vars[id]);
    } else
    { if ( (vars[id] = PL_new_term_ref()) )
      { PL_put_term(vars[id], term);
	return TRUE;
      }
      return FALSE;
    }
  } else if ( c == 't' )
  { functor_t f;
    term_t c2;

    if ( (f = (functor_t) loadXR(state)) &&
	 (c2 = PL_new_term_ref()) &&
	 PL_unify_functor(term, f) )
    { int arity = arityFunctor(f);
      int n;

      for(n=0; n < arity; n++)
      { _PL_get_arg(n+1, term, c2);
	if ( !do_load_qlf_term(state, vars, c2 PASS_LD) )
	  return FALSE;
      }

      return TRUE;
    }

    return FALSE;
  } else
  { word w;

    if ( (w=loadXRc(state, c PASS_LD)) )
      return _PL_unify_atomic(term, w);

    return FALSE;
  }
}


static int
loadQlfTerm(wic_state *state, term_t term ARG_LD)
{ IOSTREAM *fd = state->wicFd;
  int nvars;
  Word vars;
  int rc;

  DEBUG(MSG_QLF_TERM, Sdprintf("Loading from %d ...", Stell(fd)));

  if ( (nvars = getInt(fd)) )
  { term_t *v;
    int n;

    vars = alloca(nvars * sizeof(term_t));
    for(n=nvars, v=vars; n>0; n--, v++)
      *v = 0L;
  } else
    vars = NULL;

  PL_put_variable(term);
  rc = do_load_qlf_term(state, vars, term PASS_LD);
  DEBUG(MSG_QLF_TERM,
	Sdprintf("Loaded ");
	PL_write_term(Serror, term, 1200, 0);
	Sdprintf(" to %d\n", Stell(fd)));
  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Load intermediate code state from the   specified  stream. This function
loads the initial saved state, either  boot32.prc, the state attached to
the executable or the argument of pl -x <state>.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
loadWicFromStream(IOSTREAM *fd)
{ wic_state state;
  int rval;

  memset(&state, 0, sizeof(state));
  state.wicFd = fd;

  pushXrIdTable(&state);
  rval = loadWicFd(&state);
  popXrIdTable(&state);

  return rval;
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
loadWicFd(wic_state *state)
{ GET_LD
  IOSTREAM *fd = state->wicFd;
  char *s;
  Char c;
  char mbuf[100];
  int saved_wsize;
  int saved_version;
  int vm_signature;

  s = getMagicString(fd, mbuf, sizeof(mbuf));
  if ( !s || !streq(s, saveMagic) )
  { fatalError("Not a SWI-Prolog saved state");
    fail;				/* NOTREACHED */
  }

  if ( (saved_version=getInt(fd)) < LOADVERSION )
  { fatalError("Saved state has incompatible save version");
    fail;
  }
  if ( (vm_signature=getInt(fd)) != (int)VM_SIGNATURE )
  { fatalError("Saved state has incompatible VM signature");
    fail;
  }

  saved_wsize = getInt(fd);
  if ( saved_wsize != sizeof(word)*8 )
  { fatalError("Saved state has incompatible (%d) word-length", saved_wsize);
    fail;
  }
					/* fix paths for changed home */
  pushPathTranslation(state, systemDefaults.home, PATH_ISDIR);
  state->load_state->saved_version = saved_version;

  for(;;)
  { c = Sgetc(fd);

    switch( c )
    { case EOF:
      case 'T':				/* trailer */
	popPathTranslation(state);
	succeed;
      case 'W':
	{ char *name = store_string(getString(fd, NULL) );

	  loadWicFile(name);
	  continue;
	}
      case 'X':
        break;
      default:
        { loadStatement(state, c, FALSE PASS_LD);
	  continue;
	}
    }
  }
}


static bool
loadStatement(wic_state *state, int c, int skip ARG_LD)
{ IOSTREAM *fd = state->wicFd;

  switch(c)
  { case 'P':
      return loadPredicate(state, skip PASS_LD);

    case 'O':
    { word mname = loadXR(state);
      Module om = LD->modules.source;
      bool rval;

      LD->modules.source = lookupModule(mname);
      rval = loadPredicate(state, skip PASS_LD);
      LD->modules.source = om;

      return rval;
    }
    case 'I':
      return loadImport(state, skip PASS_LD);

    case 'D':
    { fid_t cid;

      if ( (cid=PL_open_foreign_frame()) )
      { term_t goal = PL_new_term_ref();
	atom_t  osf = source_file_name;
	int     oln = source_line_no;

	source_file_name = (state->currentSource ? state->currentSource->name
						 : NULL_ATOM);
	source_line_no   = getInt(fd);

	if ( !loadQlfTerm(state, goal PASS_LD) )
	  return FALSE;
	DEBUG(MSG_QLF_DIRECTIVE,
	      if ( source_file_name )
	      { Sdprintf("%s:%d: Directive: ",
			  PL_atom_chars(source_file_name), source_line_no);
	      } else
	      { Sdprintf("Directive: ");
	      }
	      pl_write(goal);
	      Sdprintf("\n"));
	if ( !skip )
	{ if ( !callProlog(MODULE_user, goal, PL_Q_NODEBUG, NULL) )
	  { printMessage(ATOM_warning,
			 PL_FUNCTOR_CHARS, "goal_failed", 2,
			   PL_CHARS, "directive",
			   PL_TERM, goal);
	  }
	}
	PL_discard_foreign_frame(cid);

	source_file_name = osf;
	source_line_no   = oln;

	succeed;
      }

      return FALSE;
    }

    case 'Q':
    { bool rc;

      state->load_nesting++;
      rc = loadPart(state, NULL, skip PASS_LD);
      state->load_nesting--;

      return rc;
    }
    case 'M':
      return loadInModule(state, skip PASS_LD);

    default:
      return qlfLoadError(state);
  }
}


static void
loadPredicateFlags(wic_state *state, Definition def, int skip)
{ int flags = getInt(state->wicFd);

  if ( !skip )
  { unsigned long lflags = 0L;

    if ( flags & PRED_SYSTEM )
      lflags |= P_LOCKED;
    if ( flags & PRED_HIDE_CHILDS )
      lflags |= HIDE_CHILDS;

    set(def, lflags);
  }
}


static bool
loadPredicate(wic_state *state, int skip ARG_LD)
{ IOSTREAM *fd = state->wicFd;
  Procedure proc;
  Definition def;
  Clause clause;
  functor_t f = (functor_t) loadXR(state);
  SourceFile csf = NULL;

  proc = lookupProcedureToDefine(f, LD->modules.source);
  DEBUG(MSG_QLF_PREDICATE, Sdprintf("Loading %s%s",
				    procedureName(proc),
				    skip ? " (skip)" : ""));

  def = proc->definition;
  if ( !skip && state->currentSource )
  { if ( def->impl.any )
    { if ( !redefineProcedure(proc, state->currentSource, DISCONTIGUOUS_STYLE) )
      { printMessage(ATOM_error, exception_term);
	exception_term = 0;
	setVar(*valTermRef(exception_bin));
	skip = TRUE;
      }
    }
    addProcedureSourceFile(state->currentSource, proc);
  }
  loadPredicateFlags(state, def, skip);

  for(;;)
  { switch(Sgetc(fd) )
    { case 'X':
      { DEBUG(MSG_QLF_PREDICATE, Sdprintf("ok\n"));
	succeed;
      }
      case 'C':
      { Code bp, ep;
	int ncodes = getInt(fd);

	DEBUG(MSG_QLF_PREDICATE, Sdprintf("."));
	clause = (Clause) PL_malloc_atomic(sizeofClause(ncodes));
	clause->code_size = (unsigned int) ncodes;
	clause->line_no = (unsigned short) getInt(fd);

	{ SourceFile of = (void *) loadXR(state);
	  SourceFile sf = (void *) loadXR(state);
	  int ono = (of ? of->index : 0);
	  int sno = (sf ? sf->index : 0);

	  clause->owner_no = ono;
	  clause->source_no = sno;
	  if ( of && of != csf )
	  { addProcedureSourceFile(sf, proc);
	    csf = of;
	  }
	}

	clearFlags(clause);
	clause->prolog_vars = (unsigned short) getInt(fd);
	clause->variables   = (unsigned short) getInt(fd);
	if ( getLong(fd) == 0 )		/* 0: fact */
	  set(clause, UNIT_CLAUSE);
	clause->procedure = proc;
	GD->statistics.codes += clause->code_size;

	bp = clause->codes;
	ep = bp + clause->code_size;

	while( bp < ep )
	{ code op = getInt(fd);
	  const char *ats;
	  int n = 0;

	  if ( op >= I_HIGHEST )
	    fatalError("Illegal op-code (%d) at %ld", op, Stell(fd));

	  ats = codeTable[op].argtype;
	  DEBUG(MSG_QLF_VMI,
		Sdprintf("\t%s from %ld\n", codeTable[op].name, Stell(fd)));
	  *bp++ = encode(op);
	  DEBUG(0,
		{ const char ca1_float[2] = {CA1_FLOAT};
		  const char ca1_int64[2] = {CA1_INT64};
		  assert(codeTable[op].arguments == VM_DYNARGC ||
			 (size_t)codeTable[op].arguments == strlen(ats) ||
			 (streq(ats, ca1_float) &&
			  codeTable[op].arguments == WORDS_PER_DOUBLE) ||
			 (streq(ats, ca1_int64) &&
			  codeTable[op].arguments == WORDS_PER_INT64));
		});

	  for(n=0; ats[n]; n++)
	  { switch(ats[n])
	    { case CA1_PROC:
	      { *bp++ = loadXR(state);
		break;
	      }
	      case CA1_FUNC:
	      case CA1_DATA:
	      { word w = loadXR(state);
		if ( isAtom(w) )
		  PL_register_atom(w);
		*bp++ = w;
		break;
	      }
	      case CA1_AFUNC:
	      { word f = loadXR(state);
		int  i = indexArithFunction(f);
		assert(i>0);
		*bp++ = i;
		break;
	      }
	      case CA1_MODULE:
		*bp++ = loadXR(state);
		break;
	      case CA1_INTEGER:
	      case CA1_JUMP:
	      case CA1_VAR:
	      case CA1_FVAR:
	      case CA1_CHP:
		*bp++ = (intptr_t)getInt64(fd);
		break;
	      case CA1_INT64:
	      { int64_t val = getInt64(fd);
		Word p = (Word)&val;

		cpInt64Data(bp, p);
		break;
	      }
	      case CA1_FLOAT:
	      { union
		{ word w[WORDS_PER_DOUBLE];
		  double f;
		} v;
		Word p = v.w;
		v.f = getFloat(fd);
		cpDoubleData(bp, p);
		break;
	      }
	      case CA1_STRING:		/* <n> chars */
	      { int l = getInt(fd);
		int lw = (l+sizeof(word))/sizeof(word);
		int pad = (lw*sizeof(word) - l);
		char *s = (char *)&bp[1];

		DEBUG(MSG_QLF_VMI, Sdprintf("String of %ld bytes\n", l));
		*bp = mkStrHdr(lw, pad);
		bp += lw;
		*bp++ = 0L;
		while(--l >= 0)
		  *s++ = Sgetc(fd);
		break;
	      }
	      case CA1_MPZ:
#ifdef O_GMP
	      DEBUG(MSG_QLF_VMI, Sdprintf("Loading MPZ from %ld\n", Stell(fd)));
	      { int mpsize = getInt(fd);
		int l      = abs(mpsize)*sizeof(mp_limb_t);
		int wsz	 = (l+sizeof(word)-1)/sizeof(word);
		word m     = mkIndHdr(wsz+1, TAG_INTEGER);
		char *s;

		*bp++     = m;
		*bp++     = mpsize;
		s         = (char*)bp;
		bp[wsz-1] = 0L;
		bp       += wsz;

		while(--l >= 0)
		  *s++ = Sgetc(fd);
		DEBUG(MSG_QLF_VMI, Sdprintf("Loaded MPZ to %ld\n", Stell(fd)));
		break;
	      }
#else
		fatalError("No support for MPZ numbers");
#endif
	      default:
		fatalError("No support for VM argtype %d (arg %d of %s)",
			   ats[n], n, codeTable[op].name);
	    }
	  }
	}

	if ( skip )
	  freeClause(clause);
	else
	  assertProcedure(proc, clause, CL_END PASS_LD);
      }
    }
  }
}


static bool
runInitialization(SourceFile sf)
{ int rc = FALSE;

  if ( sf )
  { GET_LD
    fid_t fid = PL_open_foreign_frame();
    term_t name = PL_new_term_ref();
    static predicate_t pred = NULL;

    if ( !pred )
      pred = PL_predicate("$run_initialization", 1, "system");

    PL_put_atom(name, sf->name);
    rc = PL_call_predicate(MODULE_system, PL_Q_NORMAL, pred, name);

    PL_discard_foreign_frame(fid);
  }

  return rc;
}


static bool
loadImport(wic_state *state, int skip ARG_LD)
{ Procedure proc = (Procedure) loadXR(state);
  int flags = getInt(state->wicFd);

  if ( !skip )
    return importDefinitionModule(LD->modules.source, proc->definition, flags);

  succeed;
}


static atom_t
qlfFixSourcePath(wic_state *state, const char *raw)
{ char buf[MAXPATHLEN];

  if ( state->load_state->has_moved &&
       strprefix(raw, state->load_state->save_dir) )
  { char *s;
    size_t lensave = strlen(state->load_state->save_dir);
    const char *tail = &raw[lensave];

    if ( strlen(state->load_state->load_dir)+1+strlen(tail)+1 > MAXPATHLEN )
      fatalError("Path name too long: %s", raw);

    strcpy(buf, state->load_state->load_dir);
    s = &buf[strlen(buf)];
    strcpy(s, tail);
  } else
  { if ( strlen(raw)+1 > MAXPATHLEN )
      fatalError("Path name too long: %s", raw);
    strcpy(buf, raw);
  }

  return PL_new_atom(canonicalisePath(buf));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(**) Note. When loading a qlf  file   we  must do the possible reconsult
stuff associated with loading sourcefiles. If we are loading a state all
is nice and fresh, so we can skip that. Actually, we *must* skip that as
a state is  created  based  on   modules  rather  than  files. Multifile
predicates are stored with the module. If   we  take no measures loading
the file from which a clause originates  will remove the one loaded with
the module where it is a multifile one.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
qlfLoadSource(wic_state *state)
{ IOSTREAM *fd = state->wicFd;
  char *str = getString(fd, NULL);
  double time = getFloat(fd);
  int issys = (Qgetc(fd) == 's') ? TRUE : FALSE;
  atom_t fname;

  fname = qlfFixSourcePath(state, str);

  DEBUG(MSG_QLF_PATH,
	if ( !streq(stringAtom(fname), str) )
	  Sdprintf("Replaced path %s --> %s\n", str, stringAtom(fname)));

  state->currentSource = lookupSourceFile(fname, TRUE);
  state->currentSource->mtime = time;
  state->currentSource->system = issys;
  if ( GD->bootsession )		/* (**) */
    state->currentSource->count++;
  else
    startConsult(state->currentSource);
  PL_unregister_atom(fname);		/* locked with sourceFile */

  succeed;
}


static bool
loadModuleProperties(wic_state *state, Module m, int skip ARG_LD)
{ IOSTREAM *fd = state->wicFd;

  if ( !skip )
    clearSupersModule(m);

  for(;;)
  { switch(Qgetc(fd))
    { case 'C':
      { atom_t cname = loadXR(state);

	if ( !skip )
	  m->class = cname;

	continue;
      }
      case 'S':
      { atom_t sname = loadXR(state);
	Module s = lookupModule(sname);

	if ( !skip )
	  addSuperModule(m, s, 'Z');

	continue;
      }
      case 'E':
      { functor_t f = (functor_t) loadXR(state);

	if ( !skip )
	{ Procedure proc = lookupProcedure(f, LD->modules.source);

	  addHTable(LD->modules.source->public, (void *)f, proc);
	} else
	{ if ( !lookupHTable(m->public, (void *)f) )
	  { FunctorDef fd = valueFunctor(f);

	    warning("%s: skipped module \"%s\" lacks %s/%d",
		    state->wicFile,
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
	return qlfLoadError(state);
    }
    break;
  }

  succeed;
}


static bool
loadPart(wic_state *state, Module *module, int skip ARG_LD)
{ IOSTREAM *fd		= state->wicFd;
  Module om		= LD->modules.source;
  SourceFile of		= state->currentSource;
  int stchk		= debugstatus.styleCheck;
  access_level_t alevel = LD->prolog_flag.access_level;

  switch(Qgetc(fd))
  { case 'M':
    { atom_t mname = loadXR(state);
      int c = Qgetc(fd);

      DEBUG(MSG_QLF_SECTION,
	    Sdprintf("Loading module %s\n", PL_atom_chars(mname)));

      switch( c )
      { case '-':
	{ LD->modules.source = lookupModule(mname);
					/* TBD: clear module? */
	  DEBUG(MSG_QLF_SECTION, Sdprintf("\tNo source\n"));
	  break;
	}
	case 'F':
	{ Module m;
	  int line;

	  qlfLoadSource(state);
	  line = getInt(fd);
	  DEBUG(MSG_QLF_SECTION,
		Sdprintf("\tSource = %s:%d\n",
			 PL_atom_chars(state->currentSource->name), line));

	  m = lookupModule(mname);
	  if ( m->file && m->file != state->currentSource )
	  { warning("%s:\n\tmodule \"%s\" already loaded from \"%s\" (skipped)",
		    state->wicFile, stringAtom(m->name), stringAtom(m->file->name));
	    skip = TRUE;
	    LD->modules.source = m;
	  } else
	  { if ( !declareModule(mname, NULL_ATOM, NULL_ATOM,
				state->currentSource, line, FALSE) )
	      fail;
	  }

	  if ( module )
	    *module = LD->modules.source;

	  break;
	}
	default:
	  qlfLoadError(state);
	  break;
      }

      if ( !loadModuleProperties(state, LD->modules.source, skip PASS_LD) )
	fail;

      break;
    }
    case 'F':
    { qlfLoadSource(state);

      if ( module )
	*module = NULL;

      break;
    }
    default:
      return qlfLoadError(state);
  }

  for(;;)
  { int c = Qgetc(fd);

    switch(c)
    { case 'X':
      { if ( !GD->bootsession && state->load_nesting > 0 )
	{ /* top-level '$run_initialization'/1 is called from boot/init.pl */
	  runInitialization(state->currentSource);
	}
	LD->modules.source = om;
	state->currentSource  = of;
	debugstatus.styleCheck = stchk;
	setAccessLevel(alevel);

	succeed;
      }
      default:
	loadStatement(state, c, skip PASS_LD);
    }
  }
}


static bool
loadInModule(wic_state *state, int skip ARG_LD)
{ IOSTREAM *fd = state->wicFd;
  word mname = loadXR(state);
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
	loadStatement(state, c, skip PASS_LD);
    }
  }
}


static bool
loadInclude(wic_state *state ARG_LD)
{ IOSTREAM *fd = state->wicFd;
  atom_t owner, pn, fn;
  int line;
  double time;
  fid_t fid = PL_open_foreign_frame();
  term_t t = PL_new_term_ref();
  sourceloc loc;

  owner = loadXR(state);
  pn    = loadXR(state);
  line  = getInt(fd);
  fn    = loadXR(state);
  time  = getFloat(fd);

  if ( !PL_unify_term(t,
		      PL_FUNCTOR, FUNCTOR_colon2,
			PL_ATOM, ATOM_system,
			PL_FUNCTOR_CHARS, "$included", 4,
			  PL_ATOM, pn,
			  PL_INT, line,
			  PL_ATOM, fn,
			  PL_FLOAT, time) )
    return FALSE;

  loc.file = pn;
  loc.line = line;

  assert_term(t, CL_END, owner, &loc PASS_LD);

  PL_discard_foreign_frame(fid);
  return TRUE;
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

#define STR_NOLEN ((size_t)-1)

static void
putString(const char *s, size_t len, IOSTREAM *fd)
{ const char *e;

  if ( len == STR_NOLEN )
    len = strlen(s);
  e = &s[len];

  putNum(len, fd);
  while(s<e)
  { Sputc(*s, fd);
    s++;
  }
}


static void
putStringW(const pl_wchar_t *s, size_t len, IOSTREAM *fd)
{ const pl_wchar_t *e;
  IOENC oenc = fd->encoding;

  if ( len == STR_NOLEN )
    len = wcslen(s);
  e = &s[len];

  putNum(len, fd);
  fd->encoding = ENC_UTF8;
  while(s<e)
  { Sputcode(*s, fd);
    s++;
  }
  fd->encoding = oenc;
}


static void
putAtom(wic_state *state, atom_t w)
{ IOSTREAM *fd = state->wicFd;
  Atom a = atomValue(w);
  static PL_blob_t *text_blob;		/* MT: ok */

  if ( !text_blob )
    text_blob = PL_find_blob_type("text");

  if ( a->type != text_blob )
  { Sputc(XR_BLOB, fd);
    saveXRBlobType(state, a->type);
    if ( a->type->save )
    { (*a->type->save)(a->atom, fd);
    } else
    { putString(a->name, a->length, fd);
    }
  } else
  { Sputc(XR_ATOM, fd);
    putString(a->name, a->length, fd);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Number encoding:

First byte:  bits 8&7  bits 1-6 (low order)

		0	6-bits signed value
		1      14-bits signed value
		2      22-bits signed value
		3      number of bytes following
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
putNum(int64_t n, IOSTREAM *fd)
{ int m;
  int64_t absn = (n >= 0 ? n : -n);

  DEBUG(MSG_QLF_INTEGER, Sdprintf("0x%x at %ld\n", (uintptr_t)n, Stell(fd)));

  if ( n != PLMININT )
  { if ( absn < (1L << 5) )
    { Sputc((int)(n & 0x3f), fd);
      return;
    } else if ( absn < (1L << 13) )
    { Sputc((int)(((n >> 8) & 0x3f) | (1 << 6)), fd);
      Sputc((int)(n & 0xff), fd);
      return;
    } else if ( absn < (1L << 21) )
    { Sputc((int)(((n >> 16) & 0x3f) | (2 << 6)), fd);
      Sputc((int)((n >> 8) & 0xff), fd);
      Sputc((int)(n & 0xff), fd);
      return;
    }
  }

  for(m = sizeof(n); ; m--)
  { int b = (int)(absn >> (((m-1)*8)-1)) & 0x1ff;

    if ( b == 0 )
      continue;
    break;
  }

  Sputc(m | (3 << 6), fd);

  for( ; m > 0; m--)
  { int b = (int)(n >> ((m-1)*8)) & 0xff;

    Sputc(b, fd);
  }
}


static void
putFloat(double f, IOSTREAM *fd)
{ unsigned char *cl = (unsigned char *)&f;
  unsigned int i;

  DEBUG(MSG_QLF_FLOAT, Sdprintf("putFloat(%f)\n", f));

  for(i=0; i<BYTES_PER_DOUBLE; i++)
    Sputc(cl[double_byte_order[i]], fd);
}


static void
putInt32(int v, IOSTREAM *fd)
{ Sputc((v>>24)&0xff, fd);
  Sputc((v>>16)&0xff, fd);
  Sputc((v>>8)&0xff, fd);
  Sputc(v&0xff, fd);
}


static void
freeXRSymbol(Symbol s)
{ word w = (word)s->name;

  if ( w&0x1 )
  { w &= ~0x1;
    if ( isAtom(w) )
    { PL_unregister_atom(w);
      DEBUG(5, Sdprintf("UNREG: %s\n", stringAtom(w)));
    }
  }
}


void
initXR(wic_state *state)
{ state->currentProc		   = NULL;
  state->currentSource		   = NULL;
  state->savedXRTable		   = newHTable(256);
  state->savedXRTable->free_symbol = freeXRSymbol;
  state->savedXRTableId		   = 0;
}


void
destroyXR(wic_state *state)
{ destroyHTable(state->savedXRTable);
  state->savedXRTable = NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
XR (External Reference)  table  handling.   The  table  contains  atoms,
functors  and  various  types  of    pointers   (Module,  Procedure  and
SourceFile). For savedXR()  to  work,  atom_t   and  functor_t  may  not
conflict with pointers. We assume -as in  many other places in the code-
that pointers are 4-byte aligned.

savedXRConstant()  must  be  used  for    atom_t  and  functor_t,  while
savedXRPointer  must  be  used  for   the    pointers.   The  value  for
savedXRConstant() is or-ed with 0x1 to avoid conflict with pointers.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
savedXR(wic_state *state, void *xr)
{ IOSTREAM *fd = state->wicFd;
  Symbol s;
  intptr_t id;

  if ( (s = lookupHTable(state->savedXRTable, xr)) )
  { id = (intptr_t) s->value;
    Sputc(XR_REF, fd);
    putNum(id, fd);

    succeed;
  } else
  { id = ++state->savedXRTableId;
    addHTable(state->savedXRTable, xr, (void *)id);
  }

  fail;
}


static inline int
savedXRConstant(wic_state *state, word w)
{ int rc;

  assert(tag(w) == TAG_ATOM);		/* Only functor_t and atom_t */

  if ( !(rc=savedXR(state, (void *)(w|0x1))) && isAtom(w) )
  { DEBUG(MSG_QLF_XR, Sdprintf("REG: %s\n", stringAtom(w)));
    PL_register_atom(w);
  }

  return rc;
}


static inline int
savedXRPointer(wic_state *state, void *p)
{ assert(((word)p & 0x1) == 0);

  return savedXR(state, p);
}


static void
saveXR__LD(wic_state *state, word xr ARG_LD)
{ IOSTREAM *fd = state->wicFd;

  if ( isTaggedInt(xr) )		/* TBD: switch */
  { Sputc(XR_INT, fd);
    putNum(valInt(xr), fd);
    return;
  } else if ( isBignum(xr) )
  { Sputc(XR_INT, fd);
    putNum(valBignum(xr), fd);
    return;
  } else if ( isFloat(xr) )
  { Sputc(XR_FLOAT, fd);
    putFloat(valFloat(xr), fd);
    return;
#if O_STRING
  } else if ( isString(xr) )
  { char *s;
    pl_wchar_t *w;
    size_t len;

    if ( (s = getCharsString(xr, &len)) )
    { Sputc(XR_STRING, fd);
      putString(s, len, fd);
    } else if ( (w=getCharsWString(xr, &len)) )
    { Sputc(XR_STRING_UTF8, fd);
      putStringW(w, len, fd);
    }
    return;
#endif /* O_STRING */
  }

  if ( savedXRConstant(state, xr) )
    return;

  if ( isAtom(xr) )
  { DEBUG(MSG_QLF_XR,
	  Sdprintf("XR(%d) = '%s'\n", state->savedXRTableId, stringAtom(xr)));
    putAtom(state, xr);
    return;
  }

  assert(0);
}
#define saveXR(state, xr) saveXR__LD(state, xr PASS_LD)


static void
saveXRBlobType(wic_state *state, PL_blob_t *type)
{ IOSTREAM *fd = state->wicFd;

  if ( savedXRPointer(state, type) )
    return;

  Sputc(XR_BLOB_TYPE, fd);
  putString(type->name, STR_NOLEN, fd);
}


static void
saveXRModule(wic_state *state, Module m ARG_LD)
{ IOSTREAM *fd = state->wicFd;

  if ( savedXRPointer(state, m) )
    return;

  if ( m )
  { Sputc(XR_MODULE, fd);
    DEBUG(MSG_QLF_XR,
	  Sdprintf("XR(%d) = module %s\n",
		   state->savedXRTableId, stringAtom(m->name)));
    saveXR(state, m->name);
  } else
  { Sputc(XR_NULL, fd);
  }
}


static void
saveXRFunctor(wic_state *state, functor_t f ARG_LD)
{ IOSTREAM *fd = state->wicFd;
  FunctorDef fdef;

  if ( savedXRConstant(state, f) )
    return;

  fdef = valueFunctor(f);

  DEBUG(MSG_QLF_XR,
	Sdprintf("XR(%d) = %s/%d\n",
		 state->savedXRTableId, stringAtom(fdef->name), fdef->arity));
  Sputc(XR_FUNCTOR, fd);
  saveXR(state, fdef->name);
  putNum(fdef->arity, fd);
}


static void
saveXRProc(wic_state *state, Procedure p ARG_LD)
{ IOSTREAM *fd = state->wicFd;

  if ( savedXRPointer(state, p) )
    return;

  DEBUG(MSG_QLF_XR, Sdprintf("XR(%d) = proc %s\n",
			     state->savedXRTableId, procedureName(p)));
  Sputc(XR_PRED, fd);
  saveXRFunctor(state, p->definition->functor->functor PASS_LD);
  saveXRModule(state, p->definition->module PASS_LD);
}


static void
saveXRSourceFile(wic_state *state, SourceFile f ARG_LD)
{ IOSTREAM *fd = state->wicFd;

  if ( savedXRPointer(state, f) )
    return;

  Sputc(XR_FILE, fd);

  if ( f )
  { DEBUG(MSG_QLF_XR, Sdprintf("XR(%d) = file %s\n",
			       state->savedXRTableId, stringAtom(f->name)));
    Sputc(f->system ? 's' : 'u', fd);
    saveXR(state, f->name);
    putFloat(f->mtime, fd);
  } else
  { DEBUG(MSG_QLF_XR, Sdprintf("XR(%d) = <no file>\n", state->savedXRTableId));
    Sputc('-', fd);
  }
}



static void
do_save_qlf_term(wic_state *state, Word t ARG_LD)
{ IOSTREAM *fd = state->wicFd;

  deRef(t);
  if ( isTerm(*t) )
  { functor_t f = functorTerm(*t);

    if ( f == FUNCTOR_dvard1 )
    { int id = (int)valInt(argTerm(*t, 0));

      Sputc('v', fd);
      putNum(id, fd);
    } else
    { Word q = argTermP(*t, 0);
      int n, arity = arityFunctor(f);

      Sputc('t', fd);
      saveXRFunctor(state, f PASS_LD);
      for(n=0; n < arity; n++, q++)
	do_save_qlf_term(state, q PASS_LD);
    }
  } else
  { assert(isAtomic(*t));
    saveXR(state, *t);
  }
}


static int
saveQlfTerm(wic_state *state, term_t t ARG_LD)
{ IOSTREAM *fd = state->wicFd;
  int nvars, rc=TRUE;
  fid_t cid;
  nv_options options;

  cid = PL_open_foreign_frame();

  DEBUG(MSG_QLF_TERM,
	Sdprintf("Saving ");
	PL_write_term(Serror, t, 1200, 0);
	Sdprintf(" from %d ... ", Stell(fd)));

  options.functor = FUNCTOR_dvard1;
  options.on_attvar = AV_SKIP;
  options.singletons = FALSE;		/* TBD: TRUE may be better! */
  options.numbered_check = TRUE;	/* otherwise may be wrong */

  if ( (nvars = numberVars(t, &options, 0 PASS_LD)) >= 0 )
  { putNum(nvars, fd);
    do_save_qlf_term(state, valTermRef(t) PASS_LD);	/* TBD */
    DEBUG(MSG_QLF_TERM, Sdprintf("to %d\n", Stell(fd)));
  } else
  { rc = FALSE;
  }

  PL_discard_foreign_frame(cid);

  return rc;
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
saveWicClause(wic_state *state, Clause clause)
{ GET_LD
  IOSTREAM *fd = state->wicFd;
  Code bp, ep;

  Sputc('C', fd);
  putNum(clause->code_size, fd);
  putNum(clause->line_no, fd);
  saveXRSourceFile(state, indexToSourceFile(clause->owner_no) PASS_LD);
  saveXRSourceFile(state, indexToSourceFile(clause->source_no) PASS_LD);
  putNum(clause->prolog_vars, fd);
  putNum(clause->variables, fd);
  putNum(true(clause, UNIT_CLAUSE) ? 0 : 1, fd);

  bp = clause->codes;
  ep = bp + clause->code_size;

  while( bp < ep )
  { code op = decode(*bp++);
    const char *ats = codeTable[op].argtype;
    int n;

    putNum(op, fd);
    DEBUG(MSG_QLF_VMI, Sdprintf("\t%s at %ld\n", codeTable[op].name, Stell(fd)));
    for(n=0; ats[n]; n++)
    { switch(ats[n])
      { case CA1_PROC:
	{ Procedure p = (Procedure) *bp++;
	  saveXRProc(state, p PASS_LD);
	  break;
	}
	case CA1_MODULE:
	{ Module m = (Module) *bp++;
	  saveXRModule(state, m PASS_LD);
	  break;
	}
	case CA1_FUNC:
	{ functor_t f = (functor_t) *bp++;
	  saveXRFunctor(state, f PASS_LD);
	  break;
	}
	case CA1_AFUNC:
	{ functor_t f = functorArithFunction((unsigned int)*bp++);
	  saveXRFunctor(state, f PASS_LD);
	  break;
	}
	case CA1_DATA:
	{ word xr = (word) *bp++;
	  saveXR(state, xr);
	  break;
	}
	case CA1_INTEGER:
	case CA1_JUMP:
	case CA1_VAR:
	case CA1_FVAR:
	case CA1_CHP:
	{ putNum(*bp++, fd);
	  break;
	}
	case CA1_INT64:
	{ int64_t val;
	  Word p = (Word)&val;

	  cpInt64Data(p, bp);
	  putNum(val, fd);
	  break;
	}
	case CA1_FLOAT:
	{ union
	  { word w[WORDS_PER_DOUBLE];
	    double f;
	  } v;
	  Word p = v.w;
	  cpDoubleData(p, bp);
	  putFloat(v.f, fd);
	  break;
	}
	case CA1_STRING:
	{ word m = *bp;
	  char *s = (char *)++bp;
	  size_t wn = wsizeofInd(m);
	  size_t l = wn*sizeof(word) - padHdr(m);
	  bp += wn;

	  putNum(l, fd);
	  while(l-- > 0)
	    Sputc(*s++&0xff, fd);
	  break;
	}
#ifdef O_GMP
	case CA1_MPZ:
	{ word m = *bp++;
	  size_t wn = wsizeofInd(m);
	  int mpsize = (int)*bp;
	  int l = abs(mpsize)*sizeof(mp_limb_t);
	  char *s = (char*)&bp[1];
	  bp += wn;

	  DEBUG(MSG_QLF_VMI, Sdprintf("Saving MPZ from %ld\n", Stell(fd)));
	  putNum(mpsize, fd);
	  while(--l >= 0)
	    Sputc(*s++&0xff, fd);
	  DEBUG(MSG_QLF_VMI, Sdprintf("Saved MPZ to %ld\n", Stell(fd)));
	  break;
	}
#endif
	default:
	  fatalError("No support for VM argtype %d (arg %d of %s)",
		     ats[n], n, codeTable[op].name);
      }
    }
  }
}


		/********************************
		*         COMPILATION           *
		*********************************/

static void
closeProcedureWic(wic_state *state)
{ if ( state->currentProc )
  { Sputc('X', state->wicFd);
    state->currentProc = NULL;
  }
}


static int
predicateFlags(Definition def, atom_t sclass)
{ int flags = 0;

  if ( sclass == ATOM_kernel )
  { if ( true(def, P_LOCKED) && false(def, HIDE_CHILDS) )
      return PRED_SYSTEM;
    return (PRED_SYSTEM|PRED_HIDE_CHILDS);
  }

  if ( true(def, P_LOCKED) )
    flags |= PRED_SYSTEM;
  if ( true(def, HIDE_CHILDS) )
    flags |= PRED_HIDE_CHILDS;

  return flags;
}


static void
openProcedureWic(wic_state *state, Procedure proc, atom_t sclass ARG_LD)
{ if ( proc != state->currentProc)
  { IOSTREAM *fd = state->wicFd;
    Definition def = proc->definition;
    int mode = predicateFlags(def, sclass);

    closeProcedureWic(state);
    state->currentProc = proc;

    if ( def->module != LD->modules.source )
    { Sputc('O', fd);
      saveXR(state, def->module->name);
    } else
    { Sputc('P', fd);
    }

    saveXRFunctor(state, def->functor->functor PASS_LD);
    putNum(mode, fd);
  }
}


static bool
putMagic(const char *s, IOSTREAM *fd)
{ for(; *s; s++)
    Sputc(*s, fd);
  Sputc(EOS, fd);

  succeed;
}


static bool
writeWicHeader(wic_state *state)
{ IOSTREAM *fd = state->wicFd;

  putMagic(saveMagic, fd);
  putNum(VERSION, fd);
  putNum(VM_SIGNATURE, fd);
  putNum(sizeof(word)*8, fd);	/* bits-per-word */
  if ( systemDefaults.home )
    putString(systemDefaults.home, STR_NOLEN, fd);
  else
    putString("<no home>",  STR_NOLEN, fd);

  initXR(state);

  DEBUG(MSG_QLF_SECTION, Sdprintf("Header complete ...\n"));
  succeed;
}


static bool
writeWicTrailer(wic_state *state)
{ IOSTREAM *fd = state->wicFd;

  closeProcedureWic(state);
  Sputc('X', fd);
  destroyXR(state);
  Sputc('T', fd);

  state->wicFd = NULL;
  if ( state->wicFile )
  { remove_string(state->wicFile);
    state->wicFile = NULL;
  }

  succeed;
}

/* FIXME: Deal with owner/real location in saved state
*/

static bool
addClauseWic(wic_state *state, term_t term, atom_t file ARG_LD)
{ Clause clause;
  sourceloc loc;

  loc.file = file;
  loc.line = source_line_no;

  if ( (clause = assert_term(term, CL_END, file, &loc PASS_LD)) )
  { openProcedureWic(state, clause->procedure, ATOM_development PASS_LD);
    saveWicClause(state, clause);

    succeed;
  }

  Sdprintf("Failed to compile: "); pl_write(term); Sdprintf("\n");
  fail;
}

static bool
addDirectiveWic(wic_state *state, term_t term ARG_LD)
{ IOSTREAM *fd = state->wicFd;

  closeProcedureWic(state);
  Sputc('D', fd);
  putNum(source_line_no, fd);

  return saveQlfTerm(state, term PASS_LD);
}


static bool
importWic(wic_state *state, Procedure proc, atom_t strength ARG_LD)
{ int flags = atomToImportStrength(strength);

  assert(flags >= 0);
  closeProcedureWic(state);

  Sputc('I', state->wicFd);
  saveXRProc(state, proc PASS_LD);
  putNum(flags, state->wicFd);

  succeed;
}

		 /*******************************
		 *	    PART MARKS		*
		 *******************************/

static void
initSourceMarks(wic_state *state)
{ state->has_source_marks = TRUE;
  state->source_mark_head = NULL;
  state->source_mark_tail = NULL;
}


static void
sourceMark(wic_state *state)
{ if ( state->has_source_marks )
  { SourceMark pm = allocHeapOrHalt(sizeof(struct source_mark));

    pm->file_index = Stell(state->wicFd);
    pm->next = NULL;
    if ( state->source_mark_tail )
    { state->source_mark_tail->next = pm;
      state->source_mark_tail = pm;
    } else
    { state->source_mark_tail = pm;
      state->source_mark_head = pm;
    }
  }
}


static int
writeSourceMarks(wic_state *state)
{ long n = 0;
  SourceMark pn, pm = state->source_mark_head;

  DEBUG(MSG_QLF_SECTION, Sdprintf("Writing source marks: "));

  for( ; pm; pm = pn )
  { pn = pm->next;

    DEBUG(MSG_QLF_SECTION, Sdprintf(" %d", pm->file_index));
    putInt32(pm->file_index, state->wicFd);
    freeHeap(pm, sizeof(*pm));
    n++;
  }
  state->source_mark_head = state->source_mark_tail = NULL;

  DEBUG(MSG_QLF_SECTION, Sdprintf("\nWritten %d marks\n", n));
  putInt32(n, state->wicFd);

  return 0;
}


static int
qlfSourceInfo(wic_state *state, size_t offset, term_t list ARG_LD)
{ IOSTREAM *s = state->wicFd;
  char *str;
  term_t head = PL_new_term_ref();
  atom_t fname;

  assert((long)offset >= 0);
  if ( Sseek(s, (long)offset, SIO_SEEK_SET) != 0 )
    return warning("%s: seek failed: %s", state->wicFile, OsError());
  if ( Sgetc(s) != 'F' || !(str=getString(s, NULL)) )
    return warning("QLF format error");
  fname = qlfFixSourcePath(state, str);

  return PL_unify_list(list, head, list) &&
         PL_unify_atom(head, fname);
}


static word
qlfInfo(const char *file,
	term_t cversion, term_t version, term_t wsize,
	term_t files0 ARG_LD)
{ IOSTREAM *s = NULL;
  int lversion;
  int nqlf, i;
  size_t *qlfstart = NULL;
  word rval = TRUE;
  term_t files = PL_copy_term_ref(files0);
  int saved_wsize;
  int vm_signature;
  wic_state state;

  TRY(PL_unify_integer(cversion, VERSION));

  memset(&state, 0, sizeof(state));
  state.wicFile = (char*)file;

  if ( !(s = Sopen_file(file, "rbr")) )
  { term_t f = PL_new_term_ref();

    PL_put_atom_chars(f, file);
    return PL_error(NULL, 0, OsError(), ERR_FILE_OPERATION,
		    ATOM_open, ATOM_source_sink, f);
  }
  state.wicFd = s;

  if ( !(lversion = qlfVersion(&state)) )
  { Sclose(s);
    fail;
  }
  TRY(PL_unify_integer(version, lversion));

  vm_signature = getInt(s);		/* TBD: provide to Prolog layer */
  (void)vm_signature;
  saved_wsize = getInt(s);		/* word-size of file */
  TRY(PL_unify_integer(wsize, saved_wsize));

  pushPathTranslation(&state, file, 0);

  if ( Sseek(s, -4, SIO_SEEK_END) < 0 )	/* 4 bytes of PutInt32() */
    return warning("qlf_info/4: seek failed: %s", OsError());
  nqlf = (int)getInt32(s);
  DEBUG(MSG_QLF_SECTION, Sdprintf("Found %d sources at", nqlf));
  qlfstart = (size_t*)allocHeapOrHalt(sizeof(size_t) * nqlf);
  Sseek(s, -4 * (nqlf+1), SIO_SEEK_END);
  for(i=0; i<nqlf; i++)
  { qlfstart[i] = (size_t)getInt32(s);
    DEBUG(MSG_QLF_SECTION, Sdprintf(" %ld", qlfstart[i]));
  }
  DEBUG(MSG_QLF_SECTION, Sdprintf("\n"));

  for(i=0; i<nqlf; i++)
  { if ( !qlfSourceInfo(&state, qlfstart[i], files PASS_LD) )
    { rval = FALSE;
      goto out;
    }
  }

  rval = PL_unify_nil(files);
  popPathTranslation(&state);

out:
  if ( qlfstart )
    freeHeap(qlfstart, sizeof(*qlfstart) * nqlf);
  if ( s )
    Sclose(s);

  return rval;
}


static
PRED_IMPL("$qlf_info", 5, qlf_info, 0)
{ PRED_LD
  char *name;

  if ( !PL_get_file_name(A1, &name, PL_FILE_ABSOLUTE) )
    fail;

  return qlfInfo(name, A2, A3, A4, A5 PASS_LD);
}



		 /*******************************
		 *	NEW MODULE SUPPORT	*
		 *******************************/

static wic_state *
qlfOpen(term_t file)
{ char *name;
  char *absname;
  char tmp[MAXPATHLEN];
  IOSTREAM *out;
  wic_state *state;

  if ( !PL_get_file_name(file, &name, 0) ||
       !(absname = AbsoluteFile(name, tmp)) )
    return NULL;

  if ( !(out = Sopen_file(name, "wbr")) )
  { PL_error(NULL, 0, NULL, ERR_PERMISSION, ATOM_write, ATOM_file, file);
    return NULL;
  }

  state = allocHeapOrHalt(sizeof(*state));
  memset(state, 0, sizeof(*state));
  state->wicFile = store_string(name);
  state->mkWicFile = store_string(name);
  state->wicFd = out;
  initXR(state);
  initSourceMarks(state);

  putMagic(qlfMagic, state->wicFd);
  putNum(VERSION, state->wicFd);
  putNum(VM_SIGNATURE, state->wicFd);
  putNum(sizeof(word)*8, state->wicFd);

  putString(absname, STR_NOLEN, state->wicFd);

  return state;
}


static bool
qlfClose(wic_state *state ARG_LD)
{ int rc;

  closeProcedureWic(state);
  writeSourceMarks(state);
  rc = Sclose(state->wicFd);
  state->wicFd = NULL;
  if ( state->mkWicFile )
  { remove_string(state->mkWicFile);
    state->mkWicFile = NULL;
  }
  destroyXR(state);

  LD->qlf.current_state = state->parent;
  freeHeap(state, sizeof(*state));

  return rc == 0;
}


static int
qlfVersion(wic_state *state)
{ IOSTREAM *s = state->wicFd;
  char mbuf[100];
  char *magic;

  if ( !(magic = getMagicString(s, mbuf, sizeof(mbuf))) ||
       !streq(magic, qlfMagic) )
  { Sclose(s);
    return warning("%s: not a SWI-Prolog .qlf file", state->wicFile);
  }

  return getInt(s);
}


static int
pushPathTranslation(wic_state *state, const char *absloadname, int flags)
{ IOSTREAM *fd = state->wicFd;
  char *abssavename;
  qlf_state *new = allocHeapOrHalt(sizeof(*new));

  memset(new, 0, sizeof(*new));
  new->previous = state->load_state;
  state->load_state = new;

  abssavename = getString(fd, NULL);
  if ( absloadname && !streq(absloadname, abssavename) )
  { char load[MAXPATHLEN];
    char save[MAXPATHLEN];
    char *l, *s, *le, *se;

    new->has_moved = TRUE;

    if ( (flags & PATH_ISDIR) )
    { l = strcpy(load, absloadname);
      s = strcpy(save, abssavename);
    } else
    { l = DirName(absloadname, load);
      s = DirName(abssavename, save);
    }
    le = l+strlen(l);
    se = s+strlen(s);
    for( ;le>l && se>s && le[-1] == se[-1]; le--, se--)
    { if ( le[-1] == '/' )
      { *le = EOS;
        *se = EOS;
      }
    }

    new->load_dir = store_string(l);
    new->save_dir = store_string(s);
    DEBUG(MSG_QLF_PATH,
	  Sdprintf("QLF file has moved; replacing %s --> %s\n",
		   state->load_state->save_dir,
		   state->load_state->load_dir));
  }

  succeed;
}


static void
popPathTranslation(wic_state *state)
{ if ( state->load_state )
  { qlf_state *old = state->load_state;

    state->load_state = old->previous;

    if ( old->has_moved )
    { remove_string(old->load_dir);
      remove_string(old->save_dir);
    }
    freeHeap(old, sizeof(*old));
  }
}

static bool
qlfLoad(wic_state *state, Module *module ARG_LD)
{ IOSTREAM *fd = state->wicFd;
  bool rval;
  int lversion;
  const char *absloadname;
  char tmp[MAXPATHLEN];
  int saved_wsize;
  int vm_signature;
  atom_t file;

  if ( (file = fileNameStream(fd)) )
  { PL_chars_t text;

    if ( !get_atom_text(file, &text) )
      fail;
    if ( !PL_mb_text(&text, REP_FN) )
    { PL_free_text(&text);
      fail;
    }
    state->wicFile = store_string(text.text.t);
    if ( !(absloadname = AbsoluteFile(state->wicFile, tmp)) )
      fail;
    PL_free_text(&text);
  } else
  { absloadname = NULL;
  }

  if ( !(lversion = qlfVersion(state)) || lversion < LOADVERSION )
  { if ( lversion )
      warning("$qlf_load/1: %s bad version (file version = %d, prolog = %d)",
	      state->wicFile, lversion, VERSION);
    fail;
  }
  vm_signature = getInt(fd);
  if ( vm_signature != (int)VM_SIGNATURE )
  { warning("QLF file %s has incompatible VM-signature (0x%x; expected 0x%x)",
	    stringAtom(file),
	    (unsigned int)vm_signature,
	    (unsigned int)VM_SIGNATURE);
    fail;
  }
  saved_wsize = getInt(fd);
  if ( saved_wsize != sizeof(word)*8 )
  { warning("QLF file %s has incompatible (%d) word-length",
	    stringAtom(file), (int)saved_wsize);
    fail;
  }

  pushPathTranslation(state, absloadname, 0);
  state->load_state->saved_version = lversion;

  pushXrIdTable(state);
  for(;;)
  { int c = Qgetc(fd);

    switch(c)
    { case 'Q':
        break;
      case 'I':
	loadInclude(state PASS_LD);
        continue;
      default:
	qlfLoadError(state);
    }

    break;
  }

  rval = loadPart(state, module, FALSE PASS_LD);
  popXrIdTable(state);
  popPathTranslation(state);

  return rval;
}


static bool
qlfSaveSource(wic_state *state, SourceFile f)
{ IOSTREAM *fd = state->wicFd;
  Atom a = atomValue(f->name);

  sourceMark(state);
  Sputc('F', fd);
  putString(a->name, a->length, fd);
  putFloat(f->mtime, fd);
  Sputc(f->system ? 's' : 'u', fd);

  state->currentSource = f;

  succeed;
}


static bool
qlfStartModule(wic_state *state, Module m ARG_LD)
{ IOSTREAM *fd = state->wicFd;
  ListCell c;
  closeProcedureWic(state);
  Sputc('Q', fd);
  Sputc('M', fd);
  saveXR(state, m->name);

  if ( m->file )
  { qlfSaveSource(state, m->file);
    putNum(m->line_no, fd);
  } else
  { Sputc('-', fd);
  }

  Sputc('C', fd);
  saveXR(state, m->class);
  for(c=m->supers; c; c=c->next)
  { Module s = c->value;

    Sputc('S', fd);
    saveXR(state, s->name);
  }

  DEBUG(MSG_QLF_SECTION, Sdprintf("MODULE %s\n", stringAtom(m->name)));
  for_unlocked_table(m->public, s,
		     { functor_t f = (functor_t)s->name;

		       DEBUG(MSG_QLF_EXPORT,
			     Sdprintf("Exported %s/%d\n",
				      stringAtom(nameFunctor(f)),
				      arityFunctor(f)));
		       Sputc('E', fd);
		       saveXRFunctor(state, f PASS_LD);
		     })

  Sputc('X', fd);

  succeed;
}


static bool
qlfStartSubModule(wic_state *state, Module m ARG_LD)
{ IOSTREAM *fd = state->wicFd;

  closeProcedureWic(state);
  Sputc('M', fd);
  saveXR(state, m->name);

  succeed;
}


static bool
qlfStartFile(wic_state *state, SourceFile f)
{ IOSTREAM *fd = state->wicFd;

  closeProcedureWic(state);
  Sputc('Q', fd);
  qlfSaveSource(state, f);

  succeed;
}


static bool
qlfEndPart(wic_state *state)
{ IOSTREAM *fd = state->wicFd;

  closeProcedureWic(state);
  Sputc('X', fd);

  succeed;
}


/** '$qlf_start_module'(+Module)

Start emitting a module.
*/

static
PRED_IMPL("$qlf_start_module", 1, qlf_start_module, 0)
{ PRED_LD
  wic_state *state;

  if ( (state=LD->qlf.current_state) )
  { Module m;

    if ( !PL_get_module_ex(A1, &m) )
      fail;

    return qlfStartModule(state, m PASS_LD);
  }

  succeed;
}


static
PRED_IMPL("$qlf_start_sub_module", 1, qlf_start_sub_module, 0)
{ PRED_LD
  wic_state *state;

  if ( (state=LD->qlf.current_state) )
  { Module m;

    if ( !PL_get_module_ex(A1, &m) )
      fail;

    return qlfStartSubModule(state, m PASS_LD);
  }

  succeed;
}


static
PRED_IMPL("$qlf_start_file", 1, qlf_start_file, 0)
{ PRED_LD
  wic_state *state;

  if ( (state=LD->qlf.current_state) )
  { atom_t a;

    if ( !PL_get_atom_ex(A1, &a) )
      fail;

    return qlfStartFile(state, lookupSourceFile(a, TRUE));
  }

  succeed;
}


static
PRED_IMPL("$qlf_current_source", 1, qlf_current_source, 0)
{ PRED_LD
  wic_state *state;
  SourceFile sf;

  if ( (state=LD->qlf.current_state) &&
       (sf = state->currentSource) )
  { return PL_unify_atom(A1, sf->name);
  }

  return FALSE;
}


static
PRED_IMPL("$qlf_include", 5, qlf_include, 0)
{ PRED_LD
  atom_t owner, pn, fn;
  int line;
  double time;
  wic_state *state;

  if ( PL_get_atom_ex(A1, &owner) &&
       PL_get_atom_ex(A2, &pn) &&
       PL_get_integer_ex(A3, &line) &&
       PL_get_atom_ex(A4, &fn) &&
       PL_get_float(A5, &time) &&
       (state=LD->qlf.current_state) )
  { IOSTREAM *fd = state->wicFd;

    Sputc('I', fd);
    putAtom(state, owner);
    putAtom(state, pn);
    putNum(line, fd);
    putAtom(state, fn);
    putFloat(time, fd);

    return TRUE;
  }

  return FALSE;
}


static
PRED_IMPL("$qlf_end_part", 0, qlf_end_part, 0)
{ PRED_LD
  wic_state *state;

  if ( (state=LD->qlf.current_state) )
  { return qlfEndPart(state);
  }

  succeed;
}


static
PRED_IMPL("$qlf_open", 1, qlf_open, 0)
{ PRED_LD
  wic_state *state = qlfOpen(A1);

  if ( state )
  { state->parent = LD->qlf.current_state;
    LD->qlf.current_state = state;

    return TRUE;
  }

  return FALSE;
}


static
PRED_IMPL("$qlf_close", 0, qlf_close, 0)
{ PRED_LD
  wic_state *state;

  if ( (state=LD->qlf.current_state) )
    return qlfClose(state PASS_LD);

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
$qlf_load(:Stream, -ModuleOut)

Load QLF data from Stream.

@param	ModuleOut is unified to an atom holding the name of the
	loaded module or the integer 0 if the loaded object is
	not a module.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("$qlf_load", 2, qlf_load, PL_FA_TRANSPARENT)
{ GET_LD
  term_t qstream = A1;
  term_t module = A2;
  Module m, oldsrc = LD->modules.source;
  bool rval;
  term_t stream = PL_new_term_ref();
  IOSTREAM *fd;
  IOENC saved_enc;
  wic_state state;

  m = oldsrc;
  if ( !PL_strip_module(qstream, &m, stream) )
    fail;
  if ( !PL_get_stream_handle(stream, &fd) )
    fail;

  memset(&state, 0, sizeof(state));
  state.wicFd = fd;

  saved_enc = fd->encoding;
  fd->encoding = ENC_OCTET;
  LD->modules.source = m;
  rval = qlfLoad(&state, &m PASS_LD);
  LD->modules.source = oldsrc;
  fd->encoding = saved_enc;

  if ( state.wicFile )
    remove_string(state.wicFile);

  if ( rval )
  { if ( m )
      return PL_unify_atom(module, m->name);

    return PL_unify_integer(module, 0);
  }

  fail;
}


		/********************************
		*        PROLOG SUPPORT         *
		*********************************/

/** '$open_wic'(+Stream) is det.

Write a header for a QLF-stream
*/

static
PRED_IMPL("$open_wic", 1, open_wic, 0)
{ GET_LD
  IOSTREAM *fd;

  if ( PL_get_stream_handle(A1, &fd) )
  { wic_state *state = allocHeapOrHalt(sizeof(*state));

    memset(state, 0, sizeof(*state));
    state->wicFd = fd;
    writeWicHeader(state);
    state->parent = LD->qlf.current_state;
    LD->qlf.current_state = state;

    succeed;
  }

  fail;					/* PL_get_stream_handle() */
					/* throws exception */
}


static
PRED_IMPL("$close_wic", 0, close_wic, 0)
{ PRED_LD
  wic_state *state;

  if ( (state=LD->qlf.current_state) )
  { writeWicTrailer(state);

    LD->qlf.current_state = state->parent;
    freeHeap(state, sizeof(*state));

    succeed;
  }

  fail;
}


static
PRED_IMPL("$add_directive_wic", 1, add_directive_wic, PL_FA_TRANSPARENT)
{ PRED_LD
  wic_state *state;

  if ( (state=LD->qlf.current_state) )
  { Module m = MODULE_system;
    term_t term = PL_new_term_ref();
    term_t qterm = PL_new_term_ref();

    PL_strip_module(A1, &m, term);
    if ( !(PL_is_callable(term)) )
      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, A1);

    if ( !PL_unify_term(qterm,
			PL_FUNCTOR, FUNCTOR_colon2,
			  PL_ATOM, m->name,
			  PL_TERM, term) )
      return FALSE;

    return addDirectiveWic(state, qterm PASS_LD);
  }

  succeed;
}


/** '$import_wic'(+Module, +PredicateIndicator, +Strength)
*/

static
PRED_IMPL("$import_wic", 3, import_wic, 0)
{ PRED_LD
  wic_state *state;

  if ( (state=LD->qlf.current_state) )
  { Module m = NULL;
    functor_t fd;
    atom_t strength;

    if ( !PL_get_module(A1, &m) ||
	 !get_functor(A2, &fd, &m, 0, GF_PROCEDURE) ||
	 !PL_get_atom_ex(A3, &strength) )
      fail;

    return importWic(state, lookupProcedure(fd, m), strength PASS_LD);
  }

  succeed;
}


/** '$qlf_assert_clause'(+ClauseRef, +Class) is det.
*/

static
PRED_IMPL("$qlf_assert_clause", 2, qlf_assert_clause, 0)
{ PRED_LD
  wic_state *state;

  if ( (state=LD->qlf.current_state) )
  { Clause clause;
    atom_t sclass;

    if ( (PL_get_clref(A1, &clause) != TRUE) ||
	 !PL_get_atom_ex(A2, &sclass) )
      fail;

    openProcedureWic(state, clause->procedure, sclass PASS_LD);
    saveWicClause(state, clause);
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
{ GET_LD
  atom_t name;
  int arity;
  term_t d0 = PL_new_term_ref();
  functor_t f;

  if ( !PL_get_name_arity(clause, &name, &arity) ||
       arity != 1 ||
       !streq(stringAtom(name), functor) )
    fail;

  _PL_get_arg(1, clause, d0);
  if ( PL_get_functor(d0, &f) && f == FUNCTOR_colon2 )
  { PL_put_term(directive, d0);
  } else
  { term_t m;

    if ( !(m = PL_new_term_ref()) )
      return FALSE;
    PL_put_atom(m, LD->modules.source->name);
    return PL_cons_functor(directive, FUNCTOR_colon2, m, d0);
  }

  succeed;
}

/*  Compile an entire file into intermediate code.

 ** Thu Apr 28 13:44:43 1988  jan@swivax.UUCP (Jan Wielemaker)  */

static bool
compileFile(wic_state *state, const char *file)
{ GET_LD
  char tmp[MAXPATHLEN];
  char *path;
  term_t f = PL_new_term_ref();
  SourceFile sf;
  atom_t nf;

  DEBUG(MSG_QLF_BOOT, Sdprintf("Boot compilation of %s\n", file));
  if ( !(path = AbsoluteFile(file, tmp)) )
    fail;
  DEBUG(MSG_QLF_PATH, Sdprintf("Expanded to %s\n", path));

  nf = PL_new_atom(path);			/* NOTE: Only ISO-Latin-1 */
  PL_put_atom(f, nf);
  DEBUG(MSG_QLF_BOOT, Sdprintf("Opening\n"));
  if ( !pl_see(f) )
    fail;
  DEBUG(MSG_QLF_BOOT, Sdprintf("pl_start_consult()\n"));
  sf = lookupSourceFile(nf, TRUE);
  startConsult(sf);
  if ( !LastModifiedFile(path, &sf->mtime) )
    Sdprintf("Failed to get time from %s\n", path);
  qlfStartFile(state, sf);

  for(;;)
  { fid_t	 cid = PL_open_foreign_frame();
    term_t         t = PL_new_term_ref();
    term_t directive = PL_new_term_ref();
    atom_t eof;

    DEBUG(2, Sdprintf("pl_read_clause() -> "));
    PL_put_variable(t);
    if ( !read_clause(Scurin, t, 0 PASS_LD) ) /* syntax error */
    { Sdprintf("%s:%d: Syntax error\n",
	       PL_atom_chars(source_file_name),
	       source_line_no);
      continue;
    }
    if ( PL_get_atom(t, &eof) && eof == ATOM_end_of_file )
      break;

    DEBUG(MSG_QLF_BOOT_READ,
	  PL_write_term(Serror, t, 1200, PL_WRT_NUMBERVARS);
	  Sdprintf("\n"));

    if ( directiveClause(directive, t, ":-") )
    { DEBUG(MSG_QLF_DIRECTIVE,
	    Sdprintf(":- ");
	    PL_write_term(Serror, directive, 1200, 0);
	    Sdprintf(".\n") );
      addDirectiveWic(state, directive PASS_LD);
      if ( !callProlog(MODULE_user, directive, PL_Q_NODEBUG, NULL) )
	Sdprintf("%s:%d: directive failed\n",
		 PL_atom_chars(source_file_name),
		 source_line_no);
    } else if ( directiveClause(directive, t, "$:-") )
    { DEBUG(MSG_QLF_DIRECTIVE,
	    Sdprintf("$:- ");
	    PL_write_term(Serror, directive, 1200, 0);
	    Sdprintf(".\n"));
      callProlog(MODULE_user, directive, PL_Q_NODEBUG, NULL);
    } else
      addClauseWic(state, t, nf PASS_LD);

    PL_discard_foreign_frame(cid);
  }

  qlfEndPart(state);
  pl_seen();

  succeed;
}


bool
compileFileList(IOSTREAM *fd, int argc, char **argv)
{ GET_LD
  wic_state *state = allocHeapOrHalt(sizeof(*state));
  predicate_t pred;
  int rc;
  access_level_t alevel;

  memset(state, 0, sizeof(*state));
  state->wicFd = fd;

  if ( !writeWicHeader(state) )
    return FALSE;

  alevel = setAccessLevel(ACCESS_LEVEL_SYSTEM);
  PL_set_prolog_flag("autoload", PL_BOOL, FALSE);

  LD->qlf.current_state = state; /* make Prolog compilation go into state */
  for(;argc > 0; argc--, argv++)
  { if ( streq(argv[0], "-c" ) )
      break;
    if ( !compileFile(state, argv[0]) )
      return FALSE;
  }

  PL_set_prolog_flag("autoload", PL_BOOL, TRUE);
  setAccessLevel(alevel);

  pred = PL_predicate("$load_additional_boot_files", 0, "user");
  rc = PL_call_predicate(MODULE_user, TRUE, pred, 0);
  if ( rc )
    rc = writeWicTrailer(state);

  LD->qlf.current_state = NULL;
  freeHeap(state, sizeof(*state));

  return rc;
}


		 /*******************************
		 *	     CLEANUP		*
		 *******************************/

void
qlfCleanup(void)
{ GET_LD
  wic_state *state;
  char *buf;

  while ( (state=LD->qlf.current_state) )
  { if ( state->mkWicFile )
    { printMessage(ATOM_warning,
		   PL_FUNCTOR_CHARS, "qlf", 1,
		     PL_FUNCTOR_CHARS, "removed_after_error", 1,
		       PL_CHARS, state->mkWicFile);
      RemoveFile(state->mkWicFile);
      remove_string(state->mkWicFile);
      state->mkWicFile = NULL;
    }

    LD->qlf.current_state = state->parent;
    freeHeap(state, sizeof(*state));
  }

  if ( (buf=LD->qlf.getstr_buffer) )
  { LD->qlf.getstr_buffer = NULL;
    LD->qlf.getstr_buffer_size = 0;
    free(buf);
  }
}

		 /*******************************
		 *	 PUBLIC FUNCTIONS	*
		 *******************************/

void
wicPutNum(int64_t n, IOSTREAM *fd)
{ putNum(n, fd);
}


int64_t
wicGetNum(IOSTREAM *fd)
{ return getInt64(fd);
}


void
wicPutStringW(const pl_wchar_t *w, size_t len, IOSTREAM *fd)
{ putStringW(w, len, fd);
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(wic)
  PRED_DEF("$qlf_info",		    5, qlf_info,	     0)
  PRED_DEF("$qlf_load",		    2, qlf_load,	     PL_FA_TRANSPARENT)
  PRED_DEF("$add_directive_wic",    1, add_directive_wic,    PL_FA_TRANSPARENT)
  PRED_DEF("$qlf_start_module",	    1, qlf_start_module,     0)
  PRED_DEF("$qlf_start_sub_module", 1, qlf_start_sub_module, 0)
  PRED_DEF("$qlf_start_file",	    1, qlf_start_file,	     0)
  PRED_DEF("$qlf_current_source",   1, qlf_current_source,   0)
  PRED_DEF("$qlf_include",          5, qlf_include,          0)
  PRED_DEF("$qlf_end_part",	    0, qlf_end_part,	     0)
  PRED_DEF("$qlf_open",		    1, qlf_open,	     0)
  PRED_DEF("$qlf_close",	    0, qlf_close,	     0)
  PRED_DEF("$qlf_assert_clause",    2, qlf_assert_clause,    0)
  PRED_DEF("$open_wic",		    1, open_wic,	     0)
  PRED_DEF("$close_wic",	    0, close_wic,	     0)
  PRED_DEF("$import_wic",	    3, import_wic,	     0)
EndPredDefs
