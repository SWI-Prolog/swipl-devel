/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2022, University of Amsterdam
                              VU University Amsterdam
			      CWI, Amsterdam
			      SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/*#define O_DEBUG 1*/
#include "pl-wic.h"
#include "pl-comp.h"
#include "pl-arith.h"
#include "os/pl-utf8.h"
#include "pl-dbref.h"
#include "pl-dict.h"
#include "pl-funct.h"
#include "pl-proc.h"
#include "pl-util.h"
#include "pl-modul.h"
#include "pl-srcfile.h"
#include "pl-pro.h"
#include "pl-fli.h"
#include "pl-prims.h"
#include "pl-write.h"
#include "pl-read.h"
#include "os/pl-ctype.h"
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef O_DEBUG
#define Qgetc(s) Sgetc(s)
#define TRACK_POS ""
#else
#define Qgetc(s) Snpgetc(s)		/* ignore position recording */
#define TRACK_POS "r"
#endif

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
			XR_NIL				% []
			XR_CONS				% functor of [_|_]
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

Integers are stored in  a  packed  format   to  reduce  the  size of the
intermediate code file as  99%  of  them   is  normally  small,  but  in
principle not limited (virtual  machine   codes,  arities,  table sizes,
etc). We use the "zigzag" encoding to   deal  with negative integers and
write the positive value in chunks  of   7  bits, least significant bits
first. The last byte has its 0x80 mask set.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define QLFMAGICNUM 0x716c7374		/* "qlst" on little-endian machine */

#define XR_REF		0		/* reference to previous */
#define XR_NIL		1		/* [] */
#define XR_CONS		2		/* functor of [_|_] */
#define XR_ATOM		3		/* atom */
#define XR_FUNCTOR	4		/* functor */
#define XR_PRED		5		/* procedure */
#define XR_INT		6		/* int */
#define XR_FLOAT	7		/* float */
#define XR_STRING	8		/* string */
#define XR_FILE		9		/* source file */
#define XR_MODULE      10		/* a module */
#define XR_BLOB	       11		/* a typed atom (blob) */
#define XR_BLOB_TYPE   12		/* name of atom-type declaration */
#define XR_STRING_UTF8 13		/* Wide character string */
#define XR_NULL	       14		/* NULL pointer */

#define V_LABEL	      256		/* Label pseudo opcode */
#define V_H_INTEGER   257		/* Abstract various H_INT variations */
#define V_B_INTEGER   258		/* Abstract various B_INT variations */
#define V_A_INTEGER   259		/* Abstract various A_INT variations */

#define PRED_SYSTEM	 0x01		/* system predicate */
#define PRED_HIDE_CHILDS 0x02		/* hide my childs */
#define PRED_DET         0x04		/* Determinism flag */

#define CLAUSE_UNIT_CLAUSE 0x01
#define CLAUSE_SSU_COMMIT  0x02
#define CLAUSE_SSU_CHOICE  0x04
#define CLAUSE_HEAD_TERMS  0x08

static char saveMagic[] = "SWI-Prolog state (www.swi-prolog.org)\n";
static char qlfMagic[]  = "SWI-Prolog .qlf file\n";

typedef struct source_mark
{ long		file_index;
  struct source_mark *next;
} source_mark, *SourceMark;


#define XR_BLOCKS 32
typedef struct xr_table
{ unsigned int	id;			/* next id to give out */
  struct xr_table* previous;		/* stack */
  Word	        blocks[XR_BLOCKS];	/* main table */
  word		preallocated[7];
} xr_table, *XrTable;


typedef struct path_translated
{ struct path_translated *next;
  atom_t from;
  atom_t to;
} path_translated;

typedef struct qlf_state
{ char *save_dir;			/* Directory saved */
  char *load_dir;			/* Directory loading */
  int	has_moved;			/* Paths must be translated */
  path_translated *translated;		/* Translated paths */
  struct qlf_state *previous;		/* previous saved state (reentrance) */
} qlf_state;


typedef struct wic_state
{ char *wicFile;			/* name of output file */
  char *mkWicFile;			/* Wic file under construction */
  IOSTREAM *wicFd;			/* file descriptor of wic file */

  Definition currentPred;		/* current procedure */
  SourceFile currentSource;		/* current source file */

  Table idMap;				/* mapped identifiers */
  Table	savedXRTable;			/* saved XR entries */
  intptr_t savedXRTableId;		/* next id to hand out */

  SourceMark source_mark_head;		/* Locations of sources */
  SourceMark source_mark_tail;
  int	     has_source_marks;

  int        saved_version;		/* Version saved */
  int	     obfuscate;			/* Obfuscate source */
  int	     load_nesting;		/* Nesting level of loadPart() */
  qlf_state *load_state;		/* current load-state */

  xr_table *XR;				/* external references */

  struct
  { int		invalid_wide_chars;	/* Cannot represent due to UCS-2 */
  } errors;

  struct wic_state *parent;		/* parent state */
} wic_state;

#if USE_LD_MACROS
#define	loadPredicate(state, skip)	LDFUNC(loadPredicate, state, skip)
#define	loadImport(state, skip)		LDFUNC(loadImport, state, skip)
#define	loadXRc(state, c)		LDFUNC(loadXRc, state, c)
#define	getBlob(state)			LDFUNC(getBlob, state)
#define	loadStatement(state, c, skip)	LDFUNC(loadStatement, state, c, skip)
#define	loadPart(state, module, skip)	LDFUNC(loadPart, state, module, skip)
#define	loadInModule(state, skip)	LDFUNC(loadInModule, state, skip)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

static char *	getString(IOSTREAM *, size_t *len);
static int64_t	getInt64(IOSTREAM *);
static int	getInt32(IOSTREAM *s);
static int	getInt(IOSTREAM *);
static double	getFloat(IOSTREAM *);
static bool	loadWicFd(wic_state *state);
static bool	loadPredicate(wic_state *state, int skip);
static bool	loadImport(wic_state *state, int skip);
static void	saveXRBlobType(wic_state *state, PL_blob_t *type);
static void	putString(const char *, size_t len, IOSTREAM *);
static void	putInt64(int64_t, IOSTREAM *);
static void	putFloat(double, IOSTREAM *);
static void	saveWicClause(wic_state *state, Clause cl);
static void	closePredicateWic(wic_state *state);
static word	loadXRc(wic_state *state, int c);
static atom_t   getBlob(wic_state *state);
static bool	loadStatement(wic_state *state, int c, int skip);
static bool	loadPart(wic_state *state, Module *module, int skip);
static bool	loadInModule(wic_state *state, int skip);
static int	qlfVersion(wic_state *state, const char *magic, int *vp);
static atom_t	qlfFixSourcePath(wic_state *state, const char *raw);
static int	pushPathTranslation(wic_state *state, const char *loadname, int flags);
static void	popPathTranslation(wic_state *state);
static int	qlfIsCompatible(wic_state *state, const char *magic);

#undef LDFUNC_DECLARATIONS

/* Convert CA1_VAR arguments to VM independent and back
*/
#define VAR_OFFSET(i) ((intptr_t)((i) - (ARGOFFSET / (intptr_t) sizeof(word))))
#define OFFSET_VAR(i) ((intptr_t)((i) + (ARGOFFSET / (intptr_t) sizeof(word))))

#undef LD
#define LD LOCAL_LD


		 /*******************************
		 *     LOADED XR ID HANDLING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
XR reference handling during loading. This   uses  a dynamic array using
doubling sub arrays as also used for atoms, functors, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
pushXrIdTable(wic_state *state)
{ XrTable t = allocHeapOrHalt(sizeof(*t));

  memset(t, 0, sizeof(*t));
  t->id = 0;
  t->blocks[0] = t->preallocated - 1;
  t->blocks[1] = t->preallocated - 1;
  t->blocks[2] = t->preallocated - 1;

  t->previous = state->XR;
  state->XR = t;
}


static void
popXrIdTable(wic_state *state)
{ XrTable t = state->XR;
  unsigned int id, idx;

  state->XR = t->previous;		/* pop the stack */

  for(id=0; id < 7; id++)
  { word w = t->preallocated[id];

    if ( isAtom(w) )
      PL_unregister_atom(w);
  }
  for(idx = 3; idx < XR_BLOCKS && t->blocks[idx]; idx++)
  { size_t bs = (size_t)1<<idx;
    Word p = t->blocks[idx]+bs;
    size_t i;

    for(i=0; i<bs && id < t->id; i++, id++)
    { word w = p[i];

      if ( isAtom(w) )
	PL_unregister_atom(w);
    }

    freeHeap(p, bs*sizeof(word));
  }

  freeHeap(t, sizeof(*t));
}


static word
lookupXrId(wic_state *state, unsigned int id)
{ XrTable t = state->XR;
  unsigned int idx = MSB(id);

  DEBUG(CHK_SECURE, assert(t->blocks[idx]));
  return t->blocks[idx][id];
}


static void
storeXrId(wic_state *state, unsigned int id, word value)
{ XrTable t = state->XR;
  unsigned int idx = MSB(id);

  if ( !t->blocks[idx] )
  { size_t bs = (size_t)1<<idx;
    Word newblock;

    newblock = allocHeapOrHalt(bs*sizeof(word));
    t->blocks[idx] = newblock-bs;
  }

  t->blocks[idx][id] = value;
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Load a string from the input stream.   There are two cases: 0-terminated
short strings (files, etc) have  length  set   to  NULL  and the general
Prolog string case has length pointing to  a pointer. The latter is used
only for saved (directive) terms and the   result  is thus pushed to the
global stack.

Returns NULL if the string is too large.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static char *
getString(IOSTREAM *fd, size_t *length)
{ GET_LD
  char *s;
  size_t len = (size_t)getInt64(fd);
  size_t i;

  if ( !length && len > PATH_MAX )
    return NULL;
  if ( length && len > globalStackLimit() )
    return NULL;

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
  { int c = Qgetc(fd);

    if ( c == EOF )
      fatalError("Unexpected EOF on QLF file at offset %d",
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
{ size_t i, len = (size_t)getInt64(fd);
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
  { int c = Qgetc(fd);

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
{ const char *name;

  if ( !(name = getString(fd, NULL)) )
    fatalError("Invalid blob type in QLF");

  return PL_find_blob_type(name);
}


static char *
getMagicString(IOSTREAM *fd, char *buf, int maxlen)
{ char *s;
  int c;

  for( s = buf; --maxlen >= 0 && (*s = (c = Sgetc(fd))); s++ )
  { if ( c == EOF )
      return NULL;
  }

  if ( maxlen > 0 )
    return buf;

  return NULL;
}


static inline uint64_t
zigzag_encode(int64_t n)
{ return (n << 1) ^ (n >> 63);
}


static inline int64_t
zigzag_decode(uint64_t n)
{ return (n >> 1) ^ -(n&1);
}


static int64_t
getInt64(IOSTREAM *fd)
{ int c = Qgetc(fd);

  if ( c&0x80 )
  { DEBUG(MSG_QLF_INTEGER, Sdprintf("%" PRId64 "\n", zigzag_decode(c&0x7f)));
    return zigzag_decode(c&0x7f);
  } else
  { uint64_t v = c&0x7f;
    int shift = 7;

    for(;;)
    { c = Qgetc(fd);

      if ( c&0x80 )
      { uint64_t l = (c&0x7f);
	v |= l<<shift;
	DEBUG(MSG_QLF_INTEGER, Sdprintf("%" PRId64 "\n", zigzag_decode(v)));
	return zigzag_decode(v);
      } else
      { uint64_t b = c;
	v |= b<<shift;
	shift += 7;
      }
    }
  }
}


static int
getInt(IOSTREAM *fd)
{ int64_t val = getInt64(fd);

  return (int)val;
}


static unsigned int
getUInt(IOSTREAM *fd)
{ unsigned int c = Qgetc(fd);

  if ( c&0x80 )
  { DEBUG(MSG_QLF_INTEGER, Sdprintf("%d\n", c&0x7f));
    return c&0x7f;
  } else
  { unsigned int v = c&0x7f;
    int shift = 7;

    for(;;)
    { c = Qgetc(fd);

      if ( c&0x80 )
      { unsigned int l = (c&0x7f);
	v |= l<<shift;
	DEBUG(MSG_QLF_INTEGER, Sdprintf("%d\n", v));
	return v;
      } else
      { unsigned int b = c;
	v |= b<<shift;
	shift += 7;
      }
    }
  }
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
  { int c = Qgetc(fd);

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


#define loadXR(state) LDFUNC(loadXR, state)
static inline word
loadXR(DECL_LD wic_state *state)
{ return loadXRc(state, Qgetc(state->wicFd));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
loadXRc(DECL_LD int c0, IOSTREAM *fd) loads   a constant from the stream.
Note that some constants (integers, floats and  strings) can cause GC or
stack-shifts.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static word
loadXRc(DECL_LD wic_state *state, int c)
{ IOSTREAM *fd = state->wicFd;
  word xr;
  int id = 0;				/* make gcc happy! */

  switch( c )
  { case XR_REF:
    { unsigned int xr = getUInt(fd);
      DEBUG(MSG_QLF_XR, Sdprintf("Reuse XR(%d)\n", (long)xr));
      word val = lookupXrId(state, xr);

      return val;
    }
    case XR_NIL:
      return ATOM_nil;
    case XR_CONS:
      return ATOM_dot;
    case XR_ATOM:
    { id = ++state->XR->id;
      xr = getAtom(fd, NULL);
      DEBUG(MSG_QLF_XR, Sdprintf("XR(%d) = '%s'\n", id, stringAtom(xr)));
      break;
    }
    case XR_BLOB:
    { id = ++state->XR->id;
      xr = getBlob(state);
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
    { GET_LD
      atom_t name;
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

      if ( (rc=put_int64(&w, i, ALLOW_GC)) != TRUE )
      { raiseStackOverflow(rc);
	return 0;
      }

      return w;
    }
    case XR_FLOAT:
    { word w;
      double f = getFloat(fd);
      int rc;

      if ( (rc=put_double(&w, f, ALLOW_GC)) != TRUE )
      { raiseStackOverflow(rc);
	return 0;
      }

      return w;
    }
#if O_STRING
    case XR_STRING:
    { char *s;
      size_t len;

      if ( (s = getString(fd, &len)) )
      { return globalString(len, s);
      } else
      { raiseStackOverflow(GLOBAL_OVERFLOW);
	return 0;
      }
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
	  PL_chars_t text;
	  SourceFile sf;

	  PL_STRINGS_MARK();
	  get_atom_text(name, &text);
	  PL_mb_text(&text, REP_UTF8);
	  sf = lookupSourceFile(qlfFixSourcePath(state, text.text.t), TRUE);
	  PL_STRINGS_RELEASE();

	  if ( sf->mtime == 0.0 )
	  { sf->mtime   = time;
	    sf->system = (c == 's' ? TRUE : FALSE);
	  }
	  sf->count++;
	  xr = (word)sf;
	  /* do not release sf; part of state */
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
      fatalError("Illegal XR entry at index %ld: %d", Stell(fd)-1, c);
    }
  }

  storeXrId(state, id, xr);

  return xr;
}


static atom_t
getBlob(DECL_LD wic_state *state)
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

#define do_load_qlf_term(state, vars, term) LDFUNC(do_load_qlf_term, state, vars, term)
static int
do_load_qlf_term(DECL_LD wic_state *state, term_t vars[], term_t term)
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
	if ( !do_load_qlf_term(state, vars, c2) )
	  return FALSE;
      }

      return TRUE;
    }

    return FALSE;
  } else
  { word w;

    if ( (w=loadXRc(state, c)) )
      return _PL_unify_atomic(term, w);

    return FALSE;
  }
}


#define loadQlfTerm(state, term) LDFUNC(loadQlfTerm, state, term)
static int
loadQlfTerm(DECL_LD wic_state *state, term_t term)
{ IOSTREAM *fd = state->wicFd;
  int nvars;
  Word vars;
  int rc;

  DEBUG(MSG_QLF_TERM, Sdprintf("Loading from %ld ...", (long)Stell(fd)));

  if ( (nvars = getInt(fd)) )
  { term_t *v;
    int n;

    vars = alloca(nvars * sizeof(term_t));
    for(n=nvars, v=vars; n>0; n--, v++)
      *v = 0L;
  } else
    vars = NULL;

  PL_put_variable(term);
  rc = do_load_qlf_term(state, vars, term);
  if ( rc )
    resortDictsInTerm(term);
  DEBUG(MSG_QLF_TERM,
	Sdprintf("Loaded ");
	PL_write_term(Serror, term, 1200, 0);
	Sdprintf(" to %ld\n", (long)Stell(fd)));
  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Load intermediate code state from the  specified stream. rcpath contains
the ZIP file name.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
loadWicFromStream(const char *rcpath, IOSTREAM *fd)
{ wic_state state;
  int rval;

  memset(&state, 0, sizeof(state));
  state.wicFd = fd;
  state.wicFile = (char*)rcpath;

  pushXrIdTable(&state);
  rval = loadWicFd(&state);
  popXrIdTable(&state);

  return rval;
}


static int
loadWicFile(const char *file)
{ IOSTREAM *fd;
  int rval;

  if ( !(fd = Sopen_file(file, "rb" TRACK_POS)) )
  { warning("Cannot open Quick Load File %s: %s", file, OsError());
    return FALSE;
  }

  rval = loadWicFromStream(file, fd);
  Sclose(fd);

  return rval;
}


static bool
loadWicFd(wic_state *state)
{ GET_LD
  IOSTREAM *fd = state->wicFd;

  if ( !qlfIsCompatible(state, saveMagic) ||
       !pushPathTranslation(state, systemDefaults.home, PATH_ISDIR) )
    return FALSE;

  for(;;)
  { int c = Qgetc(fd);

    switch( c )
    { case EOF:
      case 'T':				/* trailer */
	popPathTranslation(state);
	succeed;
      case 'W':
	{ char *name = store_string(getString(fd, NULL) );

	  if ( (name=getString(fd, NULL)) )
	  { name = store_string(name);
	    loadWicFile(name);
	    continue;
	  } else
	  { fatalError("Invalid QLF: bad string");
	    return FALSE;
	  }
	}
      case 'X':
        break;
      default:
        { loadStatement(state, c, FALSE);
	  continue;
	}
    }
  }
}


static bool
loadStatement(DECL_LD wic_state *state, int c, int skip)
{ IOSTREAM *fd = state->wicFd;

  switch(c)
  { case 'P':
      return loadPredicate(state, skip);

    case 'O':
    { word mname = loadXR(state);
      Module om = LD->modules.source;
      bool rval;

      LD->modules.source = lookupModule(mname);
      rval = loadPredicate(state, skip);
      LD->modules.source = om;

      return rval;
    }
    case 'I':
      return loadImport(state, skip);

    case 'D':
    { fid_t cid;

      if ( (cid=PL_open_foreign_frame()) )
      { term_t goal = PL_new_term_ref();
	atom_t  osf = source_file_name;
	int     oln = source_line_no;

	source_file_name = (state->currentSource ? state->currentSource->name
						 : NULL_ATOM);
	source_line_no   = getInt(fd);

	if ( !loadQlfTerm(state, goal) )
	  return FALSE;
	DEBUG(MSG_QLF_DIRECTIVE,
	      if ( source_file_name )
	      { Sdprintf("%s:%d: Directive: ",
			  PL_atom_chars(source_file_name), source_line_no);
	      } else
	      { Sdprintf("Directive: ");
	      }
	      PL_write_term(Serror, goal, 1200, PL_WRT_NEWLINE));
	if ( !skip )
	{ if ( !callProlog(MODULE_user, goal, PL_Q_NODEBUG, NULL) )
	  { if ( !printMessage(ATOM_warning,
			       PL_FUNCTOR_CHARS, "goal_failed", 2,
			         PL_CHARS, "directive",
			         PL_TERM, goal) )
	      PL_clear_exception();
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
      rc = loadPart(state, NULL, skip);
      state->load_nesting--;

      return rc;
    }
    case 'M':
      return loadInModule(state, skip);

    default:
      return qlfLoadError(state);
  }
}


static void
loadPredicateFlags(wic_state *state, Definition def, int skip)
{ unsigned int flags = getUInt(state->wicFd);

  if ( !skip )
  { unsigned long lflags = 0L;

    if ( flags & PRED_SYSTEM )
      lflags |= P_LOCKED;
    if ( flags & PRED_HIDE_CHILDS )
      lflags |= HIDE_CHILDS;
    if ( flags & PRED_DET )
      lflags |= P_DET;

    set(def, lflags);
  }
}

#ifdef O_GMP

static int
mp_cpsign(ssize_t hdrsize, int mpsize)
{ return hdrsize >= 0 ? mpsize : -mpsize;
}

static void
mpz_hdr_size(ssize_t hdrsize, mpz_t mpz, size_t *wszp)
{ size_t size     = hdrsize >= 0 ? hdrsize : -hdrsize;
  size_t limpsize = (size+sizeof(mp_limb_t)-1)/sizeof(mp_limb_t);
  size_t wsize    = (limpsize*sizeof(mp_limb_t)+sizeof(word)-1)/sizeof(word);

  mpz->_mp_size  = limpsize;
  mpz->_mp_alloc = limpsize;

  *wszp = wsize;
}


static void
mpz_load_bits(IOSTREAM *fd, Word p, mpz_t mpz, size_t bytes)
{ char fast[1024];
  char *cbuf;
  size_t i;

  if ( bytes < sizeof(fast) )
    cbuf = fast;
  else
    cbuf = PL_malloc(bytes);

  for(i=0; i<bytes; i++)
    cbuf[i] = Qgetc(fd);

  mpz->_mp_d = (mp_limb_t*)p;
  mpz_import(mpz, bytes, 1, 1, 1, 0, cbuf);
  assert((Word)mpz->_mp_d == p);	/* check no (re-)allocation is done */
  if ( cbuf != fast )
    PL_free(cbuf);
}


#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Label handling
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct vm_rlabel
{ size_t        offset;			/* location of jump */
  size_t	soi;			/* start of instruction */
  unsigned int	id;			/* label id */
} vm_rlabel;

typedef struct vm_rlabel_state
{ size_t	soi;			/* offset for start of instruction */
  tmp_buffer	buf;			/* buffer labels */
} vm_rlabel_state;

static void
init_rlabels(vm_rlabel_state *state)
{ initBuffer(&state->buf);
}

static void
exit_rlabels(vm_rlabel_state *state)
{ discardBuffer(&state->buf);
}

static void
push_rlabel(vm_rlabel_state *state, unsigned int id, size_t offset)
{ vm_rlabel *top    = allocFromBuffer(&state->buf, sizeof(*top));
  vm_rlabel *bottom = baseBuffer(&state->buf, vm_rlabel);
  vm_rlabel *prev   = top;

  while(prev > bottom && id > prev[-1].id)
    prev--;
  memmove(prev+1, prev, (char*)top - (char*)prev);
  prev->id     = id;
  prev->soi    = state->soi;
  prev->offset = offset;
}

static void
resolve_rlabel(vm_rlabel_state *state, unsigned int id, Code base, Clause clause)
{ vm_rlabel *top    = topBuffer(&state->buf, vm_rlabel);
  vm_rlabel *bottom = baseBuffer(&state->buf, vm_rlabel);
  size_t copy = 0;

  DEBUG(MSG_QLF_LABEL,
	Sdprintf("%s: V_LABEL %d\n", predicateName(clause->predicate), id));

  for(--top; top >= bottom; top--)
  { if ( top->id < id )
    { copy++;
      continue;
    }

    if ( top->id == id )
    { Code pc = &base[top->soi];
      size_t jmp;

      pc = stepPC(pc);				/* end of instruction */
      assert(base[top->offset] == (code)id);
      jmp = &base[state->soi] - pc;
      base[top->offset] = jmp;
      DEBUG(MSG_QLF_LABEL,
	    Sdprintf("  Put %d at %zd\n", (int)jmp, top->offset));
      continue;
    }

    if ( top->id > id )
    { top++;

      if ( copy	)
      { vm_rlabel *cptop  = topBuffer(&state->buf, vm_rlabel);
	size_t     cpsize = copy*sizeof(*cptop);

	memmove(top, cptop - copy, cpsize);
	state->buf.top = (char*)(top+copy);
      } else
      { state->buf.top = (char*)top;
      }

      break;
    }
  }
}


static void
loadClauseFlags(wic_state *state, Clause cl, int skip)
{ unsigned int flags = getUInt(state->wicFd);

  if ( !skip )
  { unsigned int lflags = 0;

    if ( (flags&CLAUSE_UNIT_CLAUSE) ) lflags |= UNIT_CLAUSE;
    if ( (flags&CLAUSE_SSU_COMMIT) )  lflags |= SSU_COMMIT_CLAUSE;
    if ( (flags&CLAUSE_SSU_CHOICE) )  lflags |= SSU_CHOICE_CLAUSE;
    if ( (flags&CLAUSE_HEAD_TERMS) )  lflags |= CL_HEAD_TERMS;

    cl->flags = lflags;
  }
}


static bool
loadPredicate(DECL_LD wic_state *state, int skip)
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
  { if ( def->impl.any.defined )
    { if ( !redefineProcedure(proc, state->currentSource, DISCONTIGUOUS_STYLE) )
      { int rc = printMessage(ATOM_error, exception_term);
	(void)rc;
	PL_clear_exception();
	skip = TRUE;
      }
    }
    addProcedureSourceFile(state->currentSource, proc);
  }
  loadPredicateFlags(state, def, skip);

  for(;;)
  { switch(Qgetc(fd) )
    { case 'X':
      { DEBUG(MSG_QLF_PREDICATE, Sdprintf("ok\n"));
	succeed;
      }
      case 'C':
      { int has_dicts = 0;
	tmp_buffer buf;
	vm_rlabel_state lstate;

	DEBUG(MSG_QLF_PREDICATE, Sdprintf("."));
	initBuffer(&buf);
	init_rlabels(&lstate);
	clause = (Clause)allocFromBuffer(&buf, sizeofClause(0));
	clause->references   = 0;
	clause->tr_erased_no = 0;
	clause->line_no	     = getUInt(fd);

	{ SourceFile of = (void *) loadXR(state);
	  SourceFile sf = (void *) loadXR(state);
	  unsigned int ono = (of ? of->index : 0);
	  unsigned int sno = (sf ? sf->index : 0);
	  if ( sf )
	  { acquireSourceFile(sf);
	    if ( of != sf )
	      acquireSourceFile(of);
	  }
	  clause->owner_no = ono;
	  clause->source_no = sno;
	  if ( of && of != csf )
	  { addProcedureSourceFile(sf, proc);
	    csf = of;
	  }
	}

	clause->prolog_vars = (unsigned short) getUInt(fd);
	clause->variables   = (unsigned short) getUInt(fd);
	loadClauseFlags(state, clause, skip);
	clause->predicate = def;

#define addCode(c) addBuffer(&buf, (c), code)

	for(;;)
	{ code op = getUInt(fd);
	  const char *ats;
	  int n = 0;

	  lstate.soi = entriesBuffer(&buf, code);
	  switch(op)
	  { case V_LABEL:
	    { unsigned lbl = getUInt(fd);
	      resolve_rlabel(&lstate, lbl, baseBuffer(&buf, code),
			     baseBuffer(&buf, struct clause));
	      continue;
	    }
	    case V_H_INTEGER:
	    case V_B_INTEGER:
	    { int64_t val = getInt64(fd);
	      word w = consInt(val);

	      if ( valInt(w) == val )
	      { addCode(encode(op==V_H_INTEGER ? H_SMALLINT : B_SMALLINT));
		addCode(w);
#if SIZEOF_VOIDP == 8
	      } else
	      { addCode(encode(op==V_H_INTEGER ? H_INTEGER : B_INTEGER));
		addCode((intptr_t)val);
	      }
#else
	      } else if ( val >= INTPTR_MIN && val <= INTPTR_MAX )
	      { addCode(encode(op==V_H_INTEGER ? H_INTEGER : B_INTEGER));
		addCode((intptr_t)val);
	      } else
	      { addCode(encode(op==V_H_INTEGER ? H_INT64 : B_INT64));
		addMultipleBuffer(&buf, (char*)&val, sizeof(int64_t), char);
	      }
#endif

	      continue;
	    }
	    case V_A_INTEGER:
	    { int64_t val = getInt64(fd);

#if SIZEOF_VOIDP == 8
	      addCode(encode(A_INTEGER));
	      addCode((intptr_t)val);
#else
	      if ( val >= INTPTR_MIN && val <= INTPTR_MAX )
	      { addCode(encode(A_INTEGER));
		addCode((intptr_t)val);
	      } else
	      { addCode(encode(A_INT64));
		addMultipleBuffer(&buf, (char*)&val, sizeof(int64_t), char);
	      }
#endif
	      continue;
	    }
	  }

	  if ( op >= I_HIGHEST )
	    fatalError("Illegal op-code (%d) at %ld", op, Stell(fd));

	  ats = codeTable[op].argtype;
	  DEBUG(MSG_QLF_VMI,
		Sdprintf("\t%s from %ld\n", codeTable[op].name, Stell(fd)));
	  if ( op == I_CONTEXT )
	  { clause = baseBuffer(&buf, struct clause);
	    set(clause, CL_BODY_CONTEXT);
	    set(def, P_MFCONTEXT);
	  }
	  addCode(encode(op));
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
	      { addCode(loadXR(state));
		break;
	      }
	      case CA1_FUNC:
	      { word w = loadXR(state);
		FunctorDef fd = valueFunctor(w);
		if ( fd->name == ATOM_dict )
		  has_dicts++;

		addCode(w);
		break;
	      }
	      case CA1_DATA:
	      { word w = loadXR(state);
		if ( isAtom(w) )
		  PL_register_atom(w);
		addCode(w);
		break;
	      }
	      case CA1_AFUNC:
	      { word f = loadXR(state);
		int  i = indexArithFunction(f);
		assert(i>0);
		addCode(i);
		break;
	      }
	      case CA1_MODULE:
		addCode(loadXR(state));
		break;
	      case CA1_JUMP:
	      { unsigned lbl = getUInt(fd);
		size_t off = entriesBuffer(&buf, code);
		addCode(lbl);
		push_rlabel(&lstate, lbl, off);
		break;
	      }
	      case CA1_INTEGER:
		addCode((code)getInt64(fd));
		break;
	      case CA1_VAR:
	      case CA1_FVAR:
	      case CA1_CHP:
		addCode((code)OFFSET_VAR(getInt64(fd)));
		break;
	      case CA1_INT64:
	      { int64_t val = getInt64(fd);

		addMultipleBuffer(&buf, (char*)&val, sizeof(int64_t), char);
		break;
	      }
	      case CA1_FLOAT:
	      { double f = getFloat(fd);

		addMultipleBuffer(&buf, (char*)&f, sizeof(double), char);
		break;
	      }
	      case CA1_STRING:		/* <n> chars */
	      { size_t l = getInt(fd);
		int   c0 = Qgetc(fd);

		if ( c0 == 'B' )
		{ int lw = (l+sizeof(word))/sizeof(word);
		  int pad = (lw*sizeof(word) - l);
		  Code bp;
		  char *s;

		  DEBUG(MSG_QLF_VMI, Sdprintf("String of %ld bytes\n", l));
		  bp = allocFromBuffer(&buf, sizeof(word)*(lw+1));
		  s = (char *)&bp[1];
		  *bp = mkStrHdr(lw, pad);
		  bp += lw;
		  *bp++ = 0L;
		  *s++ = 'B';
		  l--;
		  while(l-- > 0)
		    *s++ = Qgetc(fd);
		} else
		{ size_t i;
		  size_t  bs = (l+1)*sizeof(pl_wchar_t);
		  size_t  lw = (bs+sizeof(word))/sizeof(word);
		  int    pad = (lw*sizeof(word) - bs);
		  word	   m = mkStrHdr(lw, pad);
		  IOENC oenc = fd->encoding;

		  DEBUG(MSG_QLF_VMI,
			Sdprintf("Wide string of %zd chars; lw=%zd; pad=%d\n",
				 l, lw, pad));

		  assert(c0 == 'W');

		  addCode(m);		/* The header */
		  addBuffer(&buf, 'W', char);
		  for(i=1; i<sizeof(pl_wchar_t); i++)
		    addBuffer(&buf, 0, char);

		  fd->encoding = ENC_UTF8;
		  for(i=0; i<l; i++)
		  { int code = Sgetcode(fd);
		    pl_wchar_t c = code;

		    if ( (int)c != code )
		    { state->errors.invalid_wide_chars++;
		      c = UTF8_MALFORMED_REPLACEMENT;
		    }

		    addBuffer(&buf, c, pl_wchar_t);
		  }
		  fd->encoding = oenc;

		  for(i=0; i<pad; i++)
		    addBuffer(&buf, 0, char);
		}
		break;
	      }
	      case CA1_MPZ:
#ifdef O_GMP
#define ABS(x) ((x) >= 0 ? (x) : -(x))
	      DEBUG(MSG_QLF_VMI, Sdprintf("Loading MPZ from %ld\n", Stell(fd)));
	      { ssize_t hdrsize = getInt64(fd);
		size_t wsize;
		mpz_t mpz;
		word m;
		Word p;

		mpz_hdr_size(hdrsize, mpz, &wsize);
		m = mkIndHdr(wsize+1, TAG_INTEGER);
		p = allocFromBuffer(&buf, sizeof(word)*(wsize+2));

		*p++ = m;
		p[wsize] = 0;
		*p++ = mpz_size_stack(mp_cpsign(hdrsize, mpz->_mp_size));
		p[wsize] = 0;
		mpz_load_bits(fd, p, mpz, ABS(hdrsize));

		DEBUG(MSG_QLF_VMI, Sdprintf("Loaded MPZ to %ld\n", Stell(fd)));
		break;
	      }
	      case CA1_MPQ:
	      DEBUG(MSG_QLF_VMI, Sdprintf("Loading MPQ from %ld\n", Stell(fd)));
	      { ssize_t num_hdrsize = getInt64(fd);
		ssize_t den_hdrsize = getInt64(fd);
		size_t wsize, num_wsize, den_wsize;
		mpz_t num;
		mpz_t den;
		word m;
		Word p;

		mpz_hdr_size(num_hdrsize, num, &num_wsize);
		mpz_hdr_size(den_hdrsize, den, &den_wsize);
		wsize = num_wsize + den_wsize;
		m     = mkIndHdr(wsize+2, TAG_INTEGER);
		p     = allocFromBuffer(&buf, sizeof(word)*(wsize+3));

		*p++ = m;
		*p++ = mpq_size_stack(mp_cpsign(num_hdrsize, num->_mp_size));
		*p++ = mpq_size_stack(mp_cpsign(den_hdrsize, den->_mp_size));
		p[num_wsize] = 0;
		mpz_load_bits(fd, p, num, ABS(num_hdrsize));
		p += num_wsize;
		p[den_wsize] = 0;
		mpz_load_bits(fd, p, den, ABS(den_hdrsize));

		DEBUG(MSG_QLF_VMI, Sdprintf("Loaded MPQ to %ld\n", Stell(fd)));
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
	  switch(op)
	  { case I_EXITFACT:
	    case I_EXIT:			/* fact */
	      goto done;
	  }
	}

      done:
	exit_rlabels(&lstate);

	if ( !skip )
	{ size_t csize  = sizeOfBuffer(&buf);
	  size_t ncodes = (csize-sizeofClause(0))/sizeof(code);
	  Clause bcl    = baseBuffer(&buf, struct clause);

	  bcl->code_size = ncodes;
	  clause = (Clause)PL_malloc_atomic(csize);
	  memcpy(clause, bcl, csize);

	  if ( has_dicts )
	  { if ( !resortDictsInClause(clause) )
	    { outOfCore();
	      exit(1);
	    }
	  }
	  if ( csf )
	    csf->current_procedure = proc;

	  GD->statistics.codes += clause->code_size;
	  assertProcedureSource(csf, proc, clause);
	}

        discardBuffer(&buf);
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
    term_t av = PL_new_term_refs(2);
    predicate_t pred;

    pred = _PL_predicate("$run_initialization", 2, "system",
			 &GD->procedures.drun_initialization2);

    PL_put_atom(av+0, sf->name);
    PL_put_nil( av+1);
    rc = PL_call_predicate(MODULE_system, PL_Q_NORMAL, pred, av);

    PL_discard_foreign_frame(fid);
  }

  return rc;
}


static bool
loadImport(DECL_LD wic_state *state, int skip)
{ Procedure proc = (Procedure) loadXR(state);
  int flags = getInt(state->wicFd);

  if ( !skip )
    return importDefinitionModule(LD->modules.source, proc->definition, flags);

  succeed;
}


static atom_t
qlfFixSourcePath(wic_state *state, const char *raw)
{ char buf[PATH_MAX];
  char *canonical;

  if ( state->load_state->has_moved &&
       strprefix(raw, state->load_state->save_dir) )
  { char *s;
    size_t lensave = strlen(state->load_state->save_dir);
    const char *tail = &raw[lensave];

    if ( strlen(state->load_state->load_dir)+1+strlen(tail)+1 > PATH_MAX )
      fatalError("Path name too long: %s", raw);

    strcpy(buf, state->load_state->load_dir);
    s = &buf[strlen(buf)];
    strcpy(s, tail);
  } else
  { if ( strlen(raw)+1 > PATH_MAX )
    { fatalError("Path name too long: %s", raw);
      return NULL_ATOM;
    }
    strcpy(buf, raw);
  }

  if ( (canonical=canonicalisePath(buf)) )
  { atom_t translated = file_name_to_atom(canonical);

    if ( strcmp(raw, canonical) )
    { path_translated *tr = PL_malloc(sizeof(*tr));

      tr->from = file_name_to_atom(raw);
      tr->to   = translated;
      tr->next = state->load_state->translated;
      state->load_state->translated = tr;
    }

    return translated;
  } else
  { fatalError("Path name too long: %s", buf);
    return NULL_ATOM;
  }
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

  if ( !str )
  { fatalError("Invalid QLF: illegal string");
    return FALSE;
  }
  fname = qlfFixSourcePath(state, str);

  DEBUG(MSG_QLF_PATH,
	if ( !streq(stringAtom(fname), str) )
	  Sdprintf("Replaced path %s --> %s\n", str, stringAtom(fname)));

  state->currentSource = lookupSourceFile(fname, TRUE);
  PL_unregister_atom(fname);		/* locked with sourceFile */
  state->currentSource->mtime = time;
  state->currentSource->system = issys;
  if ( GD->bootsession )		/* (**) */
    state->currentSource->count++;
  else
    startConsult(state->currentSource);

  succeed;
}


#define loadModuleProperties(state, m, skip) LDFUNC(loadModuleProperties, state, m, skip)
static bool
loadModuleProperties(DECL_LD wic_state *state, Module m, int skip)
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

	  addNewHTable(LD->modules.source->public, (void *)f, proc);
          if ( state->currentSource )
            exportProcedureSource(state->currentSource, m, proc);
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
loadPart(DECL_LD wic_state *state, Module *module, int skip)
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

      if ( !loadModuleProperties(state, LD->modules.source, skip) )
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
      { if ( !GD->bootsession  )
	{ runInitialization(state->currentSource);
	  if ( state->currentSource )
	    endConsult(state->currentSource);
        }
	LD->modules.source = om;
	state->currentSource  = of;
	debugstatus.styleCheck = stchk;
	setAccessLevel(alevel);

	succeed;
      }
      default:
	loadStatement(state, c, skip);
    }
  }
}


static bool
loadInModule(DECL_LD wic_state *state, int skip)
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
	loadStatement(state, c, skip);
    }
  }
}


#define loadInclude(state) LDFUNC(loadInclude, state)
static bool
loadInclude(DECL_LD wic_state *state)
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

  assert_term(t, NULL, CL_END, owner, &loc, 0);

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

  putInt64(len, fd);
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

  putInt64(len, fd);
  fd->encoding = ENC_UTF8;
  while(s<e)
  { Sputcode(*s, fd);
    s++;
  }
  fd->encoding = oenc;
}


static void
putAtom(wic_state *state, atom_t w)
{ GET_LD
  IOSTREAM *fd = state->wicFd;
  atom_t mapped;
  Atom a;
  static PL_blob_t *text_blob;

  if ( state->idMap &&
       (mapped = (atom_t)lookupHTable(state->idMap, (void*)w)) )
  { assert(isAtom(mapped));
    w = mapped;
  }

  if ( !text_blob )
    text_blob = PL_find_blob_type("text");

  a = atomValue(w);
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


static void
putInt64(int64_t n, IOSTREAM *fd)
{ uint64_t i = zigzag_encode(n);

  do
  { int b = i&0x7f;

    i >>= 7;
    if ( !i )
      b |= 0x80;
    Sputc(b, fd);
  } while ( i );
}


static void
putUInt(unsigned int i, IOSTREAM *fd)
{ do
  { int b = i&0x7f;

    i >>= 7;
    if ( !i )
      b |= 0x80;
    Sputc(b, fd);
  } while ( i );
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
freeXRSymbol(void *name, void *value)
{ word w = (word)name;

  if ( w&0x1 )
  { w &= ~0x1;
    if ( isAtom(w) )
    { PL_unregister_atom(w);
      DEBUG(5, Sdprintf("UNREG: %s\n", stringAtom(w)));
    }
  }
}


static void
initXR(wic_state *state)
{ state->currentPred		   = NULL;
  state->currentSource		   = NULL;
  state->savedXRTable		   = newHTable(256);
  state->savedXRTable->free_symbol = freeXRSymbol;
  state->savedXRTableId		   = 0;
}


static void
destroyXR(wic_state *state)
{ destroyHTable(state->savedXRTable);
  state->savedXRTable = NULL;
  if ( state->idMap )
  { destroyHTable(state->idMap);
    state->idMap = NULL;
  }
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
{ GET_LD
  IOSTREAM *fd = state->wicFd;
  unsigned int id;

  if ( (id = (intptr_t)lookupHTable(state->savedXRTable, xr)) )
  { Sputc(XR_REF, fd);
    putUInt(id, fd);

    succeed;
  } else
  { id = ++state->savedXRTableId;
    addNewHTable(state->savedXRTable, xr, (void *)(intptr_t)id);
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


static int XRNullPointer = 0;

static inline int
savedXRPointer(wic_state *state, void *p)
{ assert(((word)p & 0x1) == 0);

  if ( !p )
  { return savedXR(state, &XRNullPointer);
  }

  return savedXR(state, p);
}


#define saveXR(state, xr) LDFUNC(saveXR, state, xr)
static void
saveXR(DECL_LD wic_state *state, word xr)
{ IOSTREAM *fd = state->wicFd;

  if ( isTaggedInt(xr) )		/* TBD: switch */
  { Sputc(XR_INT, fd);
    putInt64(valInt(xr), fd);
    return;
  } else if ( isBignum(xr) )
  { Sputc(XR_INT, fd);
    putInt64(valBignum(xr), fd);
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

  if ( xr == ATOM_nil )
  { Sputc(XR_NIL, fd);
    return;
  }
  if ( xr == ATOM_dot )
  { Sputc(XR_CONS, fd);
    return;
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


static void
saveXRBlobType(wic_state *state, PL_blob_t *type)
{ IOSTREAM *fd = state->wicFd;

  if ( savedXRPointer(state, type) )
    return;

  Sputc(XR_BLOB_TYPE, fd);
  putString(type->name, STR_NOLEN, fd);
}


#define saveXRModule(state, m) LDFUNC(saveXRModule, state, m)
static void
saveXRModule(DECL_LD wic_state *state, Module m)
{ IOSTREAM *fd = state->wicFd;

  if ( !m )
  { Sputc(XR_NULL, fd);
    return;
  }

  if ( savedXRPointer(state, m) )
    return;

  Sputc(XR_MODULE, fd);
  DEBUG(MSG_QLF_XR,
	Sdprintf("XR(%d) = module %s\n",
		 state->savedXRTableId, stringAtom(m->name)));
  saveXR(state, m->name);
}


#define saveXRFunctor(state, f) LDFUNC(saveXRFunctor, state, f)
static void
saveXRFunctor(DECL_LD wic_state *state, functor_t f)
{ IOSTREAM *fd = state->wicFd;
  FunctorDef fdef;
  functor_t mapped;

  if ( savedXRConstant(state, f) )
    return;

  if ( state->idMap &&
       (mapped = (functor_t)lookupHTable(state->idMap, (void*)f)) )
    f = mapped;

  fdef = valueFunctor(f);

  DEBUG(MSG_QLF_XR,
	Sdprintf("XR(%d) = %s/%d\n",
		 state->savedXRTableId, stringAtom(fdef->name), fdef->arity));
  Sputc(XR_FUNCTOR, fd);
  saveXR(state, fdef->name);
  putInt64(fdef->arity, fd);
}


#define saveXRProc(state, p) LDFUNC(saveXRProc, state, p)
static void
saveXRProc(DECL_LD wic_state *state, Procedure p)
{ IOSTREAM *fd = state->wicFd;

  if ( savedXRPointer(state, p) )
    return;

  DEBUG(MSG_QLF_XR, Sdprintf("XR(%d) = proc %s\n",
			     state->savedXRTableId, procedureName(p)));
  Sputc(XR_PRED, fd);
  saveXRFunctor(state, p->definition->functor->functor);
  saveXRModule(state, p->definition->module);
}


#define saveXRSourceFile(state, f) LDFUNC(saveXRSourceFile, state, f)
static void
saveXRSourceFile(DECL_LD wic_state *state, SourceFile f)
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



#define do_save_qlf_term(state, t) LDFUNC(do_save_qlf_term, state, t)
static void
do_save_qlf_term(DECL_LD wic_state *state, Word t)
{ IOSTREAM *fd = state->wicFd;

  deRef(t);
  if ( isTerm(*t) )
  { functor_t f = functorTerm(*t);

    if ( f == FUNCTOR_dvard1 )
    { int id = (int)valInt(argTerm(*t, 0));

      Sputc('v', fd);
      putInt64(id, fd);
    } else
    { Word q = argTermP(*t, 0);
      int n, arity = arityFunctor(f);

      Sputc('t', fd);
      saveXRFunctor(state, f);
      for(n=0; n < arity; n++, q++)
	do_save_qlf_term(state, q);
    }
  } else
  { assert(isAtomic(*t));
    saveXR(state, *t);
  }
}


#define saveQlfTerm(state, t) LDFUNC(saveQlfTerm, state, t)
static int
saveQlfTerm(DECL_LD wic_state *state, term_t t)
{ IOSTREAM *fd = state->wicFd;
  intptr_t nvars, rc=TRUE;
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

  if ( (nvars = numberVars(t, &options, 0)) != NV_ERROR )
  { putInt64(nvars, fd);
    do_save_qlf_term(state, valTermRef(t));	/* TBD */
    DEBUG(MSG_QLF_TERM, Sdprintf("to %d\n", Stell(fd)));
  } else
  { rc = FALSE;
  }

  PL_discard_foreign_frame(cid);

  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Label handling
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct vm_wlabel
{ Code		address;
  unsigned int	id;
} vm_wlabel;

typedef struct vm_wlabel_state
{ tmp_buffer	buf;
  vm_wlabel	current;
  unsigned int  next_id;
} vm_wlabel_state;

static void
init_wlabels(vm_wlabel_state *state)
{ initBuffer(&state->buf);
  state->current.address = NULL;
  state->next_id = 0;
}

static void
exit_wlabels(vm_wlabel_state *state)
{ assert(entriesBuffer(&state->buf, vm_wlabel) == 0);
  discardBuffer(&state->buf);
}

static vm_wlabel *
push_wlabel(vm_wlabel_state *state, Code to, Clause clause)
{ vm_wlabel *lbl;

  if ( state->current.address )
  { if ( to == state->current.address )
    { lbl = &state->current;
    } else if ( to < state->current.address )
    { addBuffer(&state->buf, state->current, vm_wlabel);
      state->current.address = to;
      state->current.id = ++state->next_id;
      lbl = &state->current;
    } else
    { vm_wlabel *top    = allocFromBuffer(&state->buf, sizeof(*top));
      vm_wlabel *bottom = baseBuffer(&state->buf, vm_wlabel);
      vm_wlabel *prev   = top;

      while(prev > bottom && to > prev[-1].address)
	prev--;
      if ( prev > bottom && prev[-1].address == to )
      { (void)popBuffer(&state->buf, vm_wlabel);
	lbl = &prev[-1];
      } else
      { memmove(prev+1, prev, (char*)top - (char*)prev);
	prev->address = to;
	prev->id = ++state->next_id;
	lbl = prev;
      }
    }
  } else
  { state->current.address = to;
    state->current.id = ++state->next_id;
    lbl = &state->current;
  }

  DEBUG(MSG_QLF_LABEL,
	{ Sdprintf("%s, clause %d: current: %d at %p\n",
		   predicateName(clause->predicate),
		   clauseNo(clause, 0),
		   state->current.id, state->current.address);
	  vm_wlabel *top    = topBuffer(&state->buf, vm_wlabel);
	  vm_wlabel *bottom = baseBuffer(&state->buf, vm_wlabel);
	  for(--top; top >= bottom; top--)
	    Sdprintf("    %d at %p\n", top->id, top->address);
	});

  return lbl;
}

static void
emit_wlabels(vm_wlabel_state *state, Code here, IOSTREAM *fd)
{ while(state->current.address == here)
  { putUInt(V_LABEL, fd);
    putUInt(state->current.id, fd);

    if ( entriesBuffer(&state->buf, vm_wlabel) != 0 )
      state->current = popBuffer(&state->buf, vm_wlabel);
    else
      state->current.address = NULL;
  }
}


#ifdef O_GMP
static void
put_mpz_size(IOSTREAM *fd, mpz_t mpz, size_t *szp)
{ size_t size = (mpz_sizeinbase(mpz, 2)+7)/8;
  ssize_t hdrsize;

  if ( mpz_sgn(mpz) < 0 )
    hdrsize = -(ssize_t)size;
  else
    hdrsize = (ssize_t)size;

  *szp = size;
  putInt64(hdrsize, fd);
}

static void
put_mpz_bits(IOSTREAM *fd, mpz_t mpz, size_t size)
{ size_t i, count;
  char fast[1024];
  char *buf;

  if ( size < sizeof(fast) )
    buf = fast;
  else
    buf = PL_malloc(size);

  mpz_export(buf, &count, 1, 1, 1, 0, mpz);
  assert(count == size);
  for(i=0; i<count; i++)
    Sputc(buf[i]&0xff, fd);
  if ( buf != fast )
    PL_free(buf);
}

#endif


static unsigned int
clauseFlags(const Clause clause)
{ unsigned int flags = 0;

  if ( true(clause, UNIT_CLAUSE) )       flags |= CLAUSE_UNIT_CLAUSE;
  if ( true(clause, SSU_COMMIT_CLAUSE) ) flags |= CLAUSE_SSU_COMMIT;
  if ( true(clause, SSU_CHOICE_CLAUSE) ) flags |= CLAUSE_SSU_CHOICE;
  if ( true(clause, CL_HEAD_TERMS) )     flags |= CLAUSE_HEAD_TERMS;

  return flags;
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
  vm_wlabel_state lstate;

  Sputc('C', fd);
  putUInt(state->obfuscate ? 0 : clause->line_no, fd);
  saveXRSourceFile(state,
		   state->obfuscate ? NULL
				    : indexToSourceFile(clause->owner_no));
  saveXRSourceFile(state,
		   state->obfuscate ? NULL
				    : indexToSourceFile(clause->source_no));
  putUInt(clause->prolog_vars, fd);
  putUInt(clause->variables, fd);
  putUInt(clauseFlags(clause), fd);

  bp = clause->codes;
  ep = bp + clause->code_size;
  init_wlabels(&lstate);

  while( bp < ep )
  { Code si = bp;				/* start instruction */
    unsigned int op = decode(*bp++);
    const char *ats = codeTable[op].argtype;
    int n;

    emit_wlabels(&lstate, si, fd);

    switch(op)
    { { int64_t v;

        case H_SMALLINT:
	  v = valInt(*bp++);
	  goto vh_int;
#if SIZEOF_VOIDP == 4
	case H_INT64:
	{ Word p = (Word)&v;
	  cpInt64Data(p, bp);
	  goto vh_int;
	}
#endif
	case H_INTEGER:
	  v = (intptr_t)*bp++;
	vh_int:
	  putUInt(V_H_INTEGER, fd);
	  putInt64(v, fd);
	  continue;
      }
      { int64_t v;

        case B_SMALLINT:
	  v = valInt(*bp++);
	  goto vb_int;
#if SIZEOF_VOIDP == 4
	case B_INT64:
	{ Word p = (Word)&v;
	  cpInt64Data(p, bp);
	  goto vb_int;
	}
#endif
	case B_INTEGER:
	  v = (intptr_t)*bp++;
	vb_int:
	  putUInt(V_B_INTEGER, fd);
	  putInt64(v, fd);
	  continue;
      }
      { int64_t v;

#if SIZEOF_VOIDP == 4
	case A_INT64:
	{ Word p = (Word)&v;
	  cpInt64Data(p, bp);
	  goto va_int;
	}
#endif
	case A_INTEGER:
	  v = (intptr_t)*bp++;
#if SIZEOF_VOIDP == 4
	va_int:
#endif
	  putUInt(V_A_INTEGER, fd);
	  putInt64(v, fd);
	  continue;
      }
    }

    putUInt(op, fd);

    DEBUG(MSG_QLF_VMI, Sdprintf("\t%s at %ld\n", codeTable[op].name, Stell(fd)));
    for(n=0; ats[n]; n++)
    { switch(ats[n])
      { case CA1_PROC:
	{ Procedure p = (Procedure) *bp++;
	  saveXRProc(state, p);
	  break;
	}
	case CA1_MODULE:
	{ Module m = (Module) *bp++;	/* can be NULL, see I_CALLATMV */
	  saveXRModule(state, m);
	  break;
	}
	case CA1_FUNC:
	{ functor_t f = (functor_t) *bp++;
	  saveXRFunctor(state, f);
	  break;
	}
	case CA1_AFUNC:
	{ functor_t f = functorArithFunction((unsigned int)*bp++);
	  saveXRFunctor(state, f);
	  break;
	}
	case CA1_DATA:
	{ word xr = (word) *bp++;
	  saveXR(state, xr);
	  break;
	}
	case CA1_JUMP:
	{ Code to = stepPC(si) + *bp++;
	  vm_wlabel *lbl = push_wlabel(&lstate, to, clause);
	  putUInt(lbl->id, fd);
	  break;
	}
	case CA1_INTEGER:
	{ putInt64(*bp++, fd);
	  break;
	}
	case CA1_VAR:
	case CA1_FVAR:
	case CA1_CHP:
	{ intptr_t var = *bp++;
	  putInt64(VAR_OFFSET(var), fd);
	  break;
	}
	case CA1_INT64:
	{ int64_t val;
	  Word p = (Word)&val;

	  cpInt64Data(p, bp);
	  putInt64(val, fd);
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

	  if ( *s == 'B' )
	  { putInt64(l, fd);
	    while( l-- > 0 )
	      Sputc(*s++&0xff, fd);
	  } else
	  { pl_wchar_t *w = (pl_wchar_t*)s + 1;
	    IOENC oenc = fd->encoding;

	    assert(*s == 'W');
	    l /= sizeof(pl_wchar_t);
	    l--;

	    putInt64(l, fd);
	    Sputc('W', fd);
	    fd->encoding = ENC_UTF8;
	    for( ; l-- > 0; w++)
	    { Sputcode(*w, fd);
	    }
	    fd->encoding = oenc;
	  }

	  break;
	}
#ifdef O_GMP
	case CA1_MPZ:
	{ mpz_t mpz;
	  size_t size;

	  bp = get_mpz_from_code(bp, mpz);
	  put_mpz_size(fd, mpz, &size);
	  put_mpz_bits(fd, mpz, size);

	  DEBUG(MSG_QLF_VMI, Sdprintf("Saved MPZ to %ld\n", Stell(fd)));
	  break;
	}
	case CA1_MPQ:
	{ mpq_t mpq;
	  size_t num_size;
	  size_t den_size;

	  bp = get_mpq_from_code(bp, mpq);
	  put_mpz_size(fd, mpq_numref(mpq), &num_size);
	  put_mpz_size(fd, mpq_denref(mpq), &den_size);
	  put_mpz_bits(fd, mpq_numref(mpq), num_size);
	  put_mpz_bits(fd, mpq_denref(mpq), den_size);

	  DEBUG(MSG_QLF_VMI, Sdprintf("Saved MPQ to %ld\n", Stell(fd)));
	  break;
	}
#endif
	default:
	  fatalError("No support for VM argtype %d (arg %d of %s)",
		     ats[n], n, codeTable[op].name);
      }
    }
  }

  exit_wlabels(&lstate);
}


		/********************************
		*         COMPILATION           *
		*********************************/

static void
closePredicateWic(wic_state *state)
{ if ( state->currentPred )
  { Sputc('X', state->wicFd);
    state->currentPred = NULL;
  }
}


static unsigned int
predicateFlags(Definition def, atom_t sclass)
{ unsigned int flags = 0;

  if ( sclass == ATOM_kernel )
  { if ( true(def, P_LOCKED) && false(def, HIDE_CHILDS) )
      return PRED_SYSTEM;
    return (PRED_SYSTEM|PRED_HIDE_CHILDS);
  }

  if ( true(def, P_LOCKED) )
    flags |= PRED_SYSTEM;
  if ( true(def, HIDE_CHILDS) )
    flags |= PRED_HIDE_CHILDS;
  if ( true(def, P_DET) )
    flags |= PRED_DET;

  return flags;
}


#define openPredicateWic(state, def, sclass) LDFUNC(openPredicateWic, state, def, sclass)
static void
openPredicateWic(DECL_LD wic_state *state, Definition def, atom_t sclass)
{ if ( def != state->currentPred)
  { IOSTREAM *fd = state->wicFd;
    unsigned int mode = predicateFlags(def, sclass);

    closePredicateWic(state);
    state->currentPred = def;

    if ( def->module != LD->modules.source )
    { Sputc('O', fd);
      saveXR(state, def->module->name);
    } else
    { Sputc('P', fd);
    }

    saveXRFunctor(state, def->functor->functor);
    putUInt(mode, fd);
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
  putInt64(PL_QLF_VERSION, fd);
  putInt64(VM_SIGNATURE, fd);
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

  closePredicateWic(state);
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

#define addClauseWic(state, term, file) LDFUNC(addClauseWic, state, term, file)
static bool
addClauseWic(DECL_LD wic_state *state, term_t term, atom_t file)
{ Clause clause;
  sourceloc loc;

  loc.file = file;
  loc.line = source_line_no;

  if ( (clause = assert_term(term, NULL, CL_END, file, &loc, 0)) )
  { openPredicateWic(state, clause->predicate, ATOM_development);
    saveWicClause(state, clause);

    succeed;
  }

  Sdprintf("Failed to compile: "); pl_write(term); Sdprintf("\n");
  fail;
}

#define addDirectiveWic(state, term) LDFUNC(addDirectiveWic, state, term)
static bool
addDirectiveWic(DECL_LD wic_state *state, term_t term)
{ IOSTREAM *fd = state->wicFd;

  closePredicateWic(state);
  Sputc('D', fd);
  putInt64(source_line_no, fd);

  return saveQlfTerm(state, term);
}


#define importWic(state, proc, strength) LDFUNC(importWic, state, proc, strength)
static bool
importWic(DECL_LD wic_state *state, Procedure proc, atom_t strength)
{ int flags = atomToImportStrength(strength);

  assert(flags >= 0);
  closePredicateWic(state);

  Sputc('I', state->wicFd);
  saveXRProc(state, proc);
  putInt64(flags, state->wicFd);

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

/* Raise an error of the format

	error(qlf_format_error(File, Message), _)
*/

static int
qlfError(wic_state *state, const char *error, ...)
{ va_list args;
  char message[LINESIZ];
  int rc;
  const char *file = state->wicFile;

  if ( !file )
    file = "<unknown>";

  va_start(args, error);
  Svsnprintf(message, sizeof(message), error, args);
  va_end(args);

  if ( GD->bootsession )
  { fatalError("%s: %s", file, message);
    rc = FALSE;				/* keep compiler happy */
    exit(1);
  } else
  { GET_LD
    term_t ex, fn;

    rc = ( (ex=PL_new_term_ref()) &&
	   (fn=PL_new_term_ref()) &&
	   PL_unify_chars(fn, PL_ATOM|REP_FN, (size_t)-1, file) &&
	   PL_unify_term(ex,
			 PL_FUNCTOR, FUNCTOR_error2,
			   PL_FUNCTOR_CHARS, "qlf_format_error", 2,
			     PL_TERM, fn,
			     PL_CHARS, message,
			   PL_VARIABLE) &&
	   PL_raise_exception(ex) );
  }

  return rc;
}


#define qlfSourceInfo(state, offset, list) LDFUNC(qlfSourceInfo, state, offset, list)
static int
qlfSourceInfo(DECL_LD wic_state *state, size_t offset, term_t list)
{ IOSTREAM *s = state->wicFd;
  char *str;
  term_t head = PL_new_term_ref();
  atom_t fname;

  if ( Sseek(s, (long)offset, SIO_SEEK_SET) != 0 )
    return qlfError(state, "seek to %zd failed: %s", offset, OsError());
  if ( Sgetc(s) != 'F' || !(str=getString(s, NULL)) )
    return qlfError(state, "invalid string (offset %zd)", offset);
  fname = qlfFixSourcePath(state, str);

  return PL_unify_list(list, head, list) &&
         PL_unify_atom(head, fname);
}


static int
open_qlf_file(const char *file, IOSTREAM **sp)
{ int sl;

  if ( (sl=file_name_is_iri(file)) )
  { IOSTREAM *s;

    if ( !iri_hook(file, IRI_OPEN, ATOM_read, 0, &s) )
      return FALSE;
    s->encoding = ENC_OCTET;
    clear(s, SIO_TEXT);

    *sp = s;

    return TRUE;
  } else
  { if ( (*sp = Sopen_file(file, "rbr")) )
    { return TRUE;
    } else
    { GET_LD
      term_t f = PL_new_term_ref();

      PL_put_atom_chars(f, file);
      return PL_error(NULL, 0, OsError(), ERR_FILE_OPERATION,
		      ATOM_open, ATOM_source_sink, f);
    }
  }
}



#define qlfInfo(file, cversion, minload, fversion, csig, fsig, files0) \
	LDFUNC(qlfInfo, file, cversion, minload, fversion, csig, fsig, files0)

static word
qlfInfo(DECL_LD const char *file,
	term_t cversion, term_t minload, term_t fversion,
	term_t csig, term_t fsig,
	term_t files0)
{ IOSTREAM *s = NULL;
  int lversion;
  int nqlf, i;
  size_t *qlfstart = NULL;
  word rval = FALSE;
  wic_state state;

  memset(&state, 0, sizeof(state));
  state.wicFile = (char*)file;

  if ( !open_qlf_file(file, &s) )
    return FALSE;
  state.wicFd = s;

  if ( cversion )
  { int vm_signature;

    if ( !PL_unify_integer(cversion, PL_QLF_VERSION) ||
	 !PL_unify_integer(minload, PL_QLF_LOADVERSION) ||
	 !PL_unify_integer(csig, (int)VM_SIGNATURE) )
      goto out;

    if ( !qlfVersion(&state, qlfMagic, &lversion) ||
	 !PL_unify_integer(fversion, lversion) )
      goto out;

    vm_signature = getInt(s);		/* TBD: provide to Prolog layer */

    if ( !PL_unify_integer(fsig, vm_signature) )
      goto out;
  } else
  { if ( !qlfIsCompatible(&state, qlfMagic) )
      goto out;
  }

  if ( !pushPathTranslation(&state, file, 0) )
    goto out;

  if ( files0 )
  { term_t files = PL_copy_term_ref(files0);
    if ( Sseek(s, -4, SIO_SEEK_END) < 0 )	/* 4 bytes of PutInt32() */
    { qlfError(&state, "seek to index failed: %s", OsError());
      goto out;
    }
    if ( (nqlf = getInt32(s)) < 0 )
    { qlfError(&state, "invalid number of files (%d)", nqlf);
      goto out;
    }
    if ( Sseek(s, -4 * (nqlf+1), SIO_SEEK_END) < 0 )
    { qlfError(&state, "seek to files failed: %s", OsError());
      goto out;
    }

    DEBUG(MSG_QLF_SECTION, Sdprintf("Found %d sources at", nqlf));
    if ( !(qlfstart = malloc(sizeof(size_t)*nqlf)) )
    { PL_no_memory();
      goto out;
    }
    for(i=0; i<nqlf; i++)
    { qlfstart[i] = (size_t)getInt32(s);
      DEBUG(MSG_QLF_SECTION, Sdprintf(" %ld", qlfstart[i]));
    }
    DEBUG(MSG_QLF_SECTION, Sdprintf("\n"));

    for(i=0; i<nqlf; i++)
    { if ( !qlfSourceInfo(&state, qlfstart[i], files) )
	goto out;
    }

    rval = PL_unify_nil(files);
  }

out:
  popPathTranslation(&state);
  if ( qlfstart )
    free(qlfstart);
  if ( s )
    Sclose(s);

  return rval;
}


/** '$qlf_info'(+File,
		-CurrentVersion, -MinLOadVersion, -FileVersion,
		-CurrentSignature, -FileSignature,
		-Files)
    '$qlf_info'(+File,
		-CurrentVersion, -MinLOadVersion, -FileVersion,
		-CurrentSignature, -FileSignature)

Provide information about a QLF file.

@arg CurrentVersion is the current save version
@arg FileVersion is the version of the file
@arg CurrentSignature is the current VM signature
@arg FileSignature is the signature of the file
@arg Files is a list of atoms representing the files used to create the QLF
*/

static
PRED_IMPL("$qlf_info", 6, qlf_info, 0)
{ PRED_LD
  char *name;

  if ( !PL_get_file_name(A1, &name, PL_FILE_ABSOLUTE) )
    fail;

  return qlfInfo(name, A2, A3, A4, A5, A6, 0);
}


static
PRED_IMPL("$qlf_info", 7, qlf_info, 0)
{ PRED_LD
  char *name;

  if ( !PL_get_file_name(A1, &name, PL_FILE_ABSOLUTE) )
    fail;

  return qlfInfo(name, A2, A3, A4, A5, A6, A7);
}


static
PRED_IMPL("$qlf_sources", 2, qlf_sources, 0)
{ PRED_LD
  char *name;

  if ( !PL_get_file_name(A1, &name, PL_FILE_ABSOLUTE) )
    fail;

  return qlfInfo(name, 0, 0, 0, 0, 0, A2);
}


		 /*******************************
		 *	NEW MODULE SUPPORT	*
		 *******************************/

static wic_state *
qlfOpen(term_t file)
{ char *name;
  char *absname;
  char tmp[PATH_MAX];
  IOSTREAM *out;
  wic_state *state;

  if ( !PL_get_file_name(file, &name, 0) ||
       !(absname = AbsoluteFile(name, tmp)) )
    return NULL;

  if ( !(out = Sopen_file(name, "wb" TRACK_POS)) )
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
  putInt64(PL_QLF_VERSION, state->wicFd);
  putInt64(VM_SIGNATURE, state->wicFd);

  putString(absname, STR_NOLEN, state->wicFd);

  return state;
}


#define qlfClose(state) LDFUNC(qlfClose, state)
static bool
qlfClose(DECL_LD wic_state *state)
{ int rc;

  closePredicateWic(state);
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
qlfVersion(wic_state *state, const char *exp_magic, int *vp)
{ IOSTREAM *s = state->wicFd;
  char mbuf[100];
  char *magic;

  if ( !(magic = getMagicString(s, mbuf, sizeof(mbuf))) ||
       !streq(magic, exp_magic) )
    return qlfError(state, "Not a %s", exp_magic);

  *vp = getInt(s);

  return TRUE;
}


static int
pushPathTranslation(wic_state *state, const char *absloadname, int flags)
{ IOSTREAM *fd = state->wicFd;
  char *abssavename;
  qlf_state *new = allocHeapOrHalt(sizeof(*new));

  memset(new, 0, sizeof(*new));
  new->previous = state->load_state;
  state->load_state = new;

  if ( !(abssavename = getString(fd, NULL)) )
    return qlfError(state, "bad string");

  if ( absloadname && !streq(absloadname, abssavename) )
  { char load[PATH_MAX];
    char save[PATH_MAX];
    char *l, *s, *le, *se;

    if ( ( strlen(abssavename)+1 > PATH_MAX ||
	   strlen(absloadname)+1 > PATH_MAX
	 ) )
      return PL_representation_error("max_path_length");

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

  return TRUE;
}


static void
popPathTranslation(wic_state *state)
{ if ( state->load_state )
  { qlf_state *old = state->load_state;

    state->load_state = old->previous;

    if ( old->has_moved )
    { path_translated *tr;

      remove_string(old->load_dir);
      remove_string(old->save_dir);

      if ( (tr=old->translated) )
      { GET_LD
        path_translated *n;
	predicate_t pred;
	fid_t fid = PL_open_foreign_frame();
	term_t av = PL_new_term_refs(2);

	pred = _PL_predicate("$translated_source", 2, "system",
			     &GD->procedures.dtranslated_source2);

	for(; tr; tr=n)
	{ n = tr->next;

	  PL_put_atom(av+0, tr->from);
	  PL_put_atom(av+1, tr->to);
	  PL_unregister_atom(tr->from);

	  if ( !PL_call_predicate(NULL, PL_Q_NORMAL, pred, av) )
	    Sdprintf("$translated_source/2 failed~n");

	  PL_free(tr);
	}

	PL_discard_foreign_frame(fid);
      }
    }
    freeHeap(old, sizeof(*old));
  }
}

static int
qlfIsCompatible(wic_state *state, const char *magic)
{ int lversion;
  int vm_signature;

  if ( !qlfVersion(state, magic, &lversion) )
    return FALSE;
  if ( lversion < PL_QLF_LOADVERSION )
    return qlfError(state, "incompatible version (file: %d, Prolog: %d)",
		    lversion, PL_QLF_VERSION);
  state->saved_version = lversion;

  vm_signature = getInt(state->wicFd);
  if ( vm_signature != (int)VM_SIGNATURE )
    return qlfError(state, "incompatible VM-signature (file: 0x%x; Prolog: 0x%x)",
		    (unsigned int)vm_signature, (unsigned int)VM_SIGNATURE);

  return TRUE;
}


#define qlfLoad(state, module) LDFUNC(qlfLoad, state, module)
static bool
qlfLoad(DECL_LD wic_state *state, Module *module)
{ IOSTREAM *fd = state->wicFd;
  bool rval;
  const char *absloadname;
  char tmp[PATH_MAX];
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

  if ( !qlfIsCompatible(state, qlfMagic) )
    return FALSE;

  if ( !pushPathTranslation(state, absloadname, 0) )
    return FALSE;

  pushXrIdTable(state);
  for(;;)
  { int c = Qgetc(fd);

    switch(c)
    { case 'Q':
        break;
      case 'I':
	loadInclude(state);
        continue;
      default:
	qlfLoadError(state);
    }

    break;
  }

  rval = loadPart(state, module, FALSE);
  popXrIdTable(state);
  popPathTranslation(state);

  if ( state->errors.invalid_wide_chars )
    Sdprintf("WARNING: %d wide characters could not be represented as UCS-2\n",
	     state->errors.invalid_wide_chars);

  return rval;
}


static bool
qlfSaveSource(wic_state *state, SourceFile f)
{ GET_LD
  IOSTREAM *fd = state->wicFd;
  PL_chars_t text;

  PL_STRINGS_MARK();
  get_atom_text(f->name, &text);
  PL_mb_text(&text, REP_UTF8);

  sourceMark(state);
  Sputc('F', fd);
  putString(text.text.t, text.length, fd);
  putFloat(f->mtime, fd);
  Sputc(f->system ? 's' : 'u', fd);
  PL_STRINGS_RELEASE();

  state->currentSource = f;

  succeed;
}


#define qlfStartModule(state, m) LDFUNC(qlfStartModule, state, m)
static bool
qlfStartModule(DECL_LD wic_state *state, Module m)
{ IOSTREAM *fd = state->wicFd;
  ListCell c;
  closePredicateWic(state);
  Sputc('Q', fd);
  Sputc('M', fd);
  saveXR(state, m->name);

  if ( m->file )
  { qlfSaveSource(state, m->file);
    putInt64(m->line_no, fd);
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
  for_table(m->public, name, value,
	    { functor_t f = (functor_t)name;

	      DEBUG(MSG_QLF_EXPORT,
		    Sdprintf("Exported %s/%d\n",
			     stringAtom(nameFunctor(f)),
			     arityFunctor(f)));
	      Sputc('E', fd);
	      saveXRFunctor(state, f);
	    })

  Sputc('X', fd);

  succeed;
}


#define qlfStartSubModule(state, m) LDFUNC(qlfStartSubModule, state, m)
static bool
qlfStartSubModule(DECL_LD wic_state *state, Module m)
{ IOSTREAM *fd = state->wicFd;

  closePredicateWic(state);
  Sputc('M', fd);
  saveXR(state, m->name);

  succeed;
}


static bool
qlfStartFile(wic_state *state, SourceFile f)
{ IOSTREAM *fd = state->wicFd;

  closePredicateWic(state);
  Sputc('Q', fd);
  qlfSaveSource(state, f);

  succeed;
}


static bool
qlfEndPart(wic_state *state)
{ IOSTREAM *fd = state->wicFd;

  closePredicateWic(state);
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

    return qlfStartModule(state, m);
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

    return qlfStartSubModule(state, m);
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
    saveXR(state, owner);
    saveXR(state, pn);
    putInt64(line, fd);
    saveXR(state, fn);
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
    return qlfClose(state);

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
  rval = qlfLoad(&state, &m);
  LD->modules.source = oldsrc;
  fd->encoding = saved_enc;

  if ( state.wicFile )
    remove_string(state.wicFile);
  PL_release_stream(fd);

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

static const opt_spec open_wic_options[] =
{ { ATOM_obfuscate,	    OPT_BOOL },
  { NULL_ATOM,		    0 }
};


static
PRED_IMPL("$open_wic", 2, open_wic, 0)
{ GET_LD
  IOSTREAM *fd;
  int obfuscate = FALSE;

  assert(V_LABEL > I_HIGHEST);

  if ( !scan_options(A2, 0, ATOM_state_option, open_wic_options,
		     &obfuscate) )
    fail;

  if ( PL_get_stream_handle(A1, &fd) )
  { wic_state *state = allocHeapOrHalt(sizeof(*state));

    memset(state, 0, sizeof(*state));
    state->obfuscate = obfuscate;
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

static void
freeMapping(void *name, void *value)
{ word id_from = (word)name;
  word id_to   = (word)value;

  if ( isAtom(id_from) ) PL_unregister_atom(id_from);
  if ( isAtom(id_to) )   PL_unregister_atom(id_to);
}

static int
get_id(term_t t, void **id)
{ GET_LD
  atom_t a;
  functor_t f;

  if ( PL_get_atom(t, &a) )
  { *id = (void *)a;
  } else if ( PL_get_functor(t, &f) )
  { if ( f == FUNCTOR_colon2 )
    { Procedure proc;

      if ( get_procedure(t, &proc, 0, GP_FINDHERE|GP_EXISTENCE_ERROR) )
      { *id = (void *)proc->definition;
      } else
      { return FALSE;
      }
    }
    *id = (void *)f;
  } else
  { return PL_type_error("identifier", t);
  }

  return TRUE;
}

/** '$map_id'(+IdFrom, +IdTo) is det.

Add a mapping between an identifier when saving a state.
@arg IdFrom, IdTo are either atoms or compound terms.  In the
latter case the functor is mapped.
*/

static
PRED_IMPL("$map_id", 2, map_id, 0)
{ PRED_LD
  wic_state *state;

  if ( (state=LD->qlf.current_state) )
  { void *id_from, *id_to, *old;

    if ( !get_id(A1, &id_from) ||
	 !get_id(A2, &id_to) )
      return FALSE;

    if ( (isAtom((word)id_from)    && !isAtom((word)id_to)) ||
	 (isFunctor((word)id_from) && !isFunctor((word)id_to)) )
      return PL_permission_error("map", "identifier", A1);

    if ( !state->idMap )
    { state->idMap = newHTable(256);
      state->idMap->free_symbol = freeMapping;
    }

    if ( (old=lookupHTable(state->idMap, id_from)) )
    { if ( old == id_to )
	return TRUE;
      else
	return PL_permission_error("map", "identifier", A1);
    } else
    { addNewHTable(state->idMap, id_from, id_to);
      if ( isAtom((word)id_from) )
      { PL_register_atom((atom_t)id_from);
	PL_register_atom((atom_t)id_to);
      }
      return TRUE;
    }
  } else {
    return PL_permission_error("map", "identifier", A1);
  }
}

static
PRED_IMPL("$unmap_id", 1, unmap_id, 0)
{ PRED_LD
  wic_state *state;

  if ( (state=LD->qlf.current_state) )
  { void *id_from;

    if ( !get_id(A1, &id_from) )
      return FALSE;

    if ( state->idMap )
      deleteHTable(state->idMap, id_from);
  }

  return TRUE;
}


static
PRED_IMPL("$add_directive_wic", 1, add_directive_wic, PL_FA_TRANSPARENT)
{ PRED_LD
  wic_state *state;

  if ( (state=LD->qlf.current_state) )
  { Module m = MODULE_system;
    term_t term = PL_new_term_ref();
    term_t qterm = PL_new_term_ref();

    if ( !PL_strip_module(A1, &m, term) )
      return FALSE;
    if ( !(PL_is_callable(term)) )
      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, A1);

    if ( !PL_unify_term(qterm,
			PL_FUNCTOR, FUNCTOR_colon2,
			  PL_ATOM, m->name,
			  PL_TERM, term) )
      return FALSE;

    return addDirectiveWic(state, qterm);
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

    return importWic(state, lookupProcedure(fd, m), strength);
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

    openPredicateWic(state, clause->predicate, sclass);
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
  size_t arity;
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
  char tmp[PATH_MAX];
  char *path;
  term_t f = PL_new_term_ref();
  SourceFile sf;
  atom_t nf;

  DEBUG(MSG_QLF_BOOT, Sdprintf("Boot compilation of %s\n", file));
  if ( !(path = AbsoluteFile(file, tmp)) )
    fail;
  DEBUG(MSG_QLF_PATH, Sdprintf("Expanded to %s\n", path));

  if ( PL_unify_chars(f, PL_ATOM|REP_MB, (size_t)-1, path) )
    PL_get_atom(f, &nf);
  else
    fatalError("Could not unify path");
  DEBUG(MSG_QLF_BOOT, Sdprintf("Opening\n"));
  if ( !pl_see(f) )
  { Sdprintf("Failed to open %s\n", path);
    return FALSE;
  }
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
    if ( !read_clause(Scurin, t, 0) ) /* syntax error */
    { Sdprintf("%s:%d: Syntax error\n",
	       PL_atom_chars(source_file_name),
	       source_line_no);
      continue;
    }
    if ( PL_get_atom(t, &eof) && eof == ATOM_end_of_file )
      break;

    DEBUG(MSG_QLF_BOOT_READ,
	  Sdprintf(""); /* To output line header */
	  PL_write_term(Serror, t, 1200, PL_WRT_NUMBERVARS);
	  Sdprintf("\n"));

    if ( directiveClause(directive, t, ":-") )
    { DEBUG(MSG_QLF_DIRECTIVE,
	    Sdprintf(":- ");
	    PL_write_term(Serror, directive, 1200, 0);
	    Sdprintf(".\n") );
      addDirectiveWic(state, directive);
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
      addClauseWic(state, t, nf);

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
    { if ( !printMessage(ATOM_warning,
			 PL_FUNCTOR_CHARS, "qlf", 1,
			   PL_FUNCTOR_CHARS, "removed_after_error", 1,
			     PL_CHARS, state->mkWicFile) )
	PL_clear_exception();
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
wicPutStringW(const pl_wchar_t *w, size_t len, IOSTREAM *fd)
{ putStringW(w, len, fd);
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(wic)
  PRED_DEF("$qlf_info",		    6, qlf_info,	     0)
  PRED_DEF("$qlf_info",		    7, qlf_info,	     0)
  PRED_DEF("$qlf_sources",	    2, qlf_sources,	     0)
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
  PRED_DEF("$open_wic",		    2, open_wic,	     0)
  PRED_DEF("$close_wic",	    0, close_wic,	     0)
  PRED_DEF("$map_id",               2, map_id,		     0)
  PRED_DEF("$unmap_id",             1, unmap_id,             0)
  PRED_DEF("$import_wic",	    3, import_wic,	     0)
EndPredDefs
