/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

/*#define O_SECURE 1*/
/*#define O_DEBUG 1*/
#include "pl-incl.h"

forwards RecordList lookupRecordList(word);
forwards RecordList isCurrentRecordList(word);

#define RECORDA 0
#define RECORDZ 1

#define LOCK()   PL_LOCK(L_RECORD)
#define UNLOCK() PL_UNLOCK(L_RECORD)

#undef LD
#define LD LOCAL_LD

void
initRecords(void)
{ GD->tables.record_lists = newHTable(8);
}


/* MT: locked by caller (record())
*/

static RecordList
lookupRecordList(word key)
{ Symbol s;

  if ( (s = lookupHTable(GD->tables.record_lists, (void *)key)) )
  { return s->value;
  } else
  { RecordList l;

    if ( isAtom(key) )			/* can also be functor_t */
      PL_register_atom(key);
    l = allocHeap(sizeof(*l));
    l->key = key;
    l->type = RECORD_TYPE;
    l->references = 0;
    l->flags = 0;
    l->firstRecord = l->lastRecord = NULL;
    addHTable(GD->tables.record_lists, (void *)key, l);

    return l;
  }
}


static RecordList
isCurrentRecordList(word key)
{ Symbol s;

  if ( (s = lookupHTable(GD->tables.record_lists, (void *)key)) )
    return s->value;

  return NULL;
}


/* MT: Locked by called
*/

static void
cleanRecordList(RecordList rl)
{ Record *p;
  Record r;

  for(p = &rl->firstRecord; (r=*p); )
  { if ( true(r, ERASED) )
    { *p = r->next;
      freeRecord(r);
    } else
    { p = &r->next;
    }
  }
}


		 /*******************************
		 *	    HEAP STORAGE	*
		 *******************************/


#undef uint
#undef uchar
#define uint unsigned int
#define uchar unsigned char

#ifndef offsetof
#define offsetof(structure, field) ((int) &(((structure *)NULL)->field))
#endif

#define SIZERECORD(flags) \
	((flags & R_LIST)      ? offsetof(struct record, buffer[0]) : \
	 (flags & R_DUPLICATE) ? offsetof(struct record, list) : \
	                         offsetof(struct record, references)) \

#define dataRecord(r) ((char *)addPointer(r, SIZERECORD(r->flags)))

typedef struct
{ tmp_buffer code;			/* code buffer */
  tmp_buffer vars;			/* variable pointers */
  int	     size;			/* size on global stack */
  uint	     nvars;			/* # variables */
  int	     external;			/* Allow for external storage */
} compile_info, *CompileInfo;


typedef struct
{ word w[WORDS_PER_DOUBLE];
} fword;


#define	PL_TYPE_VARIABLE	(1)	/* variable */
#define PL_TYPE_ATOM		(2)	/* atom */
#define PL_TYPE_INTEGER	  	(3)	/* big integer */
#define PL_TYPE_TAGGED_INTEGER  (4)	/* tagged integer */
#define PL_TYPE_FLOAT	  	(5)	/* double */
#define PL_TYPE_STRING	  	(6)	/* string */
#define PL_TYPE_COMPOUND	(7)	/* compound term */

#define PL_TYPE_EXT_ATOM	(8)	/* External (inlined) atom */
#define PL_TYPE_EXT_COMPOUND	(9)	/* External (inlined) functor */
#define PL_TYPE_EXT_FLOAT	(10)	/* float in standard-byte order */

static void
addOpCode(CompileInfo info, int code)
{ addBuffer(&info->code, code, uchar);
  DEBUG(1, Sdprintf("Added %d, now %d big\n",
		    code, sizeOfBuffer(&info->code)));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
addSizeInt() deals with integers that should  be large enough to specify
the size of an object on the  stack. This counts for variables, arities,
sizes of strings and atoms, etc.

Encoding: 7-bits per byte, MSF. All but the last (LSB) have the 8-th bit
set. This format allows for arbitrary   bit integers and is architecture
independent.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline void
addSizeInt(CompileInfo info, uint val)
{ if ( !(val & ~0x7f) )
    addBuffer(&info->code, val, uchar);
  else
  { int zips = ((sizeof(val))*8+7-1)/7 - 1;
    int leading = TRUE;

    for(; zips >= 0; zips--)
    { uint d = (val >> zips*7) & 0x7f;

      if ( d || !leading )
      { if ( zips != 0 )
	  d |= 0x80;
	addBuffer(&info->code, d, uchar);
	leading = FALSE;
      }
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Add a signed long value. First byte   is  number of bytes, remaining are
value-bytes, starting at most-significant.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
addLong(CompileInfo info, long v)
{ int i = sizeof(v);

  for(i = sizeof(v); i>1; i--)
  { int b = (v>>((i-1)*8)) & 0xff;

    if ( !(b == 0x00 || b == 0xff) )
      break;
  }

  addBuffer(&info->code, i, uchar);
  
  for( ; i>0; i-- )
  { int b = (v>>((i-1)*8)) & 0xff;
    
    addBuffer(&info->code, b, uchar);
  }
}


#ifdef WORDS_BIGENDIAN
static const int double_byte_order[] = { 7,6,5,4,3,2,1,0 };
#else
static const int double_byte_order[] = { 0,1,2,3,4,5,6,7 };
#endif


static inline void
addFloat(CompileInfo info, void *val)
{ if ( info->external )
  { unsigned char *cl = val;
    int i;

    addOpCode(info, PL_TYPE_FLOAT);
    for(i=0; i<sizeof(double); i++)
      addBuffer(&info->code, cl[double_byte_order[i]], uchar);
  } else
  { addOpCode(info, PL_TYPE_EXT_FLOAT);

#ifdef NON_ALIGNED_ACCESS
    { double f = *(double *)val;
      addBuffer(&info->code, f, double);
    }
#else
    addMultipleBuffer(&info->code, val, sizeof(double), char);
#endif
  }
}


static void
addWord(CompileInfo info, word w)
{
#ifdef NON_ALIGNED_ACCESS
  addBuffer(&info->code, w, word);
#else
  addMultipleBuffer(&info->code, (char *)&w, sizeof(w), char);
#endif
}


static inline void
addChars(CompileInfo info, int len, const char *data)
{ addSizeInt(info, len);

  addMultipleBuffer(&info->code, data, len, char);
}


static inline void
addAtomValue(CompileInfo info, atom_t name)
{ Atom a = atomValue(name);
  uint len = a->length;

  addSizeInt(info, a->length);

  addMultipleBuffer(&info->code, a->name, len, char);
}


static void
addAtom(CompileInfo info, atom_t a)
{ if ( info->external )
  { addOpCode(info, PL_TYPE_EXT_ATOM);
    addAtomValue(info, a);
  } else
  { addOpCode(info, PL_TYPE_ATOM);
    addWord(info, a);
    PL_register_atom(a);
  }
}


static void
addFunctor(CompileInfo info, functor_t f)
{ if ( info->external )
  { FunctorDef fd = valueFunctor(f);

    addOpCode(info, PL_TYPE_EXT_COMPOUND);
    addSizeInt(info, fd->arity);
    addAtomValue(info, fd->name);
  } else
  { addOpCode(info, PL_TYPE_COMPOUND);
    addBuffer(&info->code, f, word);
  }
}


static void
compile_term_to_heap(Word p, CompileInfo info)
{ GET_LD
  word w;

right_recursion:
  w = *p;

  switch(tag(w))
  { case TAG_VAR:
    { long n = info->nvars++;

      *p = (n<<7)|TAG_ATOM|STG_GLOBAL;
      addBuffer(&info->vars, p, Word);
      addOpCode(info, PL_TYPE_VARIABLE);
      addSizeInt(info, n);

      return;
    }
    case TAG_ATOM:
    { if ( storage(w) == STG_GLOBAL )	/* this is a variable */
      { long n = ((long)(w) >> 7);

	addOpCode(info, PL_TYPE_VARIABLE);
	addSizeInt(info, n);
      } else
	addAtom(info, w);

      return;
    }
    case TAG_INTEGER:
    { long val;

      if ( isTaggedInt(w) )
      { val = valInt(w);
	addOpCode(info, PL_TYPE_TAGGED_INTEGER);
      } else
      { info->size += sizeof(long)/sizeof(word) + 2;
	val = valBignum(w);
	addOpCode(info, PL_TYPE_INTEGER);
      }
      
      addLong(info, val);
      return;
    }
    case TAG_STRING:
    { Word f  = addressIndirect(w);
      int n   = wsizeofInd(*f);
      int pad = padHdr(*f);		/* see also sizeString() */
      long l  = n*sizeof(word)-pad;

      info->size += n+2;
      addOpCode(info, PL_TYPE_STRING);
      addChars(info, l, (const char *)(f+1)); /* +1 to skip header */
      
      return;
    }
    case TAG_FLOAT:
    { info->size += WORDS_PER_DOUBLE + 2;
      addFloat(info, valIndirectP(w));

      return;
    }
    case TAG_COMPOUND:
    { Functor f = valueTerm(w);
      int arity = arityFunctor(f->definition);

      info->size += arity+1;
      addFunctor(info, f->definition);
      p = f->arguments;
      for(; --arity > 0; p++)
	compile_term_to_heap(p, info);
      goto right_recursion;
    }
    case TAG_REFERENCE:
      p = unRef(w);
      goto right_recursion;
    default:
      assert(0);
  }
}



Record
compileTermToHeap(term_t t, int flags)
{ GET_LD
  compile_info info;
  Record record;
  Word *p;
  int n, size;
  int rsize = SIZERECORD(flags);

  SECURE(checkData(valTermRef(t)));

  initBuffer(&info.code);
  initBuffer(&info.vars);
  info.size = 0;
  info.nvars = 0;
  info.external = (flags & R_EXTERNAL);

  compile_term_to_heap(valTermRef(t), &info);
  n = info.nvars;
  p = (Word *)info.vars.base;
  while(--n >= 0)
    setVar(**p++);
  discardBuffer(&info.vars);
  
  size = rsize + sizeOfBuffer(&info.code);
  record = allocHeap(size);
  record->gsize = info.size;
  record->nvars = info.nvars;
  record->size  = size;
  record->flags = flags;
  if ( flags & R_LIST )
  { record->list  = NULL;		/* ensure initialised memory */
    record->next  = NULL;		/* idem */
  }
  if ( flags & R_DUPLICATE )
  { record->references = 1;
  }
  memcpy(addPointer(record, rsize), info.code.base, sizeOfBuffer(&info.code));
  discardBuffer(&info.code);

  DEBUG(1, Sdprintf("--> record at %p\n", record));

  return record;
}


typedef struct
{ const char *data;
  const char *base;			/* start of data */
  Word *vars;
  uint  nvars;				/* for se_record() */
  Word gstore;
} copy_info, *CopyInfo;

#define fetchBuf(b, var, type) \
		do \
		{ *var = *((type *)(b)->data); \
		  (b)->data += sizeof(type); \
		} while(0)
#define fetchMultipleBuf(b, var, times, type) \
		do \
		{ type *_src = (type *)b->data; \
		  type *_dst = var; \
		  int _len = (times); \
		  (b)->data += _len * sizeof(type); \
		  while(--_len >= 0) \
		    *_dst++ = *_src++; \
		} while(0)
#define skipBuf(b, type) \
		((b)->data += sizeof(type))


static inline int
fetchOpCode(CopyInfo b)
{ uchar tag;

  fetchBuf(b, &tag, uchar);
  DEBUG(1, Sdprintf("fetchOpCode() --> %d, (at %d)\n",
		    tag, b->data-b->base));
  return tag;
}


static uint
fetchSizeInt(CopyInfo b)
{ uint r = 0;
  uint end;

  do
  { uint d = *b->data++;
    
    end = !(d & 0x80);
    r = (r<<7)|(d&0x7f);
  } while(!end);

  return r;
}


static long
fetchLong(CopyInfo b)
{ long val = 0;
  uint bytes = *b->data++;
  uint shift = (sizeof(long)-bytes)*8;

  while(bytes-- > 0)
    val = (val << 8) | (*b->data++ & 0xff);

  val <<= shift;
  val >>= shift;

  return val;
}


static word
fetchWord(CopyInfo b)
{ word val;

#ifdef NON_ALIGNED_ACCESS
  fetchBuf(b, &val, word);
#else
  fetchMultipleBuf(b, &val, sizeof(word), char)
#endif

  return val;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Fetch a float.  Note that the destination might not be double-aligned!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
fetchFloat(CopyInfo b, void *f)
{
#ifdef NON_ALIGNED_ACCESS
  fetchMultipleBuf(b, f, WORDS_PER_DOUBLE, word);
#else
  fetchMultipleBuf(b, f, sizeof(double), char);
#endif
}


static void
fetchExtFloat(CopyInfo b, void *f)
{ unsigned char *dst = f;
  int i;

  for(i=0; i<sizeof(double); i++)
    dst[double_byte_order[i]] = *b->data++;
}


static void
fetchAtom(CopyInfo b, atom_t *a)
{ unsigned int len = fetchSizeInt(b);

  *a = lookupAtom(b->data, len);

  (b)->data += len;
}


static void
fetchChars(CopyInfo b, unsigned len, Word to)
{ fetchMultipleBuf(b, (char *)to, len, char);
}


static void
copy_record(Word p, CopyInfo b)
{ GET_LD
  long tag;

right_recursion:
  switch( (tag = fetchOpCode(b)) )
  { case PL_TYPE_VARIABLE:
    { long n = fetchSizeInt(b);

      if ( b->vars[n] )
      { if ( p > b->vars[n] )		/* ensure the reference is in the */
	  *p = makeRef(b->vars[n]);	/* right direction! */
	else
	{ setVar(*p);			/* wrong way.  make sure b->vars[n] */
	  *b->vars[n] = makeRef(p);	/* stays at the real variable */
	  b->vars[n] = p;
	}
      } else
      {	setVar(*p);
	b->vars[n] = p;
      }
      
      return;
    }
    case PL_TYPE_ATOM:
    { *p = fetchWord(b);

      return;
    }
    case PL_TYPE_EXT_ATOM:
    { fetchAtom(b, p);
      return;
    }
    case PL_TYPE_TAGGED_INTEGER:
    { long val = fetchLong(b);

      *p = consInt(val);

      return;
    }
    case PL_TYPE_INTEGER:
    { *p = consPtr(b->gstore, TAG_INTEGER|STG_GLOBAL);
      *b->gstore++ = mkIndHdr(1, TAG_INTEGER);
      *b->gstore++ = fetchLong(b);
      *b->gstore++ = mkIndHdr(1, TAG_INTEGER);

      return;
    }
    case PL_TYPE_FLOAT:
    case PL_TYPE_EXT_FLOAT:
    { *p = consPtr(b->gstore, TAG_FLOAT|STG_GLOBAL);
      *b->gstore++ = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);
      if ( tag == PL_TYPE_FLOAT )
	fetchFloat(b, b->gstore);
      else
	fetchExtFloat(b, b->gstore);
      b->gstore += WORDS_PER_DOUBLE;
      *b->gstore++ = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);

      return;
    }
    case PL_TYPE_STRING:
    { unsigned len = fetchSizeInt(b);
      int lw, pad;
      word hdr;

      lw = (len+sizeof(word))/sizeof(word); /* see globalNString() */
      pad = (lw*sizeof(word) - len);
      *p = consPtr(b->gstore, TAG_STRING|STG_GLOBAL);
      *b->gstore++ = hdr = mkStrHdr(lw, pad);
      b->gstore[lw-1] = 0L;		/* zero-padding */
      fetchChars(b, len, b->gstore);
      b->gstore += lw;
      *b->gstore++ = hdr;

      return;
    }
  { word fdef;
    long arity;
    case PL_TYPE_COMPOUND:

      fdef = fetchWord(b);
      arity = arityFunctor(fdef);

    compound:
      *p = consPtr(b->gstore, TAG_COMPOUND|STG_GLOBAL);
      *b->gstore++ = fdef;
      p = b->gstore;
      b->gstore += arity;
      for(; --arity > 0; p++)
	copy_record(p, b);
      goto right_recursion;
    case PL_TYPE_EXT_COMPOUND:
    { atom_t name;

      arity = fetchSizeInt(b);
      fetchAtom(b, &name);
      fdef = lookupFunctorDef(name, arity);

      goto compound;
    }
  }
    default:
      assert(0);
  }
}


void
copyRecordToGlobal(term_t copy, Record r ARG_LD)
{ copy_info b;
  Word *p;
  int n;

  DEBUG(1, Sdprintf("PL_recorded(%p)\n", r));

  b.base = b.data = dataRecord(r);
  if ( r->nvars > 0 )
  { if ( !(b.vars = alloca(sizeof(Word) * r->nvars)) )
      fatalError("alloca() failed");
    for(p = b.vars, n=r->nvars; --n >= 0;)
      *p++ = 0;
  }
  b.gstore = allocGlobal(r->gsize);
  
  copy_record(valTermRef(copy), &b);
  assert(b.gstore == gTop);

  SECURE(checkData(valTermRef(copy)));
}


#ifdef O_ATOMGC

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We could consider some  optimisation  here,   notably  as  this stuff in
inderlying findall() and friends.  I  guess  we   can  get  rid  of  the
recursion.   Other   options:   combine     into    copyRecordToGlobal()
(recorded+erase), add a list of atoms as a separate entity.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
skipAtom(CopyInfo b)
{ uint len = fetchSizeInt(b);

  b->data += len;
}


static void
skipSizeInt(CopyInfo b)
{ while( b->data[0] & 0x80 )
    b->data++;
  b->data++;
}


static void
skipLong(CopyInfo b)
{ b->data += b->data[0] + 1;
}


static void
unregisterAtomsRecord(CopyInfo b)
{ int tag;

right_recursion:

  switch( (tag = fetchOpCode(b)) )
  { case PL_TYPE_VARIABLE:
    { skipSizeInt(b);
      return;
    }
    case PL_TYPE_ATOM:
    { atom_t a = fetchWord(b);

      PL_unregister_atom(a);
      return;
    }
    case PL_TYPE_EXT_ATOM:
    { skipAtom(b);
      return;
    }
    case PL_TYPE_TAGGED_INTEGER:
    case PL_TYPE_INTEGER:
    { skipLong(b);
      return;
    }
    case PL_TYPE_FLOAT:
    case PL_TYPE_EXT_FLOAT:
    { skipBuf(b, double);
      return;
    }
    case PL_TYPE_STRING:
    { uint len = fetchSizeInt(b);

      b->data += len;

      return;
    }
    case PL_TYPE_COMPOUND:
    { word fdef = fetchWord(b);
      int arity;

      arity = arityFunctor(fdef);
      while(--arity > 0)
	unregisterAtomsRecord(b);
      goto right_recursion;
    }
    case PL_TYPE_EXT_COMPOUND:
    { long arity = fetchSizeInt(b);

      skipAtom(b);
      while(--arity > 0)
	unregisterAtomsRecord(b);
      goto right_recursion;
    }
    default:
      assert(0);
  }
}

#endif /*O_ATOMGC*/


		 /*******************************
		 *     STRUCTURAL EQUIVALENCE	*
		 *******************************/

static int
se_record(Word p, CopyInfo info ARG_LD)
{ word w;
  int stag;

right_recursion:
  stag = fetchOpCode(info);
unref_cont:
  w = *p;

  switch(tag(w))
  { case TAG_VAR:
      if ( stag == PL_TYPE_VARIABLE )
      { uint i = fetchSizeInt(info);

	if ( i != info->nvars )
	  fail;

	*p = (info->nvars<<7)|TAG_ATOM|STG_GLOBAL;
	info->vars[info->nvars++] = p;
	succeed;
      }
      fail;
    case TAG_ATOM:
      if ( storage(w) == STG_GLOBAL )
      { if ( stag == PL_TYPE_VARIABLE )
	{ uint n = (uint)((unsigned long)(w) >> 7);
	  uint i = fetchSizeInt(info);

	  if ( i == n )
	    succeed;
	}
	fail;
      } else if ( stag == PL_TYPE_ATOM )
      { atom_t val = fetchWord(info);

	if ( val == w )
	  succeed;
      } else if ( stag == PL_TYPE_EXT_ATOM )
      { atom_t val;

	fetchAtom((CopyInfo)info, &val);		/* TBD: Optimise! */
	if ( val == w )
	  succeed;
      }

      fail;
    case TAG_INTEGER:
      if ( isTaggedInt(w) )
      { if ( stag == PL_TYPE_TAGGED_INTEGER )
	{ long val = valInt(w);
	  long v2 = fetchLong(info);

	  if ( v2 == val )
	    succeed;
	}
      } else
      { if ( stag == PL_TYPE_INTEGER )
	{ long val = valBignum(w);
	  long v2 = fetchLong(info);

	  if ( v2 == val )
	    succeed;
	}
      }
      fail;
    case TAG_STRING:
      if ( stag == PL_TYPE_STRING )
      { Word f  = addressIndirect(w);
	int n   = wsizeofInd(*f);
	int pad = padHdr(*f);		/* see also sizeString() */
	uint l  = n*sizeof(word)-pad;

	uint llen = fetchSizeInt(info);
	if ( llen == l &&
	     memcmp((char *)(f+1), info->data, l) == 0 )
	{ info->data += l;

	  succeed;
	}
      }
      fail;
    case TAG_FLOAT:
      if ( stag == PL_TYPE_FLOAT )
      { Word v = valIndirectP(w);
	Word d = (Word)info->data;

	if ( memcmp(v, d, sizeof(double)) == 0 )
	{ info->data += sizeof(double);
	  succeed;
	}
      } else if ( stag == PL_TYPE_EXT_FLOAT )
      { Word v = valIndirectP(w);
	double d;

	fetchExtFloat(info, &d);
	if ( memcmp(v, &d, sizeof(double)) == 0 )
	  succeed;
      }

      fail;
    case TAG_COMPOUND:
      if ( stag == PL_TYPE_COMPOUND )
      { Functor f = valueTerm(w);
	word fdef = fetchWord(info);

	if ( fdef == f->definition )
	{ int arity = arityFunctor(fdef);

	  p = f->arguments;
	  for(; --arity > 0; p++)
	  { if ( !se_record(p, info PASS_LD) )
	      fail;
	  }
	  goto right_recursion;
	}
      } else if ( stag == PL_TYPE_EXT_COMPOUND )
      { Functor f = valueTerm(w);
	FunctorDef fd = valueFunctor(f->definition);
	long arity = fetchSizeInt(info);
	atom_t name;

	if ( arity != fd->arity )
	  fail;
	fetchAtom((CopyInfo)info, &name);	/* TBD: optimise */
	if ( name != fd->name )
	  fail;
	
	p = f->arguments;
	for(; --arity > 0; p++)
	{ if ( !se_record(p, info PASS_LD) )
	    fail;
	}
        goto right_recursion;
      }

      fail;
    case TAG_REFERENCE:
      p = unRef(w);
      goto unref_cont;
    default:
      assert(0);
      fail;
  }
}


int
structuralEqualArg1OfRecord(term_t t, Record r ARG_LD)
{ copy_info info;
  int n, rval;
  Word *p;
  long stag;

  info.base = info.data = dataRecord(r);
  info.nvars = 0;
  if ( r->nvars > 0 )
  { if ( !(info.vars = alloca(sizeof(Word) * r->nvars)) )
      fatalError("alloca() failed");
    for(p = info.vars, n=r->nvars; --n >= 0;)
      *p++ = 0;
  }

					/* skip PL_TYPE_COMPOUND <functor> */
  stag = fetchOpCode(&info);
  if ( stag == PL_TYPE_COMPOUND )
    skipBuf(&info, word);
  else if ( stag == PL_TYPE_EXT_COMPOUND )
  { skipBuf(&info, long);		/* arity */
    skipAtom((CopyInfo)&info);		/* name */
  } else
    assert(0);

  rval = se_record(valTermRef(t), &info PASS_LD);

  for(p = info.vars, n=info.nvars; --n >= 0; p++)
    setVar(**p);

  return rval;
}


bool
freeRecord(Record record)
{ if ( true(record, R_DUPLICATE) && --record->references > 0 )
    succeed;

#ifdef O_ATOMGC
  if ( false(record, R_EXTERNAL) )
  { copy_info ci;

    DEBUG(1, Sdprintf("PL_recorded(%p)\n", record));

    ci.base = ci.data = dataRecord(record);
    unregisterAtomsRecord(&ci);
    assert(ci.data == addPointer(record, record->size));
  }
#endif

  freeHeap(record, record->size);

  succeed;
}

		/********************************
		*       PROLOG CONNECTION       *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The key is stored as an atom, integer  or functor header as found on the
global-stack. A functor is a type with  the   same  mask as an atom, but
using the STG_GLOBAL storage indicator.  So,   the  first line denotes a
real atom.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
unifyKey(term_t key, word val)
{ if ( (isAtom(val) && storage(val) != STG_GLOBAL) ||
       isTaggedInt(val) )
    return _PL_unify_atomic(key, val);

  return PL_unify_functor(key, (functor_t) val);
}


int
getKeyEx(term_t key, word *w)
{ GET_LD
  Word k = valTermRef(key);
  deRef(k);

  if ( isAtom(*k) || isTaggedInt(*k) )
    *w = *k;
  else if ( isTerm(*k) )
    *w = (word)functorTerm(*k);
  else
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_key, key);

  succeed;
}


word
pl_current_key(term_t k, word h)
{ GET_LD
  TableEnum e;
  Symbol s;
  mark m;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      e = newTableEnum(GD->tables.record_lists);
      break;
    case FRG_REDO:
      e = ForeignContextPtr(h);
      break;
    case FRG_CUTTED:
    default:				/* fool gcc */
      e = ForeignContextPtr(h);
      freeTableEnum(e);
      succeed;
  }

  Mark(m);
  while( (s=advanceTableEnum(e)) )
  { RecordList l = s->value;

    if ( l->firstRecord && unifyKey(k, l->key) )
      ForeignRedoPtr(e);

    Undo(m);
  }

  freeTableEnum(e);
  fail;
}


static bool
record(term_t key, term_t term, term_t ref, int az)
{ RecordList l;
  Record copy;
  word k;

  if ( !getKeyEx(key, &k) )
    fail;
  if ( !PL_is_variable(ref) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_variable, ref);

  copy = compileTermToHeap(term, R_LIST);
  PL_unify_pointer(ref, copy);

  LOCK();
  l = lookupRecordList(k);
  copy->list = l;

  if ( !l->firstRecord )
  { copy->next = (Record) NULL;
    l->firstRecord = l->lastRecord = copy;
  } else if ( az == RECORDA )
  { copy->next = l->firstRecord;
    l->firstRecord = copy;
  } else
  { copy->next = (Record) NULL;
    l->lastRecord->next = copy;
    l->lastRecord = copy;
  }
  UNLOCK();

  succeed;
}


word
pl_recorda(term_t key, term_t term, term_t ref)
{ return record(key, term, ref, RECORDA);
}


word
pl_recordz(term_t key, term_t term, term_t ref)
{ return record(key, term, ref, RECORDZ);
}


word
pl_recorded(term_t key, term_t term, term_t ref, word h)
{ RecordList rl;
  Record record;
  word k;
  term_t copy;
  word rval;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      if ( PL_get_pointer(ref, (void **)&record) )
      { LOCK();
	if ( isRecord(record) )
	{ if ( unifyKey(key, record->list->key) )
	  { GET_LD
	    copy = PL_new_term_ref();
	    copyRecordToGlobal(copy, record PASS_LD);
	    rval = PL_unify(term, copy);
	  } else
	    rval = FALSE;
	} else
	  rval = PL_error("recorded", 3, NULL, ERR_TYPE, ATOM_record, record);
	UNLOCK();
	return rval;
      }
      if ( !getKeyEx(key, &k) ||
	   !(rl = isCurrentRecordList(k)) )
	fail;
      LOCK();
      rl->references++;
      record = rl->firstRecord;
      break;
    case FRG_REDO:
    { record = ForeignContextPtr(h);
      rl = record->list;

      DEBUG(0, assert(rl->references > 0));

      LOCK();
      break;
    }
    case FRG_CUTTED:
    { record = ForeignContextPtr(h);
      
      if ( record )
      { rl = record->list;

	LOCK();
	if ( --rl->references == 0 && true(rl, R_DIRTY) )
	  cleanRecordList(rl);
	UNLOCK();
      }
    }
      /* FALLTHROUGH */
    default:
      succeed;
  }

{ GET_LD
  copy = PL_new_term_ref();
  for( ; record; record = record->next )
  { mark m;

    if ( true(record, ERASED) )
      continue;

    Mark(m);
    copyRecordToGlobal(copy, record PASS_LD);
    if ( PL_unify(term, copy) && PL_unify_pointer(ref, record) )
    { if ( !record->next )
      { if ( --rl->references == 0 && true(rl, R_DIRTY) )
	  cleanRecordList(rl);
	UNLOCK();
	succeed;
      } else
      { UNLOCK();
	ForeignRedoPtr(record->next);
      }
    }
    Undo(m);
  }
}

  if ( --rl->references == 0 && true(rl, R_DIRTY) )
    cleanRecordList(rl);

  UNLOCK();
  fail;
}


word
pl_erase(term_t ref)
{ Record record;
  Record prev, r;
  RecordList l;
  word rval;

  if ( !PL_get_pointer(ref, (void **)&record) ||
       !inCore(record))
    return PL_error("erase", 1, NULL, ERR_TYPE, ATOM_db_reference, ref);

  if ( isClause(record) )
  { Clause clause = (Clause) record;
  
    if ( true(clause->procedure->definition, LOCKED) &&
	 false(clause->procedure->definition, DYNAMIC) )
      PL_error("erase", 1, NULL, ERR_PERMISSION,
	       ATOM_clause, ATOM_erase, ref);

    return retractClauseProcedure(clause->procedure, clause);
  }
  
  LOCK();
  if ( isRecord(record) )
  {
#if O_DEBUGGER
    callEventHook(PLEV_ERASED, record);
#endif

    l = record->list;
    if ( l->references )		/* a recorded has choicepoints */
    { set(record, ERASED);
      set(l, R_DIRTY);
    } else if ( record == l->firstRecord )
    { if ( !record->next )
	l->lastRecord = NULL;
      l->firstRecord = record->next;
      freeRecord(record);
    } else
    { prev = l->firstRecord;
      r = prev->next;
      for(; r; prev = r, r = r->next)
      { if (r == record)
	{ if ( r->next == (Record) NULL )
	    l->lastRecord = prev;
	  prev->next = r->next;
	  freeRecord(r);
	  goto ok;
	}
      }
      goto nok;
    }

  ok:
    rval = TRUE;
  } else
  { nok:
    rval = PL_error("erase", 1, NULL, ERR_DOMAIN, ATOM_db_reference, ref);
  }
  UNLOCK();

  return rval;
}

		 /*******************************
		 *	     COMPLEXITY		*
		 *******************************/

static int
count_term(Word t, int left ARG_LD)
{ int count = 0;

right_recursion:
  deRef(t);

  if ( --left < 0 )
    return -1;
  count++;

  if ( isTerm(*t) )
  { int arity = arityTerm(*t);
    int me;

    for(t = argTermP(*t, 0); arity-- > 0; count += me, t++ )
    { if ( arity == 0 )
	goto right_recursion;

      me = count_term(t, left PASS_LD);
      if ( me < 0 )
	return me;
      left -= me;
      if ( left < 0 )
	return -1;
    }
  }

  return count+1;
}


#ifndef INT_MAX
#define INT_MAX	    ((int)(((unsigned int)1<<(sizeof(int)*8-1))-1))
#define INT_MIN     (-(INT_MIN)-1)
#endif

word
pl_term_complexity(term_t t, term_t mx, term_t count)
{ GET_LD
  int c, m;

  if ( !PL_get_integer(mx, &m) )
    m = INT_MAX;

  c = count_term(valTermRef(t), m PASS_LD);
  if ( c < 0 || c > m )
    fail;

  return PL_unify_integer(count, c);
}


		 /*******************************
		 *     CATCH/THROW SUPPORT	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
undo_while_saving_term(Mark m, Word term)

Undo(m), but preserve `term'. Variables in  term, sharing with variables
older then the mark should be remain sharing.

The implementation exploits the  recorded   database  primitives defined
above to perform a term-copy. It merges   the two routines. In addition,
it uses the variable info gathered by the compiler and copier to restore
the variable bindings. 

This could also be used for a foreign implementation of findall.  Wonder
whether that is worth the trouble?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
undo_while_saving_term(mark *m, Word term)
{ GET_LD
  compile_info info;
  copy_info b;
  uint n;
  Word *p;

  initBuffer(&info.code);
  initBuffer(&info.vars);
  info.size = 0;
  info.nvars = 0;
  info.external = FALSE;

  compile_term_to_heap(term, &info);
  n = info.nvars;
  p = (Word *)info.vars.base;
  while(n-- > 0)
    setVar(**p++);

  Undo(*m);
  
  b.data = info.code.base;
  if ( info.nvars > 0 )
  { if ( !(b.vars = alloca(sizeof(Word) * info.nvars)) )
      fatalError("alloca(%d) failed", info.nvars);
    for(p = b.vars, n=info.nvars; n-- > 0;)
      *p++ = 0;
  }
  b.gstore = allocGlobal(info.size);
  copy_record(term, &b);
  assert(b.gstore == gTop);
  discardBuffer(&info.code);

  for(n=0; n<info.nvars; n++)
  { Word v = ((Word *)info.vars.base)[n];
    
    if ( onStack(local, v) || (v > gBase && v < m->globaltop) )
      unify_ptrs(v, ((Word *)b.vars)[n]);
  }

  discardBuffer(&info.vars);
}


