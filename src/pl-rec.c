/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2017, University of Amsterdam
                              VU University Amsterdam
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
#include "pl-incl.h"
#include "pl-dbref.h"
#include "pl-termwalk.c"
#include "pl-dict.h"

#define WORDS_PER_PLINT (sizeof(int64_t)/sizeof(word))

static RecordList lookupRecordList(word);
static RecordList isCurrentRecordList(word, int must_be_non_empty);
static void freeRecordRef(RecordRef r);
static void unallocRecordList(RecordList rl);
static int  is_external(const char *rec, size_t len);

#define RECORDA 0
#define RECORDZ 1

#undef LD
#define LD LOCAL_LD

static void
free_recordlist_symbol(void *name, void *value)
{ RecordList l = value;

  unallocRecordList(l);
}


void
initRecords(void)
{ GD->recorded_db.record_lists = newHTable(8);
  GD->recorded_db.record_lists->free_symbol = free_recordlist_symbol;
}


void
cleanupRecords(void)
{ Table t;

  if ( (t=GD->recorded_db.record_lists) )
  { GD->recorded_db.record_lists = NULL;
    destroyHTable(t);
  }
}


/* MT: locked by caller (record())
*/

static RecordList
lookupRecordList(word key)
{ GET_LD
  RecordList l;

  if ( (l = lookupHTable(GD->recorded_db.record_lists, (void *)key)) )
  { return l;
  } else
  { if ( isAtom(key) )			/* can also be functor_t */
      PL_register_atom(key);
    l = allocHeapOrHalt(sizeof(*l));
    memset(l, 0, sizeof(*l));
    l->key = key;
    addNewHTable(GD->recorded_db.record_lists, (void *)key, l);

    return l;
  }
}


static RecordRef
firstRecordRecordList(RecordList rl)
{ RecordRef record;

  for(record = rl->firstRecord; record; record = record->next)
  { if ( false(record->record, R_ERASED) )
      return record;
  }

  return NULL;
}


static RecordList
isCurrentRecordList(word key, int must_be_non_empty)
{ GET_LD
  RecordList rl;

  if ( (rl = lookupHTable(GD->recorded_db.record_lists, (void *)key)) )
  { if ( must_be_non_empty )
    { RecordRef record;

      PL_LOCK(L_RECORD);
      record = firstRecordRecordList(rl);
      PL_UNLOCK(L_RECORD);

      return record ? rl : NULL;
    } else
    { return rl;
    }
  }

  return NULL;
}


static void
remove_record(RecordRef r)
{ RecordList l = r->list;

  if ( r->prev )
    r->prev->next = r->next;
  else
    l->firstRecord = r->next;

  if ( r->next )
    r->next->prev = r->prev;
  else
    l->lastRecord = r->prev;

  freeRecordRef(r);
}

/* MT: Locked by called
*/

static void
cleanRecordList(RecordList rl)
{ RecordRef r, next=NULL;

  for(r = rl->firstRecord; r; r = next )
  { next = r->next;

    if ( true(r->record, R_ERASED) )
      remove_record(r);
  }
}


/* unallocRecordList() is used when memory is cleaned for PL_cleanup().
   We set R_NOLOCK to avoid needless update of the atom references in
   freeRecord().
*/

static void
unallocRecordList(RecordList rl)
{ RecordRef r, n;

  for(r = rl->firstRecord; r; r=n)
  { n = r->next;

    set(r->record, R_NOLOCK);
    freeRecordRef(r);
  }

  freeHeap(rl, sizeof(*rl));
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
	((flags & R_DUPLICATE) ? offsetof(struct record, buffer[0]) : \
	                         offsetof(struct record, references)) \

#define dataRecord(r) ((char *)addPointer(r, SIZERECORD(r->flags)))

typedef enum
{ ENONE = 0,
  EFAST_SERIALIZE
} cerror;

typedef struct
{ tmp_buffer code;			/* code buffer */
  tmp_buffer vars;			/* variable pointers */
  size_t     size;			/* size on global stack */
  uint	     nvars;			/* # variables */
  int	     external;			/* Allow for external storage */
  int	     lock;			/* lock compiled atoms */
  cerror     error;			/* generated error */
  word	     econtext[1];		/* error context */
} compile_info, *CompileInfo;

#define	PL_TYPE_VARIABLE	(1)	/* variable */
#define PL_TYPE_ATOM		(2)	/* atom */
#define PL_TYPE_INTEGER		(3)	/* big integer */
#define PL_TYPE_TAGGED_INTEGER  (4)	/* tagged integer */
#define PL_TYPE_FLOAT		(5)	/* double */
#define PL_TYPE_STRING		(6)	/* string */
#define PL_TYPE_COMPOUND	(7)	/* compound term */
#define PL_TYPE_CONS		(8)	/* list-cell */
#define PL_TYPE_NIL		(9)	/* [] */
#define PL_TYPE_DICT		(10)	/* The C'dict' atom */

#define PL_TYPE_EXT_ATOM	(11)	/* External (inlined) atom */
#define PL_TYPE_EXT_WATOM	(12)	/* External (inlined) wide atom */
#define PL_TYPE_EXT_COMPOUND	(13)	/* External (inlined) functor */
#define PL_TYPE_EXT_FLOAT	(14)	/* float in standard-byte order */
#define PL_TYPE_ATTVAR		(15)	/* Attributed variable */
#define PL_REC_ALLOCVAR		(16)	/* Allocate a variable on global */
#define PL_REC_CYCLE		(17)	/* cyclic reference */
#define PL_REC_MPZ		(18)	/* GMP integer */

#define PL_TYPE_EXT_COMPOUND_V2	(19)	/* Read V2 external records */

static const int v2_map[] =
{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,		/* variable..string */
  11, 12, PL_TYPE_EXT_COMPOUND_V2, 14, 15, 16, 17, 18
};

static const int *v_maps[8] =		/* 3 bits, cannot overflow */
{ NULL,
  NULL,
  v2_map
};


static inline void
addUnalignedBuf(TmpBuffer b, void *ptr, size_t bytes)
{ if ( b->top + bytes > b->max )
  { if ( !growBuffer((Buffer)b, bytes) )
      outOfCore();
  }
  memcpy(b->top, ptr, bytes);
  b->top += bytes;
}

static inline void
addOpCode(CompileInfo info, int code)
{ addBuffer(&info->code, code, uchar);
  DEBUG(9, Sdprintf("Added %d, now %d big\n",
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
addUintBuffer(Buffer b, size_t val)
{ if ( !(val & ~0x7f) )
  { addBuffer(b, (uchar)val, uchar);
  } else
  { int zips = ((sizeof(val))*8+7-1)/7 - 1;
    int leading = TRUE;

    for(; zips >= 0; zips--)
    { uint d = (uint)((val >> zips*7) & 0x7f);

      if ( d || !leading )
      { if ( zips != 0 )
	  d |= 0x80;
	addBuffer(b, d, uchar);
	leading = FALSE;
      }
    }
  }
}


static inline void
addSizeInt(CompileInfo info, size_t val)
{ addUintBuffer((Buffer)&info->code, val);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Add a signed intptr_t value. First byte   is  number of bytes, remaining
are value-bytes, starting at most-significant.  When loading, we restore
the bytes in the least significant  positions   and  perform  a left and
right shift to restore the sign. This  means that a positive number must
always have a 0 at the left side in   the  encoding. So, if bit 7 is the
MSB, we must store 2 bytes.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
addInt64(CompileInfo info, int64_t v)
{ int i;

  if ( v == 0 )
  { i = 1;
  } else if ( v == PLMININT )
  { i = sizeof(v);
  } else
  { int64_t a = v > 0 ? v :- v;

    i = (MSB64(a)+9)/8;
  }

  addBuffer(&info->code, i, uchar);

  while( --i >= 0 )
  { int b = (int)(v>>(i*8)) & 0xff;

    addBuffer(&info->code, b, uchar);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Floats. If we are adding floats for external  use they will be stored in
normalised byte-order. Otherwise they are stored verbatim.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef WORDS_BIGENDIAN
static const int double_byte_order[] = { 7,6,5,4,3,2,1,0 };
#else
static const int double_byte_order[] = { 0,1,2,3,4,5,6,7 };
#endif


static inline void
addFloat(CompileInfo info, void *val)
{ if ( info->external )
  { unsigned char *cl = val;
    unsigned int i;

    addOpCode(info, PL_TYPE_EXT_FLOAT);
    for(i=0; i<sizeof(double); i++)
      addBuffer(&info->code, cl[double_byte_order[i]], uchar);
  } else
  { addOpCode(info, PL_TYPE_FLOAT);

    addUnalignedBuf(&info->code, val, sizeof(double));
  }
}


static inline void
addWord(CompileInfo info, word w)
{ addUnalignedBuf(&info->code, &w, sizeof(word));
}


static inline void
addChars(CompileInfo info, size_t len, const char *data)
{ addSizeInt(info, len);

  addMultipleBuffer(&info->code, data, len, char);
}


static inline void
addAtomValue(CompileInfo info, Atom a)
{ addSizeInt(info, a->length);
  addMultipleBuffer(&info->code, a->name, a->length, char);
}


static int
addAtom(CompileInfo info, atom_t a)
{ if ( a == ATOM_nil )
  { addOpCode(info, PL_TYPE_NIL);
  } else if ( a == ATOM_dict )
  { addOpCode(info, PL_TYPE_DICT);
  } else if ( unlikely(info->external) )
  { Atom ap = atomValue(a);

    if ( true(ap->type, PL_BLOB_TEXT) )
    { if ( isUCSAtom(ap) )
	addOpCode(info, PL_TYPE_EXT_WATOM);
      else
	addOpCode(info, PL_TYPE_EXT_ATOM);

      addAtomValue(info, ap);
    } else
    { info->error = EFAST_SERIALIZE;
      info->econtext[0] = a;
      return FALSE;
    }
  } else
  { addOpCode(info, PL_TYPE_ATOM);
    addWord(info, a);
    if ( info->lock )
      PL_register_atom(a);
  }

  return TRUE;
}


static int
addFunctor(CompileInfo info, functor_t f)
{ if ( f == FUNCTOR_dot2 )
  { addOpCode(info, PL_TYPE_CONS);
  } else
  { if ( info->external )
    { FunctorDef fd = valueFunctor(f);

      addOpCode(info, PL_TYPE_EXT_COMPOUND);
      addSizeInt(info, fd->arity);
      return addAtom(info, fd->name);
    } else
    { addOpCode(info, PL_TYPE_COMPOUND);
      addWord(info, f);
    }
  }

  return TRUE;
}


typedef struct
{ Functor	term;
  functor_t	fdef;
} cycle_mark;


#define mkAttVarP(p)  ((Word)((word)(p) | 0x1L))
#define isAttVarP(p)  ((word)(p) & 0x1)
#define valAttVarP(p) ((Word)((word)(p) & ~0x1L))

static int
compile_term_to_heap(term_agenda *agenda, CompileInfo info ARG_LD)
{ Word p;

  while( (p=nextTermAgenda(agenda)) )
  { word w;

  again:
    w = *p;

    switch(tag(w))
    { case TAG_VAR:
      { intptr_t n = info->nvars++;

	*p = (n<<7)|TAG_ATOM|STG_GLOBAL;
	addBuffer(&info->vars, p, Word);
	addOpCode(info, PL_TYPE_VARIABLE);
	addSizeInt(info, n);

	continue;
      }
#if O_ATTVAR
      case TAG_ATTVAR:
      { intptr_t n = info->nvars++;
	Word ap = valPAttVar(w);

	if ( isEmptyBuffer(&info->code) )
	{ addOpCode(info, PL_REC_ALLOCVAR);	/* only an attributed var */
	  info->size++;
	}

	addBuffer(&info->vars, *p, word);		/* save value */
	*p = (n<<7)|TAG_ATOM|STG_GLOBAL;
	addBuffer(&info->vars, mkAttVarP(p), Word);
	addOpCode(info, PL_TYPE_ATTVAR);
	addSizeInt(info, n);
	info->size += 3;
	DEBUG(MSG_REC_ATTVAR, Sdprintf("Added attvar %d\n", n));

	p = ap;
	deRef(p);
	goto again;
      }
#endif
      case TAG_ATOM:
      { if ( storage(w) == STG_GLOBAL )	/* this is a variable */
	{ intptr_t n = ((intptr_t)(w) >> 7);

	  addOpCode(info, PL_TYPE_VARIABLE);
	  addSizeInt(info, n);
	  DEBUG(9, Sdprintf("Added var-link %d\n", n));
	} else
	{ if ( !addAtom(info, w) )
	    return FALSE;
	  DEBUG(9, Sdprintf("Added '%s'\n", stringAtom(w)));
	}

	continue;
      }
      case TAG_INTEGER:
      { int64_t val;

	if ( isTaggedInt(w) )
	{ val = valInt(w);
	  addOpCode(info, PL_TYPE_TAGGED_INTEGER);
	  addInt64(info, val);
	} else
	{ number n;

	  info->size += wsizeofIndirect(w) + 2;

	  get_integer(w, &n);
	  switch(n.type)
	  { case V_INTEGER:
	      addOpCode(info, PL_TYPE_INTEGER);
	      addInt64(info, n.value.i);
	      break;
#ifdef O_GMP
	    case V_MPZ:
	      addOpCode(info, PL_REC_MPZ);
	      addMPZToBuffer((Buffer)&info->code, n.value.mpz);
	      break;
#endif
	    default:
	      assert(0);
	  }
	}

	continue;
      }
      case TAG_STRING:
      { Word f     = addressIndirect(w);
	size_t n   = wsizeofInd(*f);
	size_t pad = padHdr(*f);		/* see also getCharsString() */
	size_t l   = n*sizeof(word)-pad;

	info->size += n+2;
	addOpCode(info, PL_TYPE_STRING);
	addChars(info, l, (const char *)(f+1)); /* +1 to skip header */

	continue;
      }
      case TAG_FLOAT:
      { info->size += WORDS_PER_DOUBLE + 2;
	addFloat(info, valIndirectP(w));

	continue;
      }
      case TAG_COMPOUND:
      { Functor f = valueTerm(w);
	int arity;
	word functor;

#if O_CYCLIC
	if ( isInteger(f->definition) )
	{ addOpCode(info, PL_REC_CYCLE);
	  addSizeInt(info, valInt(f->definition));

	  DEBUG(1, Sdprintf("Added cycle for offset = %d\n",
			    valInt(f->definition)));

	  continue;
	} else
	{ cycle_mark mark;

	  arity   = arityFunctor(f->definition);
	  functor = f->definition;

	  mark.term = f;
	  mark.fdef = f->definition;
	  pushSegStack(&LD->cycle.lstack, mark, cycle_mark);
	  f->definition = (functor_t)consUInt(info->size);
				  /* overflow test (should not be possible) */
	  DEBUG(CHK_SECURE, assert(valUInt(f->definition) == (uintptr_t)info->size));
	}
#endif

	info->size += arity+1;
	if ( !addFunctor(info, functor) )
	  return FALSE;
	DEBUG(9, if ( GD->io_initialised )
		   Sdprintf("Added %s/%d\n",
			    stringAtom(valueFunctor(functor)->name),
			    arityFunctor(functor)));
	pushWorkAgenda(agenda, arity, f->arguments);
	continue;
      }
      default:
	assert(0);
    }
  }

  return TRUE;
}


#if O_CYCLIC

static void
init_cycle(ARG1_LD)
{ LD->cycle.lstack.unit_size = sizeof(cycle_mark);
}


static void
unvisit(ARG1_LD)
{ cycle_mark mark;

  while( popSegStack(&LD->cycle.lstack, &mark, cycle_mark) )
  { mark.term->definition = mark.fdef;
  }
}

#else

static void init_cycle(ARG1_LD) {}
static void unvisit(ARG1_LD) {}

#endif

static void
restoreVars(compile_info *info)
{ Word *p = topBuffer(&info->vars, Word);
  Word *b = baseBuffer(&info->vars, Word);

  while(p > b)
  { p--;
    if (isAttVarP(*p) )
    { *valAttVarP(*p) = (word)p[-1];
      p--;
    } else
      setVar(**p);
  }
  discardBuffer(&info->vars);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
compileTermToHeap__LD() is the core of the recorded database.

Returns NULL if there is insufficient   memory.  Otherwise the result of
the  allocation  function.   The   default    allocation   function   is
PL_malloc_atomic_unmanaged().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Record
compileTermToHeap__LD(term_t t,
		      void* (*allocate)(void *closure, size_t size),
		      void* closure,
		      int flags ARG_LD)
{ compile_info info;
  Record record;
  size_t size;
  size_t rsize = SIZERECORD(flags);
  term_agenda agenda;

  DEBUG(CHK_SECURE, checkData(valTermRef(t)));

  init_cycle(PASS_LD1);
  initBuffer(&info.code);
  initBuffer(&info.vars);
  info.size = 0;
  info.nvars = 0;
  info.external = (flags & R_EXTERNAL);
  info.lock = !(info.external || (flags&R_NOLOCK));

  initTermAgenda(&agenda, 1, valTermRef(t));
  compile_term_to_heap(&agenda, &info PASS_LD);
  clearTermAgenda(&agenda);
  restoreVars(&info);
  unvisit(PASS_LD1);

  size = rsize + sizeOfBuffer(&info.code);
  if ( allocate )
    record = (*allocate)(closure, size);
  else
    record = PL_malloc_atomic_unmanaged(size);

  if ( record )
  {
#ifdef REC_MAGIC
    record->magic = REC_MAGIC;
#endif
    record->gsize = (unsigned int)info.size; /* only 28-bit */
    record->nvars = info.nvars;
    record->size  = (int)size;
    record->flags = flags;
    if ( flags & R_DUPLICATE )
    { record->references = 1;
    }
    memcpy(addPointer(record, rsize), info.code.base, sizeOfBuffer(&info.code));
  }
  discardBuffer(&info.code);

  DEBUG(3, Sdprintf("--> record at %p\n", record));

  return record;
}


int
variantRecords(const Record r1, const Record r2)
{ return ( r1->size == r2->size &&
	   memcpy(r1, r2, r1->size) == 0
	 );
}


		 /*******************************
		 *	 EXTERNAL RECORDS	*
		 *******************************/

#define	REC_32	    0x01		/* word is 32-bits	*/
#define	REC_64	    0x02		/* word is 64-bits	*/
#define	REC_INT	    0x04		/* Record just contains	int  */
#define	REC_ATOM    0x08		/* Record just contains	atom */
#define	REC_GROUND  0x10		/* Record is ground */
#define	REC_VMASK   0xe0		/* Version mask */
#define REC_VSHIFT     5		/* shift for version mask */
#define	REC_VERSION 0x03		/* Version id */

#define REC_SZMASK  (REC_32|REC_64)	/* SIZE_MASK */

#if SIZEOF_VOIDP == 8
#define REC_SZ REC_64
#else
#define REC_SZ REC_32
#endif

#define REC_HDR		(REC_SZ|(REC_VERSION<<REC_VSHIFT))
#define REC_COMPAT(m)	(((m)&(REC_VMASK|REC_SZMASK)) == REC_HDR)

typedef struct record_data
{ int simple;				/* no header */
  compile_info info;
  tmp_buffer hdr;
} record_data;


static void
discard_record_data(record_data *data)
{ discardBuffer(&data->info.code);
  if ( !data->simple )
    discardBuffer(&data->hdr);
}

static int
rec_error(CompileInfo info)
{ switch(info->error)
  { case EFAST_SERIALIZE:
    { GET_LD
      term_t t;

      return ( (t=PL_new_term_ref()) &&
		PL_put_atom(t, info->econtext[0]) &&
		PL_permission_error("fast_serialize", "blob", t) );
    }
    default:
      assert(0);
  }
}


static int
compile_external_record(term_t t, record_data *data ARG_LD)
{ Word p;
  int first = REC_HDR;
  term_agenda agenda;
  int scode, rc;

  DEBUG(CHK_SECURE, checkData(valTermRef(t)));
  p = valTermRef(t);
  deRef(p);

  init_cycle(PASS_LD1);
  initBuffer(&data->info.code);
  data->info.external = TRUE;
  data->info.lock = FALSE;

  if ( isInteger(*p) )			/* integer-only record */
  { int64_t v;

    if ( isTaggedInt(*p) )
      v = valInt(*p);
    else if ( isBignum(*p) )
      v = valBignum(*p);
    else
      goto general;

    first |= (REC_INT|REC_GROUND);
    addOpCode(&data->info, first);
    addInt64(&data->info, v);
    data->simple = TRUE;

    return TRUE;
  } else if ( isAtom(*p) )		/* atom-only record */
  { first |= (REC_ATOM|REC_GROUND);
    addOpCode(&data->info, first);
    if ( !addAtom(&data->info, *p) )
      return FALSE;
    data->simple = TRUE;

    return TRUE;
  }

					/* the real stuff */
general:
  data->simple = FALSE;
  initBuffer(&data->info.vars);
  data->info.size = 0;
  data->info.nvars = 0;

  initTermAgenda(&agenda, 1, p);
  rc = compile_term_to_heap(&agenda, &data->info PASS_LD);
  clearTermAgenda(&agenda);
  if ( data->info.nvars == 0 )
    first |= REC_GROUND;
  restoreVars(&data->info);
  unvisit(PASS_LD1);
  if ( !rc )
    return rec_error(&data->info);
  scode = (int)sizeOfBuffer(&data->info.code);

  initBuffer(&data->hdr);
  addBuffer(&data->hdr, first, uchar);			/* magic code */
  addUintBuffer((Buffer)&data->hdr, scode);		/* code size */
  addUintBuffer((Buffer)&data->hdr, data->info.size);	/* size on stack */
  if ( data->info.nvars > 0 )
    addUintBuffer((Buffer)&data->hdr, data->info.nvars);/* Number of variables */
  return TRUE;
}


char *
PL_record_external(term_t t, size_t *len)
{ GET_LD
  record_data data;

  if ( compile_external_record(t, &data PASS_LD) )
  { if ( data.simple )
    { int scode = (int)sizeOfBuffer(&data.info.code);
      char *rec = malloc(scode);

      if ( rec )
      { memcpy(rec, data.info.code.base, scode);
	discard_record_data(&data);
	*len = scode;

	return rec;
      } else
      { discard_record_data(&data);
	PL_no_memory();

	return NULL;
      }
    } else
    { int shdr  = (int)sizeOfBuffer(&data.hdr);
      int scode = (int)sizeOfBuffer(&data.info.code);
      char *rec = malloc(shdr + scode);

      if ( rec )
      { memcpy(rec, data.hdr.base, shdr);
	memcpy(rec+shdr, data.info.code.base, scode);
	discard_record_data(&data);
	*len = shdr + scode;

	return rec;
      } else
      { discard_record_data(&data);
	PL_no_memory();

	return NULL;
      }
    }
  } else
  { return NULL;
  }
}


		 /*******************************
		 *	      FASTRW		*
		 *******************************/

static
PRED_IMPL("fast_term_serialized", 2, fast_term_serialized, 0)
{ PRED_LD
  char *rec;
  size_t len;

  term_t term   = A1;
  term_t string = A2;

  if ( PL_is_variable(string) )
  { record_data data;

    if ( compile_external_record(term, &data PASS_LD) )
    { if ( data.simple )
      { int rc;

	len = sizeOfBuffer(&data.info.code);
	rc  = PL_unify_string_nchars(string, len, data.info.code.base);
	discard_record_data(&data);

	return rc;
      } else
      { size_t shdr  = sizeOfBuffer(&data.hdr);
	size_t scode = sizeOfBuffer(&data.info.code);
	Word p;

	if ( (p=allocString(shdr+scode+1 PASS_LD)) )
	{ char *q = (char *)&p[1];
	  word w  = consPtr(p, TAG_STRING|STG_GLOBAL);

	  *q++ = 'B';
	  memcpy(q,      data.hdr.base,       shdr);
	  memcpy(q+shdr, data.info.code.base, scode);

	  return _PL_unify_atomic(string, w);
	} else
	{ discard_record_data(&data);
	  return FALSE;
	}
      }
    } else
    { return FALSE;
    }
  } else if ( PL_get_nchars(string, &len, &rec,
			    CVT_STRING|BUF_RING|REP_ISO_LATIN_1|CVT_EXCEPTION) )
  { term_t tmp;

    return ( (tmp = PL_new_term_ref()) &&
	     is_external(rec, len) &&
	     PL_recorded_external(rec, tmp) &&
	     PL_unify(term, tmp) );
  } else
  { return FALSE;
  }
}

/** fast_write(+Stream, +Term)
*/

static
PRED_IMPL("fast_write", 2, fast_write, 0)
{ PRED_LD
  IOSTREAM *out;

  if ( PL_get_stream(A1, &out, SIO_OUTPUT) )
  { record_data data;
    int rc;

    if ( out->encoding == ENC_OCTET )
    { if ( (rc=compile_external_record(A2, &data PASS_LD)) )
      { if ( data.simple )
	{ size_t len = sizeOfBuffer(&data.info.code);

	  rc = (Sfwrite(data.info.code.base, 1, len, out) == len);
	} else
	{ size_t shdr  = sizeOfBuffer(&data.hdr);
	  size_t scode = sizeOfBuffer(&data.info.code);

	  rc = ( Sfwrite(data.hdr.base,       1,  shdr, out) == shdr &&
		 Sfwrite(data.info.code.base, 1, scode, out) == scode
	       );
	}

	discard_record_data(&data);
      }
    } else
    { rc = PL_permission_error("fast_write", "stream", A1);
    }

    return PL_release_stream(out) && rc;
  }

  return FALSE;
}


#define FASTRW_FAST 512

static char *
readSizeInt(IOSTREAM *in, char *to, size_t *sz)
{ size_t r = 0;
  int d;
  char *t = to;

  do
  { d = Sgetc(in);

    if ( d == -1 )
    { PL_syntax_error("fastrw_size", in);
      return NULL;
    }

    *t++ = d;
    if ( t-to > 10 )
      return NULL;
    r = (r<<7)|(d&0x7f);
  } while((d & 0x80));

  *sz = r;

  return t;
}

static char *
realloc_record(char *rec, char **np, size_t size)
{ size_t hdr   = *np-rec;
  size_t tsize = hdr + size;
  char *nrec;

  if ( tsize <= FASTRW_FAST )
  { return rec;
  } else if ( (nrec = malloc(tsize)) )
  { memcpy(nrec, rec, hdr);
    *np = nrec+hdr;

    return nrec;
  } else
  { PL_no_memory();
    return NULL;
  }
}


static
PRED_IMPL("fast_read", 2, fast_read, 0)
{ PRED_LD
  IOSTREAM *in;

  if ( PL_get_stream(A1, &in, SIO_INPUT) )
  { int rc;

    if ( in->encoding == ENC_OCTET )
    { int m = Sgetc(in);
      char fast[FASTRW_FAST];
      char *rec = fast;

      switch(m)
      { case -1:
	  rc = PL_unify_atom(A2, ATOM_end_of_file);
	  goto out;
        case REC_HDR|REC_INT|REC_GROUND:
	{ int size = Sgetc(in)&0xff;

	  if ( size <= 8 )
	  { rec[0] = m;
	    rec[1] = size;
	    if ( Sfread(&rec[2], 1, size, in) != size )
	      rc = PL_syntax_error("fastrw_integer", in);
	    else
	      rc = TRUE;
	  } else
	  { rc = PL_syntax_error("fastrw_integer", in);
	  }
	  break;
	}
	case REC_HDR|REC_ATOM|REC_GROUND:
	{ uchar op = Sgetc(in);

	  switch(op)
	  { case PL_TYPE_NIL:
	      rc = PL_unify_nil(A2);
	      goto out;
	    case PL_TYPE_DICT:
	      rc = PL_unify_atom(A2, ATOM_dict);
	      goto out;
	    case PL_TYPE_EXT_WATOM:
	    case PL_TYPE_EXT_ATOM:
	    { size_t bytes;
	      char *np;

	      rec[0] = m;
	      rec[1] = op;

	      if ( (np=readSizeInt(in, &rec[2], &bytes)) &&
		   (rec = realloc_record(rec, &np, bytes)) &&
		   Sfread(np, 1, bytes, in) == bytes )
		rc = TRUE;
	      else
		rc = PL_syntax_error("fastrw_atom", in);
	      break;
	    }
	    default:
	      rc = PL_syntax_error("fastrw_atom_type", in);
	  }
	  break;
	}
	case REC_HDR|REC_GROUND:
	case REC_HDR:
	{ char *np;
	  size_t codes, gsize, nvars;

	  rec[0] = m;

	  if ( (np=readSizeInt(in, &rec[1], &codes)) &&
	       (np=readSizeInt(in, np, &gsize)) &&
	       ((m&REC_GROUND) || (np=readSizeInt(in, np, &nvars))) &&
	       (rec = realloc_record(rec, &np, codes)) &&
	       Sfread(np, 1, codes, in) == codes )
	    rc = TRUE;
	  else
	    rc = PL_syntax_error("fastrw_term", in);
	  break;
	}
	default:
	  rc = PL_syntax_error("fastrw_magic_expected", in);
      }

      if ( rc )
      { term_t tmp;

	rc = ( (tmp = PL_new_term_ref()) &&
	       PL_recorded_external(rec, tmp) &&
	       PL_unify(A2, tmp) );
      }

      if ( rec != fast )
	free(rec);
    } else
    { rc = PL_permission_error("fast_read", "stream", A1);
    }

  out:
    return PL_release_stream(in) && rc;
  }

  return FALSE;
}


		 /*******************************
		 *	   HEAP --> STACK	*
		 *******************************/

#define MAX_FAST_VARS 100

typedef struct
{ const char   *data;
  const char   *base;			/* start of data */
  const int    *version_map;		/* translate op-codes */
  Word	       *vars;
  Word		gbase;			/* base of term on global stack */
  Word		gstore;			/* current storage location */
  uint		nvars;			/* Variables seen */
  uint		dicts;			/* # dicts found */
  TmpBuffer	avars;			/* Values stored for attvars */
  Word	        vars_buf[MAX_FAST_VARS];
} copy_info, *CopyInfo;

static void skipSizeInt(CopyInfo b);

static inline int
init_copy_vars(copy_info *info, uint n)
{ if ( n > 0 )
  { Word *p;

    if ( n <= MAX_FAST_VARS )
      info->vars = info->vars_buf;
    else if ( (info->vars = malloc(sizeof(Word)*n)) == NULL )
      return MEMORY_OVERFLOW;

    for(p = info->vars; n-- > 0;)
      *p++ = NULL;
  } else
  { info->vars = NULL;
  }

  return TRUE;
}

static inline void
free_copy_vars(const copy_info *info)
{ if ( info->vars != info->vars_buf )
    free(info->vars);
}


#define fetchBuf(b, var, type) \
		do \
		{ memcpy(var, (b)->data, sizeof(type)); \
		  (b)->data += sizeof(type); \
		} while(0)
#define fetchMultipleBuf(b, var, times, type) \
		do \
		{ memcpy(var, (b)->data, times*sizeof(type)); \
		  (b)->data +=  times*sizeof(type); \
		} while(0)
#define skipBuf(b, type) \
		((b)->data += sizeof(type))


static inline int
fetchOpCode(CopyInfo b)
{ uchar tag;

  fetchBuf(b, &tag, uchar);
  DEBUG(9, Sdprintf("fetchOpCode() --> %d, (at %d)\n",
		    tag, b->data-b->base));
  return likely(b->version_map==NULL) ? tag : b->version_map[tag];
}


static size_t
fetchSizeInt(CopyInfo b)
{ size_t r = 0;
  size_t d;

  do
  { d = *b->data++;

    r = (r<<7)|(d&0x7f);
  } while((d & 0x80));

  return r;
}


static int64_t
fetchInt64(CopyInfo b)
{ int64_t val = 0;
  uint bytes = *b->data++;
  uint shift = (sizeof(int64_t)-bytes)*8;

  while(bytes-- > 0)
    val = (val << 8) | (*b->data++ & 0xff);

  val <<= shift;
  val >>= shift;

  return val;
}


static word
fetchWord(CopyInfo b)
{ word val;

  fetchBuf(b, &val, word);

  return val;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Fetch a float.  Note that the destination might not be double-aligned!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
fetchFloat(CopyInfo b, void *f)
{ fetchBuf(b, f, double);
}


static void
fetchExtFloat(CopyInfo b, void *f)
{ unsigned char *dst = f;
  unsigned int i;

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
fetchAtomW(CopyInfo b, atom_t *a)
{ unsigned int len = fetchSizeInt(b);

  *a = lookupUCSAtom((const pl_wchar_t*)b->data, len/sizeof(pl_wchar_t));

  (b)->data += len;
}


static void
fetchChars(CopyInfo b, unsigned len, Word to)
{ fetchMultipleBuf(b, (char *)to, len, char);
}


static int
copy_record(Word p, CopyInfo b ARG_LD)
{ term_agenda agenda;
  int is_compound = FALSE;
  int tag;

  do
  {
  right_recursion:
    switch( (tag = fetchOpCode(b)) )
    { case PL_TYPE_VARIABLE:
      { intptr_t n = fetchSizeInt(b);

	if ( b->vars[n] )
	{ if ( p > b->vars[n] )		/* ensure the reference is in the */
	    *p = makeRef(b->vars[n]);	/* right direction! */
	  else
	  { *p = *b->vars[n];		/* wrong way.  make sure b->vars[n] */
	    *b->vars[n] = makeRef(p);	/* stays at the real variable */
	    b->vars[n] = p;		/* NOTE: also links attvars! */
	  }
	} else
	{ setVar(*p);
	  b->vars[n] = p;
	}

	continue;
      }
      case PL_REC_ALLOCVAR:
      { setVar(*b->gstore);
	*p = makeRefG(b->gstore);
	p = b->gstore++;
	goto right_recursion;
      }
#if O_ATTVAR
      case PL_TYPE_ATTVAR:
      { intptr_t n = fetchSizeInt(b);

	DEBUG(MSG_REC_ATTVAR,
	      Sdprintf("Restore attvar %ld at %p\n", (long)n, &b->gstore[1]));
	register_attvar(b->gstore PASS_LD);
	b->gstore[1] = consPtr(&b->gstore[2], TAG_ATTVAR|STG_GLOBAL);
	*p = makeRefG(&b->gstore[1]);
	b->vars[n] = p;
	p = &b->gstore[2];
	b->gstore += 3;
	goto right_recursion;
      }
#endif
      case PL_TYPE_NIL:
      { *p = ATOM_nil;
	continue;
      }
      case PL_TYPE_DICT:
      { *p = ATOM_dict;
	continue;
      }
      case PL_TYPE_ATOM:
      { *p = fetchWord(b);
	continue;
      }
      case PL_TYPE_EXT_ATOM:
      { fetchAtom(b, p);
	PL_unregister_atom(*p);
	continue;
      }
      case PL_TYPE_EXT_WATOM:
      { fetchAtomW(b, p);
	PL_unregister_atom(*p);
	continue;
      }
      case PL_TYPE_TAGGED_INTEGER:
      { int64_t val = fetchInt64(b);
	*p = consInt(val);
	continue;
      }
      case PL_TYPE_INTEGER:
      { size_t i;
	union
	{ int64_t i64;
	  word    w[WORDS_PER_PLINT];
	} val;

	val.i64 = fetchInt64(b);

	*p = consPtr(b->gstore, TAG_INTEGER|STG_GLOBAL);
	*b->gstore++ = mkIndHdr(WORDS_PER_PLINT, TAG_INTEGER);
	for(i=0; i<WORDS_PER_PLINT; i++)
	  *b->gstore++ = val.w[i];
	*b->gstore++ = mkIndHdr(WORDS_PER_PLINT, TAG_INTEGER);
	continue;
      }
#ifdef O_GMP
      case PL_REC_MPZ:
      { b->data = loadMPZFromCharp(b->data, p, &b->gstore);
	continue;
      }
#endif
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
	continue;
      }
      case PL_TYPE_STRING:
      { size_t lw, len = fetchSizeInt(b);
	int pad;
	word hdr;

	lw = (len+sizeof(word))/sizeof(word); /* see globalNString() */
	pad = (lw*sizeof(word) - len);
	*p = consPtr(b->gstore, TAG_STRING|STG_GLOBAL);
	*b->gstore++ = hdr = mkStrHdr(lw, pad);
	b->gstore[lw-1] = 0L;		/* zero-padding */
	fetchChars(b, len, b->gstore);
	b->gstore += lw;
	*b->gstore++ = hdr;
	continue;
      }
#ifdef O_CYCLIC
      case PL_REC_CYCLE:
      { unsigned offset = fetchSizeInt(b);
	Word ct = b->gbase+offset;

	*p = consPtr(ct, TAG_COMPOUND|STG_GLOBAL);
	continue;
      }
#endif
    { word fdef;
      int arity;
      case PL_TYPE_COMPOUND:

	fdef = fetchWord(b);
	arity = arityFunctor(fdef);

      compound:
	*p = consPtr(b->gstore, TAG_COMPOUND|STG_GLOBAL);
	*b->gstore++ = fdef;
	p = b->gstore;
	b->gstore += arity;
	if ( !is_compound )
	{ is_compound = TRUE;
	  initTermAgenda(&agenda, arity, p);
	} else
	{ if ( !pushWorkAgenda(&agenda, arity, p) )
	    return MEMORY_OVERFLOW;
	}
	continue;
      case PL_TYPE_EXT_COMPOUND:
      { atom_t name;
	int opcode_atom;

	arity = (int)fetchSizeInt(b);
	opcode_atom = fetchOpCode(b);
	switch(opcode_atom)
	{ case PL_TYPE_EXT_ATOM:
	    fetchAtom(b, &name);
	    break;
	  case PL_TYPE_EXT_WATOM:
	    fetchAtomW(b, &name);
	    break;
	  case PL_TYPE_NIL:
	    name = ATOM_nil;
	    break;
	  case PL_TYPE_DICT:
	    b->dicts++;
	    name = ATOM_dict;
	    break;
	  default:
	    assert(0);
	}

	fdef = lookupFunctorDef(name, arity);
	goto compound;
      }
      case PL_TYPE_EXT_COMPOUND_V2:
      { atom_t name;

	arity = (int)fetchSizeInt(b);
	fetchAtom(b, &name);
	fdef = lookupFunctorDef(name, arity);
	goto compound;
      }
    }
      case PL_TYPE_CONS:
      { *p = consPtr(b->gstore, TAG_COMPOUND|STG_GLOBAL);
	*b->gstore++ = FUNCTOR_dot2;
	p = b->gstore;
	b->gstore += 2;
	if ( !is_compound )
	{ is_compound = TRUE;
	  initTermAgenda(&agenda, 2, p);
	} else
	{ if ( !pushWorkAgenda(&agenda, 2, p) )
	    return MEMORY_OVERFLOW;
	}
	continue;
      }
      default:
	assert(0);
    }
  } while ( is_compound && (p=nextTermAgendaNoDeRef(&agenda)) );

  return TRUE;
}


int
copyRecordToGlobal(term_t copy, Record r, int flags ARG_LD)
{ copy_info b;
  int rc;

  DEBUG(3, Sdprintf("PL_recorded(%p)\n", r));

#ifdef REC_MAGIC
  assert(r->magic == REC_MAGIC);
#endif
  if ( !hasGlobalSpace(r->gsize) )
  { if ( (rc=ensureGlobalSpace(r->gsize, flags)) != TRUE )
      return rc;
  }
  b.base = b.data = dataRecord(r);
  b.gbase = b.gstore = gTop;
  b.version_map = NULL;

  if ( (rc=init_copy_vars(&b, r->nvars)) == TRUE )
  { gTop += r->gsize;
    rc = copy_record(valTermRef(copy), &b PASS_LD);
    free_copy_vars(&b);
  }
  if ( rc != TRUE )
    return rc;

  assert(b.gstore == gTop);
  DEBUG(CHK_SECURE, checkData(valTermRef(copy)));

  return TRUE;
}


static int
is_external(const char *rec, size_t len)
{ if ( len >= 2 )
  { copy_info info;
    uchar m;

    info.data = info.base = rec;
    fetchBuf(&info, &m, uchar);

    switch(m)
    { case REC_HDR|REC_INT|REC_GROUND:
      { uint bytes = *info.data++;
	return len == bytes+2;
      }
      case REC_HDR|REC_ATOM|REC_GROUND:
      { uchar code;

	fetchBuf(&info, &code, uchar);
	switch(code)
	{ case PL_TYPE_NIL:
	  case PL_TYPE_DICT:
	    return len == 2;
	  case PL_TYPE_EXT_WATOM:
	  case PL_TYPE_EXT_ATOM:
	  { size_t bytes = fetchSizeInt(&info);
	    return len == (info.data-info.base)+bytes;
	  }
	}
      }
      case REC_HDR|REC_GROUND:
      case REC_HDR:
      { size_t codes = fetchSizeInt(&info);
	skipSizeInt(&info);		/* global stack size */
	if ( !(m & REC_GROUND) )
	  skipSizeInt(&info);		/* # variables */
	return len == (info.data-info.base)+codes;
      }
      default:
	assert(0);
    }
  }

  return FALSE;
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
scanAtomsRecord(CopyInfo b, void (*func)(atom_t a))
{ size_t work = 0;

  do
  { switch( fetchOpCode(b) )
    { case PL_TYPE_VARIABLE:
      case PL_REC_CYCLE:
      { skipSizeInt(b);
	continue;
      }
      case PL_REC_ALLOCVAR:
	work++;
	continue;
#ifdef O_ATTVAR
      case PL_TYPE_ATTVAR:
      { skipSizeInt(b);
	work++;
	continue;
      }
#endif
      case PL_TYPE_NIL:
      { (*func)(ATOM_nil);
	continue;
      }
      case PL_TYPE_DICT:
      { (*func)(ATOM_dict);
	continue;
      }
      case PL_TYPE_ATOM:
      { atom_t a = fetchWord(b);

	(*func)(a);
	continue;
      }
      case PL_TYPE_EXT_ATOM:
      case PL_TYPE_EXT_WATOM:
      { skipAtom(b);
	continue;
      }
      case PL_TYPE_TAGGED_INTEGER:
      case PL_TYPE_INTEGER:
      { skipLong(b);
	continue;
      }
#ifdef O_GMP
      case PL_REC_MPZ:
	b->data = skipMPZOnCharp(b->data);
	continue;
#endif
      case PL_TYPE_FLOAT:
      case PL_TYPE_EXT_FLOAT:
      { skipBuf(b, double);
	continue;
      }
      case PL_TYPE_STRING:
      { uint len = fetchSizeInt(b);
	b->data += len;
	continue;
      }
      case PL_TYPE_COMPOUND:
      { word fdef = fetchWord(b);
	int arity;

	arity = arityFunctor(fdef);
	work += arity;
	continue;
      }
      case PL_TYPE_EXT_COMPOUND:
      { intptr_t arity = fetchSizeInt(b);

	skipAtom(b);
	work += arity;
	continue;
      }
      case PL_TYPE_CONS:
      { work += 2;
	continue;
      }
      default:
	assert(0);
    }
  } while ( work-- );
}

#endif /*O_ATOMGC*/


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
markAtomsRecord(Record record ARG_LD) must be called on all records that
use the R_NOLOCK option.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
markAtomsRecord(Record record)
{
#ifdef O_ATOMGC
  copy_info ci;

#ifdef REC_MAGIC
  assert(record->magic == REC_MAGIC);
#endif

  ci.base = ci.data = dataRecord(record);
  ci.version_map = NULL;
  scanAtomsRecord(&ci, markAtom);
  assert(ci.data == addPointer(record, record->size));
#endif
}


bool
freeRecord(Record record)
{ if ( true(record, R_DUPLICATE) && --record->references > 0 )
    succeed;

#ifdef O_ATOMGC
  if ( false(record, (R_EXTERNAL|R_NOLOCK)) )
  { copy_info ci;

    DEBUG(3, Sdprintf("freeRecord(%p)\n", record));

    ci.base = ci.data = dataRecord(record);
    ci.version_map = NULL;
    scanAtomsRecord(&ci, PL_unregister_atom);
    assert(ci.data == addPointer(record, record->size));
  }
#endif

  PL_free(record);

  succeed;
}


void
unallocRecordRef(RecordRef r)
{ freeHeap(r, sizeof(*r));
}


static void
freeRecordRef(RecordRef r)
{ int reclaim_now = false(r->record, R_DBREF);

  freeRecord(r->record);
  if ( reclaim_now )
    freeHeap(r, sizeof(*r));
  else
    r->record = NULL;
}


		 /*******************************
		 *	 EXTERNAL RECORDS	*
		 *******************************/

int
PL_recorded_external(const char *rec, term_t t)
{ GET_LD
  copy_info b;
  uint gsize;
  uchar m;
  int rc;

  b.base = b.data = rec;
  b.version_map = NULL;
  fetchBuf(&b, &m, uchar);

  if ( !REC_COMPAT(m) )
  { if ( (m&REC_SZMASK) != REC_SZ )
    { Sdprintf("PL_recorded_external(): "
	       "Incompatible word-length (%d)\n",
	       (m&REC_32) ? 32 : 64);
      fail;
    } else
    { int save_version = (m&REC_VMASK)>>REC_VSHIFT;

      b.version_map = v_maps[save_version];
      if ( !b.version_map )
      { Sdprintf("PL_recorded_external(): "
		 "Incompatible version (%d, current %d)\n",
		 save_version, REC_VERSION);
	fail;
      }
    }
  }

  if ( m & (REC_INT|REC_ATOM) )		/* primitive cases */
  { if ( m & REC_INT )
    { int64_t v = fetchInt64(&b);

      rc = PL_put_int64(t, v);
    } else
    { atom_t a;
      int code = fetchOpCode(&b);

      switch(code)
      { case PL_TYPE_NIL:
	  return PL_put_nil(t);
        case PL_TYPE_DICT:
	  return PL_put_atom(t, ATOM_dict);
        case PL_TYPE_EXT_ATOM:
	  fetchAtom(&b, &a);
	  break;
        case PL_TYPE_EXT_WATOM:
	  fetchAtomW(&b, &a);
	  break;
	default:
	  assert(0);
      }
      rc = PL_put_atom(t, a);
      PL_unregister_atom(a);
    }

    return rc;
  }

  skipSizeInt(&b);			/* code-size */
  gsize = fetchSizeInt(&b);
  if ( !(b.gbase = b.gstore = allocGlobal(gsize)) )
    return FALSE;			/* global stack overflow */
  b.dicts = 0;
  if ( !(m & REC_GROUND) )
  { uint nvars = fetchSizeInt(&b);

    if ( (rc=init_copy_vars(&b, nvars)) == TRUE )
    { rc = copy_record(valTermRef(t), &b PASS_LD);
      free_copy_vars(&b);
    }
  } else
  { rc = copy_record(valTermRef(t), &b PASS_LD);
  }

  if ( rc != TRUE )
    return raiseStackOverflow(rc);

  assert(b.gstore == gTop);

  if ( b.dicts )
    resortDictsInTerm(t);
  DEBUG(CHK_SECURE, checkData(valTermRef(t)));

  return TRUE;
}


int
PL_erase_external(char *rec)
{ PL_free(rec);
  return TRUE;
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
{ if ( isAtom(val) || isTaggedInt(val) )
  { return _PL_unify_atomic(key, val);
  } else
  { GET_LD

    return PL_unify_functor(key, (functor_t) val);
  }
}


int
getKeyEx(term_t key, word *w ARG_LD)
{ Word k = valTermRef(key);
  deRef(k);

  if ( isAtom(*k) || isTaggedInt(*k) )
    *w = *k;
  else if ( isTerm(*k) )
    *w = (word)functorTerm(*k);
  else
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_key, key);

  succeed;
}


static
PRED_IMPL("current_key", 1, current_key, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  fid_t fid;
  TableEnum e;
  word k = 0L;


  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { if ( PL_is_variable(A1) )
      { e = newTableEnum(GD->recorded_db.record_lists);
	break;
      } else if ( getKeyEx(A1, &k PASS_LD) &&
		  isCurrentRecordList(k, TRUE) )
	succeed;

      fail;
    }
    case FRG_REDO:
      e = CTX_PTR;
      break;
    case FRG_CUTTED:
      e = CTX_PTR;
      freeTableEnum(e);
      /*FALLTHROUGH*/
    default:				/* fool gcc */
      return TRUE;
  }

  if ( (fid = PL_open_foreign_frame()) )
  { void *sk, *sv;

    while(advanceTableEnum(e, &sk, &sv))
    { RecordList rl = sv;

      if ( rl->firstRecord && unifyKey(A1, rl->key) )
      { PL_close_foreign_frame(fid);

	ForeignRedoPtr(e);
      }

      PL_rewind_foreign_frame(fid);
    }

    PL_close_foreign_frame(fid);
  }

  freeTableEnum(e);
  return FALSE;
}


static bool
record(term_t key, term_t term, term_t ref, int az)
{ GET_LD
  RecordList l;
  RecordRef r;
  Record copy;
  word k = 0L;

  DEBUG(3, Sdprintf("record() of ");
	   PL_write_term(Serror, term, 1200, PL_WRT_ATTVAR_WRITE);
	   Sdprintf("\n"));

  if ( !getKeyEx(key, &k PASS_LD) )
    fail;
  if ( ref && !PL_is_variable(ref) )
    return PL_uninstantiation_error(ref);

  if ( !(copy = compileTermToHeap(term, 0)) )
    return PL_no_memory();
  r = allocHeapOrHalt(sizeof(*r));
  r->record = copy;
  if ( ref && !PL_unify_recref(ref, r) )
  { PL_erase(copy);
    freeHeap(r, sizeof(*r));
    return FALSE;
  }

  PL_LOCK(L_RECORD);
  l = lookupRecordList(k);
  r->list = l;

  if ( !l->firstRecord )
  { r->next = r->prev = NULL;
    l->firstRecord = l->lastRecord = r;
  } else if ( az == RECORDA )
  { r->prev = NULL;
    r->next = l->firstRecord;
    l->firstRecord->prev = r;
    l->firstRecord = r;
  } else
  { r->next = NULL;
    r->prev = l->lastRecord;
    l->lastRecord->next = r;
    l->lastRecord = r;
  }

  PL_UNLOCK(L_RECORD);

  succeed;
}


static
PRED_IMPL("recorda", va, recorda, 0)
{ return record(A1, A2, CTX_ARITY == 3 ? A3 : 0, RECORDA);
}


static
PRED_IMPL("recordz", va, recordz, 0)
{ return record(A1, A2, CTX_ARITY == 3 ? A3 : 0, RECORDZ);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
recorded/2,3. The state enumerates keys using the  `e` member if the key
is unbound on  entry.  The  `r`  member   is  the  current  record.

Enumeration first scans the records and then, if `e` is set, advanced to
the next key.

All manipulation on the state is done whild holding L_RECORD.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct
{ TableEnum e;				/* enumerating over keys */
  RecordRef r;				/* current record */
  int	    saved;
} recorded_state;

static recorded_state *
save_state(recorded_state *state)
{ if ( state->saved )
  { return state;
  } else
  { recorded_state *newstate = allocForeignState(sizeof(*state));
    memcpy(newstate, state, sizeof(*state));
    newstate->saved = TRUE;
    return newstate;
  }
}

/* MT: must hold L_RECORD */

static void
free_state(recorded_state *state)
{ if ( state->e )
    freeTableEnum(state->e);
  if ( state->r )
  { RecordList rl = state->r->list;

    if ( --rl->references == 0 && true(rl, RL_DIRTY) )
      cleanRecordList(rl);
  }
  if ( state->saved )
    freeForeignState(state, sizeof(*state));
}


/* set state to the next non-erased record.  Cleanup the record
   list if we reached the end.
*/

static RecordRef
advance_state(recorded_state *state)
{ RecordRef r = state->r;

  do
  { if ( !r->next )
    { RecordList rl = r->list;

      if ( --rl->references == 0 && true(rl, RL_DIRTY) )
	cleanRecordList(rl);
    }
    r = r->next;
  } while ( r && true(r->record, R_ERASED) );

  state->r = r;
  return r;
}


static
PRED_IMPL("recorded", va, recorded, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  recorded_state state_buf;
  recorded_state *state = &state_buf;
  word k = 0L;
  int rc;
  fid_t fid;

  term_t key  = A1;
  term_t term = A2;
  term_t ref  = (CTX_ARITY == 3 ? A3 : 0);

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { if ( ref && !PL_is_variable(ref) )		/* recorded(?,?,+) */
      { RecordRef record;

	if ( PL_get_recref(ref, &record) )
	{ PL_LOCK(L_RECORD);
	  if ( unifyKey(key, record->list->key) )
	  { term_t copy = PL_new_term_ref();

	    if ( (rc=copyRecordToGlobal(copy, record->record,
					ALLOW_GC PASS_LD)) < 0 )
	      rc = raiseStackOverflow(rc);
	    else
	      rc = PL_unify(term, copy);
	  } else
	    rc = FALSE;
	  PL_UNLOCK(L_RECORD);

	  return rc;
	}
	return FALSE;
      }

      memset(state, 0, sizeof(*state));
      if ( PL_is_variable(key) )
      { state->e = newTableEnum(GD->recorded_db.record_lists);
	PL_LOCK(L_RECORD);
      } else if ( getKeyEx(key, &k PASS_LD) )
      { RecordList rl;

	if ( !(rl = isCurrentRecordList(k, TRUE)) )
	  return FALSE;
	PL_LOCK(L_RECORD);
	rl->references++;
	state->r = rl->firstRecord;
      } else
      { return FALSE;
      }
      break;
    }
    case FRG_REDO:
    { state = CTX_PTR;
      PL_LOCK(L_RECORD);
      break;
    }
    case FRG_CUTTED:
    { state = CTX_PTR;
      PL_LOCK(L_RECORD);
      free_state(state);
      PL_UNLOCK(L_RECORD);
    }
    /*FALLTHROUGH*/
    default:
      succeed;
  }

  /* Now holding L_RECORD */
  if ( (fid = PL_open_foreign_frame()) )
  { int answered = FALSE;
    term_t copy = 0;

    while( !answered )
    { for( ; state->r; advance_state(state) )
      { RecordRef record;

      next:
	record = state->r;

	if ( !copy && !(copy = PL_new_term_ref()) )
	  goto error;
	if ( (rc=copyRecordToGlobal(copy, record->record, ALLOW_GC PASS_LD)) < 0 )
	{ raiseStackOverflow(rc);
	  goto error;
	}

	if ( PL_unify(term, copy) &&
	     (!ref || PL_unify_recref(ref, record)) )
	{ if ( state->e && !unifyKey(key, record->list->key) )
	    goto error;			/* stack overflow */
	} else
	{ if ( PL_exception(0) )
	    goto error;
	  PL_rewind_foreign_frame(fid);
	  continue;
	}

	answered = TRUE;

	if ( record->next )
	{ state->r = record->next;
	  PL_UNLOCK(L_RECORD);
	  PL_close_foreign_frame(fid);
	  ForeignRedoPtr(save_state(state));
	}
      }

      if ( state->e )
      { void *sk, *sv;

	while(advanceTableEnum(state->e, &sk, &sv))
	{ RecordList rl = sv;
	  RecordRef r;

	  if ( (r=firstRecordRecordList(rl)) )
	  { rl->references++;
	    state->r = r;
	    if ( answered )
	      break;
	    goto next;			/* try next list */
	  }
	}
      }

      if ( answered )
      { PL_close_foreign_frame(fid);
	if ( state->e )
	{ PL_UNLOCK(L_RECORD);
	  ForeignRedoPtr(save_state(state));
	} else
	{ free_state(state);
	  PL_UNLOCK(L_RECORD);
	  return TRUE;
	}
      }

      break;
    }

  error:
    PL_close_foreign_frame(fid);
  }

  free_state(state);
  PL_UNLOCK(L_RECORD);

  return FALSE;
}


/** instance(+Ref, -Term)
*/

static
PRED_IMPL("instance", 2, instance, 0)
{ PRED_LD
  void *ptr;
  db_ref_type type;

  term_t ref  = A1;
  term_t term = A2;

  if ( !(ptr=PL_get_dbref(ref, &type)) )
    return FALSE;

  if ( type == DB_REF_CLAUSE )
  { Clause clause = ptr;
    gen_t generation = generationFrame(environment_frame);

    if ( true(clause, GOAL_CLAUSE) ||
	 !visibleClause(clause, generation) )
      return FALSE;

    if ( true(clause, UNIT_CLAUSE) )
    { term_t head = PL_new_term_ref();

      return ( decompile(clause, head, 0) &&
	       PL_unify_term(term,
			     PL_FUNCTOR, FUNCTOR_prove2,
			       PL_TERM, head,
			       PL_ATOM, ATOM_true) );
    } else
    { return decompile(clause, term, 0);
    }
  } else
  { RecordRef rref = ptr;
    term_t t = PL_new_term_ref();

    if ( copyRecordToGlobal(t, rref->record, ALLOW_GC PASS_LD) == TRUE )
      return PL_unify(term, t);
  }

  return FALSE;
}



static
PRED_IMPL("erase", 1, erase, 0)
{ void *ptr;
  RecordList l;
  db_ref_type type;

  term_t ref = A1;

  if ( !(ptr=PL_get_dbref(ref, &type)) )
    return FALSE;

  if ( type == DB_REF_CLAUSE )
  { Clause clause = ptr;
    Definition def = clause->predicate;

    if ( !true(def, P_DYNAMIC) )
      return PL_error("erase", 1, NULL, ERR_PERMISSION,
		      ATOM_clause, ATOM_erase, ref);

    return retractClauseDefinition(def, clause);
  } else
  { RecordRef r = ptr;
    int rc;

    rc = callEventHook(PLEV_ERASED_RECORD, r);

    PL_LOCK(L_RECORD);
    l = r->list;
    if ( l->references )		/* a recorded has choicepoints */
    { set(r->record, R_ERASED);
      set(l, RL_DIRTY);
    } else
    { remove_record(r);
    }
    PL_UNLOCK(L_RECORD);
    return rc;
  }
}

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

#define NDET PL_FA_NONDETERMINISTIC

BeginPredDefs(rec)
  PRED_SHARE("recorded",	   2, recorded,		    NDET)
  PRED_SHARE("recorded",	   3, recorded,		    NDET)
  PRED_SHARE("recordz",		   2, recordz,		    0)
  PRED_SHARE("recordz",		   3, recordz,		    0)
  PRED_SHARE("recorda",		   2, recorda,		    0)
  PRED_SHARE("recorda",		   3, recorda,		    0)
  PRED_DEF("erase",		   1, erase,		    0)
  PRED_DEF("instance",		   2, instance,		    0)
  PRED_DEF("current_key",	   1, current_key,	    NDET)

  PRED_DEF("fast_term_serialized", 2, fast_term_serialized, 0)
  PRED_DEF("fast_write",	   2, fast_write,	    0)
  PRED_DEF("fast_read",		   2, fast_read,	    0)
EndPredDefs
