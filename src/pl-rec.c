/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: recorded database (record[az], recorded, erase)
    MT-status: SAFE
*/

/*#define O_SECURE 1*/
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
  { if ( r->erased )
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


#ifndef offsetof
#define offsetof(structure, field) ((int) &(((structure *)NULL)->field))
#endif

#define SIZERECORD  offsetof(struct record, buffer[0])

typedef struct
{ tmp_buffer code;			/* code buffer */
  tmp_buffer vars;			/* variable pointers */
  int	     size;			/* size on global stack */
  int	     nvars;			/* # variables */
} compile_info, *CompileInfo;


#define	PL_TYPE_VARIABLE	(1)	/* variable */
#define PL_TYPE_ATOM		(2)	/* atom */
#define PL_TYPE_INTEGER	  	(3)	/* big integer */
#define PL_TYPE_TAGGED_INTEGER  (4)	/* tagged integer */
#define PL_TYPE_FLOAT	  	(5)	/* double */
#define PL_TYPE_STRING	  	(6)	/* string */
#define PL_TYPE_COMPOUND	(7)	/* compound term */

static void
compile_term_to_heap(Word p, CompileInfo info)
{ GET_LD
  word w;

right_recursion:
  w = *p;

  switch(tag(w))
  { case TAG_VAR:
    { int n = info->nvars++;

      *p = (n<<7)|TAG_ATOM|STG_GLOBAL;
      addUnalignedBuffer(&info->vars, p, Word);
      addBuffer(&info->code, PL_TYPE_VARIABLE, char);
      addUnalignedBuffer(&info->code, n, int);

      return;
    }
    case TAG_ATOM:
    { if ( storage(w) == STG_GLOBAL )
      { int n = ((long)(w) >> 7);

	addBuffer(&info->code, PL_TYPE_VARIABLE, char);
	addUnalignedBuffer(&info->code, n, int);
      } else
      { addBuffer(&info->code, PL_TYPE_ATOM, char);
	addUnalignedBuffer(&info->code, w, atom_t);
	PL_register_atom(w);
      }
      return;
    }
    case TAG_INTEGER:
    { long val;

      if ( isTaggedInt(w) )
      { val = valInt(w);
	addBuffer(&info->code, PL_TYPE_TAGGED_INTEGER, char);
      } else
      { info->size += sizeof(long)/sizeof(word) + 2;
	val = valBignum(w);
	addBuffer(&info->code, PL_TYPE_INTEGER, char);
      }
      
      addUnalignedBuffer(&info->code, val, long);
      return;
    }
    case TAG_STRING:
    { Word f  = addressIndirect(w);
      int n   = wsizeofInd(*f);
      int pad = padHdr(*f);		/* see also sizeString() */
      int l   = n*sizeof(word)-pad;

      info->size += n+2;
      addBuffer(&info->code, PL_TYPE_STRING, char);
      addUnalignedBuffer(&info->code, l, int);
      addMultipleBuffer(&info->code, f+1, n, word);
      
      return;
    }
    case TAG_FLOAT:
    { double val = valReal(w);

      info->size += sizeof(double)/sizeof(word) + 2;
      addBuffer(&info->code, PL_TYPE_FLOAT, char);
      addUnalignedBuffer(&info->code, val, double);

      return;
    }
    case TAG_COMPOUND:
    { Functor f = valueTerm(w);
      int arity = arityFunctor(f->definition);

      info->size += arity+1;
      addBuffer(&info->code, PL_TYPE_COMPOUND, char);
      addUnalignedBuffer(&info->code, f->definition, word);
      p = f->arguments;
      for(; --arity > 0; p++)
      { compile_term_to_heap(p, info);
      }
      goto right_recursion;
    }
    case TAG_REFERENCE:
      p = unRef(w);
      goto right_recursion;
  }
}



Record
compileTermToHeap(term_t t)
{ GET_LD
  compile_info info;
  Record record;
  Word *p;
  int n, size;

  SECURE(checkData(valTermRef(t)));

  initBuffer(&info.code);
  initBuffer(&info.vars);
  info.size = 0;
  info.nvars = 0;

  compile_term_to_heap(valTermRef(t), &info);
  n = info.nvars;
  p = (Word *)info.vars.base;
  while(--n >= 0)
    setVar(**p++);
  discardBuffer(&info.vars);
  
  size = SIZERECORD + sizeOfBuffer(&info.code);
  record = allocHeap(size);
  record->gsize = info.size;
  record->nvars = info.nvars;
  record->size = size;
  record->erased = FALSE;
  record->references = 1;
  memcpy(record->buffer, info.code.base, sizeOfBuffer(&info.code));
  discardBuffer(&info.code);

  return record;
}


typedef struct
{ char *data;
  Word *vars;
  Word gstore;
} copy_info, *CopyInfo;

#define fetchBuf(b, var, type) \
		do \
		{ *var = *((type *)(b)->data); \
		  (b)->data += sizeof(type); \
		} while(0)
#define fetchUnalignedBuf(b, var, type) \
		do \
		{ memcpy(var, (b)->data, sizeof(type)); \
		  (b)->data += sizeof(type); \
		} while(0)
#define fetchMultipleBuf(b, var, times, type) \
		do \
		{ int _n = (times) * sizeof(type); \
		  memcpy(var, (b)->data, _n); \
		  (b)->data += _n; \
		} while(0)
#define skipBuf(b, type) \
		((b)->data += sizeof(type))


#ifndef WORDS_PER_DOUBLE
#define WORDS_PER_DOUBLE ((sizeof(double)+sizeof(word)-1)/sizeof(word))
#endif

static void
copy_record(Word p, CopyInfo b)
{ GET_LD
  int tag;

right_recursion:
  fetchBuf(b, &tag, char);
  switch(tag)
  { case PL_TYPE_VARIABLE:
    { int n;

      fetchUnalignedBuf(b, &n, int);
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
    { atom_t val;

      fetchUnalignedBuf(b, &val, atom_t);
      *p = val;

      return;
    }
    case PL_TYPE_TAGGED_INTEGER:
    { long val;

      fetchUnalignedBuf(b, &val, long);
      *p = consInt(val);

      return;
    }
    case PL_TYPE_INTEGER:
    { long val;

      fetchUnalignedBuf(b, &val, long);
      *p = consPtr(b->gstore, TAG_INTEGER|STG_GLOBAL);
      *b->gstore++ = mkIndHdr(1, TAG_INTEGER);
      *b->gstore++ = val;
      *b->gstore++ = mkIndHdr(1, TAG_INTEGER);

      return;
    }
    case PL_TYPE_FLOAT:
    { double val;

      fetchUnalignedBuf(b, &val, double);
      *p = consPtr(b->gstore, TAG_FLOAT|STG_GLOBAL);
      *b->gstore++ = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);
      memcpy(b->gstore, &val, WORDS_PER_DOUBLE * sizeof(word));
      b->gstore += WORDS_PER_DOUBLE;
      *b->gstore++ = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);

      return;
    }
    case PL_TYPE_STRING:
    { int len, lw, pad;
      word hdr;

      fetchUnalignedBuf(b, &len, int);
      lw = (len+sizeof(word))/sizeof(word); /* see globalNString() */
      pad = (lw*sizeof(word) - len);
      *p = consPtr(b->gstore, TAG_STRING|STG_GLOBAL);
      *b->gstore++ = hdr = mkStrHdr(lw, pad);
      memcpy(b->gstore, b->data, lw * sizeof(word));
      b->gstore += lw;
      *b->gstore++ = hdr;
      b->data += lw * sizeof(word);

      return;
    }
    case PL_TYPE_COMPOUND:
    { word fdef;
      int arity;

      fetchUnalignedBuf(b, &fdef, word);
      arity = arityFunctor(fdef);

      *p = consPtr(b->gstore, TAG_COMPOUND|STG_GLOBAL);
      *b->gstore++ = fdef;
      p = b->gstore;
      b->gstore += arity;
      for(; --arity > 0; p++)
	copy_record(p, b);
      goto right_recursion;
    }
  }
}


void
copyRecordToGlobal(term_t copy, Record r ARG_LD)
{ copy_info b;
  Word *p;
  int n;

  b.data = r->buffer;
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
unregisterAtomsRecord(CopyInfo b)
{ int tag;

right_recursion:
  fetchBuf(b, &tag, char);
  switch(tag)
  { case PL_TYPE_VARIABLE:
    { skipBuf(b, int);
      return;
    }
    case PL_TYPE_ATOM:
    { atom_t val;

      fetchUnalignedBuf(b, &val, atom_t);
      PL_unregister_atom(val);
      return;
    }
    case PL_TYPE_TAGGED_INTEGER:
    case PL_TYPE_INTEGER:
    { skipBuf(b, long);
      return;
    }
    case PL_TYPE_FLOAT:
    { skipBuf(b, double);
      return;
    }
    case PL_TYPE_STRING:
    { int len, lw;

      fetchUnalignedBuf(b, &len, int);
      lw = (len+sizeof(word))/sizeof(word); /* see globalNString() */
      b->data += lw * sizeof(word);

      return;
    }
    case PL_TYPE_COMPOUND:
    { word fdef;
      int arity;

      fetchUnalignedBuf(b, &fdef, word);
      arity = arityFunctor(fdef);
      while(--arity > 0)
	unregisterAtomsRecord(b);
      goto right_recursion;
    }
  }
}

#endif /*O_ATOMGC*/


		 /*******************************
		 *     STRUCTURAL EQUIVALENCE	*
		 *******************************/

typedef struct
{ char *data;
  tmp_buffer vars;
} se_info, *SeInfo;


static int
se_record(Word p, SeInfo info ARG_LD)
{ word w;
  int stag;

right_recursion:
  fetchBuf(info, &stag, char);
unref_cont:
  w = *p;

  switch(tag(w))
  { case TAG_VAR:
      if ( stag == PL_TYPE_VARIABLE )
      { int n = entriesBuffer(&info->vars, Word);
	int i;

	fetchUnalignedBuf(info, &i, int);
	if ( i != n )
	  fail;

	*p = (n<<7)|TAG_ATOM|STG_GLOBAL;
	addUnalignedBuffer(&info->vars, p, Word);
	succeed;
      }
      fail;
    case TAG_ATOM:
      if ( storage(w) == STG_GLOBAL )
      { if ( stag == PL_TYPE_VARIABLE )
	{ int n = ((long)(w) >> 7);
	  int i;

	  fetchUnalignedBuf(info, &i, int);
	  if ( i == n )
	    succeed;
	}
	fail;
      } else if ( stag == PL_TYPE_ATOM )
      { atom_t val;

	fetchUnalignedBuf(info, &val, atom_t);
	if ( val == w )
	  succeed;
      }

      fail;
    case TAG_INTEGER:
      if ( isTaggedInt(w) )
      { if ( stag == PL_TYPE_TAGGED_INTEGER )
	{ long val = valInt(w);
	  long v2;

	  fetchUnalignedBuf(info, &v2, long);
	  if ( v2 == val )
	    succeed;
	}
      } else
      { if ( stag == PL_TYPE_INTEGER )
	{ long val = valBignum(w);
	  long v2;

	  fetchUnalignedBuf(info, &v2, long);
	  if ( v2 == val )
	    succeed;
	}
      }
      fail;
    case TAG_STRING:
      if ( stag == PL_TYPE_STRING )
      { int len;
	char *s1 = valString(w);
	word m  = *((Word)addressIndirect(w));
	int wn  = wsizeofInd(m);

	fetchUnalignedBuf(info, &len, int);
	if ( wn == len && memcmp(s1, info->data, len * sizeof(word)) == 0 )
	{ info->data += len * sizeof(word);
	  succeed;
	}
      }
      fail;
    case TAG_FLOAT:
      if ( stag == PL_TYPE_FLOAT )
      { double val = valReal(w);
	
	if ( memcmp(&val, info->data, sizeof(double)) == 0 )
	{ info->data += sizeof(double);
	  succeed;
	}
      }

      fail;
    case TAG_COMPOUND:
      if ( stag == PL_TYPE_COMPOUND )
      { Functor f = valueTerm(w);
	word fdef;

	fetchUnalignedBuf(info, &fdef, word);
	if ( fdef == f->definition )
	{ int arity = arityFunctor(fdef);

	  p = f->arguments;
	  for(; --arity > 0; p++)
	  { if ( !se_record(p, info PASS_LD) )
	      fail;
	  }
	  goto right_recursion;
	}
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
{ se_info info;
  int n, rval;
  Word *p;

  initBuffer(&info.vars);
  info.data = r->buffer + sizeof(char) + sizeof(word);
					/* skip PL_TYPE_COMPOUND <functor> */
  rval = se_record(valTermRef(t), &info PASS_LD);
  n = entriesBuffer(&info.vars, Word);
  p = (Word *)info.vars.base;
  while(--n >= 0)
    setVar(**p++);
  discardBuffer(&info.vars);

  return rval;
}


bool
freeRecord(Record record)
{ if ( --record->references == 0 )
  {
#ifdef O_ATOMGC
    copy_info ci;

    ci.data = record->buffer;
    unregisterAtomsRecord(&ci);
    assert(ci.data == (record->buffer + record->size - SIZERECORD));
#endif
    freeHeap(record, record->size);
  }

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

  copy = compileTermToHeap(term);
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

    if ( record->erased )
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
    { record->erased = TRUE;
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
  int n;
  Word *p;

  initBuffer(&info.code);
  initBuffer(&info.vars);
  info.size = 0;
  info.nvars = 0;

  compile_term_to_heap(term, &info);
  n = info.nvars;
  p = (Word *)info.vars.base;
  while(--n >= 0)
    setVar(**p++);

  assert(m->trailtop != INVALID_TRAILTOP);
  Undo(*m);
  
  b.data = info.code.base;
  if ( info.nvars > 0 )
  { if ( !(b.vars = alloca(sizeof(Word) * info.nvars)) )
      fatalError("alloca(%d) failed", info.nvars);
    for(p = b.vars, n=info.nvars; --n >= 0;)
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
