/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: recorded database (record[az], recorded, erase)
*/

/*#define O_SECURE 1*/
#include "pl-incl.h"

forwards RecordList lookupRecordList(word);
forwards RecordList isCurrentRecordList(word);

#define RECORDA 0
#define RECORDZ 1

static RecordList recordTable[RECORDHASHSIZE];

void
initRecords(void)
{ register RecordList *l;
  register int n;

  for(n=0, l=recordTable; n < (RECORDHASHSIZE-1); n++, l++)
    *l = makeTableRef(l+1);
}


static RecordList
lookupRecordList(register word key)
{ int v = pointerHashValue(key, RECORDHASHSIZE);
  register RecordList l;

  for(l=recordTable[v]; l && !isTableRef(l); l = l->next)
  { if (l->key == key)
      return l;
  }
  l = (RecordList) allocHeap(sizeof(struct recordList) );
  l->next = recordTable[v];
  recordTable[v] = l;
  l->key = key;
  l->firstRecord = l->lastRecord = (Record) NULL;
  l->type = RECORD_TYPE;

  return l;
}


static RecordList
isCurrentRecordList(register word key)
{ int v = pointerHashValue(key, RECORDHASHSIZE);
  register RecordList l;

  for(l=recordTable[v]; l && !isTableRef(l); l = l->next)
  { if (l->key == key)
      return l;
  }
  return NULL;
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
{ word w;

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
    { Word f = addressIndirect(w);
      int n = wsizeofInd(*f);

      info->size += n+2;
      addBuffer(&info->code, PL_TYPE_STRING, char);
      addUnalignedBuffer(&info->code, n, int);
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
{ compile_info info;
  Record record;
  Word *p;
  int n, size;

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


#ifndef WORDS_PER_DOUBLE
#define WORDS_PER_DOUBLE ((sizeof(double)+sizeof(word)-1)/sizeof(word))
#endif

static void
copy_record(Word p, CopyInfo b)
{ int tag;

right_recursion:
  fetchBuf(b, &tag, char);
  switch(tag)
  { case PL_TYPE_VARIABLE:
    { int n;

      fetchUnalignedBuf(b, &n, int);
      if ( b->vars[n] )
	*p = makeRef(b->vars[n]);
      else
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
    { int len;

      fetchUnalignedBuf(b, &len, int);
      *p = consPtr(b->gstore, TAG_STRING|STG_GLOBAL);
      *b->gstore++ = mkIndHdr(len, TAG_STRING);
      memcpy(b->gstore, b->data, len * sizeof(word));
      b->gstore += len;
      *b->gstore++ = mkIndHdr(len, TAG_STRING);
      b->data += len * sizeof(word);

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
copyRecordToGlobal(term_t copy, Record r)
{ copy_info b;
  Word *p;
  int n;

  b.data = r->buffer;
  if ( !(b.vars = alloca(sizeof(Word) * r->nvars)) )
    fatalError("alloca() failed");
  for(p = b.vars, n=r->nvars; --n >= 0;)
    *p++ = 0;
  b.gstore = allocGlobal(r->gsize);
  
  copy_record(valTermRef(copy), &b);
  if ( b.gstore != gTop )
  { Sdprintf("b.gstore = %p, gTop = %p\n", b.gstore, gTop);
  }
}

		 /*******************************
		 *     STRUCTURAL EQUIVALENCE	*
		 *******************************/

typedef struct
{ char *data;
  tmp_buffer vars;
} se_info, *SeInfo;


static int
se_record(Word p, SeInfo info)
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
	  { if ( !se_record(p, info) )
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
structuralEqualArg1OfRecord(term_t t, Record r)
{ se_info info;
  int n, rval;
  Word *p;

  initBuffer(&info.vars);
  info.data = r->buffer + sizeof(char) + sizeof(word);
					/* skip PL_TYPE_COMPOUND <functor> */
  rval = se_record(valTermRef(t), &info);
  n = entriesBuffer(&info.vars, Word);
  p = (Word *)info.vars.base;
  while(--n >= 0)
    setVar(**p++);
  discardBuffer(&info.vars);

  return rval;
}


bool
freeRecord(Record record)
{ freeHeap(record, record->size);

  succeed;
}

		/********************************
		*       PROLOG CONNECTION       *
		*********************************/

bool
unifyKey(term_t key, word val)
{ if ( isAtom(val) || isTaggedInt(val) ) /* TBD? */
    return _PL_unify_atomic(key, val);

  return PL_unify_functor(key, (FunctorDef) val);
}


word
getKey(term_t key)
{ Word k = valTermRef(key);
  deRef(k);

  if ( isAtom(*k) || isTaggedInt(*k) )
    return *k;
  else if ( isTerm(*k) )
    return (word)functorTerm(*k);
  else
    return (word)NULL;
}


word
pl_current_key(term_t k, word h)
{ RecordList l;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      l = recordTable[0];
      break;
    case FRG_REDO:
      l = ForeignContextPtr(h);
      break;
    case FRG_CUTTED:
    default:
      succeed;
  }

  for(; l; l = l->next)
  { while(isTableRef(l) )
    { l = unTableRef(RecordList, l);
      if ( !l )
	fail;
    }
    if ( l->firstRecord == NULL || unifyKey(k, l->key) == FALSE )
      continue;

    return_next_table(RecordList, l, ;);
  }

  fail;
}

static bool
record(term_t key, term_t term, term_t ref, int az)
{ RecordList l;
  Record copy;
  word k;

  if ( !(k = getKey(key)) )
    return warning("record%c/3: illegal key", az == RECORDA ? 'a' : 'z');

  l = lookupRecordList(k);
  copy = compileTermToHeap(term);
  copy->list = l;

  TRY(PL_unify_pointer(ref, copy));
  if ( !l->firstRecord )
  { copy->next = (Record) NULL;
    l->firstRecord = l->lastRecord = copy;
    succeed;
  }
  if ( az == RECORDA )
  { copy->next = l->firstRecord;
    l->firstRecord = copy;
    succeed;
  }
  copy->next = (Record) NULL;
  l->lastRecord->next = copy;
  l->lastRecord = copy;

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

  DEBUG(5, Sdprintf("recorded: h=0x%lx, control = %d\n",
		    h, ForeignControl(h)));

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      if ( PL_get_pointer(ref, (void **)&record) )
      { if ( !isRecord(record) )
	  return warning("recorded/3: Invalid reference");
	if ( !unifyKey(key, record->list->key) )
	  fail;
	copy = PL_new_term_ref();
	copyRecordToGlobal(copy, record);
	return PL_unify(term, copy);
      }
      if ( !(k = getKey(key)) )
	return warning("recorded/3: illegal key");
      if ( !(rl = isCurrentRecordList(k)) )
	fail;
      record = rl->firstRecord;
      break;
    case FRG_REDO:
      record = ForeignContextPtr(h);
      break;
    case FRG_CUTTED:
    default:
      succeed;
  }

  copy = PL_new_term_ref();
  for( ;record; record = record->next )
  { mark m;

    Mark(m);
    copyRecordToGlobal(copy, record);	/* unifyRecordToGlobal()? */
    if ( PL_unify(term, copy) && PL_unify_pointer(ref, record) )
    { if ( !record->next )
	succeed;
      else
	ForeignRedoPtr(record->next);
    }
    Undo(m);
  }

  fail;
}


word
pl_erase(term_t ref)
{ Record record;
  Record prev, r;
  RecordList l;

  if ( !PL_get_pointer(ref, (void **)&record) ||
       !inCore(record))
    return warning("erase/1: Invalid reference");

  if ( isClause(record) )
  { Clause clause = (Clause) record;
  
    if ( true(clause->procedure->definition, LOCKED) &&
	 false(clause->procedure->definition, DYNAMIC) )
      return warning("erase/1: Attempt to erase clause from system predicate");

    return retractClauseProcedure(clause->procedure, clause);
  }
  
  if ( !isRecord(record) )
    return warning("erase/1: Invalid reference");

#if O_DEBUGGER
  callEventHook(PLEV_ERASED, record);
#endif

  l = record->list;
  if ( record == l->firstRecord )
  { if ( record->next == (Record) NULL )
      l->lastRecord = (Record) NULL;
    l->firstRecord = record->next;
    freeRecord(record);
    succeed;
  }

  prev = l->firstRecord;
  r = prev->next;
  for(; r; prev = r, r = r->next)
  { if (r == record)
    { if ( r->next == (Record) NULL )
        l->lastRecord = prev;
      prev->next = r->next;
      freeRecord(r);
      succeed;
    }
  }

  return warning("erase/1: Invalid reference");
}
