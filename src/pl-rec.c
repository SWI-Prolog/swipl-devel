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
forwards void copyTermToHeap2(Word, Record, Word);
forwards void freeHeapTerm(Word);

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
  return (RecordList) NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Copy a term from the global stack onto the heap.  Terms on the heap  are
not represented as `word', but as `record'.  A `record' holds additional
information  for  linking  it in the record list and to make copying the
term back on the global stack faster.

All variables of a term  on  the  heap   are  together  in  an  array of
variables of which the record knows  the   base  address  as well as the
number of variables. The term  itself   holds  no  variables, except for
direct references into the variable array.  Using this representation we
can easily create a new variable array   on  the global stack and change
the variables of the copied term  to   point  to this new variable array
when copying back to the global stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
copyTermToHeap2(register Word term, Record result, Word copy)
{ 
right_recursion:
  switch(tag(*term))
  { case TAG_VAR:
    case TAG_ATOM:
      *copy = *term;
      break;
    case TAG_INTEGER:
      if ( storage(*term) == STG_INLINE )
      { *copy = *term;
        break;
      }
    case TAG_FLOAT:
    case TAG_STRING:
      *copy = heapIndirect(*term);
      break;
    case TAG_COMPOUND:
    { Functor f = valueTerm(*term);

      if ( f->definition == FUNCTOR_var1->functor )
	*copy = makeRef(result->variables + valInt(f->arguments[0]));
      else
      { int arity = arityFunctor(f->definition);
	Word c = allocHeap((arity+1) * sizeof(word));

	*copy = consPtr(c, TAG_COMPOUND|STG_HEAP);
	*c++ = f->definition;
	copy = c;
	term = f->arguments;
	for(; --arity > 0; copy++, term++)
	  copyTermToHeap2(term, result, copy);
	goto right_recursion;
      }
      break;
    } 
    case TAG_REFERENCE:
      term = unRef(*term);
      goto right_recursion;
  }
}


Record
copyTermToHeap(term_t term)
{ fid_t cid = PL_open_foreign_frame();
  Record result;
  int n;
  Word v;

  SECURE(checkData(term, FALSE));
  result = (Record) allocHeap(sizeof(struct record) );
  result->n_vars = numberVars(term, FUNCTOR_var1, 0);
  if (result->n_vars > 0)
    result->variables = allocHeap(sizeof(word)*result->n_vars);
  for(n=result->n_vars, v=result->variables; n > 0; n--, v++)
    setVar(*v);
  copyTermToHeap2(valTermRef(term), result, &result->term);
  PL_discard_foreign_frame(cid);
  SECURE(checkData(term, FALSE));
  SECURE(checkData(&result->term, TRUE));

  return result;
}


static void
copyTermToGlobal2(Word orgvars, term_t *vars, Word term, term_t copy)
{ if ( isRef(*term) )
  { int nth = unRef(*term) - orgvars;

    if ( vars[nth] )
      PL_unify(copy, vars[nth]);
    else
    { vars[nth] = PL_new_term_ref();
      PL_put_term(vars[nth], copy);
    }

    return;
  }

  if ( isAtomic(*term) )
  { term_t c2 = PL_new_term_ref();

    _PL_copy_atomic(c2, *term);
    PL_unify(copy, c2);
  } else				/* term */
  { FunctorDef fd = functorTerm(*term);
    int arity = fd->arity;
    int n;
    term_t c2 = PL_new_term_ref();

    PL_unify_functor(copy, fd);
    term = argTermP(*term, 0);
    for(n = 0; n < arity; n++, term++)
    { PL_get_arg(n+1, copy, c2);
      copyTermToGlobal2(orgvars, vars, term, c2);
    }
  }
}


void
copyTermToGlobal(term_t copy, Record term)
{ term_t *vars;
  
  if ( term->n_vars > 0 )
  { int n;

    vars = alloca(term->n_vars * sizeof(term_t));
    for(n=0; n<term->n_vars; n++)
      vars[n] = 0L;			/* special constant? */
  } else
    vars = NULL;

  SECURE(checkData(&term->term, TRUE));
  PL_put_variable(copy);
  copyTermToGlobal2(term->variables, vars, &term->term, copy);
  SECURE(checkData(&copy, FALSE));
}


static void
freeHeapTerm(register Word term)
{ int arity, n;
  Word arg;
  
  deRef(term);

  if ( isAtom(*term) || isTaggedInt(*term) )
    return;

  if (isIndirect(*term))
  { freeHeapIndirect(*term);
    return;
  }

  if (isTerm(*term))
  { Functor f = (Functor)valPtr(*term);

    arity = arityFunctor(f->definition);
    arg = f->arguments;
    for(n = arity; n > 0; n--, arg++)
      freeHeapTerm(arg);

    freeHeap(f, (arity+1) * sizeof(word));
  }
}


bool
freeRecord(Record record)
{ SECURE(checkData(&record->term, TRUE));
  freeHeapTerm(&record->term);
  if (record->n_vars > 0)
    freeHeap(record->variables, sizeof(word)*record->n_vars);
  record->list = (RecordList) NULL;
  freeHeap(record, sizeof(struct record));

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
  copy = copyTermToHeap(term);
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

  DEBUG(5, Sdprintf("recorded: h=0x%lx, control = %d\n",
		    h, ForeignControl(h)));

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      if ( PL_get_pointer(ref, (void **)&record) )
      { if ( !isRecord(record) )
	  return warning("recorded/3: Invalid reference");
	
	if ( can_unify(valTermRef(term), &record->term) &&
	     unifyKey(key, record->list->key) )
	{ term_t copy = PL_new_term_ref();

	  copyTermToGlobal(copy, record);
	  return PL_unify(term, copy);
	} else
	{ fail;
	}
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

  for( ;record; record = record->next )
  { if ( can_unify(valTermRef(term), &record->term) &&
	 PL_unify_pointer(ref, record) )
    { term_t copy = PL_new_term_ref();

      copyTermToGlobal(copy, record);
      PL_unify(term, copy);

      if ( !record->next )
	succeed;
      else
	ForeignRedoPtr(record->next);
    }
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
