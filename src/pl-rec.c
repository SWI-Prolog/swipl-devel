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
forwards word	   heapFunctor(FunctorDef);
forwards void	   copyTermToHeap2(Word, Record, Word);
forwards void	   copyTermToGlobal2(Word, Word *, Word, Word);
forwards void	   freeHeapTerm(Word);
forwards bool	   record(Word, Word, Word, char);

static RecordList recordTable[RECORDHASHSIZE];

void
initRecords(void)
{ register RecordList *l;
  register int n;

  for(n=0, l=recordTable; n < (RECORDHASHSIZE-1); n++, l++)
    *l = (RecordList) makeRef(l+1);
}

static RecordList
lookupRecordList(register word key)
{ int v = pointerHashValue(key, RECORDHASHSIZE);
  register RecordList l;

  for(l=recordTable[v]; l && !isRef((word)l); l = l->next)
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

  for(l=recordTable[v]; l && !isRef((word)l); l = l->next)
  { if (l->key == key)
      return l;
  }
  return (RecordList) NULL;
}

static word
heapFunctor(FunctorDef def)
{ Functor f;
  register int n;
  register Word a;

  f = (Functor)allocHeap(sizeof(FunctorDef) + sizeof(word)*def->arity);
  f->definition = def;
  for(n=def->arity, a=argTermP(f, 0); n > 0; n--, a++)
    setVar(*a);

  return (word) f;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Copy a term from the global stack onto the heap.  Terms on the heap  are
not represented as `word', but as `record'.  A `record' holds additional
information  for  linking  it in the record list and to make copying the
term back on the global stack faster.

All variables of a term  on  the  heap  are  together  in  an  array  of
variables  of  which  the  record  knows the base address as well as the
number of variables.  The term itself holds no  references,  except  for
direct references into the variable array.  Using this representation we
can  easily  create  a new variable array on the global stack and change
the variables of the copied term to point to  this  new  variable  array
when copying back to the global stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
copyTermToHeap2(register Word term, Record result, Word copy)
{ int arity;

  deRef(term);

  if (isAtom(*term) || isInteger(*term) )
  { *copy = *term;
    return;
  }
  if ( isIndirect(*term) )
  {
#if O_STRING
    if ( isString(*term) )
    { *copy = heapString(valString(*term));
      return;
    }
#endif
    *copy = heapReal(valReal(*term));
    return;
  }
  SECURE(if (!isTerm(*term) )
	    sysError("Illegal type in copyTermToHeap()") );
  if (functorTerm(*term) == FUNCTOR_var1)
  { *copy = makeRef(result->variables + valNum(argTerm(*term, 0)));
    return;
  }
  arity = functorTerm(*term)->arity;
  *copy = heapFunctor(functorTerm(*term) );
  copy = argTermP(*copy, 0);
  term = argTermP(*term, 0);
  for(; arity > 0; arity--, copy++, term++)
    copyTermToHeap2(term, result, copy);
}

Record
copyTermToHeap(Word term)
{ mark m;
  Record result;
  register int n;
  register Word v;

  SECURE(checkData(term, FALSE));
  result = (Record) allocHeap(sizeof(struct record) );
  Mark(m);
  result->n_vars = numberVars(term, FUNCTOR_var1, 0);

  if (result->n_vars > 0)
    result->variables = allocHeap(sizeof(word)*result->n_vars);
  for(n=result->n_vars, v=result->variables; n > 0; n--, v++)
    setVar(*v);

  copyTermToHeap2(term, result, &result->term);
  Undo(m);
  SECURE(checkData(term, FALSE));
  SECURE(checkData(&result->term, TRUE));

  return result;
}


static void
copyTermToGlobal2(Word orgvars, Word *vars, register Word term, Word copy)
{ bool locked;

  if (isRef(*term) )
  { *copy = makeRef(unRef(*term) - orgvars + *vars);
    return;
  }
  if (isAtom(*term) || isInteger(*term))
  { *copy = *term;
    return;
  }

  if ( (locked = onStack(global, copy)) )
    lockp(&copy);

  if (isIndirect(*term))
  {
#if O_STRING
    if ( isString(*term) )
      *copy = globalString(valString(*term));
    else
#endif /* O_STRING */
      *copy = globalReal(valReal(*term));
  } else				/* term */
  { int arity = functorTerm(*term)->arity;
    int n;

    *copy = globalFunctor(functorTerm(*term));

    term = argTermP(*term, 0);
    for(n = 0; n < arity; n++, term++)
      copyTermToGlobal2(orgvars, vars, term, argTermP(*copy, n));
  }

  if ( locked )
    unlockp(&copy);
}

word
copyTermToGlobal(register Record term)
{ Word vars;
  word copy = 0;

  if (term->n_vars > 0)
  { register int n;
    register Word v;

    vars = allocGlobal(sizeof(word) * term->n_vars);
    for(n=term->n_vars, v=vars; n>0; n--, v++)
      setVar(*v);
  } else
    vars = (Word) NULL;

  SECURE(checkData(&term->term, TRUE));
  lockp(&vars);
  lockw(&copy);
  copyTermToGlobal2(term->variables, &vars, &term->term, &copy);
  unlockw(&copy);
  unlockp(&vars);
  SECURE(checkData(&copy, FALSE));

  return copy;
}


static void
freeHeapTerm(register Word term)
{ int arity, n;
  Word arg;
  
  deRef(term);

  if (isAtom(*term) || isInteger(*term))
    return;
  if (isIndirect(*term))
  {
#if O_STRING
    if ( isString(*term) )
    { freeHeap(unMask(*term), allocSizeString(sizeString(*term)));
      return;
    }
#endif /* O_STRING */
    freeHeap(unMask(*term), sizeof(real));
    return;
  }
  if (isTerm(*term))
  { arity = functorTerm(*term)->arity;
    arg = argTermP(*term, 0);
    for(n = arity; n > 0; n--, arg++)
      freeHeapTerm(arg);
    freeHeap(*term, sizeof(FunctorDef) + arity * sizeof(word));
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
unifyKey(Word key, word val)
{ if ( isAtom(val) || isInteger(val) )
    return unifyAtomic(key, val);

  return unifyFunctor(key, (FunctorDef) val);
}

word
getKey(register Word key)
{ if (isAtom(*key) || isInteger(*key))
    return *key;
  else if (isTerm(*key))
    return (word)functorTerm(*key);
  else
    return (word) NULL;
}

word
pl_current_key(Word k, word h)
{ RecordList l;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      l = recordTable[0];
      break;
    case FRG_REDO:
      l = (RecordList) ForeignContextAddress(h);
      break;
    case FRG_CUTTED:
    default:
      succeed;
  }

  for(; l; l = l->next)
  { while(isRef((word)l) )
    { l = *((RecordList *)unRef(l));
      if (l == (RecordList) NULL)
	fail;
    }
    if ( l->firstRecord == NULL || unifyKey(k, l->key) == FALSE )
      continue;

    return_next_table(RecordList, l);
  }

  fail;
}

static bool
record(Word key, Word term, Word ref, char az)
{ RecordList l;
  Record copy;
  word k;

  if ((k = getKey(key)) == (word) NULL)
    return warning("record%c/3: illegal key", az);

  l = lookupRecordList(k);
  copy = copyTermToHeap(term);
  copy->list = l;

  TRY(unifyAtomic(ref, pointerToNum(copy)));
  if (l->firstRecord == (Record) NULL)
  { copy->next = (Record) NULL;
    l->firstRecord = l->lastRecord = copy;
    succeed;
  }
  if (az == 'a')
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
pl_recorda(Word key, Word term, Word ref)
{ return record(key, term, ref, 'a');
}

word
pl_recordz(Word key, Word term, Word ref)
{ return record(key, term, ref, 'z');
}

word
pl_recorded(Word key, Word term, Word ref, word h)
{ RecordList rl;
  Record record;
  word k;
  mark m;
  word copy;

  DEBUG(5, printf("recorded: h=0x%lx, control = %d\n", h, ForeignControl(h)));
  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      if ( isInteger(*ref) )
      { record = (Record) numToPointer(*ref);

	if ( !inCore(record) || !isRecord(record) )
	  return warning("recorded/3: Invalid reference");
	
	Mark(m);
	if ( pl_unify(term, &record->term) )
	{ Undo(m);
	  copy = copyTermToGlobal(record);
	  TRY( unifyKey(key, record->list->key) );
	  return pl_unify(term, &copy);
	} else
	{ Undo(m);
	  fail;
	}
      }
      if ((k = getKey(key)) == (word) NULL)
	return warning("recorded/3: illegal key");
      if ((rl = isCurrentRecordList(k)) == (RecordList) NULL)
	fail;
      record = rl->firstRecord;
      break;
    case FRG_REDO:
      record = (Record) ForeignContextAddress(h);
      break;
    case FRG_CUTTED:
    default:
      succeed;
  }

  Mark(m);
  for(;record; record = record->next)
  { if (pl_unify(term, &record->term) )
    { Undo(m);
      TRY(unifyAtomic(ref, pointerToNum(record) ));
      copy = copyTermToGlobal(record);
      TRY(pl_unify(term, &copy) );

      if (record->next == (Record) NULL)
	succeed;
      else
	ForeignRedo(record->next);
    }
  }

  Undo(m);
  fail;
}

word
pl_erase(Word ref)
{ Record record;
  Record prev, r;
  RecordList l;

  if (!isInteger(*ref))
    return warning("erase/1: instantiation fault");

  record = (Record) numToPointer(*ref);
  
  if (!inCore(record))
    return warning("erase/1: Invalid reference");

  if (isClause(record))
  { Clause clause = (Clause) record;
  
    if ( true(clause->procedure->definition, SYSTEM) &&
	 false(clause->procedure->definition, DYNAMIC) )
      return warning("erase/1: Attempt to erase clause from system predicate");

    return retractClauseProcedure(clause->procedure, clause);
  }
  
  if (!isRecord(record))
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

  return warning("erase/1: illegal reference");
}
