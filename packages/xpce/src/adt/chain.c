/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include "../unx/proto.h"		/* file operations */

forwards Cell	newCell P((Chain, Any));
forwards Cell	previousCell P((Chain, Cell));
static status	deleteCurrentChain(Chain ch);
static Int	getCellIndexChain(Chain ch, Cell c);

#define ChangedChain(ch, op, ctx) \
	if ( onFlag(ch, F_INSPECT) && notNil(ClassChain->changed_messages) ) \
	  changedObject(ch, op, ctx, 0)

/* (JW)	Class chain is not a truely object oriented class as its internal
	representation as cell is no class.  assign() is used such that
	reference counts to other objects are kept, most internal 
	assignments are done with '='.
 */

static Cell
newCell(Chain ch, register Any value)
{ Cell cell;

  cell = alloc(sizeof(struct cell));
  cell->value = NIL;
  cell->next  = NIL;
  assignField((Instance) ch, &cell->value, value);

  return cell;
}


static void
freeCell(Chain ch, Cell cell)
{ assignField((Instance) ch, &cell->value, NIL);

  unalloc(sizeof(struct cell), cell);
}


static Cell
previousCell(Chain ch, register Cell next)
{ register Cell cell;

  for_cell(cell, ch)
    if (cell->next == next)
      return cell;
  fail;
}


status
initialiseChainv(Chain ch, int argc, Any *argv)
{ int i;

  assign(ch, size, ZERO);
  ch->current = ch->head = ch->tail = NIL;
  for(i=0; i<argc; i++)
    appendChain(ch, argv[i]);
  ch->current = ch->tail;

  succeed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Load/store a chain on a file. Format:

<chain>		::= {<cell>} 'X'

<cell>		::= 'e' <object>	(cell holding <object>)
		  | 'E' <object>	(`current' cell holding <object>)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
storeChain(Chain ch, FileObj file)
{ Cell cell;

  TRY(storeSlotsObject(ch, file));
  for_cell(cell, ch)
  { storeCharFile(file, cell == ch->current ? 'E' : 'e');
    TRY( storeObject(cell->value, file) );
  }
  storeCharFile(file, 'X');

  succeed;
}
    
static status
loadChain(Chain ch, FILE *fd, ClassDef def)
{ Any obj;
  Cell current;
  char c;

  if ( restoreVersion != 2 )
    TRY(loadSlotsObject(ch, fd, def));

  current = ch->current = ch->head = ch->tail = NIL;
  assign(ch, size, ZERO);

  for(;;)
    switch( c=getc(fd) )
    { case 'e':
      case 'E':
	  TRY( obj=loadObject(fd) );
	  appendChain(ch, obj);
	  if ( c == 'E' )
	    current = ch->tail;
	  continue;
      case 'X':
	  ch->current = current;
	  succeed;
      default:
	  errorPce(LoadFile, NAME_illegalCharacter,
		   toInt(c), toInt(ftell(fd)));
    }
} 


static status
unlinkChain(Chain ch)
{ return clearChain(ch);
}


static status
cloneChain(Chain ch, Chain clone)
{ Cell cell;

  clonePceSlots(ch, clone);
  clone->current = clone->head = clone->tail = NIL;

  for_cell(cell, ch)
  { appendChain(clone, getClone2Object(cell->value));
    if ( ch->current == cell )
      clone->current = clone->tail;
  }

  assign(clone, size, ch->size);

  succeed;
}


status
clearChain(Chain ch)
{ register Cell p, q;

  for_cell_save(p, q, ch)
    freeCell(ch, p);
  ch->head = ch->tail = ch->current = NIL;

  assign(ch, size, ZERO);
  ChangedChain(ch, NAME_clear, 0);

  succeed;
}


Int
getSizeChain(Chain ch)
{ answer(ch->size);
}


status
prependChain(register Chain ch, Any obj)
{ Cell cell;

  cell = newCell(ch, obj);

  if (isNil(ch->head))
    ch->head = ch->tail = cell;
  else
    cell->next = ch->head, 
    ch->head = cell;

  assign(ch, size, inc(ch->size));
  ChangedChain(ch, NAME_insert, ONE);

  succeed;
}


status
appendChain(register Chain ch, Any obj)
{ Cell cell;

  cell = newCell(ch, obj);

  if (isNil(ch->head))
    ch->head = ch->tail = cell;
  else
    ch->tail->next = cell, 
    ch->tail = cell;

  assign(ch, size, inc(ch->size));
  ChangedChain(ch, NAME_insert, getSizeChain(ch));

  succeed;
}


status
addChain(register Chain ch, Any obj)
{ if ( memberChain(ch, obj) != SUCCEED )
    return prependChain(ch, obj);

  succeed;
}


static status
insertChain(Chain ch, Any obj)
{ Cell cell, prev, current = ch->current;

  if ( current == ch->head )
    return prependChain(ch, obj);
  if ( isNil(current) )
    return appendChain(ch, obj);

  cell = newCell(ch, obj);
  prev = previousCell(ch, current);
  prev->next = cell;
  cell->next = current;
  ch->current = cell;

  assign(ch, size, inc(ch->size));
  ChangedChain(ch, NAME_insert, getCellIndexChain(ch, cell));

  succeed;
}


status
insertAfterChain(Chain ch, Any obj, Any obj2)
{ int i = 1;
  Cell cell;

  if ( isNil(obj2) )
    return prependChain(ch, obj);

  for_cell(cell, ch)
  { if ( cell->value == obj2 )
    { if ( ch->tail == cell )
      { return appendChain(ch, obj);
      } else
      { Cell c2 = newCell(ch, obj);
      
	c2->next = cell->next;
	cell->next = c2;
	assign(ch, size, inc(ch->size));
	ChangedChain(ch, NAME_insert, toInt(i+1));

	succeed;
      }
    }

    i++;
  }

  fail;
}


status
swapChain(Chain ch, Any obj1, Any obj2)
{ Cell cell;

  TRY( memberChain(ch, obj1) );
  cell = ch->current;
  TRY( memberChain(ch, obj2) );

  ch->current->value = obj1;
  cell->value = obj2;

  ChangedChain(ch, NAME_cell, getCellIndexChain(ch, ch->current));
  ChangedChain(ch, NAME_cell, getCellIndexChain(ch, cell));

  succeed;
}


status
deleteHeadChain(Chain ch)
{ EXISTS(ch->head);
  ch->current = ch->head;

  return deleteCurrentChain(ch);
}


static status
deleteTailChain(Chain ch)
{ EXISTS(ch->tail);
  ch->current = ch->tail;

  return deleteCurrentChain(ch);
}


status
deleteChain(Chain ch, register Any obj)
{ register Cell cell, p;
  int i;

  EXISTS(ch->head);

  if ( notNil(ch->current) && ch->current->value == obj )
    ch->current = NIL;

  if (ch->head == ch->tail)
  { if (ch->head->value != obj)
      fail;
    freeCell(ch, ch->head);
    ch->head = ch->tail = NIL;
    assign(ch, size, ZERO);
    ChangedChain(ch, NAME_clear, 0);
    succeed;
  }

  if (ch->head->value == obj)
  { Cell next = ch->head->next;
    freeCell(ch, ch->head);
    ch->head = next;
    assign(ch, size, dec(ch->size));
    ChangedChain(ch, NAME_delete, ONE);
    succeed;
  }

  for(p=ch->head, cell=p->next, i=2;
      notNil(cell);
      p=cell, cell=cell->next, i++)
  { if (cell->value == obj)
    { p->next = cell->next;
      if (cell == ch->tail)
	ch->tail = p;
      freeCell(ch, cell);
      assign(ch, size, dec(ch->size));
      ChangedChain(ch, NAME_delete, toInt(i));
      succeed;
    }
  }

  fail;
}


static status
deleteCurrentChain(Chain ch)
{ if ( notNil(ch->current) )
    return deleteCellChain(ch, ch->current);

  succeed;
}


status
deleteCellChain(Chain ch, Cell cell)
{ Cell prev;
  Int i = ONE;

  if ( cell == ch->head  && ch->head == ch->tail )
  { freeCell(ch, ch->head);
    ch->head = ch->tail = ch->current = NIL;
    ChangedChain(ch, NAME_clear, 0);
    assign(ch, size, ZERO);

    succeed;
  }

  if (cell == ch->head)
  { ch->head = cell->next;
  } else
  { if ( notNil(ClassChain->changed_messages) )
      i = getCellIndexChain(ch, cell);
    prev = previousCell(ch, cell);
    prev->next = cell->next;
    if (cell == ch->tail)
      ch->tail = prev;
  }
  ch->current = NIL;
  freeCell(ch, cell);
  assign(ch, size, dec(ch->size));
  ChangedChain(ch, NAME_delete, i);

  succeed;
}


static status
deleteAllChain(Chain ch, Any obj)
{ while( deleteChain(ch, obj) != FAIL ) ;	 /* can be more efficient */

  succeed;
}


status
memberChain(Chain ch, Any obj)
{ register Cell cell;

  for_cell(cell, ch)
  { if (cell->value == obj)
    { ch->current = cell;
      succeed;
    }
  }
  fail;
}


static status
currentChain(Chain ch, Any obj)
{ if (isNil(obj))
  { ch->current = NIL;
    succeed;
  }
  return memberChain(ch, obj);
}


static status
currentNoChain(Chain ch, Int index)
{ register Cell cell;
  register int i = valInt(index);

  if (i == 0)
  { ch->current = NIL;
    succeed;
  }

  for_cell(cell, ch)
  { if (--i < 1)
    { ch->current = cell;
      succeed;
    }
  }
  fail;
}


static Int
getCurrentNoChain(Chain ch)
{ register Cell cell;
  register int n;

  if (isNil(ch->current))
    n = 0;
  else
    for(n=1, cell=ch->head; cell != ch->current; cell=cell->next)
      n++;
  answer(toInt(n));
}


static Any
getCurrentChain(Chain ch)
{ EXISTS(ch->current);
  answer(ch->current->value);
}


static Any
getNextChain(Chain ch)
{ Any result;

  EXISTS(ch->current);
  result = ch->current->value;
  ch->current = ch->current->next;

  answer(result);
}


status
forAllChain(Chain ch, Code code, Bool safe)
{ if ( safe == OFF )
  { Cell cell;

    for_cell(cell, ch)
      TRY(forwardCodev(code, 1, &cell->value));
  } else
  { Any obj;

    for_chain(ch, obj, TRY(forwardCodev(code, 1, &obj)));
  }

  succeed;
}


status
forSomeChain(Chain ch, Code code, Bool safe)
{ if ( safe == OFF )
  { Cell cell;

    for_cell(cell, ch)
      forwardCodev(code, 1, &cell->value);
  } else
  { Any obj;

    for_chain(ch, obj, forwardCodev(code, 1, &obj));
  }

  succeed;
}


Any
getFindChain(Chain ch, Code code)
{ Cell cell;

  for_cell(cell, ch)
  { if ( forwardCodev(code, 1, &cell->value) )
      answer(cell->value);
  }

  fail;
}


Chain
getFindAllChain(Chain ch, Code code)
{ Chain result = answerObject(ClassChain, 0);
  Cell cell;

  for_cell(cell, ch)
  { if (forwardCodev(code, 1, &cell->value) != FAIL)
      appendChain(result, cell->value);
  }

  answer(result);
}


static Chain
getMapChain(Chain ch, Function f)
{ Any obj;
  Chain result = answerObject(ClassChain, 0);
  Any rval;

  for_chain(ch, obj,
	    if ( (rval = getForwardFunctionv(f, 1, &obj)) )
	      appendChain(result, rval));

  answer(result);
}


static status
findChain(Chain ch, Code code)
{ Cell cell;

  for_cell(cell, ch)
  { if ( forwardCodev(code, 1, &cell->value) != FAIL)
    { ch->current = cell;
      succeed;
    }
  }
  fail;
}


status
mergeChain(Chain ch, Chain ch2)
{ register Cell cell;
  register Cell tail = ch->tail;

  for_cell(cell, ch2)
  { appendChain(ch, cell->value);
    if ( cell == tail )			/* @ch ->merge @ch */
      break;
  }
  
  succeed;
}


static status
unionChain(Chain ch, Chain ch2)
{ register Cell cell;

  for_cell(cell, ch2)
  { if (memberChain(ch, cell->value) != FAIL)
      continue;
    appendChain(ch, cell->value);
  }
  succeed;
}


static status
intersectionChain(Chain ch, Chain ch2)
{ register Cell cell, c2;

  for_cell_save(cell, c2, ch)
  { if ( memberChain(ch2, cell->value) == FAIL )
      deleteCellChain(ch, cell);
  }
  succeed;
}


static status
subtractChain(Chain ch, Chain ch2)
{ Cell cell, c2;

  for_cell_save(cell, c2, ch)
  { if ( memberChain(ch2, cell->value) != FAIL )
      deleteCellChain(ch, cell);
  }
  succeed;

}


status
replaceChain(Chain ch, Any obj1, Any obj2)
{ Cell cell;

  for_cell(cell, ch)
  { if ( cell->value == obj1 )
      cellValueChain(ch, PointerToInt(cell), obj2);
  }

  succeed;
}


static status
intersectsChain(Chain ch, Chain ch2)
{ Cell cell;

  for_cell(cell, ch)
  { if ( memberChain(ch2, cell->value) == SUCCEED )
      succeed;
  }

  fail;
}


static status
equalChain(Chain ch, Chain ch2)
{ Cell c1, c2;

  if ( !instanceOfObject(ch2, ClassChain) )
    fail;

  for(c1 = ch->head, c2 = ch2->head;
      notNil(c1) && notNil(c2);
      c1 = c1->next, c2 = c2->next)
  { if ( c1->value != c2->value )
      fail;
  }

  if ( c1 == c2 )			/* should both be NIL */
    succeed;

  fail;
}


status
emptyChain(Chain ch)
{ if ( isNil(ch) || isNil(ch->head) )
    succeed;

  fail;
}


Chain
getCopyChain(Chain ch)
{ if ( notNil(ch) )
  { Chain r = answerObject(classOfObject(ch), 0); /* Same class */
    Cell cell;

    for_cell(cell, ch)
      appendChain(r, cell->value);

    answer(r);
  }

  answer(NIL);
}


static Chain
getMergeChain(Chain ch, Chain ch2)
{ register Cell cell;
  Chain r;

  r = answerObject(ClassChain, 0);

  for_cell(cell, ch)
    appendChain(r, cell->value);
  for_cell(cell, ch2)
    appendChain(r, cell->value);
  
  answer(r);
}


static Chain
getUnionChain(Chain ch, Chain ch2)
{ register Cell cell;
  Chain r;

  r = answerObject(classOfObject(ch), 0);

  for_cell(cell, ch)
  { if (memberChain(r, cell->value) != FAIL)
      continue;
    appendChain(r, cell->value);
  }

  for_cell(cell, ch2)
  { if (memberChain(r, cell->value) != FAIL)
      continue;
    appendChain(r, cell->value);
  }
  
  answer(r);
}


static int
forwardCompareCode(Code c, Any o1, Any o2)
{ Any argv[2];

  argv[0] = o1;
  argv[1] = o2;

  if ( isFunction(c) )
  { Any r;

    withArgs(2, argv, r = getExecuteFunction((Function)c));

    if ( equalName(r, NAME_smaller) || (isInteger(r) && valInt(r) < 0) )
      return -1;
    else if ( r == NAME_equal || r == ZERO )
      return 0;
    else
      return 1;
  } else
  { status r;

    withArgs(2, argv, r = executeCode(c));

    return r ? -1 : 1;
  }
}


int
qsortCompareObjects(const void *o1, const void *o2)
{ int rval = forwardCompareCode(qsortCompareCode, *((Any *) o1), *((Any *)o2));

  DEBUG(NAME_sort, printf("compare %s %s --> %d\n",
			  pp(*((Any *)o1)), pp(*((Any *)o2)), rval));

  return qsortReverse ? -rval : rval;
}


status
sortChain(Chain ch, Code msg)
{ if ( isDefault(msg) )
    return sortNamesChain(ch);
  else
  { int size = valInt(ch->size);
    Any *buf = alloca(sizeof(Any) * size);
    Cell cell;
    int i;
    Code old = qsortCompareCode;		/* make reentrant */

    qsortCompareCode = msg;

    i = 0;
    for_cell(cell, ch)
    { buf[i] = cell->value;
      if ( isObject(buf[i]) )
	addRefObj(buf[i]);
      i++;
    }
    qsort(buf, size, sizeof(Any), qsortCompareObjects);
    clearChain(ch);
    for(i=0; i<size; i++)
    { appendChain(ch, buf[i]);
      if ( isObject(buf[i]) )
	delRefObj(buf[i]);
    }

    qsortCompareCode = old;
    succeed;
  }
}

typedef struct
{ CharArray	name;			/* name of object */
  Any		object;			/* the object */
} scell, *Scell;


static int
compare_names(const void *p1, const void *p2)
{ Scell s1 = (Scell)p1;
  Scell s2 = (Scell)p2;

  return str_cmp(&s1->name->data, &s2->name->data);
}


status
sortNamesChain(Chain ch)
{ int size = valInt(ch->size);
  Scell buf = alloca(sizeof(scell) * size);
  Cell cell;
  int i;

  i = 0;
  for_cell(cell, ch)
  { buf[i].object = cell->value;
    if ( isObject(buf[i].object) ) addRefObj(buf[i].object);
    if ( instanceOfObject(cell->value, ClassCharArray) )
      buf[i].name = cell->value;
    else
      buf[i].name = getv(cell->value, NAME_printName, 0, NULL);

    i++;
  }
  qsort(buf, size, sizeof(scell), compare_names);
  clearChain(ch);
  for(i=0; i<size; i++)
  { appendChain(ch, buf[i].object);
    if ( isObject(buf[i].object) ) delRefObj(buf[i].object);
    doneObject(buf[i].name);
  }

  succeed;
}


Tuple
getCompleteNameChain(Chain ch, CharArray prefix, Function map,
		     Bool ignore_case)
{ Chain matches = NIL;
  LocalString(common, &prefix->data, LINESIZE);
  Cell cell;

  for_cell(cell, ch)
  { Any obj = cell->value;
    string prt;
    status rval;
					/* get printable representation */
    if ( isNil(map) )
      rval = toString(obj, &prt);
    else if ( isDefault(map) )
      rval = toString(getv(obj, NAME_printName, 0, NULL), &prt);
    else
      rval = toString(getForwardFunctionv(map, 1, &obj), &prt);

    if ( rval )
    { if ( ((ignore_case == ON && str_icase_prefix(&prt, &prefix->data)) ||
	    (ignore_case != ON && str_prefix(&prt, &prefix->data))) &&
	   prt.size < LINESIZE ) /* hit */
      { if ( isNil(matches) )
	{ matches = answerObject(ClassChain, obj, 0);
	  str_cpy(common, &prt);
	} else
	{ common->size = str_common_length(&prt, common);

	  appendChain(matches, obj);
	}
      }
    } else
    { errorPce(obj, NAME_noPrintName);
      fail;
    }
  }

  if ( notNil(matches) )
    answer(answerObject(ClassTuple, matches, StringToString(common), 0));
  else
    fail;
}


Chain
getIntersectionChain(Chain ch, Chain ch2)
{ register Cell cell;
  Chain r;
  
  r = answerObject(classOfObject(ch), 0);
  
  for_cell(cell, ch)
  { if (memberChain(ch2, cell->value) != FAIL)
      appendChain(r, cell->value);
  }
  
  answer(r);
}


Any
getHeadChain(Chain ch)
{ Any result;
  
  EXISTS(ch->head);
  result = ch->head->value;
  ch->current = ch->head;
  
  answer(result);
}


Any
getDeleteHeadChain(Chain ch)
{ Any result;
  
  EXISTS(ch->head);
  result = ch->head->value;
  ch->current = ch->head;
  if ( isObject(result) && !isProtectedObj(result) )
  { addCodeReference(result);
    deleteCurrentChain(ch);
    delCodeReference(result);
    pushAnswerObject(result);
  } else
    deleteCurrentChain(ch);

  answer(result);
}


Any
getTailChain(Chain ch)
{ Any result;
  
  EXISTS(ch->tail);
  result = ch->tail->value;
  ch->current = ch->tail;
  
  answer(result);
}


static status
uniqueChain(Chain ch)
{ Cell cell, cell2;
  
  for_cell(cell, ch)
  { for (cell2=ch->head; cell2 != cell; cell2=cell2->next)
    { if (cell2->value == cell->value)
      { deleteChain(ch, cell->value);
	break;
      }
    }
  }
  succeed;
}


status
moveBeforeChain(Chain ch, Any obj1, Any obj2)
{ Cell cell;
  
  if ( obj1 == obj2 )
    fail;
  
  TRY( memberChain(ch, obj2) );
  cell = ch->current;
  addCodeReference(obj1);
  if ( deleteChain(ch, obj1) != SUCCEED )
  { delCodeReference(obj1);
    fail;
  }
  ch->current = cell;
  insertChain(ch, obj1);
  delCodeReference(obj1);
  
  succeed;
}


status
moveAfterChain(Chain ch, Any obj1, Any obj2)
{ Cell cell;
  int is_obj = isObject(obj1);
  status rval;
  
  if ( notDefault(obj2) && notNil(obj2) )
  { if ( obj1 == obj2 || memberChain(ch, obj2) == FAIL )
      fail;
    cell = ch->current->next;
    if ( notNil(cell) && cell->value == obj1 )
      succeed;				/* already true */
  } else
  { if ( obj1 == getHeadChain(ch) )
      fail;
    cell = ch->head;
  }
  
  if ( is_obj ) 
    addCodeReference(obj1);
  
  if ( deleteChain(ch, obj1) != FAIL )
  { ch->current = cell;
    insertChain(ch, obj1);
    
    rval = SUCCEED;
  } else
    rval = FAIL;
  
  if ( is_obj )
    delCodeReference(obj1);
  
  return rval;
}


status
beforeChain(Chain ch, Any obj1, Any obj2)
{ Cell cell;
  int i1 = 0, i2 = 0, i=1;

  for_cell(cell, ch)
  { if ( cell->value == obj1 )
      i1 = i;
    if ( cell->value == obj2 )
      i2 = i;
    
    if ( i1 && i2 )
      return (i1 < i2) ? SUCCEED : FAIL;
    
    i++;
  }

  return errorPce(NAME_noMember, !i1 ? obj1 : obj2);
}


static status
afterChain(Chain ch, Any obj1, Any obj2)
{ Cell cell;
  int i1 = 0, i2 = 0, i=1;

  for_cell(cell, ch)
  { if ( cell->value == obj1 )
      i1 = i;
    if ( cell->value == obj2 )
      i2 = i;
    
    if ( i1 && i2 )
      return (i1 > i2) ? SUCCEED : FAIL;

    i++;
  }

  return errorPce(NAME_noMember, !i1 ? obj1 : obj2);
}


Any
getNth1Chain(Chain ch, Int index)
{ register Cell cell;
  register int n = valInt(index);
  
  for_cell(cell, ch)
  { if (--n == 0)
      answer(cell->value);
  }
  
  fail;
}


static status
nth1Chain(Chain ch, Int index, Any value)
{ register Cell cell;
  register int n = valInt(index);
  
  for_cell(cell, ch)
  { if (--n == 0)
      return cellValueChain(ch, PointerToInt(cell), value);
  }
  
  fail;
}


static Any
getNth0Chain(Chain ch, Int index)
{ register Cell cell;
  register int n = valInt(index);
  
  for_cell(cell, ch)
  { if (n-- == 0)
      answer(cell->value);
  }
  
  fail;
}


static status
nth0Chain(Chain ch, Int index, Any value)
{ register Cell cell;
  register int n = valInt(index);
  
  for_cell(cell, ch)
  { if (n-- == 0)
      return cellValueChain(ch, PointerToInt(cell), value);
  }
  
  fail;
}


static Int
getHeadCellChain(Chain ch)
{ if ( notNil(ch->head) )
    answer(PointerToInt(ch->head));
  fail;
}


static Int
getNextCellChain(Chain ch, Int c)
{ Cell cell = (Cell) IntToPointer(c);
  
  if ( notNil(cell->next) )
    answer(PointerToInt(cell->next));
  
  fail;
}


static Any
getCellValueChain(Chain ch, Int c)
{ Cell cell = (Cell) IntToPointer(c);

  answer(cell->value);
}


status
cellValueChain(Chain ch, Int c, Any obj)
{ Cell cell = (Cell) IntToPointer(c);
  
  if ( cell->value != obj )
  { assignField((Instance) ch, &cell->value, obj);
    ChangedChain(ch, NAME_cell, getCellIndexChain(ch, cell));
  }
  
  succeed;
}


Cell
getNth0CellChain(Chain ch, Int index)
{ register Cell cell;
  register int n = valInt(index);

  for_cell(cell, ch)
  { if ( n-- == 0 )
      return cell;
  }

  fail;
}


static Int
getCellIndexChain(Chain ch, Cell c)
{ int i = 1;
  Cell cell;
  
  for_cell(cell, ch)
  { if ( cell == c )
      answer(toInt(i));
    i++;
  }
  
  fail;
}


Int
getIndexChain(Chain ch, Any obj)
{ int n = 0;
  Cell cell;
  
  for_cell(cell, ch)
  { n++;
    if (cell->value == obj)
      answer(toInt(n));
  }
  
  fail;
}


Int
getArityChain(Chain ch)
{ answer(getSizeChain(ch));
}


Any
getArgChain(Chain ch, Int arg)
{ answer(getNth1Chain(ch, arg));
} 


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Trapping changes to chains.

The following elementary changes to a chain are recognised and forwarded:
      NAME_insert, Index:	Element is inserted at cell <Index>
      NAME_delete, Index:	Element at index <Index> is deleted
      NAME_cell,   Index:	Element at index <Index> changed value
      NAME_clear:		Chain has been cleared
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
changedChain(Chain ch, Any *field)
{ succeed;
}


status
makeClassChain(Class class)
{ sourceClass(class, makeClassChain, __FILE__, "$Revision$");
  
  localClass(class, NAME_size, NAME_cardinality, "int", NAME_get,
	     "Number of elements");
  localClass(class, NAME_head, NAME_internal, "alien:Cell", NAME_none,
	     "Pointer to first cell");
  localClass(class, NAME_tail, NAME_internal, "alien:Cell", NAME_none,
	     "Pointer to last cell");
  localClass(class, NAME_current, NAME_internal, "alien:Cell", NAME_none,
	     "Pointer to current cell");
  
  termClass(class, "chain", ARGC_UNKNOWN);

  setLoadStoreFunctionClass(class, loadChain, storeChain);
  setCloneFunctionClass(class, cloneChain);
  setChangedFunctionClass(class, changedChain);
  
  sendMethod(class, NAME_initialise, DEFAULT, 1, "member=any ...",
	     "Create a chain with initial elements",
	     initialiseChainv);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Clear the chain",
	     unlinkChain);
  sendMethod(class, NAME_after, NAME_order, 2, "first=any", "second=any",
	     "Test if first argument is after second",
	     afterChain);
  sendMethod(class, NAME_add, NAME_set, 1, "value=any",
	     "Prepend object if not already ->member",
	     addChain);
  sendMethod(class, NAME_append, NAME_list, 1, "value=any",
	     "Append argument to chain",
	     appendChain);
  sendMethod(class, NAME_Append, NAME_list, 1, "value=any|function",
	     "Append argument to chain (not expanding obtainers)",
	     appendChain);
  sendMethod(class, NAME_before, NAME_order, 2, "first=any", "second=any",
	     "Test if first argument is before second",
	     beforeChain);
  sendMethod(class, NAME_cellValue, NAME_cell, 2,
	     "cell_reference=int", "value=any",
	     "Change value of cell",
	     cellValueChain);
  sendMethod(class, NAME_nth0, NAME_index, 2, "index=int", "value=any",
	     "Change content of nth (0-based) cell",
	     nth0Chain);
  sendMethod(class, NAME_nth1, NAME_index, 2, "index=int", "value=any",
	     "Change content of nth (1-based) cell",
	     nth1Chain);
  sendMethod(class, NAME_clear, NAME_list, 0,
	     "Remove all elements from chain",
	     clearChain);
  sendMethod(class, NAME_current, NAME_current, 1, "value=any*",
	     "Make cell with `value' the current cell",
	     currentChain);
  sendMethod(class, NAME_currentNo, NAME_current, 1, "index=int",
	     "Set current cell to nth-1 (0: no current)",
	     currentNoChain);
  sendMethod(class, NAME_delete, NAME_list, 1, "value=any",
	     "Delete first occurence of argument",
	     deleteChain);
  sendMethod(class, NAME_deleteAll, NAME_list, 1, "value=any",
	     "Delete all occurences of argument",
	     deleteAllChain);
  sendMethod(class, NAME_deleteCurrent, NAME_current, 0,
	     "Delete current cell",
	     deleteCurrentChain);
  sendMethod(class, NAME_deleteHead, NAME_list, 0,
	     "Delete first element",
	     deleteHeadChain);
  sendMethod(class, NAME_deleteTail, NAME_list, 0,
	     "Delete last element",
	     deleteTailChain);
  sendMethod(class, NAME_find, NAME_current, 1, "test=code",
	     "Set current to first cell accepted by code",
	     findChain);
  sendMethod(class, NAME_forAll, NAME_iterate, 2, "action=code", "safe=[bool]",
	     "Run code on all elements, demand acceptance ([safe])",
	     forAllChain);
  sendMethod(class, NAME_forSome, NAME_iterate, 2, "action=code", "safe=[bool]",
	     "Run code on all elements ([safe])",
	     forSomeChain);
  sendMethod(class, NAME_insert, NAME_current, 1, "value=any",
	     "Insert argument before current",
	     insertChain);
  sendMethod(class, NAME_insertAfter, NAME_list, 2, "value=any", "after=any*",
	     "Insert first after second object (@nil: prepend)",
	     insertAfterChain);
  sendMethod(class, NAME_intersection, NAME_set, 1, "chain",
	     "Delete elements not in argument",
	     intersectionChain);
  sendMethod(class, NAME_intersects, NAME_set, 1, "chain",
	     "Test if both chains have a common member",
	     intersectsChain);
  sendMethod(class, NAME_subtract, NAME_set, 1, "chain",
	     "Delete all elements in argument",
	     subtractChain);
  sendMethod(class, NAME_member, NAME_set, 1, "value=any",
	     "Test if argument is an element",
	     memberChain);
  sendMethod(class, NAME_merge, NAME_list, 1, "chain",
	     "Append all elements from argument",
	     mergeChain);
  sendMethod(class, NAME_prepend, NAME_list, 1, "value=any",
	     "Add argument as first element",
	     prependChain);
  sendMethod(class, NAME_swap, NAME_order, 2, "value_1=any", "value_2=any",
	     "Swap position of arguments",
	     swapChain);
  sendMethod(class, NAME_union, NAME_set, 1, "chain",
	     "Append only new elements from argument",
	     unionChain);
  sendMethod(class, NAME_unique, NAME_set, 0,
	     "Remove all duplicates from chain",
	     uniqueChain);
  sendMethod(class, NAME_replace, NAME_list, 2, "old=any", "new=any",
	     "Replace all occurences",
	     replaceChain);
  sendMethod(class, NAME_empty, NAME_cardinality, 0,
	     "Test if chain has no elements",
	     emptyChain);
  sendMethod(class, NAME_equal, NAME_compare, 1, "[chain]*",
	     "Test if both chains have the same objects",
	     equalChain);
  sendMethod(class, NAME_moveAfter, NAME_order, 2, "value=any", "after=[any]",
	     "Move 1st object just after second",
	     moveAfterChain);
  sendMethod(class, NAME_moveBefore, NAME_order, 2, "value=any", "before=any",
	     "Move 1st object just before second",
	     moveBeforeChain);
  sendMethod(class, NAME_sort, NAME_order, 1, "compare=[code|function]",
	     "Sort according to code's return-value (or name)",
	     sortChain);
  
  getMethod(class, NAME_Arg, NAME_term, "any", 1, "index=int",
	    "Nth-1 argument for object description",
	    getArgChain);
  getMethod(class, NAME_Arity, NAME_term, "int", 0,
	    "Number of arguments for object description",
	    getArityChain);
  getMethod(class, NAME_cellValue, NAME_cell, "any", 1, "cell_reference=int",
	    "Value for cell-reference",
	    getCellValueChain);
  getMethod(class, NAME_copy, NAME_copy, "chain", 0,
	    "New chain with same elements",
	    getCopyChain);
  getMethod(class, NAME_current, NAME_current, "any", 0,
	    "Value for the current cell",
	    getCurrentChain);
  getMethod(class, NAME_currentNo, NAME_current, "int", 0,
	    "Index number of current cell (1-based)",
	    getCurrentNoChain);
  getMethod(class, NAME_map, NAME_iterate, "chain", 1, "function",
	    "New chain with result of applying function",
	    getMapChain);
  getMethod(class, NAME_find, NAME_iterate, "any", 1, "test=code",
	    "First element accepted by code",
	    getFindChain);
  getMethod(class, NAME_findAll, NAME_iterate, "chain", 1, "test=code",
	    "New chain with elements accepted by code",
	    getFindAllChain);
  getMethod(class, NAME_head, NAME_list, "any", 0,
	    "First element",
	    getHeadChain);
  getMethod(class, NAME_deleteHead, NAME_list, "any", 0,
	    "First element and delete it",
	    getDeleteHeadChain);
  getMethod(class, NAME_headCell, NAME_cell, "int", 0,
	    "Reference (int) to first cell",
	    getHeadCellChain);
  getMethod(class, NAME_intersection, NAME_set, "chain", 1, "chain",
	    "New chain holding common elements",
	    getIntersectionChain);
  getMethod(class, NAME_index, NAME_index, "index=int", 1, "value=any",
	    "Index (1-based) at which argument is",
	    getIndexChain);
  getMethod(class, NAME_merge, NAME_list, "chain", 1, "chain",
	    "New chain holding concatenation",
	    getMergeChain);
  getMethod(class, NAME_next, NAME_current, "any", 0,
	    "Get current and make current the next",
	    getNextChain);
  getMethod(class, NAME_nextCell, NAME_cell, "cell_reference=int",
	    1, "cell_reference=int",
	    "Reference to next cell at reference",
	    getNextCellChain);
  getMethod(class, NAME_nth1, NAME_index, "value=any", 1, "index=int",
	    "Element at 1-based index",
	    getNth1Chain);
  getMethod(class, NAME_nth0, NAME_index, "value=any", 1, "index=int",
	    "Element at 0-based index",
	    getNth0Chain);
  getMethod(class, NAME_tail, NAME_list, "value=any", 0,
	    "Last element",
	    getTailChain);
  getMethod(class, NAME_union, NAME_set, "chain", 1, "chain",
	    "New chain with union of elements",
	    getUnionChain);
  getMethod(class, NAME_completeName, NAME_completion, "tuple", 3,
	    "prefix=char_array", "extract_name=[function]*",
	    "ignore_case=[bool]",
	    "New tuple with matches and common prefix",
	    getCompleteNameChain);

  succeed;
}

