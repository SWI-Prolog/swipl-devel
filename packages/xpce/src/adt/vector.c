/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

#define assignVector(v, n, o)	{ assignField((Instance)(v), \
					      &(v)->elements[n], \
					      (o)); \
				}
#define indexVector(v, e)	( valInt(e) - valInt(v->offset) - 1 )
#define validIndex(v, i)	{ if ( i < 0 || i >= valInt(v->size) ) fail; }

status
initialiseVectorv(Vector v, int argc, Any *argv)
{ int n;
  
  v->offset = ZERO;
  v->size = toInt(argc);
  v->elements = alloc(argc * sizeof(Any));
  
  for(n=0; n < argc; n++)
  { v->elements[n] = NIL;
    assignVector(v, n, argv[n]);
  }

  succeed;
}


static status
cloneVector(Vector v, Vector clone)
{ int n, size = valInt(v->size);

  clonePceSlots(v, clone);
  clone->elements = alloc(size * sizeof(Any));

  for( n=0; n<size; n++ )
  { clone->elements[n] = NIL;
    assignField((Instance) clone,
		&clone->elements[n],
		getClone2Object(v->elements[n]));
  }

  succeed;
}


Vector
createVectorv(int argc, Any *argv)
{ Vector v = alloc(sizeof(struct vector));

  initHeaderObj(v, ClassVector);
  v->offset      = (Int) NIL;
  v->size        = (Int) NIL;
  v->elements    = NULL;

  initialiseVectorv(v, argc, argv);
  createdObject(v, NAME_new);

  return v;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Load/store a vector on a file. Format:

<vector>		::= {<element>}

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
storeVector(Vector v, FileObj file)
{ int n;

  TRY(storeSlotsObject(v, file));
  for(n = 0; n < valInt(v->size); n++)
    TRY(storeObject(v->elements[n], file));

  succeed;
}
    
static status
loadVector(Vector v, FILE *fd, ClassDef def)
{ int n;
  Any obj;
  int size;

  loadSlotsObject(v, fd, def);
  size = valInt(v->size);
  v->elements = alloc(size * sizeof(Any));
  for(n = 0; n < size; n++)
  { TRY( obj = loadObject(fd) );
    v->elements[n] = NIL;
    assignVector(v, n, obj);
  }

  succeed;
}


static status
unlinkVector(Vector v)
{ if ( v->elements != NULL )
  { fillVector(v, NIL, DEFAULT, DEFAULT);

    unalloc(valInt(v->size)*sizeof(Any), v->elements);
    v->elements = NULL;
    assign(v, size, ZERO);
  }

  succeed;
}


static Int
getLowIndexVector(Vector v)
{ answer(add(v->offset, ONE));
}


static Int
getHighIndexVector(Vector v)
{ answer(add(v->size, v->offset));
}


Any
getTailVector(Vector v)
{ if ( v->size != ZERO )
    answer(v->elements[valInt(v->size)-1]);

  fail;
}


static Any
getHeadVector(Vector v)
{ if ( v->size != ZERO )
    answer(v->elements[0]);

  fail;
}


Vector
getCopyVector(Vector v)
{ Vector v2 = answerObjectv(classOfObject(v), valInt(v->size), v->elements);

  assign(v2, offset, v->offset);

  answer(v2);
}


status
fillVector(Vector v, Any obj, Int from, Int to)
{ int f, t;

  f = (isDefault(from) ? valInt(getLowIndexVector(v)) : valInt(from));
  t = (isDefault(to)   ? valInt(getHighIndexVector(v)) : valInt(to));

  if ( t < f )
    fail;

  if ( v->size == ZERO )
  { int size = t-f+1;
    int n;

    assign(v, offset, toInt(f - 1));
    assign(v, size,   toInt(size));
    unalloc(0, v->elements);
    v->elements = alloc(sizeof(Any) * size);
    for(n=0; n<size; n++)
    { v->elements[n] = NIL;
      if ( notNil(obj) )
	assignVector(v, n, obj);
    }
  } else
  { elementVector(v, toInt(f), obj);
    elementVector(v, toInt(t), obj);
    while( ++f < t )
      elementVector(v, toInt(f), obj);
  }

  succeed;
}


status
shiftVector(Vector v, Int places)
{ int n = valInt(v->size);
  int s = valInt(places);
  int i;
  
  if ( s > 0 )
  { for(i=n-s; i < n; i++)
      assignVector(v, i, NIL);
    for(i = n - 1; i >= s; i--)
      v->elements[i] = v->elements[i-s];
    for( ; i >= 0; i-- )
      v->elements[i] = NIL;
  } else
  { for(i = 0; i < -s; i++)
      assignVector(v, i, NIL);
    for(i = 0; i < n+s; i++)
      v->elements[i] = v->elements[i-s];
    for( ; i < n; i++ )
      v->elements[i] = NIL;
  }
    
  succeed;
}


Any
getElementVector(Vector v, Int e)
{ int n = indexVector(v, e);

  validIndex(v, n);

  answer(v->elements[n]);
}


status
elementVector(Vector v, Int e, Any obj)
{ int n = indexVector(v, e);

  if ( n < 0 )
  { Any *newElements = alloc((valInt(v->size)-n)*sizeof(Any));
    int m;

    memcpy(&newElements[-n], v->elements, valInt(v->size)*sizeof(Any));
    unalloc(valInt(v->size)*sizeof(Any), v->elements);
    v->elements = newElements;
    for( m = 0; m < -n; m++ )
      v->elements[m] = DEFAULT;
    assignVector(v, 0, obj);

    assign(v, size, toInt(valInt(v->size)-n));
    assign(v, offset, toInt(valInt(e)-1));

    succeed;
  }

  if ( n >= valInt(v->size) )
  { Any *newElements = alloc((n+1) * sizeof(Any));
    int m;

    memcpy(newElements, v->elements, valInt(v->size)*sizeof(Any));
    unalloc(valInt(v->size)*sizeof(Any), v->elements);
    v->elements = newElements;
    for( m = valInt(v->size); m <= n ; m++ )
      v->elements[m] = DEFAULT;
    assignVector(v, n, obj);

    assign(v, size, toInt(n+1));

    succeed;
  }

  assignVector(v, n, obj);

  succeed;
}


status
appendVector(Vector v, int argc, Any obj[])
{ if ( argc )
  { int start = valInt(v->size) + valInt(v->offset) + 1;
  
    fillVector(v, NIL, toInt(start), toInt(start + argc - 1));
    for( ; argc-- > 0; start++, obj++ )
      elementVector(v, toInt(start), *obj);
  }

  succeed;
}


static status
sortVector(Vector v, Code code)
{ if ( valInt(v->size) > 1 )
  { Code old = qsortCompareCode;		/* make reentrant */
    qsortCompareCode = code;

    qsort(v->elements, valInt(v->size), sizeof(Any), qsortCompareObjects);

    qsortCompareCode = old;
  }

  succeed;
}


static status
swapVector(Vector v, Int e1, Int e2)
{ int n1 = indexVector(v, e1);
  int n2 = indexVector(v, e2);
  Any tmp;

  validIndex(v, n1);
  validIndex(v, n2);

  tmp = v->elements[n1];		/* do not use assign() here: tmp */
  v->elements[n1] = v->elements[n2];	/* might drop out in that case (JW) */
  v->elements[n2] = tmp;

  succeed;
}


static status
forAllVector(Vector v, Code code)
{ int n;

  for(n = 0; n < valInt(v->size); n++)
    TRY( forwardCode(code, v->elements[n], toInt(n+1), 0) );

  succeed;
}

static status
forSomeVector(Vector v, Code code)
{ int n;

  for(n = 0; n < valInt(v->size); n++)
    forwardCode(code, v->elements[n], toInt(n+1), 0);

  succeed;
}


static Any
getFindVector(Vector v, Code code)
{ int n;

  for(n = 0; n < valInt(v->size); n++)
    if ( forwardCode(code, v->elements[n], toInt(n+1), 0) )
      answer(v->elements[n]);

  fail;
}


static Chain
getFindAllVector(Vector v, Code code)
{ Chain result = answerObject(ClassChain, 0);
  int n;

  for(n = 0; n < valInt(v->size); n++)
    if ( forwardCode(code, 0, v->elements[n], toInt(n+1), 0) )
      appendChain(result, v->elements[n]);

  answer(result);
}


static Int
getIndexVector(Vector v, Any obj)
{ int n;
  int size = valInt(v->size);

  for( n=0; n<size; n++)
    if ( v->elements[n] == obj )
      answer(toInt(n + valInt(v->offset) + 1));
  
  fail;
}


static Int
getRindexVector(Vector v, Any obj)
{ int n;
  int size = valInt(v->size);

  for( n=size-1; n>=0; n--)
    if ( v->elements[n] == obj )
      answer(toInt(n + valInt(v->offset) + 1));
  
  fail;
}

		/********************************
		*      TERM REPRESENTATION      *
		*********************************/

Any
getArgVector(Vector v, Int arg)
{ int n = valInt(arg) - 1;

  validIndex(v, n);

  answer(v->elements[n]);
}


Int
getArityVector(Vector v)
{ answer(v->size);
}


static status
changedVector(Vector v, Any *field)
{ if ( onFlag(v, F_INSPECT) )
  { Class class = classOfObject(v);

    if ( notNil(class->changed_messages) )
    { int index = field - v->elements;

      if ( index >= 0 && index < valInt(v->size) )
	return changedObject(v, toName((Any) toInt(index)), 0);

      return changedFieldObject(v, field);
    }
  }

  succeed;
}


status
makeClassVector(Class class)
{ sourceClass(class, makeClassVector, __FILE__, "$Revision$");

  localClass(class, NAME_offset, NAME_range, "int", NAME_get,
	     "Offset relative to 1-based");
  localClass(class, NAME_size, NAME_range, "int", NAME_get,
	     "Number of elements");
  localClass(class, NAME_elements, NAME_storage, "alien:Any *", NAME_none,
	     "The elements themselves");

  termClass(class, "vector", ARGC_UNKNOWN);

  setLoadStoreFunctionClass(class, loadVector, storeVector);
  setCloneFunctionClass(class, cloneVector);
  setChangedFunctionClass(class, changedVector);

  sendMethod(class, NAME_initialise, DEFAULT, 1, "element=any ...",
	     "Create vector with elements at 1, ...",
	     initialiseVectorv);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Dealocates -elements",
	     unlinkVector);
  sendMethod(class, NAME_fill, NAME_element, 3,
	     "value=any", "from=[int]", "to=[int]",
	     "Fill index range with one value",
	     fillVector);
  sendMethod(class, NAME_element, NAME_element, 2, "index=int", "value=any",
	     "Set specified element",
	     elementVector);
  sendMethod(class, NAME_append, NAME_list, 1, "value=any ...",
	     "Append element at <-high_index+1",
	     appendVector);
  sendMethod(class, NAME_swap, NAME_order, 2, "index_1=int", "index_2=int",
	     "Swap two elements",
	     swapVector);
  sendMethod(class, NAME_sort, NAME_order, 1, "compare=code",
	     "Sort according to code exit status",
	     sortVector);
  sendMethod(class, NAME_forAll, NAME_iterate, 1, "action=code",
	     "Run code on all elements; demand acceptance",
	     forAllVector);
  sendMethod(class, NAME_forSome, NAME_iterate, 1, "action=code",
	     "Run code on all elements",
	     forSomeVector);
  sendMethod(class, NAME_shift, NAME_range, 1, "places=int",
	     "Shift contents by n places",
	     shiftVector);

  getMethod(class, NAME_element, NAME_element, "any", 1, "index=int",
	    "Get element at index",
	    getElementVector);
  getMethod(class, NAME_Arg, DEFAULT, "any", 1, "int",
	    "Get argument for term",
	    getArgVector);
  getMethod(class, NAME_Arity, DEFAULT, "int", 0,
	    "Get arity for term",
	    getArityVector);
  getMethod(class, NAME_tail, NAME_element, "any", 0,
	    "Last element (as class chain)",
	    getTailVector);
  getMethod(class, NAME_head, NAME_element, "any", 0,
	    "First element (as class chain)",
	    getHeadVector);
  getMethod(class, NAME_index, NAME_search, "int", 1, "any",
	    "Get first index holding argument",
	    getIndexVector);
  getMethod(class, NAME_rindex, NAME_search, "int", 1, "any",
	    "Get last index holding argument",
	    getRindexVector);
  getMethod(class, NAME_find, NAME_search, "unchecked", 1, "code",
	    "First element accepted by code",
	    getFindVector);
  getMethod(class, NAME_findAll, NAME_search, "unchecked", 1, "code",
	    "Chain of elements accepted by code",
	    getFindAllVector);
  getMethod(class, NAME_lowIndex, NAME_range, "int", 0,
	    "Get lowest valid index",
	    getLowIndexVector);
  getMethod(class, NAME_highIndex, NAME_range, "int", 0,
	    "Get highest valid index",
	    getHighIndexVector);
  getMethod(class, NAME_copy, NAME_copy, "vector", 0,
	    "Create a copy of a vector",
	    getCopyVector);

  succeed;
}

