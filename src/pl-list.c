/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: List manipulation predicates in C
*/

#include "pl-incl.h"

word
pl_is_list(list)
Word list;
{ if ( isList(*list) || isNil(*list) )
    succeed;

  fail;
}

word
pl_proper_list(list)
Word list;
{ if ( lengthList(list) >= 0 )
    succeed;

  fail;
}

word
pl_length(list, l)
Word list, l;
{ if ( isInteger(*l) )
  { long n = valNum(*l);
    if ( n < 0 )
      fail;
    while( n-- > 0 )
    { TRY( unifyFunctor(list, FUNCTOR_dot2) );
      list = TailList(list); deRef(list);
    }
    CLOSELIST(list);
    succeed;
  }

  if ( isVar(*l) )
  { long n;
  
    if ( (n=lengthList(list)) >= 0 )
      return unifyAtomic(l, consNum(n));
    fail;			/* both variables: generate in Prolog */
  }
  
  return warning("length/2: instantiation fault");
}  


word
pl_memberchk(e, list)
Word e, list;
{ for(;;)
  { TRY( unifyFunctor(list, FUNCTOR_dot2) );
    if ( pl_unify(e, HeadList(list)) == TRUE )
      succeed;
    list = TailList(list);
    deRef(list);
  }
}

forwards int	qsort_compare_standard P((const void *, const void *));
forwards Word	*list_to_sorted_array P((Word, int *));

#ifdef __STDC__
static int
qsort_compare_standard(p1, p2)
const void *p1, *p2;
{ return compareStandard(*((Word *)p1), *((Word *)p2));
}

#else

static int
qsort_compare_standard(p1, p2)
Word *p1, *p2;
{ return compareStandard(*p1, *p2);
}
#endif

static Word *
list_to_sorted_array(list, size)
Word list;
int *size;
{ int n = lengthList(list);
  Word *array, *a;

  if ( n < 0 )
    fail;			/* not a proper list */
  initAllocLocal();
  array = (Word *)allocLocal(n * sizeof(Word));
  stopAllocLocal();
  for(a=array; isList(*list); a++)
  { *a = HeadList(list);
    deRef(*a);
    list = TailList(list);
    deRef(list);
  }
  SECURE(if (!isNil(*list)) sysError("list_to_sorted_array()"));
  qsort(array, n, sizeof(Word), qsort_compare_standard);
  
  *size = n;
  return array;
}


word
pl_msort(list, sorted)
Word list, sorted;
{ Word *array;
  int n;

  if ( (array=list_to_sorted_array(list, &n)) == (Word *) NULL )
    return warning("msort/1: first argument is not a proper list");
  for(; n > 0; n--, array++)
    APPENDLIST(sorted, *array);
  CLOSELIST(sorted);
  
  succeed;
}


word
pl_sort(list, sorted)
Word list, sorted;
{ Word *array;
  int n, size;

  if ( (array=list_to_sorted_array(list, &size)) == (Word *) NULL )
    return warning("sort/1: first argument is not a proper list");
  for(n = 0; n < size; n++, array++)
  { if ( n == 0 || compareStandard(array[-1], array[0]) != 0 )
      APPENDLIST(sorted, *array);
  }
  CLOSELIST(sorted);
  
  succeed;
}
