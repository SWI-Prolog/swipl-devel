/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#include "pl-incl.h"

word
pl_is_list(term_t list)
{ Word p = valTermRef(list);

  deRef(p);

  if ( isList(*p) || isNil(*p) )
    succeed;

  fail;
}


word
pl_proper_list(term_t list)
{ if ( lengthList(list, FALSE) >= 0 )
    succeed;

  fail;
}


word
pl_length(term_t list, term_t l)
{ int n;

  if ( PL_get_integer(l, &n) )
  { if ( n >= 0 )
    { term_t h = PL_new_term_ref();
      term_t l = PL_copy_term_ref(list);

      while( n-- > 0 )
      { TRY(PL_unify_list(l, h, l));
      }

      return PL_unify_nil(l);
    }
    fail;
  }

  if ( PL_is_variable(l) )
  { long n;
  
    if ( (n=lengthList(list, FALSE)) >= 0 )
      return PL_unify_integer(l, n);

    fail;			/* both variables: generate in Prolog */
  }
  
  return PL_error("length", 2, NULL, ERR_TYPE, ATOM_integer, l);
}  


word
pl_memberchk(term_t e, term_t list)
{ term_t h = PL_new_term_ref();
  term_t l = PL_copy_term_ref(list);

  for(;;)
  { TRY(PL_unify_list(l, h, l));
      
    if ( PL_unify(e, h) )
      succeed;
  }
}


static int
qsort_compare_standard(const void *p1, const void *p2)
{ return compareStandard((Word) p1, (Word) p2);
}


static term_t
list_to_sorted_array(term_t List, int *size)
{ int n = lengthList(List, TRUE);
  term_t rval;
  term_t list = PL_copy_term_ref(List);
  term_t head = PL_new_term_ref();
  int i;

  if ( n < 0 )
    fail;			/* not a proper list */
  rval = PL_new_term_refs(n);
  
  for(i=0; PL_get_list(list, head, list); i++)
    PL_put_term(rval+i, head);

  qsort(valTermRef(rval), n, sizeof(word), qsort_compare_standard);
  
  *size = n;
  return rval;
}


word
pl_msort(term_t list, term_t sorted)
{ term_t array;
  term_t l = PL_copy_term_ref(sorted);
  term_t h = PL_new_term_ref();
  int n, i;

  if ( !(array = list_to_sorted_array(list, &n)) )
    fail;
  for(i=0; i < n; i++)
  { if ( !PL_unify_list(l, h, l) ||
	 !PL_unify(h, array+i) )
      fail;
  }

  return PL_unify_nil(l);
}


word
pl_sort(term_t list, term_t sorted)
{ term_t array;
  term_t l = PL_copy_term_ref(sorted);
  term_t h = PL_new_term_ref();
  int n, size;

  if ( !(array=list_to_sorted_array(list, &size)) )
    fail;
  for(n = 0; n < size; n++)
  { if ( n == 0 || !pl_equal(array+n-1, array+n) )
    { if ( !PL_unify_list(l, h, l) ||
	   !PL_unify(h, array+n) )
	fail;
    }
  }

  return PL_unify_nil(l);
}
