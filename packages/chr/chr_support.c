#include <SWI-Prolog.h>
#include <stdlib.h>
#include <ctype.h>


/*
	lookup_ht(HT,Key,Values) :-
		term_hash(Key,Hash),
		HT = ht(Capacity,_,Table),
		Index is (Hash mod Capacity) + 1,
		arg(Index,Table,Bucket),
		nonvar(Bucket),
		( Bucket = K-Vs ->
		    K == Key,	
		    Values = Vs
		;
		    lookup(Bucket,Key,Values)
		).

	lookup([K - V | KVs],Key,Value) :-
		( K = Key ->
			V = Value
		;
			lookup(KVs,Key,Value)
		).
*/
foreign_t
pl_lookup_ht1(term_t ht, term_t pl_hash, term_t key, term_t values)
{
  int capacity;
  int hash;
  int index;

  term_t pl_capacity = PL_new_term_ref();
  term_t table       = PL_new_term_ref();
  term_t bucket      = PL_new_term_ref();

  /* HT = ht(Capacity,_,Table) */
  _PL_get_arg(1, ht, pl_capacity);
  PL_get_integer(pl_capacity, &capacity);
  _PL_get_arg(3, ht, table);

  /* Index is (Hash mod Capacity) + 1 */
  PL_get_integer(pl_hash, &hash);
  index = (hash % capacity) + 1;  

  /* arg(Index,Table,Bucket) */
  _PL_get_arg(index, table, bucket);

  /* nonvar(Bucket) */ 
  if (PL_is_variable(bucket)) PL_fail;  

  if (PL_is_list(bucket)) {
  	term_t pair	     = PL_new_term_ref();
  	term_t k	     = PL_new_term_ref();
	term_t vs	     = PL_new_term_ref();
	while (PL_get_list(bucket, pair,bucket)) {
  		_PL_get_arg(1, pair, k);
		if ( PL_compare(k,key) == 0 ) {
      			/* Values = Vs */
			_PL_get_arg(2, pair, vs);
			return PL_unify(values,vs);
		}
	}
	PL_fail;
  } else {
  	term_t k	     = PL_new_term_ref();
	term_t vs	     = PL_new_term_ref();
  	_PL_get_arg(1, bucket, k);
        /* K == Key */	
	if ( PL_compare(k,key) == 0 ) {
      		/* Values = Vs */
		_PL_get_arg(2, bucket, vs);
		return PL_unify(values,vs);
	} else {
		PL_fail;
	}
  }
}

foreign_t
pl_memberchk_eq(term_t element, term_t maybe_list)
{

  term_t head = PL_new_term_ref();      	/* variable for the elements */
  term_t list = PL_copy_term_ref(maybe_list);   /* copy as we need to write */

  while( PL_get_list(list, head, list) )
  { if ( PL_compare(element,head) == 0 )
     PL_succeed ;
  }

  PL_fail;  

}

	/* INSTALL */

install_t
install_chr_support()
{
  PL_register_foreign("memberchk_eq",2, pl_memberchk_eq, 0);
  PL_register_foreign("lookup_ht1",4, pl_lookup_ht1, 0);
}

