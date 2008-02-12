#include <SWI-Prolog.h>
#include <stdlib.h>
#include <ctype.h>

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
install()
{
  PL_register_foreign("memberchk_eq",2, pl_memberchk_eq, 0);
}
