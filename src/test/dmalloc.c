#include <SWI-Prolog.h>
#include <dmalloc.h>

static foreign_t
pl_dmalloc_mark(term_t t)
{ unsigned long mark = dmalloc_mark();

  return PL_unify_int64(t, mark);
}


static foreign_t
pl_dmalloc_log_changed(term_t mark,
		       term_t not_freed, term_t freed, term_t details)
{ unsigned long m;
  int64_t i;
  int nf, f, d;

  if ( !PL_get_int64(mark, &i) ||
       !PL_get_bool(not_freed, &nf) ||
       !PL_get_bool(freed, &f) ||
       !PL_get_bool(details, &d) )
    return FALSE;

  m = (unsigned long)i;
  dmalloc_log_changed(m, nf, f, d);

  return TRUE;
}


static foreign_t
pl_dmalloc_message(term_t msg)
{ char *s;

  if ( PL_get_chars(msg, &s, CVT_ALL|CVT_EXCEPTION|REP_MB) )
  { dmalloc_message("%s", s);
    return TRUE;
  }

  return FALSE;
}


install_t
install_dmalloc()
{ PL_register_foreign("dmalloc_mark",	     1, pl_dmalloc_mark,        0);
  PL_register_foreign("dmalloc_log_changed", 4, pl_dmalloc_log_changed, 0);
  PL_register_foreign("_dmalloc_message",    1, pl_dmalloc_message,     0);
}
