/*
 * mk_wcwidth.c - thin wrapper that routes the historical
 * mk_wcwidth() / mk_wcswidth() / mk_wcwidth_cjk() / mk_wcswidth_cjk()
 * entry points to PL_wcwidth() (defined in pl-read.c). The Unicode
 * width tables are now per-code-point bits 4..5 in uflags_map; this
 * file is just here so external callers of the mk_* names continue
 * to link against the same Unicode data the kernel uses elsewhere.
 *
 * The CJK variants used to bump East Asian Ambiguous (A) characters
 * to width 2; that distinction is no longer carried in uflags_map,
 * so the CJK variants behave identically to their non-CJK siblings.
 * No SWI-Prolog code path used them.
 */

#include "mk_wcwidth.h"
#include "SWI-Prolog.h"

int
mk_wcwidth(uchar_t ucs)
{ return PL_wcwidth((int)ucs);
}

int
mk_wcswidth(const uchar_t *pwcs, size_t n)
{ int w, width = 0;

  for (; *pwcs && n-- > 0; pwcs++)
  { w = PL_wcwidth((int)*pwcs);
    if ( w < 0 )
      return -1;
    width += w;
  }
  return width;
}

int
mk_wcwidth_cjk(uchar_t ucs)
{ return PL_wcwidth((int)ucs);
}

int
mk_wcswidth_cjk(const uchar_t *pwcs, size_t n)
{ return mk_wcswidth(pwcs, n);
}
