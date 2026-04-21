/*
 * mk_wcwidth.h - declarations for Markus Kuhn's wcwidth() implementation.
 *
 * On Windows (where no system wcwidth() exists), this header also defines
 * wcwidth(c) as mk_wcwidth(c) so the rest of libedit can use wcwidth()
 * uniformly.
 *
 * Source: http://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c
 */

#ifndef MK_WCWIDTH_H
#define MK_WCWIDTH_H

#include <wchar.h>

#ifdef __cplusplus
extern "C" {
#endif

int mk_wcwidth(wchar_t ucs);
int mk_wcswidth(const wchar_t *pwcs, size_t n);
int mk_wcwidth_cjk(wchar_t ucs);
int mk_wcswidth_cjk(const wchar_t *pwcs, size_t n);

#ifdef __WINDOWS__
#define wcwidth(c) mk_wcwidth(c)
#endif

#ifdef __cplusplus
}
#endif

#endif /* MK_WCWIDTH_H */
