/*
 * mk_wcwidth.h - declarations for Markus Kuhn's wcwidth() implementation.
 *
 * The functions take a `uchar_t` (uint32_t) so the full Unicode range
 * is reachable on every platform.  The system `wchar_t` is 16-bit on
 * Windows, which silently truncates non-BMP code points before they
 * ever reach a function call — we route around that here.
 *
 * On Windows (where no system wcwidth() exists), this header also
 * defines wcwidth(c) as mk_wcwidth((uchar_t)(c)) so the rest of
 * libedit / SWI-Prolog can use wcwidth() uniformly.
 *
 * Source: http://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c
 */

#ifndef MK_WCWIDTH_H
#define MK_WCWIDTH_H

#include <stdint.h>
#include <stddef.h>
#include <wchar.h>

#ifdef __cplusplus
extern "C" {
#endif

/* 32-bit Unicode code point.  Guarded so multiple includers (and
 * xpce's matching definition in h/charwidth.h / txt/terminal.h) don't
 * collide.  Note: collides with Solaris/illumos `<sys/types.h>` which
 * defines uchar_t as `unsigned char` — those are not supported
 * platforms. */
#ifndef UCHAR_T_DEFINED
#define UCHAR_T_DEFINED
typedef uint32_t uchar_t;
#endif

int mk_wcwidth(uchar_t ucs);
int mk_wcswidth(const uchar_t *pwcs, size_t n);
int mk_wcwidth_cjk(uchar_t ucs);
int mk_wcswidth_cjk(const uchar_t *pwcs, size_t n);

#ifdef __WINDOWS__
#define wcwidth(c) mk_wcwidth((uchar_t)(c))
#endif

#ifdef __cplusplus
}
#endif

#endif /* MK_WCWIDTH_H */
