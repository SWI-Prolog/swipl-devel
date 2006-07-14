#ifndef TAIA_H
#define TAIA_H

#include "tai.h"

struct taia {
  struct tai sec;
  unsigned long nano; /* 0...999999999 */
  unsigned long atto; /* 0...999999999 */
} ;

extern void taia_tai(struct taia *ta, struct tai *t);

extern void taia_now(struct taia *t);

extern double taia_approx(struct taia *t);
extern double taia_frac(struct taia *t);

extern void taia_add(struct taia *t, struct taia *u, struct taia *v);
extern void taia_sub(struct taia *t, struct taia *u, struct taia *v);
extern void taia_half(struct taia *t, struct taia *u);
extern int taia_less(struct taia *t, struct taia *u);

#define TAIA_PACK 16
extern void taia_pack(char *s, struct taia *t);
extern void taia_unpack(char *s, struct taia *t);

#define TAIA_FMTFRAC 19
extern unsigned int taia_fmtfrac(char *s, struct taia *t);

#endif
