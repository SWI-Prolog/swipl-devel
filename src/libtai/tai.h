#ifndef TAI_H
#define TAI_H

#ifdef __WINDOWS__
typedef __int64 int64_t;
typedef unsigned __int64 uint64_t;
#else
#include <inttypes.h>			/* more portable than stdint.h */
#endif

#ifdef __WINDOWS__
#define LL(x)  x ## i64
#define ULL(x) x ## ui64
#else
#define LL(x)  x ## LL
#define ULL(x) x ## ULL
#endif

struct tai {
  uint64_t x;
} ;

extern void tai_now(struct tai *t);

/* JW: MSVC cannot convert unsigned to double :-( */
#define tai_approx(t) ((double) ((int64_t)(t)->x))

extern void tai_add(struct tai *t, struct tai *u, struct tai *v);
extern void tai_sub(struct tai *t, struct tai *u, struct tai *v);
#define tai_less(t,u) ((t)->x < (u)->x)

#define TAI_PACK 8
extern void tai_pack(char *s, struct tai *t);
extern void tai_unpack(char *s, struct tai *t);

#endif
