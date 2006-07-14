#ifndef TAI_H
#define TAI_H

#include <stdint.h>

struct tai {
  uint64_t x;
} ;

extern void tai_now(struct tai *t);

#define tai_approx(t) ((double) ((t)->x))

extern void tai_add(struct tai *t, struct tai *u, struct tai *v);
extern void tai_sub(struct tai *t, struct tai *u, struct tai *v);
#define tai_less(t,u) ((t)->x < (u)->x)

#define TAI_PACK 8
extern void tai_pack(char *s, struct tai *t);
extern void tai_unpack(char *s, struct tai *t);

#endif
