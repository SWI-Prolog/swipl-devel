#ifndef LEAPSECS_H
#define LEAPSECS_H

extern int leapsecs_init(void);
extern int leapsecs_read(const char *file);

extern void leapsecs_add(struct tai *t, int hit);
extern int leapsecs_sub(struct tai *t);

#ifndef GLOBAL
#define GLOBAL extern
#endif

GLOBAL struct tai *leapsecs;
GLOBAL int leapsecs_num;
#endif
