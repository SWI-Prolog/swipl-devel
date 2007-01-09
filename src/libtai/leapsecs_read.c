#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <stdlib.h>
#ifdef __WINDOWS__
#include <io.h>
#else
#include <unistd.h>
#endif
#include "tai.h"

#define GLOBAL
#include "leapsecs.h"

#ifndef O_BINARY
#define O_BINARY 0
#endif
#ifndef O_NDELAY
#define O_NDELAY 0
#endif

int leapsecs_read(const char *file)
{ int fd;
  struct stat st;
  struct tai *t;
  int n;
  int i;
  struct tai u;

  fd = open(file, O_RDONLY|O_NDELAY|O_BINARY);
  if (fd == -1) {
    if (errno != ENOENT) return -1;
    if (leapsecs) free(leapsecs);
    leapsecs = 0;
    leapsecs_num = 0;
    return 0;
  }

  if (fstat(fd,&st) == -1) { close(fd); return -1; }

  t = (struct tai *) malloc(st.st_size);
  if (!t) { close(fd); return -1; }

  n = read(fd,(char *) t,st.st_size);
  close(fd);
  if (n != st.st_size) { free(t); return -1; }

  n /= sizeof(struct tai);

  for (i = 0;i < n;++i) {
    tai_unpack((char *) &t[i],&u);
    t[i] = u;
  }

  if (leapsecs) free(leapsecs);

  leapsecs = t;
  leapsecs_num = n;

  return 0;
}
