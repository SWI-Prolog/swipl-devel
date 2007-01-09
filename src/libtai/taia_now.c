#include <sys/types.h>
#ifdef __WINDOWS__
#define WINDOWS_LEAN_AND_MEAN 1
#include <windows.h>
#else
#include <sys/time.h>
#endif
#include "taia.h"

/* XXX: breaks tai encapsulation */

    /*-------------------------------------------------------------------------
    * The Microsoft Win32 API can return the current system time in "file
    * timestamp" format, which is a 64-bit value representing the number of
    * 100-nanosecond ticks since {AD1601-01-01 00:00:00 Z}.
    * 11644473600 is seconds offset AD1601 to AD1970
    *------------------------------------------------------------------------*/

void taia_now(struct taia *t)
{
#ifdef __WINDOWS__
  FILETIME ft;
  int64_t cns;				/* 100ns ticks */

  /* Get the current system time */
  GetSystemTimeAsFileTime(&ft);

  /* Convert to longtime_t form */
  cns = ((int64_t)ft.dwHighDateTime << 32) + ft.dwLowDateTime;
  t->sec.x = cns/10000000 - 11644473600 + ULL(4611686018427387914);
  t->nano  = (long)((cns % 10000000))*100;
  t->atto  = 0;
#else
  struct timeval now;
  gettimeofday(&now,(struct timezone *) 0);
  t->sec.x = ULL(4611686018427387914) + (uint64_t) now.tv_sec;
  t->nano = 1000 * now.tv_usec + 500;
  t->atto = 0;
#endif
}
