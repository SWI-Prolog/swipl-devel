#include <sys/types.h>
#include <signal.h>
#include <unistd.h>

static int signalled;

static void
catch(int s)
{ signalled = 1;
}

int
main()
{ signal(SIGINT, (void*)catch);
  kill(getpid(), SIGINT);
  while(!signalled)
    sleep(1);
  if ( (void*)signal(SIGINT, (void*)catch) == (void*)catch )
    return 0;
  return 1;
}
