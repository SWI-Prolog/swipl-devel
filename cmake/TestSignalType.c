#include <sys/types.h>
#include <signal.h>
#ifdef signal
# undef signal
#endif
#ifdef __cplusplus
extern "C"
#else
void ( *signal(int signum, void (*handler)(int)) ) (int);
#endif

int
main(int argc, char **argv)
{
  return 0;
}
