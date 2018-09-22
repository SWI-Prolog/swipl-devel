#include <unistd.h>

int
main()
{ long n = sysconf(_SC_NPROCESSORS_CONF);
}
