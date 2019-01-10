# Get TIME_WITH_SYS_TIME, compatible with autoconf AC_HEADER_TIME

check_c_source_compiles(
    "#include <sys/time.h>
     #include <time.h>
     int main() { return 0; }"
    TIME_WITH_SYS_TIME)
