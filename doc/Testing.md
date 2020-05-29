# Testing SWI-Prolog

By  default  the  `cmake`  configuration  is   setup  to  build  a  test
configuration for `ctest`. The tests are  designed to allow for parallel
execution. The normal way to run them is   like  this. The `-j 8` runs 8
tests concurrently. At the time of writing,   using more than 16 jobs is
useless because some of the tests take long.  A good value is the number
of cores you have, maximizing at 16 or   something  lower if you want to
limit resource usage.

    ctest -j 8 --output-on-failure

These  defaults  can  be  set  using   these  environment  variables  in
`~/.bashrc`:

    export CTEST_PARALLEL_LEVEL=8
    export CTEST_OUTPUT_ON_FAILURE=y


## Network testing

Most of the network testing is done   by  creating multiple threads that
run both the server and client  in   one  process. This does require the
test suite to open network ports,   but  only requires localhost access.
There  are  also  some  tests  that  test  access  to  public  networks,
contacting  https://www.swi-prolog.org.  As  some    testing  containers
disallow public network access, these tests are disabled by default.  To
enable them set the variable `SWIPL_PUBLIC_NETWORK_TESTS` to `true`:

    SWIPL_PUBLIC_NETWORK_TESTS=true ctest

or in `~/.bashrc`:

    export SWIPL_PUBLIC_NETWORK_TESTS=true


## Testing the installed system

In some scenarios one  wishes  to   test  in  the deployment environment
rather than the build environment.  This   feature  was  added for cross
compiling SWI-Prolog, a condition  that  makes   testing  in  the  build
environment impossible. Testing in a   compatible (in theory) deployment
environment is another use case.

This is facilitated by running `cmake` as

    cmake -DINSTALL_TESTS=ON ...

Installing the system adds a subdirectory  `test` to the SWI-Prolog home
directory, containing about 12Mb  (at  the   time  of  writing)  of test
scripts and data.  To run the test, start Prolog and run

    ?- test_installation.

Scripts may wish to do  so   non-interactively  like below. This command
exits with status 0 on success and non-zero if some test(s) failed.

    swipl -g test_installation -t halt
