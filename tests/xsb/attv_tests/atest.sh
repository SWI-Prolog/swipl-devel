#! /bin/sh

#============================================================================
echo "-------------------------------------------------------"
echo "--- Running attv_tests/test.sh (using call abstraction)   ---"
echo "-------------------------------------------------------"

XEMU=$1
opts=$2

../agentest.sh "$XEMU $opts" attv_test "test."
#../agentest.sh "$XEMU $opts" tabled_attv "test."
#../agentest.sh "$XEMU $opts" interrupt1 "test."
#../agentest.sh "$XEMU $opts" interrupt2 "test."
#../agentest.sh "$XEMU $opts" pre_image "test."
../agentest.sh "$XEMU $opts" copyterm_attv "test."
../agentest.sh "$XEMU $opts" findall_attv "test."
../agentest.sh "$XEMU $opts" trie_intern_attv "test."
../agentest.sh "$XEMU $opts" trie_assert_attv "test."
#../agentest.sh "$XEMU $opts" trie_assert_attv2 "test."
../agentest.sh "$XEMU $opts" assert_attv "test."
../agentest.sh "$XEMU $opts" ret_attv "test."
../agentest.sh "$XEMU $opts" fd1 "test."
../agentest.sh "$XEMU $opts" general "test."

# the following is obsolete (attvs now allowed in subsumptive tables)
#../agentest.sh "$XEMU $opts" catch1 "test."
