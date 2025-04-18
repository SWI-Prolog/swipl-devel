#! /bin/sh

#============================================================================
echo "-------------------------------------------------------"
echo "--- Running attv_tests/test.sh                      ---"
echo "-------------------------------------------------------"

XEMU=$1
opts=$2

../gentest.sh "$XEMU $opts" attv_test "test."
#../gentest.sh "$XEMU $opts" tabled_attv "test."
#../gentest.sh "$XEMU $opts" interrupt1 "test."
#../gentest.sh "$XEMU $opts" interrupt2 "test."
#../gentest.sh "$XEMU $opts" pre_image "test."
../gentest.sh "$XEMU $opts" copyterm_attv "test."
../gentest.sh "$XEMU $opts" findall_attv "test."
../gentest.sh "$XEMU $opts" trie_intern_attv "test."
../gentest.sh "$XEMU $opts" trie_assert_attv "test."
#../gentest.sh "$XEMU $opts" trie_assert_attv2 "test."
../gentest.sh "$XEMU $opts" assert_attv "test."
../gentest.sh "$XEMU $opts" ret_attv "test."
../gentest.sh "$XEMU $opts" fd1 "test."
../gentest.sh "$XEMU $opts" general "test."
../gentest.sh "$XEMU $opts" attvar_assert_regalloc "test."

# the following is obsolete (attvs now allowed in subsumptive tables)
#../gentest.sh "$XEMU $opts" catch1 "test."
