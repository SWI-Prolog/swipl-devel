#! /bin/sh
 
#============================================================================
echo "-------------------------------------------------------"
echo "--- Running wfs_tests/test.sh (using call subsumption)  ---"
echo "-------------------------------------------------------"

XEMU=$1
options=$2

file_list=p*.P

for file in $file_list ; do
	prog=`basename $file .P`
	# XEMU and options must be together in quotes
	sh ./sgentest.sh "$XEMU $options" wfs_test $prog "test($prog)."
done
