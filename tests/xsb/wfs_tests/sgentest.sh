#! /bin/sh
 
# $1 is expected to have xsb ececutable + command line options
EMU=$1
CONSULT_FILE=$2
TEST_FILE=$3
CMD=$4

DIR=`pwd`
BASEDIR=`basename $DIR`

echo "--------------------------------------------------------------------"
echo "Testing $BASEDIR/$TEST_FILE"
#echo "$EMU"     # debug check: verify that options have been passed to xsb

# dont need to tell here, done in test.P
$EMU << EOF
[$CONSULT_FILE].
$CMD
told.
EOF

# print out differences.
if test -f ${TEST_FILE}_new; then
	rm -f ${TEST_FILE}_new
fi
    
sort temp > ${TEST_FILE}_new
sort ${TEST_FILE}_old > temp

#-----------------------
# print out differences.
#-----------------------
d=`diff -w ${TEST_FILE}_new temp`
if test -z "$d"; then 
	echo "$BASEDIR/$TEST_FILE tested"
	rm -f ${TEST_FILE}_new
else
	echo "$BASEDIR/$TEST_FILE differ!!!  (on subsumption)"
	diff -w ${TEST_FILE}_new ${TEST_FILE}_old
fi

rm -f temp

