#! /bin/sh

echo "--------------------------------------------------------"

suffixes="s iv cn te1"

for suff in $suffixes ; do
    infile=$2_${suff}1
    tempinfile=$2_${suff}_tmp
    outfile=$1_${suff}
    outfile1=$1_${suff}1
    test -f "$infile" || touch "$infile"

# resorting infile to avoid problems with incompatable sorts (a la Redhat 6.1)
    sort -u $infile > $tempinfile

    status=0
    diff -w $outfile1 $tempinfile || status=1
    if test "$status" = 0 ; then
	echo "$outfile1 tested"
            /bin/rm -f $outfile1 $outfile $tempinfile
    else
	echo "$outfile1 and $infile differ!!!"
	diff -w ${FILE}_new ${FILE}_old
    fi

done
