#! /bin/sh

XEMU=$3

suffixes="s te1 cn iv s1 te11 cn1 iv1"
for suff in $suffixes ; do
    file=$2_$suff
    test -f "$file" &&  /bin/rm -f $file
done

$XEMU << EOF
[ptq,ptq_out].
$1.
output_table($2_s,s(_,_,_,_,_)).
output_table($2_te1,te1(_,_,_,_,_,_,_)).
output_table($2_cn,cn(_,_,_,_,_,_)).
output_table($2_iv,iv(_,_,_,_,_,_)).
halt.
EOF

suffixes="iv s te1 cn"
for suff in $suffixes ; do
    file=$2_$suff
    sort -u $file > ${file}1
done
