s/^#[ 	]*include[ 	]/&/
s/^#[ 	]*if[ 	]/&/
s/^#[ 	]*ifdef[ 	]/&/
s/^#[ 	]*else/&/
s/^#[ 	]*elif[ 	]/&/
s/^#[ 	]*endif/&/
s/^#[ 	]*define[ 	]/&/
t
s/\\#/@HASH@/g
s/#.*//
s/@HASH@/\\#/g

