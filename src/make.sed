s/^#[ 	]*include[ 	]/&/
s/^#[ 	]*if[ 	]/&/
s/^#[ 	]*ifdef[ 	]/&/
s/^#[ 	]*ifndef[ 	]/&/
s/^#[ 	]*else/&/
s/^#[ 	]*elif[ 	]/&/
s/^#[ 	]*endif/&/
s/^#[ 	]*define[ 	]/&/
t
s/\\#/@HASH@/g
s/#.*//
s/@HASH@/\\#/g

