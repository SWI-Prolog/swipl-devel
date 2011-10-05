#!/bin/bash

# Find unused functions in a set of object files
# Typical use:
#
# ../scripts/unused.sh *.o */*.o

files="$*"
deffed=unused.deffed$$
used=unused.used$$

nm $files | grep -w T | awk '{print $3}' | sort -u >> $deffed
nm $files | grep -w U | awk '{print $2}' | sort -u >> $used

comm -23 $deffed $used | egrep -v '^_?(PL|SP)_'

rm -f $deffed $used
