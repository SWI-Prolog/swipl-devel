#!/bin/bash

# This script contains checks against known GCC warnings that indicate the
# compiled program may exhibit runtime failures (including crashes).
#
# The script is based on Gentoo Linux's portage install_qa_check() GCC checks.

if [ -z "$1" ]; then
    echo "Usage: test.sh logfile"
    exit 1
fi

LOG_FILE=$1

RED="\033[01;31m"
RESET="\033[00m"

msgs=(
      # GCC warnings that may cause runtime failures
      ": warning: dereferencing type-punned pointer will break strict-aliasing rules"
      ": warning: dereferencing pointer .* does break strict-aliasing rules"
      ": warning: implicit declaration of function"
      ": warning: incompatible implicit declaration of built-in function"
      ": warning: is used uninitialized in this function" # we'll ignore "may" and "might"
      ": warning: comparisons like X<=Y<=Z do not have their mathematical meaning"
      ": warning: null argument where non-null required"
      ": warning: array subscript is below array bounds"
      ": warning: array subscript is above array bounds"
      ": warning: attempt to free a non-heap object"
      ": warning: .* called with .*bigger.* than .* destination buffer"
      ": warning: call to .* will always overflow destination buffer"
      ": warning: assuming pointer wraparound does not occur when comparing"
      ": warning: hex escape sequence out of range"
      ": warning: [^ ]*-hand operand of comma .*has no effect"
      ": warning: converting to non-pointer type .* from NULL"
      ": warning: NULL used in arithmetic"
      ": warning: passing NULL to non-pointer argument"
      ": warning: the address of [^ ]* will always evaluate as"
      ": warning: the address of [^ ]* will never be NULL"
      ": warning: too few arguments for format"
      ": warning: reference to local variable .* returned"
      ": warning: returning reference to temporary"
      ": warning: function returns address of local variable"
      ": warning: .*\\[-Wsizeof-pointer-memaccess\\]"
      # GCC warnings that may indicate problems on 64 bit builds
      ": warning: .* makes pointer from integer without a cast"
      ": warning: cast to pointer from integer of different size"
     )

i=0
while [[ -n ${msgs[${i}]} ]] ; do
    m=${msgs[$((i++))]}
    # force C locale to work around slow unicode locales
    f=$(LC_ALL=C grep "${m}" $LOG_FILE)
    if [[ -n ${f} ]] ; then
	echo -ne '\n'
	echo -e "${RED}QA Notice: Package triggers severe warnings which indicate that it${RESET}"
	echo -e "${RED}           may exhibit random runtime failures.${RESET}"
	echo "${f}"
	echo -ne '\n'
    fi
done
