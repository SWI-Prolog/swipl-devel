#!/bin/bash
#
# This script is used to  create  the   daily  builds  for Windows using
# cross-compilation on Linux. The script is intended   to  run as a cron
# job.
#
# As  is,  it  assumes  a  user    `prolog`  doing  the  job,  installed
# prerequisites and SSH certificates to do the uploads.

export MINGW32_ROOT=$HOME/mingw32
export MINGW64_ROOT=$HOME/mingw64

export DAILY=true
export DISPLAY=:32
Xvfb $DISPLAY > /dev/null 2>&1 &
XPID=$!

targeturl=ops:web/download/daily/bin
finaldir=/home/prolog/daily/bin
date=$(date +%F)

( cd /home/prolog/src/swipl-devel
  git pull
  git submodule update --init
) > /home/prolog/src/swipl-devel/daily-update.log 2>&1

if [ -z "$1" -o "$1" = win32 ]; then
( cd /home/prolog/src/swipl-devel
  rm -rf win32
  mkdir win32
  cd win32
  find . -name '*.qlf' | xargs rm
  cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCHAIN_FILE=../cmake/cross/linux_win32.cmake -G Ninja ..
  ninja -j 2
  cpack
) > /home/prolog/src/swipl-devel/daily-win32.log 2>&1
fi

if [ -z "$1" -o "$1" = win64 ]; then
( cd /home/prolog/src/swipl-devel
  rm -rf win64
  mkdir win64
  cd win64
  find . -name '*.qlf' | xargs rm
  cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCHAIN_FILE=../cmake/cross/linux_win64.cmake -G Ninja ..
  ninja -j 2
  cpack
) > /home/prolog/src/swipl-devel/daily-win64.log 2>&1
fi

kill $XPID

find $finaldir -name '*.exe' -ctime +7 | xargs rm 2>/dev/null

mv /home/prolog/src/swipl-devel/win32/*.exe $finaldir/swipl-w32-$date.exe
mv /home/prolog/src/swipl-devel/win64/*.exe $finaldir/swipl-w64-$date.exe

rsync -a --delete $finaldir/ $targeturl
