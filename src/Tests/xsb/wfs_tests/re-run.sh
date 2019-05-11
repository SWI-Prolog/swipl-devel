old="$(ls *_new | wc -l)"
rm -f *_new
./test.sh swipl -q 2>&1 | tee test.log
echo "OLD: $old failed tests"
echo "NEW: $(ls *_new | wc -l) failed tests"
ls *_new
