The ptq testsuite was the first testsuite written for XSB, as I recall
in 1992, before we even had much of a system.  It has been extremely
helpful in finding a number of bugs in XSB, but can be a little
difficult to read and to understand if you don't have the code.

The first idea was to automatically compare the output of the emulator
against that of David and Weidong's XOLDT meta-interpreter for each
sentance parsed by a Montague-style grammar.  The emulator and SLG
were developing at the same time; the testsuite was done before there
was a stable SLG meta-interpreter and before the name XSB was stumbled
upon.  For instance, 

./genlredscaff.csh "tran([john,talks,about,a,unicorn])" jtaulredemu $XEMU 
./diffall.csh jtaulredemu jtaulredint 

The file jtaulredemu stands for 

	jtau = john,talks,about,a,unicorn

	lred = cuts (for lambda reduction) are done in a particular
	part of the parse

	emu = this file was created by XSB.

The reference files are marked int and are created by the interpreter.

For jtaulredemu, four files are then created (via genlredscaff.sh)

output_table(${argv[2]}_s,s(_,_,_,_,_)).
output_table(${argv[2]}_te1,te1(_,_,_,_,_,_,_)).
output_table(${argv[2]}_cn,cn(_,_,_,_,_,_)).
output_table(${argv[2]}_iv,iv(_,_,_,_,_,_)).
	
         jtaulredemu_cn
         jtaulredemu_iv
         jtaulredemu_s
         jtaulredemu_te1

which represent all tabled predicatesin the lred parse (e.g. cn, iv,
s, and te1 --- not my names).  In order to avoid any difficulties in
ordering, each of these files are sorted to produce

         jtaulredemu_cn1
         jtaulredemu_iv1
         jtaulredemu_s1
         jtaulredemu_te11

each of which are compared (via diffall.sh) to their corresponding
int files.

Thus, the only data files that need to be checked in are 

         *int_cn1
         int_iv1
         int_s1
         int_te11

In addition, of course, to the appropriate .sh files, which I think
are

test.sh
genscaff.sh
genlredscaff.sh
diffall.sh

In January 1999, Kostis modified the ptq testsuite by adding a file
named ptq_out.P which does the output using the table routines
get_calls/3 and get_returns/2, rather than get_calls_for_table and
get_returns_for_call.

- Terry (1/14/1999)

