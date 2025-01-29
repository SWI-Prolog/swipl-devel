# XSB test suite

This directory contains tests that are copied from XSB Prolog. Using XSB
tests was proposed by Theresa Swift as part as a Kyndi-initiated project
to get XSB and SWI-Prolog more compatible.   This project is carried out
by Jan Wielemaker, Theresa Swift,  David   Warren,  Benjamin  Grosof and
Fabrizio Riguzzi.

The original XSB tests use shell scripts   to drive the tests. This copy
is integrated into the  SWI-Prolog  test   framework  and  uses a Prolog
replacement of the test framework provided by `xsb_tests.pl`.

## Legal

The XSB project is governed by the LGPL-2.0-only license. This allows us
to distribute these files with SWI-Prolog.  As   the  test files are not
part of user applications, these LGPL tests  have no impact on the BSD-2
license conditions of SWI-Prolog. The   LGPL-2.0-only license applies to
all .P files in subdirectories of this  directory unless the file states
otherwise. All .pl  files  are  developed   as  part  of  SWI-Prolog and
licensed under BSD-2 unless the file states otherwise.
