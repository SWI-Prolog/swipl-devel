# SWI-Prolog manual sources


This directory contains the sources to  the SWI-Prolog manual. From this
source we create PDF, HTML and the   plain  text online manual available
through the help/1 and apropos/1 commands.

The *.doc files form the input. The  program doc2tex converts these into
plain LaTeX files. It is responsible   for  handling characters that are
difficult to handle in LaTeX, such as \  and to make references for such
constructs like member/2.

The style file pl.sty ensures _ (underscore) can be used without special
precautions outside math mode.  Do NOT write expand\_file/2, but simply
expand_file/2.


## Handling LaTeX special characters

Predicate names that cannot  be  handled   by  TeX  because they contain
TeX-special characters are translated by doc2tex if they appear as plain
LaTeX argument. We use the  URL   quoting  mechanism for this, producing
sequences that can be use in  any   TeX  environment (unlike \verb). The
sequences are named \S<name>. If you add such a sequence you must:

	* Add it to doc2tex
	* Add it to pl.sty
	* Add it to ../packages/ltx2htm/pl.cmd
	* Run "make install" in ../packages/ltx2htm

## Summaries

For each described predicate  there  MUST  be   an  entry  in  the  file
summary.doc.  Note  that  the  content   of    this   file   is  ordered
alphabetically.

## Documenting libraries

Libraries are documented in their own file  in the lib subdirectory. The
library file has to be added to  the input statements of lib/library.doc
and the LIBFILES macro of the  Makefile.   To  modify the Makefile, edit
Makefile.in and run ./config.status from the build-directory of the base
system (normally 'src').  The  summary  of   libraries  is  in  the file
lib/summary.doc. Please update this with   summary  information for your
library.

Many of the libraries are nowadays documented  in the source and the TeX
is created through PlDoc.  To add a library

    - Edit Makefile.in:
	- $LIBFILES
	- Add a rule to the section "PlDoc generated manual files"
	- Add an \input to lib/library.doc and lib/summary.doc.

Note that the LaTeX filename should   not contain underscores (see e.g.,
pure_input for an example).


## Generating the documentation

You need a recent latex installation   with  many optional packages. You
find the documentation dependencies for Debian   based  Linux systems at
http://www.swi-prolog.org/build/Debian.txt

You also need to install the fixed-width font libraries available in the
txt subdirectory. The README there explains how this must be installed.

For the HTML version, you need to   install the Prolog based latex2html.
This is available in  ../packages/ltx2htm.  This   is  _not  a  default
package_. If you install SWI-Prolog from  the GIT sources, perform these
steps to add it:

  - run `git submodule update --init packages/ltx2htm`
  - Add `ltx2htm` to the variable `EXTRA_PKGS` in the `build` script.

After installing SWI-Prolog and with `swipl`   in your `PATH`, run `make
install` in `packages/ltx2htm` install `latex2html` in your `$HOME/bin`

Targets:

	make online	- Generate the library/MANUAL file and index
	make pdf	- Generate SWI-Prolog-<version>.pdf
	make html	- Generate the HTML version in Manual/
	make clean	- Remove generated intermediate files
