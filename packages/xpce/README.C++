		What's where for using C++ with XPCE?
		=====================================

			  Jan Wielemaker


Alpha Version
=============

The interface as packed with XPCE-4.6.4 is alpha.  We try to get answers
to the following questions:

	# Do *you* think it is useful?  For doing what?
	
	# Do you think this is the way XPCE should look from the
	  C++ world?  If not, do you have ideas on improvements or
	  even a completely different approach?

	# Are there holes (missing functionality) in the interface?
	
	# Are there bugs in the interface?

	# Anything you (dis)like?

The C++ interface is almost entirely in source-form (except for a couple
of hooks in the message-passing kernel).

Please send your comments to jan@swi.psy.uva.nl


XPCE/C++: For whom?
===================

Below are some cases where the C++ interface may be benificial to you:

	# I like programming XPCE/Prolog/Lisp and my boss tells me to
	program this application in C/C++.

	You are lucky: you don't have to  start learning a new interface
	library!

	# I like programming XPCE/Prolog/Lisp, but my application needs
	this module to be really speedy.

	Write the module in C(++) and pack it into an XPCE class.  Now
	you can use it from your favorite programming environment as if
	it has always been part of XPCE.

	# I want to write a multi-language application.
	
	XPCE can communicate with Prolog, Lisp and C++; if necessary
	with all three at the same time as long as Prolog and Lisp
	don't dislike each other too much.

	# I'm looking for a GUI library for C++.
	
	All GUI libraries have strong and weak points, and so has XPCE.
	Weak points are its portability, which is limited to systems
	running the X11 windowing environment.  Unix is preferred, but
	any machine with a uniform address-space, an ANSI C-compiler and
	a (possible emulated) X11 library is a potential target.
	Standard look-and-feel is propretary.  OpenLook can be 90%
	emulated.  Motif is under study.

	Strong points are the high level of support given for
	interactive graphics and the high-level runtime environment.


Versions and status
===================

XPCE version 4.6.4 is the first   distributed  version which supports an
interface to the C++ language.   The  interface   has  been  tested on a
reasonable  variety  of  (small)  programs  to  ensure  that  the  basic
functionality is stable enough for alpha   testing on a wider community.
As things have not yet moved from the two development platforms, bugs in
the distribution (missing files for example) are not unlikely.

Versions tested:

	SUN:	OS:	Sparc-sunos-4.1.3
		CC/C++	gcc/g++ 2.5.4 
		M4	gnu-m4-1.1
		make	gnu-make-3.62, /bin/make

	PC:	OS:	linux-0.99.13
		CC/C++	gcc/g++ 2.4.5
		M4	gnu-m4
		make	gnu-make

Documentation
=============

The interface is described (informally, with a   lot of examples) in ``A
C++  interface  for  XPCE''.   This  document    is  in  LaTeX  form  in
$PCEHOME/man/C++/*.  There is  a  PostScript   version  available  using
anonymous ftp from swi.psy.uva.nl:/pub/xpce/doc/C++/pce-C++.ps.gz.


Where to start?
===============

Install XPCE as normal.  Ensure g++,  gcc   and  gnu-m4 are installed in
your search path.  If you  don't  have   gnu-m4  you  can  try any other
system-V m4.  BSD m4's won't do.  Type `which m4' to find the m4 you are
running.  Next, add PCEHOME to your (Unix)  environment and make sure it
points to the XPCE base directory.  The base directory is normally wired
in the XPCE/Prolog/Lisp executable at boot time, but for the C++ version
we have not yet established  a  standard.    $PCEHOME  is  used  to find
bitmaps and the PostScript header files.

To  test  the  interface,  goto    C++/demo.   Edit  Makefile  variables
(documented in the Makefile in this directory) and type

	% make test.o
	
Now, if you are interested in   using  stand-alone XPCE/C++ application,
type

	% make test
	% ./test

To see a simple example.

You can load the same thing on top of XPCE/SWI-Prolog using:

	% xpce
	...
	?- 'pceloadc++'(test).

You can look at the various demo programs   in  this directory to get an
impression on the capabilities of the   interface.  The Makefile in this
directory may be used as a template for you own XPCE/C++ code.


Other C++ compilers
===================

I've tried no other C++ compilers.  No major problems are to be expected,
but there are some potential problems:

	# Library (libgcc.a)
	If XPCE is compiled with gcc, you will need to link the library
	libgcc.a to create a complete executable.  The linker should consider
	libgcc.a just before libc.a

	# Loading on top of XPCE/SWI-Prolog: global constructors
	Getting C++ global constructors to work with Prolog incremental
	linking required a -propably gnu-specific- trick. 
	See pl/src/pl-crt0.c and pl/src/mangle.c

	# Calling conventions
	Different compilers sometimes have different calling conventions.
	GCC generally confirms the calling conventions of the native C
	compiler ...

Issues
======

	* At the moment, XPCE is a static library.  It would be better
	to turn it into a shared library object.

	* Runtime licencing policies have not yet been considered.  In
	the meanwhile, the PC/Linux version of XPCE.a may be copied
	free of charge.  Other versions may not be re-distributed, nor
	in source-form, neither in object-form.
	
