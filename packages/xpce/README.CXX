		What's where for using C++ with XPCE?
		=====================================

			  Jan Wielemaker

Last updated:	March 27, 1997 for XPCE 4.9.3

Aim
===

The C++ interface is intended to serve   two  purposes. One is to extend
the XPCE class hierarchy with classes that need to be fast and/or access
(OS) features not available from Prolog.  The   other  was  to allow for
stand-alone XPCE/C++ executables, in which Prolog was not involved.

Though still supported, we dropped the second (XPCE/C++ applications) as
an official goal for two reasons.  First   of  all, there are already so
many C++ GUI tools that is appears pointless   to  add one more. This is
not really true if you need to   write an incidental C++ GUI application
and XPCE/Prolog is your basic GUI tool, in which case you may wish to go
for it anyhow.  Second,  when  we   started  the  C++  interface, Prolog
environments were not as mature  as  they   are  now,  and machines were
small. Neither is the case these days, which eliminates the need for C++
in most applications.


Status
======

This interface was originally written for XPCE 4.6.4 in 1993. It has not
been used much. One of the reasons was that g++ was not very stable, and
C++ objects were badly  supported  in   Unix  shared  objects.  With the
introduction of the ELF binary format,   this has changed radically, and
C++ defined classes can now be easily compiled and loaded into a running
XPCE/Prolog system.

Maybe we can start serious evaluation now.  Our questions to you are:

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

	Strong points are the high level of support given for
	interactive graphics and the high-level runtime environment.


Versions and status
===================

XPCE version 4.6.4 is the first   distributed  version which supports an
interface to the C++ language.   The  interface   has  been  tested on a
reasonable  variety  of  (small)  programs  to  ensure  that  the  basic
functionality is stable enough for alpha   testing on a wider community.

Versions tested:

	SUN:	OS:	Sparc-solaris-2.5
		CC/C++	gcc/g++ 2.7.2

	PC:	OS:	linux-2.0.29
		CC/C++	gcc/g++ 2.7.2

Documentation
=============

The interface is described (informally, with a   lot of examples) in ``A
C++  interface  for  XPCE''.   This  document    is  in  LaTeX  form  in
$PCEHOME/man/CXX/*.  There is  a  PostScript   version  available  using
anonymous ftp from swi.psy.uva.nl:/pub/xpce/doc/CXX/pce-CXX.ps.gz.


Where to start?
===============

With the demos in $PCEHOME/CXX/demo. The README file there tells how the
demos are compiled and run.
