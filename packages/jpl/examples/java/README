JPL 3.0.0 for Windows
=====================

Tests for the Java-calls-Prolog interface:
------------------------------------------

This directory contains various rudimentary tests for JPL3's
Java-calls-Prolog interface, each within a separate folder.

Exceptions
	demonstrates how an uncaught Prolog exception is turned
	into a Java exception (also uncaught, in this example)

Exceptions2
	demonstrates how an uncaught Prolog exception is turned
	into a Java exception, which in this example is caught,
	converted to a String and printed on System.out (and
	also shown in a Swing treeview gadget)

Family
	consults some parent-child relationship clauses (from
	family.pl) and runs some simple queries

FamilyMT
	consults some parent-child relationship clauses (from
	family.pl) and then sets off twenty threads, each doing the
	queries of Family (above).  As there are only five available
	Prolog engines by default, the threads sometimes get to wait.

Test
	runs a simple set of tests of the High-Level Interface.
	You should run this test once you have gotten JPL built,
	or if you are experiencing any problems with the package.

Test2
	recursively computes factorial(10) by alternately calling
	Prolog from Java and Java from Prolog

Time
	runs a set of timing experiments, designed to gauge the speed
	with which terms are exchanged between Java and Prolog

Versions
	tries to discover which versions (e.g. 3.0.3-alpha) of each JPL
	library (i.e. Prolog, C and Java) are installed, and congratulates
	you if they are the same

Zahed
	creates a trivial-in-Prolog-but-horrible-in-JPL1.x query goal
	and calls it against a trivial Prolog database; this would be much
	nicer if it used recent facilities for creating queries from
	Prolog source text fragments

NB each folder contains a Java application which should be run within
its folder so it can find any corresponding .pl (Prolog source) files:
Windows users can use 'compile.bat' to (re)compile each demo, and
'run.bat' to run it.

----

Paul Singleton (paul.singleton@bcs.org.uk)
July 2003
January 2004 (revised)

