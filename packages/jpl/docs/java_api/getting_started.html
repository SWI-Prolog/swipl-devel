<!DOCTYPE html PUBLIC "-//w3c//dtd html 4.0 transitional//en">
<html>
<head>
  <meta http-equiv="Content-Type"
 content="text/html; charset=iso-8859-1">
  <meta name="GENERATOR"
 content="Mozilla/4.74 [en] (WinNT; U) [Netscape]">
  <meta name="Author" content="Fred Dushin">
  <meta name="Keywords" content="JPL,java,prolog">
  <title>Getting Started</title>
</head>
<body text="#000000" bgcolor="#ffffff" link="#00009c" vlink="#ce31ce"
 alink="#ff0000">
<center>
<h1 style="text-align: left;"><span style="font-style: italic;">JPL 2.x</span>
Getting Started</h1>
<hr style="width: 100%; height: 2px;">
<div style="text-align: left;">This section provides a tutorial
introduction to <span style="font-weight: bold; font-style: italic;">JPL</span>
through its <a href="high-level_interface.html"><b><span
 style="font-style: italic;">JPL 2.x</span> Java API overview</b></a>,
the
interface most programmers are likely to use.&nbsp;
The source code described here can be found in the <span
 style="font-family: courier new,courier,monospace;">examples</span>
directory of
the
JPL distribution.&nbsp; Feel free to consult the source files and run
the
demonstration program as you read this section.
</div>
</center>
<h3>Verifying the installation</h3>
To confirm that <b><i>JPL</i></b> and SWI-Prolog are basically able to
work together, open a console window and go into this directory:
<blockquote>
  <pre>jpl/examples/</pre>
</blockquote>
read the README.txt file, and run the various examples which it
describes.<br>
<br>
Each Java example will run as an application (i.e. each has a <tt>main()</tt>
method), exercises SWI-Prolog, and writes something to <b>System.out</b>
or <b>System.err</b>.
<p>If you see some plausible output, with no serious error messages,
then
all may be well.
</p>
<h3>Creating a Prolog database in a text file</h3>
To experiment with <b><i>JPL</i></b>, we'll first create a Prolog
database
in a text file.&nbsp; We will eventually load this database into the
Prolog
engine through the <a
 href="file:///D:/jpl/docs/java_api/high-level_interface.html"><b><span
 style="font-style: italic;">JPL 2.x</span> Java API overview</b></a>.
<p>Type the following in a text editor and save the result in a file
called
<i>test.pl</i>
</p>
<blockquote><tt><font size="+1">child_of(joe, ralf).</font></tt> <br>
  <tt><font size="+1">child_of(mary, joe).</font></tt> <br>
  <tt><font size="+1">child_of(steve, joe).</font></tt>
  <p><tt><font size="+1">descendent_of(X, Y) :-</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp; child_of(X, Y).</font></tt> <br>
  <tt><font size="+1">descendent_of(X, Y) :-</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp; child_of(Z, Y),</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp; descendent_of(X, Z).</font></tt></p>
</blockquote>
You may wish to load this database into an interactive Prolog session
to
experiment with the predicates in this database before experimenting
with
<b><i>JPL</i></b>.
<h3>Initializing The Prolog engine</h3>
Although the <b>jpl.JPL</b> class provides a number of methods for
initializing
the Prolog engine from within Java, their use is not usually necessary:
Prolog will be automatically initialised with default parameters
at the first attempt to use it.
<h3>Consulting the Prolog database from its text file</h3>
In an ordinary interactive Prolog session, we'd load the above Prolog
database
using the Prolog <tt><font size="+1">consult/1</font></tt> predicate,
a built-in
predicate in standard Prolog.&nbsp; Note, however, that as a Prolog
predicate,
"calling" <tt><font size="+1">consult/1</font></tt> is just an example
of making
a Prolog query, and this is how we perform it with <b><i>JPL</i></b>.
<p>First we construct an instance of <b>jpl.Query</b>, whose name is <b><tt><font
 size="+1">consult</font></tt></b>
and whose arguments (just one) comprise the atom <b><tt><font size="+1">'test.pl'</font></tt></b>:
</p>
<blockquote><tt><font size="+1">Query q1 =</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp; new Query(</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
"consult",</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; new
Term[]
{new Atom("test.pl")}</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp; );</font></tt></blockquote>
Then we call the <b>query()</b> method of this <b>Query</b> object,
which
returns a Boolean value indicating its success:
<blockquote><tt><font size="+1">System.out.println( "consult " +
(q1.query()
? "succeeded" : "failed"));</font></tt></blockquote>
At this point, this process may seem a bit long-winded; however, you
should
soon see that the classes are sufficiently general that they provide a
robust and powerful interface into the Prolog engine.&nbsp; There is
also
considerable scope for writing "convenience" classes and methods, but
in
this introduction we deliberately employ the general, primitive
facilities
of the&nbsp;<a
 href="file:///D:/jpl/docs/java_api/high-level_interface.html"><b><span
 style="font-style: italic;">JPL 2.x</span> Java API overview</b></a>.
<h3>Querying the Database</h3>
Using the same technique, we can query the Prolog database about
inferences
it can make.&nbsp; To ask whether the Prolog query <tt><font size="+1">child_of(joe,ralf)</font></tt>is
true, given the above Prolog database, for example, we write:
<blockquote><tt><font size="+1">Query q2 =</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp; new Query(</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
"child_of",</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; new
Term[]
{new Atom("joe"),new Atom("ralf")}</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp; );</font></tt>
  <p><tt><font size="+1">System.out.println(</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp; "child_of(joe,ralf) is " +</font></tt>
  <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp; ( q2.query() ? "provable" :
"not
provable" )</font></tt> <br>
  <tt><font size="+1">);</font></tt></p>
</blockquote>
To take an example that requires a bit more work on the part of the
Prolog
engine, on the other hand, we can ask whether <tt><font size="+1">descendent_of(steve,ralf)</font></tt>
is true:
<blockquote><tt><font size="+1">Query q3 =</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp; new Query(</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
"descendent_of",</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; new
Term[]
{new Atom("steve"),new Atom("ralf")}</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp; );</font></tt>
  <p><tt><font size="+1">System.out.println(</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp; "descendent_of(joe,ralf) is "
+</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp; ( q3.query() ? "provable" :
"not
provable" )</font></tt> <br>
  <tt><font size="+1">);</font></tt></p>
</blockquote>
<h3>
Querying with Variables</h3>
A ground query is relatively straightforward; it is essentially
either provable or not, and there is typically no point in
backtracking.&nbsp;
Once we use variables, however, things get
a bit more complicated.
<p>Using the <b>jpl.Variable</b> class, we can construct a non ground
query;
and using other methods of <b>Query</b> we can obtain a <i>solution</i>
in the form of a <b>java.util.Hashtable</b>.&nbsp; If the
<b>Query</b> has one or more solutions, then its
<b>Query.</b><b>oneSolution()</b>
method returns a <b>Hashtable</b> representing
the first solution, otherwise
it returns <b>null</b>:
</p>
<blockquote><tt><font size="+1">Variable <b>X</b> = new Variable();</font></tt>
  <p><tt><font size="+1">Query <b>q4</b> =</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp; new Query(</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
"descendent_of",</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; new
Term[]
{<b>X</b>,new Atom("ralf")}</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp; );</font></tt> </p>
  <p><tt><font size="+1">java.util.Hashtable solution;</font></tt> </p>
  <p><tt><font size="+1">solution = <b>q4</b>.oneSolution();</font></tt>
  </p>
  <p><tt><font size="+1">System.out.println( "first solution of
descendent_of(X,
ralf)");</font></tt> <br>
  <tt><font size="+1">System.out.println( "X = " + solution.get(<b>X</b>));</font></tt></p>
</blockquote>
The <b>HashTable</b> contains bindings in the form of <b>Term</b>s,
each
of which is indexed by its corresponding <b>Variable</b> in the <b>Query</b>.
<h3>
Finding all solutions</h3>
The previous query finds only the first solution.&nbsp; Often, however,
one wants all solutions, or at least more than just the first.&nbsp;
The
<b>Query</b>
class also provides the <b>allSolutions()</b> method, which returns an
array of zero or more <b>Hashtable</b>s, each of which represents a
given
solution.
<p>In this example we reuse the query <b><tt><font size="+1">q4</font></tt></b>,
which was reset to its initial state by the call of <b><tt><font
 size="+1">oneSolution()</font></tt></b>,
and instead call <b><tt><font size="+1">allSolutions()</font></tt></b>,
which
returns an array of solutions:
</p>
<blockquote><tt><font size="+1">java.util.Hashtable[] solutions = <b>q4</b>.allSolutions();</font></tt>
  <p><tt><font size="+1">for ( int i=0 ; i&lt;solutions.length ; i++ ) {</font></tt>
  <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp; System.out.println( "X = " +
solutions[i].get(<b>X</b>));</font></tt> <br>
  <tt><font size="+1">}</font></tt></p>
</blockquote>
Equivalently, one can obtain each solution by exploiting the <b>Enumeration</b>
interface, which the <b>Query</b> class implements.&nbsp; In this
example,
we iteratively call hasMoreSolutions() and nextSolution() to
exhaustion:
<blockquote>
  <pre><tt><font size="+1">System.out.println( "each solution of descendent_of(X, ralf)");</font></tt></pre>
  <pre><tt><font size="+1">while ( q4.hasMoreSolutions() ){<br>&nbsp;&nbsp;&nbsp; solution = q4.nextSolution();<br>&nbsp;&nbsp;&nbsp; System.out.println( "X = " + solution.get(X));<br>}</font></tt></pre>
</blockquote>
In this final example, we reuse the previous variable <b><tt><font
 size="+1">X</font></tt></b>
with a new variable <b><tt><font size="+1">Y</font></tt></b> in a new
query
<b><tt><font size="+1">q5</font></tt></b>:
<blockquote><tt><font size="+1">Variable <b>Y</b> = new Variable();</font></tt>
  <p><tt><font size="+1">Query <b>q5</b> =</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp; new Query(</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
"descendent_of",</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; new
Term[]
{<b>X</b>,<b>Y</b>}</font></tt> <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp; );</font></tt> </p>
  <p><tt><font size="+1">while ( <b>q5</b>.hasMoreSolutions() ){</font></tt>
  <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp; solution = <b>q5</b>.nextSolution();</font></tt>
  <br>
  <tt><font size="+1">&nbsp;&nbsp;&nbsp; System.out.println( "X = " +
solution.get(<b>X</b>)
+ ", Y = " + solution.get(<b>Y</b>));</font></tt> <br>
  <tt><font size="+1">}</font></tt></p>
</blockquote>
The <b>hasMoreSolutions</b> method of the <b>Query</b> class returns
a
<b>boolean</b>,
indicating whether there are any solutions "left" in the query.&nbsp;
If
the answer to this is 'yes', then the solution can be obtained in the
form
of a <b>Hashtable</b> by the <b>nextSolution</b> method.
<blockquote><i><font size="-1"><b>Note.</b>&nbsp; By calling <b>hasMoreSolutions</b>
you are actually making the query to the Prolog engine; the "answer" to
the query is cached in the <b>Query</b> class instance and returned
from <b>nextSolution</b>.</font></i></blockquote>
<h3>
Where to Go From Here</h3>
This section provides a brief tutorial on getting started with the
High-Level
Interface.&nbsp; You should read the&nbsp;<a
 href="file:///D:/jpl/docs/java_api/high-level_interface.html"><b><span
 style="font-style: italic;">JPL 2.x</span> Java API overview</b></a>
section for more information about using these
interfaces.&nbsp;
Feel free to consult the <a href="javadoc/index.html"><span
 style="font-style: italic;">JPL 3.x</span> Java API reference</a>
section
for detailed information about particular classes.
<br>
<div align="right">
<hr noshade="noshade" width="100%">
<div style="text-align: left;">
<address>December 2003 (revised)</address>
<br>
<span style="text-decoration: underline;"></span></div>
</div>
</body>
</html>
