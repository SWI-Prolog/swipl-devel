<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<html>
<head>
  <meta http-equiv="CONTENT-TYPE"
 content="text/html; charset=windows-1252">
  <title>High-Level Interface</title>
  <meta name="GENERATOR" content="StarOffice 7  (Win32)">
  <meta name="AUTHOR" content="Fred Dushin">
  <meta name="CREATED" content="20040216;23165440">
  <meta name="CHANGEDBY" content="Paul Singleton">
  <meta name="CHANGED" content="20040216;23225011">
  <meta name="KEYWORDS" content="JPL,java,prolog">
  <style>
	<!--
		P { color: #000000 }
		H3 { color: #000000 }
		BLOCKQUOTE { color: #000000 }
		PRE { color: #000000 }
		H4 { color: #000000 }
		A:link { color: #00009c }
		A:visited { color: #ce31ce }
	-->
	</style>
</head>
<body lang="en-US" text="#000000" link="#00009c" vlink="#ce31ce"
 bgcolor="#ffffff" dir="ltr">
<h1><b><span style="font-style: italic;">JPL 3.x</span>
Java API overview</b><font size="7"><b><br>
</b></font></h1>
<hr style="width: 100%; height: 2px;">
<h3>Table of Contents</h3>
<ul>
  <li>
    <p style="margin-bottom: 0cm;"><a href="#Introduction">Introduction</a>
    </p>
  </li>
  <li>
    <p style="margin-bottom: 0cm;"><a href="#The%20Class%20Hierarchy">The
Class Hierarchy</a> </p>
  </li>
  <li>
    <p style="margin-bottom: 0cm;"><a
 href="#Initializing%20and%20Terminating%20Prolog">Initializing Prolog</a>
    </p>
  </li>
  <li>
    <p style="margin-bottom: 0cm;"><a href="#Creating%20Terms">Creating
Terms</a> </p>
    <ul>
      <li>
        <p style="margin-bottom: 0cm;"><a href="#Atoms">Atoms</a> </p>
      </li>
      <li>
        <p style="margin-bottom: 0cm;"><a href="#Variables">Variables</a>
        </p>
      </li>
      <li>
        <p style="margin-bottom: 0cm;"><a href="#Integers">Integers</a>
        </p>
      </li>
      <li>
        <p style="margin-bottom: 0cm;"><a href="#Floats">Floats</a> </p>
      </li>
      <li>
        <p style="margin-bottom: 0cm;"><a href="#Compound%20Terms">Compound
Terms</a> </p>
      </li>
    </ul>
  </li>
  <li>
    <p style="margin-bottom: 0cm;"><a href="#Creating%20Queries">Creating
Queries</a> </p>
  </li>
  <li>
    <p style="margin-bottom: 0cm;"><a href="#The%20Util%20Class">The
Util Class</a> </p>
  </li>
  <li>
    <p style="margin-bottom: 0cm;"><a href="#Querying%20Prolog">Querying
Prolog</a> </p>
    <ul>
      <li>
        <p style="margin-bottom: 0cm;"><a
 href="#Obtaining%20one%20Solution">Obtaining One Solution</a> </p>
      </li>
      <li>
        <p style="margin-bottom: 0cm;"><a
 href="#Obtaining%20all%20Solutions">Obtaining all Solutions</a> </p>
      </li>
      <li>
        <p style="margin-bottom: 0cm;"><a href="#Ground%20Queries">Discovering
whether a Query has any Solutions</a></p>
      </li>
      <li>
        <p style="margin-bottom: 0cm;"><a href="#Terminating%20Queries">Terminating
Queries</a> </p>
      </li>
    </ul>
  </li>
  <li>
    <p style="margin-bottom: 0cm;"><a href="#Multi-Threaded%20Queries">Multi-Threaded
Queries</a> </p>
  </li>
  <li>
    <p style="margin-bottom: 0cm;"><a href="#Exceptions">Exceptions</a>
    </p>
  </li>
  <li>
    <p style="margin-bottom: 0cm;"><a href="#Debugging">Debugging</a> </p>
  </li>
  <li>
    <p style="margin-bottom: 0cm;"><a href="#Version">Version
information</a></p>
  </li>
  <li>
    <p><a href="#What%27s%20Missing">What's Missing</a> </p>
  </li>
</ul>
<hr size="4" noshade="noshade">
<h3><a name="Introduction"></a>Introduction</h3>
<p>The <b><i>JPL 3.0.1 Java-calls-Prolog API</i></b> provides a set
of classes that hide almost all of the messy detail in the <a
 href="low-level_interface.html">Low-Level
Interface</a>.&nbsp; It is less flexible than the Low-Level
Interface, but it also has less of a learning curve, and in many ways
is more natural and Prolog-like than the Low-Level Interface. </p>
<p>The Java package <b>jpl</b> contains all of the classes in this
interface.&nbsp; None of the classes correspond with any of the data
types in the Prolog Foreign Language Interface (FLI). <br>
&nbsp; </p>
<h3><a name="The Class Hierarchy"></a>The Class Hierarchy</h3>
<p>The <b><i>API</i></b> consists of the following class hierarchy: </p>
<pre style="margin-left: 3cm; margin-right: 3cm;">Term<br> |<br> +--- Variable<br> |<br> +--- Compound<br> |      |<br> |      +--- Atom<br> |<br> +--- Integer<br> |<br> +--- Float<br> <br>Query<br> <br>JPLException<br> |<br> +-- PrologException</pre>
<p><b>Term</b> is an abstract class: only its subclasses can be
instantiated. </p>
<p>Each instance of <b>Query</b> contains a <b>Term</b> (denoting the
goal which is to be proven), and much more besides. </p>
<p>Each instance of <b>Compound</b> has a (java.lang.String) name and
an array of (<b>Term</b>) arguments (it must have at least one). </p>
<p><b>Atom</b> is a specialisation of <b>Compound</b> with zero
arguments. <br>
&nbsp; </p>
<h3><a name="Initializing and Terminating Prolog"></a>Initializing
Prolog</h3>
<p>The <b>jpl.JPL</b> class initializes the Prolog VM (e.g.
<tt>libpl.dll</tt> in Win32), if necessary, when the first <b>Query</b>
is activated, using default parameter values.&nbsp; Before
initialization takes place, these default values can be read, and
altered. </p>
<pre style="margin-left: 1cm; margin-right: 1cm;">public String[] getDefaultInitArgs();<br>public void setDefaultInitArgs(String[] args);</pre>
<p>After initialization, the parameter values which were actually used
can be read. </p>
<pre style="margin-left: 1cm; margin-right: 1cm; margin-bottom: 0.5cm;">public String[] getActualInitArgs();</pre>
<p>(This method returns <tt>null</tt> if initialization has not
occurred, and thus it can be used as a test.) <br>
This allows Java
library classes to employ <b><i>JPL</i></b> without placing any
burden of initialization upon the applications which use them.&nbsp;
It can also ensure that the Prolog VM is initialized only if and when
it is
needed. </p>
<p>Explicit initialization is supported as in <b><i>JPL 1.0.1</i></b>:
</p>
<pre style="margin-left: 1cm; margin-right: 1cm;">public void init();<br>public void init( String args[] );</pre>
<p>Java code which requires a Prolog VM to be initialized in a
particular way can check whether initialization has already occurred:
if not, it can specify parameters and force it to be attempted; if
so, it can retrieve and check the initialisation parameters actually
used, to determine whether the initialization meets its requirements.
<br>
&nbsp; <br>
This version of <span style="font-weight: bold; font-style: italic;">JPL</span>
does not support reinitialization of a Prolog VM. </p>
<p>For details about the legal parameter values, see your local
Prolog documentation.&nbsp; Most users will rely on automatic
initialization. <br>
&nbsp; </p>
<h3><a name="Creating Terms"></a>Creating Terms</h3>
<p>The <b>Term</b>-based classes in the <span
 style="font-weight: bold;">jpl</span> package are best
thought of as a structured concrete syntax for Prolog terms: they do
not correspond to any particular terms within the Prolog engine;
rather, they are a means for constructing queries which can called
within Prolog, and they are also a means for representing (and
exploring) the results
of such calls. <br>
<b>Term</b> instances are never changed by any
activity within the Prolog engine: indeed; it doesn't know of their
existence. <br>
The <b>Term</b> class is
abstract, so it cannot be directly instantiated; to create a Term,
create an instance of one of its five subclasses. </p>
<blockquote><font size="2"><b><i>Note.</i></b> <i>A <b>Term</b> in
the <span style="font-weight: bold;">jpl</span> packagee is not to be
confused with a <b>term_t</b> in
the <span style="font-weight: bold;">jpl.fli</span> package.&nbsp; The
latter has an important internal role in
managing state in the Prolog stack; the former is just a data
structure in the Java heap.</i></font></blockquote>
<h4><a name="Atoms"></a>Atoms</h4>
<p>An <b>Atom</b> is a <b>Compound</b> with zero arguments.&nbsp; To
create an <b>Atom</b>, pass a (String) name to its constructor: </p>
<pre style="margin-left: 1cm; margin-right: 1cm;">Atom aristotle = new Atom("aristotle");<br>Atom alexander = new Atom("alexander");</pre>
<blockquote><i><font size="2"><b>Note.</b>&nbsp; Two <b>Atom</b>s by
the same name
are effectively identical.&nbsp; Feel free to reuse <b>Atom</b>
instances when constructing compound <b>Term</b>s.</font></i> <br>
  <i><font size="2"><b>Note.</b>&nbsp;
The name in an <b>Atom</b> need not be lower case: it can be any
UTF-8 string (?).</font></i></blockquote>
<p>The <b>Atom</b> class inherits <b>Compound</b>'s <b>name()</b>
accessor to obtain the name of the <b>Atom</b> (it also inherits
<b>Compound</b>'s <b>arity()</b> accessor, but this always returns
zero for an <b>Atom)</b>.&nbsp; <span style="font-weight: bold;">Atom</span>'s
<span style="font-weight: bold;">toString()</span> method yields a
String form of the atom's name which is quoted, iff necessary,
according to Prolog source text syntax, and can thus be used when
constructing fragments of Prolog source text, e.g. new queries.<br>
</p>
<h4><a name="Variables"></a>Variables</h4>
<p><b>Variable</b>s have identifying
names, which must comply with conventional Prolog source text syntax. </p>
<pre style="margin-left: 1cm; margin-right: 1cm; margin-bottom: 0.5cm;">Variable X = new Variable("X"); // a regular variable</pre>
<pre style="margin-left: 1cm; margin-right: 1cm; margin-bottom: 0.5cm;">Variable X = new Variable("_"); // an "anonymous" variable</pre>
<pre style="margin-left: 1cm; margin-right: 1cm; margin-bottom: 0.5cm;">Variable X = new Variable("_Y"); // a "dont-tell-me" variable, whose bindings we don't want to know</pre>
<h4><a name="Integers"></a>Integers</h4>
<p>An <b>Integer</b> is a specialized <b>Term</b> that holds a Java <b>long</b>
value.&nbsp; This class corresponds to the Prolog <i>integer</i>
type (SWI-Prolog integers are 32-bit for now, but we are looking ahead
and beyond...). </p>
<pre style="margin-left: 1cm; margin-right: 1cm; margin-bottom: 0.5cm;">jpl.Integer i = new jpl.Integer(5);</pre>
<p>Be careful to avoid confusion with <tt>java.lang.integer</tt>, e.g.
by always qualifying the class name as in the example above. </p>
<p>The <b>jpl.Integer</b> class has an <b>intValue()</b> accessor to
obtain the <b>int</b> value of an instance, and also <span
 style="font-weight: bold;">longValue()</span>, <span
 style="font-weight: bold;">floatValue()</span> and <span
 style="font-weight: bold;">doubleValue()</span> (just like
java.lang.Integer has). </p>
<h4><a name="Floats"></a>Floats</h4>
<p>A <b>Float</b> is a specialized <b>Term</b> that holds a Java
<b>double</b> value.&nbsp; This class corresponds to the Prolog float
type (SWI-Prolog floats are 64-bit ISO/IEC), on which arithmetic
operations can be performed. </p>
<pre style="margin-left: 1cm; margin-right: 1cm; margin-bottom: 0.5cm;">jpl.Float f = new jpl.Float(3.14159265);</pre>
<p>As with integers, avoid confusion between <b>jpl.Float</b> and
<b>java.lang.Float</b>. </p>
<p>The <b>jpl.Float</b> class has a <b>doubleValue()</b> accessor to
obtain
the <b>double</b> value of an instance, and also a <span
 style="font-weight: bold;">floatValue()</span> accessor. </p>
<h4><a name="Compound Terms"></a>Compounds</h4>
<p>A <b>Compound</b> is a <b>Term</b> that contains a name and a
sequence (array) of <b>Term</b> arguments, as reflected in this
class's constructor: </p>
<pre style="margin-left: 1cm; margin-right: 1cm;">Compound teacher_of = new Compound(<br> "teacher_of",<br> new Term[] {<br> new Atom("aristotle"),<br> new Atom("alexander")<br> }<br> );</pre>
<p>Note the use of Java's <i>anonymous array</i> syntax</p>
<pre style="margin-left: 0.85cm; margin-bottom: 0.5cm;">new Term[] { ..., ... }</pre>
<p>to specify the arguments (any quantity &gt;= 1) of the <b>Compound</b>.<br>
<br>
In
this example, the Java variable <tt>teacher_of</tt> refers to a
<b>Compound</b> instance, which represents the Prolog term
<i>teacher_of(aristotle,alexander).</i> </p>
<blockquote><a name="Note.vars"></a><font size="2"><i><b>Note.</b> Care
should be taken in creating <b>Compound</b> <b>Term</b>s, especially
if <b>Variable</b> references are used.&nbsp; For example, the
following construction:</i></font></blockquote>
<pre style="margin-left: 2cm; margin-right: 2cm;"><font size="2"><i>Variable X = new Variable();</i></font>
<font size="2"><i>Variable Y = new Variable();</i></font>
<font size="2"><i>Compound father_of = new Compound( "teacher_of", new Term[]{X,Y});</i></font></pre>
<blockquote><font size="2"><i>corresponds with the Prolog term </i><font
 face="monospace">teacher_of(X,Y)</font><i>,
whereas</i></font> </blockquote>
<pre style="margin-left: 2cm; margin-right: 2cm;"><font size="2"><i>Variable X = new Variable();</i></font>
<font size="2"><i>Compound father_of = new Compound( "teacher_of", new Term[]{X,X});</i></font></pre>
<blockquote><font size="2"><i>corresponds with the Prolog term </i><font
 face="monospace">teacher_of(X,X)</font><i>,
two terms that can resolve very differently depending on the Prolog
database.&nbsp; The general rule of thumb should be, reuse </i><b>Term</b>
  <i>references that are or contain <b>Variable</b>s only if you know
that that is what you mean.</i></font></blockquote>
<p>To obtain the (String) name of a <b>Compound</b>, use the <b>name()</b>
accessor method. </p>
<pre style="margin-left: 1cm; margin-right: 1cm; margin-bottom: 0.5cm;">public String name();</pre>
<p>To obtain the arity of a <b>Compound</b>, use the <b>arity()</b>
accessor method. </p>
<pre style="margin-left: 1cm; margin-right: 1cm; margin-bottom: 0.5cm;">public int arity();</pre>
<p>To obtain an array of a <b>Compound</b>'s arguments, use the <b>args()</b>
accessor method. </p>
<pre style="margin-left: 1cm; margin-right: 1cm; margin-bottom: 0.5cm;">public Term[] args();</pre>
<p>To obtain the <i>ith</i> argument of a compound (numbered from 1),
use the <b>arg() </b>accessor method (with an <b>int </b>parameter
value between 1 and Arity inclusive). </p>
<pre style="margin-left: 1cm; margin-right: 1cm; margin-bottom: 0.5cm;">public Term arg( int i);</pre>
<p>To obtain the <i>ith</i> argument of a compound (numbered from 0),
use the <b>arg0() </b>accessor method (with an <b>int </b>parameter
value between 0 and Arity-1 inclusive). </p>
<pre style="margin-left: 1cm; margin-right: 1cm; margin-bottom: 0.5cm;">public Term arg0( int i);</pre>
<h3><a name="Creating Queries"></a>Queries</h3>
<p>A <b>Query</b> contains a <b>Term</b>, representing a Prolog goal:
</p>
<pre style="margin-left: 1cm; margin-right: 1cm;">Term goal = new Compound( "teacher_of", new Term[]{new Atom("aristotle"),new Atom("alexander")});<br>Query q = new Query( goal );</pre>
<p>The <b>Query</b> <b>q</b> in this example represents the Prolog
query
</p>
<blockquote><i>?- teacher_of(aristotle,alexander).</i></blockquote>
<h3><a name="The Util Class"></a>The Util Class</h3>
<p>The <b>Util</b> class provides various static utility methods for
managing <b><i>JPL</i></b> <b>Term</b>s. </p>
<pre style="margin-left: 1cm; margin-right: 1cm; margin-bottom: 0.5cm;">Term termArrayToList( Term t[])<br>Term[] listToTermArray( Term t)<br>Term[] bindingsToTermArray( Hashtable bs)</pre>
<h3><a name="Querying Prolog"></a>Querying Prolog</h3>
<p>To ask the Prolog engine a query via the High-Level Interface, one
first constructs a <b>Query</b> instance, as in the above example,
and then uses the <b>java.util.Enumeration</b> interface, which the
<b>Query</b> class implements, to obtain solutions (where a
"solution" is what is known in logic programming jargon as
a <i>substitution</i>, which is a collection of <i>bindings</i>, each
of which relates one of the <b>Variables</b> within the <b>Query</b>'s
goal to a <b>Term</b> representation of the Prolog term to which the
corresponding Prolog variable was bound by the proof). </p>
<pre style="margin-left: 1cm; margin-right: 1cm;">public interface Enumeration {<br> public boolean hasMoreElements();<br> public Object nextElement();<br>}</pre>
<p>The <b>hasMoreElements()</b> method can be used to determine
whether
a <b>Query</b> has any (or any further) solutions.&nbsp; In the above
example, the method call </p>
<pre style="margin-left: 1cm; margin-right: 1cm; margin-bottom: 0.5cm;">q.hasMoreElements()</pre>
<p>returns <b>true</b> if the Prolog query <i>teaches(aristotle,alexander)</i>
is provable, and <b>false</b> otherwise.&nbsp; In this example, the
Prolog query is a ground term, so the "solution" to the
<b>Query</b> is merely a truth value, and is given by the
<b>hasMoreElements()</b> method. </p>
<p>Where a <b>Query</b>'s goal contains <b>Variable</b>s, on the
other hand, its execution yields a sequence of bindings of these
<b>Variable</b>s to <b>Term</b>s.&nbsp; The High-Level interface uses
a <b>java.util.Hashtable</b> to represent these bindings; the <b>Object</b>s
in the table are <b>Term</b>s, keyed (uniquely) by <b>Variable</b>
instances. </p>
<p>For example, to print all of Aristotle's pupils, i.e., all the
bindings of <b>X</b> which satisfy <i>teaches(aristotle,X)</i>, one
could write </p>
<pre style="margin-left: 1cm; margin-right: 1cm;">Variable X = new Variable();<br>Query q = new Query( "teaches", new Term[]{new Atom("aristotle"),X});<br>while ( q.hasMoreElements() ) {<br> Hashtable binding = (Hashtable) q.nextElement();<br> Term t = (Term) binding.get( X);<br> System.out.println( t);<br>}</pre>
<blockquote><i><font size="2"><b>Note.</b>&nbsp; If a <b>Query</b>'s
goal contains
no variables (i.e. it is "ground"), the <b>Query.
nextElement() </b>method will still return a <b>Hashtable</b> for
each solution, although each table will be empty.</font></i> <br>
  <i><font size="2"><b>Note.</b>&nbsp;
If a <b>Query</b>'s goal contains more than one occurrence of some <b>Variable</b>,
then each&nbsp; solution <b>Hashtable</b> will have
only one binding for that <b>Variable</b>.</font></i></blockquote>
<p>For convenience, the <b>Query</b> class provides a
<b>hasMoreSolutions()</b> and <b>nextSolution() </b>method with the
following signatures: </p>
<pre style="margin-left: 1cm; margin-right: 1cm;">public boolean hasMoreSolutions();<br>public Hashtable nextSolution();</pre>
<p>Using the <b>nextSolution()</b> method avoids having to cast the
result of the <b>nextElement() </b>method to <b>Hashtable</b>. </p>
<h4><a name="Obtaining one Solution"></a>Obtaining one Solution</h4>
<p>Often, you'll just want just the first solution to a query.&nbsp;
The <b>Query</b> class provides a method for this: </p>
<pre style="margin-left: 1cm; margin-right: 1cm; margin-bottom: 0.5cm;">public Hashtable oneSolution();</pre>
<p>If the <b>Query</b> has no solutions, this method returns <b>null</b>;
otherwise, a non-null return indicates success.&nbsp; If the <b>Query</b>
is a ground query (i.e. contains no variables), the returned
<b>Hashtable</b> will be empty (i.e. will contain no bindings). </p>
<h4><a name="Obtaining all Solutions"></a>Obtaining all Solutions</h4>
<p>You may want all solutions to a query.&nbsp; The <b>Query</b>
class provides a method for this: </p>
<pre style="margin-left: 1cm; margin-right: 1cm; margin-bottom: 0.5cm;">public Hashtable[] allSolutions();</pre>
<p>The returned array will contain all the <b>Query</b>'s solutions,
in
the order they were obtained (as with Prolog's findall/3, duplicates
are not removed).&nbsp; If the <b>Query</b> has no solutions, this
method returns an empty array (N.B. not <b>null</b> as in <b>JPL
1.0.1</b>). </p>
<h4><a name="Ground Queries"></a>Discovering whether a query has any
solutions</h4>
<p>Sometimes an application is interested only in whether or not a
query is provable, but not in any details of its possible
solutions.&nbsp;
The <b>Query</b> class provides the <b>hasSolution</b> method for
this common special case: </p>
<pre style="margin-left: 1cm; margin-right: 1cm; margin-bottom: 0.5cm;">public boolean hasSolution();</pre>
<p>This method is equivalent to (but sometimes more efficient than)
calling <b>oneSolution</b> and asking whether the return value is
non-<b>null</b> (i.e. whether the query succeeded). </p>
<h4><a name="Terminating Queries"></a>Terminating Queries</h4>
<p>Queries terminate automatically when the <b>hasMoreSolutions()
</b>method returns <b>false</b>, and once a <b>Query</b> is
terminated, another can be started.&nbsp; Unfortunately, the Prolog
engine is currently such that it can handle only one query at a
time.&nbsp; As a result, <i>it is not possible, in the High-Level
Interface, to ask two different <b>Query</b> objects whether they
have any solutions without first exhausting all of the solutions of
one.</i>&nbsp; Therefore, programmers must take care to ensure that
all solutions are exhausted before starting new queries.&nbsp; This
has particular importance in multi-threaded contexts, but it can also
present difficulties even in single-threaded programs.&nbsp; See the
<a href="#Multi-Threaded%20Queries">Multi-Threaded Queries</a> section
for a discussion of how to manage Queries in multi-threaded contexts.
</p>
<p>To terminate a <b>Query</b> before all of its solutions have been
exhausted, use the <b>rewind()</b> method: </p>
<pre style="margin-left: 1cm; margin-right: 1cm; margin-bottom: 0.5cm;">public void rewind();</pre>
<p>This method stops a <b>Query</b>, setting it back into a state
where
it can be restarted.&nbsp; It also permits other queries to be
started.&nbsp; Here is an example in which the first three solutions
to the <b>Query</b> are obtained: </p>
<pre style="margin-left: 1cm; margin-right: 1cm;">Query query = // obtain Query somehow<br>for ( int i = 0; i &lt; 3 &amp;&amp; query.hasMoreSolutions(); ++i ){<br> Hashtable solution = query.nextSolution();<br> // process solution...<br>}<br>query.rewind();</pre>
<p>You may call <b>rewind()</b> on an inactive <b>Query</b> without
ill-effect, and you should <i>always</i> call rewind if you have not
exhausted all solutions to a <b>Query</b>. </p>
<p>If you are using the <b>query()</b>, <b>oneSolution()</b>, or
<b>allSolutions() </b>methods, you need not worry about rewinding the
<b>Query</b>; it is done automatically for you. <br>
&nbsp; </p>
<h3><a name="Multi-Threaded Queries"></a>Multi-Threaded Queries</h3>
<p>The Prolog engine can only have one query open at a time.&nbsp;
This presents difficulties for multi-threaded programs in which the
programmer has no control over when Queries are executed.&nbsp; <b><i>JPL</i></b>
makes as much of the High-Level Interface thread-safe as it can.&nbsp;
Unfortunately, the programmer must take responsibility in a limited
set of circumstances to ensure that all calls to the High-Level
Interface are thread safe. </p>
<p>It is worth noting that if the programmer confines use of Query
methods to <b>hasSolution()</b>, <b>oneSolution()</b>, and
<b>allSolutions()</b>, that subset of the <b>Query</b> interface <i>is</i>
thread-safe.&nbsp; For many programmers, these methods suffice.&nbsp;
However, if the <b>hasMoreSolutions()</b>, <b>hasMoreElements()</b>,
<b>nextSolution()</b>, <b>nextElement()</b>, or <b>rewind()</b>
methods are explicitly invoked, thread-safety is lost.&nbsp; The
problem is that while the blocks of these programs are synchronized
so that in effect no two <b>Query</b> objects can invoke any of these
methods concurrently, there is nothing that prevents a <b>Query</b>
object in one thread from calling one of these methods, and another
<b>Query</b> object in a different thread from calling this same
method, or even another that could produce indeterminate results. </p>
<p>The <b>Query</b> class, however, does make synchronization around
these methods possible by providing a reference to the monitor object
that locks competing threads from executing critical code.&nbsp; The
reference is obtained by the static method </p>
<pre style="margin-left: 1cm; margin-right: 1cm; margin-bottom: 0.5cm;">public static Object lock();</pre>
<p>Thus, programmers can wrap calls to these non-thread-safe methods in
synchronized blocks, using the lock object to prevent other threads
from entering any of these methods.&nbsp; To write a thread-safe loop
to process all of a <b>Query</b>'s solutions, for example, one might
write </p>
<pre style="margin-left: 1cm; margin-right: 1cm;">Query query = // obtain Query<br>synchronized ( Query.lock() ){<br> while ( query.hasMoreSolutions() ){<br> Hashtable solution = query.nextSolution();<br> // process solution...<br> }<br>}</pre>
<p><br>
Note that the <b>query()</b>, <b>oneSolution()</b>, and
<b>allSolutions() </b>methods effectively do the same as the above
code snippet, so there is no need to explicitly synchronized on the
<b>Query</b>'s monitor object when these methods are called. <br>
&nbsp;
</p>
<h3><a name="Exceptions"></a>Exceptions</h3>
<p>The <b><i>JPL</i></b> package provides fairly crude exception
handling.&nbsp; The base class for all <b><i>JPL</i></b> Exceptions
is <b>JPLException</b>, which is a <b>java.lang.RuntimeException</b>
(and hence need not be declared), and which will be thrown in the
absence of any other kind of exception that can be thrown, usually as
the result of some programming error.&nbsp; Converting the exception
to a <b>java.lang.String</b> should provide some descriptive
information about the reason for the error.&nbsp; All other <b><i>JPL</i></b>
excpetion classes extend this class.&nbsp; Currently there are two,
the <b>QueryInProgressException</b> class and the <b>PrologException</b>
class. </p>
<p>A <b>QueryInProgressException</b> is thrown when a <b>Query</b> is
opened while another is in progress; this exception can be caught in
multi-threaded situations, but a better strategy for managing
multi-threaded situations is discussed in the <a
 href="#Multi-Threaded%20Queries">Multi-Threaded
Queries</a> section.&nbsp; If you obey the rules discussed in this
section, you should have no reason to catch this exception. </p>
<p>A <b>PrologException</b> is thrown either during execution of a
Prolog built-in predicate or by an explicit call, by Prolog
application code, of the Prolog predicate <i>throw/1.</i> </p>
<p>There is currently no means of gracefully handling exceptions
caused by malformed parameters (e.g., undefined predicates) passed
through the High-Level Interface to the Prolog engine (?). <br>
&nbsp;
</p>
<h3><a name="Debugging"></a>Debugging</h3>
<p>Each <b>Term</b> type (together with the <b>Query</b> class)
supports an implementation of <b>toString()</b> which returns a
more-or-less familiar Prolog textual representation of the <b>Term</b>
or <b>Query</b>. </p>
<p>Sometimes, however, this information is not sufficient, so we have
provided a method <b>debugString()</b> which provides a more verbose
and explicit representation, including the types (atom, integer etc)
of each term and subterm. </p>
<p>In general, <b>Term</b> and <b>Query</b> instances are represented
in the form (<i>type data</i>), where <i>type</i> is the name of the
type (e.g., <b>Atom</b>, <b>Compound</b>, <b>Tuple</b>, etc.), and
<i>data</i> is a representation of the contents of the <b>Term</b>.&nbsp;
For example, if the <b>Term</b> is an <b>Atom</b>, the data is the
<b>Atom</b>'s name.&nbsp; The arguments of <b>Compounds</b> are
represented by comma-separated lists within square brackets ('['
']'). </p>
<p>Viewing the structure of a <b>Term</b> or <b>Query</b> can be
useful in determining whether an error lies on the Prolog or Java
side of your JPL applications. </p>
<p>Perhaps better still, <b>Term</b> implements (in a basic but
adequate way) the <b>javax.swing.TreeModel</b> interface, and its
<b>display()</b> method creates a <b>JFrame</b> containing a
browseable <b>JTree</b> representation of the term. <br>
&nbsp; </p>
<h3><a name="Version"></a>Version information</h3>
<p>To obtain the current version of <b><i>JPL</i></b> you are using,
you may obtain a reference to the <b>jpl.Version</b> static instance
of the <b><i>JPL</i></b> class by calling the <b>JPL.version() </b>static
method.&nbsp; This will return a <b>jpl.Version</b> structure, which
has the following final fields: </p>
<pre style="margin-left: 1cm; margin-right: 1cm;">package jpl;<br>public class Version {<br> public final int major;                // e.g. 2<br> public final int minor;                // e.g. 0<br> public final int patch;                // e.g. 2<br> public final java.lang.String status;  // e.g. "alpha"<br>}</pre>
<p>You may wish to use this class instance to obtain fine-grained
information about the current JPL version, e.g.</p>
<pre style="margin-left: 0.85cm; margin-bottom: 0.5cm;">if ( JPL.version().major == 2 ) {</pre>
<p>You may also simply call the <b>version_string() </b>static method
of
the <b>jpl.JPL</b> class.&nbsp; This will return a <b>java.lang.String</b>
representation of the current <b><i>JPL</i></b> version. </p>
<p>The version string can be written to the standard output stream by
running the <b>main()</b> method of the <b>jpl.JPL</b> class. </p>
<pre style="margin-left: 1cm; margin-right: 1cm;">linux% java jpl.JPL<br>JPL 2.0.2-alpha</pre>
<h3><a name="What's Missing"></a>What's Missing</h3>
<p style="margin-bottom: 0cm;">The current implementation of the
High-Level Interface lacks support for modules, and for multiple
Prolog engines.<br>
&nbsp; <br>
&nbsp; </p>
<hr size="4" noshade="noshade">
<p align="right" style="margin-bottom: 0cm;"><a href="index.html">up</a>&nbsp;&nbsp;
<a href="low-level_interface.html">prev</a>&nbsp; next&nbsp; <a
 href="api/packages.html">API</a></p>
</body>
</html>
