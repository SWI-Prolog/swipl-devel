<!DOCTYPE html PUBLIC "-//w3c//dtd html 4.0 transitional//en">
<html>
<head>
  <meta http-equiv="Content-Type"
 content="text/html; charset=iso-8859-1">
  <meta name="GENERATOR"
 content="Mozilla/4.74 [en] (WinNT; U) [Netscape]">
  <title>JPL release notes</title>
</head>
<body>
<h1><span style="font-style: italic;">
JPL</span> release notes</h1>
<ul>
  <li><a href="#JPL_3.0.3_Java_API">JPL 3.0.3 Release Notes</a></li>
  <li><a href="#JPL_3.0.2_Java_API">JPL 3.0.2 Release Notes</a></li>
  <li><a href="#JPL_3.0.0_Java_API">JPL 3.0.0 Java API Release Notes</a><br>
  </li>
  <li><a href="#JPL_2.0.2_Java_API">JPL 2.0.2 Java API Release Notes</a></li>
</ul>
<br>
<hr style="width: 100%; height: 4px;">
<h2><span style="font-style: italic;"><a name="JPL_3.0.3_Java_API"></a>JPL
3.0.3</span> Release Notes</h2>
<h3>Changes within the distribution</h3>
<ul>
  <li>the <small style="font-weight: bold;"><span
 style="font-family: helvetica,arial,sans-serif;">demo</span></small>
folder has been renamed <small style="font-weight: bold;"><span
 style="font-family: helvetica,arial,sans-serif;">examples</span></small>
(more idiomatic)(?) and its contents have been moved into a new <small><span
 style="font-family: helvetica,arial,sans-serif; font-weight: bold;">java</span></small>
folder, which is accompanied by a new <small><span
 style="font-family: helvetica,arial,sans-serif; font-weight: bold;">prolog</span></small>
folder for Prolog examples</li>
</ul>
<h3>Java API changes</h3>
<ul>
  <li>to simplify the construction of queries, the <span
 style="font-weight: bold;">Query(java.lang.String)</span> constructor
now parses its string arg as if it were Prolog source text, and
constructs a new query whose goal is the resulting term.&nbsp; This is
backwards compatible with (all but very unusual) previous usage, e.g.</li>
</ul>
<pre style="margin-left: 80px;">new Query("statistics")<br></pre>
<div style="margin-left: 40px;">and allows arbitrarily complex goals to
be created textually, e.g.<br>
<pre style="margin-left: 40px;">new Query("setof(_A,current_atom(_A),_As),length(_As,N)")<br></pre>
NB <span style="font-family: monospace;">_A</span> and <span
 style="font-family: monospace;">_As</span> are <span
 style="font-style: italic;">dont-tell-me</span> variables (this
property is determined by their initial underscore), whose bindings are
by default not returned when the query is called (saving computational
time and space).&nbsp; This behaviour can be overridden (globally) with<br>
<pre style="margin-left: 40px;">jpl.JPL.setDTMMode( false)<br></pre>
to
allow Java+<span style="font-weight: bold; font-style: italic;">JPL</span>+Prolog
implementation of a Prolog IDE which emulates the behaviour of the
traditional top-level interpreter.&nbsp; <br>
</div>
<ul>
  <li>to further simplify construction of queries, the <span
 style="font-weight: bold;">Query(java.lang.String <span
 style="font-style: italic;">text</span>, jpl.Term[] <span
 style="font-style: italic;">args</span>)</span> constructor now parses
its <span style="font-weight: bold; font-style: italic;">text</span>
argument as a Prolog source text fragment; if it represents an atom,
the constructor behaves as before (building a <span
 style="font-weight: bold;">Compound</span> goal from the given name
and args), but if it represents a compound term with one or more atomic
subterms whose names are a single <span style="font-style: italic;">questionmark</span>
character, e.g.</li>
</ul>
<pre style="margin-left: 80px;">"setof(P,mypred(?,P,?),Ps), length(Ps,?)"<br></pre>
<div style="margin-left: 40px;">and the <span
 style="font-weight: bold; font-style: italic;">args</span> comprise as
many terms as there are questionmarks, then the new query's goal is a
rewriting of <span style="font-weight: bold; font-style: italic;">text</span>'s
term, with each questionmark replaced by the corresponding element of <span
 style="font-weight: bold; font-style: italic;">args</span>.&nbsp; This
is designed to mimic the established and useful idiom of passing
parameters into SQL <span style="font-style: italic;">prepared
statements</span>.&nbsp; It allows all the constant parts of a
parameterised query to be defined textually.<br>
<br>
</div>
<address><a href="mailto:paul.singleton@bcs.org.uk">Paul Singleton</a></address>
<address>Friday12th March 2004<br>
&nbsp;<br>
</address>
<hr style="width: 100%; height: 4px;">
<h2><span style="font-style: italic;"><a name="JPL_3.0.2_Java_API"></a>JPL
3.0.2</span> Release Notes</h2>
<h3>Changes within the distribution</h3>
<ul>
  <li><span style="font-style: italic;">new classes folder:</span> the
root directory of the distribution now contains
a classes folder, holding copies of the jpl.* and jpl.fli.* class files</li>
  <li><span style="font-style: italic;">new demo:</span> in
demo/FamilyMT is a new variant of the Family demo which
exercises multiple Prolog engines (from a&nbsp; shared pool) called by
multiple Java threads.</li>
</ul>
<h3>C library changes:</h3>
<ul>
  <li>lots of refactoring and tidying in preparation for porting (to
Linux+gcc initially)</li>
  <li>added Prolog "foreign" functions <span style="font-weight: bold;">jpl_c_lib_version/1</span>
and <span style="font-weight: bold;">jpl_c_lib_version/4</span> for
making library version identification available to Prolog</li>
  <li>added Java "native" method <span style="font-weight: bold;">get_string_chars()</span>,
needed if Prolog returns a string to Java (which it does sometime even
if you don't want it to)</li>
  <li>commented out various unused functions</li>
  <li>added Java "native" method <span style="font-weight: bold;">action_abort()</span>
but it doesn't work yet...</li>
  <li>added support for new <span style="font-weight: bold;">jpl.JRef</span>
type<br>
  </li>
</ul>
<h3>Java API changes<br>
</h3>
<ul>
  <li>altered the semantics of <span style="font-weight: bold;">Variable</span>
to be a purely lexical entity; a <span style="font-weight: bold;">Variable</span>
instance should be created with a name which is valid in Prolog source
syntax; the argumentless <span style="font-weight: bold;">Variable()</span>
constructor is currently deprecated and constructs a variable with a
"new" name in the sequence "_1", "_2" etc. so as not to completely
break older programs</li>
  <li>bindings from successful calls are now keyed by the <span
 style="font-style: italic;">names</span> of the variables, rather than
by <span style="font-weight: bold;">Variable</span> objects
themselves; this is part of a revamp to allow goals to be defined by
source text fragments<br>
  </li>
  <li>implemented these methods for <span style="font-style: italic;">all</span>
    <span style="font-weight: bold;">Term</span> subclasses (to
simplify coding of term checking and traversal etc.):</li>
  <dl>
    <dt><span style="font-weight: bold;">type()</span></dt>
  </dl>
  <dl>
    <dd>returns <span style="font-weight: bold;">jpl.fli.Prolog.ATOM</span>,
      <span style="font-weight: bold;">COMPOUND</span>, <span
 style="font-weight: bold;">FLOAT</span>, <span
 style="font-weight: bold;">INT</span> or <span
 style="font-weight: bold;">VARIABLE</span><br>
    </dd>
    <dt><span style="font-weight: bold;">hasFunctor( String <span
 style="font-style: italic;">name</span>, int <span
 style="font-style: italic;">arity</span>) </span></dt>
    <dd>fails unless called appropriately; <br>
    </dd>
    <dt style="font-weight: bold;">intValue()<br>
longValue()<br>
floatValue()<br>
doubleValue()</dt>
    <dd><span style="font-weight: normal;">yield
the Java int value (or long, float or double value respectively) of an <span
 style="font-weight: bold;">Integer</span> or <span
 style="font-weight: bold;">Float</span> instance; each will throw an
exception
for <span style="font-weight: bold;">Atom</span>, <span
 style="font-weight: bold;">Compound</span> and <span
 style="font-weight: bold;">Variable</span> instances; the names of
these methods follow the precedent set by <span
 style="font-weight: bold;">java.lang.Integer</span> etc. and remember
that a Prolog integer is not equivalent to a Java int (it may be longer
or shorter), nor is a Prolog float equivalent to a Java float (it may
have a different precision)</span></dd>
    <dd><ddstyle ="margin-left: 80px;"> </ddstyle></dd>
    <dt><span style="font-weight: bold;">arg( int <span
 style="font-style: italic;">argNo</span>)</span><br>
    </dt>
    <dd><span style="font-weight: normal;">calling </span><span
 style="font-weight: bold;">arg()</span><span
 style="font-weight: normal;"> inappropriately (i.e. for </span><span
 style="font-weight: bold;">jpl.Atom</span><span
 style="font-weight: normal;">, </span><span style="font-weight: bold;">jpl.Integer</span><span
 style="font-weight: normal;">, </span><span style="font-weight: bold;">jpl.Float</span><span
 style="font-weight: normal;"> and </span><span
 style="font-weight: bold;">jpl.Variable</span><span
 style="font-weight: normal;"> instances) throws a runtime exception
but is not a compile-time error; this method considers the args to be
numbered from 1 upwards (i.e. Prolog convention, not Java array
convention)</span></dd>
    <dt><span style="font-weight: normal;"><span
 style="font-weight: bold;">name()</span></span></dt>
  </dl>
  <dl>
    <dd><span style="font-weight: normal;"><span
 style="font-weight: bold;">Variable.name()</span> returns the
variable's lexical name, <span style="font-weight: bold;">Atom.name()</span>
and <span style="font-weight: bold;">Compound.name()</span> behave as
before, <span style="font-weight: bold;">Integer.name()</span> and <span
 style="font-weight: bold;">Float.name()</span> each throw an exception
if called (but are valid at compile time)<br>
      </span></dd>
  </dl>
  <li>altered these methods for all <span style="font-weight: bold;">Term</span>
subclasses:</li>
  <ul>
    <li><span style="font-weight: bold;">toString()</span> now yields
a valid (quoted if necessary) Prolog source text representation<br>
    </li>
  </ul>
  <li>deprecated <span style="font-weight: bold;">Compound.arg0()</span>
and all *<span style="font-weight: bold;">.debugString()</span> methods</li>
  <li>deprecated <span style="font-weight: bold;">Float.value()</span>
(see <span style="font-weight: bold;">floatValue()</span> and <span
 style="font-weight: bold;">doubleValue()</span>)<br>
  </li>
  <li><span style="font-weight: bold;">jpl.Integer</span> now holds a <span
 style="font-weight: bold;">long</span> value (to allow for
other/future Prolog implementations with &gt;32-bit integers) but this
is backwards compatible if I understand Java correctly...<br>
  </li>
  <li>deprecated <span style="font-weight: bold;">jpl.Halt()</span>
pending a rethink about how best to clean up and terminate a hybrid
Java+Prolog application<span style="font-weight: bold;"><br>
    </span></li>
  <li>added <span style="font-weight: bold;">Query.abort()</span> but
it doesn't work yet...<br>
  </li>
</ul>
<br>
<address><a href="mailto:paul.singleton@bcs.org.uk">Paul Singleton</a></address>
<address>Sunday 22nd February 2004<br>
&nbsp;
<br>
</address>
<hr style="width: 100%; height: 4px;">
<h2><span style="font-style: italic;"><a name="JPL_3.0.0_Java_API"></a>JPL
3.0.0</span> Release Notes</h2>
<span style="font-style: italic;">This
release is a
work-in-progress, and
is being made available only to a few enthusiasts who don't mind the
likelihood that the API will change before 3.x becomes stable.</span><br>
<h3>Java API: new Variable semantics<br>
</h3>
<blockquote>A <span style="font-weight: bold;">Variable</span> must be
created with a name, e.g.<span style="font-family: monospace;"><br>
  </span>
  <pre style="margin-left: 40px;"><span style="font-family: monospace;">new Variable("X")<br></span></pre>
or as an anonymous variable<br>
  <pre style="margin-left: 40px;"><span style="font-family: monospace;">new Variable("_")</span></pre>
</blockquote>
<div style="margin-left: 40px;">or as a <span
 style="font-style: italic;">dont-tell-me</span> variable<br>
</div>
<blockquote>
  <pre style="margin-left: 40px;"><span style="font-family: monospace;">new Variable("_Q")</span></pre>
</blockquote>
<div style="margin-left: 40px;">Each binding within a solution is now
indexed by the <span
 style="text-decoration: underline; font-style: italic;">name</span> of
its associated <span style="font-weight: bold;">Variable</span>, hence<br>
</div>
<blockquote>
  <pre style="margin-left: 40px;">solution.get("X")<br></pre>
</blockquote>
<div style="margin-left: 40px;">New variables returned in bindings are
given new, sequential names, e.g. "_283".<br>
&nbsp;<br>
Each <span style="font-weight: bold;">Variable </span>instance within
a Java application is just a lexical token in the alternative Prolog
concrete syntax which <span style="font-weight: bold;">Term </span>and
its subclasses comprise.&nbsp; Two instances of <span
 style="font-family: monospace;">Variable("X")</span> are no different
from one shared instance: you are free to reuse such lexical elements,
but this has nothing to do with the sharing of variables which can
occur within a Prolog engine.<br>
&nbsp;<br>
The bindings of anonymous and <span style="font-style: italic;">dont-tell-me</span>
variables (i.e. those whose names begin with an underscore character)
are not returned to Java: use them to avoid the computational time and
space costs of constructing <span style="font-weight: bold;">Term</span>
representations of bindings in which you are not interested.<br>
</div>
<h3>Java API: easier Term and Query construction</h3>
<blockquote>Now that <span style="font-weight: bold;">Variable</span>s
are named, and bindings are keyed by the names of variables, it is
easier to construct <span style="font-weight: bold;">Term</span> (and
hence <span style="font-weight: bold;">Query</span>) instances.<br>
&nbsp;<br>
This utility (NB liable to be renamed or moved into a different class)
converts a valid Prolog source text representation of a term into a
corresponding Term hierarchy:<br>
  <pre style="margin-left: 40px;">Term jpl.Util.textToTerm( String sourcetext)<br></pre>
A new (in JPL 3.0.0) <span style="font-weight: bold;">Query</span>
constructor<br>
  <pre style="margin-left: 40px;">Query( String sourcetext)<br></pre>
allows queries to be created from source text, e.g.<br>
  <pre style="margin-left: 40px;">new Query("findall(_A,current_atom(_A),_As),length(_As,N)")<br></pre>
and <span style="font-family: monospace;">oneSolution()</span>, <span
 style="font-family: monospace;">allSolutions()</span> and <span
 style="font-family: monospace;">nextSolution()</span> will return
bindings of <span style="font-family: monospace; font-weight: bold;">N</span>
(but not of the <span style="font-style: italic;">dont-tell-me</span>
variables <span style="font-family: monospace; font-weight: bold;">_A</span>
and<span style="font-family: monospace; font-weight: bold;"> _As</span>),
e.g.<br>
  <pre style="margin-left: 40px;">q.oneSolution().get("N")<br></pre>
returns a <span style="font-weight: bold;">jpl.Integer</span>
representing the Prolog integer value to which <span
 style="font-family: monospace;">N</span> was bound by the successful
call of the query.<br>
</blockquote>
<h3>Java API: deprecated methods<br>
</h3>
<ul>
  <li><span style="font-weight: bold;">Query</span>.query()</li>
</ul>
<div style="margin-left: 80px;">use <span style="font-weight: bold;">Query</span>.hasSolution()
instead<br>
</div>
<ul>
  <li><span style="font-weight: bold;">Query</span>.rewind()</li>
</ul>
<div style="margin-left: 80px;">use <span style="font-weight: bold;">Query</span>.close()
instead<br>
</div>
<h3>Java API: fixes</h3>
<div style="margin-left: 40px;">array methods inherited from <span
 style="font-weight: bold;">java.lang.Object</span> are now callable,
e.g.<br>
</div>
<ul>
  <ul>
    <pre>jpl_new(array(int), [4,5,6], A),<br>jpl_call(A, hashCode, [], H).</pre>
  </ul>
</ul>
<i></i>
<h3>Java API: planned or under consideration<br>
</h3>
<ul>
  <li>drop <span style="font-weight: bold;">Term.display()</span>,
which cutely displays any <span style="font-weight: bold;">Term</span>
in a Swing tree view in a new window.</li>
</ul>
<ul>
  <li>support non-virtual method calls, e.g. by</li>
</ul>
<pre style="margin-left: 80px;">jpl_call(+Obj, +Class:Method, +Args, -Result)<br></pre>
<ul>
  <li>finish the current tidy-up<br>
  </li>
</ul>
<ul>
  <li>passing (or returning) Prolog terms to Java by reference; we
might stash them in Prolog's <span style="font-style: italic;">recorded
database</span> (see <span style="font-weight: bold;">PL_record</span>
in the SWI-Prolog Reference Manual), and return an instance of some
yet-to-be-designed JPL class which erases the recorded term when the
referring object is garbage-collected<br>
  </li>
</ul>
<ul>
  <li>convenience constructs in Prolog akin to import in Java, allowing
us to write e.g.</li>
</ul>
<pre style="margin-left: 80px;">jpl_new('Timestamp', X, R)<br></pre>
<div style="margin-left: 40px;">when we mean</div>
<pre style="margin-left: 80px;">jpl_new('javax.sql.Timestamp', X, R)</pre>
<ul>
  <li>renaming the package <span
 style="font-family: monospace; font-weight: bold;">jpl</span> more
globally: unfortunately, <span
 style="font-family: monospace; font-weight: bold;">org.jpl</span> has
already been taken :-)</li>
</ul>
<ul>
  <li>ditching <span style="font-weight: bold;">jpl.Util</span> and
moving its (static, utility) methods into <span
 style="font-weight: bold;">jpl.JPL</span></li>
</ul>
<ul>
  <li>deprecate all <span style="font-family: monospace;">.args()</span>,
    <span style="font-family: monospace;">.arg0()</span>, <span
 style="font-family: monospace;">.arg1()</span> methods and replace with</li>
</ul>
<pre style="margin-left: 80px;">public final Term[] args;<br></pre>
<ul>
  <li>require any <span style="font-weight: bold;">Variable</span>'s
name to conform to Prolog source syntax, so that valid source texts can
be reconstructed from <span style="font-weight: bold;">Term</span>
instances by the <span style="font-family: monospace;">toString()</span>
methods</li>
</ul>
<address><a href="mailto:paul.singleton@bcs.org.uk">Paul Singleton</a></address>
<address>Wednesday 4th February 2004<br>
&nbsp;<br>
</address>
<hr style="width: 100%; height: 4px;">
<h2><span style="font-style: italic;"><a name="JPL_2.0.2_Java_API"></a>JPL
2.0.2</span> Release Notes</h2>
<h3>Java API: canonical representation of terms</h3>
<blockquote>
  <h3>rationale</h3>
"<b>List</b>" and "<b>Tuple</b>" terms are not recognised as distinct
types
by the Prolog engine: they are just conventions: it doesn't follow that
every <tt><font size="+1">./2</font></tt> or <tt><font size="+1">[]/0</font></tt>
should be represented externally as instances of <b>List</b> or <b>Nil</b>,
nor that <tt><font size="+1">{}/2</font></tt> should be represented as
  <b>Tuple</b>.&nbsp;
There are many other informal types, and it's not clear which of them
deserve
the same treatment.&nbsp; The simplest policy is to provide special
support
for none of them, and this is what <b><i>JPL 2.x.x</i></b> does.&nbsp;
This also ensures that there is only one valid representation of a
Prolog
term as <b><i>JPL</i></b> class instances (otherwise we would have to
be
careful to recognise every <b>Atom</b> whose name is "[]" as being
equivalent
to an instance of <b>Nil</b>).</blockquote>
<ul>
  <li>these classes have been dropped (sorry, not deprecated: they
don't fit
into the new scheme)</li>
  <ul>
    <li> <b>Tuple</b> (see above)</li>
    <li> <b>List</b> (see above)</li>
    <li> <b>Nil</b> (see above)</li>
    <li> <b>String</b> (these are obsolete and more-or-less deprecated
in
recent
SWI-Prolog releases)</li>
    <li> <b>Long</b> (this doesn't have a clear role)</li>
  </ul>
  <li>the Term class hierarchy has been rearranged thus:</li>
  <ul>
    <pre>Term (abstract)<br>&nbsp; |<br>&nbsp; +--- Compound<br>&nbsp; |&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; |<br>&nbsp; |&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; +--- Atom (special case)<br>&nbsp; |<br>&nbsp; +--- Integer<br>&nbsp; |<br>&nbsp; +--- Float<br>&nbsp; |<br>&nbsp; +--- Variable<br><br>Query</pre>
  </ul>
Note that an <b>Atom</b> is a <b>Compound</b> whose arity is
zero.&nbsp;
It is naughty to construct a <b>Compound</b> with zero arity (this
violates
canonicity), and <b><i>JPL</i></b> code never does this when exporting
terms from Prolog.&nbsp; Application code written in Java, using <b><i>JPL</i></b>,
should avoid doing this.&nbsp; Maybe we should raise an exception if it
is attempted (maybe we do, I can't remember :-)
  <p>Note also that, although a <b>Query</b> contains a <b>Term</b>
(among
other Prolog-related state), it is not related to it by inheritance.</p>
</ul>
<h3>&nbsp;Java API: lazy initialisation</h3>
<blockquote>It is no longer necessary to explicitly initialise <b><i>JPL</i></b>
before calling any of the methods which access the Prolog engine.&nbsp;
This allows you to develop Java classes which make use of <b><i>JPL</i></b>,
and to make them available as "library" classes for use freely in
applications,
without placing a burden upon the application programmer to explicitly
initialise <b><i>JPL</i></b>.
  <p>Instead, <b><i>JPL</i></b> (and, if necessary, the Prolog engine)
is
initialised "lazily", at the first attempt to invoke the Prolog
engine.&nbsp;
At this point, a "default" sequence of initialisation parameters is
used:
initial values for these are compiled into <b><i>JPL</i></b>, but they
can be redefined at any time up until initialisation occurs. </p>
  <p>It is also possible for Java applications to discover (as a <b>String[]</b>)
exactly what sequence of <b><i>JPL</i></b> initialisation parameters
were
actually used (this call returns null if Prolog is not yet initialised,
and can thus be used as a test of this state). </p>
  <p>If a Java application needs to use Prolog with, say, a larger than
normal
heap or stack, it should attempt to redefine the default initialisation
parameters, and hope that the Prolog engine is not yet initialised (if
it is, there's not much it can do about it) (in newer versions of
SWI-Prolog
it could restart it, but this is rather drastic, might disrupt other
activities,
and is not yet supported via <b><i>JPL</i></b>). </p>
  <p>Finally, the <b><i>JPL 1.0.1</i></b> static <tt><font size="+1">jpl.JPL.init()</font></tt>
method is still supported, for backwards compatibility. </p>
  <p>These changes are not only for convenience, and to allow
development
of easy-to-use library code, but are part of a plan to combine Fred
Dushin's&nbsp;
Java-calls-Prolog interface with Paul Singleton's Prolog-calls-Java
interface,
to support hybrid Prolog+Java application programming in which either </p>
  <ul>
    <li>the JVM is alive before Prolog is started</li>
    <li>the Prolog engine is alive before a JVM is started</li>
    <li>a C or C++ main() starts both of them.</li>
  </ul>
</blockquote>
<h3>Java API: miscellaneous changes<br>
</h3>
<ul>
  <li> <i>new constructors:</i></li>
  <ul>
    <pre>new jpl.Query( Term t)</pre>
  </ul>
  <li> <i>withdrawn constructors:</i></li>
  <ul>
&nbsp; <br>
all the multi-argument convenience constructors for <b>Compound</b>
etc., since Java 1.1 onwards supports "anonymous arrays" which can
(fairly)
conveniently be used to create <b>Compound</b>s of any arity, e.g.
    <pre>new Compound( "pair", new Term[] { new Atom("one"), new Atom("two") } )</pre>
  </ul>
  <li> <i>new accessor methods:</i></li>
  <ul>
    <pre>String Compound.name()</pre>
    <pre>int Compound.arity()</pre>
  </ul>
NB an <b>Atom</b> is a special case of a <b>Compound</b>, and
inherits
its&nbsp; <tt>name()</tt> and an <tt>arity()</tt> accessors <br>
&nbsp; <li><i>deprecated accessor methods:</i></li>
  <ul>
    <pre>Compound.atom()</pre>
  </ul>
&nbsp;(although Prolog conventionally, and necessarily, returns the
"name"
of a term's principal functor as an atom, this "name" is really a
string,
and in Java we can represent it as such; the best Prolog can return is
"the atom whose name is the same as the name of this compound", whereas
we can simply return the name). <br>
&nbsp; <li><i>deprecated method:</i></li>
  <ul>
    <pre>jpl.Query.query() <i>is renamed</i> jpl.Query.hasSolution()</pre>
  </ul>
for consistency with oneSolution() and allSolutions()
</ul>
<h3>Java API: bug fixes</h3>
<blockquote>Only one "bug" has been fixed, and this was already flagged
by Fred as an issue: it concerns the conversion, from Prolog into <b><i>JPL</i></b>,
of terms which contain shared variables (i.e. several instances of the
same variable).&nbsp; Transput of any (non-cyclic) term from Prolog
into
Java and back, using <b><i>JPL</i></b>, should yield a new term which
is
identical to the original apart from having all new variables (but in a
similar pattern of sharing).</blockquote>
<br>
<address><a href="mailto:p.singleton@keele.ac.uk">Paul Singleton</a></address>
<address>
drafted Tuesday 20th February 2001<br>
revised Thursday 19th April 2001</address>
<address><br>
<br>
</address>
</body>
</html>
