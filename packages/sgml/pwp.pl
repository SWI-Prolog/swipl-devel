:- module(pwp,
	  [ pwp_files/2,		% +FileIn, +FileOut
	    pwp_stream/3,		% +StreamIn, +StreamOut, +Context
	    pwp_xml/3			% +DomIn, -DOMOut, +Context
	  ]).

/** <module> Prolog Well-formed Pages

PWP is an approach to server-side scripting using Prolog
which is based on a simple key principle:

    - The source form of a PWP should be WELL-FORMED XML

Especially when generating XML rather than HTML, this is such an
obvious thing to do.  We have many kinds of XML checking tools.

    - We can tell whether an XML document is WELL FORMED (all the
      punctuation is right, all tags balance) using practically
      any decent parser, including SWI Prolog's 'sgml'.

    - If we can write a Document Type Description then we can check
      that a document is VALID using tools like Open SP (formerly
      James Clark's SP) or SWI Prolog's 'sgml'.  This does not
      guarantee that the output will be valid, but it does catch
      a lot of errors very early.

    - If we can write an XML Schema then we can check that a
      document is schema-valid.  (SWI Prolog's 'sgml' does not
      yet come with a schema validator, but who knows what the
      future holds?).

    - Since an XML document is just a data structure, we can use
      any checking tool that we can write in Prolog, IF the input
      is well-formed so that we can load a template as a Prolog
      data structure.

Having decided that the input should be well formed, that means
*|NO NEW SYNTAX|*

None of the weird and horrible <% ... %> or whatever not-quite-XML
stuff you see in other template systems, making checking so very hard
(and therefore, making errors so distressingly common).

That in turns means that PWP "markup" must be based on special
elements or special attributes.  The fact that an XML parser must
allow undeclared attributes on any element even when validating,
but must not allow undeclared elements, suggests doing this through
attributes.  In particular, one should be able to take an existing
DTD, such as an XHTML DTD, and just use that without modification.
So the design reduces to

    - Allow dynamic data selection, insertion, and transformation
    just using a small number of extra attributes.

This description uses the following name space:

    ==
    xmlns:pwp='http://www.cs.otago.ac.nz/staffpriv/ok/pwp.pl'
    ==

The attributes are

    - pwp:ask = Query
    - pwp:use = Term
    - pwp:how = text | xml
    - pwp:tag = QName or '-'
    - pwp:att = '' | 'one non-alphanumeric character'

Here's what they mean.  Each element is expanded in the context
of a set of variable bindings.  After expansion, if the tag is
not mapped to '-', all attributes in the pwp: namespace are removed
and the children elements are recursively expanded.

    * pwp:ask = Query

	* Query is a Prolog goal.  For each solution of Query, the element
	is further processed with the new variables of Query added to
	the context.

	* If Query is not a well formed Prolog goal, or if execution of
	Query throws an exception, page transformation comes to a complete
	halt and no page is generated.

    * pwp:use = Term
    * pwp:how = text | xml | text-file | xml-file

	Term is a Prolog term; variables in Term are bound by the context.
	An empty Term is regarded as a missing value for this attribute.
	The Prolog variable CONTEXT refers to the entire context, a list
	of Name = Value, where Name is a Prolog atom holding the name of	the context variable and Value is an arbitrary Prolog term.

	- If pwp:how is text,
	    The value of Term is used to define a sequence of characters.

	    - A number produces the same characters that write/1 would.
	    - An atom produces the same characters that write/1 would.
	    - A string produces the same characters that write/1 would.
	    - A list of character codes produces those characters.

	    - The following terms produce the same sequence of characters
	      that the corresponding goal would have sent to the current
	      output stream:

	      - write(Datum)
	      - writeq(Datum)
	      - write_canonical(Datum)
	      - print(Datum)
	      - print(Datum)
	      - format(Format)
	      - format(Format, Arguments)

	    - A singleton list [X] defines the characters that X defines.
	    - Any other term F(T1,...,Tn) defines the characters that T1
	      defines, followed by the characters that T2 defines, ...,
	      followed by the characters that Tn defines.

	- If pwp:how is xml,
	    The value of Term must be an XML term as defined in the
	    SGML2PL documentation or a list of such terms.  A single
	    term is taken as if it had been [Term].  The resulting
	    list of terms replaces the children of the current element
	    and will not be further processed.

	- If pwp:how is text-file,
	    The value of Term is used to define a sequence of characters.
	    That sequence of characters is used as a file name.
	    The file is read as a sequence of characters, and that
	    sequence used as character data.

	- If pwp:how is xml-file,
	    The value of Term is used to define a sequence of characters.
	    That sequence of characters is used as a file name.
	    The file is loaded as XML, and the sequence of XML items thus
	    obtained used.  This means that PWP provides XML inclusion
	    without depending on the parser to support XInclude.

	The default value for pwp:how is text.

    * pwp:tag = QName or '-'

	If pwp:tag is missing or the value is empty, the current element
	appears in the output (after further processing) with its present
	tag.  If pwp:tag is a QName, the current element appears (...)
	with that as its tag.  That option is most useful in DTDs, where
	an "authoring" DTD may use one tag and have it automatically mapped
	to another tag in the output, e.g., <item> -> <li>.  Finally, if
	pwp:tag is '-', the children of the current element (either the
	result of pwp:use or the transformed original children, whichever
	applies) appear in the output but there is no element around them.

	A missing or empty pwp:ask is just like pwp:ask = 'true'.

    * pwp:att = '' | 'one non-alphanumeric character'.

	Attributes in the pwp namespace are not affected by this attribute.
	Such attributes are always stripped out and never substituted into.

	If pwp:att is missing or empty, attributes of the current
	element are copied over to the output unchanged.

	If pwp:att = 'c' for some non-alphanumeric character c,
	each attribute is examined for occurrences of c(...)c which
	are as short as possible.
	There is no one character which could be used every time, so you
	have to explicitly choose a substitution marker which is safe
	for the data you do not want to be altered.  None of the pwp
	attributes are inherited, least of all this one.

	Text outside c(...)c groups is copied unchanged; text inside
	such a group is parsed as a Prolog term and treated as if by
	pwp:how = text.


Examples:

    1. *|A "Hello World" like example|*

    ==
    <html
      xmlns:pwp="http://www.cs.otago.ac.nz/staffpriv/ok/pwp.pl"
      pwp:ask = "ensure_loaded(msg), once(msg(Greeting))">
      <head>
	<title pwp:use="Greeting"/>
      </head>
      <body>
	<p><span pwp:use="Greeting" pwp:tag='-'/></p>
      </body>
    </html>
    ==

    where msg.pl contains

    ==
    msg('Hello, World!').
    ==

    This example illustrates an important point.  Prolog Well-Formed
    Pages provide *NO* way to physically incorporate Prolog *clauses*
    into a page template.   Prolog clauses must be put in separate
    files which can be checked by a Prolog syntax checker, compiler,
    cross-referencer, &c WITHOUT the Prolog tool in question needing
    to know anything whatsoever about PWP.  You load the files using
    pwp:ask on the root element.

    2. *|Binding some variables and using them|*

    ==
    <html
      xmlns:pwp="http://www.cs.otago.ac.nz/staffpriv/ok/pwp.pl">
      <head><title>Example 2</title></head>
      <body pwp:ask="Hello = 'Hello world', A = 20, B = 22">
	<h1 pwp:use="Hello"/>
	<p>The answer is <span pwp:use="C" pwp:ask="C is A+B"/>.</p>
      </body>
    </html>
    ==

    3. *|Making a table|*
    We are given a Prolog database staff.pl defining
    staff(NickName, FullName, Office, Phone, E_Mail_Address).
    status(NickName, full_time | part_time).
    We want to make a phone list of full time staff.

    ==
    <html
      xmlns:pwp="http://www.cs.otago.ac.nz/staffpriv/ok/pwp.pl"
      pwp:ask='ensure_loaded(staff)'>
      <head>
	<title>Phone list for Full-Time staff.</title>
      </head>
      <body>
	<h1>Phone list for Full-Time staff.</h1>
	<table
	  pwp:ask = "setof(FullName-Phone,
			   N^O^E^(
			     status(N, full_time),
			     staff(N, FullName, O, Phone, E)
			   ),
			   Staff_List)">
	  <tr><th>Name</th><th>Phone</th></tr>
	  <tr pwp:ask="member(FullName-Phone, Staff_List)">
	    <td pwp:use="FullName"/>
	    <td pwp:use="Phone"/>
	  </tr>
	</table>
      </body>
    </html>
    ==

    4. *|Substituting into an attribute|*
    Same data base as before, but now we want to make a mailing list
    page.

    ==
    <html
      xmlns:pwp="http://www.cs.otago.ac.nz/staffpriv/ok/pwp.pl"
      pwp:ask='ensure_loaded(staff)'>
      <head>
	<title>Phone list for Full-Time staff.</title>
      </head>
      <body>
	<h1>Phone list for Full-Time staff.</h1>
	<table
	  pwp:ask = "setof(FullName-E_Mail,
			   N^O^P^staff(N, FullName, O, P, E_Mail),
			   Staff_List)">
	  <tr><th>Name</th><th>Address</th></tr>
	  <tr pwp:ask="member(FullName-E_Mail, Staff_List)">
	    <td pwp:use="FullName"/>
	    <td><a pwp:use="E_Mail"
		   pwp:att='$' href="mailto:$(E_Mail)$"/></td>
	  </tr>
	</table>
      </body>
    </html>
    ==

    5. *|If-then-else effect|*
    A page that displays the value of the 'SHELL' environment
    variable if it has one, otherwise displays 'There is no
    default shell.'

    ==
    <html
      xmlns:pwp="http://www.cs.otago.ac.nz/staffpriv/ok/pwp.pl">
      <head><title>$SHELL</title></head>
      <body>
	<p pwp:ask="getenv('SHELL', Shell)"
	>The default shell is <span pwp:tag="-" pwp:use="Shell"/>.</p>
	<p pwp:ask="\+getenv('SHELL',_)">There is no default shell.</p>
      </body>
    </html>
    ==

    There is one other criterion for a good server-side template
    language:

    It should be possible to compile templates so as to eliminate
    most if not all interpretation overhead.

    This particular notation satisfies that criterion with the
    limitation that the conversion of a term to character data requires
    run-time traversal of terms (because the terms are not known until
    run time).

@author Richard O'Keefe
@tbd	Support compilation of PWP input files
*/

:- use_module(library(sgml),       [load_xml_file/2]).
:- use_module(library(sgml_write), [xml_write/3]).
:- use_module(library(lists),      [append/3]).

:- meta_predicate
	pwp_files(:, +),
	pwp_stream(:, +, +),
	pwp_xml(:, -, +).


%%  pwp_files(:In:atom, +Out:atom) is det.
%
%   loads an Xml document from the file named In,
%   transforms it using the PWP attributes, and
%   writes the transformed version to the new file named Out.

pwp_files(M:In, Out) :-
	load_xml_file(In, Contents),
	pwp_xml(M:Contents, Transformed, []), !,
	setup_call_cleanup(open(Out, write, Output),
			   xml_write(Output, Transformed, []),
			   close(Output)).


%%	pwp_stream(:Input:input_stream, +Output:output_stream,
%%		   +Context:list) is det.
%
%	Loads an Xml document from the given Input stream, transforms it
%	using the PWP attributes, and writes  the transformed version to
%	the given Output stream.  Context   provides  initial contextual
%	variables and is a list of Name=Value.

pwp_stream(M:Input, Output, Context) :-
	load_xml_file(stream(Input), Contents),
	pwp_xml(M:Contents, Transformed, Context), !,
	xml_write(Output, Transformed, []).


/*  Recall that an XML term is one of

	<atom>			Character Data
	sdata(...)		SDATA (SGML only)
	ndata(...)		NDATA
	pi(...)			Processing instruction

	element(Name, [Att...], [Child...])

	    where Att is Attribute=Value and Child is an XML term.

    We are only concerned with elements; all other XML terms are
    left alone.  I have given some thought to recognising

	<?pwp ...Command...?>

    processing instructions, executing the Command, and removing
    the processing instructions, as a debugging tool.  But this
    is a proof-of-concept implementation; debugging features can
    wait for The Real Thing.
*/



%%  pwp_xml(:In:list(xml), -Out:list(xml), +Context)
%
%   maps down a list of XML items, acting specially on elements and
%   copying everything else unchanged, including white space.
%   The Context is a list of 'VariableName'=CurrentValue bindings.

pwp_xml(M:In, Out, Context) :-
	pwp_list(In, Out, M, Context).

pwp_list([], [], _, _).
pwp_list([element(Tag0,Atts0,Kids0)|Xs], Ys0, M, Context) :- !,
	pwp_attributes(Atts0, Ask, Use, How, Att, Tag1, Atts1),
	(   nonvar(Tag1), Tag1 \== '' -> Tag2 = Tag1
	;   Tag2 = Tag0
	),
	(   nonvar(Ask), Ask \== '', Ask \== 'true'
	->  atom_to_term(Ask, Query, Bindings),
	    pwp_unite(Bindings, Context, Context1),
	    findall(Xml,
		    ( M:Query,
		      pwp_element(Tag2, Atts1, Kids0, Use, How, Att,
				  M, Context1, Xml)),
		    NewContent)
	;   /* Ask is missing, empty, or true */
	    pwp_element(Tag2, Atts1, Kids0, Use, How, Att,
			M, Context, NewContent)
	),
	pwp_attach(NewContent, Ys0, Ys1),
	pwp_list(Xs, Ys1, M, Context).
pwp_list([X|Xs], [X|Ys], M, Context) :-
	pwp_list(Xs, Ys, M, Context).


%%	pwp_attributes(+Atts0:list(=(atom,atom)),
%%		       -Ask:atom, -Use:atom, -How:atom, -Att:atom,
%%		       -Tag:atom, -Atts1:list(=(atom,atom)))
%
%	Walks down a list of AttributeName=ItsValue pairs, stripping out
%	those whose AttributeName begins  with   the  'pwp:' prefix, and
%	copying the rest to Atts1.   Along  the way, Ask/Use/How/Att/Tag
%	are      bound      to       the        values       of      the
%	pwp:ask/pwp:use/pwp:how/pwp:att/pwp:tag attributes, if   any. At
%	the end, any of these variables   that  are still unbound REMAIN
%	unbound; they are not bound to default values.

pwp_attributes([], _, _, _, _, _, []).
pwp_attributes([AV|AVs], Ask, Use, How, Att, Tag, New_Atts1) :-
	AV = (Name=Value),
	(   pwp_attr(Name, PWPName)
	->  (   pwp_attr(PWPName, Value, Ask, Use, How, Att, Tag)
	    ->	New_Atts1 = New_Atts2
	    ;	New_Atts1 = New_Atts2
	    )
	;   New_Atts1 = [AV|New_Atts2]
	),
	pwp_attributes(AVs, Ask, Use, How, Att, Tag, New_Atts2).


pwp_attr(ask, Value, Value, _Use, _How, _Att, _Tag).
pwp_attr(use, Value, _Ask, Value, _How, _Att, _Tag).
pwp_attr(how, Value, _Ask, _Use, Value, _Att, _Tag).
pwp_attr(att, Value, _Ask, _Use, _How, Value, _Tag).
pwp_attr(tag, Value, _Ask, _Use, _How, _Att, Value).

%%	pwp_attr(+XMLAttr, -PWPLocal) is semidet.
%
%	True if PWPLocal is the local name  of a pwp:Local expression in
%	XML.  This  predicate  deals  with    the  three  different  XML
%	representations:  the  form  is  returned    of   XML  namespace
%	processing is not enabled. The second if   it is enabled and the
%	namespace is properly defined and the   last if the namespace is
%	not defined.

pwp_attr(Atom, PWP) :-
	atom(Atom),
	atom_concat('pwp:', PWP, Atom), !.
pwp_attr('http://www.cs.otago.ac.nz/staffpriv/ok/pwp.pl':PWP, PWP) :- !.
pwp_attr('pwp':PWP, PWP) :- !.
pwp_attr('xmlns:pwp', -).

%%  pwp_unite(+Bindings, +Context0, -Context:list(=(atom,any)))
%
%   merges the new Bindings with the bindings in the outer Context0,
%   constructing a new list of VariableName=CurrentValue bindings in
%   Context1.  This is only used when the CurrentValue parts of the
%   new Bindings are known to be distinct new variables, so the
%   Bindings cannot possibly conflict with any existing binding in
%   Context0.  This is O(|Bindings|.|Context0|), which is not that
%   efficient, but since we do not expect there to be very many
%   variables it doesn't matter much.

pwp_unite(Bindings, Context0, Context) :-
	pwp_unite(Bindings, Context0, Context0, Context).


pwp_unite([], _, Context, Context).
pwp_unite([Binding|Bindings], Context0, Context1, Context) :-
	memberchk(Binding, Context0), !,
	pwp_unite(Bindings, Context0, Context1, Context).
pwp_unite(['CONTEXT'=Context0|Bindings], Context0, Context1, Context) :- !,
	pwp_unite(Bindings, Context0, Context1, Context).
pwp_unite([Binding|Bindings], Context0, Context1, Context) :-
	pwp_unite(Bindings, Context0, [Binding|Context1], Context).



%%  pwp_unite(+Bindings, +Context0: list(=(atom,any)))
%
%   looks up the bindings in Bindings in the outer Context0.
%   This is only used for 'pwp:use' terms (and the related terms
%   in $(...)$ attribute value substitutions), so that we have
%   no interest in forming a new context.  (If we did, we'd use
%   pwp_unite/3 instead.)  This is only used when the CurrentValue
%   parts of the new Bindings are known to be distinct new variables,
%   so the Bindings cannot possibly conflict with any existing
%   binding in Context0.  However, there _could_ be new variables
%   in Bindings, and that would cause problems.  An XML term may
%   not contain variables, and a term we want to convert to a list
%   of character codes had better not contain variables either.
%   One approach would be to just bind such variables to something,
%   another is to throw some kind of exception.  For the moment we
%   call functor/3 so as to get an instantiation error.

pwp_unite([], _).
pwp_unite([Binding|Bindings], Context) :-
	memberchk(Binding, Context), !,
	pwp_unite(Bindings, Context).
pwp_unite([_=Value|_], _) :-
	functor(Value, _, _).

%%  pwp_attach(+Tree, ?Ys0: list(xml), ?Ys: list(xml))
%
%   is a combination of "flatten" and "append".
%   It unifies Ys0\Ys with the result of flattening Tree.

pwp_attach([], Ys, Ys) :- !.
pwp_attach([X|Xs], Ys0, Ys) :- !,
	pwp_attach(X, Ys0, Ys1),
	pwp_attach(Xs, Ys1, Ys).
pwp_attach(X, [X|Ys], Ys).



pwp_element('-', _, Kids, Use, How, _, M, Context, Xml) :- !,
	pwp_use(Use, How, Kids, M, Context, Xml).
pwp_element(Tag, Atts, Kids, Use, How, Magic, M, Context,
	    element(Tag,Atts1,Kids1)) :-
	(   nonvar(Magic)
	->  pwp_substitute(Atts, Magic, Context, Atts1)
	;   Atts1 = Atts
	),
	pwp_use(Use, How, Kids, M, Context, Kids1).

pwp_use('', _, Kids, M, Context, Kids1) :- !,
	pwp_list(Kids, Kids1, M, Context).
pwp_use(Use, How, _, _, Context, Kids1) :-
	atom_to_term(Use, Term, Bindings),
	pwp_unite(Bindings, Context),
	pwp_how(How, Term, Kids1).

pwp_how('text', Term, [CData]) :- !,
	pwp_use_codes(Term, Codes, []),
	atom_codes(CData, Codes).
pwp_how('xml', Term, Kids1) :-
	(   Term == []   -> Kids1 = Term
	;   Term = [_|_] -> Kids1 = Term
	;                   Kids1 = [Term]
	).
pwp_how('text-file', Term, [CData]) :-
	pwp_use_codes(Term, Codes, []),
	atom_codes(FileName, Codes),
	read_file_to_codes(FileName, FileCodes, []),
	atom_codes(CData, FileCodes).
pwp_how('xml-file', Term, Kids1) :-
	pwp_use_codes(Term, Codes, []),
	atom_codes(FileName, Codes),
	load_xml_file(FileName, Kids1).


pwp_substitute([], _, _, []).
pwp_substitute([AV|AVs], Magic, Context, [AV1|Atts1]) :-
	AV = (Name = Value),
	(   sub_atom(Value, _, _, _, Magic)
	->  char_code(Magic, C),
	    atom_codes(Value, Codes),
	    pwp_split(Codes, C, B0, T0, A0), !,
	    pwp_substitute(B0, T0, A0, C, Context, V),
	    atom_codes(New_Value, V),
	    AV1 = (Name = New_Value),
	    pwp_substitute(AVs, Magic, Context, Atts1)
	).
pwp_substitute([AV|AVs], Magic, Context, [AV|Atts1]) :-
	pwp_substitute(AVs, Magic, Context, Atts1).


pwp_substitute(B0, T0, A0, C, Context, V0) :-
	append(B0, V1, V0),
	atom_codes(Atom, T0),
	atom_to_term(Atom, Term, Bindings),
	pwp_unite(Bindings, Context, _),
	pwp_use_codes(Term, V1, V2),
	(   pwp_split(A0, C, B1, T1, A1)
	->  pwp_substitute(B1, T1, A1, C, Context, V2)
	;   V2 = A0
	).


pwp_split(Codes, C, Before, Text, After) :-
	append(Before, [C,0'(|Rest], Codes),
	append(Text,   [0'),C|After], Rest), !.


pwp_use_codes(format(Format), S0, S) :- !,
	pwp_format(Format, [], S0, S).
pwp_use_codes(format(Format,Args), S0, S) :- !,
	pwp_format(Format, Args, S0, S).
pwp_use_codes(write_canonical(Datum), S0, S) :- !,
	pwp_format('~k', [Datum], S0, S).
pwp_use_codes(print(Datum), S0, S) :- !,
	pwp_format('~p', [Datum], S0, S).
pwp_use_codes(writeq(Datum), S0, S) :- !,
	pwp_format('~q', [Datum], S0, S).
pwp_use_codes(write(Datum), S0, S) :- !,
	pwp_format('~w', [Datum], S0, S).
pwp_use_codes(Atomic, S0, S) :-
	atomic(Atomic), !,
	(   number(Atomic) -> number_codes(Atomic, Codes)
	;   atom(Atomic)   -> atom_codes(Atomic, Codes)
	;   string(Atomic) -> string_to_list(Atomic, Codes)
	;   pwp_format('~w', [Atomic], S0, S)
	),
	append(Codes, S, S0).
pwp_use_codes([X|Xs], S0, S) :-
	pwp_is_codes([X|Xs]), !,
	append([X|Xs], S, S0).
pwp_use_codes([X|Xs], S0, S) :- !,
	pwp_use_codes(Xs, X, S0, S).
pwp_use_codes(Compound, S0, S) :-
	Compound =.. [_,X|Xs],
	pwp_use_codes(Xs, X, S0, S).



pwp_use_codes([], X, S0, S) :- !,
	pwp_use_codes(X, S0, S).
pwp_use_codes([Y|Ys], X, S0, S) :-
	pwp_use_codes(X, S0, S1),
	pwp_use_codes(Ys, Y, S1, S).



%%  pwp_is_codes(+String: any)
%
%   is true when String is a list of integers and each of those
%   integers is a possible Unicode value (in the range U+0000..U+10FFFF).
%   Back in the days of ISO Latin 1 we would have checked for 0..255,
%   and way back in the days of ASCII for 0..127.  Yes, there are more
%   than a million possible characters in Unicode; currently about
%   100 000 of them are in use.

pwp_is_codes([]).
pwp_is_codes([C|Cs]) :-
	integer(C), C >= 0, C =< 0x10FFFF,
	pwp_is_codes(Cs).

pwp_format(Format, Arguments, S0, S) :-
	format(codes(S0, S), Format, Arguments).
