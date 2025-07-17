/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018-2024, CWI Amsterdam
			      SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(prolog_help,
	  [ help/0,
	    help/1,                     % +Object
	    apropos/1,                  % +Search
	    help_text/2
	  ]).
:- use_module(library(pldoc), []).
:- use_module(library(isub), [isub/4]).

:- autoload(library(apply), [maplist/3]).
:- autoload(library(error), [must_be/2]).
:- autoload(library(lists), [append/3, sum_list/2]).
:- autoload(library(pairs), [pairs_values/2]).
:- autoload(library(porter_stem), [tokenize_atom/2]).
:- autoload(library(process), [process_create/3]).
:- autoload(library(sgml), [load_html/3]).
:- autoload(library(solution_sequences), [distinct/1]).
:- autoload(library(http/html_write), [html/3, print_html/1]).
:- autoload(library(lynx/html_text), [html_text/2]).
:- autoload(pldoc(doc_man), [man_page/4]).
:- autoload(pldoc(doc_modes), [(mode)/2]).
:- autoload(pldoc(doc_words), [doc_related_word/3]).
:- autoload(pldoc(man_index), [man_object_property/2, doc_object_identifier/2]).
:- autoload(library(prolog_code), [pi_head/2]).
:- autoload(library(prolog_xref), [xref_source/2]).

:- use_module(library(lynx/pldoc_style), []).

/** <module> Text based manual

This module provides help/1 and apropos/1 that   give help on a topic or
searches the manual for relevant topics.

By default the result of  help/1  is   sent  through  a  _pager_ such as
`less`. This behaviour is controlled by the following:

  - The Prolog flag `help_pager`, which can be set to one of the
    following values:

    - false
    Never use a pager.
    - default
    Use default behaviour.  This tries to determine whether Prolog
    is running interactively in an environment that allows for
    a pager.  If so it examines the environment variable =PAGER=
    or otherwise tries to find the `less` program.
    - Callable
    A Callable term is interpreted as program_name(Arg, ...).  For
    example, `less('-r')` would be the default.  Note that the
    program name can be an absolute path if single quotes are
    used.
*/

:- meta_predicate
    with_pager(0).

:- multifile
    show_html_hook/1.

% one of `default`, `false`, an executable or executable(options), e.g.
% less('-r').
:- create_prolog_flag(help_pager, default,
		      [ type(term),
			keep(true)
		      ]).

%!  help is det.
%!  help(+What) is det.
%
%   Show help for What. What is a   term that describes the topics(s) to
%   give help for.  Notations for What are:
%
%     - Atom
%       This ambiguous form is most commonly used and shows all
%       matching documents.  For example:
%
%           ?- help(append).
%
%     - Name/Arity
%       Give help on predicates with matching Name/Arity.  Arity may
%       be unbound.
%     - Name//Arity
%       Give help on the matching DCG rule (non-terminal)
%     - Module:Name
%       Give help on predicates with Name in Module and any arity.
%       Used for loaded code only.
%     - Module:Name/Arity
%       Give help on predicates with Name in Module and Arity.
%       Used for loaded code only.
%     - f(Name/Arity)
%       Give help on the matching Prolog arithmetic functions.
%     - c(Name)
%       Give help on the matching C interface function
%     - section(Label)
%       Show the section from the manual with matching Label.
%
%   help/1 shows documentation from the manual   as  well as from loaded
%   user code if the code is documented   using  PlDoc. To show only the
%   documentatoion of the  loaded  predicate   we  may  prefix predicate
%   indicator with the module in which it is defined.
%
%   If an exact match fails this predicates attempts fuzzy matching and,
%   when successful, display the results headed   by  a warning that the
%   matches are based on fuzzy matching.
%
%   If possible, the results are sent  through   a  _pager_  such as the
%   `less` program. This behaviour is  controlled   by  the  Prolog flag
%   `help_pager`. See section level documentation.
%
%   @see apropos/1 for searching the manual names and summaries.

help :-
    notrace(show_matches([help/1, apropos/1], exact-help)).

help(What) :-
    notrace(help_no_trace(What)).

help_no_trace(What) :-
    help_objects_how(What, Matches, How),
    !,
    show_matches(Matches, How-What).
help_no_trace(What) :-
    print_message(warning, help(not_found(What))).

show_matches(Matches, HowWhat) :-
    help_html(Matches, HowWhat, HTML),
    !,
    show_html(HTML).

%!  show_html_hook(+HTML:string) is semidet.
%
%   Hook called to display the  extracted   HTML  document. If this hook
%   fails the HTML is rendered  to  the   console  as  plain  text using
%   html_text/2.

show_html(HTML) :-
    show_html_hook(HTML),
    !.
show_html(HTML) :-
    setup_call_cleanup(
	open_string(HTML, In),
	load_html(stream(In), DOM, []),
	close(In)),
    page_width(PageWidth),
    LineWidth is PageWidth - 4,
    with_pager(html_text(DOM, [width(LineWidth)])).

help_html(Matches, How, HTML) :-
    phrase(html(html([ head([]),
		       body([ \match_type(How),
			      dl(\man_pages(Matches,
					    [ no_manual(fail),
					      links(false),
					      link_source(false),
					      navtree(false),
					      server(false),
                                              qualified(always)
					    ]))
			    ])
		     ])),
	   Tokens),
    !,
    with_output_to(string(HTML),
		   print_html(Tokens)).

match_type(exact-_) -->
    [].
match_type(dwim-For) -->
    html(p(class(warning),
	   [ 'WARNING: No matches for "', span(class('help-query'), For),
	     '" Showing closely related results'
	   ])).

man_pages([], _) -->
    [].
man_pages([H|T], Options) -->
    (   man_page(H, Options)
    ->  []
    ;   html(p(class(warning),
               [ 'WARNING: No help for ~p'-[H]
               ]))
    ),
    man_pages(T, Options).

page_width(Width) :-
    tty_width(W),
    Width is min(100,max(50,W)).

%!  tty_width(-Width) is det.
%
%   Return the believed width of the terminal.   If we do not know Width
%   is bound to 80.

tty_width(W) :-
    \+ running_under_emacs,
    catch(tty_size(_, W), _, fail),
    !.
tty_width(80).

help_objects_how(Spec, Objects, exact) :-
    help_objects(Spec, exact, Objects),
    !.
help_objects_how(Spec, Objects, dwim) :-
    help_objects(Spec, dwim, Objects),
    !.

help_objects(Spec, How, Objects) :-
    findall(ID-Obj, help_object(Spec, How, Obj, ID), Objects0),
    Objects0 \== [],
    sort(1, @>, Objects0, Objects1),
    pairs_values(Objects1, Objects2),
    sort(Objects2, Objects).

help_object(Fuzzy/Arity, How, Name/Arity, ID) :-
    match_name(How, Fuzzy, Name),
    man_object_property(Name/Arity, id(ID)).
help_object(Fuzzy//Arity, How, Name//Arity, ID) :-
    match_name(How, Fuzzy, Name),
    man_object_property(Name//Arity, id(ID)).
help_object(Fuzzy/Arity, How, f(Name/Arity), ID) :-
    match_name(How, Fuzzy, Name),
    man_object_property(f(Name/Arity), id(ID)).
help_object(Fuzzy, How, Name/Arity, ID) :-
    atom(Fuzzy),
    match_name(How, Fuzzy, Name),
    man_object_property(Name/Arity, id(ID)).
help_object(Fuzzy, How, Name//Arity, ID) :-
    atom(Fuzzy),
    match_name(How, Fuzzy, Name),
    man_object_property(Name//Arity, id(ID)).
help_object(Fuzzy, How, f(Name/Arity), ID) :-
    atom(Fuzzy),
    match_name(How, Fuzzy, Name),
    man_object_property(f(Name/Arity), id(ID)).
help_object(Fuzzy, How, c(Name), ID) :-
    atom(Fuzzy),
    match_name(How, Fuzzy, Name),
    man_object_property(c(Name), id(ID)).
help_object(SecID, _How, section(Label), ID) :-
    atom(SecID),
    (   atom_concat('sec:', SecID, Label)
    ;   sub_atom(SecID, _, _, 0, '.html'),
	Label = SecID
    ),
    man_object_property(section(_Level,_Num,Label,_File), id(ID)).
help_object(Func, How, c(Name), ID) :-
    compound(Func),
    compound_name_arity(Func, Fuzzy, 0),
    match_name(How, Fuzzy, Name),
    man_object_property(c(Name), id(ID)).
% for currently loaded predicates
help_object(Module, _How, Module:Name/Arity, _ID) :-
    atom(Module),
    current_module(Module),
    atom_concat('sec:', Module, SecLabel),
    \+ man_object_property(section(_,_,SecLabel,_), _), % not a section
    current_predicate_help(Module:Name/Arity).
help_object(Module:Name, _How, Module:Name/Arity, _ID) :-
    atom(Name),
    current_predicate_help(Module:Name/Arity).
help_object(Module:Name/Arity, _How, Module:Name/Arity, _ID) :-
    atom(Name),
    current_predicate_help(Module:Name/Arity).
help_object(Name/Arity, _How, Module:Name/Arity, _ID) :-
    atom(Name),
    current_predicate_help(Module:Name/Arity).
help_object(Fuzzy, How, Module:Name/Arity, _ID) :-
    atom(Fuzzy),
    match_name(How, Fuzzy, Name),
    current_predicate_help(Module:Name/Arity).

%!  current_predicate_help(?PI) is nondet.
%
%   True when we have documentation on  PI.   First  we decide we have a
%   definition  for  PI,  then  we  check    whether   or  not  we  have
%   documentation for the module in which PI  resides. If not, we switch
%   to documentation collect mode and reload the file that defines PI.

current_predicate_help(M:Name/Arity) :-
    current_predicate(M:Name/Arity),
    pi_head(Name/Arity,Head),
    \+ predicate_property(M:Head, imported_from(_)),
    module_property(M, class(user)),
    (   mode(M:_, _)             % Some predicates are documented
    ->  true
    ;   \+ module_property(M, class(system)),
        main_source_file(M:Head, File),
	xref_source(File,[comments(store)])
    ),
    mode(M:Head, _).             % Test that our predicate is documented

match_name(exact, Name, Name).
match_name(dwim,  Name, Fuzzy) :-
    freeze(Fuzzy, dwim_match(Fuzzy, Name)).

%!  main_source_file(+Pred, -File) is semidet.
%
%   True when File is the main (not included) file that defines Pred.

main_source_file(Pred, File) :-
    predicate_property(Pred, file(File0)),
    main_source(File0, File).

main_source(File, Main) :-
    source_file(File),
    !,
    Main = File.
main_source(File, Main) :-
    source_file_property(File, included_in(Parent, _Time)),
    main_source(Parent, Main).


%!  with_pager(+Goal)
%
%   Send the current output of Goal through a  pager. If no pager can be
%   found we simply dump the output to the current output.

with_pager(Goal) :-
    pager_ok(Pager, Options),
    !,
    Catch = error(io_error(_,_), _),
    current_output(OldIn),
    setup_call_cleanup(
	process_create(Pager, Options,
		       [stdin(pipe(In))]),
	( set_stream(In, tty(true)),
	  set_output(In),
	  catch(Goal, Catch, true)
	),
	( set_output(OldIn),
	  close(In, [force(true)])
	)).
with_pager(Goal) :-
    call(Goal).

pager_ok(_Path, _Options) :-
    current_prolog_flag(help_pager, false),
    !,
    fail.
pager_ok(Path, Options) :-
    current_prolog_flag(help_pager, default),
    !,
    stream_property(current_output, tty(true)),
    \+ running_under_emacs,
    (   distinct((   getenv('PAGER', Pager)
		 ;   Pager = less
		 )),
	absolute_file_name(path(Pager), Path,
			   [ access(execute),
			     file_errors(fail)
			   ])
    ->  pager_options(Path, Options)
    ).
pager_ok(Path, Options) :-
    current_prolog_flag(help_pager, Term),
    callable(Term),
    compound_name_arguments(Term, Pager, Options),
    absolute_file_name(path(Pager), Path,
			   [ access(execute),
			     file_errors(fail)
			   ]).

pager_options(Path, Options) :-
    file_base_name(Path, File),
    file_name_extension(Base, _, File),
    downcase_atom(Base, Id),
    pager_default_options(Id, Options).

pager_default_options(less, ['-r']).


%!  running_under_emacs
%
%   True when we believe to be running  in Emacs. Unfortunately there is
%   no easy unambiguous way to tell.

running_under_emacs :-
    current_prolog_flag(emacs_inferior_process, true),
    !.
running_under_emacs :-
    getenv('TERM', dumb),
    !.
running_under_emacs :-
    current_prolog_flag(toplevel_prompt, P),
    sub_atom(P, _, _, _, 'ediprolog'),
    !.


%!  apropos(+Query) is det.
%
%   Print objects from the  manual  whose   name  or  summary match with
%   Query. Query takes one of the following forms:
%
%     - Type:Text
%       Find objects matching Text and filter the results by Type.
%       Type matching is a case intensitive _prefix_ match.
%       Defined types are `section`, `cfunction`, `function`,
%       `iso_predicate`, `swi_builtin_predicate`, `library_predicate`,
%       `dcg` and aliases `chapter`, `arithmetic`, `c_function`,
%       `predicate`, `nonterminal` and `non_terminal`.  For example:
%
%           ?- apropos(c:close).
%           ?- apropos(f:min).
%
%     - Text
%       Text is broken into tokens.  A topic matches if all tokens
%       appear in the name or summary of the topic. Matching is
%	case insensitive.  Results are ordered depending on the
%	quality of the match.

apropos(Query) :-
    notrace(apropos_no_trace(Query)).

apropos_no_trace(Query) :-
    findall(Q-(Obj-Summary), apropos(Query, Obj, Summary, Q), Pairs),
    (   Pairs == []
    ->  print_message(warning, help(no_apropos_match(Query)))
    ;   sort(1, >=, Pairs, Sorted),
	length(Sorted, Len),
	(   Len > 20
	->  length(Truncated, 20),
	    append(Truncated, _, Sorted)
	;   Truncated = Sorted
	),
	pairs_values(Truncated, Matches),
	print_message(information, help(apropos_matches(Matches, Len)))
    ).

apropos(Query, Obj, Summary, Q) :-
    parse_query(Query, Type, Words),
    man_object_property(Obj, summary(Summary)),
    apropos_match(Type, Words, Obj, Summary, Q).

parse_query(Type:String, Type, Words) :-
    !,
    must_be(atom, Type),
    must_be(text, String),
    tokenize_atom(String, Words).
parse_query(String, _Type, Words) :-
    must_be(text, String),
    tokenize_atom(String, Words).

apropos_match(Type, Query, Object, Summary, Q) :-
    maplist(amatch(Object, Summary), Query, Scores),
    match_object_type(Type, Object),
    sum_list(Scores, Q).

amatch(Object, Summary, Query, Score) :-
    (   doc_object_identifier(Object, String)
    ;   String = Summary
    ),
    amatch(Query, String, Score),
    !.

amatch(Query, To, Quality) :-
    doc_related_word(Query, Related, Distance),
    sub_atom_icasechk(To, _, Related),
    isub(Related, To, false, Quality0),
    Quality is Quality0*Distance.

match_object_type(Type, _Object) :-
    var(Type),
    !.
match_object_type(Type, Object) :-
    downcase_atom(Type, LType),
    object_class(Object, Class),
    match_object_class(LType, Class).

match_object_class(Type, Class) :-
    (   TheClass = Class
    ;   class_alias(Class, TheClass)
    ),
    sub_atom(TheClass, 0, _, _, Type),
    !.

class_alias(section,               chapter).
class_alias(function,              arithmetic).
class_alias(cfunction,             c_function).
class_alias(iso_predicate,         predicate).
class_alias(swi_builtin_predicate, predicate).
class_alias(library_predicate,     predicate).
class_alias(dcg,                   predicate).
class_alias(dcg,                   nonterminal).
class_alias(dcg,                   non_terminal).

class_tag(section,               'SEC').
class_tag(function,              '  F').
class_tag(iso_predicate,         'ISO').
class_tag(swi_builtin_predicate, 'SWI').
class_tag(library_predicate,     'LIB').
class_tag(dcg,                   'DCG').

object_class(section(_Level, _Num, _Label, _File), section).
object_class(c(_Name), cfunction).
object_class(f(_Name/_Arity), function).
object_class(Name/Arity, Type) :-
    functor(Term, Name, Arity),
    (   current_predicate(system:Name/Arity),
	predicate_property(system:Term, built_in)
    ->  (   predicate_property(system:Term, iso)
	->  Type = iso_predicate
	;   Type = swi_builtin_predicate
	)
    ;   Type = library_predicate
    ).
object_class(_M:_Name/_Arity, library_predicate).
object_class(_Name//_Arity, dcg).
object_class(_M:_Name//_Arity, dcg).

%! help_text(+Predicate:term, -HelpText:string) is semidet.
%
%  When =Predicate= is a term of the form =Name/Arity= for which
%  documentation exists, =HelpText= is the documentation in textual
%  format (parsed from the HTML help).
help_text(Pred, HelpText) :-
    help_objects(Pred, exact, Matches), !,
    catch(help_html(Matches, exact-exact, HtmlDoc), _, fail),
    setup_call_cleanup(open_string(HtmlDoc, In),
                       load_html(stream(In), Dom, []),
                       close(In)),
    with_output_to(string(HelpText), html_text(Dom, [])).

		 /*******************************
		 *            MESSAGES		*
		 *******************************/

:- multifile prolog:message//1.

prolog:message(help(not_found(What))) -->
    [ 'No help for ~p.'-[What], nl,
      'Use ?- apropos(query). to search for candidates.'-[]
    ].
prolog:message(help(no_apropos_match(Query))) -->
    [ 'No matches for ~p'-[Query] ].
prolog:message(help(apropos_matches(Pairs, Total))) -->
    { tty_width(W),
      Width is max(30,W),
      length(Pairs, Count)
    },
    matches(Pairs, Width),
    (   {Count =:= Total}
    ->  []
    ;   [ nl,
	  ansi(fg(red), 'Showing ~D of ~D matches', [Count,Total]), nl, nl,
	  'Use ?- apropos(Type:Query) or multiple words in Query '-[], nl,
	  'to restrict your search.  For example:'-[], nl, nl,
	  '  ?- apropos(iso:open).'-[], nl,
	  '  ?- apropos(\'open file\').'-[]
	]
    ).

matches([], _) --> [].
matches([H|T], Width) -->
    match(H, Width),
    (   {T == []}
    ->  []
    ;   [nl],
	matches(T, Width)
    ).

match(Obj-Summary, Width) -->
    { Left is min(40, max(20, round(Width/3))),
      Right is Width-Left-2,
      man_object_summary(Obj, ObjS, Tag),
      write_length(ObjS, LenObj, [portray(true), quoted(true)]),
      Spaces0 is Left - LenObj - 4,
      (   Spaces0 > 0
      ->  Spaces = Spaces0,
	  SummaryLen = Right
      ;   Spaces = 1,
	  SummaryLen is Right + Spaces0 - 1
      ),
      truncate(Summary, SummaryLen, SummaryE)
    },
    [ ansi([fg(default)], '~w ~p', [Tag, ObjS]),
      '~|~*+~w'-[Spaces, SummaryE]
%     '~*|~w'-[Spaces, SummaryE]		% Should eventually work
    ].

truncate(Summary, Width, SummaryE) :-
    string_length(Summary, SL),
    SL > Width,
    !,
    Pre is Width-4,
    sub_string(Summary, 0, Pre, _, S1),
    string_concat(S1, " ...", SummaryE).
truncate(Summary, _, Summary).

man_object_summary(section(_Level, _Num, Label, _File), Obj, 'SEC') :-
    atom_concat('sec:', Obj, Label),
    !.
man_object_summary(section(0, _Num, File, _Path), File, 'SEC') :- !.
man_object_summary(c(Name), Obj, '  C') :- !,
    compound_name_arguments(Obj, Name, []).
man_object_summary(f(Name/Arity), Name/Arity, '  F') :- !.
man_object_summary(Obj, Obj, Tag) :-
    (   object_class(Obj, Class),
	class_tag(Class, Tag)
    ->  true
    ;   Tag = '  ?'
    ).

		 /*******************************
		 *            SANDBOX		*
		 *******************************/

sandbox:safe_primitive(prolog_help:apropos(_)).
sandbox:safe_primitive(prolog_help:help(_)).
