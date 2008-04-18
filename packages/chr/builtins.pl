%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author:	Tom Schrijvers
% Email:	Tom.Schrijvers@cs.kuleuven.be
% Copyright:	K.U.Leuven 2004
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(builtins,
	[
		negate_b/2,
		entails_b/2,
		binds_b/2,
		builtin_binds_b/2
	]).

:- use_module(hprolog).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
negate_b(A,B) :- once(negate(A,B)).
negate((A,B),NotB) :- A==true,negate(B,NotB). % added by jon
negate((A,B),NotA) :- B==true,negate(A,NotA). % added by jon
negate((A,B),(NotA;NotB)) :- negate(A,NotA),negate(B,NotB). % added by jon
negate((A;B),(NotA,NotB)) :- negate(A,NotA),negate(B,NotB). % added by jon
negate(true,fail).
negate(fail,true).
negate(X =< Y, Y < X).
negate(X > Y, Y >= X).
negate(X >= Y, Y > X).
negate(X < Y, Y =< X).
negate(X == Y, X \== Y). % added by jon
negate(X \== Y, X == Y). % added by jon
negate(X =:= Y, X =\= Y). % added by jon
negate(X is Y, X =\= Y). % added by jon
negate(X =\= Y, X =:= Y). % added by jon
negate(X = Y, X \= Y). % added by jon
negate(X \= Y, X = Y). % added by jon
negate(var(X),nonvar(X)).
negate(nonvar(X),var(X)).
negate(\+ X,X). % added by jon
negate(X,\+ X). % added by jon

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
entails_b(fail,_) :-!.
entails_b(A,B) :-
	( var(B) ->
		entails(A,B,[A])
	;
		once((
			entails(A,C,[A]),
			B == C
		))
	).

entails(A,A,_).
entails(A,C,History) :-
	entails_(A,B),
	\+ hprolog:memberchk_eq(B,History),
	entails(B,C,[B|History]).		

entails_(X > Y, X >= Y).
entails_(X > Y, Y < X).
entails_(X >= Y, Y =< X).
entails_(X =< Y, Y >= X). %added by jon
entails_(X < Y, Y > X).
entails_(X < Y, X =< Y).
entails_(X > Y, X \== Y).
entails_(X \== Y, Y \== X).
entails_(X == Y, Y == X).
entails_(X == Y, X =:= Y) :- ground(X). %added by jon
entails_(X == Y, X =:= Y) :- ground(Y). %added by jon
entails_(X \== Y, X =\= Y) :- ground(X). %added by jon
entails_(X \== Y, X =\= Y) :- ground(Y). %added by jon
entails_(X =:= Y, Y =:= X). %added by jon
entails_(X =\= Y, Y =\= X). %added by jon
entails_(X == Y, X >= Y). %added by jon
entails_(X == Y, X =< Y). %added by jon
entails_(ground(X),nonvar(X)).
entails_(compound(X),nonvar(X)).
entails_(atomic(X),nonvar(X)).
entails_(number(X),nonvar(X)).
entails_(atom(X),nonvar(X)).
entails_(fail,true).

builtin_binds_b(G,Vars) :-
	builtin_binds_(G,L,[]),
	sort(L,Vars).

builtin_binds_(var(_),L,L).
builtin_binds_(nonvar(_),L,L).
builtin_binds_(ground(_),L,L).
builtin_binds_(compound(_),L,L).
builtin_binds_(number(_),L,L).
builtin_binds_(atom(_),L,L).
builtin_binds_(atomic(_),L,L).
builtin_binds_(integer(_),L,L).
builtin_binds_(float(_),L,L).

builtin_binds_(?=(_, _), L, L).
builtin_binds_(_<_, L, L).
builtin_binds_(_=:=_, L, L).
builtin_binds_(_=<_, L, L).
builtin_binds_(_==_, L, L).
builtin_binds_(_=@=_, L, L).
builtin_binds_(_=\=_, L, L).
builtin_binds_(_>=_, L, L).
builtin_binds_(_>_, L, L).
builtin_binds_(_@<_, L, L).
builtin_binds_(_@=<_, L, L).
builtin_binds_(_@>=_, L, L).
builtin_binds_(_@>_, L, L).
builtin_binds_(_\==_, L, L).
builtin_binds_(_\=@=_, L, L).
builtin_binds_(true,L,L).

% TODO: check all these SWI-Prolog built-ins for binding behavior.
%
% builtin_binds_(format(_,_),L,L).
% builtin_binds_(portray(_), L, L).
% builtin_binds_(write(_), L, L).
% builtin_binds_(write(_),L,L).
% builtin_binds_(write(_, _), L, L).
% builtin_binds_(write_canonical(_), L, L).
% builtin_binds_(write_canonical(_, _), L, L).
% builtin_binds_(write_term(_, _), L, L).
% builtin_binds_(write_term(_, _, _), L, L).
% builtin_binds_(writef(_), L, L).
% builtin_binds_(writef(_, _), L, L).
% builtin_binds_(writeln(_), L, L).
% builtin_binds_(writeln(_),L,L).
% builtin_binds_(writeq(_), L, L).
% builtin_binds_(writeq(_, _), L, L).
% 
% builtin_binds_(!(_), L, L).
% builtin_binds_(!, L, L).
% builtin_binds_((_'|'_), L, L).
% builtin_binds_((_*->_), L, L).
% builtin_binds_(abolish(_), L, L).
% builtin_binds_(abolish(_, _), L, L).
% builtin_binds_(abort, L, L).
% builtin_binds_(absolute_file_name(_, _), L, L).
% builtin_binds_(absolute_file_name(_, _, _), L, L).
% builtin_binds_(access_file(_, _), L, L).
% builtin_binds_(acyclic_term(_), L, L).
% builtin_binds_(add_import_module(_, _, _), L, L).
% builtin_binds_(append(_), L, L).
% builtin_binds_(apply(_, _), L, L).
% builtin_binds_(arg(_, _, _), L, L).
% builtin_binds_(arithmetic_function(_), L, L).
% builtin_binds_(assert(_), L, L).
% builtin_binds_(assert(_, _), L, L).
% builtin_binds_(asserta(_), L, L).
% builtin_binds_(asserta(_, _), L, L).
% builtin_binds_(assertz(_), L, L).
% builtin_binds_(assertz(_, _), L, L).
% builtin_binds_(at_end_of_stream(_), L, L).
% builtin_binds_(at_end_of_stream, L, L).
% builtin_binds_(at_halt(_), L, L).
% builtin_binds_(at_initialization(_), L, L).
% builtin_binds_(atom(_), L, L).
% builtin_binds_(atom_chars(_, _), L, L).
% builtin_binds_(atom_codes(_, _), L, L).
% builtin_binds_(atom_concat(_, _, _), L, L).
% builtin_binds_(atom_length(_, _), L, L).
% builtin_binds_(atom_number(_, _), L, L).
% builtin_binds_(atom_prefix(_, _), L, L).
% builtin_binds_(atom_to_term(_, _, _), L, L).
% builtin_binds_(atomic(_), L, L).
% builtin_binds_(attvar(_), L, L).
% builtin_binds_(autoload(_), L, L).
% builtin_binds_(autoload, L, L).
% builtin_binds_(b_getval(_, _), L, L).
% builtin_binds_(b_setval(_, _), L, L).
% builtin_binds_(bagof(_, _, _), L, L).
% builtin_binds_(between(_, _, _), L, L).
% builtin_binds_(block(_, _, _), L, L).
% builtin_binds_(break, L, L).
% builtin_binds_(byte_count(_, _), L, L).
% builtin_binds_(call(_), L, L).
% builtin_binds_(call(_, _), L, L).
% builtin_binds_(call(_, _, _), L, L).
% builtin_binds_(call(_, _, _, _), L, L).
% builtin_binds_(call(_, _, _, _, _), L, L).
% builtin_binds_(call(_, _, _, _, _, _), L, L).
% builtin_binds_(call(_, _, _, _, _, _, _), L, L).
% builtin_binds_(call(_, _, _, _, _, _, _, _), L, L).
% builtin_binds_(call(_, _, _, _, _, _, _, _, _), L, L).
% builtin_binds_(call(_, _, _, _, _, _, _, _, _, _), L, L).
% builtin_binds_(call(_, _, _, _, _, _, _, _, _, _, _), L, L).
% builtin_binds_(call_cleanup(_, _), L, L).
% builtin_binds_(call_cleanup(_, _, _), L, L).
% builtin_binds_(call_shared_object_function(_, _), L, L).
% builtin_binds_(call_with_depth_limit(_, _, _), L, L).
% builtin_binds_(callable(_), L, L).
% builtin_binds_(catch(_, _, _), L, L).
% builtin_binds_(char_code(_, _), L, L).
% builtin_binds_(char_conversion(_, _), L, L).
% builtin_binds_(char_type(_, _), L, L).
% builtin_binds_(character_count(_, _), L, L).
% builtin_binds_(clause(_, _), L, L).
% builtin_binds_(clause(_, _, _), L, L).
% builtin_binds_(clause_property(_, _), L, L).
% builtin_binds_(close(_), L, L).
% builtin_binds_(close(_, _), L, L).
% builtin_binds_(close_shared_object(_), L, L).
% builtin_binds_(code_type(_, _), L, L).
% builtin_binds_(collation_key(_, _), L, L).
% builtin_binds_(compare(_, _, _), L, L).
% builtin_binds_(compile_aux_clauses(_), L, L).
% builtin_binds_(compile_predicates(_), L, L).
% builtin_binds_(compiling, L, L).
% builtin_binds_(compound(_), L, L).
% builtin_binds_(concat_atom(_, _), L, L).
% builtin_binds_(concat_atom(_, _, _), L, L).
% builtin_binds_(consult(_), L, L).
% builtin_binds_(context_module(_), L, L).
% builtin_binds_(copy_stream_data(_, _), L, L).
% builtin_binds_(copy_stream_data(_, _, _), L, L).
% builtin_binds_(copy_term(_, _), L, L).
% builtin_binds_(copy_term_nat(_, _), L, L).
% builtin_binds_(current_arithmetic_function(_), L, L).
% builtin_binds_(current_atom(_), L, L).
% builtin_binds_(current_blob(_, _), L, L).
% builtin_binds_(current_char_conversion(_, _), L, L).
% builtin_binds_(current_flag(_), L, L).
% builtin_binds_(current_format_predicate(_, _), L, L).
% builtin_binds_(current_functor(_, _), L, L).
% builtin_binds_(current_input(_), L, L).
% builtin_binds_(current_key(_), L, L).
% builtin_binds_(current_module(_), L, L).
% builtin_binds_(current_module(_, _), L, L).
% builtin_binds_(current_op(_, _, _), L, L).
% builtin_binds_(current_output(_), L, L).
% builtin_binds_(current_predicate(_), L, L).
% builtin_binds_(current_predicate(_, _), L, L).
% builtin_binds_(current_prolog_flag(_, _), L, L).
% builtin_binds_(current_resource(_, _, _), L, L).
% builtin_binds_(current_signal(_, _, _), L, L).
% builtin_binds_(cyclic_term(_), L, L).
% builtin_binds_(date_time_stamp(_, _), L, L).
% builtin_binds_(debugging, L, L).
% builtin_binds_(default_module(_, _), L, L).
% builtin_binds_(del_attr(_, _), L, L).
% builtin_binds_(delete_directory(_), L, L).
% builtin_binds_(delete_file(_), L, L).
% builtin_binds_(delete_import_module(_, _), L, L).
% builtin_binds_(deterministic(_), L, L).
% builtin_binds_(downcase_atom(_, _), L, L).
% builtin_binds_(duplicate_term(_, _), L, L).
% builtin_binds_(dwim_match(_, _), L, L).
% builtin_binds_(dwim_match(_, _, _), L, L).
% builtin_binds_(dwim_predicate(_, _), L, L).
% builtin_binds_(ensure_loaded(_), L, L).
% builtin_binds_(erase(_), L, L).
% builtin_binds_(eval_license, L, L).
% builtin_binds_(exists_directory(_), L, L).
% builtin_binds_(exists_file(_), L, L).
% builtin_binds_(exit(_, _), L, L).
% builtin_binds_(expand_file_name(_, _), L, L).
% builtin_binds_(expand_file_search_path(_, _), L, L).
% builtin_binds_(expand_goal(_, _), L, L).
% builtin_binds_(expand_term(_, _), L, L).
% builtin_binds_(export(_), L, L).
% builtin_binds_(export_list(_, _), L, L).
% builtin_binds_(fail(_), L, L).
% builtin_binds_(fail, L, L).
% builtin_binds_(file_base_name(_, _), L, L).
% builtin_binds_(file_directory_name(_, _), L, L).
% builtin_binds_(file_name_extension(_, _, _), L, L).
% builtin_binds_(fileerrors(_, _), L, L).
% builtin_binds_(findall(_, _, _), L, L).
% builtin_binds_(findall(_, _, _, _), L, L).
% builtin_binds_(flag(_, _, _), L, L).
% builtin_binds_(float(_), L, L).
% builtin_binds_(flush_output(_), L, L).
% builtin_binds_(flush_output, L, L).
% builtin_binds_(forall(_, _), L, L).
% builtin_binds_(format(_), L, L).
% builtin_binds_(format(_, _), L, L).
% builtin_binds_(format(_, _, _), L, L).
% builtin_binds_(format_predicate(_, _), L, L).
% builtin_binds_(format_time(_, _, _), L, L).
% builtin_binds_(format_time(_, _, _, _), L, L).
% builtin_binds_(freeze(_, _), L, L).
% builtin_binds_(frozen(_, _), L, L).
% builtin_binds_(functor(_, _, _), L, L).
% builtin_binds_(garbage_collect, L, L).
% builtin_binds_(garbage_collect_atoms, L, L).
% builtin_binds_(garbage_collect_clauses, L, L).
% builtin_binds_(get(_), L, L).
% builtin_binds_(get(_, _), L, L).
% builtin_binds_(get0(_), L, L).
% builtin_binds_(get0(_, _), L, L).
% builtin_binds_(get_attr(_, _, _), L, L).
% builtin_binds_(get_attrs(_, _), L, L).
% builtin_binds_(get_byte(_), L, L).
% builtin_binds_(get_byte(_, _), L, L).
% builtin_binds_(get_char(_), L, L).
% builtin_binds_(get_char(_, _), L, L).
% builtin_binds_(get_code(_), L, L).
% builtin_binds_(get_code(_, _), L, L).
% builtin_binds_(get_single_char(_), L, L).
% builtin_binds_(get_time(_), L, L).
% builtin_binds_(getenv(_, _), L, L).
% builtin_binds_(ground(_), L, L).
% builtin_binds_(halt(_), L, L).
% builtin_binds_(halt, L, L).
% builtin_binds_(hash(_), L, L).
% builtin_binds_(term_hash(_, _), L, L).
% builtin_binds_(ignore(_), L, L).
% builtin_binds_(import(_), L, L).
% builtin_binds_(import_module(_, _), L, L).
% builtin_binds_(index(_), L, L).
% builtin_binds_(integer(_), L, L).
% builtin_binds_(is_absolute_file_name(_), L, L).
% builtin_binds_(is_list(_), L, L).
% builtin_binds_(is_stream(_), L, L).
% builtin_binds_(keysort(_, _), L, L).
% builtin_binds_(leash(_), L, L).
% builtin_binds_(length(_, _), L, L).
% builtin_binds_(license(_), L, L).
% builtin_binds_(license(_, _), L, L).
% builtin_binds_(line_count(_, _), L, L).
% builtin_binds_(line_position(_, _), L, L).
% builtin_binds_(load_files(_), L, L).
% builtin_binds_(load_files(_, _), L, L).
% builtin_binds_(make_directory(_), L, L).
% builtin_binds_(make_library_index(_), L, L).
% builtin_binds_(make_library_index(_, _), L, L).
% builtin_binds_(maplist(_, _), L, L).
% builtin_binds_(maplist(_, _, _), L, L).
% builtin_binds_(maplist(_, _, _, _), L, L).
% builtin_binds_(memberchk(_, _), L, L).
% builtin_binds_(message_queue_create(_), L, L).
% builtin_binds_(message_queue_create(_, _), L, L).
% builtin_binds_(message_queue_destroy(_), L, L).
% builtin_binds_(message_queue_property(_, _), L, L).
% builtin_binds_(message_to_string(_, _), L, L).
% builtin_binds_(module(_), L, L).
% builtin_binds_(msort(_, _), L, L).
% builtin_binds_(mutex_create(_), L, L).
% builtin_binds_(mutex_create(_, _), L, L).
% builtin_binds_(mutex_destroy(_), L, L).
% builtin_binds_(mutex_lock(_), L, L).
% builtin_binds_(mutex_property(_, _), L, L).
% builtin_binds_(mutex_statistics, L, L).
% builtin_binds_(mutex_trylock(_), L, L).
% builtin_binds_(mutex_unlock(_), L, L).
% builtin_binds_(mutex_unlock_all, L, L).
% builtin_binds_(name(_, _), L, L).
% builtin_binds_(nb_current(_, _), L, L).
% builtin_binds_(nb_delete(_), L, L).
% builtin_binds_(nb_getval(_, _), L, L).
% builtin_binds_(nb_linkarg(_, _, _), L, L).
% builtin_binds_(nb_linkval(_, _), L, L).
% builtin_binds_(nb_setarg(_, _, _), L, L).
% builtin_binds_(nb_setval(_, _), L, L).
% builtin_binds_(nl(_), L, L).
% builtin_binds_(nl, L, L).
% builtin_binds_(nonvar(_), L, L).
% builtin_binds_(noprofile(_), L, L).
% builtin_binds_(noprotocol, L, L).
% builtin_binds_(nospy(_), L, L).
% builtin_binds_(nospyall, L, L).
% builtin_binds_(not(_), L, L).
% builtin_binds_(notrace(_), L, L).
% builtin_binds_(notrace, L, L).
% builtin_binds_(nth_clause(_, _, _), L, L).
% builtin_binds_(number(_), L, L).
% builtin_binds_(number_chars(_, _), L, L).
% builtin_binds_(number_codes(_, _), L, L).
% builtin_binds_(numbervars(_, _, _), L, L).
% builtin_binds_(numbervars(_, _, _, _), L, L).
% builtin_binds_(on_signal(_, _, _), L, L).
% builtin_binds_(once(_), L, L).
% builtin_binds_(op(_, _, _), L, L).
% builtin_binds_(open(_, _, _), L, L).
% builtin_binds_(open(_, _, _, _), L, L).
% builtin_binds_(open_null_stream(_), L, L).
% builtin_binds_(open_resource(_, _, _), L, L).
% builtin_binds_(open_resource(_, _, _, _), L, L).
% builtin_binds_(open_shared_object(_, _), L, L).
% builtin_binds_(open_shared_object(_, _, _), L, L).
% builtin_binds_(open_xterm(_, _, _, _), L, L).
% builtin_binds_(peek_byte(_), L, L).
% builtin_binds_(peek_byte(_, _), L, L).
% builtin_binds_(peek_char(_), L, L).
% builtin_binds_(peek_char(_, _), L, L).
% builtin_binds_(peek_code(_), L, L).
% builtin_binds_(peek_code(_, _), L, L).
% builtin_binds_(phrase(_, _), L, L).
% builtin_binds_(phrase(_, _, _), L, L).
% builtin_binds_(plus(_, _, _), L, L).
% builtin_binds_(predicate_property(_, _), L, L).
% builtin_binds_(preprocessor(_, _), L, L).
% builtin_binds_(print(_), L, L).
% builtin_binds_(print(_, _), L, L).
% builtin_binds_(print_message(_, _), L, L).
% builtin_binds_(print_message_lines(_, _, _), L, L).
% builtin_binds_(profiler(_, _), L, L).
% builtin_binds_(prolog, L, L).
% builtin_binds_(prolog_choice_attribute(_, _, _), L, L).
% builtin_binds_(prolog_current_frame(_), L, L).
% builtin_binds_(prolog_frame_attribute(_, _, _), L, L).
% builtin_binds_(prolog_load_context(_, _), L, L).
% builtin_binds_(prolog_skip_level(_, _), L, L).
% builtin_binds_(prolog_to_os_filename(_, _), L, L).
% builtin_binds_(prompt(_, _), L, L).
% builtin_binds_(prompt1(_), L, L).
% builtin_binds_(protocol(_), L, L).
% builtin_binds_(protocola(_), L, L).
% builtin_binds_(protocolling(_), L, L).
% builtin_binds_(put(_), L, L).
% builtin_binds_(put(_, _), L, L).
% builtin_binds_(put_attr(_, _, _), L, L).
% builtin_binds_(put_attrs(_, _), L, L).
% builtin_binds_(put_byte(_), L, L).
% builtin_binds_(put_byte(_, _), L, L).
% builtin_binds_(put_char(_), L, L).
% builtin_binds_(put_char(_, _), L, L).
% builtin_binds_(put_code(_), L, L).
% builtin_binds_(put_code(_, _), L, L).
% builtin_binds_(qcompile(_), L, L).
% builtin_binds_(rational(_), L, L).
% builtin_binds_(rational(_, _, _), L, L).
% builtin_binds_(read(_), L, L).
% builtin_binds_(read(_, _), L, L).
% builtin_binds_(read_clause(_), L, L).
% builtin_binds_(read_clause(_, _), L, L).
% builtin_binds_(read_history(_, _, _, _, _, _), L, L).
% builtin_binds_(read_link(_, _, _), L, L).
% builtin_binds_(read_pending_input(_, _, _), L, L).
% builtin_binds_(read_term(_, _), L, L).
% builtin_binds_(read_term(_, _, _), L, L).
% builtin_binds_(recorda(_, _), L, L).
% builtin_binds_(recorda(_, _, _), L, L).
% builtin_binds_(recorded(_, _), L, L).
% builtin_binds_(recorded(_, _, _), L, L).
% builtin_binds_(recordz(_, _), L, L).
% builtin_binds_(recordz(_, _, _), L, L).
% builtin_binds_(redefine_system_predicate(_), L, L).
% builtin_binds_(reload_library_index, L, L).
% builtin_binds_(rename_file(_, _), L, L).
% builtin_binds_(repeat, L, L).
% builtin_binds_(require(_), L, L).
% builtin_binds_(reset_profiler, L, L).
% builtin_binds_(retract(_), L, L).
% builtin_binds_(retractall(_), L, L).
% builtin_binds_(same_file(_, _), L, L).
% builtin_binds_(same_term(_, _), L, L).
% builtin_binds_(see(_), L, L).
% builtin_binds_(seeing(_), L, L).
% builtin_binds_(seek(_, _, _, _), L, L).
% builtin_binds_(seen, L, L).
% builtin_binds_(set_input(_), L, L).
% builtin_binds_(set_output(_), L, L).
% builtin_binds_(set_prolog_IO(_, _, _), L, L).
% builtin_binds_(set_prolog_flag(_, _), L, L).
% builtin_binds_(set_stream(_, _), L, L).
% builtin_binds_(set_stream_position(_, _), L, L).
% builtin_binds_(setarg(_, _, _), L, L).
% builtin_binds_(setenv(_, _), L, L).
% builtin_binds_(setlocale(_, _, _), L, L).
% builtin_binds_(setof(_, _, _), L, L).
% builtin_binds_(setup_and_call_cleanup(_, _, _), L, L).
% builtin_binds_(setup_and_call_cleanup(_, _, _, _), L, L).
% builtin_binds_(shell(_), L, L).
% builtin_binds_(shell(_, _), L, L).
% builtin_binds_(shell, L, L).
% builtin_binds_(size_file(_, _), L, L).
% builtin_binds_(skip(_), L, L).
% builtin_binds_(skip(_, _), L, L).
% builtin_binds_(sleep(_), L, L).
% builtin_binds_(sort(_, _), L, L).
% builtin_binds_(source_file(_), L, L).
% builtin_binds_(source_file(_, _), L, L).
% builtin_binds_(source_location(_, _), L, L).
% builtin_binds_(spy(_), L, L).
% builtin_binds_(stamp_date_time(_, _, _), L, L).
% builtin_binds_(statistics(_, _), L, L).
% builtin_binds_(statistics, L, L).
% builtin_binds_(stream_position_data(_, _, _), L, L).
% builtin_binds_(stream_property(_, _), L, L).
% builtin_binds_(string(_), L, L).
% builtin_binds_(string_concat(_, _, _), L, L).
% builtin_binds_(string_length(_, _), L, L).
% builtin_binds_(string_to_atom(_, _), L, L).
% builtin_binds_(string_to_list(_, _), L, L).
% builtin_binds_(strip_module(_, _, _), L, L).
% builtin_binds_(style_check(_), L, L).
% builtin_binds_(sub_atom(_, _, _, _, _), L, L).
% builtin_binds_(sub_string(_, _, _, _, _), L, L).
% builtin_binds_(succ(_, _), L, L).
% builtin_binds_(swritef(_, _), L, L).
% builtin_binds_(swritef(_, _, _), L, L).
% builtin_binds_(tab(_), L, L).
% builtin_binds_(tab(_, _), L, L).
% builtin_binds_(tell(_), L, L).
% builtin_binds_(telling(_), L, L).
% builtin_binds_(term_to_atom(_, _), L, L).
% builtin_binds_(term_variables(_, _), L, L).
% builtin_binds_(term_variables(_, _, _), L, L).
% builtin_binds_(thread_at_exit(_), L, L).
% builtin_binds_(thread_create(_, _, _), L, L).
% builtin_binds_(thread_detach(_), L, L).
% builtin_binds_(thread_exit(_), L, L).
% builtin_binds_(thread_get_message(_), L, L).
% builtin_binds_(thread_get_message(_, _), L, L).
% builtin_binds_(thread_join(_, _), L, L).
% builtin_binds_(thread_kill(_, _), L, L).
% builtin_binds_(thread_peek_message(_), L, L).
% builtin_binds_(thread_peek_message(_, _), L, L).
% builtin_binds_(thread_property(_, _), L, L).
% builtin_binds_(thread_self(_), L, L).
% builtin_binds_(thread_send_message(_, _), L, L).
% builtin_binds_(thread_setconcurrency(_, _), L, L).
% builtin_binds_(thread_signal(_, _), L, L).
% builtin_binds_(thread_statistics(_, _, _), L, L).
% builtin_binds_(throw(_), L, L).
% builtin_binds_(time_file(_, _), L, L).
% builtin_binds_(tmp_file(_, _), L, L).
% builtin_binds_(told, L, L).
% builtin_binds_(trim_stacks, L, L).
% builtin_binds_(tty_get_capability(_, _, _), L, L).
% builtin_binds_(tty_goto(_, _), L, L).
% builtin_binds_(tty_put(_, _), L, L).
% builtin_binds_(tty_size(_, _), L, L).
% builtin_binds_(ttyflush, L, L).
% builtin_binds_(unifiable(_, _, _), L, L).
% builtin_binds_(unify_with_occurs_check(_, _), L, L).
% builtin_binds_(unsetenv(_), L, L).
% builtin_binds_(upcase_atom(_, _), L, L).
% builtin_binds_(wait_for_input(_, _, _), L, L).
% builtin_binds_(wildcard_match(_, _), L, L).
% builtin_binds_(with_mutex(_, _), L, L).
% builtin_binds_(with_output_to(_, _), L, L).
% builtin_binds_(working_directory(_, _), L, L).


% builtin_binds_(functor(Term, Functor, Arity), [Term,Functor,Arity|T], T).
% builtin_binds_(arg(Arg, Term, Pos), [Arg,Term,Pos|T], T).
% builtin_binds_(term_variables(_, _), L, L).
% builtin_binds_(X=Y, [X,Y|T], T).


builtin_binds_(X is _,[X|L],L).
builtin_binds_((G1,G2),L,T) :-
	builtin_binds_(G1,L,R),
	builtin_binds_(G2,R,T).
builtin_binds_((G1;G2),L,T) :-
	builtin_binds_(G1,L,R),
	builtin_binds_(G2,R,T).
builtin_binds_((G1->G2),L,T) :-
	builtin_binds_(G1,L,R),
	builtin_binds_(G2,R,T).

builtin_binds_(\+ G,L,T) :-
	builtin_binds_(G,L,T).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
binds_b(G,Vars) :-
	binds_(G,L,[]),
	sort(L,Vars).

binds_(var(_),L,L).
binds_(nonvar(_),L,L).
binds_(ground(_),L,L).
binds_(compound(_),L,L).
binds_(number(_),L,L).
binds_(atom(_),L,L).
binds_(atomic(_),L,L).
binds_(integer(_),L,L).
binds_(float(_),L,L).

binds_(_ > _ ,L,L).
binds_(_ < _ ,L,L).
binds_(_ =< _,L,L).
binds_(_ >= _,L,L).
binds_(_ =:= _,L,L).
binds_(_ =\= _,L,L).
binds_(_ == _,L,L).
binds_(_ \== _,L,L).
binds_(true,L,L).

binds_(write(_),L,L).
binds_(writeln(_),L,L).
binds_(format(_,_),L,L).

binds_(X is _,[X|L],L).
binds_((G1,G2),L,T) :-
	binds_(G1,L,R),
	binds_(G2,R,T).
binds_((G1;G2),L,T) :-
	binds_(G1,L,R),
	binds_(G2,R,T).
binds_((G1->G2),L,T) :-
	binds_(G1,L,R),
	binds_(G2,R,T).

binds_(\+ G,L,T) :-
	binds_(G,L,T).

binds_(G,L,T) :- term_variables(G,GVars),append(GVars,T,L).	%jon
