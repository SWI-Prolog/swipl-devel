:- module(chr_compiler_errors,
		[	
			chr_info/3,
			chr_warning/3,
			chr_error/3,
			print_chr_error/1
		]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% chr_info(+Type,+FormattedMessage,+MessageParameters)

chr_info(_,Message,Params) :-
	( \+verbosity_on ->
		true
	;
		long_line_with_equality_signs,
		format(user_error,'CHR compiler:\n',[]),	
		format(user_error,Message,Params),
		long_line_with_equality_signs
	).


%% SWI begin
verbosity_on :- prolog_flag(verbose,V), V == yes.
%% SWI end

%% SICStus begin
%% verbosity_on.  % at the moment
%% SICStus end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% chr_warning(+Type,+FormattedMessage,+MessageParameters)

chr_warning(deprecated(Term),Message,Params) :- !,
	long_line_with_equality_signs,
	format(user_error,'CHR compiler WARNING: deprecated syntax      ~w.\n',[Term]),	
	format(user_error,'    `--> ',[]),
	format(user_error,Message,Params),
        format(user_error,'    Support for deprecated syntax will be discontinued in the near future!\n',[]),
	long_line_with_equality_signs.

chr_warning(internal,Message,Params) :- !,
	long_line_with_equality_signs,
	format(user_error,'CHR compiler WARNING: something unexpected happened in the CHR compiler.\n',[]),	
	format(user_error,'    `--> ',[]),
	format(user_error,Message,Params),
        format(user_error,'    Your program may not have been compiled correctly!\n',[]),
        format(user_error,'    Please contact tom.schrijvers@cs.kuleuven.be.\n',[]),
	long_line_with_equality_signs.

chr_warning(unsupported_pragma(Pragma,Rule),Message,Params) :- !,
	long_line_with_equality_signs,
	format(user_error,'CHR compiler WARNING: unsupported pragma ~w in ~@.\n',[Pragma,format_rule(Rule)]),	
	format(user_error,'    `--> ',[]),
	format(user_error,Message,Params),
        format(user_error,'    Pragma is ignored!\n',[]),
	long_line_with_equality_signs.
chr_warning(problem_pragma(Pragma,Rule),Message,Params) :- !,
	long_line_with_equality_signs,
	format(user_error,'CHR compiler WARNING: unsupported pragma ~w in ~@.\n',[Pragma,format_rule(Rule)]),	
	format(user_error,'    `--> ',[]),
	format(user_error,Message,Params),
	long_line_with_equality_signs.

chr_warning(_,Message,Params) :-
	long_line_with_equality_signs,
	format(user_error,'CHR compiler WARNING:\n',[]),	
	format(user_error,'    `--> ',[]),
	format(user_error,Message,Params),
	long_line_with_equality_signs.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% chr_error(+Type,+FormattedMessage,+MessageParameters)

chr_error(Type,Message,Params) :-
	throw(chr_error(error(Type,Message,Params))).

print_chr_error(error(Type,Message,Params)) :-
	print_chr_error(Type,Message,Params).

print_chr_error(syntax(Term),Message,Params) :- !,
	long_line_with_equality_signs,
	format(user_error,'CHR compiler ERROR: invalid syntax "~w".\n',[Term]),	
	format(user_error,'    `--> ',[]),
	format(user_error,Message,Params),
	long_line_with_equality_signs.
print_chr_error(internal,Message,Params) :- !,
	long_line_with_equality_signs,
	format(user_error,'CHR compiler ERROR: something unexpected happened in the CHR compiler.\n'),	
	format(user_error,'    `--> ',[]),
	format(user_error,Message,Params),
        format(user_error,'    Please contact tom.schrijvers@cs.kuleuven.be.\n'),
	long_line_with_equality_signs.

print_chr_error(cyclic_alias(Alias),Message,Params) :- !,
	long_line_with_equality_signs,
	format(user_error,'CHR compiler ERROR: cyclic alias "~w".\n',[Alias]),	
	format(user_error,'    `--> Aborting compilation.\n',[]),
	long_line_with_equality_signs.

print_chr_error(_,Message,Params) :-
	long_line_with_equality_signs,
	format(user_error,'CHR compiler ERROR:\n',[]),	
	format(user_error,'    `--> ',[]),
	format(user_error,Message,Params),
	long_line_with_equality_signs.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


format_rule(PragmaRule) :-
	PragmaRule = pragma(_,_,_,MaybeName,N),
	( MaybeName = yes(Name) ->
		write('rule '), write(Name)
	;
		write('rule number '), write(N)
	).

long_line_with_equality_signs :-
	format(user_error,'================================================================================\n',[]).
