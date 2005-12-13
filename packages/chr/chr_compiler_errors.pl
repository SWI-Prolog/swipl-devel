:- module(chr_compiler_errors,
		[	chr_warning/3,
			chr_error/3,
			print_chr_error/1
		]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% chr_warning(+Type,+FormattedMessage,+MessageParameters)

chr_warning(deprecated(Term),Message,Params) :- !,
	format('================================================================================\n'),
	format('CHR compiler WARNING: deprecated syntax	~w.\n',[Term]),	
	format('    `--> '),
	format(Message,Params),
        format('    Support for deprecated syntax will be discontinued in the near future!\n'),
	format('================================================================================\n').

chr_warning(internal,Message,Params) :- !,
	format('================================================================================\n'),
	format('CHR compiler WARNING: something unexpected happened in the CHR compiler.\n'),	
	format('    `--> '),
	format(Message,Params),
        format('    Your program may not have been compiled correctly!\n'),
        format('    Please contact tom.schrijvers@cs.kuleuven.be.\n'),
	format('================================================================================\n').

chr_warning(unsupported_pragma(Pragma,Rule),Message,Params) :- !,
	format('================================================================================\n'),
	format('CHR compiler WARNING: unsupported pragma ~w in ~@.\n',[Pragma,format_rule(Rule)]),	
	format('    `--> '),
	format(Message,Params),
        format('    Pragma is ignored!\n'),
	format('================================================================================\n').

chr_warning(_,Message,Params) :-
	format('================================================================================\n'),
	format('CHR compiler WARNING:\n'),	
	format('    `--> '),
	format(Message,Params),
	format('================================================================================\n').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% chr_error(+Type,+FormattedMessage,+MessageParameters)

chr_error(Type,Message,Params) :-
	throw(chr_error(error(Type,Message,Params))).

print_chr_error(error(Type,Message,Params)) :-
	print_chr_error(Type,Message,Params).

print_chr_error(syntax(Term),Message,Params) :- !,
	format('================================================================================\n'),
	format('CHR compiler ERROR: invalid syntax "~w".\n',[Term]),	
	format('    `--> '),
	format(Message,Params),
	format('================================================================================\n').
print_chr_error(internal,Message,Params) :- !,
	format('================================================================================\n'),
	format('CHR compiler ERROR: something unexpected happened in the CHR compiler.\n'),	
	format('    `--> '),
	format(Message,Params),
        format('    Please contact tom.schrijvers@cs.kuleuven.be.\n'),
	format('================================================================================\n').

print_chr_error(_,Message,Params) :-
	format('================================================================================\n'),
	format('CHR compiler ERROR:\n'),	
	format('    `--> '),
	format(Message,Params),
	format('================================================================================\n').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


format_rule(PragmaRule) :-
	PragmaRule = pragma(_,_,_,MaybeName,N),
	( MaybeName = yes(Name) ->
		write('rule '), write(Name)
	;
		write('rule number '), write(N)
	).

