%% File: fd.P
%%
%% A simple finite domain constrait solver implemented using the lowlevel 
%% attributes variables interface.  

:- import put_attr/3, get_attr/3, del_attr/3, 
   install_verify_attribute_handler/4 from machine.
:- import memberchk/2,member/2 from basics.

:- install_verify_attribute_handler(fd,AttrValue,Target,fd_handler(AttrValue,Target)).

fd_handler(Da, Target) :-
%    writeln(fd_handler(Da, Target)),
        (var(Target),                       % Target is an attributed variable
         get_attr(Target, fd, Db) ->            % has a domain
           intersection(Da, Db, [E|Es]),    % intersection not empty
           (Es = [] ->                      % exactly one element
%	  writeln(unifying(Target,E)),
              Target = E                    % bind Var (and Value) to E
           ;  put_attr(Target, fd, [E|Es]) % update Var's (and Value's)
           )
        ;   %writeln(membering(Target,Da)),
	memberchk(Target, Da)               % is Target a member of Da?
        ).

intersection([], _, []).
intersection([H|T], L2, [H|L3]) :-
        member(H, L2), !,
        intersection(T, L2, L3).
intersection([_|T], L2, L3) :-
        intersection(T, L2, L3).

domain(X, Dom) :- 
        var(Dom), !, 
        get_attr(X, fd, Dom). 
domain(X, List) :- 
        List = [El|Els],                     % at least one element 
        (Els = []                            % exactly one element
         -> X = El                           % implied binding 
        ;  put_attr(Fresh, fd, List),       % create a new attributed variable
           X = Fresh                         % may call verify_attributes/2
        ).

show_domain(X) :-                            % print out the domain of X
        var(X),                              % X must be a variable
        get_attr(X, fd, D),
        write('Domain of '), write(X),
        write(' is '), writeln(D).

