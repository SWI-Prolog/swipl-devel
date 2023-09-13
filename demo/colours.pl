:- module(colour_demo,                  % module name
          [ colour_demo/2,              % defined export
            no_colour_demo/2,           % undefined export
            op(500, fx, &&)             % exported operator
          ]).
:- use_module(library(record)).         % existing library, used
:- use_module(library(utf8)).           % existing library, not used
:- use_module(no_such_file).            % non-existing library

/** <module> Show PceEmacs colors for all syntactic elements

This code does not run. It is just to show syntax highlighting elements.
*/

%!  colour_demo(+In, -Out) is det.
%
%   Very simple demo clause trying to show as many as possible colors.

colour_demo(In, Out) :-                 % exported predicate
    must_be(atom, In),                  % autoloaded
    some_module:some_pred(SingleTon),   % qualified call, singleton
    var(Out),                           % built-in
    not_defined,                        % call to undefined predicate
    format('From ~w~n', [In]),          % Quoted atom
    current_prolog_flag(bounded, _),    % existing flag
    current_prolog_flag(no_flag, _),    % non-existing flag
    Out is sin(1r3) +                   % function, rational
           nofunc(42) *                 % undefined function, integer
           exp(2.7),                    % function, float
    p,                                  % call to local predicate
    open(In, read, Stream,
         [ type(binary),                % ok option
           type(error),                 % illegal value
           nooption(value)              % no such option
         ]).

p :-                                    % local predicate
    q,
    p.                                  % recursive call

ssu(Head),
  var(Head) =>                          % SSU guard, SSU neck
    Head = 42.

dcg(Head), [A] --> [A], more(Head).     % DCG push back, DCG neck

not_called.                             % not called predicate

:- record
    point(x:integer,
          y:integer).

:- multifile
    prolog:message//1.                  % known hook

prolog:message(something(X)) -->
    [ 'Just some message: ~p'-[X] ].
