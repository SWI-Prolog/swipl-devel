%% Unicode in SWI-Prolog source code
%%
%% This file demonstrates the Unicode features of the SWI-Prolog
%% reader: identifiers from any script, super- and subscript indices on
%% variables, mathematical operators declared with op/3, and
%% the new "all Sm/Sc/Sk/So/P* are solo" tokenisation rule.
%%
%% Load it with `swipl demo/unicode.pl` and run `?- demo.`.

% --- Operators ---------------------------------------------------------
% Each Unicode symbol is now a *solo* atom on its own (the old
% behaviour glued adjacent symbols together).  We make them infix
% operators with op/3.

:- op(700, xfx, ≤).
:- op(700, xfx, ≥).
:- op(700, xfx, ∈).

% --- Comparison --------------------------------------------------------
% Defined for integers via the standard arithmetic operators.

X ≤ Y :- X =< Y.
X ≥ Y :- X >= Y.

% --- Set-style list predicates -----------------------------------------

X ∈ Set :- member(X, Set).

union([], B, B).
union([H|T], B, [H|R]) :- \+ member(H, B), !, union(T, B, R).
union([_|T], B, R) :- union(T, B, R).

intersection([], _, []).
intersection([H|T], B, [H|R]) :- member(H, B), !, intersection(T, B, R).
intersection([_|T], B, R) :- intersection(T, B, R).

% --- Variables with super- and subscript indices -----------------------
% A common mathematical idiom: "the i-th iterate" written X₀, X₁, X²
% rather than X0, X1, Xsq.

quadratic(A, B, C, X, Y²) :-
    X² is X*X,
    Y² is A*X² + B*X + C.

% --- Identifiers in non-Latin scripts ----------------------------------
% Lo (other letter) starts an atom; underscore-prefixed identifiers
% are variables in scripts without case (Greek, CJK, ...).

π(3.141592653589793).            % atom 'π' as a 1-arity predicate name

капитал(амстердам, нидерланды).
капитал(париж,     франция).
капитал(берлин,    германия).

国(中国, アジア).
国(日本, アジア).
国(ブラジル, 南米).

ストア(_, _) :-                 % '_店' / '_商品' would be variables in
    capitalised_in_japanese.    %   non-cased scripts via underscore prefix
                                %   (using plain `_` here to avoid singleton
                                %   warnings).

capitalised_in_japanese.

% --- Driver predicate --------------------------------------------------

demo :-
    current_prolog_flag(unicode_syntax_version, Version),
    findall(X, X ∈ [1,2,3], Xs),
    quadratic(1, -3, 2, 5, Y²),
    findall(C-L, капитал(C, L), Capitals),
    findall(C, 国(C, アジア), Asian),
    format("SWI-Prolog Unicode demo~n"),
    format("  unicode_syntax_version flag: ~w~n", [Version]),
    format("  3 ≤ 5 succeeds:              ~w~n", [3 ≤ 5]),
    format("  X ∈ [1,2,3] gives:           ~w~n", [Xs]),
    format("  x² - 3x + 2 at x=5:          ~w~n", [Y²]),
    format("  капиталы:                    ~w~n", [Capitals]),
    format("  Asian 国:                    ~w~n", [Asian]).

% --- What no longer works ----------------------------------------------
% These lines, if uncommented, illustrate behaviours that changed
% with the Unicode reform.  Don't uncomment unless you want a syntax
% error.
%
%   foo(X) :- X ≤≤ 1.        % under the old regime, ≤≤ was glued into a
%                            % single atom; now each ≤ is solo and ≤≤ is
%                            % a syntax error without an op/3 declaration.
%
%   ǅabc = 1.                % ǅ (Lt, U+01C5) used to start a variable
%                            % via the derived Uppercase property.  Under
%                            % the Lu-only rule it starts an atom.
%
%   ͺ = 1.                  % U+037A (GREEK YPOGEGRAMMENI) is in
%                            % ID_Continue but not XID_Continue, so under
%                            % the XID-based rules it cannot start an
%                            % identifier.
