/* This file contains a grammar for PTQ. */

:- import append/3, member/2 from basics.
:- import parse/2, lred/2, lwrite/1 from parser.
:- import abolish_table_info/0 from machine.
:- import numbervars/3 from num_vars.

:- op(200, xfy,[\]).    % lambda when bound var occurs only once
:- op(190, yfx,[@]).
:- op(150, fx,[^]).
:- op(150, fx,[*]).

:- op(800, xfx,[<==]).

:- op(900,xfx, (<-)).

/* TL formulas are represented as:
        Variables are Prolog variables
        lambda X T -->  X\T
        P(Q) --> P@Q
        P and Q --> P /\ Q
        P or Q --> P \/ Q
        P implies Q --> P -> Q
        Exists X P --> exists(X,P)
        Forall X P --> all(X,P)
        ^X --> ^X
        extension of X --> *X
*/

:- table(s/5).
:- table(te1/7).
:- table(cn/6).
:- table(iv/6).
%:- notable te/7, iav/5.

tran(String) :-
	abolish_table_info,
        (	parse(s(_M),String),
%        	(numbervars(_M,0,_),     %write(' Direct: '),
%	         write('top level solution'),
%	                lwrite(_M),nl,
%%	                write_canonical(_M),nl,
%        	        fail;true),
%%       	lred(_M,R),
%%       	(numbervars(R,0,_),write('Reduced: '),lwrite(R),nl,fail;true),
	        fail
	;   true
	).

s(M) --> s(M,[],[]).

st(r2,Te,(Det,Cn)) :- Te <==Det@ ^Cn.
st(r3,Cn,(X,Cn1,T)) :-  Cn <== X\ (Cn1@X /\ T).  
st(r4,T,(Te,Iv)) :- T <== Te@ ^Iv.   
st(r5,Iv,(Tv,Te)) :-  Iv <== Tv@ ^Te.  
st(r6,Adv,(Pp,Te)) :- Adv <== Pp@ ^Te.  
st(r7,Iv,(Vb,T)) :- Iv <== Vb@ ^T.  
st(r8,Iv,(Vb,Iv1)) :- Iv <==  Vb@ ^Iv1.   
st(r10,Iv,(Adv,Iv1)) :- Iv <== Adv@ ^Iv1.  
st(r11a,T,(S1,S2)) :- T <== S1 /\ S2.  
st(r11b,T,(S1,S2)) :- T <==  S1 \/  S2.   
st(r12a,Iv,(Iv1,Iv2)) :-  Iv <==  X\ (Iv1@X /\ Iv2@X).
st(r12b,Iv,(Iv1,Iv2)) :- Iv <== X\ (Iv1@X  \/ Iv2@X).   
st(r13,Te,(Te1,Te2)) :- Te  <==  P\ (Te1@P  \/ Te2@P).   
st(r14,T,(X,Te,T1))  :- T  <== Te@ ^(X\T1).
st(r15,Cn,(X,Te,Cn1)) :- Cn <== (Y\Te@ ^(X\ (Cn1@Y))).  
st(r16,Iv,(X,Te,Iv1)) :- Iv <== (Y\Te@ ^(X\ (Iv1@Y))).  
st(rpro,Te,X) :- Te <== P\ *P@X.  

X <== Y :- X=Y.       % all translations
%X <== Y :- lred(Y,X).   % reduce on the way.

/* The simple grammar */
s(T,Sa,Sb) -->                  % s4
        te(Te,sub,_G,Sa,Sb1), 
                {append(Sa,Sb1,Sa1)},
        iv(Iv,s,Sa1,Sb2),
                {append(Sb1,Sb2,Sb),
                 st(r4,T,(Te,Iv))}.
s(T,Sa,Sb) -->                  % s11
        s(S1,Sa,Sb1), 
        word(and), 
                {append(Sa,Sb1,Sa1)},
        s(S2,Sa1,Sb2),
                {append(Sb1,Sb2,Sb),
                st(r11a,T,(S1,S2))}.
s(T,Sa,Sb) -->                  % s11
        s(S1,Sa,Sb1), 
        word(or), 
                {append(Sa,Sb1,Sa1)},
        s(S2,Sa1,Sb2),
                {append(Sb1,Sb2,Sb),
                st(r11b,T,(S1,S2))}.
s(T,Sa,Sb) -->                  % s14
        s(T1,Sa,Sb1),
                {delete(sub(Te,X,_),Sb1,Sb), not_occurs_in(X,Sb),
                st(r14,T,(X,Te,T1))}.

iv(Iv,T,_,[]) --> biv(Iv,T).            % s1
iv(Iv,T,Sa,Sb) -->              % s5
        tv(Tv,T), te(Te,obj,_G,Sa,Sb),
                {st(r5,Iv,(Tv,Te))}.
iv(Iv,T,Sa,Sb) -->      % s12
        iv(Iv1,T,Sa,Sb1), word(and), 
                {append(Sa,Sb1,Sa1)},
        iv(Iv2,T,Sa1,Sb2),
                {append(Sb1,Sb2,Sb),
                st(r12a,Iv,(Iv1,Iv2))}.
iv(Iv,T,Sa,Sb) -->      % s12
        iv(Iv1,T,Sa,Sb1), word(or), 
                {append(Sa,Sb1,Sa1)},
        iv(Iv2,T,Sa1,Sb2),
                {append(Sb1,Sb2,Sb),
                st(r12b,Iv,(Iv1,Iv2))}.
iv(Iv,T,Sa,Sb) -->              % s10
        iv(Iv1,T,Sa,Sb1), 
                {append(Sa,Sb1,Sa1)},
        iav(Adv,Sa1,Sb2),
                {append(Sb1,Sb2,Sb),
                st(r10,Iv,(Adv,Iv1))}.
iv(Iv,Tn,Sa,Sb) -->             % s7
        bivt(Vb,Tn), s(T,Sa,Sb),
                {st(r7,Iv,(Vb,T))}.
iv(Iv,T,Sa,Sb) -->                      % s8
        biviv(Vb,T), iv(Iv1,i,Sa,Sb),
                {st(r8,Iv,(Vb,Iv1))}.
iv(Iv,T,Sa,Sb) --> % s16
        iv(Iv1,T,Sa,Sb1),
                {delete(sub(Te,X,_),Sb1,Sb), not_occurs_in(X,Sb),
                st(r16,Iv,(X,Te,Iv1))}.



iav(Adv,_,[]) --> biav(Adv).            % s1
iav(Adv,Sa,Sb) -->                      % s6
        pr(Pp), te(Te,obj,_G,Sa,Sb),
                {st(r6,Adv,(Pp,Te))}.

te(Te,C,G,Sa,Sb) --> te1(Te,C,G,Sa,Sb).
te(Te,C,G,Sa,[]) --> 
        word(Pro),
                {pro(Pro,G,C),member(sub(_,X,G),Sa),
                st(rpro,Te,X)}.
te(Te,C,G,Sa,[sub(M,X,G)|Sb]) -->
        te1(M,C,G,Sa,Sb),
                {st(rpro,Te,X)}.

te1(Te,_C,G,_,[]) --> bte(Te,G).
te1(Te,_C,G,Sa,Sb) -->          % s2
        det(Det), 
        cn(Cn,G,Sa,Sb),
                {st(r2,Te,(Det,Cn))}.
te1(Te,C,G,Sa,Sb) -->   % s13
        te(Te1,C,G,Sa,Sb1), 
        word(or), 
                {append(Sa,Sb1,Sa1)},
        te(Te2,C,_,Sa1,Sb2),
                {append(Sb1,Sb2,Sb),
                st(r13,Te,(Te1,Te2))}.


det(P\Q\exists(X,(*P@X /\ *Q@X))) --> word(a).  % s2
det(P\Q\exists(X,(*P@X /\ *Q@X))) --> word(the).        % s2, wrong hack
det(P\Q\all(X,(*P@X -> *Q@X))) --> word(every). % s2

cn(Cn,G,_,[]) --> bcn(Cn,G).
cn(Cn,G,Sa,Sb) -->      %s3
        cn(Cn1,G,Sa,Sb1), 
                {append(Sa,Sb1,Sa1)},
        word(such_that),
        s(T,[sub(Cn1,X,G)|Sa1],Sb2),
                {append(Sb1,Sb2,Sb),
                st(r3,Cn,(X,Cn1,T))}.
cn(Cn,G,Sa,Sb) --> % s15
        cn(Cn1,G,Sa,Sb1),
        {delete(sub(Te,X,_),Sb1,Sb), not_occurs_in(X,Sb),
        st(r15,Cn,(X,Te,Cn1))}.


tv(M,T) --> btv(M,T).

pro(him,m,obj).
pro(he,m,sub).
pro(her,f,obj).
pro(she,f,sub).
pro(it,n,_).

/** intensional version **
biv(run,s) --> word(runs).
biv(run,i) --> word(run).
biv(walk,s) --> word(walks).
biv(walk,i) --> word(walk).
biv(talk,s) --> word(talks).
biv(talk,i) --> word(talk).     **/

biv(X\ 'run*'@ *X,s) --> word(runs).
biv(X\ 'run*'@ *X,i) --> word(run).
biv(X\ ('walk*'@ *X),s) --> word(walks).
biv(X\ ('walk*'@ *X),i) --> word(walk).
biv(X\ ('talk*'@ *X),s) --> word(talks).
biv(X\ ('talk*'@ *X),i) --> word(talk).
biv(rise,s) --> word(rises).
biv(rise,i) --> word(rise).
biv(change,s) --> word(changes).
biv(change,i) --> word(change).

bte(P\ *P@ (^j),m) --> word(john).
bte(P\ *P@ (^b),m) --> word(bill).
bte(P\ *P@ (^m),f) --> word(mary).
bte(P\ *P@ (^n),n) --> word(ninety).

/** intensional version **
btv(find,s) --> word(finds).
btv(find,i) --> word(find).
btv(lose,s) --> word(loses).
btv(lose,i) --> word(lose).
btv(eat,s) --> word(eats).
btv(eat,i) --> word(eat).
btv(love,s) --> word(loves).
btv(love,i) --> word(love).
btv(date,s) --> word(dates).
btv(date,i) --> word(date).     **/

btv(P\X\ (*P@ (^(Y\ 'find*'@ *Y @ *X))),s) --> word(finds).
btv(P\X\ (*P@ (^(Y\ 'find*'@ *Y @ *X))),i) --> word(find).
btv(P\X\ (*P@ (^(Y\ 'lose*'@ *Y @ *X))),s) --> word(loses).
btv(P\X\ (*P@ (^(Y\ 'lose*'@ *Y @ *X))),i) --> word(lose).
btv(P\X\ (*P@ (^(Y\ 'eat*'@ *Y @ *X))),s) --> word(eats).
btv(P\X\ (*P@ (^(Y\ 'eat*'@ *Y @ *X))),i) --> word(eat).
btv(P\X\ (*P@ (^(Y\ 'love*'@ *Y @ *X))),s) --> word(loves).
btv(P\X\ (*P@ (^(Y\ 'love*'@ *Y @ *X))),i) --> word(love).
btv(P\X\ (*P@ (^(Y\ 'date*'@ *Y @ *X))),s) --> word(dates).
btv(P\X\ (*P@ (^(Y\ 'date*'@ *Y @ *X))),i) --> word(date).


btv(P\X\ *P@ (Y\ (*X= *Y)),s) --> word(is).
btv(P\X\ *P@ (Y\ (*X= *Y)),i) --> word(be).

/** intensional version
btv(seek,s) --> word(seeks).
btv(seek,i) --> word(seek).
btv(conceive,s) --> word(conceives).
btv(conceive,i) --> word(conceive).     **/

btv(PP\X\ 'seek*'@ PP@ *X,s) --> word(seeks).
btv(PP\X\ 'seek*'@ PP@ *X,i) --> word(seek).
btv(PP\X\ 'conceive*'@PP@ *X,s) --> word(conceives).
btv(PP\X\ 'conceive*'@PP@ *X,i) --> word(conceive).

btt(necessarily) --> word(necessarily). % wrong

bcn(man,m) --> word(man).
bcn(woman,f) --> word(woman).
bcn(park,n) --> word(park).
bcn(fish,n) --> word(fish).
bcn(pen,n) --> word(pen).
bcn(unicorn,n) --> word(unicorn).
bcn(price,n) --> word(price).
bcn(temperature,n) --> word(temperature).

biav(rapidly) --> word(rapidly).
biav(slowly) --> word(slowly).
biav(voluntarily) --> word(voluntarily).
biav(allegedly) --> word(allegedly).

/** intensional version
pr(in) --> word(in).    **/

pr(PP\Q\X\ *PP@ ^(Y\ 'in*'@ *Y@ Q@X)) --> word(in).

pr(about) --> word(about).

/** intension version
bivt(believe_that,s) --> word(believes_that).
bivt(believe_that,i) --> word(believe_that).
bivt(assert_that,s) --> word(asserts_that).
bivt(assert_that,i) --> word(assert_that). **/

bivt(Pp\X\ 'believe_that*'@Pp@X,s) --> word(believes_that).
bivt(Pp\X\ 'believe_that*'@Pp@X,i) --> word(believe_that).
bivt(Pp\X\ 'assert_that*'@Pp@X,s) --> word(asserts_that).
bivt(Pp\X\ 'assert_that*'@Pp@X,i) --> word(assert_that).

/** intensional version
biviv(try_to,s) --> word(tries_to).
biviv(try_to,i) --> word(try_to).
biviv(wish_to,s) --> word(wishes_to).
biviv(wish_to,i) --> word(wish_to). **/

biviv(Pp\X\ 'try_to*'@Pp@X,s) --> word(tries_to).
biviv(Pp\X\ 'try_to*'@Pp@X,i) --> word(try_to).
biviv(Pp\X\ 'wish_to*'@Pp@X,s) --> word(wishes_to).
biviv(Pp\X\ 'wish_to*'@Pp@X,i) --> word(wish_to).


delete(X,[X|L],L).
delete(X,[Y|L0],[Y|L1]) :- delete(X,L0,L1).

not_occurs_in(X,T) :- X==T,!,fail.
not_occurs_in(_X,T) :- var(T),!.
not_occurs_in(_X,T) :- atomic(T),!.
not_occurs_in(X,T) :- functor(T,_F,A),not_occurs_in(X,T,A).

not_occurs_in(_X,_T,0) :- !.
not_occurs_in(X,T,A) :- arg(A,T,Y),not_occurs_in(X,Y),
        A1 is A-1, not_occurs_in(X,T,A1).

/* Examples:

tran([john,walks]).
tran([john,dates,mary]).
tran([a,price,rises]).
tran([john,dates,him]).
tran([john,walks,in,a,park]).
tran([john,finds,a,unicorn]).

tran([john,seeks,a,unicorn]).

tran([a,man,tries_to,walk]).
tran([a,woman,tries_to,run,allegedly]).
tran([john,believes_that,mary,walks,in,a,park]).
tran([john,believes_that,mary,wishes_to,walk,in,a,park]).
	(78 sec xemu, 52 xemuopt)
tran([john,talks,about,a,unicorn]).
tran([john,wishes_to,find,a,unicorn,and,eat,it]).
tran([john,seeks,a,unicorn,and,mary,seeks,it]).
tran([mary,believes_that,john,finds,a,unicorn,and,he,eats,it]).
tran([john,tries_to,find,a,unicorn,and,wishes_to,eat,it]).
tran([every,man,loves,a,woman]).
tran([every,man,loves,a,woman,in,a,park,voluntarily,and,every,fish,eats,a,pen,slowly]).
tran([a,woman,such_that,she,walks,runs]).
tran([every,man,loves,a,woman,such_that,she,loves,him]).
tran([every,man,loves,a,woman,and,she,loves,him]).

*/
