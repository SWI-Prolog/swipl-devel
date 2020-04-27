% Author: Bart Demoen K.U.Leuven
% Date: Oct 21 1996

makeground([]) .
makeground([X|R]) :- mkgr(X) , makeground(R) .

mkgr(X) :- var(X) , ! , (X = g ; X = i ; X = v) .
mkgr(_) .

mymember(X,[Y|R]) :- mm(X,R,Y) .
mm(X,_,X) .
mm(X,[Y|R],_) :- mm(X,R,Y) .

:- table(compute1/4) .

compute(Vars,DRs,Mal,DR) :- compute1(Vars,DRs,Mal,DR), fail .
compute(Vars,DRs,Mal,DR) :- compute1(Vars,DRs,Mal,DR).

compute1(Vars,DRs,Mal,DR) :-
	findall(Mal,(lookup(Vars,DRs),makeground(Mal)),All),
	sort(All,DR) .

lookup([],[]) .
lookup([V|VarList],[DR|DRList]) :- mymember(V,DR) , lookup(VarList,DRList) .

fail_iff_0([],_) :- fail .

iffequal_2([_,_],[[g,g],[i,i],[v,v]]) .

iff_1([_],[[g]]) .

iff_2([_,_],
[[g,g],
[i,i],
[i,v]]) .

iff_3([_,_,_],
[[g,g,g],
[i,g,i],
[i,g,v],
[i,i,g],
[i,i,i],
[i,i,v],
[i,v,g],
[i,v,i],
[i,v,v]]) .

iff_4([_,_,_,_],
[[g,g,g,g],
[i,g,g,i],
[i,g,g,v],
[i,g,i,g],
[i,g,i,i],
[i,g,i,v],
[i,g,v,g]|R]) :- r4_1(R) .
r4_1([
[i,g,v,i],
[i,g,v,v],
[i,i,g,g],
[i,i,g,i],
[i,i,g,v],
[i,i,i,g],
[i,i,i,i],
[i,i,i,v],
[i,i,v,g],
[i,i,v,i],
[i,i,v,v]|R]) :- r4_2(R) .
r4_2([
[i,v,g,g],
[i,v,g,i],
[i,v,g,v],
[i,v,i,g],
[i,v,i,i],
[i,v,i,v],
[i,v,v,g],
[i,v,v,i],
[i,v,v,v]]) .


iff_5([_,_,_,_,_],
[[g,g,g,g,g],
[i,g,g,g,i],
[i,g,g,g,v],
[i,g,g,i,g],
[i,g,g,i,i],
[i,g,g,i,v],
[i,g,g,v,g],
[i,g,g,v,i],
[i,g,g,v,v],
[i,g,i,g,g]|R]) :- r5_1(R) .
r5_1([
[i,g,i,g,i],
[i,g,i,g,v],
[i,g,i,i,g],
[i,g,i,i,i],
[i,g,i,i,v],
[i,g,i,v,g],
[i,g,i,v,i],
[i,g,i,v,v],
[i,g,v,g,g],
[i,g,v,g,i]|R]) :- r5_2(R) .
r5_2([
[i,g,v,g,v],
[i,g,v,i,g],
[i,g,v,i,i],
[i,g,v,i,v],
[i,g,v,v,g],
[i,g,v,v,i],
[i,g,v,v,v],
[i,i,g,g,g],
[i,i,g,g,i]|R]) :- r5_3(R) .
r5_3([
[i,i,g,g,v],
[i,i,g,i,g],
[i,i,g,i,i],
[i,i,g,i,v],
[i,i,g,v,g],
[i,i,g,v,i],
[i,i,g,v,v],
[i,i,i,g,g],
[i,i,i,g,i]|R]) :- r5_4(R) .
r5_4([
[i,i,i,g,v],
[i,i,i,i,g],
[i,i,i,i,i],
[i,i,i,i,v],
[i,i,i,v,g],
[i,i,i,v,i],
[i,i,i,v,v],
[i,i,v,g,g],
[i,i,v,g,i]|R]) :- r5_5(R) .
r5_5([
[i,i,v,g,v],
[i,i,v,i,g],
[i,i,v,i,i],
[i,i,v,i,v],
[i,i,v,v,g],
[i,i,v,v,i],
[i,i,v,v,v],
[i,v,g,g,g],
[i,v,g,g,i]|R]) :- r5_6(R) .
r5_6([
[i,v,g,g,v],
[i,v,g,i,g],
[i,v,g,i,i],
[i,v,g,i,v],
[i,v,g,v,g],
[i,v,g,v,i],
[i,v,g,v,v],
[i,v,i,g,g],
[i,v,i,g,i]|R]) :- r5_7(R) .
r5_7([
[i,v,i,g,v],
[i,v,i,i,g],
[i,v,i,i,i],
[i,v,i,i,v],
[i,v,i,v,g],
[i,v,i,v,i],
[i,v,i,v,v],
[i,v,v,g,g]|R]) :- r5_8(R) .
r5_8([
[i,v,v,g,i],
[i,v,v,g,v],
[i,v,v,i,g],
[i,v,v,i,i],
[i,v,v,i,v],
[i,v,v,v,g],
[i,v,v,v,i],
[i,v,v,v,v]]) .

iff_6([_,_,_,_,_,_],
[[g,g,g,g,g,g],
[i,g,g,g,g,i],
[i,g,g,g,g,v],
[i,g,g,g,i,g],
[i,g,g,g,i,i]|R]) :- r6_1(R) .
r6_1([
[i,g,g,g,i,v],
[i,g,g,g,v,g],
[i,g,g,g,v,i],
[i,g,g,g,v,v],
[i,g,g,i,g,g],
[i,g,g,i,g,i]|R]) :- r6_2(R) .
r6_2([
[i,g,g,i,g,v],
[i,g,g,i,i,g],
[i,g,g,i,i,i],
[i,g,g,i,i,v],
[i,g,g,i,v,g],
[i,g,g,i,v,i],
[i,g,g,i,v,v]|R]) :- r6_3(R) .
r6_3([
[i,g,g,v,g,g],
[i,g,g,v,g,i],
[i,g,g,v,g,v],
[i,g,g,v,i,g],
[i,g,g,v,i,i],
[i,g,g,v,i,v],
[i,g,g,v,v,g],
[i,g,g,v,v,i]|R]) :- r6_4(R) .
r6_4([
[i,g,g,v,v,v],
[i,g,i,g,g,g],
[i,g,i,g,g,i],
[i,g,i,g,g,v],
[i,g,i,g,i,g],
[i,g,i,g,i,i],
[i,g,i,g,i,v],
[i,g,i,g,v,g]|R]) :- r6_5(R) .
r6_5([
[i,g,i,g,v,i],
[i,g,i,g,v,v],
[i,g,i,i,g,g],
[i,g,i,i,g,i],
[i,g,i,i,g,v],
[i,g,i,i,i,g],
[i,g,i,i,i,i],
[i,g,i,i,i,v],
[i,g,i,i,v,g]|R]) :- r6_6(R) .
r6_6([
[i,g,i,i,v,i],
[i,g,i,i,v,v],
[i,g,i,v,g,g],
[i,g,i,v,g,i],
[i,g,i,v,g,v],
[i,g,i,v,i,g],
[i,g,i,v,i,i],
[i,g,i,v,i,v]|R]) :- r6_7(R) .
r6_7([
[i,g,i,v,v,g],
[i,g,i,v,v,i],
[i,g,i,v,v,v],
[i,g,v,g,g,g],
[i,g,v,g,g,i],
[i,g,v,g,g,v],
[i,g,v,g,i,g]|R]) :- r6_8(R) .
r6_8([
[i,g,v,g,i,i],
[i,g,v,g,i,v],
[i,g,v,g,v,g],
[i,g,v,g,v,i],
[i,g,v,g,v,v],
[i,g,v,i,g,g]|R]) :- r6_9(R) .
r6_9([
[i,g,v,i,g,i],
[i,g,v,i,g,v],
[i,g,v,i,i,g],
[i,g,v,i,i,i],
[i,g,v,i,i,v],
[i,g,v,i,v,g],
[i,g,v,i,v,i]|R]) :- r6_10(R) .
r6_10([
[i,g,v,i,v,v],
[i,g,v,v,g,g],
[i,g,v,v,g,i],
[i,g,v,v,g,v],
[i,g,v,v,i,g],
[i,g,v,v,i,i],
[i,g,v,v,i,v]|R]) :- r6_11(R) .
r6_11([
[i,g,v,v,v,g],
[i,g,v,v,v,i],
[i,g,v,v,v,v],
[i,i,g,g,g,g],
[i,i,g,g,g,i],
[i,i,g,g,g,v],
[i,i,g,g,i,g],
[i,i,g,g,i,i]|R]) :- r6_12(R) .
r6_12([
[i,i,g,g,i,v],
[i,i,g,g,v,g],
[i,i,g,g,v,i],
[i,i,g,g,v,v],
[i,i,g,i,g,g],
[i,i,g,i,g,i],
[i,i,g,i,g,v]|R]) :- r6_13(R) .
r6_13([
[i,i,g,i,i,g],
[i,i,g,i,i,i],
[i,i,g,i,i,v],
[i,i,g,i,v,g],
[i,i,g,i,v,i],
[i,i,g,i,v,v],
[i,i,g,v,g,g]|R]) :- r6_14(R) .
r6_14([
[i,i,g,v,g,i],
[i,i,g,v,g,v],
[i,i,g,v,i,g],
[i,i,g,v,i,i],
[i,i,g,v,i,v],
[i,i,g,v,v,g],
[i,i,g,v,v,i],
[i,i,g,v,v,v]|R]) :- r6_15(R) .
r6_15([
[i,i,i,g,g,g],
[i,i,i,g,g,i],
[i,i,i,g,g,v],
[i,i,i,g,i,g],
[i,i,i,g,i,i],
[i,i,i,g,i,v],
[i,i,i,g,v,g],
[i,i,i,g,v,i]|R]) :- r6_16(R) .
r6_16([
[i,i,i,g,v,v],
[i,i,i,i,g,g],
[i,i,i,i,g,i],
[i,i,i,i,g,v],
[i,i,i,i,i,g],
[i,i,i,i,i,i],
[i,i,i,i,i,v]|R]) :- r6_17(R) .
r6_17([
[i,i,i,i,v,g],
[i,i,i,i,v,i],
[i,i,i,i,v,v],
[i,i,i,v,g,g],
[i,i,i,v,g,i],
[i,i,i,v,g,v],
[i,i,i,v,i,g]|R]) :- r6_18(R) .
r6_18([
[i,i,i,v,i,i],
[i,i,i,v,i,v],
[i,i,i,v,v,g],
[i,i,i,v,v,i],
[i,i,i,v,v,v],
[i,i,v,g,g,g],
[i,i,v,g,g,i]|R]) :- r6_19(R) .
r6_19([
[i,i,v,g,g,v],
[i,i,v,g,i,g],
[i,i,v,g,i,i],
[i,i,v,g,i,v],
[i,i,v,g,v,g],
[i,i,v,g,v,i],
[i,i,v,g,v,v],
[i,i,v,i,g,g]|R]) :- r6_21(R) .
r6_21([
[i,i,v,i,g,i],
[i,i,v,i,g,v],
[i,i,v,i,i,g],
[i,i,v,i,i,i],
[i,i,v,i,i,v],
[i,i,v,i,v,g],
[i,i,v,i,v,i]|R]) :- r6_31(R) .
r6_31([
[i,i,v,i,v,v],
[i,i,v,v,g,g],
[i,i,v,v,g,i],
[i,i,v,v,g,v],
[i,i,v,v,i,g],
[i,i,v,v,i,i],
[i,i,v,v,i,v]|R]) :- r6_41(R) .
r6_41([
[i,i,v,v,v,g],
[i,i,v,v,v,i],
[i,i,v,v,v,v],
[i,v,g,g,g,g],
[i,v,g,g,g,i],
[i,v,g,g,g,v],
[i,v,g,g,i,g],
[i,v,g,g,i,i]|R]) :- r6_51(R) .
r6_51([
[i,v,g,g,i,v],
[i,v,g,g,v,g],
[i,v,g,g,v,i],
[i,v,g,g,v,v],
[i,v,g,i,g,g],
[i,v,g,i,g,i],
[i,v,g,i,g,v],
[i,v,g,i,i,g]|R]) :- r6_61(R) .
r6_61([
[i,v,g,i,i,i],
[i,v,g,i,i,v],
[i,v,g,i,v,g],
[i,v,g,i,v,i],
[i,v,g,i,v,v],
[i,v,g,v,g,g],
[i,v,g,v,g,i]|R]) :- r6_71(R) .
r6_71([
[i,v,g,v,g,v],
[i,v,g,v,i,g],
[i,v,g,v,i,i],
[i,v,g,v,i,v],
[i,v,g,v,v,g]|R]) :- r6_81(R) .
r6_81([
[i,v,g,v,v,i],
[i,v,g,v,v,v],
[i,v,i,g,g,g],
[i,v,i,g,g,i],
[i,v,i,g,g,v],
[i,v,i,g,i,g],
[i,v,i,g,i,i]|R]) :- r6_91(R) .
r6_91([
[i,v,i,g,i,v],
[i,v,i,g,v,g],
[i,v,i,g,v,i],
[i,v,i,g,v,v],
[i,v,i,i,g,g],
[i,v,i,i,g,i],
[i,v,i,i,g,v]|R]) :- r6_111(R) .
r6_111([
[i,v,i,i,i,g],
[i,v,i,i,i,i],
[i,v,i,i,i,v],
[i,v,i,i,v,g],
[i,v,i,i,v,i],
[i,v,i,i,v,v]|R]) :- r6_221(R) .
r6_221([
[i,v,i,v,g,g],
[i,v,i,v,g,i],
[i,v,i,v,g,v],
[i,v,i,v,i,g],
[i,v,i,v,i,i],
[i,v,i,v,i,v]|R]) :- r6_331(R) .
r6_331([
[i,v,i,v,v,g],
[i,v,i,v,v,i],
[i,v,i,v,v,v],
[i,v,v,g,g,g],
[i,v,v,g,g,i],
[i,v,v,g,g,v]|R]) :- r6_441(R) .
r6_441([
[i,v,v,g,i,g],
[i,v,v,g,i,i],
[i,v,v,g,i,v],
[i,v,v,g,v,g],
[i,v,v,g,v,i],
[i,v,v,g,v,v],
[i,v,v,i,g,g]|R]) :- r6_551(R) .
r6_551([
[i,v,v,i,g,i],
[i,v,v,i,g,v],
[i,v,v,i,i,g],
[i,v,v,i,i,i],
[i,v,v,i,i,v],
[i,v,v,i,v,g]|R]) :- r6_661(R) .
r6_661([
[i,v,v,i,v,i],
[i,v,v,i,v,v],
[i,v,v,v,g,g],
[i,v,v,v,g,i],
[i,v,v,v,g,v],
[i,v,v,v,i,g]|R]) :- r6_771(R) .
r6_771([
[i,v,v,v,i,i],
[i,v,v,v,i,v],
[i,v,v,v,v,g],
[i,v,v,v,v,i],
[i,v,v,v,v,v]]) .
