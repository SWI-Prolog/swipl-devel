:- op(200, xfy,[\]).    % lambda when bound var occurs only once
:- op(190, yfx,[@]).
:- op(150, fx,[^]).
:- op(150, fx,[*]).

:- op(800, xfx,[<==]).

:- op(900,xfx, (<-)).

:- table(interp1/1).

interp1(G) :- interp0(G).

interp0(true) :- !.
interp0((A,B)) :- !, interp1(A), interp1(B).
interp0(ge(A,B)) :- A >= B.
interp0(lt(A,B)) :- A < B.
interp0(G) :- myclause(G,B), interp1(B).

myclause(\A.*A(^m)(^\B.\C.believe_that*(B)(C)(^\D.\E.Ex F(*D(F) /\ *E(F))(^unicorn)(^\G.\H.*H(^j)(^\I.\J.*J(I)(^\K.\L.*K(^\M.find*(*M)(*L))(^\N.*N(G))) /\ \O.*O(I)(^\P.\Q.*P(^\R.eat*(*R)(*Q))(^\S.*S(G))))))),true).
myclause(\A.*A(^m)(^\B.\C.believe_that*(B)(C)(^\D.*D(^j)(^\E.F(^\G.\H.*H(E)(^\I.\J.*I(^\K.find*(*K)(*J))(^\L.*L(M))) /\ \N.*N(E)(^\O.\P.*O(^\Q.eat*(*Q)(*P))(^\R.*R(M))))))),true).
myclause(\A.*A(^m)(^\B.\C.*C(^j)(^\D.\E.\F.believe_that*(E)(F)(^G(^\H.\I.*I(D)(^\J.\K.*J(^\L.find*(*L)(*K))(^\M.*M(N))) /\ \O.*O(D)(^\P.\Q.*P(^\R.eat*(*R)(*Q))(^\S.*S(N)))))(B))),true).
myclause(\A.*A(^m)(^\B.\C.\D.Ex E(*C(E) /\ *D(E))(^unicorn)(^\F.\G.\H.believe_that*(G)(H)(^\I.*I(^j)(^\J.\K.*K(J)(^\L.\M.*L(^\N.find*(*N)(*M))(^\O.*O(F))) /\ \P.*P(J)(^\Q.\R.*Q(^\S.eat*(*S)(*R))(^\T.*T(F)))))(B))),true).
myclause(\A.*A(^m)(^\B.\C.\D.Ex E(*C(E) /\ *D(E))(^unicorn)(^\F.\G.\H.*H(^j)(^\I.\J.\K.believe_that*(J)(K)(^\L.*L(I)(^\M.\N.*M(^\O.find*(*O)(*N))(^\P.*P(F))) /\ \Q.*Q(I)(^\R.\S.*R(^\T.eat*(*T)(*S))(^\U.*U(F))))(G))(B))),true).
myclause(\A.*A(^m)(^\B.\C.*C(^j)(^\D.\E.F(^\G.\H.\I.believe_that*(H)(I)(^\J.*J(D)(^\K.\L.*K(^\M.find*(*M)(*L))(^\N.*N(O))) /\ \P.*P(D)(^\Q.\R.*Q(^\S.eat*(*S)(*R))(^\T.*T(O))))(E))(B))),true).
myclause(\A.*A(^j)(^\B.C(^\D.\E.*E(^m)(^\F.\G.believe_that*(F)(G)(^\H.*H(B)(^\I.\J.*I(^\K.find*(*K)(*J))(^\L.*L(M))) /\ \N.*N(B)(^\O.\P.*O(^\Q.eat*(*Q)(*P))(^\R.*R(M))))))),true).
myclause(\A.\B.Ex C(*A(C) /\ *B(C))(^unicorn)(^\D.\E.*E(^j)(^\F.\G.*G(^m)(^\H.\I.believe_that*(H)(I)(^\J.*J(F)(^\K.\L.*K(^\M.find*(*M)(*L))(^\N.*N(D))) /\ \O.*O(F)(^\P.\Q.*P(^\R.eat*(*R)(*Q))(^\S.*S(D))))))),true).
myclause(\A.*A(^m)(^\B.C(^\D.\E.*E(B)(^\F.G(^\H.\I.\J.believe_that*(I)(J)(^\K.*K(L)(^\M.\N.*M(^\O.find*(*O)(*N))(^\P.*P(Q))) /\ \R.*R(L)(^\S.\T.*S(^\U.eat*(*U)(*T))(^\V.*V(Q))))(F))))),true).
myclause(\A.*A(^j)(^\B.\C.*C(^m)(^\D.\E.*E(D)(^\F.G(^\H.\I.\J.believe_that*(I)(J)(^\K.*K(B)(^\L.\M.*L(^\N.find*(*N)(*M))(^\O.*O(P))) /\ \Q.*Q(B)(^\R.\S.*R(^\T.eat*(*T)(*S))(^\U.*U(P))))(F))))),true).
myclause(\A.*A(^m)(^\B.C(^\D.\E.*E(B)(^\F.\G.*G(^j)(^\H.\I.\J.believe_that*(I)(J)(^\K.*K(H)(^\L.\M.*L(^\N.find*(*N)(*M))(^\O.*O(P))) /\ \Q.*Q(H)(^\R.\S.*R(^\T.eat*(*T)(*S))(^\U.*U(P))))(F))))),true).
myclause(\A.\B.Ex C(*A(C) /\ *B(C))(^unicorn)(^\D.\E.*E(^m)(^\F.\G.*G(F)(^\H.\I.*I(^j)(^\J.\K.\L.believe_that*(K)(L)(^\M.*M(J)(^\N.\O.*N(^\P.find*(*P)(*O))(^\Q.*Q(D))) /\ \R.*R(J)(^\S.\T.*S(^\U.eat*(*U)(*T))(^\V.*V(D))))(H))))),true).
myclause(\A.*A(^m)(^\B.C(^\D.\E.*E(B)(^\F.\G.believe_that*(F)(G)(^H(^\I.\J.*J(K)(^\L.\M.*L(^\N.find*(*N)(*M))(^\O.*O(P))) /\ \Q.*Q(K)(^\R.\S.*R(^\T.eat*(*T)(*S))(^\U.*U(P)))))))),true).
myclause(\A.*A(^j)(^\B.\C.*C(^m)(^\D.\E.*E(D)(^\F.\G.believe_that*(F)(G)(^H(^\I.\J.*J(B)(^\K.\L.*K(^\M.find*(*M)(*L))(^\N.*N(O))) /\ \P.*P(B)(^\Q.\R.*Q(^\S.eat*(*S)(*R))(^\T.*T(O)))))))),true).
myclause(\A.*A(^m)(^\B.C(^\D.\E.*E(B)(^\F.\G.believe_that*(F)(G)(^\H.*H(^j)(^\I.\J.*J(I)(^\K.\L.*K(^\M.find*(*M)(*L))(^\N.*N(O))) /\ \P.*P(I)(^\Q.\R.*Q(^\S.eat*(*S)(*R))(^\T.*T(O)))))))),true).
myclause(\A.\B.Ex C(*A(C) /\ *B(C))(^unicorn)(^\D.\E.*E(^m)(^\F.\G.*G(F)(^\H.\I.believe_that*(H)(I)(^\J.*J(^j)(^\K.\L.*L(K)(^\M.\N.*M(^\O.find*(*O)(*N))(^\P.*P(D))) /\ \Q.*Q(K)(^\R.\S.*R(^\T.eat*(*T)(*S))(^\U.*U(D)))))))),true).
myclause(\A.*A(^j)(^\B.\C.*C(^m)(^\D.E(^\F.\G.*G(D)(^\H.\I.believe_that*(H)(I)(^\J.*J(B)(^\K.\L.*K(^\M.find*(*M)(*L))(^\N.*N(O))) /\ \P.*P(B)(^\Q.\R.*Q(^\S.eat*(*S)(*R))(^\T.*T(O)))))))),true).
myclause(\A.*A(^m)(^\B.C(^\D.E(^\F.\G.*G(B)(^\H.\I.believe_that*(H)(I)(^\J.*J(K)(^\L.\M.*L(^\N.find*(*N)(*M))(^\O.*O(P))) /\ \Q.*Q(K)(^\R.\S.*R(^\T.eat*(*T)(*S))(^\U.*U(P)))))))),true).
myclause(\A.\B.Ex C(*A(C) /\ *B(C))(^unicorn)(^\D.\E.*E(^m)(^\F.G(^\H.\I.*I(F)(^\J.\K.believe_that*(J)(K)(^\L.*L(M)(^\N.\O.*N(^\P.find*(*P)(*O))(^\Q.*Q(D))) /\ \R.*R(M)(^\S.\T.*S(^\U.eat*(*U)(*T))(^\V.*V(D)))))))),true).
myclause(\A.*A(^j)(^\B.C(^\D.\E.*E(^m)(^\F.\G.*G(F)(^\H.\I.believe_that*(H)(I)(^\J.*J(B)(^\K.\L.*K(^\M.find*(*M)(*L))(^\N.*N(O))) /\ \P.*P(B)(^\Q.\R.*Q(^\S.eat*(*S)(*R))(^\T.*T(O)))))))),true).
myclause(\A.\B.Ex C(*A(C) /\ *B(C))(^unicorn)(^\D.\E.*E(^j)(^\F.\G.*G(^m)(^\H.\I.*I(H)(^\J.\K.believe_that*(J)(K)(^\L.*L(F)(^\M.\N.*M(^\O.find*(*O)(*N))(^\P.*P(D))) /\ \Q.*Q(F)(^\R.\S.*R(^\T.eat*(*T)(*S))(^\U.*U(D)))))))),true).
myclause(\A.*A(^j)(^\B.\C.*C(^m)(^\D.E(^\F.\G.*G(D)(^\H.\I.believe_that*(H)(I)(^\J.*J(B)(^\K.\L.*K(^\M.find*(*M)(*L))(^\N.*N(O))))) /\ \P.*P(B)(^\Q.\R.*Q(^\S.eat*(*S)(*R))(^\T.*T(O)))))),true).
myclause(\A.*A(^m)(^\B.C(^\D.E(^\F.\G.*G(B)(^\H.\I.believe_that*(H)(I)(^\J.*J(K)(^\L.\M.*L(^\N.find*(*N)(*M))(^\O.*O(P))))) /\ \Q.*Q(K)(^\R.\S.*R(^\T.eat*(*T)(*S))(^\U.*U(P)))))),true).
myclause(\A.\B.Ex C(*A(C) /\ *B(C))(^unicorn)(^\D.\E.*E(^m)(^\F.G(^\H.\I.*I(F)(^\J.\K.believe_that*(J)(K)(^\L.*L(M)(^\N.\O.*N(^\P.find*(*P)(*O))(^\Q.*Q(D))))) /\ \R.*R(M)(^\S.\T.*S(^\U.eat*(*U)(*T))(^\V.*V(D)))))),true).
myclause(\A.*A(^j)(^\B.C(^\D.\E.*E(^m)(^\F.\G.*G(F)(^\H.\I.believe_that*(H)(I)(^\J.*J(B)(^\K.\L.*K(^\M.find*(*M)(*L))(^\N.*N(O))))) /\ \P.*P(B)(^\Q.\R.*Q(^\S.eat*(*S)(*R))(^\T.*T(O)))))),true).
myclause(\A.\B.Ex C(*A(C) /\ *B(C))(^unicorn)(^\D.\E.*E(^j)(^\F.\G.*G(^m)(^\H.\I.*I(H)(^\J.\K.believe_that*(J)(K)(^\L.*L(F)(^\M.\N.*M(^\O.find*(*O)(*N))(^\P.*P(D))))) /\ \Q.*Q(F)(^\R.\S.*R(^\T.eat*(*T)(*S))(^\U.*U(D)))))),true).
myclause(\A.*A(^j)(^\B.C(^\D.\E.*E(^m)(^\F.\G.believe_that*(F)(G)(^\H.*H(B)(^\I.\J.*I(^\K.find*(*K)(*J))(^\L.*L(M))))) /\ \N.*N(B)(^\O.\P.*O(^\Q.eat*(*Q)(*P))(^\R.*R(M))))),true).
myclause(\A.\B.Ex C(*A(C) /\ *B(C))(^unicorn)(^\D.\E.*E(^j)(^\F.\G.*G(^m)(^\H.\I.believe_that*(H)(I)(^\J.*J(F)(^\K.\L.*K(^\M.find*(*M)(*L))(^\N.*N(D))))) /\ \O.*O(F)(^\P.\Q.*P(^\R.eat*(*R)(*Q))(^\S.*S(D))))),true).
myclause(\A.*A(^j)(^\B.C(^\D.\E.*E(^m)(^\F.\G.*G(F)(^\H.\I.believe_that*(H)(I)(^\J.*J(B)(^\K.\L.*K(^\M.find*(*M)(*L))(^\N.*N(O)))))) /\ \P.*P(B)(^\Q.\R.*Q(^\S.eat*(*S)(*R))(^\T.*T(O))))),true).
myclause(\A.\B.Ex C(*A(C) /\ *B(C))(^unicorn)(^\D.\E.*E(^j)(^\F.\G.*G(^m)(^\H.\I.*I(H)(^\J.\K.believe_that*(J)(K)(^\L.*L(F)(^\M.\N.*M(^\O.find*(*O)(*N))(^\P.*P(D)))))) /\ \Q.*Q(F)(^\R.\S.*R(^\T.eat*(*T)(*S))(^\U.*U(D))))),true).
myclause(\A.*A(^m)(^\B.\C.*C(B)(^\D.\E.believe_that*(D)(E)(^\F.\G.Ex H(*F(H) /\ *G(H))(^unicorn)(^\I.\J.*J(^j)(^\K.\L.*L(K)(^\M.\N.*M(^\O.find*(*O)(*N))(^\P.*P(I))) /\ \Q.*Q(K)(^\R.\S.*R(^\T.eat*(*T)(*S))(^\U.*U(I)))))))),true).
myclause(\A.*A(^m)(^\B.\C.*C(B)(^\D.\E.believe_that*(D)(E)(^\F.*F(^j)(^\G.H(^\I.\J.*J(G)(^\K.\L.*K(^\M.find*(*M)(*L))(^\N.*N(O))) /\ \P.*P(G)(^\Q.\R.*Q(^\S.eat*(*S)(*R))(^\T.*T(O)))))))),true).
myclause(\A.*A(^m)(^\B.\C.*C(B)(^\D.\E.*E(^j)(^\F.\G.\H.believe_that*(G)(H)(^I(^\J.\K.*K(F)(^\L.\M.*L(^\N.find*(*N)(*M))(^\O.*O(P))) /\ \Q.*Q(F)(^\R.\S.*R(^\T.eat*(*T)(*S))(^\U.*U(P)))))(D)))),true).
myclause(\A.*A(^m)(^\B.\C.*C(B)(^\D.\E.\F.Ex G(*E(G) /\ *F(G))(^unicorn)(^\H.\I.\J.believe_that*(I)(J)(^\K.*K(^j)(^\L.\M.*M(L)(^\N.\O.*N(^\P.find*(*P)(*O))(^\Q.*Q(H))) /\ \R.*R(L)(^\S.\T.*S(^\U.eat*(*U)(*T))(^\V.*V(H)))))(D)))),true).
myclause(\A.*A(^m)(^\B.\C.*C(B)(^\D.\E.\F.Ex G(*E(G) /\ *F(G))(^unicorn)(^\H.\I.\J.*J(^j)(^\K.\L.\M.believe_that*(L)(M)(^\N.*N(K)(^\O.\P.*O(^\Q.find*(*Q)(*P))(^\R.*R(H))) /\ \S.*S(K)(^\T.\U.*T(^\V.eat*(*V)(*U))(^\W.*W(H))))(I))(D)))),true).
myclause(\A.*A(^m)(^\B.\C.*C(B)(^\D.\E.*E(^j)(^\F.\G.H(^\I.\J.\K.believe_that*(J)(K)(^\L.*L(F)(^\M.\N.*M(^\O.find*(*O)(*N))(^\P.*P(Q))) /\ \R.*R(F)(^\S.\T.*S(^\U.eat*(*U)(*T))(^\V.*V(Q))))(G))(D)))),true).
myclause(\A.\B.Ex C(*A(C) /\ *B(C))(^unicorn)(^\D.\E.*E(^m)(^\F.\G.believe_that*(F)(G)(^\H.*H(^j)(^\I.\J.*J(I)(^\K.\L.*K(^\M.find*(*M)(*L))(^\N.*N(D))) /\ \O.*O(I)(^\P.\Q.*P(^\R.eat*(*R)(*Q))(^\S.*S(D))))))),true).
myclause(\A.*A(^j)(^\B.\C.*C(^m)(^\D.\E.believe_that*(D)(E)(^F(^\G.\H.*H(B)(^\I.\J.*I(^\K.find*(*K)(*J))(^\L.*L(M))) /\ \N.*N(B)(^\O.\P.*O(^\Q.eat*(*Q)(*P))(^\R.*R(M))))))),true).
myclause(\A.\B.Ex C(*A(C) /\ *B(C))(^unicorn)(^\D.\E.*E(^m)(^\F.\G.*G(^j)(^\H.\I.\J.believe_that*(I)(J)(^\K.*K(H)(^\L.\M.*L(^\N.find*(*N)(*M))(^\O.*O(D))) /\ \P.*P(H)(^\Q.\R.*Q(^\S.eat*(*S)(*R))(^\T.*T(D))))(F)))),true).
myclause(\A.*A(^j)(^\B.\C.*C(^m)(^\D.E(^\F.\G.\H.believe_that*(G)(H)(^\I.*I(B)(^\J.\K.*J(^\L.find*(*L)(*K))(^\M.*M(N))) /\ \O.*O(B)(^\P.\Q.*P(^\R.eat*(*R)(*Q))(^\S.*S(N))))(D)))),true).
