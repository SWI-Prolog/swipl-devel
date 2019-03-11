:- table a/2.

tw :- a(X,Y),write(X),write(' '),write(Y),nl,fail.
tw.

ts :- statistics,a(X,Y),fail.
ts :- statistics.

a(X,Y) :- p(X,Y).
a(X,Y) :- a(X,Z),p(Z,Y).

p(a1,a2).
p(a2,a3).
p(a3,a4).
p(a4,a5).
p(a5,a6).
p(a6,a7).
p(a7,a8).
p(a8,a9).
p(a9,a10).
p(a10,a11).
p(a11,a12).
p(a12,a13).
p(a13,a14).
p(a14,a15).
p(a15,a16).
p(a16,a17).
p(a17,a18).
p(a18,a19).
p(a19,a20).
p(a20,a21).
p(a21,a22).
p(a22,a23).
p(a23,a24).
p(a24,a25).
p(a25,a26).
p(a26,a27).
p(a27,a28).
p(a28,a29).
p(a29,a30).
p(a30,a31).
p(a31,a32).
p(a32,a33).
p(a33,a34).
p(a34,a35).
p(a35,a36).
p(a36,a37).
p(a37,a38).
p(a38,a39).
p(a39,a40).
p(a40,a41).
p(a41,a42).
p(a42,a43).
p(a43,a44).
p(a44,a45).
p(a45,a46).
p(a46,a47).
p(a47,a48).
p(a48,a49).
p(a49,a50).
p(a50,a51).
p(a51,a52).
p(a52,a53).
p(a53,a54).
p(a54,a55).
p(a55,a56).
p(a56,a57).
p(a57,a58).
p(a58,a59).
p(a59,a60).
p(a60,a1).
