likes(mary,john).
likes(mary,potplants).
likes(mary,jane).
likes(mary,paul).
likes(mary,'Heartbreak High').
likes(john,mary).
likes(fidothedog,mary).
likes(mary,sunbathing).
likes(bugs,mary).
likes(john,'The X files').
likes(paul,mary).
likes(paul,sue).

person(mary).
person(john).
person(sue).
person(paul).
person(jane).

happy(X) :-
	person(X),
	likes(Y, X),
	person(Y).
