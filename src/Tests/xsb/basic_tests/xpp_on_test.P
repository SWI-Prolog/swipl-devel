
test :-
	compile('empty.P',[xpp_on]), writeln(ok1),
	compile('empty.P',[xpp_on]), writeln(ok2),
	compile('empty2.P',[xpp_on]), writeln(ok3),
	compile('empty2.P',[xpp_on]), writeln(ok4).
