/**********************************************************************

             Unit Tests for

                split_string/4
		concat_atom/2
		concat_string/2
		string_list/2
		substring/3
		substring/5
		string_code/3

***********************************************************************/

split_string(S, " ", " ", Sub) should_raise 4.
split_string("this is a string", Sep, " ", Sub) should_raise 4.
split_string("this is a string", " ", Pad, Sub) should_raise 4.
%SWI split_string('this is an atom', " ", " ", Sub) should_raise 5.
%SWI split_string("this is a string", ' ', " ", Sub) should_raise 5.
%SWI split_string("this is a string", " ", ' ', Sub) should_raise 5.
%SWI split_string("this is a string", " ", " ", atom) should_raise 5.
split_string(1, " ", " ", Sub) should_raise 5.
split_string(1.1, " ", " ", Sub) should_raise 5.
split_string(1_1, " ", " ", Sub) should_raise 5.
split_string(11111111111111111111111111111111111111, " ", " ", Sub) should_raise 5.
%SWI split_string(1.0__1.1, " ", " ", Sub) should_raise 5.
split_string(f(1,2,3), " ", " ", Sub) should_raise 5.
%SWI split_string([1,2,3], " ", " ", Sub) should_raise 5.
%SWI split_string([], " ", " ", Sub) should_raise 5.
%SWI    bag_create(B),
%SWI split_string(B, " ", " ", Sub) should_raise 5.
%SWI    make_suspension(true,3,S),
%SWI split_string(S, " ", " ", Sub) should_raise 5.
split_string("string", 1, " ", Sub) should_raise 5.
split_string("string", 1.1, " ", Sub) should_raise 5.
split_string("string", 1_1, " ", Sub) should_raise 5.
split_string("string", 111111111111111111111111111111, " ", Sub) should_raise 5.
%SWI split_string("string", 1.1__1.2, " ", Sub) should_raise 5.
%SWI split_string("string", [], " ", Sub) should_raise 5.
%SWI split_string("string", [1,2,3,4], " ", Sub) should_raise 5.
split_string("string", f(1.1,2,3), " ", Sub) should_raise 5.
split_string("string", " ", f(1.1,2,3), Sub) should_raise 5.
%SWI split_string("string", " ", [1,2,3], Sub) should_raise 5.
split_string("string", " ", 1, Sub) should_raise 5.
split_string("string", " ", 1.1, Sub) should_raise 5.
split_string("string", " ", 1_1, Sub) should_raise 5.
%SWI split_string("string", " ", 1.0__1.1, Sub) should_raise 5.
%SWI split_string("string", " ", 11111111111111111111111111111111111, Sub) should_raise 5.
%SWI    make_suspension(true,3,S),
%SWI split_string("string", " ", S, Sub) should_raise 5.
%SWI    bag_create(B),
%SWI split_string("string", " ", B, Sub) should_raise 5.
%SWI split_string("string", " ", " ", 1) should_raise 5.
%SWI split_string("string", " ", " ", 1.1) should_raise 5.
%SWI split_string("string", " ", " ", 1_1) should_raise 5.
%SWI split_string("string", " ", " ", 1.0__1.1) should_raise 5.
%SWI split_string("string", " ", " ", 1111111111111111111111111111111111111) should_raise 5.
split_string("string", " ", " ", f(1,2,3)) should_raise 5.
%SWI    make_suspension(true,3,S),
%SWI split_string("string", " ", " ", S) should_raise 5.
%SWI    bag_create(B),
%SWI split_string("string", " ", " ", B) should_raise 5.

split_string("/home/joachim/eclipse/sepia/i386_linux/","/","",L) should_give
    L=["", "home", "joachim", "eclipse", "sepia", "i386_linux", ""].
split_string("/home/joachim//","/","",L) should_give
    L=["", "home", "joachim", "", ""].
split_string("home/joachim/","/","",L) should_give
    L=["home", "joachim", ""].
split_string("home//joachim/","/","",L) should_give
    L=["home", "", "joachim", ""].
split_string("//home/joachim/","/","",L) should_give
    L=["","","home", "joachim", ""].
split_string("/home/joachim/","/","e",L) should_give
    L=["","hom", "joachim", ""].
split_string("/home/joachim/","/","o",L) should_give
    L=["","home", "joachim", ""].
split_string("/home/joachim/","/","m ",L) should_give
    L=["","home", "joachi", ""].
split_string("","/","m ",L)		should_give L=[""].
split_string("/","/","m ",L)		should_give L=["",""].
split_string(" / ","/","m ",L)		should_give L=["",""].
split_string(" m ","/","m ",L)		should_give L=[""].
split_string("/abc/def//","","",L)	should_give L=["/abc/def//"].
split_string("/abc/def//","/","",L)	should_give L=["","abc","def","",""].
split_string("/abc/def//","","/",L)	should_give L=["abc/def"].
split_string("/abc/def//","/","/",L)	should_give L=["abc","def"].
split_string("///","","",L)		should_give L=["///"].
split_string("///","","/",L)		should_give L=[""].
split_string("///","/","",L)		should_give L=["","","",""].
split_string("///","/","/",L)		should_give L=[""].
split_string(" ///",""," ",L)		should_give L=["///"].
split_string(" ///",""," /",L)		should_give L=[""].
split_string(" ///","/"," ",L)		should_give L=["","","",""].
split_string(" ///","/"," /",L)		should_give L=[""].
split_string("/// ",""," ",L)		should_give L=["///"].
split_string("/// ",""," /",L)		should_give L=[""].
split_string("/// ","/"," ",L)		should_give L=["","","",""].
split_string("/// ","/"," /",L)		should_give L=[""].
split_string("////usr/local//eclipse/", "/", "/", L)	should_give
	L = ["usr", "local", "eclipse"].
split_string(" ////usr/local//eclipse/", "/", "/ ", L)	should_give
	L = ["usr", "local", "eclipse"].
split_string("/// /usr/local//eclipse/", "/", "/ ", L)	should_give
	L = ["usr", "local", "eclipse"].
split_string("/// /usr:local//eclipse/", ":/", "/ ", L)	should_give
	L = ["usr", "local", "eclipse"].
split_string("/// /usr/:local//eclipse/", ":/", "/ ", L)	should_give
	L = ["usr", "", "local", "eclipse"].
split_string("/// /usr/:/local//eclipse/", ":/", "/ ", L)	should_give
	L = ["usr", "", "", "local", "eclipse"].
split_string("/// /usr/://local//eclipse/", ":/", "/ ", L)	should_give
	L = ["usr", "", "", "local", "eclipse"].
split_string("  ", ":/", "/ ", L)	should_give L = [""].
split_string("///", ":/", "/ ", L)	should_give L = [""].
split_string(":", ":/", "/ ", L)	should_give L = ["", ""].
split_string(":///", ":/", "/ ", L)	should_give L = ["", ""].
split_string("//:///", ":/", "/ ", L)	should_give L = ["", ""].
split_string("/// ", ":/", "/ ", L)	should_give L = [""].
split_string("/ ", ":/", "/ ", L)	should_give L = [""].
split_string(" /", ":/", "/ ", L)	should_give L = [""].
split_string("a/ /bug", "/", " ", L)	should_give L = ["a","","bug"].

% atomic_list_concat/2.

atomic_list_concat(L, S) should_raise 4.
atomic_list_concat([this, contains, V, variables], S) should_raise 4.
atomic_list_concat([a,b,f(a,b),c], S) should_raise 5.
atomic_list_concat([[a,b,c],b,c], S) should_raise 5.
%SWI atomic_list_concat([concat, this], "concatthis") should_raise 5.
/*
   make_suspension(true,3,S),
atomic_list_concat([a,S,c], X) should_raise 5.
   create_bag(B),
atomic_list_concat([a,B,c], X) should_raise 5.
*/
%SWI atomic_list_concat([a,b,c], 1) should_raise 5.
%SWI atomic_list_concat([a,b,c], 1.1) should_raise 5.
%SWI atomic_list_concat([a,b,c], 1_1) should_raise 5.
%SWI atomic_list_concat([a,b,c], 11111111111111111111111111111111111111) should_raise 5.
%SWI atomic_list_concat([a,b,c], 1.0__1.1) should_raise 5.
%SWI    make_suspension(true,3,S),
%SWI atomic_list_concat([a,b,c], S) should_raise 5.
%SWI    bag_create(B),
%SWI atomic_list_concat([a,b,c], B) should_raise 5.
atomic_list_concat(["this", "is"], this) should_fail.
atomic_list_concat(atom, X) should_raise 5.
atomic_list_concat("string", X) should_raise 5.
atomic_list_concat(1, X) should_raise 5.
atomic_list_concat(1.1, X) should_raise 5.
atomic_list_concat(1_1, X) should_raise 5.
atomic_list_concat(1111111111111111111111111111111111, X) should_raise 5.
%SWI atomic_list_concat(1.0__1.1, X) should_raise 5.
%SWI     make_suspension(true,3,S),
%SWI atomic_list_concat(S, X) should_raise 5.
%SWI     bag_create(B),
%SWI atomic_list_concat(B, X) should_raise 5.
atomic_list_concat(f(1,2,3), X) should_raise 5.

atomic_list_concat(["this","will","become","an","atom"], S) should_give
	atom(S), S = thiswillbecomeanatom.
atomic_list_concat([123,456], S) should_give atom(S), S == '123456'.
atomic_list_concat(["how",  "about", " ", ?], S) should_give S == 'howabout ?'.
atomic_list_concat([1.234,abc,
	9223372036854775807,atomic,'Atom',"string"], S) should_give
	  S == '1.234abc9223372036854775807atomicAtomstring'.
atomic_list_concat([a,/,b,/,c,+,>], S) should_give  S == 'a/b/c+>'.
atomic_list_concat(["","","","",''], S) should_give S == ''.
%SWI (foreach(E, L), for(I, 1, 1234) do  E = a), atomic_list_concat(L, S) should_give
%SWI	    atom(S), atom_length(S, 1234).
atomic_list_concat(["this", '\n', should, "\n", have, "\n","newlines"], S) should_give
            S == 'this\nshould\nhave\nnewlines'.
atomic_list_concat(["  spaces ", " ", ' ', "   ", are, "significant!    "], S) should_give
	    S == '  spaces      aresignificant!    '.
atomic_list_concat(["\\","\\"], S) should_give atom_length(S,2), S == '\\\\'.
atomic_list_concat(["\"", string, "\""], S) should_give S == '\"string\"'.
%SWI atomic_list_concat([2_4,0.00000000001], S) should_give S == '1_21e-11'.

% atomics_to_string/2

atomics_to_string(L, S) should_raise 4.
atomics_to_string([this, contains, V, variables], S) should_raise 4.
atomics_to_string([a,b,f(a,b),c], S) should_raise 5.
atomics_to_string([[a,b,c],b,c], S) should_raise 5.
%SWI atomics_to_string([concat, this], concatthis) should_raise 5.
/*
   make_suspension(true,3,S),
atomics_to_string([a,S,c], X) should_raise 5.
   create_bag(B),
atomics_to_string([a,B,c], X) should_raise 5.
*/
%SWI atomics_to_string([a,b,c], 1) should_raise 5.
%SWI atomics_to_string([a,b,c], 1.1) should_raise 5.
%SWI atomics_to_string([a,b,c], 1_1) should_raise 5.
%SWI atomics_to_string([a,b,c], 11111111111111111111111111111111111111) should_raise 5.
%SWI atomics_to_string([a,b,c], 1.0__1.1) should_raise 5.
%SWI    make_suspension(true,3,S),
%SWI atomics_to_string([a,b,c], S) should_raise 5.
%SWI    bag_create(B),
%SWI atomics_to_string([a,b,c], B) should_raise 5.
atomics_to_string(["this", "is"], "this") should_fail.
atomics_to_string(atom, X) should_raise 5.
atomics_to_string("string", X) should_raise 5.
atomics_to_string(1, X) should_raise 5.
atomics_to_string(1.1, X) should_raise 5.
atomics_to_string(1_1, X) should_raise 5.
atomics_to_string(1111111111111111111111111111111111, X) should_raise 5.
%SWI atomics_to_string(1.0__1.1, X) should_raise 5.
%SWI     make_suspension(true,3,S),
%SWI atomics_to_string(S, X) should_raise 5.
%SWI     bag_create(B),
atomics_to_string(B, X) should_raise 4.
atomics_to_string(f(1,2,3), X) should_raise 5.

atomics_to_string([this,will,become,a,string], S) should_give
	string(S), S = "thiswillbecomeastring".
atomics_to_string([123,456], S) should_give string(S), S == "123456".
atomics_to_string(["how",  "about", " ", ?], S) should_give S == "howabout ?".
atomics_to_string([1.234,abc,
	9223372036854775807,atomic,'Atom',"string"], S) should_give
	  S == "1.234abc9223372036854775807atomicAtomstring".
atomics_to_string([a,/,b,/,c,+,>], S) should_give  S == "a/b/c+>".
atomics_to_string(["","","","",''], S) should_give S == "".
%SWI (foreach(E, L), for(I, 1, 1234) do  E = a), atomics_to_string(L, S) should_give
%SWI	    string(S), string_length(S, 1234).
atomics_to_string(["this", '\n', should, "\n", have, "\n","newlines"], S) should_give
            S == "this\nshould\nhave\nnewlines".
atomics_to_string(["  spaces ", " ", ' ', "   ", are, "significant!    "], S) should_give
	    S == "  spaces      aresignificant!    ".
atomics_to_string(["\\","\\"], S) should_give string_length(S,2), S == "\\\\".
atomics_to_string(["\"", string, "\""], S) should_give S == "\"string\"".
%SWI atomics_to_string([2_4,0.00000000001], S) should_give S == "1_21e-11".


% string_codes/2

string_codes(X,Y) should_raise 4.
%SWI string_codes(atom, Y) should_raise 5.
%SWI string_codes(1, Y) should_raise 5.
%SWI string_codes(1.1, Y) should_raise 5.
%SWI string_codes(1_1, Y) should_raise 5.
%SWI string_codes(1.0__1.1, Y) should_raise 5.
%SWI string_codes(111111111111111111111111111111111111111111, Y) should_raise 5.
string_codes(f(1,2,3), Y) should_raise 5.
%SWI string_codes([1,2,3], Y) should_raise 5.
%SWI    make_suspension(true,3,S),
%SWI string_codes(S, Y) should_raise 5.
%SWI    bag_create(B),
%SWI string_codes(B, Y) should_raise 5.
%SWI string_codes(X, atom) should_raise 5.
%SWI string_codes(X, "string") should_raise 5.
%SWI string_codes(X, 1) should_raise 5.
%SWI string_codes(X, 1.1) should_raise 5.
%SWI string_codes(X, 1_2) should_raise 5.
%SWI string_codes(X, 1.0__1.1) should_raise 5.
%SWI string_codes(X, 11111111111111111111111111111111111111111111111) should_raise 5.
string_codes(X, f(1,2,3)) should_raise 5.
%SWI    make_suspension(true,3,S),
%SWI string_codes(X,S) should_raise 5.
%SWI    bag_create(B),
%SWI string_codes(X,B) should_raise 5.
string_codes(X, [1,a,3]) should_raise 5.
string_codes(X, [1,A,3]) should_raise 4.
string_codes(X, [1,"string",3]) should_raise 5.
string_codes(X, [1,1.1,3]) should_raise 5.
%SWI string_codes(X, [1,1_1,3]) should_raise 5.
%SWI string_codes(X, [1,1.1__1.2,3]) should_raise 5.
string_codes(X, [1,1111111111111111111111111111111111111111111,3]) should_raise 5.
string_codes(X, [1,f(1.1,2,3),3]) should_raise 5.
string_codes(X, [1,[1,2,3],3]) should_raise 5.
string_codes(X, [1,[],3]) should_raise 5.
%SWI    bag_create(B),
%SWI string_codes(X, [1,B,3]) should_raise 5.
%SWI    make_suspension(true,3,S),
%SWI string_codes(X, [1,S,3]) should_raise 5.
string_codes(S,[A|[128]]) should_raise 4.
string_codes(S,[1|A]) should_raise 4.
%SWI string_codes('string',L) should_raise 5.
%SWI string_codes(S,"list") should_raise 5.
%SWI string_codes('string',[128]) should_raise 5.
string_codes(S,["B"]) should_raise 5.
%SWI string_codes(S,[256]) should_raise 6.
%SWI string_codes(S,[-1]) should_raise 6.

string_codes(S,[65,98,99])          should_give S="Abc".
string_codes("abc",L)               should_give L=[97,98,99].
string_codes("abc",[97,A,99])       should_give A=98.
string_codes(S,[127])               should_give S="\177".
string_codes("abc",[97|A])          should_give A=[98,99].

string_codes("abc",[98,99,100]) should_fail.

% string_code(+Index, +String, ?Code)

string_code(1,_,_) should_raise 4.
get_string_code(_,"abc",_) should_raise 4.
%SWI string_code(1,abc,_) should_raise 5.
string_code(1,1,_) should_raise 5.
string_code(1.2,"abc",_) should_raise 5.
string_code(a,"abc",_) should_raise 24.
get_string_code(0,"abc",_) should_raise 6.
get_string_code(4,"abc",_) should_raise 6.
string_code(1,"abc",C) should_give C==97.
string_code(2,"abc",C) should_give C==98.
string_code(3,"abc",C) should_give C==99.
string_code(-1, "abc",_) should_raise 6.
% possible alternative behaviour:
%string_code(-1,"abc",C) should_give C==99.
%string_code(-2,"abc",C) should_give C==98.
%string_code(-3,"abc",C) should_give C==97.



% ECLiPSe's number_string doesn't allow leading nor trailing spaces
% ISO number_chars/codes allows leading spaces
% ECLiPSe's number_string/2 is not the parser: it is not syntax-setting
% dependent and does not allow:
% - leading space or comments
% - spaces or comment between sign and number
% - ISO-syntax 0xf3

number_string(1.2,"1.2")	should_give true.
number_string(1.0e9,"1.0E9")	should_give true.
number_string(1,"01")	should_give true.
number_string(1,"a")	should_fail.
number_string(1,"")	should_fail.
number_string(N,X)	should_raise 4.
number_string(N,'1')	should_raise 5.
number_string(N,[a|a])	should_raise 5.
%SWI number_string(N,[49])	should_raise 5.
%SWI number_string(N,[])	should_raise 5.
number_string(N,"3 ")	should_fail.
number_string(N,"3.")	should_fail.
number_string(N,"0'a")	should_give N==97.
number_string(N," 0'a")	should_fail.
number_string(N,"-1")	should_give N== -1.
number_string(N,"- 1")	should_fail.
number_string(N,"/**/1")	should_fail.
number_string(N,"-/**/1")	should_fail.
number_string(N,"1e1")	should_give N==10.0.
number_string(N,"1.0e")	should_fail.
number_string(N,"1.0ee")	should_fail.
%SWI number_string(N,"0x1")	should_fail.
number_string(N,"0X1")	should_fail.
number_string(1E1,S)	should_give S=="10.0".
%SWI number_string(3_4,S)	should_give S="3_4".
%SWI number_string(N,"3_4")	should_give N=3_4.
%SWI number_string(3_4,S)	should_give S="3_4".
%SWI number_string(N,"3_4")	should_give N=3_4.
number_string(-0.0,S)	should_give S="-0.0".
number_string(N,"-0.0")	should_give N= -0.0.
%SWI number_string(-0.0__0.5,S)	should_give S="-0.0__0.5".
%SWI number_string(N,"-0.0__0.5")	should_give N= -0.0__0.5.
number_string(N,"16'ffff")	should_give N= 65535.

number_string(a,"1")	should_raise 5.
number_string("1","1")	should_raise 5.
number_string(foo(1),"1")	should_raise 5.
number_string([],"1")	should_raise 5.
number_string([1],"1")	should_raise 5.

