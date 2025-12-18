%
% Unit tests for format/2
%
% Corresponding to PIP-0110-format, Draft Nov 2025
%

format('hello',[])              should_output "hello".
format("hello",[])              should_output "hello".
format([0'h,0'e,0'l,0'l,0'o],[]) should_output "hello".
format([h,e,l,l,o],[])          should_output "hello".
format([h,e,l,0'l,0'o],[])      should_throw _something.
format([h,0'e|_],[])            should_throw _something.
format(_,[])                    should_throw _something.
format(foo(bar),[])             should_throw _something.
format(123,[])                  should_throw _something.
format(hello,42)                should_throw _something.
format(hello,world)             should_throw _something.
format(hello,{})                should_throw _something.
format(hello,foo(bar))          should_throw _something.
format(hello,_)                 should_throw _something.
format(hello,[world])           should_throw _something.


format("~a",[hello])            should_output "hello".
format("~a",['[]'])             should_output "[]".
format("~a",[{}])               should_output "{}".
format("~3a",[hello])           should_output "hello".  % ignore count
format("~*a",[3,hello])         should_output "hello".  % ignore count
format("~*a",[1+2,hello])       should_output "hello".  % ignore count
format("~a",[])                 should_throw _something.
format("~a",[hello,world])      should_throw _something.
format("~a",[[0'h,0'e,0'l,0'l,0'o]]) should_throw _something.
format("~a",[[h,e,l,l,o]])      should_throw _something.
format("~a",[42])               should_throw _something.
format("~a",[foo(bar)])         should_throw _something.
format("~a",[_])                should_throw _something.


format("~c", [0'a])             should_output "a".
format("~3c", [0'a])            should_output "aaa".
format("~*c", [3,0'a])          should_output "aaa".
format("~*c", [1+2,0'a])        should_output "aaa".
format("~c", [a])               should_throw _something.
format("~c", [[]])              should_throw _something.
format("~c", [foo(bar)])        should_throw _something.
format("~c", [_])               should_throw _something.


format("~s",[abc])              should_output "abc".
format("~s",["abc"])            should_output "abc".
format("~s",[[]])               should_output "".
format("~s",[[0'a,0'b,0'c]])    should_output "abc".
format("~s",[[a,b,c]])          should_output "abc".
format("~6s",[abcdefghi])       should_output "abcdef".
format("~6s",[abcdef])          should_output "abcdef".
format("~6s",[abc])             should_output "abc   ".
format("~*s",[6,abcdefghi])     should_output "abcdef".
format("~*s",[4+2,abcdefghi])   should_output "abcdef".
format("~*s",[4+2,abc])         should_output "abc   ".
format("~0s",[abc])             should_output "".
format("~s",[42])               should_throw _something.
format("~s",[[h,e,l,0'l,0'o]])  should_throw _something.
format("~s",[[0'h,0'e|_]])      should_throw _something.
format("~s",[foo(bar)])         should_throw _something.
format("~s",[_])                should_throw _something.


format("~~",[])                 should_output "~".
%format("~3~",[])                should_output "~".     % Extension
format("~~",[abc])              should_throw _something.


format("~e",[123.456789])	should_output "1.234568e+02".
format("~e",[122+1.456789])	should_output "1.234568e+02".
format("~12e",[123.456789])	should_output "1.234567890000e+02".
format("~3e",[123.456789])	should_output "1.235e+02".
format("~*e",[1+2,123.456789])	should_output "1.235e+02".
format("~0e",[123.456789])	should_output "1e+02".
format("~e",[123])              should_output "1.230000e+02".
format("~0e",[123])	        should_output "1e+02".
format("~0e",[foo(bar)])        should_throw _something.
format("~0e",[_])	        should_throw _something.


format("~E",[123.456789])	should_output "1.234568E+02".
format("~E",[122+1.456789])	should_output "1.234568E+02".
format("~12E",[123.456789])	should_output "1.234567890000E+02".
format("~3E",[123.456789])	should_output "1.235E+02".
format("~*E",[1+2,123.456789])	should_output "1.235E+02".
format("~0E",[123.456789])	should_output "1E+02".
format("~E",[123])              should_output "1.230000E+02".
format("~0E",[123])	        should_output "1E+02".
format("~0E",[foo(bar)])        should_throw _something.
format("~0E",[_])	        should_throw _something.


format("~f",[12.3456789])	should_output "12.345679".
format("~f",[11+1.3456789])	should_output "12.345679".
format("~12f",[12.3456789])	should_output "12.345678900000".
format("~3f",[12.3456789])	should_output "12.346".
format("~0f",[12.3456789])	should_output "12".
format("~f",[123])              should_output "123.000000".
format("~0f",[123])	        should_output "123".
format("~0f",[foo(bar)])        should_throw _something.
format("~0f",[_])	        should_throw _something.


format("~g",[12.3456789])	should_output "12.3457".
format("~12g",[12.3456789])	should_output "12.3456789".
format("~3g",[12.3456789])	should_output "12.3".
format("~2g",[12.3456789])	should_output "12".
format("~1g",[12.3456789])	should_output "1e+01".
format("~0g",[12.3456789])	should_output "1e+01".
format("~g",[1234567.89])	should_output "1.23457e+06".
format("~12g",[1234567.89])	should_output "1234567.89".
format("~3g",[1234567.89])	should_output "1.23e+06".
format("~2g",[1234567.89])	should_output "1.2e+06".
format("~1g",[1234567.89])	should_output "1e+06".
format("~0g",[1234567.89])	should_output "1e+06".
format("~3g",[1234567])	        should_output "1.23e+06".


format("~G",[12.3456789])	should_output "12.3457".
format("~12G",[12.3456789])	should_output "12.3456789".
format("~3G",[12.3456789])	should_output "12.3".
format("~2G",[12.3456789])	should_output "12".
format("~1G",[12.3456789])	should_output "1E+01".
format("~0G",[12.3456789])	should_output "1E+01".
format("~G",[1234567.89])	should_output "1.23457E+06".
format("~12G",[1234567.89])	should_output "1234567.89".
format("~3G",[1234567.89])	should_output "1.23E+06".
format("~2G",[1234567.89])	should_output "1.2E+06".
format("~1G",[1234567.89])	should_output "1E+06".
format("~0G",[1234567.89])	should_output "1E+06".
format("~3G",[1234567])         should_output "1.23E+06".


format("~d",[1234567])          should_output "1234567".
format("~0d",[1234567])         should_output "1234567".
format("~3d",[1234567])         should_output "1234.567".
format("~d",[-1234567])         should_output "-1234567".
format("~0d",[-1234567])        should_output "-1234567".
format("~3d",[-1234567])        should_output "-1234.567".
format("~d",[123.0])            should_throw _something.
format("~d",[foo(bar)])         should_throw _something.
format("~d",[_])                should_throw _something.


format("~D",[1234567])          should_output "1,234,567".
format("~0D",[1234567])         should_output "1,234,567".
format("~3D",[1234567])         should_output "1,234.567".
format("~D",[-1234567])         should_output "-1,234,567".
format("~0D",[-1234567])        should_output "-1,234,567".
format("~3D",[-1234567])        should_output "-1,234.567".
format("~D",[123.0])            should_throw _something.
format("~D",[foo(bar)])         should_throw _something.
format("~D",[_])                should_throw _something.


format("~r",[123])              should_output "173".
format("~2r",[123])             should_output "1111011".
format("~8r",[123])             should_output "173".
format("~10r",[123])            should_output "123".
format("~16r",[123])            should_output "7b".
format("~36r",[123])            should_output "3f".
format("~36r",[36*36*36-1])     should_output "zzz".
format("~*r",[36,123])          should_output "3f".
format("~r",[-123])             should_output "-173".
format("~36r",[-123])           should_output "-3f".
format("~37r",[123])            should_throw _something.
format("~1r",[123])             should_throw _something.
format("~0r",[123])             should_throw _something.
format("~*r",[-1,123])          should_throw _something.
format("~r",[123.0])            should_throw _something.
format("~r",[foo(bar)])         should_throw _something.
format("~r",[_])                should_throw _something.

format("~16R",[123])            should_output "7B".
format("~36R",[123])            should_output "3F".
format("~36R",[36*36*36-1])     should_output "ZZZ".
format("~36R",[-123])           should_output "-3F".
format("~R",[123.0])            should_throw _something.
format("~R",[foo(bar)])         should_throw _something.
format("~R",[_])                should_throw _something.


format('~k', ['A'+'B'])         should_output "+('A','B')".
format('~q', ['A'+'B'])         should_output "'A'+'B'".
format('~q', ['A'+'B'])         should_output "'A'+'B'".
format('~w', ['A'+'B'])         should_output "A+B".
format('~w', [])                should_throw _something.
format('~w', ['A'+'B'])         should_output "A+B".


format('~W', [a+'B',[]])        should_output "a+B".
format('~W', [a+'B',[quoted(true),ignore_ops(true)]])
                                should_output "+(a,'B')".


format('~a~i~a',[abc,def,ghi])	should_output "abcghi".
format('~a~3i~a',[abc,def,ghi])	should_output "abcghi".  % ignore count


format('a~nb', [])              should_output "a\nb".
format('a~0nb', [])             should_output "ab".
format('a~n~nb', [])            should_output "a\n\nb".
format('a~3nb', [])             should_output "a\n\n\nb".


format('a~Nb', [])              should_output "a\nb".
format('a~N~Nb', [])            should_output "a\nb".
format('a~3N~3Nb', [])          should_output "a\nb".    % ignore count


format('left~tright', [])	        should_output "leftright".
format('left~tright~|', [])	        should_output "leftright".
format('left~tright~8|', [])	        should_output "leftright".
format('left~tright~20|', [])	        should_output "left           right".
format('~|left~tright~20|', [])	        should_output "left           right".
format('left~tright~20+', [])	        should_output "left           right".
format('left~10+next', [])	        should_output "left      next".
format('left~t~10+next', [])	        should_output "left      next".
format('~tright~10+next', [])	        should_output "     rightnext".

format('^~|abc~10+$',[])                should_output "^abc       $".
format('^~|abc~11|$',[])                should_output "^abc       $".
format('^~|abc~*+$',[10])               should_output "^abc       $".
format('^~|abc~*|$',[11])               should_output "^abc       $".
format('^~|abc~t~10+$',[])	        should_output "^abc       $".
format('^~|~tabc~t~10+$',[])	        should_output "^   abc    $".
format('^~|~t~tabc~t~10+$',[])	        should_output "^    abc   $".
format('^~|~t~t~tabc~t~10+$',[])	should_output "^     abc  $".
format('^~|~tabc~10+$',[])	        should_output "^       abc$".
format('^~|~ta~tb~tc~t~10+$',[])        should_output "^ a  b  c  $".
format('^~|a~tb~tc~t~10+$',[])          should_output "^a  b  c   $".

format('^~|~*tabc~*t~10+$',[0'.,95])	should_output "^...abc____$".
format('^~|~`.tabc~95t~10+$',[])	should_output "^...abc____$".
format('~*+.~n~3+.~n~*+.~n', [3,3])     should_output "   .\n   .\n   .\n".

format('~', [])                         should_throw _something.
