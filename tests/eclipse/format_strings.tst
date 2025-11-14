%
% Extra unit tests for format/2 with a `string` data type
%
%     - swipl
%     - eclipse -L iso
%
% JW: ECLiPSe uses `hello` for a packed string, whereas SWI-Prolog uses "hello".

format("hello",[]) should_output "hello".

format("~a",["hello"]) should_output "hello".

format("~s",["abc"]) should_output "abc".

