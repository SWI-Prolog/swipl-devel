:- encoding(utf8).

% data(?Data)
%
%  Provide data for QLF round trip.
%  @bug: If this is turned into a PlDoc comment and PlDoc is enabled
%  qcompile/1 creates invalid output.

data(1).
data('Hallo').
data('ታዲያስ').
data('مرحبا').
data('Kaixo').
data('নমস্কার').
data('Olá').
data('سلام').
data('მიესალმები').
data('नमस्ते').
data('こんにちは').

data('😁').
data('🤣').

data('with\u0000null').
