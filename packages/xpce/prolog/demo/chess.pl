/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
\chapter{Example: A Chess Interface}	\label{sec:chess}

\index{chess front-end}
In         chapters~\ref{sec:starting},~\ref{sec:graphics}         and
\ref{sec:pceprolog} we have explained  the fundamentals of programming
PCE/Prolog.  In this section we will combine  much of this material in
a realistic  example.  The  domain   is a graphics  front-end for  the
terminal oriented program /usr/games/chess.

The  interface  presented  in    this chapter  is rather primitive: no
administration of taken pieces, no time management, no  indication who
is to move next, etc.  The (annotated) sources of this example  may be
found in the library file demo/chess.pl.  The chesstool may be started
from the manual tools or from the Prolog top level by typing:

	1 ?- [demo(chess)].
	2 ?- chess.

Which will (after some moves) produce the window shown in
figure~\ref{fig:chess}.

\postscriptfig{chess}{Graphical front-end for chess}


\section{Functional Decomposition}

A chess-board consists  of $8\times8$ squares,  each  of which  has  a
black-  or   white   background and optionally  a  piece  on  it.  The
basic following operations need to be implemented on the board:

	- Create board.
	- Put a black- or white piece at some square.
	- Remove a piece from a square.
	- Generate the initial chess position.
	- Detect a gesture with the mouse to move a piece and generate
	  the moved piece and coordinates from the gesture.


\section{Design}

The most obvious realisation is to use a  compound graphical object as
a starting point  to    represent the board and   primitive  graphical
objects to represent each of the 64 squares.

The pieces themselves are represented by bitmap objects.  A  bitmap is
a   graphical  object that   displays  an {\em  image}  object:  a two
dimensional array  of   (possibly  coloured)  pixels.     One possible
implementation  would be to  create the board as  a  whole using black
(grey) and  white  bitmaps for each of  the squares   and display  the
pieces on top of these squares.

In the current implementation  of PCE  images are not  transparent and
are always rectangular, which  makes it impossible to  display a piece
on a black square.  For this reason we use 4 images  for each piece: a
white piece on a white square, a white piece on a black square, etc.

The PCE bitmap library defines bitmaps for the various chess-pieces in
both colours and with both a black- and a white background.  The image
`bishop.bm' is a $64\times64$ image.  See figure~\ref{fig:bishop}.

\postscriptfig{bishop}{Library bitmap for a bishop}

\section{Implementation}

\subsection{Declarations: Using Prolog Modules}

\index{prolog,modules}\index{module (prolog)}
The chess interface contains a  large  number of local predicates.  To
avoid the necessity to use long names for these  local predicates it is
implemented as a Prolog module.   See \cite{SWI-Prolog:manual}  and/or
\cite{SICStus:manual} for details on Prolog modules.

The module `library(pce)'  defines the  basic interface predicates and
is  typically imported into any  module using PCE.  Additional library
predicates are best  declared   using the  directive  require/1,   see
\cite{SICStus:manual}.  Note that  SWI-Prolog does not need  either of
these  declarations.   It  will  automatically  import  the  basic PCE
predicates from the module `user' when they  are encountered.  Library
predicates    are  normally   loaded   by  SWI-Prolog's    autoloader.
PCE/SWI-Prolog   processes the   use_module/1 declaration similar   to
SICStus Prolog and ignores the require/1 directive.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(pcechess, [chess/0]).
:- use_module(library(pce)).
:- require([ between/3
	   , call/2
	   , concat/3
	   , concat_atom/2
	   , free_variables/2
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Squares and Pieces}		\label{sec:squareimage}

As described above bitmaps are used to represent squares with optional
pieces  on them.  The pixels  of a bitmap  are represented by an image
object.  An image object  may be used  in many bitmaps simultaneously.
The predicate square_image/4 maps a  piece,  its colour and the colour
of the square into an image object for  this combination.  Because the
image is reusable and many copies are needed  it maintains  a table of
already computed images using the dynamic predicate computed_image/4.

The computed images are $32 \times  32$ sub-images of the image loaded
from file and is created  using the method `image  <-clip: area'.  The
resulting image is stored in the  Prolog database.  The method `object
->lock_object' informs  PCE that Prolog  has a permanent  reference to
this   object  and  protects it  from    PCE  garbage collection  (see
appendix~\ref{sec:memory}).

\index{attribute,to object}
Next, it  is useful to  know {\em what}  is  represented  on the image
object.  This is achieved  by associating  two {\em attribute} objects
to the image: `piece' and `colour'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
	computed_image/4.

%	square_image(+PieceName, +PieceColour, +SquareColour, -Image)

square_image(Piece, PieceColour, SquareColour, Image) :-
	computed_image(Piece, PieceColour, SquareColour, Image), !.
square_image(Piece, PieceColour, SquareColour, Image) :-
	image_name(Piece, _, ImageName),
	new(TotalImage, image(ImageName)),
	sub_area(PieceColour, SquareColour, Area), !,
	get(TotalImage, clip, Area, Image),
	send(Image, lock_object, @on),
	send(Image, attribute, attribute(piece, Piece)),
	send(Image, attribute, attribute(colour, PieceColour)),
	asserta(computed_image(Piece, PieceColour, SquareColour, Image)).

%	image_name(?PieceName, ?ChessProgram Id, ?ImageName)

image_name(empty,	_, 'chesssquare.bm').
image_name(pawn,	p, 'pawn.bm').
image_name(rook,	r, 'rook.bm').
image_name(knight,	n, 'knight.bm').
image_name(bishop,	b, 'bishop.bm').
image_name(king,	k, 'king.bm').
image_name(queen,	q, 'queen.bm').

%	sub_area(+PieceColour, +SquareColour, -AreaTerm)

sub_area(white, white, area(32,  0, 32, 32)).
sub_area(white, black, area(0,   0, 32, 32)).
sub_area(black, white, area(32, 32, 32, 32)).
sub_area(black, black, area(0,  32, 32, 32)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Below is a  small  test to demonstrate the  usage of this  part of the
program.

	1 ?- test_square(rook, black, white, Image),
	     get(Image, colour, Colour),
	     get(Image, piece, Piece).
	Colour = black, Piece = rook, Image = @773336/image 
	
	test_square(Piece, Colour, SquareColour, Image) :-
		(object(@p) -> true ; send(new(@p, picture), open)),
		square_image(Piece, Colour, SquareColour, Image),
		send(@p, display, bitmap(Image)).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		/********************************
		*           THE BOARD		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
In this section we will create  the chess-board.  The chess-board  is an
instance of  class  \class{device},  which represents a collection  of
graphical objects.  Each  of the  objects is a bitmap  of $32\times32$
pixels.   Each  of the bitmap  objects will  be  extended with various
attributes:

	# square_colour
	Represents  the  colour  of the  square.   This  attribute  is
	attached to the  bitmap rather than  to  the image  because it
	will not change during the life-time of the board.

	# Get methods for <-piece and <-colour
	These methods allow  us to ask the piece  and colour currently
	represented by this  bitmap.  The information itself is stored
	on the <-image of the bitmap (see above).

	A  \class{get_method} object consists  of a selector, the type
	of the return  value,  a vector  describing the   types of the
	requested arguments  and the implementation.   The latter must
	be an instance of class \class{function}.

	In  section~\ref{sec:starting}       we       introduced   the
	\class{message}  object as  a template  for a  send-operation.
	The get-operation  has a  similar  object:  an \idx{obtainer}.
	The class name for an  obtainer  is  `?'\index{?  (obtainer)}.
	The PCE/Prolog interface defines   `?'  as an  infix operator,
	which allows us to write
	
		\line{Receiver?Selector}
	
	for  obtainers that  do   not  require  arguments.   When  the
	get_method  is  invoked,  it will   process the arguments  and
	evaluate the   obtainer object, returning  the   result of the
	get-operation.

	\index{method,arguments to}
	The implementation of a  method may refer to the  object it is
	attached to using the  function  @receiver.  The arguments  to
	the method  may be referred  to as @arg1, @arg2,  ...  As  the
	resulting method object has no direct references to the object
	it is attached to it may be associated with multiple objects.

	# Name = location in algebraic chess notation
	Graphical objects have a name.  This name may be used to refer
	to them    (see  section~\ref{sec:devicemember}).  The default
	name of a graphical is its class name.   In our case it is more
	natural to give each  bitmap the name of  its location in  the
	chess notation: a1,  a2, ... a8,  b1, ... b8, ... h8.

The directive pce_global/2 declares a globally available object.  See
section~\ref{sec:global}.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@square_colour, new(get_method(colour, name, new(vector),
					    @receiver?image?colour))).
:- pce_global(@square_piece, new(get_method(piece, name, new(vector),
					   @receiver?image?piece))).

make_chess_board(Board) :-
	new(Board, device),
	(   between(0, 7, X),
	    between(0, 7, Y),
		GX is X * 32,
		GY is (7-Y) * 32,
		square_colour(X, Y, Colour),
		square_image(empty, _, Colour, Image),
		send(Board, display,
		     new(Bitmap, bitmap(Image)),
		     point(GX, GY)),
		send(Bitmap, attribute, attribute(square_colour, Colour)),
		send(Bitmap, get_method, @square_piece),
		send(Bitmap, get_method, @square_colour),
		xy_where(X, Y, Where),
		send(Bitmap, name, Where),
	    fail ; true
	).


%	square_colour(+X, +Y, -Colour)

square_colour(X, Y, Colour) :-
	(X+Y) mod 2 =:= 0, !,
	Colour = black.
square_colour(_, _, white).


%	xy_where(?X, ?Y, ?Where).

xy_where(X, Y, Where) :-
	var(Where), !,
	CX is X + 0'a,
	CY is Y + 0'1,
	name(Where, [CX, CY]).
xy_where(X, Y, Where) :-
	name(Where, [CX, CY]),
	X is CX - 0'a,
	Y is CY - 0'1.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
\index{function objects}\index{garbage collection}
The creation  of the  empty board may  now be   tested using the query
below.  Note that class \class{*} denotes an (integer) multiplication.
It  is a  subclass  of  the  PCE  class  \class{function}.   The  term
`size(8*32,~8*32))'  is  evaluated by send/[2-12].  First  both `8*32'
are translated  into two instances of  class  \class{*}.  Next  a size
instance is  created.  The initialisation arguments  of a size  do not
accept functions.  Therefore both functions are evaluated and the size
object is  initialised  from the    result.  Finally  the  incremental
garbage collector   of PCE   destroys      both   *   objects.     See
appendix~\ref{sec:memory}.

	1 ?- make_chess_board(@cb),
	     new(W, window('Chess Board', size(8*32, 8*32))),
	     send(W, display, @cb),
	     send(W, open).

We may now issue various requests to the board:

	2 ?- get(@cb, member, e2, Bitmap),
	     get(Bitmap, square_colour, SquareColour),
	     get(Bitmap, piece, Piece).
	SquareColour = black, Bitmap = @802111/bitmap, Piece = empty 

The predicates  put_piece/4  and move/3  below complete  the primitive
layer.  put_piece/4 allows  us  to put  pieces of  any  colour  at any
square.  First it exploits the <-member method of class \class{device}
to  find   the bitmap for   the  specified  location.   Then  it calls
square_image/4 to  compute the (new)  image and associates   the image
with the bitmap.  Try:

	3 ?- put_piece(@cb, bishop, white, b1).

The predicate move/3 moves a piece.  First it determines the piece and
colour of the `from' location.   Then it  puts this piece on  the `to'
location and clears the `from' location to empty.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	put_piece(+Board, +Piece, +Colour, +Where)

put_piece(Board, Piece, Colour, Where) :-
	get(Board, member, Where, Bitmap),
	get(Bitmap, square_colour, SquareColour),
	square_image(Piece, Colour, SquareColour, Image),
	send(Bitmap, image, Image).


%	move(+Board, +From, +To).

move(Board, From, To) :-
	get(Board, member, From, FromBitmap),
	get(FromBitmap, piece, Piece),
	get(FromBitmap, colour, Colour),
	put_piece(Board, Piece, Colour, To),
	put_piece(Board, empty, _, From).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The predicate below resets the board to the initial position.  Test it
using:

	3 ?- initial_position(@cb).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initial_position(Board) :-
	between(0, 7, X),
	between(0, 7, Y),
	    initial_piece(X, Y, Piece, Colour),
	    xy_where(X, Y, Where),
	    put_piece(Board, Piece, Colour, Where),
	fail.
initial_position(_).

initial_piece(0, 0, rook,   white) :- !.
initial_piece(1, 0, knight, white) :- !.
initial_piece(2, 0, bishop, white) :- !.
initial_piece(3, 0, queen,  white) :- !.
initial_piece(4, 0, king,   white) :- !.
initial_piece(5, 0, bishop, white) :- !.
initial_piece(6, 0, knight, white) :- !.
initial_piece(7, 0, rook,   white) :- !.
initial_piece(_, 1, pawn,   white) :- !.
initial_piece(X, Y, Piece,  black) :-
	WY is 7 - Y,
	initial_piece(X, WY, Piece, white), !.
initial_piece(_, _, empty,  black).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This definition completes the basic board  routines.  We can paint the
initial position and move  pieces around by calling Prolog predicates.
In the next section we will discuss how we can detect and interpret an
attempt to move a piece by the user.

\subsection{Moving Pieces with the mouse}	\label{sec:movepiece}


\index{event,processing}\index{moving graphicals}\index{graphical,moving}
In this section we will make it  possible to move  a piece.  Graphical
objects are  made  sensitive  to  mouse  operations by adding  a  {\em
recogniser} object to them.  PCE predefines various recogniser objects
handling both  simple  events  such as a  depressed  key  and  complex
operations  such as connecting two graphical objects. 

In our case   the standard  move-gestures won't work    as they   move
arbitrary graphicals to arbitrary positions.   We want to move  pieces
rather than squares.  Also, we only want to actually move the piece if
it is a  valid move.  Below, we  will  define  a  gesture object  that
checks whether a square with a piece is moved and calls  the predicate
user_move(Board,~Piece,~From,~To) instead of  moving the piece itself.
The      starting     point         will  be     the        predefined
\class{move_outline_gesture}.

Class  \class{gesture} traps user-events  (mouse-button actions, mouse
movement,  modifier keys and normal keys).   It  is designed to detect
and handle the most typical mouse gesture: button-down,  optional move
events followed by a button-up.  A gesture  sends various  messages to
itself, allowing refined versions of it  to modify the start, dragging
and termination of the gesture.

In  this particular case we will  redefine  ->verify  and ->terminate.
There are two ways to redefine these methods.  The  first is to create
a subclass and  define a method  on this  subclass.   This approach is
taken  in   PceDraw  \cite{PCE:draw}.  The  second   is  to attach  an
object-level method object.  Attaching get_method objects is described
at  the   start of this chapter.     Attaching send_methods is similar
(except a send_method has no return-type argument). We will make these
methods call a Prolog predicate.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@move_piece_gesture, make_move_piece_gesture).

make_move_piece_gesture(G) :-
	new(G, move_outline_gesture),
	send(G, send_method,
	     send_method(verify, vector(event),
			 message(@prolog, verify, @receiver, @arg1))),
	send(G, send_method,
	     send_method(terminate, vector(event),
			 message(@prolog, terminate, @receiver, @arg1))).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The method  ->verify is invoked to  verify all conditions to start the
gesture are met.  If it fails, the gesture will refuse the event.  The
move_outline_gesture moves  any  graphical  object.  In our  case only
bitmaps  with pieces may  be moved.  We  will first  test  whether the
receiver of the event (the  bitmap) is  not an empty square.   If  this
test is passed we  will invoke  the  original message  defined at  the
class-level to complete the test.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

verify(Gesture, Event) :-
	get(Event, receiver, SquareBitmap),
	\+ get(SquareBitmap, piece, empty),
	send(Gesture, send_class, verify, Event).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The   ->terminate method is   invoked   on  the button-up.  The normal
behaviour  is to  move the object  to   the  current  location of  the
outline.  Our modified  definition will determine  the location of the
outline in the chess coordinate system.

First it  determines the original  bitmap, the  board, the outline and
the current area object associated with  the outline.  It  removes the
outline from the board.

Next it  will find  all  bitmaps  on  the  board  overlapping with the
outlines' area.  In the next step it will sort the overlapping bitmaps
according to the  size of the  common sub area  with the  outline.  The
method `chain  ->sort' accepts  a function as argument.  This function
will be called to  compare  pairs  of elements  of the chain.  In this
case the function is <-compare on the <-measure%
  \footnote{The method `area <-measure' returns $abs(w) \times abs(h)$
	    of the area object.  Actually class `area' should have been
	    named `region' or `rectangle' and <-measure should have
	    been named `area'.}
of both <-intersection  areas.  Sorting is  done `smallest  first' and
the  <-tail   of the chain thus   holds  the   bitmap with the largest
intersection.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

terminate(Gesture, Event) :-
	get(Event, receiver, FromBitmap),
	get(FromBitmap, device, Board),		  % the board
	get(Gesture, outline, Box),
	get(Box, area, Area),			  % current area
	send(Box, device, @nil),		  % Undisplay the outline
	get(Board?graphicals, find_all,
	    message(@arg1?area, overlap, Area),
	    OverlappingSquares),
	send(OverlappingSquares, sort,
	     ?((?(@arg1?area, intersection, Area)) ? measure, compare,
	       (?(@arg2?area, intersection, Area)) ? measure)),
	get(OverlappingSquares, tail, Best),
	get(Best, name, ToLocation),
	get(FromBitmap, name, FromLocation),
	get(FromBitmap, piece, Piece),
	user_move(Board, Piece, FromLocation, ToLocation).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
With  this  definition   the  generic    piece-move-gesture   has been
completed.   If the user  moves a piece  the  predicate user_move/4 is
called.

\index{for_all}
The predicate  attach_recognisers/1 attaches a   recogniser to each of
the squares on  the board.  The method  `chain ->for_all' takes a code
object as argument and invokes this code object on all members  of the
chain.  Most of PCE's classes that manage a set of objects provide the
methods ->for_all, ->for_some,  <-find and <-find_all.   Without these
methods  the  individual members  had  to be transferred to   the host
language and the iteration had to  take place there.  Using ->for_all,
etc.\ is much easier to read and write and much faster.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

attach_recognisers(Board) :-
	send(Board?graphicals, for_all,
	     message(@arg1, recogniser, @move_piece_gesture)).

	
		/********************************
		*    CHESS PROCESS INTERFACE	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
In  this section  we  will connect our  interface  to the Unix program
/usr/games/chess.   To interface with  an  external Unix process,  PCE
defines the class  \class{process}.   A process  object  allows us  to
start and control a \idx{Unix process}.  The method `process ->format'
sends  (formatted) text to the  process' standard  input.  Output from
the process is cut into records (default a  line).  For  each complete
record  the  `process <->input_message'  attribute of   the process in
invoked.

The   predicate   attach_chess_program/1     creates  a  process   for
/usr/games/chess and attaches it  as an attribute  to the chess-board.
The message `process ->open' starts the Unix process.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

attach_chess_program(Board) :-
	send(Board, attribute,
	     attribute(process, new(P, process('/usr/games/chess')))),
	send(P, use_tty, @off),		  % use pipes to communicate
	send(P, input_message,
	     message(@prolog, chess_utterance, Board, @arg1)),
	send(P, open).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The predicate user_move/4 is called from the  move-gesture attached to
the squares.  It  translates the notation  used in the interface to the
notation expected by the chess-program and invokes `process ->format'.
This call sends the  data to  the process and returns  without waiting
for a response.  The chess program will echo the move if  it was legal
and `Illegal Move' otherwise.

As the process may have died or could not be started in the first place,
pce_catch_error/2 is used  to  send  the   message.  If  the  error {\tt
not_open} is raised by `process ->format', the call will fail silently.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

user_move(Board, Piece, FromLocation, ToLocation) :-
	chess_move_name(Piece, FromLocation, ToLocation, Move),
	get(Board, process, ChessProcess),
	(   pce_catch_error(not_open,
			    send(ChessProcess, format, '%s\n', Move))
	->  true
	;   send(Board, report, error, 'No chess process')
	).
	     

chess_move_name(Piece, F, T, Move) :-
	image_name(Piece, Id, _),
	chess_coordinate(F, CF),
	chess_coordinate(T, CT),
	concat_atom([Id, /, CF, -, CT], Move).


chess_coordinate(Where, C) :-
	xy_where(X, Y, Where),
	CY is Y + 1,
	chess_x(X, CX),
	concat(CX, CY, C).

chess_x(0, qr).
chess_x(1, qn).
chess_x(2, qb).
chess_x(3, q).
chess_x(4, k).
chess_x(5, kb).
chess_x(6, kn).
chess_x(7, kr).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The predicate chess_utterance/2 is called from the  <-input_message of
the process object.   The process  object will  invoke <-input_message
each time it has received an input record, which is by default a line.

The output of the chess process may be parsed in  several manners.  In
this case we decided to use regular expressions for this purpose.  The
predicate utterance/2 defines  regular  expressions and  goals  to  be
called if the expression matches on the input line.  The  arguments of
the  goal are  filled    from the  register   values (indicated   with
\verb$\($...\verb$\)$).   The  predicate make_regex/2  is  similar  to
square_image/4 in maintaining a database of reusable objects.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

chess_utterance(Board, Utterance) :-
	utterance(Pattern, Goal),
	make_regex(Pattern, RegEx),
	send(RegEx, search, Utterance), !,
	free_variables(Goal, List),
	bind_args(List, 1, RegEx, Utterance),
	call(Goal, Board).

:- dynamic
	regex/2.

make_regex(Pattern, RegEx) :-
	regex(Pattern, RegEx), !.
make_regex(Pattern, RegEx) :-
	new(RegEx, regex(Pattern)),
	send(RegEx, lock_object, @on),
	assert(regex(Pattern, RegEx)).

bind_args([], _, _, _).
bind_args([H|T], N, R, U) :-
	get(R, register_value, U, N, Str),
	get(Str, value, H),
	NN is N + 1,
	bind_args(T, NN, R, U).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
\index{regex}\index{regular expressions}\index{GNU-Emacs}
The chess program utterances.  PCE uses GNU-Emacs regular expressions.
The main difference  to standard Unix  (egrep)  regular expressions is
\verb$\w$, which represent  any word-constituent character.   The last
clause catches anything unrecognised.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

utterance('[0-9]+\. \w/\(\w+\)\([1-8]\)-\(\w+\)\([1-8]\)',
	  player_move(_FX, _FY, _TX, _TY)).
utterance('[0-9]+\. \w/\(\w+\)\([1-8]\)x\w/\(\w+\)\([1-8]\)',
	  player_move(_FX, _FY, _TX, _TY)).
utterance('[0-9]+\. \(o-o\(-o\)?\)',
	  player_oo(_OO)).
utterance('[0-9]+\. \.\.\. \w/\(\w+\)\([1-8]\)x\w/\(\w+\)\([1-8]\)',
	  opponent_move(_FX, _FY, _TX, _TY)).
utterance('[0-9]+\. \.\.\. \(o-o\(-o\)?\)',
	  opponent_oo(_OO)).
utterance('[0-9]+\. \.\.\. \w/\(\w+\)\([1-8]\)-\(\w+\)\([1-8]\)',
	  opponent_move(_FX, _FY, _TX, _TY)).
utterance('Chess',	banner).
utterance('\(.*\)',	warn(_Message)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Below are the various  predicates called by  chess_utterance/2 through
the utterance/2 matcher and argument binder.  They basically translate
chess' coordinate system into ours  and  call move/3 to make the moves
on the board.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

warn(Utterance, _Board) :-
	send(@display, inform, Utterance).

player_move(CFX, CFY, CTX, CTY, Board) :-
	chess_x(FX, CFX),
	chess_x(TX, CTX),
	get(CFY - 1, value, FY),
	get(CTY - 1, value, TY),
	xy_where(FX, FY, F),
	xy_where(TX, TY, T),
	move(Board, F, T).

player_oo('o-o', Board) :-
	move(Board, e1, g1),
	move(Board, h1, f1).
player_oo('o-o-o', Board) :-
	move(Board, e1, c1),
	move(Board, a1, d1).

opponent_move(CFX, CFY, CTX, CTY, Board) :-
	chess_x(FX, CFX),
	chess_x(TX, CTX),
	get(8 - CFY, value, FY),
	get(8 - CTY, value, TY),
	xy_where(FX, FY, F),
	xy_where(TX, TY, T),
	move(Board, F, T).

opponent_oo('o-o', Board) :-
	move(Board, e8, g8),
	move(Board, h8, f8).
opponent_oo('o-o-o', Board) :-
	move(Board, e8, c8),
	move(Board, a8, d8).

banner(_).


		/********************************
		*            TOPLEVEL		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Below is  the toplevel that combines   all  the declarations  above to
create   the final  chess-tool.    Most graphical applications use  an
instance  of \class{picture}  for the drawing window.   The difference
between a window and a picture is that the  latter has scrollbars.  We
do not want to scroll the chess-board.

\index{window,destruction}
The message <->done_message is called  on   a  request from the X-window
manager to delete the window (normally from the menu associated with the
title-bar of the window).  The default  message destroys the window.  In
our case we also have to kill the   chess  program.  The message to kill
the process is placed in an if with epty then- and else-branch to ensure
success of the message.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

chess :-
	new(Window, window('ChessBoard 0.1', size(8*32, 8*32))),
	make_chess_board(Board),
	send(Window, display, Board),
	attach_recognisers(Board),
	initial_position(Board),
	send(Window, open),
	attach_chess_program(Board),
	send(Window, done_message,
	     and(if(message(Board?process, kill)),
		 message(Window, destroy))).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
\section{Summary}

This chapter contains an annotated version of a complete and realistic
PCE/Prolog  program.  In  introduces  various aspects  of  programming
PCE/Prolog:

\begin{itemize}
    \tick{Using Prolog Modules}
PCE is transparent to the Prolog module system.  When using SICStus
Prolog modules we need to import the module `library(pce)' and declare
the PCE library predicates (see appendix~\ref{sec:interface}) using
the require/1 directive.
    \tick{Compound Graphicals}
The PCE class \class{device} is a graphical that can display other
graphicals.  Graphical devices have their own coordinate system.
    \tick{Bitmaps and Images}
A bitmap allows an image (two-dimensional array of pixels) to be
used as a graphical object.
    \tick{Object-level attributes}
Information about an object may be stored with the object itself using
object level attributes (see `object ->attribute').  See also
chapter~\ref{sec:pceprolog}.
    \tick{Object-level methods}
Methods may be attached both to objects and classes.  The
implementation of a method is a PCE code object (function for
get_methods).  The var object @receiver evaluates to the receiver
of a message.  @arg1 ... refer to the arguments by position.
    \tick{Functions: obtainers (`?'), var objects and arithmetic}
Functions are objects that may be evaluated.  `?' (obtainer)
evaluates by executing a get-operation.  Functions are evaluated
if they appear in a code object and the code object is executed
or if they are passed to an argument whose type-specification
does not accept a function (except for code objects only very
few methods accept functions without evaluating them).
    \tick{Declaring reusable objects using :- pce_global/2}
Global objects that are reused are normally declared using
pce_global/2.  See section~\ref{sec:global} for details.
    \tick{Recognisers: handling mouse events}
Graphical objects are made sensitive to the mouse and keyboard using
{\em recogniser} objects.  PCE defines both primitive recognisers and
recognisers to handle event-sequences\index{event,sequences}.
A \class{gesture} handles the sequence mouse-button-down, dragging,
mouse-button-up.  Various standard gestures are defined.  New gestures
are created using subclasses or by attaching object-level methods
to an instance of one of the standard classes.
    \tick{Interface to Unix Processes}
Class \class{process} offers an interface to Unix processes.  The
managed process runs asynchronously.  Data may be send to the
process' standard input.  Output data from the process is received
interleaved with events.  It is collected in records and handled
by the `processes <->input_message'.
\end{itemize}
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
