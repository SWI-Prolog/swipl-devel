\section{Multi-lingual applications}		\label{sec:mlingual}

\product{} provides some support for building multi-lingual applications
without explicitly mapping terms all the time.  This section provides
an overview of how multi-lingual operation was realised in a simulator
for optics.

When writing a multi-lingual application, several different types of
information needs to be translated.  We will discuss each of them
below.

\begin{itemlist}
    \item [Labels]
Labels as they are used by the subclasses of \class{dialog_item},
menu items, etc.  These can be mapped by redefining a number of
methods that realise the default mapping between internal names
and externally visible names:
    \begin{description}
	\getmethod{dialog_item}{label_name}{Id}{Label}
    This method performs the mapping that ensures that the code
    \exam{text_item(user_name, '')} renders as:

    \begin{center}
    % OOPS ... fsfig doesn't appear to center properly if the
    % figure is wider then the environment.  Hence to makeboxes.
    \makebox[0pt][c]{\makebox[\textwidth][c]{%
    \includegraphics{username}}}
    \end{center}

    This method may be redefined to return another name or image
    object, depending on the current language mapping.
	
	\getmethod{menu_item}{label_name}{Id}{Label}
    Similar to `dialog_item <-label_name'.

	\getmethod{dialog_group}{label_name}{Id}{Label}
    Similar to `dialog_item <-label_name', but is, in the current
    implementation, not allowed to return an \class{image}.  This
    method needs to be redefined separately as \class{dialog_group}
    (a super-class of \class{tab}) is not in the same branch of the
    inheritance hierarchy as \class{dialog_item}.

    \end{description}

    In the current implementation, window and frame labels are not
    covered by this schema.

    \item [Error messages]
    Although it is convenient to present error messages directly using
    the report mechanism described in \secref{report}, this approach is
    not very well suited for multi-lingual applications. A better
    approach is to use \class{error} objects, as described in
    \secref{errors}.

    Using error objects is slightly more cumbersome as errors need to
    be declared separately, but they improve the embedding possibilities
    using error handling, and the mapping from an error identifier to a
    format string provides the indirection necessary in multi-lingual
    applications.

    \item [Other messages and help text]
    There is no special support for other textual information,
    help-texts, etc.
\end{itemlist}

Below is a summary of the file \file{language.pl} as using in the
optics simulator to reach at an English/Dutch application.

\hr
\begin{pcecode}
:- module(language,
	  [ message/2,			% Id, Message
	    current_language/1,		% -Language
	    set_language/1		% +Language
	  ]).
:- use_module(pce).
:- use_module(configdb).
:- require([ concat_atom/2
	   , is_list/1
	   , memberchk/2
	   ]).

:- dynamic
	current_language/1.

current_language(english).		% the default
%current_language(dutch).

set_language(Lan) :-
	retractall(current_language(_)),
	assert(current_language(Lan)),
	make_errors.

%	message(+Term, -Translation)
%	The heart of the translator. Map a term
%	(normally an atom, but if can be an arbitrary
%	Prolog term, into an image or atom. If no
%	translation is found, the default case and
%	underscore translation is performed.

message(Term, Translation) :-
	current_language(Lan),
	term(Term, Translations),
	(   is_list(Translations)
	->  T =.. [Lan, Translation0],
	    memberchk(T, Translations),
	    (	is_list(Translation0)
	    ->	concat_atom(Translation0, Translation)
	    ;	Translation = Translation0
	    )
	;   Translation = Translations
	), !.
message(Term, Translation) :-
	get(Term, label_name, Translation).


		 /*******************************
		 *    MAP DIALOG IDENTIFIERS	*
		 *******************************/

:- pce_extend_class(dialog_item).

label_name(DI, Id:name, Label:'name|image') :<-
	"Multi-lingual label service"::
	message(Id, Label0),
	(   atomic(Label0)
	->  get(DI, label_suffix, Suffix),
	    get(Label0, ensure_suffix, Suffix, Label)
	;   Label = Label0
	).

:- pce_end_class.

:- pce_extend_class(dialog_group).

label_name(_DI, Id:name, Label:name) :<-
	"Multi-lingual label service"::
	(   message(Id, Label),
	    atomic(Label)
	->  true
	;   get(Id, label_name, Label)
	).

:- pce_end_class.

:- pce_extend_class(menu_item).

default_label(_MI, Id:name, Label:'name|image') :<-
	"Multilingual label service"::
	message(Id, Label).

:- pce_end_class.

		 /*******************************
		 *        GENERIC LABELS	*
		 *******************************/

%	term(+Term, -Translated)
%
%	Term translates a term. There are three examples
%	here. The first only contains the translation
%	for an English label name into a Dutch one. The
%	second replaces all labels named `label' into an
%	image. The last is for generating a more
%	elaborate message from an identifier.

term(settings,
     [ dutch('Instellingen')
     ]).
term(label,
     image('label.lbl')).
term(start_named_test(Name),
     [ english(['Click "OK" to start test "', Name, '"']),
       dutch(['Klik op "OK" om aan de toets "', Name,
       	      '" te beginnen'])
     ]).
     

		 /*******************************
		 *	       ERRORS		*
		 *******************************/

%	error(Id, Kind, Translations)
%
%	Specify and create the required error messages.
%	An object that detects there are too many
%	instruments directs this information to the user
%	by
%
%		...
%		send(MySelf, error, max_instruments, 5),
%		...

error(max_instruments, error,
      [ dutch('%IU kunt niet meer dan %d van deze \
               instrumenten gebruiken'),
	english('%IYou can not use more than %d of \
		 these instruments')
      ]).

make_errors :-
	current_language(Lan),
	T =.. [Lan, Message],
	error(Id, Kind, Messages),
	(   memberchk(T, Messages)
	->  true
	;   Message = Id
	),
	new(_E, error(Id, Message, Kind, report)),
	fail.
make_errors.

:- initialization make_errors.
\end{pcecode}
\hr
