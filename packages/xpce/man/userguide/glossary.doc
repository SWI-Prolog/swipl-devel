\chapter{Glossary}			\label{sec:glossary}

\newcommand{\glossitem}[1]{\tick{#1}}
\newcommand{\g}[1]{{\em #1}}

\begin{description}
\glossitem{Attribute}
    A \class{attribute} object is used to define additional properties
    of an object.  The term \g{attribute} is also used as a synonym
    for \g{slot} and \g{instance-variable} referring to class defined
    properties.
\glossitem{Class}
    A \g{class} is an object that acts as a description of other
    objects called \g{instances} of the \g{class}.  Besides various
    house-keeping information, a PCE \g{class} describes the \g
    {instance-variables} and \g{methods}  of its \g{instances}.
\glossitem{Class-Variable}
    A \g{class-variable} defines a constant for all instances of the
    class.  Class variables can be used to define default values for
    an \g{instance-variable}.  Initial values for class-variables can
    be specified in the \file{Defaults} file.  See \secref{classvar}.
\glossitem{Code}
    A \g{code} object is an object that represents a procedure.  \g{Code}
    objects are used for implementation of methods and to associate
    actions with various events.  For example a button object executes
    its associated code object when depressed.  The most typical code
    object is a \class{message}.
\glossitem{Control}
    A \g{control} is a standard \g{GUI} object normally placed in dialog
    windows.  Examples are buttons, text-entry fields and menus.
\glossitem{Event}
    An \g{event} is an object that represents an activity of the user:
    mouse-movements, mouse-buttons, keyboard activities.
\glossitem{Forwarding of argument}
    When code objects are executed it is common to bind the \g{var}
    objects @arg1, @arg2, ... to pass context information for the
    executing code.  For example, when a method object executes its
    code it will bind the arguments given to the method to @arg1, ...
\glossitem{Function}
    A \g{function} is a subclass of class \class{code} which yields
    a value when executed.  The most important functions are local
    variables (\g{var}), \g{obtainers} and mathetical operations.
    They may be used as arguments to code objects.  They are executed
    when the code object is executed or when the function needs to be
    converted to a type that does not accept a function.
\glossitem{Get operation}
    Virtual machine operation to request information from
    some object.  Started by the Prolog predicate get/[3-13], when
    an obtainer is executed or from PCE's built-in functionality.
\glossitem{GUI}
    Abbreviation for Graphical User Interface.
\glossitem{Inheritance}
    The sharing of definition from a super-class.  When a PCE \g{class}
    is created from a \g{super-class} it is initially a copy of this
    \g{super-class}.  After creation, instance variables and methods may
    be added and/or redefined.
\glossitem{Instance}
    Synonym for \g{object}, often use to stress the fact that an object
    belongs to a particular class.
\glossitem{Instance-variable}
    Placeholder for the local-state associated with an \g{object}.  An
    \g{instance-variable} is associated with a class and has a name and a
    type.  Each of the \g{instances} of the class defines a value for
    the instance variable.  Instance variables are represented by class
    \class{variable}.
\glossitem{Message}
    A \g{message} is an object representing a \g{send-operation}.  The
    phrase ``sending a message to X'' is equivalent to ``invoking a
    get- or send-operation on X''.
\glossitem{Method}
    A \g{method} maps a \g{selector} and a type vector onto an
    implementation which is either a C-function or a \g{code}
    object.  PCE defines both get- and send-methods.  If a \g
    {send-operation} is invoked on an object, PCE will find a method
    associated with the class of the object with a matching \g
    {selector}, check the argument types and invoke the implementation
    of the method.
\glossitem{Object-reference}
    An {\em object-reference} is the identifier for a particular
    instance.  In Prolog \g{object-references} are represented by
    @Integer or @Atom.
\glossitem{Object}
    An \g{object} is en entity in PCE's world that is identified by
    an \g{object-reference} and has a local state.  An object is an
    \g{instance} of a \g{class}.  The \g{class} defines both the
    constituents of the local state as well as the operations (\g{methods})
    understood by the object.
\glossitem{Obtainer}
    An \g{obtainer} is a \g{function} which invokes a \g{get-operation}
    when evaluated.  The class name is `?'.
\glossitem{Recogniser}
    A \g{recogniser} object parses \g{events} for a graphical object.
\glossitem{Selector}
    A \g{selector} is the name of a \g{send-operation} or \g{get-operation}.
\glossitem{Send Method}
    Refinement of \class{method} that maps a \g{send-operation} onto
    its implementation.  See also \g{Method}
\glossitem{Send operation}
    Virtual machine operation which invokes of a \g{send-method} on
    some object.  Started by the Prolog predicate send/[2-12], when
    an \g{message} is executed or from PCE's built-in functionality.
\glossitem{Slot}
    Equivalent to \g{instance_variable}.
\glossitem{Super-class}
    The \g{super-class} of a \g{class} serves as the initial definition
    of a \g{class}.  See also \g{inheritance}.
\glossitem{Template-class}
    User-defined subclass of class \class{template}.  The refinements
    introduced from \class{template} can be imported in another user-defined
    class using the predicate use_class_template/1.
\glossitem{Var}
    A \g{var} object is a \g{function}.  The commonly used \g{vars}
    objects are: @arg1, ... (general argument forwarding), @receiver
    (receiver or a message), @event (currently processes event object).
\end{description}
