                     A brief note on customising XPCE
                     ================================

1. Customising `look-and-feel'
==============================

1.1.  Where do I specify look-and-feel?
=======================================

Look-and-feel is specified using a resource specification similar to the
X11 resources. The main resource file   loaded by XPCE is <pcehome>/Pce,
which includes the file ~/.xpce/Xpce if it exists. 


1.2.  How do I find attributes that can be specified?
=====================================================

You  can  find  the  defined  resources    for   some  class  using  the
`ClassBrowser' as available from the XPCE Manual under `Browsers':

	1) Select the desired class using the `Class' item
	2) Set the `Filter' to `all'
	3) Set the `Display' to `Resource' only
	4) `Apply'
	5) Double-click on `object' in the top-right window to extend
	   the search to all classes this class inherits from.
	6) A list of applicable resources will be presented.
	   Double-click (left) on the resource if you want a further
	   description.

1.3.  What is the syntax to use for specifying a resource?
==========================================================

The X11 resource class of XPCE resources  is `Pce'.  Thus, if the manual
describes the resource `Graphical.colour', you   can specify all circles
to be green using:

	Pce.Circle.colour:	colour(green)

1.4.  What is the syntax for resource values?
=============================================

The syntax for resource values is close to the Prolog syntax with a few
exceptions:

	* Creating an instance that requires no initialisation arguments
	is done using <classname>(): `chain()' creates an empty chain.
	`new(chain)' as used from Prolog will try to make an instance
	of class `new' (and thus fail).

	* Atoms starting with a Capital do not need to be quoted (but
	may be) as this syntax defines no variables.

	* '[' {<term>} ']' maps onto an XPCE chain and thus is
	equivalent to 'chain(' {<term>} ')'.

Parsing the string-value from the  X11   resource  database into an XPCE
value is done by the object   @resource_parser.  Use the `Global Object'
browser to see the documentation of this parser.


1.5.  Conditional resources?
============================

Sometimes you would like to specify   resource values conditionally, for
example depending on whether or not  you   have  a  monochrome or colour
display, on the size of your display, etc.

The trick to use  is  to  specify   the  value  as  function object (see
ClassBrowser for full documentation).  A function will be converted into
an object of the requested type by (first) evaluation the function.

Thus, the specification:

	Pce.Window.size:	size(@display?height / 3, \
				     @display?width / 3)

will define default windows to be 1/3 of the display's <-width and
<-height, while:

	Pce.Window.background:	when(@colour_display, grey95, white)

will make window's background a little   grey  (so that coloured objects
become much better visible) on colour displays and just white otherwise.
The object @colour_display is a predefined conditional object.


1.6.  Constants?
================

Sometimes you would like to give various objects the same colour palette
or define objects you can reuse in  the further specification.  For this
reason, the resource:

	Pce.display.initialise

describes an executable  (code)  object  that   will  be  converted  and
executed by `display  ->open',  which  is   before  any  other  resource
conversion will take place.  Below is   the defenition from the supplied
`Pce' file:

    Pce.Display.initialise: \
	and(_dialog_bg @= when(@colour_display, grey80, white), \
	    _graph_bg  @= when(@colour_display, grey95, white), \
	    _win_pen   @= when(@colour_display, 0, 1))

The symbol `@=' is an infix operator,   which  implies <ref> @= <object>
evaluates to an instance of class  @=   which,  when  executed will give
<object> the reference <ref>.  Thus,  after   this  object  is executed,
@_dialog_bg and @_graph_bg and @_win_pen are  global objects that can be
used in resource specifications.


2. Prolog customisation
=======================

XPCE/Prolog reads the file ~/xpcerc  when   present  *after* reading the
normal Prolog initialisation file.

ProWindows/Quintus reads the file ~/.pwrc instead of ~/.xpcerc.
