/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/


#include <h/kernel.h>
#include <h/dialog.h>
#include <h/text.h>
#include <h/lang.h>
#include <h/arith.h>
#include <h/unix.h>
#include <rel/proto.h>

#ifdef __WIN32__
extern Class ClassWinMF;
extern Class ClassWinPrinter;
#endif

extern Class ClassIntItem;

		 /*******************************
		 *	     CLASSES		*
		 *******************************/

static struct class_definition classdefs[] =
{ { NAME_and, NAME_code, makeClassAnd,
    &ClassAnd, "Logical connective: and" },

  { NAME_arc, NAME_joint, makeClassArc,
    &ClassArc, "Ellipse part (pie-slice)" },

  { NAME_area, NAME_object, makeClassArea,
    &ClassArea, "Rectangular set of pixels" },

  { NAME_application, NAME_visual, makeClassApplication,
    &ClassApplication, "Collection of frames" },

  { NAME_arrow, NAME_graphical, makeClassArrow,
    &ClassArrow, "Arrow head (normally for joints)" },

  { NAME_assign, NAME_code, makeClassAssign,
    &ClassAssign, "Assignment statement for a var" },

  { NAME_attribute, NAME_programObject, makeClassAttribute,
    &ClassAttribute, "Name-value pair for sheet or object" },

  { NAME_behaviour, NAME_programObject, makeClassBehaviour,
    &ClassBehaviour, "Methods, variables and attributes" },

  { NAME_binaryCondition, NAME_code, makeClassBinaryCondition,
    &ClassBinaryCondition, "Arithmetic binary conditional expression" },

  { NAME_binaryExpression, NAME_function, makeClassBinaryExpression,
    &ClassBinaryExpression, "Binary arithmetic expression" },

  { NAME_bitmap, NAME_graphical, makeClassBitmap,
    &ClassBitmap, "2-dimensional map of (coloured) pixels" },

  { NAME_block, NAME_and, makeClassBlock,
    &ClassBlock, "Sequence with local variables" },

  { NAME_bool, NAME_constant, makeClassBool,
    &ClassBool, "Boolean constant" },

  { NAME_box, NAME_graphical, makeClassBox,
    &ClassBox, "Rectangle with rounded corners and shadow" },

  { NAME_browser, NAME_window, makeClassBrowser,
    &ClassBrowser, "Window version of list_browser" },

  { NAME_button, NAME_dialogItem, makeClassButton,
    &ClassButton, "Push button to invoke a command" },

  { NAME_c, NAME_host, makeClassC,
    &ClassC, "C language (for communication)" },

  { NAME_cPointer, NAME_object, makeClassCPointer,
    &ClassCPointer, "C void * encapsulation" },

  { NAME_chain, NAME_object, makeClassChain,
    &ClassChain, "Single linked list of objects" },

  { NAME_chainTable, NAME_hashTable, makeClassChainTable,
    &ClassChainTable, "Association with multiple values" },

  { NAME_charArray, NAME_object, makeClassCharArray,
    &ClassCharArray, "Array of characters" },

  { NAME_circle, NAME_graphical, makeClassCircle,
    &ClassCircle, "Circle" },

  { NAME_classStub, NAME_programObject, makeClassClassStub,
    &ClassClassStub, "Autoload stub for a class" },

  { NAME_class, NAME_classStub, makeClassClass,
    &ClassClass, "Represent classes" },

  { NAME_clickGesture, NAME_gesture, makeClassClickGesture,
    &ClassClickGesture, "Gesture to handle a click" },

  { NAME_code, NAME_programObject, makeClassCode,
    &ClassCode, "Activation of code objects" },

  { NAME_codeVector, NAME_vector, makeClassCodeVector,
    &ClassCodeVector, "Argument vector" },

  { NAME_colour, NAME_object, makeClassColour,
    &ClassColour, "Colour (RGB) definition" },

  { NAME_colourMap, NAME_object, makeClassColourMap,
    &ClassColourMap, "Colour map (palette)" },

  { NAME_connectGesture, NAME_gesture, makeClassConnectGesture,
    &ClassConnectGesture, "Gesture to connect two objects by dragging" },

  { NAME_connection, NAME_line, makeClassConnection,
    &ClassConnection, "Link between two graphicals" },

  { NAME_constant, NAME_object, makeClassConstant,
    &ClassConstant, "Constant value" },

  { NAME_constraint, NAME_object, makeClassConstraint,
    &ClassConstraint, "Binary constraint" },

  { NAME_create, NAME_function, makeClassCreate,
    &ClassCreate, "Create instance of a class" },

  { NAME_cursor, NAME_object, makeClassCursor,
    &ClassCursor, "Cursor for the pointer" },

  { NAME_date, NAME_object, makeClassDate,
    &ClassDate, "Represent point in time" },

  { NAME_delegateVariable, NAME_variable, makeClassDelegateVariable,
    &ClassDelegateVariable, "Instance-variable for delegation" },

  { NAME_device, NAME_graphical, makeClassDevice,
    &ClassDevice, "Collection of graphicals" },

  { NAME_dialog, NAME_window, makeClassDialog,
    &ClassDialog, "Window with dialog_items" },

  { NAME_dialogItem, NAME_graphical, makeClassDialogItem,
    &ClassDialogItem, "Item (menu) in a dialog window" },

  { NAME_dialogGroup, NAME_device, makeClassDialogGroup,
    &ClassDialogGroup, "Group of dialog items" },

  { NAME_dict, NAME_visual, makeClassDict,
    &ClassDict, "Dictionary of objects (also for browser)" },

  { NAME_dictItem, NAME_visual, makeClassDictItem,
    &ClassDictItem, "Item (element) of a dictionary" },

  { NAME_directory, NAME_object, makeClassDirectory,
    &ClassDirectory, "Unix directory" },

  { NAME_display, NAME_visual, makeClassDisplay,
    &ClassDisplay, "Global management of display" },

  { NAME_displayManager, NAME_visual, makeClassDisplayManager,
    &ClassDisplayManager, "Manager of available displays" },

  { NAME_editor, NAME_device, makeClassEditor,
    &ClassEditor, "EMACS look-alike text editor" },

  { NAME_elevation, NAME_object, makeClassElevation,
    &ClassElevation, "Description of an elevation" },

  { NAME_ellipse, NAME_graphical, makeClassEllipse,
    &ClassEllipse, "Ellipse" },

  { NAME_error, NAME_object, makeClassError,
    &ClassError, "Description of some (internal) problem" },

  { NAME_event, NAME_object, makeClassEvent,
    &ClassEvent, "PCE's notion of a user event" },

  { NAME_eventNode, NAME_object, makeClassEventNode,
    &ClassEventNode, "Node in event-type hierarchy" },

  { NAME_eventTree, NAME_object, makeClassEventTree,
    &ClassEventTree, "Hierarchy of event types" },

  { NAME_figure, NAME_device, makeClassFigure,
    &ClassFigure, "Collection of graphicals" },

  { NAME_file, NAME_object, makeClassFile,
    &ClassFile, "Unix file" },

  { NAME_font, NAME_object, makeClassFont,
    &ClassFont, "Text font" },

  { NAME_format, NAME_object, makeClassFormat,
    &ClassFormat, "Table specification for device" },

  { NAME_fragment, NAME_visual, makeClassFragment,
    &ClassFragment, "Fragment (range) of a text_buffer" },

  { NAME_frame, NAME_visual, makeClassFrame,
    &ClassFrame, "Collection of tiled windows" },

  { NAME_function, NAME_code, makeClassFunction,
    &ClassFunction, "Activation of function objects" },

  { NAME_gesture, NAME_recogniser, makeClassGesture,
    &ClassGesture, "Handler for sequence down ... up" },

  { NAME_getMethod, NAME_method, makeClassGetMethod,
    &ClassGetMethod, "Query status method" },

  { NAME_graphical, NAME_visual, makeClassGraphical,
    &ClassGraphical, "Displayable graphical object" },

  { NAME_handle, NAME_object, makeClassHandle,
    &ClassHandle, "Connection point on graphical" },

  { NAME_handler, NAME_recogniser, makeClassHandler,
    &ClassHandler, "Map event to message" },

  { NAME_handlerGroup, NAME_recogniser, makeClassHandlerGroup,
    &ClassHandlerGroup, "Collection of recognisers" },

  { NAME_hashTable, NAME_object, makeClassHashTable,
    &ClassHashTable, "One-way association table" },

  { NAME_host, NAME_object, makeClassHost,
    &ClassHost, "Host language (for communication)" },

  { NAME_hyper, NAME_programObject, makeClassHyper,
    &ClassHyper, "Named binary association (hyper link)" },

  { NAME_identity, NAME_relation, makeClassIdentity,
    &ClassIdentity, "Identity between attributes" },

  { NAME_if, NAME_code, makeClassIf,
    &ClassIf, "Conditional branch" },

  { NAME_image, NAME_visual, makeClassImage,
    &ClassImage, "2-dimensional map of (coloured) pixels" },

  { NAME_joint, NAME_graphical, makeClassJoint,
    &ClassJoint, "Various kinds of lines with arrows" },

  { NAME_keyBinding, NAME_recogniser, makeClassKeyBinding,
    &ClassKeyBinding, "Parse keyboard events" },

  { NAME_label, NAME_dialogItem, makeClassLabel,
    &ClassLabel, "Provide feedback in a dialog window" },

  { NAME_line, NAME_joint, makeClassLine,
    &ClassLine, "Line segment" },

  { NAME_link, NAME_object, makeClassLink,
    &ClassLink, "Generic part of a connection" },

  { NAME_listBrowser, NAME_device, makeClassListBrowser,
    &ClassListBrowser, "Select object, visualisation of dict" },

  { NAME_menu, NAME_dialogItem, makeClassMenu,
    &ClassMenu, "Dialog item for lists of options" },

  { NAME_menuBar, NAME_dialogItem, makeClassMenuBar,
    &ClassMenuBar, "List of pulldown menus" },

  { NAME_menuItem, NAME_visual, makeClassMenuItem,
    &ClassMenuItem, "Item in a menu or popup" },

  { NAME_message, NAME_code, makeClassMessage,
    &ClassMessage, "Invoke a send method" },

  { NAME_method, NAME_behaviour, makeClassMethod,
    &ClassMethod, "Mapping from selector to implementation" },

  { NAME_modifier, NAME_object, makeClassModifier,
    &ClassModifier, "Condition on shift, control and meta-keys" },

  { NAME_moveGesture, NAME_gesture, makeClassMoveGesture,
    &ClassMoveGesture, "Gesture to move an object by dragging" },

  { NAME_editTextGesture, NAME_gesture, makeClassEditTextGesture,
    &ClassEditTextGesture, "Edit a text-object" },

  { NAME_moveOutlineGesture, NAME_moveGesture, makeClassMoveOutlineGesture,
    &ClassMoveOutlineGesture, "Gesture to move an object by outline" },

  { NAME_name, NAME_charArray, makeClassName,
    &ClassName, "Atom (unique representation of a string)" },

  { NAME_node, NAME_visual, makeClassNode,
    &ClassNode, "Node in a tree" },

  { NAME_not, NAME_code, makeClassNot,
    &ClassNot, "Logical connective: negation" },

  { NAME_number, NAME_object, makeClassNumber,
    &ClassNumber, "Object version of integer" },

  { NAME_object, (Name) NIL, makeClassObject,
    &ClassObject, "Root of inheritance tree" },

  { NAME_operator, NAME_object, makeClassOperator,
    &ClassOperator, "Operator (see class parser)" },

  { NAME_or, NAME_code, makeClassOr,
    &ClassOr, "Logical connective: or" },

  { NAME_parser, NAME_object, makeClassParser,
    &ClassParser, "Parse input" },

  { NAME_path, NAME_joint, makeClassPath,
    &ClassPath, "(Smooth) line through chain of points" },

  { NAME_pce, NAME_object, makeClassPce,
    &ClassPce, "Environment control" },

  { NAME_pen, NAME_object, makeClassPen,
    &ClassPen, "Drawing pen" },

  { NAME_picture, NAME_window, makeClassPicture,
    &ClassPicture, "Graphics window" },

  { NAME_pixmap, NAME_image, makeClassPixmap,
    &ClassPixmap, "Coloured image" },

  { NAME_point, NAME_object, makeClassPoint,
    &ClassPoint, "Position in a two-dimensional space" },

  { NAME_popup, NAME_menu, makeClassPopup,
    &ClassPopup, "Popup menu" },

  { NAME_popupGesture, NAME_gesture, makeClassPopupGesture,
    &ClassPopupGesture, "Gesture to display a popup menu" },

  { NAME_process, NAME_stream, makeClassProcess,
    &ClassProcess, "Unix process" },

  { NAME_progn, NAME_function, makeClassProgn,
    &ClassProgn, "Sequence (as function)" },

  { NAME_programObject, NAME_object, makeClassProgramObject,
    &ClassProgramObject, "Object of PCE's program world" },

  { NAME_quoteFunction, NAME_object, makeClassQuoteFunction,
    &ClassQuoteFunction, "Quote functions" },

  { NAME_real, NAME_object, makeClassReal,
    &ClassReal, "Floating point number" },

  { NAME_recogniser, NAME_object, makeClassRecogniser,
    &ClassRecogniser, "Mapping from event to message (action)" },

  { NAME_regex, NAME_object, makeClassRegex,
    &ClassRegex, "Regular expression for string matching" },

  { NAME_region, NAME_object, makeClassRegion,
    &ClassRegion, "Region of an area" },

  { NAME_relation, NAME_object, makeClassRelation,
    &ClassRelation, "Superclass for all relations" },

  { NAME_resizeGesture, NAME_gesture, makeClassResizeGesture,
    &ClassResizeGesture, "Gesture to resize by dragging" },

  { NAME_resizeOutlineGesture, NAME_resizeGesture, makeClassResizeOutlineGesture,
    &ClassResizeOutlineGesture, "Gesture to resize an object by outline" },

  { NAME_resource, NAME_object, makeClassResource,
    &ClassResource, "Access to the X11 resource manager" },

  { NAME_scrollBar, NAME_graphical, makeClassScrollBar,
    &ClassScrollBar, "Scroll windows, text or other objects" },

  { NAME_sendMethod, NAME_method, makeClassSendMethod,
    &ClassSendMethod, "Manipulation method" },

  { NAME_sheet, NAME_object, makeClassSheet,
    &ClassSheet, "Collection of attributes" },

  { NAME_size, NAME_object, makeClassSize,
    &ClassSize, "Width and Height of a square area" },

  { NAME_slider, NAME_dialogItem, makeClassSlider,
    &ClassSlider, "DialogItem to select from a numeric range" },

#if defined(HAVE_SOCKET) || defined(HAVE_WINSOCK)
  { NAME_socket, NAME_stream, makeClassSocket,
    &ClassSocket, "Unix socket" },
#endif

  { NAME_sourceLocation, NAME_object, makeClassSourceLocation,
    &ClassSourceLocation, "Position in a sourcefile" },

  { NAME_spatial, NAME_relation, makeClassSpatial,
    &ClassSpatial, "Spatial relation between grahicals" },

  { NAME_stream, NAME_object, makeClassStream,
    &ClassStream, "(Unix) I/O stream" },

  { NAME_string, NAME_charArray, makeClassString,
    &ClassString, "Modifyable version of char_array" },

  { NAME_style, NAME_object, makeClassStyle,
    &ClassStyle, "Definition of character attributes" },

  { NAME_syntaxTable, NAME_object, makeClassSyntaxTable,
    &ClassSyntaxTable, "Table of character types" },

  { NAME_tab, NAME_dialogGroup, makeClassTab,
    &ClassTab, "Tab-sheet for tabbed dialog window" },

  { NAME_labelBox, NAME_dialogGroup, makeClassLabelBox,
    &ClassLabelBox, "Labeled (compound) dialog item" },

  { NAME_relationTable, NAME_object, makeClassAtable,
    &ClassRelationTable, "Simple relational database table" },

  { NAME_tabStack, NAME_device, makeClassTabStack,
    &ClassTabStack, "Stack of tabbed control-windows" },

  { NAME_text, NAME_graphical, makeClassText,
    &ClassText, "Draw text on graphical devices" },

  { NAME_textBuffer, NAME_object, makeClassTextBuffer,
    &ClassTextBuffer, "Editable text with fragments" },

  { NAME_textCursor, NAME_graphical, makeClassTextCursor,
    &ClassTextCursor, "Caret in an editor" },

  { NAME_textImage, NAME_graphical, makeClassTextImage,
    &ClassTextImage, "(Re)display large text fields" },

  { NAME_textItem, NAME_dialogItem, makeClassTextItem,
    &ClassTextItem, "Text entry field" },

  { NAME_intItem, NAME_textItem, makeClassIntItem,
    &ClassIntItem, "Integer entry field" },

  { NAME_textMargin, NAME_graphical, makeClassTextMargin,
    &ClassTextMargin, "Annotation margin for editors" },

  { NAME_tile, NAME_object, makeClassTile,
    &ClassTile, "Manage non-overlapping areas" },

  { NAME_timer, NAME_object, makeClassTimer,
    &ClassTimer, "Generate messages at intervals" },

  { NAME_tokeniser, NAME_object, makeClassTokeniser,
    &ClassTokeniser, "Tokenise input" },

  { NAME_tree, NAME_figure, makeClassTree,
    &ClassTree, "Hierarchy of graphicals" },

  { NAME_tuple, NAME_object, makeClassTuple,
    &ClassTuple, "Pair of objects" },

  { NAME_type, NAME_programObject, makeClassType,
    &ClassType, "Argument-type specifier" },

  { NAME_var, NAME_function, makeClassVar,
    &ClassVar, "Variable (as a function)" },

  { NAME_variable, NAME_behaviour, makeClassVariable,
    &ClassVariable, "Instance- or local-variable" },

  { NAME_vector, NAME_object, makeClassVector,
    &ClassVector, "Dynamic array of objects" },

  { NAME_view, NAME_window, makeClassView,
    &ClassView, "Window version of an editor" },

  { NAME_visual, NAME_object, makeClassVisual,
    &ClassVisual, "Superclass of everything that is visible" },

  { NAME_vmi, NAME_programObject, makeClassVmi,
    &ClassVmi, "Virtual machine instruction" },

  { NAME_when, NAME_function, makeClassWhen,
    &ClassWhen, "Conditional function" },

  { NAME_while, NAME_code, makeClassWhile,
    &ClassWhile, "While-loop control structure" },

#ifdef __WIN32__
  { NAME_winMetafile, NAME_graphical, makeClassWinMF,
    &ClassWinMF, "Windows .WMF based figure" },
  { NAME_winPrinter, NAME_object, makeClassWinPrinter,
    &ClassWinPrinter, "Windows printer interface" },
#endif

  { NAME_window, NAME_device, makeClassWindow,
    &ClassWindow, "Interface between X-window and device" },

  { NAME_windowDecorator, NAME_window, makeClassWindowDecorator,
    &ClassWindowDecorator, "Attach scrollbars and label to a window" },

		 /*******************************
		 *	LAYOUT MANAGEMENT	*
		 *******************************/

  { NAME_layoutManager, NAME_object, makeClassLayoutManager,
    &ClassLayoutManager, "Manage layout of graphical objects" },

  { NAME_layoutInterface, NAME_object, makeClassLayoutInterface,
    &ClassLayoutInterface, "Interface between graphical and layout_manager" },

  { NAME_table, NAME_layoutManager, makeClassTable,
    &ClassTable, "Table layout of graphicals" },

  { NAME_tableSlice, NAME_vector, makeClassTableSlice,
    &ClassTableSlice, "Row/column of a table" },

  { NAME_tableRow, NAME_tableSlice, makeClassTableRow,
    &ClassTableRow, "Row of a table" },

  { NAME_tableColumn, NAME_tableSlice, makeClassTableColumn,
    &ClassTableColumn, "Column of a table" },

  { NAME_tableCell, NAME_layoutInterface, makeClassTableCell,
    &ClassTableCell, "Cell in a table" },

  { NULL, NULL, NULL, NULL, NULL }
};


void
initClassDefs()
{ defineClasses(classdefs);

#define defClass(n, s, mf, cp, summ) \
		(cp) = defineClass(n, s, CtoString(summ), mf);
  defClass(CtoName("*"), NAME_binaryExpression, makeClassTimes,
	   ClassTimes, "Multiplication expression");
  defClass(CtoName("-"), NAME_binaryExpression, makeClassMinus,
	   ClassMinus, "Subtraction expression");
  defClass(CtoName("+"), NAME_binaryExpression, makeClassPlus,
	   ClassPlus, "Addition expression");
  defClass(CtoName("/"), NAME_binaryExpression, makeClassDivide,
	   ClassDivide, "Division expression");
  defClass(CtoName(":="), NAME_object, makeClassBinding,
	   ClassBinding, "Name-value pair for argument-list");
  defClass(CtoName("<"), NAME_binaryCondition, makeClassLess,
	   ClassLess, "Compare expressions on less-then");
  defClass(CtoName("="), NAME_binaryCondition, makeClassEquation,
	   ClassEquation, "Identity between two expressions");
  defClass(CtoName("=<"), NAME_binaryCondition, makeClassLessEqual,
	   ClassLessEqual, "Compare expressions on less-or-equal");
  defClass(CtoName("=="), NAME_code, makeClassEqual,
	   ClassEqual, "Test equivalence of arguments");
  defClass(CtoName(">"), NAME_binaryCondition, makeClassGreater,
	   ClassGreater, "Test equivalence of arguments");
  defClass(CtoName(">="), NAME_binaryCondition, makeClassGreaterEqual,
	   ClassGreaterEqual, "Compare expressions on greater-or-equal");
  defClass(CtoName("?"), NAME_function, makeClassObtain,
	   ClassObtain, "Invoke a get method");
  defClass(CtoName("\\=="), NAME_code, makeClassNonEqual,
	   ClassNonEqual, "Test non-equivalence of arguments");
  defClass(CtoName("@="), NAME_code, makeClassAssoc,
	   ClassAssoc, "Assign named reference to object");

  numberTreeClass(ClassObject, 0);
}
  

		 /*******************************
		 *	      TYPES		*
		 *******************************/

static struct type_alias
{ char *alias;				/* alias-name */
  char *description;			/* syntax for the real thing */
} type_aliases[] =
{ { "button_name",	"{left,middle,right}" },
  { "texture_name",     "{none,dotted,dashed,dashdot,dashdotted,longdash}" },
  { "geometry",		"name" },

  { NULL, NULL }
};


void
initTypeAliases(void)
{ struct type_alias *alias = type_aliases;

  for(; alias->alias; alias++)
    defineType(alias->alias, alias->description);
}  

