/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

		/********************************
		*       KERNEL IDENTIFIERS	*
		********************************/

#define CLASSINDEX_OBJECT	(1)
#define CLASSINDEX_TYPE		(2)
#define CLASSINDEX_CLASS	(3)
#define CLASSINDEX_NAME		(4)
#define CLASSINDEX_CHAR_ARRAY	(5)
#define CLASSINDEX_START	(6)	/* start index for others */


		/********************************
		*     MISCELLENEOUS TYPES	*
		********************************/

typedef struct _goal	       *Goal;
typedef struct pceITFSymbol    *PceITFSymbol;
typedef struct _string	       *String;

#ifndef LONGLONG
#define LONGLONG long
#endif

typedef	LONGLONG		PseudoFloat;

		/********************************
		*         KERNEL TYPES		*
		********************************/

typedef struct and *			And;
typedef struct application *		Application;
typedef struct area *			Area;
typedef struct atable *			Atable;
typedef struct attribute *		Attribute;
typedef struct behaviour *		Behaviour;
typedef struct binary_expression *	BinaryExpression;
typedef struct binary_condition *	BinaryCondition;
typedef struct binding *		Binding;
typedef struct assignment *		Assignment;
typedef struct block *			Block;
typedef struct bool *			Bool;
typedef struct c_pointer *		CPointer;
typedef struct cell *			Cell;
typedef struct chain *			Chain;
typedef struct chain_table *		ChainTable;
typedef struct char_array *		CharArray;
typedef struct class *			Class;
typedef struct class_stub *		ClassStub;
typedef struct class_variable * 	ClassVariable;
typedef struct code *			Code;
typedef struct constant *		Constant;
typedef struct constraint *		Constraint;
typedef struct date *			Date;
typedef struct delegate_variable *	DelegateVariable;
typedef struct dict *			Dict;
typedef struct dictitem *		DictItem;
typedef struct directory *		Directory;
typedef struct divide *			Divide;
typedef struct equal *			Equal;
typedef struct equation *		Equation;
typedef struct error *			Error;
typedef struct eventobj *		EventObj;
typedef struct expression *		Expression;
typedef struct fileobj *		FileObj;
typedef struct function *		Function;
typedef struct gesture * 		Gesture;
typedef struct get_method *		GetMethod;
typedef struct handle *			Handle;
typedef struct handler *		Handler;
typedef struct handlergroup *		HandlerGroup;
typedef struct hash_table *		HashTable;
typedef struct host *			Host;
typedef struct host_data *		HostData;
typedef struct hyper *			Hyper;
typedef struct identity *		Identity;
typedef struct if_obj *			If;
typedef struct instance *		Instance;
typedef struct key_binding *		KeyBinding;
typedef struct message *		Message;
typedef struct method *			Method;
typedef struct minus *			Minus;
typedef struct modifier *		Modifier;
typedef struct name *			Name;
typedef struct non_equal *		NonEqual;
typedef struct not *			Not;
typedef struct number *			Number;
typedef struct object *			PceObject;
typedef struct obtain *			Obtain;
typedef struct or *			Or;
typedef struct pce *			Pce;
typedef struct pen *			Pen;
typedef struct plus *			Plus;
typedef struct point *			Point;
typedef struct progn *			Progn;
typedef struct popup *			Popup;
typedef struct process *		Process;
typedef struct quote_function *		QuoteFunction;
typedef struct socketobj *		Socket;
typedef struct program_object *		ProgramObject;
typedef struct rc *			RC;
typedef struct real *			Real;
typedef struct recogniser *		Recogniser;
typedef struct regex *			Regex;
typedef struct region *			RegionObj;
typedef struct relation *		Relation;
typedef struct send_method *		SendMethod;
typedef struct sheet *			Sheet;
typedef struct size *			Size;
typedef struct symbol *			Symbol;
typedef struct syntax_table *		SyntaxTable;
typedef struct source_location *	SourceLocation;
typedef struct source_sink *		SourceSink;
typedef struct spatial *		Spatial;
typedef struct stream *			Stream;
typedef struct string *			StringObj;
typedef struct times *			Times;
typedef struct tuple *			Tuple;
typedef struct type *			Type;
typedef struct variable *		Variable;
typedef struct var *			Var;
typedef struct vector *			Vector;
typedef struct visual * 		VisualObj;
typedef struct when *			When;
typedef struct while_obj *		While;
typedef struct create_obj *		Create;


		/********************************
		*        GRAPHICAL TYPES	*
		********************************/

typedef struct arc 			* Arc;
typedef struct arrow			* Arrow;
typedef struct bitmapobj		* BitmapObj;
typedef struct box			* Box;
typedef struct browser			* Browser;
typedef struct button			* Button;
typedef struct circle			* Circle;
typedef struct click_gesture    	* ClickGesture;
typedef struct colour			* Colour;
typedef struct colour_map		* ColourMap;
typedef struct connect_gesture    	* ConnectGesture;
typedef struct connection		* Connection;
typedef struct cursorobj		* CursorObj;
typedef struct device			* Device;
typedef struct dialog			* Dialog;
typedef struct dialog_item		* DialogItem;
typedef struct dialog_group		* DialogGroup;
typedef struct display_manager		* DisplayManager;
typedef struct displayobj		* DisplayObj;
typedef struct draw_context		* DrawContext;
typedef struct edit_text_gesture     	* EditTextGesture;
typedef struct editor 			* Editor;
typedef struct elevation		* Elevation;
typedef struct ellipse			* Ellipse;
typedef struct event_nodeobj		* EventNodeObj;
typedef struct event_treeobj		* EventTreeObj;
typedef struct figure			* Figure;
typedef struct fontobj			* FontObj;
typedef struct format			* Format;
typedef struct fragment			* Fragment;
typedef struct frameobj			* FrameObj;
typedef struct graphical		* Graphical;
typedef struct image			* Image;
typedef struct joint			* Joint;
typedef struct label			* Label;
typedef struct label_box		* LabelBox;
typedef struct line			* Line;
typedef struct link			* Link;
typedef struct list_browser		* ListBrowser;
typedef struct menu			* Menu;
typedef struct menu_bar			* MenuBar;
typedef struct menu_item		* MenuItem;
typedef struct move_gesture     	* MoveGesture;
typedef struct move_outline_gesture     * MoveOutlineGesture;
typedef struct node			* Node;
typedef struct path			* Path;
typedef struct picture			* Picture;
typedef struct pixmapobj		* PixmapObj;
typedef struct popup_gesture    	* PopupGesture;
typedef struct popupobj			* PopupObj;
typedef struct resize_gesture   	* ResizeGesture;
typedef struct resize_outline_gesture   * ResizeOutlineGesture;
typedef struct scrollbar		* ScrollBar;
typedef struct slider			* Slider;
typedef struct style			* Style;
typedef struct tab			* Tab;
typedef struct tab_stack		* TabStack;
typedef struct text_cursor		* TextCursor;
typedef struct text_image		* TextImage;
typedef struct text_margin		* TextMargin;
typedef struct text_buffer		* TextBuffer;
typedef struct textitem			* TextItem;
typedef struct textobj			* TextObj;
typedef struct tileobj			* TileObj;
typedef struct timer			* Timer;
typedef struct tree			* Tree;
typedef struct undo_buffer		* UndoBuffer;
typedef struct view			* View;
typedef struct windowobj		* PceWindow;
typedef struct window_decorator 	* WindowDecorator;
typedef struct resize_table_slice_gesture * ResizeTableSliceGesture;

typedef Any				EventId;

		/********************************
		*         TYPE POINTERS		*
		********************************/

GLOBAL Type TypeAlien;
GLOBAL Type TypeAny;
GLOBAL Type TypeAny;
GLOBAL Type TypeArg;
GLOBAL Type TypeAtomic;
GLOBAL Type TypeBool;
GLOBAL Type TypeChar;
GLOBAL Type TypeChain;
GLOBAL Type TypeCharArray;
GLOBAL Type TypeClass;
GLOBAL Type TypeCode;
GLOBAL Type TypeConstant;
GLOBAL Type TypeColour;
GLOBAL Type TypeDefault;
GLOBAL Type TypeEventId;
GLOBAL Type TypeExpression;
GLOBAL Type TypeEquation;
GLOBAL Type TypeFunction;
GLOBAL Type TypeGetMethod;
GLOBAL Type TypeGraphical;
GLOBAL Type TypeInt;
GLOBAL Type TypeImage;
GLOBAL Type TypeName;
GLOBAL Type TypeNil;
GLOBAL Type TypeObject;
GLOBAL Type TypeReal;
GLOBAL Type TypeSendMethod;
GLOBAL Type TypeType;
GLOBAL Type TypeUnchecked;
GLOBAL Type TypeVar;
GLOBAL Type TypeVariable;
GLOBAL Type TypeVector;

		/********************************
		*        CLASS POINTERS		*
		********************************/

PUBLIC_GLOBAL Class ClassAnd;
PUBLIC_GLOBAL Class ClassApplication;
PUBLIC_GLOBAL Class ClassArc;
PUBLIC_GLOBAL Class ClassArea;
PUBLIC_GLOBAL Class ClassArrow;
PUBLIC_GLOBAL Class ClassRelationTable;
PUBLIC_GLOBAL Class ClassAttribute;
PUBLIC_GLOBAL Class ClassBehaviour;
PUBLIC_GLOBAL Class ClassBinaryCondition;
PUBLIC_GLOBAL Class ClassBinaryExpression;
PUBLIC_GLOBAL Class ClassAssign;
PUBLIC_GLOBAL Class ClassAssoc;
PUBLIC_GLOBAL Class ClassBinding;
PUBLIC_GLOBAL Class ClassBitmap;
PUBLIC_GLOBAL Class ClassBlock;
PUBLIC_GLOBAL Class ClassBox;
PUBLIC_GLOBAL Class ClassBool;
PUBLIC_GLOBAL Class ClassBrowser;
PUBLIC_GLOBAL Class ClassBrowserSelectGesture;
PUBLIC_GLOBAL Class ClassButton;
PUBLIC_GLOBAL Class ClassChain;
PUBLIC_GLOBAL Class ClassChainTable;
PUBLIC_GLOBAL Class ClassCharArray;
PUBLIC_GLOBAL Class ClassCircle;
PUBLIC_GLOBAL Class ClassClass;
PUBLIC_GLOBAL Class ClassClassStub;
PUBLIC_GLOBAL Class ClassClassVariable;
PUBLIC_GLOBAL Class ClassClickGesture;
PUBLIC_GLOBAL Class ClassCode;
PUBLIC_GLOBAL Class ClassCodeVector;
PUBLIC_GLOBAL Class ClassColour;
PUBLIC_GLOBAL Class ClassColourMap;
PUBLIC_GLOBAL Class ClassConnectGesture;
PUBLIC_GLOBAL Class ClassConnection;
PUBLIC_GLOBAL Class ClassConstant;
PUBLIC_GLOBAL Class ClassConstraint;
PUBLIC_GLOBAL Class ClassCPointer;
PUBLIC_GLOBAL Class ClassCreate;
PUBLIC_GLOBAL Class ClassCursor;
PUBLIC_GLOBAL Class ClassDate;
PUBLIC_GLOBAL Class ClassDelegateVariable;
PUBLIC_GLOBAL Class ClassDevice;
PUBLIC_GLOBAL Class ClassDialog;
PUBLIC_GLOBAL Class ClassDialogItem;
PUBLIC_GLOBAL Class ClassDialogGroup;
PUBLIC_GLOBAL Class ClassDict;
PUBLIC_GLOBAL Class ClassDictItem;
PUBLIC_GLOBAL Class ClassDirectory;
PUBLIC_GLOBAL Class ClassDisplay;
PUBLIC_GLOBAL Class ClassDisplayManager;
PUBLIC_GLOBAL Class ClassEditTextGesture;
PUBLIC_GLOBAL Class ClassEditor;
PUBLIC_GLOBAL Class ClassElevation;
PUBLIC_GLOBAL Class ClassEllipse;
PUBLIC_GLOBAL Class ClassEqual;
PUBLIC_GLOBAL Class ClassEquation;
PUBLIC_GLOBAL Class ClassError;
PUBLIC_GLOBAL Class ClassEvent;
PUBLIC_GLOBAL Class ClassEventNode;
PUBLIC_GLOBAL Class ClassEventTree;
PUBLIC_GLOBAL Class ClassFigure;
PUBLIC_GLOBAL Class ClassFile;
PUBLIC_GLOBAL Class ClassFunction;
PUBLIC_GLOBAL Class ClassFont;
PUBLIC_GLOBAL Class ClassFormat;
PUBLIC_GLOBAL Class ClassFragment;
PUBLIC_GLOBAL Class ClassFrame;
PUBLIC_GLOBAL Class ClassGesture;
PUBLIC_GLOBAL Class ClassGetMethod;
PUBLIC_GLOBAL Class ClassGraphical;
PUBLIC_GLOBAL Class ClassHandle;
PUBLIC_GLOBAL Class ClassHandler;
PUBLIC_GLOBAL Class ClassHandlerGroup;
PUBLIC_GLOBAL Class ClassHashTable;
PUBLIC_GLOBAL Class ClassHost;
PUBLIC_GLOBAL Class ClassHostData;
PUBLIC_GLOBAL Class ClassHyper;
PUBLIC_GLOBAL Class ClassChainHyper;
PUBLIC_GLOBAL Class ClassIdentity;
PUBLIC_GLOBAL Class ClassIf;
PUBLIC_GLOBAL Class ClassImage;
PUBLIC_GLOBAL Class ClassKeyBinding;
PUBLIC_GLOBAL Class ClassJoint;
PUBLIC_GLOBAL Class ClassLabel;
PUBLIC_GLOBAL Class ClassLabelBox;
PUBLIC_GLOBAL Class ClassLine;
PUBLIC_GLOBAL Class ClassLink;
PUBLIC_GLOBAL Class ClassListBrowser;
PUBLIC_GLOBAL Class ClassMenu;
PUBLIC_GLOBAL Class ClassMenuBar;
PUBLIC_GLOBAL Class ClassMenuItem;
PUBLIC_GLOBAL Class ClassMessage;
PUBLIC_GLOBAL Class ClassMethod;
PUBLIC_GLOBAL Class ClassModifier;
PUBLIC_GLOBAL Class ClassMoveGesture;
PUBLIC_GLOBAL Class ClassMoveOutlineGesture;
PUBLIC_GLOBAL Class ClassName;
PUBLIC_GLOBAL Class ClassNode;
PUBLIC_GLOBAL Class ClassNonEqual;
PUBLIC_GLOBAL Class ClassNot;
PUBLIC_GLOBAL Class ClassNumber;
PUBLIC_GLOBAL Class ClassObject;
PUBLIC_GLOBAL Class ClassObject;
PUBLIC_GLOBAL Class ClassObtain;
PUBLIC_GLOBAL Class ClassOr;
PUBLIC_GLOBAL Class ClassPath;
PUBLIC_GLOBAL Class ClassPce;
PUBLIC_GLOBAL Class ClassPicture;
PUBLIC_GLOBAL Class ClassPixmap;
PUBLIC_GLOBAL Class ClassPoint;
PUBLIC_GLOBAL Class ClassPen;
PUBLIC_GLOBAL Class ClassProgn;
PUBLIC_GLOBAL Class ClassPopup;
PUBLIC_GLOBAL Class ClassProcess;
PUBLIC_GLOBAL Class ClassQuoteFunction;
PUBLIC_GLOBAL Class ClassSocket;
PUBLIC_GLOBAL Class ClassPopupGesture;
PUBLIC_GLOBAL Class ClassRC;
PUBLIC_GLOBAL Class ClassReal;
PUBLIC_GLOBAL Class ClassRecogniser;
PUBLIC_GLOBAL Class ClassRegex;
PUBLIC_GLOBAL Class ClassRegion;
PUBLIC_GLOBAL Class ClassRelation;
PUBLIC_GLOBAL Class ClassResizeGesture;
PUBLIC_GLOBAL Class ClassResizeOutlineGesture;
PUBLIC_GLOBAL Class ClassResizeTableSliceGesture;
PUBLIC_GLOBAL Class ClassClassVariable;
PUBLIC_GLOBAL Class ClassScrollBar;
PUBLIC_GLOBAL Class ClassSendMethod;
PUBLIC_GLOBAL Class ClassSheet;
PUBLIC_GLOBAL Class ClassSize;
PUBLIC_GLOBAL Class ClassSlider;
PUBLIC_GLOBAL Class ClassSourceLocation;
PUBLIC_GLOBAL Class ClassSourceSink;
PUBLIC_GLOBAL Class ClassSpatial;
PUBLIC_GLOBAL Class ClassStream;
PUBLIC_GLOBAL Class ClassString;
PUBLIC_GLOBAL Class ClassStyle;
PUBLIC_GLOBAL Class ClassSyntaxTable;
PUBLIC_GLOBAL Class ClassTab;
PUBLIC_GLOBAL Class ClassTabStack;
PUBLIC_GLOBAL Class ClassText;
PUBLIC_GLOBAL Class ClassTextBuffer;
PUBLIC_GLOBAL Class ClassTextCursor;
PUBLIC_GLOBAL Class ClassTextImage;
PUBLIC_GLOBAL Class ClassTextItem;
PUBLIC_GLOBAL Class ClassTextMargin;
PUBLIC_GLOBAL Class ClassTile;
PUBLIC_GLOBAL Class ClassTimer;
PUBLIC_GLOBAL Class ClassTuple;
PUBLIC_GLOBAL Class ClassType;
PUBLIC_GLOBAL Class ClassTree;
PUBLIC_GLOBAL Class ClassVar;
PUBLIC_GLOBAL Class ClassObjOfVariable;
PUBLIC_GLOBAL Class ClassVector;
PUBLIC_GLOBAL Class ClassView;
PUBLIC_GLOBAL Class ClassVisual;
PUBLIC_GLOBAL Class ClassWhen;
PUBLIC_GLOBAL Class ClassWhile;
PUBLIC_GLOBAL Class ClassWindow;
PUBLIC_GLOBAL Class ClassDivide;
PUBLIC_GLOBAL Class ClassProgramObject;
PUBLIC_GLOBAL Class ClassPlus;
PUBLIC_GLOBAL Class ClassMinus;
PUBLIC_GLOBAL Class ClassTimes;
PUBLIC_GLOBAL Class ClassWindowDecorator;
PUBLIC_GLOBAL Class ClassC;
PUBLIC_GLOBAL Class ClassLess;
PUBLIC_GLOBAL Class ClassLessEqual;
PUBLIC_GLOBAL Class ClassGreater;
PUBLIC_GLOBAL Class ClassGreaterEqual;
