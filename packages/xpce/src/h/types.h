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
typedef	long			PseudoFloat;
typedef struct pceITFSymbol    *PceITFSymbol;
typedef struct _string	       *String;

		/********************************
		*         KERNEL TYPES		*
		********************************/

typedef struct and *			And;
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
typedef struct cell *			Cell;
typedef struct chain *			Chain;
typedef struct chain_table *		ChainTable;
typedef struct char_array *		CharArray;
typedef struct class *			Class;
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
typedef struct plus *			Plus;
typedef struct point *			Point;
typedef struct progn *			Progn;
typedef struct popup *			Popup;
typedef struct process *		Process;
typedef struct quote_function *		QuoteFunction;
typedef struct socket *			Socket;
typedef struct program_object *		ProgramObject;
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
typedef struct spatial *		Spatial;
typedef struct stream *			Stream;
typedef struct string *			StringObj;
typedef struct times *			Times;
typedef struct tuple *			Tuple;
typedef struct type *			Type;
typedef struct variable *		Variable;
typedef struct var *			Var;
typedef struct vector *			Vector;
typedef struct vmi *			Vmi;
typedef struct visual * 		VisualObj;
typedef struct when *			When;
typedef struct while_obj *		While;
typedef struct create *			Create;


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
typedef struct connect_gesture    	* ConnectGesture;
typedef struct connection		* Connection;
typedef struct cursorobj		* CursorObj;
typedef struct device			* Device;
typedef struct dialog			* Dialog;
typedef struct dialog_item		* DialogItem;
typedef struct display_manager		* DisplayManager;
typedef struct displayobj		* DisplayObj;
typedef struct draw_context		* DrawContext;
typedef struct editor 			* Editor;
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
typedef struct popup_gesture    	* PopupGesture;
typedef struct popupobj			* PopupObj;
typedef struct resize_gesture   	* ResizeGesture;
typedef struct resize_outline_gesture   * ResizeOutlineGesture;
typedef struct resource			* Resource;
typedef struct scrollbar		* ScrollBar;
typedef struct slider			* Slider;
typedef struct style			* Style;
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


typedef Any				EventId;

		/********************************
		*         TYPE POINTERS		*
		********************************/

GLOBAL Type TypeAlien;
GLOBAL Type TypeAny;
GLOBAL Type TypeAny;
GLOBAL Type TypeArg;
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

GLOBAL Class ClassAnd;
GLOBAL Class ClassArc;
GLOBAL Class ClassArea;
GLOBAL Class ClassArrow;
GLOBAL Class ClassTable;
GLOBAL Class ClassAttribute;
GLOBAL Class ClassBehaviour;
GLOBAL Class ClassBinaryCondition;
GLOBAL Class ClassBinaryExpression;
GLOBAL Class ClassAssign;
GLOBAL Class ClassBinding;
GLOBAL Class ClassBitmap;
GLOBAL Class ClassBlock;
GLOBAL Class ClassBox;
GLOBAL Class ClassBool;
GLOBAL Class ClassBrowser;
GLOBAL Class ClassButton;
GLOBAL Class ClassChain;
GLOBAL Class ClassChainTable;
GLOBAL Class ClassCharArray;
GLOBAL Class ClassCircle;
GLOBAL Class ClassClass;
GLOBAL Class ClassClickGesture;
GLOBAL Class ClassCode;
GLOBAL Class ClassCodeVector;
GLOBAL Class ClassColour;
GLOBAL Class ClassConnectGesture;
GLOBAL Class ClassConnection;
GLOBAL Class ClassConstant;
GLOBAL Class ClassConstraint;
GLOBAL Class ClassCreate;
GLOBAL Class ClassCursor;
GLOBAL Class ClassDate;
GLOBAL Class ClassDelegateVariable;
GLOBAL Class ClassDevice;
GLOBAL Class ClassDialog;
GLOBAL Class ClassDialogItem;
GLOBAL Class ClassDict;
GLOBAL Class ClassDictItem;
GLOBAL Class ClassDirectory;
GLOBAL Class ClassDisplay;
GLOBAL Class ClassDisplayManager;
GLOBAL Class ClassEditor;
GLOBAL Class ClassEllipse;
GLOBAL Class ClassEqual;
GLOBAL Class ClassEquation;
GLOBAL Class ClassError;
GLOBAL Class ClassEvent;
GLOBAL Class ClassEventNode;
GLOBAL Class ClassEventTree;
GLOBAL Class ClassFigure;
GLOBAL Class ClassFile;
GLOBAL Class ClassFunction;
GLOBAL Class ClassFont;
GLOBAL Class ClassFormat;
GLOBAL Class ClassFragment;
GLOBAL Class ClassFrame;
GLOBAL Class ClassGesture;
GLOBAL Class ClassGetMethod;
GLOBAL Class ClassGraphical;
GLOBAL Class ClassHandle;
GLOBAL Class ClassHandler;
GLOBAL Class ClassHandlerGroup;
GLOBAL Class ClassHashTable;
GLOBAL Class ClassHost;
GLOBAL Class ClassHyper;
GLOBAL Class ClassIdentity;
GLOBAL Class ClassIf;
GLOBAL Class ClassImage;
GLOBAL Class ClassKeyBinding;
GLOBAL Class ClassJoint;
GLOBAL Class ClassLabel;
GLOBAL Class ClassLine;
GLOBAL Class ClassLink;
GLOBAL Class ClassListBrowser;
GLOBAL Class ClassMenu;
GLOBAL Class ClassMenuBar;
GLOBAL Class ClassMenuItem;
GLOBAL Class ClassMessage;
GLOBAL Class ClassMethod;
GLOBAL Class ClassModifier;
GLOBAL Class ClassMoveGesture;
GLOBAL Class ClassMoveOutlineGesture;
GLOBAL Class ClassName;
GLOBAL Class ClassNode;
GLOBAL Class ClassNonEqual;
GLOBAL Class ClassNot;
GLOBAL Class ClassNumber;
GLOBAL Class ClassObject;
GLOBAL Class ClassObject;
GLOBAL Class ClassObtain;
GLOBAL Class ClassOr;
GLOBAL Class ClassPath;
GLOBAL Class ClassPce;
GLOBAL Class ClassPicture;
GLOBAL Class ClassPoint;
GLOBAL Class ClassProgn;
GLOBAL Class ClassPopup;
GLOBAL Class ClassProcess;
GLOBAL Class ClassQuoteFunction;
GLOBAL Class ClassSocket;
GLOBAL Class ClassPopupGesture;
GLOBAL Class ClassReal;
GLOBAL Class ClassRecogniser;
GLOBAL Class ClassRegex;
GLOBAL Class ClassRegion;
GLOBAL Class ClassRelation;
GLOBAL Class ClassResizeGesture;
GLOBAL Class ClassResizeOutlineGesture;
GLOBAL Class ClassResource;
GLOBAL Class ClassScrollBar;
GLOBAL Class ClassSendMethod;
GLOBAL Class ClassSheet;
GLOBAL Class ClassSize;
GLOBAL Class ClassSlider;
GLOBAL Class ClassSourceLocation;
GLOBAL Class ClassSpatial;
GLOBAL Class ClassStream;
GLOBAL Class ClassString;
GLOBAL Class ClassStyle;
GLOBAL Class ClassSyntaxTable;
GLOBAL Class ClassText;
GLOBAL Class ClassTextBuffer;
GLOBAL Class ClassTextCursor;
GLOBAL Class ClassTextImage;
GLOBAL Class ClassTextItem;
GLOBAL Class ClassTextMargin;
GLOBAL Class ClassTile;
GLOBAL Class ClassTimer;
GLOBAL Class ClassTuple;
GLOBAL Class ClassType;
GLOBAL Class ClassTree;
GLOBAL Class ClassVar;
GLOBAL Class ClassVariable;
GLOBAL Class ClassVector;
GLOBAL Class ClassView;
GLOBAL Class ClassVisual;
GLOBAL Class ClassWhen;
GLOBAL Class ClassWhile;
GLOBAL Class ClassWindow;
GLOBAL Class ClassDivide;
GLOBAL Class ClassProgramObject;
GLOBAL Class ClassVmi;
GLOBAL Class ClassPlus;
GLOBAL Class ClassMinus;
GLOBAL Class ClassTimes;
GLOBAL Class ClassWindowDecorator;
GLOBAL Class ClassC;
GLOBAL Class ClassLess;
GLOBAL Class ClassLessEqual;
GLOBAL Class ClassGreater;
GLOBAL Class ClassGreaterEqual;
