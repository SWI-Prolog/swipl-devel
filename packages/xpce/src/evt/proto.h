#define COMMON(type) type SO_LOCAL

/* /staff/jan/src/pl/packages/xpce/src/evt/clickgesture.c */
COMMON(status)	makeClassClickGesture(Class class);

/* /staff/jan/src/pl/packages/xpce/src/evt/conngesture.c */
COMMON(status)	makeClassConnectGesture(Class class);

/* /staff/jan/src/pl/packages/xpce/src/evt/event.c */
COMMON(void)	considerLocStillEvent(void);
COMMON(PceWindow) WindowOfLastEvent(void);
COMMON(void)	unlinkedWindowEvent(Any sw);
COMMON(unsigned long) LastEventTime(void);
COMMON(void)	setLastEventTime(unsigned long time);
COMMON(status)	isAEvent(EventObj e, Any id);
COMMON(status)	eventName(Name name);
COMMON(status)	allButtonsUpEvent(EventObj e);
COMMON(status)	isUpEvent(EventObj e);
COMMON(status)	isDownEvent(EventObj e);
COMMON(Name)	getButtonEvent(EventObj e);
COMMON(status)	isDragEvent(EventObj ev);
COMMON(status)	hasModifierEvent(EventObj ev, Modifier m);
COMMON(Name)	getMulticlickEvent(EventObj e);
COMMON(Int)	getClickTimeEvent(EventObj e);
COMMON(Int)	getClickDisplacementEvent(EventObj e);
COMMON(status)	windowEvent(EventObj ev, PceWindow sw);
COMMON(status)	get_xy_event(EventObj ev, Any obj, Bool area, Int *rx, Int *ry);
COMMON(Point)	getPositionEvent(EventObj ev, Any obj);
COMMON(Point)	getAreaPositionEvent(EventObj ev, Graphical gr);
COMMON(Int)	getXEvent(EventObj ev, Any obj);
COMMON(Int)	getYEvent(EventObj ev, Any obj);
COMMON(status)	insideEvent(EventObj ev, Graphical gr);
COMMON(Int)	getDistanceEvent(EventObj ev1, EventObj ev2);
COMMON(Any)	getIdEvent(EventObj ev);
COMMON(Any)	getReceiverEvent(EventObj ev);
COMMON(status)	postNamedEvent(EventObj ev, Graphical obj, Recogniser rec, Name method);
COMMON(status)	postEvent(EventObj ev, Graphical obj, Recogniser rec);
COMMON(Any)	getMasterEvent(EventObj ev);
COMMON(DisplayObj) getDisplayEvent(EventObj ev);
COMMON(status)	mapWheelMouseEvent(EventObj ev, Any rec);
COMMON(status)	makeClassEvent(Class class);

/* /staff/jan/src/pl/packages/xpce/src/evt/eventnode.c */
COMMON(status)	isAEventNode(EventNodeObj sb, EventNodeObj super);
COMMON(status)	makeClassEventNode(Class class);

/* /staff/jan/src/pl/packages/xpce/src/evt/eventtree.c */
COMMON(status)	addNodeEventTree(EventTreeObj t, EventNodeObj n);
COMMON(EventNodeObj) getNodeEventTree(EventTreeObj t, Any value);
COMMON(status)	makeClassEventTree(Class class);

/* /staff/jan/src/pl/packages/xpce/src/evt/gesture.c */
COMMON(status)	initialiseGesture(Gesture g, Name button, Modifier modifier);
COMMON(status)	eventGesture(Any obj, EventObj ev);
COMMON(status)	cancelGesture(Gesture g, EventObj ev);
COMMON(status)	restrictAreaEvent(EventObj ev, Graphical gr);
COMMON(status)	makeClassGesture(Class class);

/* /staff/jan/src/pl/packages/xpce/src/evt/handler.c */
COMMON(status)	makeClassHandler(Class class);

/* /staff/jan/src/pl/packages/xpce/src/evt/handlergroup.c */
COMMON(status)	makeClassHandlerGroup(Class class);

/* /staff/jan/src/pl/packages/xpce/src/evt/modifier.c */
COMMON(status)	makeClassModifier(Class class);

/* /staff/jan/src/pl/packages/xpce/src/evt/movegesture.c */
COMMON(status)	initialiseMoveGesture(MoveGesture g, Name button, Modifier modifier);
COMMON(status)	makeClassMoveGesture(Class class);

/* /staff/jan/src/pl/packages/xpce/src/evt/mvolgesture.c */
COMMON(status)	makeClassMoveOutlineGesture(Class class);

/* /staff/jan/src/pl/packages/xpce/src/evt/popupgesture.c */
COMMON(status)	makeClassPopupGesture(Class class);
COMMON(Recogniser) popupGesture(void);

/* /staff/jan/src/pl/packages/xpce/src/evt/recogniser.c */
COMMON(status)	initialiseRecogniser(Recogniser r);
COMMON(status)	makeClassRecogniser(Class class);

/* /staff/jan/src/pl/packages/xpce/src/evt/resizegesture.c */
COMMON(status)	initialiseResizeGesture(ResizeGesture g, Name button, Modifier modifier);
COMMON(status)	makeClassResizeGesture(Class class);

/* /staff/jan/src/pl/packages/xpce/src/evt/rzolgesture.c */
COMMON(status)	makeClassResizeOutlineGesture(Class class);

/* /staff/jan/src/pl/packages/xpce/src/evt/edittextgest.c */
COMMON(status)	makeClassEditTextGesture(Class class);

/* /staff/jan/src/pl/packages/xpce/src/evt/browserselgesture.c */
COMMON(status)	makeClassBrowserSelectGesture(Class class);

/* /staff/jan/src/pl/packages/xpce/src/evt/resizetabslice.c */
COMMON(status)	makeClassResizeTableSliceGesture(Class class);
