
/* ../src/evt/clickgesture.c */
status		makeClassClickGesture(Class class);

/* ../src/evt/conngesture.c */
status		makeClassConnectGesture(Class class);

/* ../src/evt/event.c */
void		considerLocStillEvent(void);
PceWindow	WindowOfLastEvent(void);
void		unlinkedWindowEvent(Any sw);
unsigned long	LastEventTime(void);
void		setLastEventTime(unsigned long time);
status		isAEvent(EventObj e, Any id);
status		eventName(Name name);
status		allButtonsUpEvent(EventObj e);
status		isUpEvent(EventObj e);
status		isDownEvent(EventObj e);
Name		getButtonEvent(EventObj e);
status		isDragEvent(EventObj ev);
status		hasModifierEvent(EventObj ev, Modifier m);
Name		getMulticlickEvent(EventObj e);
Int		getClickTimeEvent(EventObj e);
Int		getClickDisplacementEvent(EventObj e);
status		windowEvent(EventObj ev, PceWindow sw);
status		get_xy_event(EventObj ev, Any obj, Bool area, Int *rx, Int *ry);
Point		getPositionEvent(EventObj ev, Any obj);
Point		getAreaPositionEvent(EventObj ev, Graphical gr);
Int		getXEvent(EventObj ev, Any obj);
Int		getYEvent(EventObj ev, Any obj);
status		insideEvent(EventObj ev, Graphical gr);
Int		getDistanceEvent(EventObj ev1, EventObj ev2);
Any		getIdEvent(EventObj ev);
Any		getReceiverEvent(EventObj ev);
status		postEvent(EventObj ev, Graphical obj, Recogniser rec);
Any		getMasterEvent(EventObj ev);
DisplayObj	getDisplayEvent(EventObj ev);
status		makeClassEvent(Class class);

/* ../src/evt/eventnode.c */
status		isAEventNode(EventNodeObj sb, EventNodeObj super);
status		makeClassEventNode(Class class);

/* ../src/evt/eventtree.c */
status		addNodeEventTree(EventTreeObj t, EventNodeObj n);
EventNodeObj	getNodeEventTree(EventTreeObj t, Any value);
status		makeClassEventTree(Class class);

/* ../src/evt/gesture.c */
status		initialiseGesture(Gesture g, Name button, Modifier modifier);
status		eventGesture(Any obj, EventObj ev);
status		cancelGesture(Gesture g, EventObj ev);
status		restrictAreaEvent(EventObj ev, Graphical gr);
status		makeClassGesture(Class class);

/* ../src/evt/handler.c */
status		makeClassHandler(Class class);

/* ../src/evt/handlergroup.c */
status		makeClassHandlerGroup(Class class);

/* ../src/evt/modifier.c */
status		makeClassModifier(Class class);

/* ../src/evt/movegesture.c */
status		initialiseMoveGesture(MoveGesture g, Name button, Modifier modifier);
status		makeClassMoveGesture(Class class);

/* ../src/evt/mvolgesture.c */
status		makeClassMoveOutlineGesture(Class class);

/* ../src/evt/popupgesture.c */
status		makeClassPopupGesture(Class class);
Recogniser	popupGesture(void);

/* ../src/evt/recogniser.c */
status		initialiseRecogniser(Recogniser r);
status		makeClassRecogniser(Class class);

/* ../src/evt/resizegesture.c */
status		initialiseResizeGesture(ResizeGesture g, Name button, Modifier modifier);
status		makeClassResizeGesture(Class class);

/* ../src/evt/rzolgesture.c */
status		makeClassResizeOutlineGesture(Class class);

/* ../src/evt/edittextgest.c */
status		makeClassEditTextGesture(Class class);

/* ../src/evt/browserselgesture.c */
status		makeClassBrowserSelectGesture(Class class);
