
/* evt/clickgesture.c */
status makeClassClickGesture ( Class class );

/* evt/conngesture.c */
status makeClassConnectGesture ( Class class );

/* evt/event.c */
PceWindow WindowOfLastEvent ( void );
ulong LastEventTime ( void );
void setLastEventTime ( ulong time );
Int getTimeEvent ( EventObj ev , EventObj ev2 );
status isAEvent ( EventObj e , Any id );
status eventName ( Name name );
status allButtonsUpEvent ( EventObj e );
status isUpEvent ( EventObj e );
status isDownEvent ( EventObj e );
Name getButtonEvent ( EventObj e );
status isDragEvent ( EventObj ev );
status hasModifierEvent ( EventObj ev , Modifier m );
Name getMulticlickEvent ( EventObj e );
Int getClickTimeEvent ( EventObj e );
Int getClickDisplacementEvent ( EventObj e );
status get_xy_event ( EventObj ev , Any obj , Bool area , Int *rx , Int *ry );
Point getPositionEvent ( EventObj ev , Any obj );
Point getAreaPositionEvent ( EventObj ev , Graphical gr );
Int getXEvent ( EventObj ev , Any obj );
Int getYEvent ( EventObj ev , Any obj );
status insideEvent ( EventObj ev , Graphical gr );
Any getInsideSubWindow ( EventObj ev , Any root );
Int getDistanceEvent ( EventObj ev1 , EventObj ev2 );
Any getIdEvent ( EventObj ev );
Any getReceiverEvent ( EventObj ev );
Name getKeyEvent ( EventObj ev );
status postEvent ( EventObj ev , Graphical obj , Recogniser rec );
Any getMasterEvent ( EventObj ev );
DisplayObj getDisplayEvent ( EventObj ev );
status makeClassEvent ( Class class );

/* evt/eventnode.c */
status isAEventNode ( EventNodeObj sb , EventNodeObj super );
status makeClassEventNode ( Class class );

/* evt/eventtree.c */
status addNodeEventTree ( EventTreeObj t , EventNodeObj n );
EventNodeObj getNodeEventTree ( EventTreeObj t , Any value );
status makeClassEventTree ( Class class );

/* evt/gesture.c */
status initialiseGesture ( Gesture g , Name button , Modifier modifier );
status eventGesture ( Any obj , EventObj ev );
status cancelGesture ( Gesture g , EventObj ev );
status makeClassGesture ( Class class );

/* evt/handler.c */
status makeClassHandler ( Class class );

/* evt/handlergroup.c */
status makeClassHandlerGroup ( Class class );

/* evt/modifier.c */
status makeClassModifier ( Class class );

/* evt/movegesture.c */
status initialiseMoveGesture ( MoveGesture g , Name button , Modifier modifier );
status makeClassMoveGesture ( Class class );

/* evt/mvolgesture.c */
status makeClassMoveOutlineGesture ( Class class );

/* evt/popupgesture.c */
status makeClassPopupGesture ( Class class );
Recogniser popupGesture ( void );

/* evt/recogniser.c */
status initialiseRecogniser ( Recogniser r );
status makeClassRecogniser ( Class class );

/* evt/resizegesture.c */
status initialiseResizeGesture ( ResizeGesture g , Name button , Modifier modifier );
status makeClassResizeGesture ( Class class );

/* evt/rzolgesture.c */
status makeClassResizeOutlineGesture ( Class class );
