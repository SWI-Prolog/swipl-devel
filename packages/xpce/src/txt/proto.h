#define COMMON(type) type SO_LOCAL

/* /staff/jan/src/pl/packages/xpce/src/txt/chararray.c */
COMMON(status)	initialiseCharArray(CharArray n, CharArray value);
COMMON(Name)	getValueCharArray(CharArray n);
COMMON(status)	equalCharArray(CharArray n1, CharArray n2, Bool ign_case);
COMMON(status)	prefixCharArray(CharArray n1, CharArray n2, Bool ign_case);
COMMON(status)	suffixCharArray(CharArray n, CharArray s, Bool ign_case);
COMMON(CharArray) getCopyCharArray(CharArray n);
COMMON(CharArray) getCapitaliseCharArray(CharArray n);
COMMON(CharArray) getLabelNameCharArray(CharArray n);
COMMON(CharArray) getDowncaseCharArray(CharArray n);
COMMON(CharArray) getAppendCharArray(CharArray n1, CharArray n2);
COMMON(CharArray) getDeleteSuffixCharArray(CharArray n, CharArray s);
COMMON(CharArray) getEnsureSuffixCharArray(CharArray n, CharArray s);
COMMON(Int)	getSizeCharArray(Any n);
COMMON(void)	initCharArrays(void);
COMMON(CharArray) CtoScratchCharArray(const char *s);
COMMON(CharArray) StringToScratchCharArray(const String s);
COMMON(void)	doneScratchCharArray(CharArray n);
COMMON(CharArray) CtoCharArray(char *s);
COMMON(status)	makeClassCharArray(Class class);

/* /staff/jan/src/pl/packages/xpce/src/txt/editor.c */
COMMON(status)	normaliseEditor(Editor e, Int start, Int end);
COMMON(Any)	ReceiverOfEditor(Editor e);
COMMON(status)	forwardModifiedEditor(Editor e, Bool val);
COMMON(status)	scrollToEditor(Editor e, Int pos, Int screenline);
COMMON(status)	selectionEditor(Editor e, Int from, Int to, Name status);
COMMON(Point)	getSelectionEditor(Editor e);
COMMON(StringObj) getSelectedEditor(Editor e);
COMMON(status)	formatEditor(Editor e, CharArray fmt, int argc, Any *argv);
COMMON(status)	clearEditor(Editor e);
COMMON(status)	backgroundEditor(Editor e, Any bg);
COMMON(status)	colourEditor(Editor e, Any c);
COMMON(status)	makeClassEditor(Class class);

/* /staff/jan/src/pl/packages/xpce/src/txt/fragment.c */
COMMON(status)	makeClassFragment(Class class);

/* /staff/jan/src/pl/packages/xpce/src/txt/keybinding.c */
COMMON(Any)	getFunctionKeyBinding(KeyBinding kb, EventId id);
COMMON(status)	functionKeyBinding(KeyBinding kb, EventId id, Any f);
COMMON(status)	typedKeyBinding(KeyBinding kb, Any id, Graphical receiver);
COMMON(status)	makeClassKeyBinding(Class class);
COMMON(KeyBinding) KeyBindingText(void);
COMMON(KeyBinding) KeyBindingTextItem(void);
COMMON(KeyBinding) KeyBindingTextItemView(void);

/* /staff/jan/src/pl/packages/xpce/src/txt/regex.c */
COMMON(status)	ignoreCaseRegex(Regex re, Bool val);
COMMON(status)	compileRegex(Regex re, Bool optimize);
COMMON(status)	search_regex(Regex re, char *str1, int size1, char *str2, int size2, int start, int end);
COMMON(status)	searchRegex(Regex re, Any obj, Int start, Int end);
COMMON(Int)	getMatchRegex(Regex re, Any obj, Int start, Int end);
COMMON(status)	matchRegex(Regex re, Any obj, Int start, Int end);
COMMON(Int)	getRegisterEndRegex(Regex re, Int which);
COMMON(status)	makeClassRegex(Class class);

/* /staff/jan/src/pl/packages/xpce/src/txt/str.c */
COMMON(int)	str_allocsize(String s);
COMMON(void)	str_pad(String s);
COMMON(void)	str_alloc(String s);
COMMON(void)	str_unalloc(String s);
COMMON(String)	str_init(String s, String proto, char8 *data);
COMMON(status)	str_set_n_ascii(String str, int len, char *text);
COMMON(status)	str_set_ascii(String str, char *text);
COMMON(status)	str_set_static(String str, const char *text);
COMMON(void)	str_ncpy(String dest, int at, String src, int from, int len);
COMMON(void)	str_cpy(String dest, String src);
COMMON(char8 *)	str_textp(String s, int i);
COMMON(void)	str_upcase(String str, int from, int to);
COMMON(void)	str_downcase(String str, int from, int to);
COMMON(int)	str_cmp(String s1, String s2);
COMMON(int)	str_icase_cmp(String s1, String s2);
COMMON(int)	str_eq(String s1, String s2);
COMMON(int)	str_icase_eq(String s1, String s2);
COMMON(int)	str_prefix_offset(String s1, unsigned int offset, String s2);
COMMON(int)	str_prefix(String s1, String s2);
COMMON(int)	str_icase_prefix(String s1, String s2);
COMMON(int)	str_suffix(String s1, String s2);
COMMON(int)	str_icase_suffix(String s1, String s2);
COMMON(int)	str_sub(String s1, String s2);
COMMON(int)	str_icasesub(String s1, String s2);
COMMON(int)	str_next_index(String s, int from, wchar chr);
COMMON(int)	str_next_rindex(String s, int from, wchar chr);
COMMON(int)	str_index(String s, wchar chr);
COMMON(int)	str_rindex(String s, wchar chr);
COMMON(int)	str_count_chr(String s, int from, int to, wchar chr);
COMMON(int)	str_lineno(String s, int at);
COMMON(wchar)	str_fetch(String s, int idx);
COMMON(int)	str_store(String s, int idx, unsigned int chr);
COMMON(String)	str_nl(String proto);
COMMON(String)	str_spc(String proto);
COMMON(String)	str_tab(String proto);
COMMON(void)	str_strip(String s);
COMMON(int)	str_common_length(String s1, String s2);
COMMON(int)	str_icase_common_length(String s1, String s2);

/* /staff/jan/src/pl/packages/xpce/src/txt/string.c */
COMMON(StringObj) StringToString(String s);
COMMON(StringObj) CtoString(const char *s);
COMMON(StringObj) staticCtoString(const char *s);
COMMON(StringObj) CtoTempString(char *s);
COMMON(status)	initialiseStringv(StringObj str, CharArray fmt, int argc, Any *argv);
COMMON(status)	valueString(StringObj s1, CharArray s2);
COMMON(status)	insertCharacterString(StringObj str, Int chr, Int where, Int times);
COMMON(status)	upcaseString(StringObj s);
COMMON(status)	deleteString(StringObj str, Int start, Int length);
COMMON(status)	insertString(StringObj s1, Int n, CharArray s2);
COMMON(status)	str_insert_string(StringObj str, Int where, String s);
COMMON(StringObj) getSubString(StringObj n, Int start, Int end);
COMMON(status)	makeClassString(Class class);

/* /staff/jan/src/pl/packages/xpce/src/txt/style.c */
COMMON(status)	boldStyle(Style s, Bool on);
COMMON(status)	makeClassStyle(Class class);

/* /staff/jan/src/pl/packages/xpce/src/txt/syntax.c */
COMMON(status)	makeClassSyntaxTable(Class class);

/* /staff/jan/src/pl/packages/xpce/src/txt/textbuffer.c */
COMMON(status)	changedTextBuffer(TextBuffer tb);
COMMON(status)	ChangedRegionTextBuffer(TextBuffer tb, Int start, Int end);
COMMON(status)	ChangedFragmentListTextBuffer(TextBuffer tb);
COMMON(status)	clearTextBuffer(TextBuffer tb);
COMMON(status)	insertFileTextBuffer(TextBuffer tb, Int where, SourceSink file, Int times);
COMMON(status)	insertTextBuffer(TextBuffer tb, Int where, CharArray ca, Int times);
COMMON(status)	appendTextBuffer(TextBuffer tb, CharArray ca, Int times);
COMMON(status)	deleteTextBuffer(TextBuffer tb, Int where, Int times);
COMMON(status)	saveTextBuffer(TextBuffer tb, SourceSink file, Int from, Int len);
COMMON(status)	CmodifiedTextBuffer(TextBuffer tb, Bool val);
COMMON(status)	characterTextBuffer(TextBuffer tb, Int where, Int c);
COMMON(status)	transposeTextBuffer(TextBuffer tb, Int f1, Int t1, Int f2, Int t2);
COMMON(status)	downcaseTextBuffer(TextBuffer tb, Int from, Int len);
COMMON(status)	upcaseTextBuffer(TextBuffer tb, Int from, Int len);
COMMON(status)	capitaliseTextBuffer(TextBuffer tb, Int from, Int len);
COMMON(Int)	getScanTextBuffer(TextBuffer tb, Int from, Name unit, Int amount, Name az);
COMMON(StringObj) getContentsTextBuffer(TextBuffer tb, Int from, Int length);
COMMON(status)	parsep_line_textbuffer(TextBuffer tb, int here);
COMMON(int)	scan_textbuffer(TextBuffer tb, int from, Name unit, int amount, int az);
COMMON(Int)	getMatchingBracketTextBuffer(TextBuffer tb, Int idx, Int bracket);
COMMON(Int)	getSkipBlanksTextBuffer(TextBuffer tb, Int where, Name direction, Bool skipnl);
COMMON(Int)	getLineNumberTextBuffer(TextBuffer tb, Int i);
COMMON(int)	find_textbuffer(TextBuffer tb, int here, String str, int times, char az, int ec, int wm);
COMMON(long)	fill_line_textbuffer(TextBuffer tb, long int here, long int to, int sc, int rm, int justify);
COMMON(status)	sortTextBuffer(TextBuffer tb, Int from, Int to);
COMMON(int)	count_lines_textbuffer(TextBuffer tb, int f, int t);
COMMON(int)	start_of_line_n_textbuffer(TextBuffer tb, int lineno);
COMMON(int)	fetch_textbuffer(TextBuffer tb, int where);
COMMON(status)	change_textbuffer(TextBuffer tb, int where, void *s, int len);
COMMON(status)	str_sub_text_buffer(TextBuffer tb, String s, int start, int len);
COMMON(status)	insert_textbuffer(TextBuffer tb, int where, int times, String s);
COMMON(status)	delete_textbuffer(TextBuffer tb, int where, int length);
COMMON(status)	makeClassTextBuffer(Class class);

/* /staff/jan/src/pl/packages/xpce/src/txt/textcursor.c */
COMMON(status)	setTextCursor(TextCursor c, Int x, Int y, Int w, Int h, Int b);
COMMON(status)	makeClassTextCursor(Class class);

/* /staff/jan/src/pl/packages/xpce/src/txt/textimage.c */
COMMON(status)	InsertTextImage(TextImage ti, Int where, Int amount);
COMMON(status)	ChangedRegionTextImage(TextImage ti, Int from, Int to);
COMMON(status)	ChangedEntireTextImage(TextImage ti);
COMMON(status)	get_character_box_textimage(TextImage ti, int index, int *x, int *y, int *w, int *h, int *b);
COMMON(Int)	getLinesTextImage(TextImage ti);
COMMON(Int)	getIndexTextImage(TextImage ti, EventObj ev);
COMMON(status)	computeTextImage(TextImage ti);
COMMON(status)	startTextImage(TextImage ti, Int start, Int skip);
COMMON(status)	centerTextImage(TextImage ti, Int position, Int screen_line);
COMMON(Int)	getStartTextImage(TextImage ti, Int line);
COMMON(status)	backgroundTextImage(TextImage ti, Any bg);
COMMON(status)	tabDistanceTextImage(TextImage ti, Int tab);
COMMON(status)	tabStopsTextImage(TextImage ti, Vector v);
COMMON(Int)	getViewTextImage(TextImage ti);
COMMON(status)	bubbleScrollBarTextImage(TextImage ti, ScrollBar sb);
COMMON(Int)	getScrollStartTextImage(TextImage ti, Name dir, Name unit, Int amount);
COMMON(Int)	getUpDownColumnTextImage(TextImage ti, Int here);
COMMON(Int)	getUpDownCursorTextImage(TextImage ti, Int here, Int updown, Int column);
COMMON(Int)	getBeginningOfLineCursorTextImage(TextImage ti, Int here);
COMMON(Int)	getEndOfLineCursorTextImage(TextImage ti, Int here);
COMMON(status)	ensureVisibleTextImage(TextImage ti, Int caret);
COMMON(status)	makeClassTextImage(Class class);

/* /staff/jan/src/pl/packages/xpce/src/txt/textmargin.c */
COMMON(status)	makeClassTextMargin(Class class);

/* /staff/jan/src/pl/packages/xpce/src/txt/undo.c */
COMMON(void)	destroyUndoBuffer(UndoBuffer ub);
COMMON(Int)	getUndoTextBuffer(TextBuffer tb);
COMMON(status)	undoTextBuffer(TextBuffer tb);
COMMON(status)	undoBufferSizeTextBuffer(TextBuffer tb, Int size);
COMMON(status)	markUndoTextBuffer(TextBuffer tb);
COMMON(status)	resetUndoTextBuffer(TextBuffer tb);
COMMON(status)	checkpointUndoTextBuffer(TextBuffer tb);
COMMON(void)	register_insert_textbuffer(TextBuffer tb, long int where, long int len);
COMMON(void)	register_delete_textbuffer(TextBuffer tb, long where, long len);
COMMON(void)	register_change_textbuffer(TextBuffer tb, long int where, long int len);
