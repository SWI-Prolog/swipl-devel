
/* ../src/txt/chararray.c */
status		initialiseCharArray(CharArray n, CharArray value);
Name		getValueCharArray(CharArray n);
status		equalCharArray(CharArray n1, CharArray n2);
status		prefixCharArray(CharArray n1, CharArray n2, Bool ign_case);
status		suffixCharArray(CharArray n, CharArray s, Bool ign_case);
CharArray	getCopyCharArray(CharArray n);
CharArray	getCapitaliseCharArray(CharArray n);
CharArray	getLabelNameCharArray(CharArray n);
CharArray	getDowncaseCharArray(CharArray n);
CharArray	getAppendCharArray(CharArray n1, CharArray n2);
CharArray	getDeleteSuffixCharArray(CharArray n, CharArray s);
CharArray	getEnsureSuffixCharArray(CharArray n, CharArray s);
Int		getSizeCharArray(Any n);
void		initCharArrays(void);
CharArray	CtoScratchCharArray(const char *s);
CharArray	StringToScratchCharArray(const String s);
void		doneScratchCharArray(CharArray n);
CharArray	CtoCharArray(char *s);
status		makeClassCharArray(Class class);

/* ../src/txt/editor.c */
status		normaliseEditor(Editor e, Int start, Int end);
Any		ReceiverOfEditor(Editor e);
status		forwardModifiedEditor(Editor e, Bool val);
status		scrollToEditor(Editor e, Int pos);
status		selectionEditor(Editor e, Int from, Int to);
Point		getSelectionEditor(Editor e);
StringObj	getSelectedEditor(Editor e);
status		formatEditor(Editor e, CharArray fmt, int argc, Any *argv);
status		clearEditor(Editor e);
status		makeClassEditor(Class class);

/* ../src/txt/fragment.c */
status		makeClassFragment(Class class);

/* ../src/txt/keybinding.c */
Any		getFunctionKeyBinding(KeyBinding kb, EventId id);
status		functionKeyBinding(KeyBinding kb, EventId id, Any f);
status		typedKeyBinding(KeyBinding kb, EventId id, Graphical receiver);
status		makeClassKeyBinding(Class class);
KeyBinding	KeyBindingText(void);
KeyBinding	KeyBindingTextItem(void);
KeyBinding	KeyBindingTextItemView(void);

/* ../src/txt/regex.c */
status		ignoreCaseRegex(Regex re, Bool val);
status		compileRegex(Regex re, Bool optimize);
status		search_regex(Regex re, char *str1, int size1, char *str2, int size2, int start, int end);
status		searchRegex(Regex re, Any obj, Int start, Int end);
Int		getMatchRegex(Regex re, Any obj, Int start, Int end);
status		matchRegex(Regex re, Any obj, Int start, Int end);
Int		getRegisterEndRegex(Regex re, Int which);
status		makeClassRegex(Class class);

/* ../src/txt/str.c */
int		str_allocsize(String s);
void		str_pad(String s);
void		str_alloc(String s);
void		str_unalloc(String s);
String		str_init(String s, String proto, char8 *data);
void		str_set_ascii(String str, char *text);
void		str_set_n_ascii(String str, int len, char *text);
void		str_set_static(String str, const char *text);
void		str_ncpy(String dest, int at, String src, int from, int len);
void		str_cpy(String dest, String src);
char8 *		str_textp(String s, int i);
void		str_upcase(String str, int from, int to);
void		str_downcase(String str, int from, int to);
int		str_cmp(String s1, String s2);
int		str_icase_cmp(String s1, String s2);
int		str_eq(String s1, String s2);
int		str_icase_eq(String s1, String s2);
int		str_prefix_offset(String s1, unsigned int offset, String s2);
int		str_prefix(String s1, String s2);
int		str_icase_prefix(String s1, String s2);
int		str_suffix(String s1, String s2);
int		str_icase_suffix(String s1, String s2);
int		str_sub(String s1, String s2);
int		str_icasesub(String s1, String s2);
int		str_next_index(String s, int from, wchar chr);
int		str_next_rindex(String s, int from, wchar chr);
int		str_index(String s, wchar chr);
int		str_rindex(String s, wchar chr);
int		str_count_chr(String s, int from, int to, wchar chr);
int		str_lineno(String s, int at);
int		str_fetch(String s, int idx);
int		str_store(String s, int idx, unsigned int chr);
String		str_nl(String proto);
String		str_spc(String proto);
String		str_tab(String proto);
void		str_strip(String s);
int		str_common_length(String s1, String s2);
int		str_icase_common_length(String s1, String s2);

/* ../src/txt/string.c */
StringObj	StringToString(String s);
StringObj	CtoString(const char *s);
StringObj	staticCtoString(const char *s);
StringObj	CtoTempString(char *s);
status		initialiseStringv(StringObj str, CharArray fmt, int argc, Any *argv);
status		valueString(StringObj s1, CharArray s2);
status		insertCharacterString(StringObj str, Int chr, Int where, Int times);
status		upcaseString(StringObj s);
status		deleteString(StringObj str, Int start, Int length);
status		insertString(StringObj s1, Int n, CharArray s2);
status		str_insert_string(StringObj str, Int where, String s);
StringObj	getSubString(StringObj n, Int start, Int end);
status		makeClassString(Class class);

/* ../src/txt/style.c */
status		boldStyle(Style s, Bool on);
status		makeClassStyle(Class class);

/* ../src/txt/syntax.c */
status		makeClassSyntaxTable(Class class);

/* ../src/txt/textbuffer.c */
status		changedTextBuffer(TextBuffer tb);
status		ChangedRegionTextBuffer(TextBuffer tb, Int start, Int end);
status		ChangedFragmentListTextBuffer(TextBuffer tb);
status		clearTextBuffer(TextBuffer tb);
status		insertFileTextBuffer(TextBuffer tb, Int where, SourceSink file, Int times);
status		insertTextBuffer(TextBuffer tb, Int where, CharArray ca, Int times);
status		appendTextBuffer(TextBuffer tb, CharArray ca, Int times);
status		deleteTextBuffer(TextBuffer tb, Int where, Int times);
status		saveTextBuffer(TextBuffer tb, SourceSink file, Int from, Int len);
status		CmodifiedTextBuffer(TextBuffer tb, Bool val);
status		characterTextBuffer(TextBuffer tb, Int where, Int c);
status		transposeTextBuffer(TextBuffer tb, Int f1, Int t1, Int f2, Int t2);
status		downcaseTextBuffer(TextBuffer tb, Int from, Int len);
status		upcaseTextBuffer(TextBuffer tb, Int from, Int len);
status		capitaliseTextBuffer(TextBuffer tb, Int from, Int len);
Int		getScanTextBuffer(TextBuffer tb, Int from, Name unit, Int amount, Name az);
StringObj	getContentsTextBuffer(TextBuffer tb, Int from, Int length);
status		parsep_line_textbuffer(TextBuffer tb, int here);
int		scan_textbuffer(TextBuffer tb, int from, Name unit, int amount, int az);
Int		getMatchingBracketTextBuffer(TextBuffer tb, Int idx, Int bracket);
Int		getSkipBlanksTextBuffer(TextBuffer tb, Int where, Name direction, Bool skipnl);
Int		getLineNumberTextBuffer(TextBuffer tb, Int i);
int		find_textbuffer(TextBuffer tb, int here, String str, int times, char az, int ec, int wm);
long		fill_line_textbuffer(TextBuffer tb, long int here, long int to, int sc, int rm, int justify);
status		sortTextBuffer(TextBuffer tb, Int from, Int to);
int		count_lines_textbuffer(TextBuffer tb, int f, int t);
int		start_of_line_n_textbuffer(TextBuffer tb, int lineno);
int		fetch_textbuffer(TextBuffer tb, int where);
status		change_textbuffer(TextBuffer tb, int where, void *s, int len);
status		str_sub_text_buffer(TextBuffer tb, String s, int start, int len);
status		insert_textbuffer(TextBuffer tb, int where, int times, String s);
status		delete_textbuffer(TextBuffer tb, int where, int length);
status		makeClassTextBuffer(Class class);

/* ../src/txt/textcursor.c */
status		setTextCursor(TextCursor c, Int x, Int y, Int w, Int h, Int b);
status		makeClassTextCursor(Class class);

/* ../src/txt/textimage.c */
status		InsertTextImage(TextImage ti, Int where, Int amount);
status		ChangedRegionTextImage(TextImage ti, Int from, Int to);
status		ChangedEntireTextImage(TextImage ti);
status		get_character_box_textimage(TextImage ti, int index, int *x, int *y, int *w, int *h, int *b);
Int		getLinesTextImage(TextImage ti);
Int		getIndexTextImage(TextImage ti, EventObj ev);
status		computeTextImage(TextImage ti);
status		startTextImage(TextImage ti, Int start, Int skip);
status		centerTextImage(TextImage ti, Int position, Int screen_line);
Int		getStartTextImage(TextImage ti, Int line);
status		tabDistanceTextImage(TextImage ti, Int tab);
status		tabStopsTextImage(TextImage ti, Vector v);
Int		getViewTextImage(TextImage ti);
status		bubbleScrollBarTextImage(TextImage ti, ScrollBar sb);
Int		getScrollStartTextImage(TextImage ti, Name dir, Name unit, Int amount);
Int		getUpDownColumnTextImage(TextImage ti, Int here);
Int		getUpDownCursorTextImage(TextImage ti, Int here, Int updown, Int column);
Int		getBeginningOfLineCursorTextImage(TextImage ti, Int here);
Int		getEndOfLineCursorTextImage(TextImage ti, Int here);
status		ensureVisibleTextImage(TextImage ti, Int caret);
status		makeClassTextImage(Class class);

/* ../src/txt/textmargin.c */
status		makeClassTextMargin(Class class);

/* ../src/txt/undo.c */
void		destroyUndoBuffer(UndoBuffer ub);
Int		getUndoTextBuffer(TextBuffer tb);
status		undoTextBuffer(TextBuffer tb);
status		undoBufferSizeTextBuffer(TextBuffer tb, Int size);
status		markUndoTextBuffer(TextBuffer tb);
status		resetUndoTextBuffer(TextBuffer tb);
status		checkpointUndoTextBuffer(TextBuffer tb);
void		register_insert_textbuffer(TextBuffer tb, long int where, long int len);
void		register_delete_textbuffer(TextBuffer tb, long where, long len);
void		register_change_textbuffer(TextBuffer tb, long int where, long int len);
