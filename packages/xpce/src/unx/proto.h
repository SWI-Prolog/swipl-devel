#define COMMON(type) SO_LOCAL type

/* /home/jan/src/pl/packages/xpce/src/unx/directory.c */
COMMON(status)	cdDirectory(Directory d);
COMMON(status)	makeClassDirectory(Class class);
COMMON(char *)	dirName(const char *f, char *dir, size_t dirlen);
COMMON(char *)	baseName(const char *f);
COMMON(int)	isAbsolutePath(const char *p);
COMMON(int)	absolutePath(const char *file, char *path, size_t buflen);
COMMON(int)	expandFileNameW(const wchar_t *pattern, wchar_t *bin, size_t binlen);

/* /home/jan/src/pl/packages/xpce/src/unx/file.c */
COMMON(Name)	expandFileName(Name in);
COMMON(Name)	getOsNameFile(FileObj f);
COMMON(status)	closeFile(FileObj f);
COMMON(status)	existsFile(FileObj f, Bool mustbefile);
COMMON(status)	sameOsPath(const char *s1, const char *s2);
COMMON(Name)	getAbsolutePathFile(FileObj f);
COMMON(status)	isAbsoluteFile(FileObj f);
COMMON(status)	doBOMFile(FileObj f);
COMMON(status)	openFile(FileObj f, Name mode, Name filter, CharArray extension);
COMMON(status)	removeFile(FileObj f);
COMMON(Name)	getBaseNameFile(FileObj f);
COMMON(status)	reportErrorFile(FileObj f);
COMMON(status)	checkErrorFile(FileObj f);
COMMON(status)	storeCharFile(FileObj f, int c);
COMMON(void)	putstdw(unsigned long w, IOSTREAM *fd);
COMMON(status)	storeWordFile(FileObj f, Any w);
COMMON(status)	storeDoubleFile(FileObj file, double f);
COMMON(status)	storeStringFile(FileObj f, String s);
COMMON(status)	storeNameFile(FileObj f, Name n);
COMMON(status)	storeIntFile(FileObj f, Int i);
COMMON(status)	findFile(FileObj f, CharArray path, Name mode);
COMMON(status)	makeClassFile(Class class);

/* /home/jan/src/pl/packages/xpce/src/unx/process.c */
COMMON(void)	killAllProcesses(int status);
COMMON(status)	pidProcess(Process p, Int pid);
COMMON(status)	closeInputProcess(Process p);
COMMON(status)	makeClassProcess(Class class);

/* /home/jan/src/pl/packages/xpce/src/unx/socket.c */
COMMON(Name)	SockError(void);
COMMON(status)	acceptSocket(Socket s);
COMMON(status)	makeClassSocket(Class class);

/* /home/jan/src/pl/packages/xpce/src/unx/stream.c */
COMMON(status)	initialiseStream(Stream s, Int rfd, Int wfd, Code input, Any sep);
COMMON(status)	closeStream(Stream s);
COMMON(status)	closeInputStream(Stream s);
COMMON(status)	closeOutputStream(Stream s);
COMMON(status)	inputStream(Stream s, Int fd);
COMMON(void)	add_data_stream(Stream s, char *data, int len);
COMMON(status)	handleInputStream(Stream s);
COMMON(status)	makeClassStream(Class class);
COMMON(status)	makeClassStream(Class class);
