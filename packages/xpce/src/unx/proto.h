
/* unx/directory.c */
status		cdDirectory(Directory d);
status		makeClassDirectory(Class class);
char *		dirName(char *f);
char *		baseName(register char *f);
int		isAbsolutePath(const char *p);
char *		absolutePath(char *file);
char *		expandFileName(char *pattern);

/* unx/file.c */
Name		getOsNameFile(FileObj f);
status		closeFile(FileObj f);
status		existsFile(FileObj f, Bool mustbefile);
Name		getAbsolutePathFile(FileObj f);
status		backup_name(char *old, char *ext, char *bak);
status		backup_name(char *old, char *ext, char *bak);
status		backupFile(FileObj f, Name ext);
status		openFile(FileObj f, Name mode, Name filter, CharArray extension);
status		removeFile(FileObj f);
Int		getSizeFile(FileObj f);
Name		getBaseNameFile(FileObj f);
status		reportErrorFile(FileObj f);
status		checkErrorFile(FileObj f);
status		storeCharFile(FileObj f, int c);
void		putstdw(ulong w, FILE *fd);
status		storeWordFile(FileObj f, Any w);
status		storeCharpFile(FileObj f, char *s);
status		storeNameFile(FileObj f, Name n);
status		storeIntFile(FileObj f, Int i);
status		findFile(FileObj f, CharArray path, Name mode);
status		checkObjectFile(FileObj f);
status		makeClassFile(Class class);

/* unx/process.c */
void		killAllProcesses(void);
status		pidProcess(Process p, Int pid);
status		makeClassProcess(Class class);

/* unx/socket.c */
Name		SockError(void);
status		acceptSocket(Socket s);
status		makeClassSocket(Class class);

/* unx/stream.c */
status		initialiseStream(Stream s, Int rfd, Int wfd, Code input, Regex sep);
status		closeInputStream(Stream s);
status		closeOutputStream(Stream s);
status		inputStream(Stream s, Int fd);
status		handleInputStream(Stream s);
status		makeClassStream(Class class);
status		makeClassStream(Class class);
