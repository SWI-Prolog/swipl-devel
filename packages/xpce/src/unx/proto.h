
/* ../src/unx/directory.c */
status		cdDirectory(Directory d);
status		makeClassDirectory(Class class);
char *		dirName(const char *f);
char *		baseName(const char *f);
int		isAbsolutePath(const char *p);
char *		absolutePath(char *file);
char *		expandFileName(char *pattern, char *bin);

/* ../src/unx/file.c */
Name		getOsNameFile(FileObj f);
status		closeFile(FileObj f);
status		existsFile(FileObj f, Bool mustbefile);
Name		getAbsolutePathFile(FileObj f);
status		isAbsoluteFile(FileObj f);
status		backup_name(char *old, char *ext, char *bak);
status		backup_name(char *old, char *ext, char *bak);
status		openFile(FileObj f, Name mode, Name filter, CharArray extension);
status		removeFile(FileObj f);
Name		getBaseNameFile(FileObj f);
status		reportErrorFile(FileObj f);
status		checkErrorFile(FileObj f);
status		storeCharFile(FileObj f, int c);
void		putstdw(unsigned long w, FILE *fd);
status		storeWordFile(FileObj f, Any w);
status		storeCharpFile(FileObj f, char *s);
status		storeNameFile(FileObj f, Name n);
status		storeIntFile(FileObj f, Int i);
status		findFile(FileObj f, CharArray path, Name mode);
status		makeClassFile(Class class);

/* ../src/unx/process.c */
void		killAllProcesses(int status);
status		pidProcess(Process p, Int pid);
status		closeInputProcess(Process p);
status		makeClassProcess(Class class);

/* ../src/unx/socket.c */
Name		SockError(void);
status		acceptSocket(Socket s);
status		makeClassSocket(Class class);

/* ../src/unx/stream.c */
status		initialiseStream(Stream s, Int rfd, Int wfd, Code input, Any sep);
status		closeStream(Stream s);
status		closeInputStream(Stream s);
status		closeOutputStream(Stream s);
status		inputStream(Stream s, Int fd);
void		add_data_stream(Stream s, char *data, int len);
status		handleInputStream(Stream s);
status		makeClassStream(Class class);
status		makeClassStream(Class class);
