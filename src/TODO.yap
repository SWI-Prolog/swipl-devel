TODO for integration of YAP and SWI I/O systems

Conclusions:

    * Make Prolog I/O library from
	- pl-stream.c
	- pl-file.c
	- pl-utf8.c
	- pl-text.c (minus conversion text<->Prolog)
	- pl-option.c
	- pl-error.c

    * Take some bits from pl-os.c
	- initEncoding()

    * Needs state structures for engine and global state
    	- IO_local_data
	- IO_global_data

TODO:

    * Abstract some interfaces
	- read_pending_input/3
    * Define generic interfaces
	- Memory allocation
	- Hash-table management (just renaming?)
    * Cleanup foreign flag interface (naming)

