# WATCOMC Port of readline
# Assuming wmake --- WATCOM make

.NOCHECK

CC=wcl386 /zq

xcnvcurs:	xcnvcurs.c
		$(CC) xcnvcurs.c

xcursors:	cursor.fon xcnvcurs
		./xcnvcurs cursor.fon > xcursors

test:		xcursor.c
		$(CC) -DTEST xcursor.c
