/*  Part of XPCE

    This example code is in the public domain
*/

which(Name, File) :-
	get(@pce, environment_variable, 'PATH', Path),
	new(F, file(Name)),
	send(F, find, Path, execute),
	get(F, absolute_path, File).
