:- pce_begin_class(master_slave_hyper, hyper,
		   "Maintain a master-slave relationship").

initialise(H, F:object, T:object, FN:[name], TN:[name]) :->
	default(FN, slave, FromName),
	default(TN, master, ToName),
	send(H, send_super, initialise, F, T, FromName, ToName).

unlink_from(H) :->
	"From-side (master is destroyed"::
	ignore(send(H?to, free)),
	free(H).

:- pce_end_class.

hyperdemo :-
	new(F, frame('Main Window')),
	send(F, append, new(P, picture)),
	send(new(D, dialog), below, P),
	send(D, append, button(quit, message(F, destroy))),
	send(F, open),

	new(E, dialog('Edit label')),
	send(E, append, text_item(label, F?label,
				  message(F, label, @arg1))),
	send(E, append, button(cancel, message(E, destroy))),
	new(_, master_slave_hyper(F, E)),
	send(E, open).
