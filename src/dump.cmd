echo save_program('%2.cmd', [ goal = '''$welcome''', toplevel = prolog]). halt. | %1 -f none -B
echo forall(library_directory(X), ((exists_directory(X) , write('Index for '), write(X), nl, make_library_index(X)) ; true)). |  %1
