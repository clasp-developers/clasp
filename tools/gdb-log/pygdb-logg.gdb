# gdb script: pygdb-logg.gdb
# easier interface for pygdb-logg.py stuff
# from within gdb: (gdb) source -v pygdb-logg.gdb
# from cdmline: gdb -x pygdb-logg.gdb -se test.exe

# first, "include" the python file:
source -v pygdb-logg.py

# define shorthand for nextUntilBreakpoint():
define nub
  python nextUntilBreakpoint()
end

# set up breakpoints for test.exe:
b main
b doFunction

# go to main breakpoint
run
