#
# This is a little helper to use dmalloc within gdb, use it via
# "source dmalloc.gdb"
#
# It will provide a command dmalloc, which will ask the user for
# arguments to provide to the dmalloc application.  The output will be 
# in gdb format and will be parsed by gdb.  
#
# Its main purpose is to generate breaks at allocations of unfreed
# memory (via dmalloc -a ...).
#
# As gdb's language is very basic, there is no back substitution and
# variable arguments to user defined commands.  Therefore the user
# defined command dmalloc will use a shell escape to ask the user for
# the arguments to the dmalloc applications.  The dmalloc applications 
# will execute with the provided arguments and write the output to a
# temporary file.  This temporary file will then be read by gdb and
# deleted afterwards.
#
# November 10, 1998 - Jens Krinke
#
define dmalloc
  echo Enter dmalloc options: 
  shell read arg; dmalloc -g $arg > /tmp/dmalloc-gdb
  source /tmp/dmalloc-gdb
  shell rm -f /tmp/dmalloc-gdb
  show env DMALLOC_OPTIONS
  # the following will not work if no symboltable is loaded, but that
  # doesn't matter.
  break dmalloc_error
end
