This directory contains extensions to GDB and LLDB to assist in debugging Clasp at a low level. A variety of commands for examining Lisp objects and state are provided. These are described in more detail once you start the extension.

The GDB extension is compatible with the UDB time-travelling debugger. Everything in this README that mentions GDB also works with UDB.

# Setup

To load the extension, you need to load `gdb-loader.py` or `lldb-loader.py` from within your debugger. You can do this manually with the `source` command in GDB, or `command script import` in LLDB.

The recommended way to load the extension is to set it up to auto load when you start debugging Clasp. This is possible for GDB (and UDB) but it is not clear if it can be done with LLDB. GDB documents its auto loading [here](https://sourceware.org/gdb/current/onlinedocs/gdb.html/objfile_002dgdbdotext-file.html).

Basically, all you need to do is put a symlink from GDB's auto load directory to `gdb-loader.py`, named as your Clasp binary. For example, if you have clasp installed at `/usr/local/bin/iclasp`, and your GDB is set up as usual, you could run

```bash
ln -s /path/to/gdb/loader.py /usr/share/gdb/auto-load/usr/local/bin/iclasp-gdb.py
```

and then the distinction will load whenever you start debugging clasp. Note that the actual executable is `iclasp` here rather than `clasp`, which is a symlink.

If that doesn't work, you can source the extension in your initfile. Some examples are provided in `dot-files/`. Keep in mind that this will load the extension any time you run your debugger, even if you're debugging something other than Clasp.

# Caveats

In order for the debugger to get much information from Clasp, Clasp has to have actually initialized a few variables read by the extension. If you start your debugger with an inferior Clasp rather than attaching to an existing process, this may not immediately be done.

Once Clasp is running, you can reload this information with the `lreload` command in the extension. This is done automatically for extension commands but not for built in commands, so for example a backtrace from `bt` will not be able to use this information until you execute an extension command.
