(k:recurse #P"include/"
           #P"src/")

(k:includes #~"")

(k:library "gmpxx" :required t :min-version "6.0.0")

(k:library "libffi" :required t :min-version #+bsd "3.3-rc1" #-bsd "3.0.0")

(k:library "zlib" :required t :min-version "1.0.0")

#-darwin (k:library "libelf" :required t :min-version #+bsd "0.8.13" #-bsd ".183")

(k:library "ncurses" :required t :min-version "5.7.0")

#-bsd (k:library "libbsd" :required t :min-version "0.10.0")

