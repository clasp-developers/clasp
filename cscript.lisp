(k:recurse #P"include/"
           #P"src/")

(k:includes #~"")

(k:library "fmt" :required t :min-version "7.1.0")

(k:library "gmpxx" :required t :min-version "6.0.0")

#-darwin (k:library "libelf" :required t :min-version #+bsd "0.8.13" #-bsd ".183")
