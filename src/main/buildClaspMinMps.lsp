(core:bformat t "Builder clasp-min\n")
(core:clean-system nil :no-prompt t)
(core:copy-system :start :min :from-target-backend "min-boehm" :to-target-backend "min-mps")
(core:compile-min-system)
(core:quit)

