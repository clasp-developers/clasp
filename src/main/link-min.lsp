(select-package :core)
(load-system :start :min)
(link-system :init :min (default-prologue-form) +minimal-epilogue-form+)
(quit)
