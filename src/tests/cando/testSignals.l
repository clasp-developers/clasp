
(defClass AModel () (_a))

(defMethod seta AModel (self z)
  (setq (slot self _a) z)
  (signal self :changed_a))

(defClass Observer () ())

(defMethod __catchSignal__ Observer (self signal sender)
  (print (% "Caught the signal[%s] from sender[%s]" signal (repr sender))))

(print (dumpMethods Observer))

(defvar *m* (AModel))
(defvar *o* (Observer))


(connectListener *m* *o* :changed_a)


(print "About to call seta")
(seta *m* 34)
