(block nil
  (let ((x 1))
    (catch 'test (throw 'test nil))
    )
  (print "Bottom of block")
  )
