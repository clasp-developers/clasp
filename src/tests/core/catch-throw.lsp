(print (catch 'a
	   (apply #'(lambda () (throw 'a "Test")) nil )))
