; fixed code
(defun describeRender  (o   ) ;END1
(setq controller  (createRenderController   o  ) )

(describe   controller  )

(setq rend  (getRender   controller  ) )

(check   rend  )
)

(global *fileNames*  (cdr   l:*ARGS*  ) )

(ifTrue (isTopLevelScript   ) ;END1
(let (obj   ) ;END1
(foreach fn  *fileNames*   ;END1
(setq obj  (loadArchive  fn   ) )

(describeRender obj   )
)
)
)
