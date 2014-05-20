; fixed code
(require "CANDO/db/jobSetup.l"   )

(defun clusterFinderForFragmentCoordinates  (fragmentCoordinates   ) ;END1
(let (clusterFinder  coords   ) ;END1
(setq clusterFinder  (ClusterFinder  fragmentCoordinates   ) )

(setq coords  (coordinateArraysAsCons   fragmentCoordinates  ) )

(foreach c  coords   ;END1
(addCoordinateArray   clusterFinder   c  () )
)

(return clusterFinder   )
)
)

(defun describeClusters  (clusterFinder   ) ;END1
(let (cls  singleMemberClusters   ) ;END1
(setq cls  (clustersAsCons   clusterFinder  ) )

(println (%   "Clusters %d ===>"   (length   cls  ) ) )

(setq singleMemberClusters  0   )

(foreach c  cls   ;END1
(if (>   (clusterSize   c  ) 1   ) ;END1
(then ;END1
(println (%   "Cluster contains %d entries: has radius %lf"   (clusterSize   c  )(clusterRadius   c  ) ) )
)

(else ;END1
(setq singleMemberClusters  (+   singleMemberClusters   1   ) )
)
)
)

(ifTrue (>   singleMemberClusters   0   ) ;END1
(println (%   "There were %d clusters with one member"   singleMemberClusters   ) )
)
)
)

(defun renderClusters  (clusterFinder  fragmentName  monomerCoordinates   ) ;END1
(let (allDl  (renderWithBonds   clusterFinder   monomerCoordinates   ) ) ;END1
(return allDl   )
)
)

(defun simplifyFragmentCoordinates  (fragmentCoordinates  clusterFinder   ) ;END1
(let (fragmentName  clusters  conformation  coords   ) ;END1
(setq fragmentName  (getFragmentName   fragmentCoordinates  ) )

(ifTrue (!=   fragmentName   "body"   ) ;END1
(setq clusters  (clustersAsCons   clusterFinder  ) )

(clearCoordinateArrays   fragmentCoordinates  )

(foreach cluster  clusters   ;END1
(setq conformation  (entryClosestToCenter   cluster  ) )

(setq coords  (getCoordinateArray   conformation  ) )

(addCoordinateArray   fragmentCoordinates   coords   )
)
)
)
)

(defun simplifyMonomerCoordinates  (&key  (saveClusters  true   ) ) ;END1
(let (allMonomerCoordinates  count  total  parts  fileName  fragmentCoordinates  contextString  frameDl  num  clusters  dl  filePath  scaffoldList  numScaffolds   ) )

(setq allMonomerCoordinates  (uniqueMonomerCoordinatesAsCons   m:*DATABASE*  ) )

(createDirectory (Path  :path  "./clusters"   ) )

(setq count  0   )

(setq total  (length   allMonomerCoordinates  ) )

(foreach mc  allMonomerCoordinates   ;END1
(println  "======================================================="    )

(println  "======================================================="    )

(println (%   "mc = %s"   (getComment   mc  ) ) )

(println (%   "index = %d/%d"   count  total   ) )

(setq count  (+   count   1   ) )

(println  "-------------------------------------------------------"    )

(setq parts  (split   (getComment   mc  ) "_."   ) )

(setq fileName  (@   parts   2   ) )

(setq fragmentCoordinates  (fragmentCoordinatesAsCons   mc  ) )

(setq contextString  (getKey   (getContext   mc  )) )

(setq frameDl  (RenderFrameList   ) )

(setName   frameDl   "top"   )

(setSliderName   frameDl   "fragment"   )

(foreach fc  fragmentCoordinates   ;END1
(println (%   "fc = %s"   (getFragmentName   fc  ) ) )

(setq num  (numberOfCoordinateArrays   fc  ) )

(println (%   "    Number of coordinate arrays: %d"   num   ) )

(setq clusters  (clusterFinderForFragmentCoordinates  fc   ) )

(findClusters   clusters   0.2   )

(describeClusters clusters   )

(simplifyFragmentCoordinates fc  clusters   )

(when saveClusters   ;END1
(setq dl  (renderClusters  clusters  (getFragmentName   fc  )mc   ) )

(addGraphics   frameDl   dl   )
)
)

(when saveClusters   ;END1
(setq filePath  (%   "./clusters/%s.cxml"   fileName   ) )

(println (%   "Saving frames to %s"   filePath   ) )

(saveArchive frameDl  filePath   )
)

(setq scaffoldList  (getScaffoldList   mc  ) )

(setq numScaffolds  (numberOfScaffolds   scaffoldList  ) )

(println (%   "   Number of scaffolds = %d"   numScaffolds   ) )
)
)

(defun optimizeDatabase  (inputDatabase  outputDatabase  &key  (saveClusters  true   ) ) ;END1
(println (%   "Loading standardDatabase(%s)"   inputDatabase   ) )

(database inputDatabase   )

(println  "Optimizing database"    )

(simplifyMonomerCoordinates :saveClusters  saveClusters   )

(println (%   "Saving optimized database to: %s"   outputDatabase   ) )

(saveArchive m:*DATABASE*  outputDatabase   )
)

(global *unoptimizedName*  (unoptimizedDatabaseName   ) )

(global *fullName*  (fullDatabaseName   ) )

(optimizeDatabase *unoptimizedName*  *fullName*  :saveClusters  false   )
