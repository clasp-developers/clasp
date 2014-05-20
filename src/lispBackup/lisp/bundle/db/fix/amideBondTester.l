; fixed code
(global *dkpSecondaryAmideSmarts*  (ChemInfo  :smarts  "[N&H]1C2(=O3)CNC(=O)C[N&H;?1]"   ) )

(let ((good  (compileSucceeded   *dkpSecondaryAmideSmarts*  ) ) ) ;END1
(ifTrue (not  good   ) ;END1
(raise "Failed to compile *dkpSecondaryAmideSmarts*"   )
)
)

(global *primaryAmideSmarts*  (ChemInfo  :smarts  "[N&H2]C(=O)C"   ) )

(let ((good  (compileSucceeded   *primaryAmideSmarts*  ) ) ) ;END1
(ifTrue (not  good   ) ;END1
(raise "Failed to compile *primaryAmideSmarts*"   )
)
)

(global *secondaryAmideSmarts*  (ChemInfo  :smarts  "[N&H1&R0]1(C)C2(=O3)C"   ) )

(let ((good  (compileSucceeded   *secondaryAmideSmarts*  ) ) ) ;END1
(ifTrue (not  good   ) ;END1
(raise "Failed to compile *secondaryAmideSmarts*"   )
)
)

(defClass AmideBondTester  ()(_DkpAmides  _AcyclicAmides  _Matter  _Messages   ) ;END1
(method __init__  (self  matter   ) ;END1
(=   (slot   self   _Matter   ) matter   )

(=   (slot   self   _DkpAmides   ) () )

(=   (slot   self   _AcyclicAmides   ) () )

(=   (slot   self   _Messages   ) (StringStream   ) )

(identifyAmideBonds   self  )

(return self   )
)

(method assembleAmide  (self  smarts   ) ;END1
(let ((match  (getMatch   smarts  ) )ah  ac  ao  amide  an   ) ;END1
(=   an   (tag   match   "1"   ) )

(foreach b  (bondsAsList   an  ) ;END1
(ifTrue (==   (getElement   (getTo   b  )) :H   ) ;END1
(=   ah   (getTo   b  ) )
)
)

(=   ac   (tag   match   "2"   ) )

(=   ao   (tag   match   "3"   ) )

(=   amide   (list  ah  an  ac  ao   ) )

(return amide   )
)
)

(method measureDihedral  (self  amide   ) ;END1
(let (ah  an  ac  ao  vh  vn  vc  vo  dih   ) ;END1
(=   ah   (@   amide   0   ) )

(=   an   (@   amide   1   ) )

(=   ac   (@   amide   2   ) )

(=   ao   (@   amide   3   ) )

(=   vh   (getPosition   ah  ) )

(=   vn   (getPosition   an  ) )

(=   vc   (getPosition   ac  ) )

(=   vo   (getPosition   ao  ) )

(=   dih   (/   (dihedral  vh  vn  vc  vo   ) 0.0174533   ) )

(log (%   "dihedral = %lf "   dih   ) )

(return dih   )
)
)

(method identifyAmideBonds  (self   ) ;END1
(let (amide  ns   ) ;END1
(=   (slot   self   _DkpAmides   ) () )

(=   (slot   self   _AcyclicAmides   ) () )

(=   ns   (allAtomsOfElementAsList   (slot   self   _Matter   ) :N   ) )

(foreach a  ns   ;END1
(ifTrue (matches   *primaryAmideSmarts*   a   ) ;END1
(continue )
)

(if (matches   *dkpSecondaryAmideSmarts*   a   ) ;END1
(then ;END1
(ifTrue (>   (getBondedHydrogenCount   a  ) 1   ) ;END1
(raise "We ignore primary amides but this nitrogen has two hydrogens"   )
)

(=   amide   (assembleAmide   self   *dkpSecondaryAmideSmarts*   ) )

(=   (slot   self   _DkpAmides   ) (cons  amide  (slot   self   _DkpAmides   ) ) )
)

(else ;END1
(ifTrue (matches   *secondaryAmideSmarts*   a   ) ;END1
(ifTrue (>   (getBondedHydrogenCount   a  ) 1   ) ;END1
(raise "We ignore primary amides but this nitrogen has two hydrogens"   )
)

(=   amide   (assembleAmide   self   *secondaryAmideSmarts*   ) )

(=   (slot   self   _AcyclicAmides   ) (cons  amide  (slot   self   _AcyclicAmides   ) ) )
)
)
)
)
)
)

(method countBadAmidesInMatter  (self  matter   ) ;END1
(let (badAmideCount  dih   ) ;END1
(clear   (slot   self   _Messages   ))

(=   badAmideCount   0   )

(foreach dkp  (slot   self   _DkpAmides   ) ;END1
(=   dih   (measureDihedral   self   dkp   ) )

(ifTrue (or   (>   dih   90.0   ) (<   dih   -90.0   ) ) ;END1
(log (%   "bad dkp secondary amide, H-N-C=O dihedral angle = %lf degrees"   dih   ) )

(writeln   (slot   self   _Messages   ) (%   "bad dkp secondary amide, H-N-C=O dihedral angle = %lf degrees"   dih   ) )

(=   badAmideCount   (+   badAmideCount   1   ) )
)
)

(foreach am  (slot   self   _AcyclicAmides   ) ;END1
(=   dih   (measureDihedral   self   am   ) )

(ifTrue (and   (>   dih   -90.0   ) (<   dih   90.0   ) ) ;END1
(log (%   "bad acyclic secondary amide, H-N-C=O dihedral angle = %lf degrees"   dih   ) )

(writeln   (slot   self   _Messages   ) (%   "bad acyclic secondary amide, H-N-C=O dihedral angle = %lf degrees"   dih   ) )

(=   badAmideCount   (+   badAmideCount   1   ) )
)
)

(return badAmideCount   )
)
)

(method getMessages  (self   ) ;END1
(return (str   (slot   self   _Messages   )) )
)

(method countBadAmidesInEntry  (self  entry   ) ;END1
(let ((stage  (getSelectedStage   entry  ) )badAmideCount   ) ;END1
(writeCoordinatesToMatter   stage   matter   )

(=   badAmideCount   (countBadAmidesInMatter   self   matter   ) )

(return badAmideCount   )
)
)
)
