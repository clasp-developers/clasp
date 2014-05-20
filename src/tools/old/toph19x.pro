REMARKS TOPH19.PRO ( protein topology )
REMARKS ===============================
REMARKS Charges and atom order modified for neutral GROUPs.
REMARKS Histidine charges set to Del Bene and Cohen sto-3g calculations.
REMARKS Amide charges set to match the experimental dipole moment.
REMARKS Default for HIStidines is the doubly protonated state 

set echo=false end
!! for use with PARAM19 parameters ( no special hydrogen bonding potential )
!! donor and acceptor terms just for analysis

AUTOGENERATE  ANGLES=TRUE  END
{*===========================*}

{* protein default masses *}
MASS   H      1.00800! hydrogen which can h-bond to neutral atom
MASS   HC     1.00800!    ="=     ="=     ="=    to charged atom
MASS   HA     1.00800! aliphatic hydrogen
MASS   CT    12.01100! aliphatic carbon
MASS   C     12.01100! carbonyl carbon
MASS   CH1E  13.01900! extended atom carbon with one hydrogen
MASS   CH2E  14.02700!    ="=      ="=     ="=   two hydrogens
MASS   CH3E  15.03500!    ="=      ="=     ="=   three hydrogens
MASS   CR1E  13.01900!    ="=      ="=      in an aromatic ring with one H
MASS   N     14.00670! peptide nitrogen with no hydrogens attached
MASS   NR    14.00670! nitrogen in an aromatic ring with no hydrogens
MASS   NP    14.00670! pyrole nitrogen
MASS   NH1   14.00670! peptide nitrogen bound to one hydrogen
MASS   NH2   14.00670!    ="=       ="=      ="= two hydrogens
MASS   NH3   14.00670! nitrogen bound to three hydrogens
MASS   NC2   14.00670! charged guandinium nitrogen bound to two hydrogens
MASS   O     15.99940! carbonyl oxygen
MASS   OC    15.99940! carboxy oxygen
MASS   OH1   15.99940! hydroxy oxygen
MASS   S     32.06000! sulphur
MASS   SH1E  33.06800! extended atom sulfur with one hydrogen

!some empirical rules for the following topologies:
!
! 1. angles are taken between all permutations of atoms bonded to
!    a particular atom. Exception: 2 angles linking the THR double ring
! 2. each bond with non-terminal atoms creates one dihedral. Exception:
!    ring bonds in aromatic side chains (but not PRO).
! 3. each planar atom vertex creates one improper-planar term
!    execption: ARG head groups.
! 4. each 1-extended-H carbon atom creates one improper-tetrahedral term
!    (for chirality)
! 5. Each bond in an aromatic ring creates one improper-torsion term
!    (exception: PRO)
! 6. LYS head groups and methyl head groups create one dihedral for 
!    each hydrogen
! 7. all 1:2 and 1:3 nonbonded interactions are assumed to be excluded
!

! ---------------------------------------------------------------------

RESIdue ALA
 GROUp
  ATOM N    TYPE=NH1   CHARge=-0.35   END
  ATOM H    TYPE=H     CHARge= 0.25   END
  ATOM CA   TYPE=CH1E  CHARge= 0.10   END
  ATOM CB   TYPE=CH3E  CHARge= 0.00   END
  ATOM C    TYPE=C     CHARge= 0.55   END  !#
  ATOM O    TYPE=O     CHARge=-0.55   END  !#

 BOND N    CA
 BOND CA   C
 BOND C    O
 BOND N    H
 BOND CA   CB

 IMPRoper  CA     N    C  CB  !tetrahedral CA

 DONOr H    N
 ACCEptor O C

END {ALA}

!------------------------------------------------------------------

RESIdue ARG
 GROUp
  ATOM N     TYPE=NH1   CHARge=-0.35   END
  ATOM H     TYPE=H     CHARge= 0.25   END
  ATOM CA    TYPE=CH1E  CHARge= 0.10   END
  ATOM CB    TYPE=CH2E  CHARge= 0.00   END
  ATOM CG    TYPE=CH2E  CHARge= 0.00   END
  ATOM CD    TYPE=CH2E  CHARge= 0.10   END  !#
  ATOM NE    TYPE=NH1   CHARge=-0.40   END  !#
  ATOM HE    TYPE=H     CHARge= 0.30   END  !#
  ATOM CZ    TYPE=C     CHARge= 0.50   END  !#
  ATOM NH1   TYPE=NC2   CHARge= -0.45  END  !#
  ATOM HH11  TYPE=HC    CHARge=  0.35  END  !#
  ATOM HH12  TYPE=HC    CHARge=  0.35  END  !#
  ATOM NH2   TYPE=NC2   CHARge= -0.45  END  !#
  ATOM HH21  TYPE=HC    CHARge=  0.35  END  !#
  ATOM HH22  TYPE=HC    CHARge=  0.35  END  !#
  ATOM C     TYPE=C     CHARge=  0.55  END  !#
  ATOM O     TYPE=O     CHARge= -0.55  END  !#

 BOND N    CA
 BOND CA   C
 BOND C    O
 BOND N    H
 BOND CA   CB
 BOND CB   CG
 BOND CG   CD
 BOND CD   NE
 BOND NE   HE
 BOND NE   CZ
 BOND CZ   NH1
 BOND CZ   NH2
 BOND NH1  HH11
 BOND NH1  HH12
 BOND NH2  HH21
 BOND NH2  HH22

 DIHEdral N    CA   CB   CG
 DIHEdral CA   CB   CG   CD
 DIHEdral CB   CG   CD   NE
 DIHEdral CG   CD   NE   CZ
 DIHEdral CD   NE   CZ   NH1
 DIHEdral NE   CZ   NH1  HH11 !note: one dehidral for each hydrogen
 DIHEdral NE   CZ   NH2  HH21 !
 DIHEdral NE   CZ   NH1  HH12 !
 DIHEdral NE   CZ   NH2  HH22 !

 IMPRoper CA   N    C    CB !tetrahedral CA
 IMPRoper NE   CD   CZ   HE !planar NE
 IMPRoper CZ   NH1  NH2  NE !planar CZ

 DONOr H    N
 DONOr HE   NE
 DONOr HH11 NH1
 DONOr HH12 NH1
 DONOr HH21 NH2
 DONOr HH22 NH2
 ACCEptor O C

END {ARG}

!------------------------------------------------------------------

RESIdue ASN
 GROUp
  ATOM N    TYPE=NH1   CHARge=-0.35   END
  ATOM H    TYPE=H     CHARge= 0.25   END
  ATOM CA   TYPE=CH1E  CHARge= 0.10   END
  ATOM CB   TYPE=CH2E  CHARge= 0.00   END
  ATOM CG   TYPE=C     CHARge= 0.55   END  !#
  ATOM OD1  TYPE=O     CHARge=-0.55   END  !#
  ATOM ND2  TYPE=NH2   CHARge=-0.60   END  !#
  ATOM HD21 TYPE=H     CHARge= 0.30   END  !#
  ATOM HD22 TYPE=H     CHARge= 0.30   END  !#
  ATOM C    TYPE=C     CHARge= 0.55   END  !#
  ATOM O    TYPE=O     CHARge=-0.55   END  !#

 BOND N    CA
 BOND CA   C
 BOND C    O
 BOND N    H
 BOND CA   CB
 BOND CB   CG
 BOND CG   OD1
 BOND CG   ND2
 BOND ND2  HD21
 BOND ND2  HD22

 DIHEdral N    CA   CB   CG
 DIHEdral CA   CB   CG   OD1
 DIHEdral CB   CG   ND2  HD21

 IMPRoper CA   N    C    CB  !tetrahedral CA
 IMPRoper CG   OD1  ND2  CB  !planar CG
 IMPRoper ND2  HD21 HD22 CG  !planar ND2

 DONOr H    N  
 DONOr HD21 ND2
 DONOr HD22 ND2
 ACCEptor OD1 CG
 ACCEptor O C

END {ASN}

!------------------------------------------------------------------

RESIdue ASP
 GROUp
  ATOM N    TYPE=NH1   CHARge=-0.35   END
  ATOM H    TYPE=H     CHARge= 0.25   END
  ATOM CA   TYPE=CH1E  CHARge= 0.10   END
  ATOM CB   TYPE=CH2E  CHARge=-0.16   END
  ATOM CG   TYPE=C     CHARge= 0.36   END
  ATOM OD1  TYPE=OC    CHARge=-0.60   END
  ATOM OD2  TYPE=OC    CHARge=-0.60   END
  ATOM C    TYPE=C     CHARge= 0.55   END  !#
  ATOM O    TYPE=O     CHARge=-0.55   END  !#

 BOND N    CA
 BOND CA   C
 BOND C    O
 BOND N    H
 BOND CA   CB
 BOND CB   CG
 BOND CG   OD1
 BOND CG   OD2

 DIHEdral N    CA   CB   CG
 DIHEdral CA   CB   CG   OD1

 IMPRoper CA   N    C    CB  !tetrahedral CA
 IMPRoper CG   OD1  OD2  CB  !planar CG

 DONOr H    N
 ACCEptor OD1 CG
 ACCEptor OD2 CG
 ACCEptor O C

END {ASP}

!------------------------------------------------------------------

RESIdue CYS
 GROUp
  ATOM N    TYPE=NH1   CHARge=-0.35   END
  ATOM H    TYPE=H     CHARge= 0.25   END
  ATOM CA   TYPE=CH1E  CHARge= 0.10   END
  ATOM CB   TYPE=CH2E  CHARge= 0.19   END
  ATOM SG   TYPE=SH1E  CHARge=-0.19   END
  ATOM C    TYPE=C     CHARge= 0.55   END  !#
  ATOM O    TYPE=O     CHARge=-0.55   END  !#

 BOND N    CA
 BOND CA   C
 BOND C    O
 BOND N    H
 BOND CA   CB
 BOND CB   SG

 DIHEdral N    CA   CB   SG

 IMPRoper CA   N    C    CB   !tetrahedral CA

 DONOr H    N
 ACCEptor O C

END {CYS}

!------------------------------------------------------------------

RESIdue GLN
 GROUp
  ATOM N    TYPE=NH1   CHARge=-0.35   END
  ATOM H    TYPE=H     CHARge= 0.25   END
  ATOM CA   TYPE=CH1E  CHARge= 0.10   END
  ATOM CB   TYPE=CH2E  CHARge= 0.00   END
  ATOM CG   TYPE=CH2E  CHARge= 0.00   END
  ATOM CD   TYPE=C     CHARge= 0.55   END !#
  ATOM OE1  TYPE=O     CHARge=-0.55   END !#
  ATOM NE2  TYPE=NH2   CHARge=-0.60   END
  ATOM HE21 TYPE=H     CHARge= 0.30   END
  ATOM HE22 TYPE=H     CHARge= 0.30   END
  ATOM C    TYPE=C     CHARge= 0.55   END
  ATOM O    TYPE=O     CHARge=-0.55   END

 BOND N    CA
 BOND CA   C
 BOND C    O
 BOND N    H
 BOND CA   CB
 BOND CB   CG
 BOND CG   CD
 BOND CD   OE1
 BOND CD   NE2
 BOND NE2  HE21
 BOND NE2  HE22

 DIHEdral N    CA   CB   CG
 DIHEdral CA   CB   CG   CD 
 DIHEdral CB   CG   CD   OE1
 DIHEdral CG   CD   NE2  HE21

 IMPRoper CA   N    C    CB   !tetrahedral CA
 IMPRoper CD   OE1  NE2  CG   !planar CD
 IMPRoper NE2  HE21 HE22 CD   !planar NE2

 DONOr H    N
 DONOr HE21 NE2
 DONOr HE22 NE2
 ACCEptor OE1 CD
 ACCEptor O C

END {GLN}

!------------------------------------------------------------------

RESIdue GLU
 GROUp
  ATOM N    TYPE=NH1   CHARge=-0.35   END
  ATOM H    TYPE=H     CHARge= 0.25   END
  ATOM CA   TYPE=CH1E  CHARge= 0.10   END
  ATOM CB   TYPE=CH2E  CHARge= 0.00   END
  ATOM CG   TYPE=CH2E  CHARge=-0.16   END
  ATOM CD   TYPE=C     CHARge= 0.36   END
  ATOM OE1  TYPE=OC    CHARge=-0.60   END
  ATOM OE2  TYPE=OC    CHARge=-0.60   END
  ATOM C    TYPE=C     CHARge= 0.55   END   !#
  ATOM O    TYPE=O     CHARge=-0.55   END   !#

 BOND N    CA
 BOND CA   C
 BOND C    O
 BOND N    H
 BOND CA   CB
 BOND CB   CG
 BOND CG   CD
 BOND CD   OE1
 BOND CD   OE2

 DIHEdral N    CA   CB   CG
 DIHEdral CA   CB   CG   CD
 DIHEdral CB   CG   CD   OE1  !note: only one dihedral for CG-CD

 IMPRoper CA   N    C    CB  !tetrahedral CA
 IMPRoper CD   OE1  OE2  CG  !planar CD

 DONOr H    N
 ACCEptor OE1 CD
 ACCEptor OE2 CD
 ACCEptor O C

END {GLU}

!------------------------------------------------------------------

RESIdue GLY
 GROUp
  ATOM N    TYPE=NH1   CHARge=-0.35   END
  ATOM H    TYPE=H     CHARge= 0.25   END
  ATOM CA   TYPE=CH2E  CHARge= 0.10   END
  ATOM C    TYPE=C     CHARge= 0.55   END  !#
  ATOM O    TYPE=O     CHARge=-0.55   END  !#

 BOND N    CA
 BOND CA   C
 BOND C    O
 BOND N    H

 DONOr H    N
 ACCEptor O C

END {GLY}

!------------------------------------------------------------------


RESIdue HIS    { Doubly protonated histidine.  Ring charges
                 Hayes and Kollman jacs 98:3335 (1976)      }
 GROUp
  ATOM N    TYPE=NH1   CHARge=-0.35   END
  ATOM H    TYPE=H     CHARge= 0.25   END
  ATOM CA   TYPE=CH1E  CHARge= 0.10   END
  ATOM CB   TYPE=CH2E  CHARge= 0.10   END
  ATOM CG   TYPE=C     CHARge= 0.15   END
  ATOM CD2  TYPE=CR1E  CHARge= 0.20   END
  ATOM ND1  TYPE=NH1   CHARge=-0.30   END  !#
  ATOM HD1  TYPE=H     CHARge= 0.35   END  !#
  ATOM CE1  TYPE=CR1E  CHARge= 0.45   END  !#
  ATOM NE2  TYPE=NH1   CHARge=-0.30   END  !#
  ATOM HE2  TYPE=H     CHARge= 0.35   END  !#
  ATOM C    TYPE=C     CHARge= 0.55   END  !#
  ATOM O    TYPE=O     CHARge=-0.55   END  !#

 BOND N    CA
 BOND CA   C
 BOND C    O
 BOND N    H
 BOND CA   CB
 BOND CB   CG
 BOND CG   ND1
 BOND CG   CD2
 BOND ND1  HD1
 BOND ND1  CE1
 BOND CD2  NE2
 BOND CE1  NE2
 BOND NE2  HE2

 DIHEdral N    CA   CB   CG 
 DIHEdral CA   CB   CG   ND1

 IMPRoper CA   N    C    CB  !tetrahedral CA
 IMPRoper CG   ND1  CD2  CB  !planar CG
 IMPRoper ND1  CG   CE1  HD1 !planar ND1
 IMPRoper NE2  CD2  CE1  HE2 !planar NE2

 IMPRoper CG   ND1  CE1  NE2 !!
 IMPRoper ND1  CE1  NE2  CD2 !!
 IMPRoper CE1  NE2  CD2  CG  !! ring torsions
 IMPRoper NE2  CD2  CG   ND1 !!
 IMPRoper CD2  CG   ND1  CE1 !!

 DONOr H    N 
 DONOr HD1  ND1
 DONOr HE2  NE2
 ACCEptor O C

END {HIS}

!------------------------------------------------------------------


RESIdue ILE
 GROUp
  ATOM N    TYPE=NH1   CHARge=-0.35   END
  ATOM H    TYPE=H     CHARge= 0.25   END
  ATOM CA   TYPE=CH1E  CHARge= 0.10   END
  ATOM CB   TYPE=CH1E  CHARge= 0.00   END
  ATOM CG2  TYPE=CH3E  CHARge= 0.00   END
  ATOM CG1  TYPE=CH2E  CHARge= 0.00   END
  ATOM CD   TYPE=CH3E  CHARge= 0.00   END
  ATOM C    TYPE=C     CHARge= 0.55   END  !#
  ATOM O    TYPE=O     CHARge=-0.55   END  !#

 BOND N    CA
 BOND CA   C
 BOND C    O
 BOND N    H
 BOND CA   CB
 BOND CB   CG1
 BOND CB   CG2
 BOND CG1  CD

 DIHEdral N    CA   CB   CG1  !note: only one dihedral around CA-CB
 DIHEdral CA   CB   CG1  CD

 IMPRoper CA   N    C    CB  !tetrahedral CA
 IMPRoper CB   CG1  CG2  CA  !planar CB

 DONOr H    N
 ACCEptor O C

END {ILE}

!------------------------------------------------------------------

RESIdue LEU
 GROUp
  ATOM N    TYPE=NH1   CHARge=-0.35   END
  ATOM H    TYPE=H     CHARge= 0.25   END
  ATOM CA   TYPE=CH1E  CHARge= 0.10   END
  ATOM CB   TYPE=CH2E  CHARge= 0.00   END
  ATOM CG   TYPE=CH1E  CHARge= 0.00   END
  ATOM CD1  TYPE=CH3E  CHARge= 0.00   END
  ATOM CD2  TYPE=CH3E  CHARge= 0.00   END
  ATOM C    TYPE=C     CHARge= 0.55   END  !#
  ATOM O    TYPE=O     CHARge=-0.55   END  !#

 BOND N    CA
 BOND CA   C
 BOND C    O
 BOND N    H
 BOND CA   CB
 BOND CB   CG
 BOND CG   CD1
 BOND CG   CD2

 DIHEdral N    CA   CB   CG
 DIHEdral CA   CB   CG   CD2  !note: only one dihedral around CB-CG

 IMPRoper CA   N    C    CB  !tetrahedral CA
 IMPRoper CG   CD2  CD1  CB  !planar CG

 DONOr H    N
 ACCEptor O C

END {LEU}

!------------------------------------------------------------------

RESIdue LYS
 GROUp
  ATOM N    TYPE=NH1   CHARge=-0.35   END
  ATOM H    TYPE=H     CHARge= 0.25   END
  ATOM CA   TYPE=CH1E  CHARge= 0.10   END
  ATOM CB   TYPE=CH2E  CHARge= 0.00   END
  ATOM CG   TYPE=CH2E  CHARge= 0.00   END
  ATOM CD   TYPE=CH2E  CHARge= 0.00   END
  ATOM CE   TYPE=CH2E  CHARge= 0.25   END  !#
  ATOM NZ   TYPE=NH3   CHARge=-0.30   END  !#
  ATOM HZ1  TYPE=HC    CHARge= 0.35   END  !#
  ATOM HZ2  TYPE=HC    CHARge= 0.35   END  !#
  ATOM HZ3  TYPE=HC    CHARge= 0.35   END  !#
  ATOM C    TYPE=C     CHARge= 0.55   END  !#
  ATOM O    TYPE=O     CHARge=-0.55   END  !#

 BOND N    CA
 BOND CA   C
 BOND C    O
 BOND N    H
 BOND CA   CB
 BOND CB   CG
 BOND CG   CD
 BOND CD   CE
 BOND CE   NZ
 BOND NZ   HZ1
 BOND NZ   HZ2
 BOND NZ   HZ3

 DIHEdral N    CA   CB   CG
 DIHEdral CA   CB   CG   CD
 DIHEdral CB   CG   CD   CE
 DIHEdral CG   CD   CE   NZ
 DIHEdral CD   CE   NZ   HZ1  !!
 DIHEdral CD   CE   NZ   HZ2  !!note: dihedrals for each hydrogen
 DIHEdral CD   CE   NZ   HZ3  !!

 IMPRoper CA   N    C    CB  !tetrahedral CA

 DONOr H    N
 DONOr HZ1  NZ
 DONOr HZ2  NZ
 DONOr HZ3  NZ
 ACCEptor O C

END {LYS}

!------------------------------------------------------------------

RESIdue MET
 GROUp
  ATOM N    TYPE=NH1   CHARge=-0.35   END
  ATOM H    TYPE=H     CHARge= 0.25   END
  ATOM CA   TYPE=CH1E  CHARge= 0.10   END
  ATOM CB   TYPE=CH2E  CHARge= 0.00   END
  ATOM CG   TYPE=CH2E  CHARge= 0.06   END
  ATOM SD   TYPE=S     CHARge=-0.12   END
  ATOM CE   TYPE=CH3E  CHARge= 0.06   END
  ATOM C    TYPE=C     CHARge= 0.55   END  !#
  ATOM O    TYPE=O     CHARge=-0.55   END  !#

 BOND N    CA
 BOND CA   C
 BOND C    O
 BOND N    H
 BOND CA   CB
 BOND CB   CG
 BOND CG   SD
 BOND SD   CE

 DIHEdral N    CA   CB   CG
 DIHEdral CA   CB   CG   SD
 DIHEdral CB   CG   SD   CE

 IMPRoper CA   N    C    CB  !tetrahedral CA

 DONOr H    N
 ACCEptor O C

END {MET}

!------------------------------------------------------------------
RESIdue PEN  ! Penicillamine
 GROUp
  ATOM N    TYPE=NH1   CHARGE=-0.35   END
  ATOM H    TYPE=H     CHARGE=0.25    END
  ATOM CA   TYPE=CH1E  CHARGE=0.10    END
  ATOM CB   TYPE=CT    CHARGE=0.19    END
  ATOM CG1  TYPE=CH3E  CHARGE=0.0     END
  ATOM CG2  TYPE=CH3E  CHARGE=0.0     END
  ATOM SG   TYPE=SH1E  CHARGE=-0.19   END
  ATOM C    TYPE=C     CHARGE=0.55    END
  ATOM O    TYPE=O     CHARGE=-0.55   END

 BOND N    CA      BOND CA   C     BOND C    O      BOND N    H
 BOND CA   CB      BOND CB   SG    BOND CB   CG1    BOND CB   CG2

 DIHEdral N    CA   CB   SG
 IMPRoper CA   N    C    CB  !tetrahedral CA

 DONOr  H  N
 ACCEptor  O  C

END {* PEN *}

!------------------------------------------------------------------

RESIdue PHE
 GROUp
  ATOM N    TYPE=NH1   CHARge=-0.35   END
  ATOM H    TYPE=H     CHARge= 0.25   END
  ATOM CA   TYPE=CH1E  CHARge= 0.10   END
  ATOM CB   TYPE=CH2E  CHARge= 0.00   END
  ATOM CG   TYPE=C     CHARge= 0.00   END
  ATOM CD1  TYPE=CR1E  CHARge= 0.00   END
  ATOM CD2  TYPE=CR1E  CHARge= 0.00   END
  ATOM CE1  TYPE=CR1E  CHARge= 0.00   END
  ATOM CE2  TYPE=CR1E  CHARge= 0.00   END
  ATOM CZ   TYPE=CR1E  CHARge= 0.00   END
  ATOM C    TYPE=C     CHARge= 0.55   END  !#
  ATOM O    TYPE=O     CHARge=-0.55   END  !#

 BOND N    CA
 BOND CA   C
 BOND C    O
 BOND N    H
 BOND CA   CB
 BOND CB   CG
 BOND CG   CD1
 BOND CG   CD2
 BOND CD1  CE1
 BOND CD2  CE2
 BOND CE1  CZ
 BOND CE2  CZ

 DIHEdral N    CA   CB   CG
 DIHEdral CA   CB   CG   CD1  !note: only one dihedral around CB-CG

 IMPRoper CA   N    C    CB  !tetrahedral CA
 IMPRoper CG   CD1  CD2  CB  !planar CG

 IMPRoper CG   CD1  CE1  CZ  !!
 IMPRoper CD1  CE1  CZ   CE2 !!
 IMPRoper CE1  CZ   CE2  CD2 !! ring torsions
 IMPRoper CZ   CE2  CD2  CG  !!
 IMPRoper CE2  CD2  CG   CD1 !!
 IMPRoper CD2  CG   CD1  CE1 !!

 DONOr H    N
 ACCEptor O C

END {PHE}

!------------------------------------------------------------------

RESIdue PRO
 GROUp
  ATOM N    TYPE=N     CHARge=-0.20   END
  ATOM CD   TYPE=CH2E  CHARge= 0.10   END
  ATOM CA   TYPE=CH1E  CHARge= 0.10   END
  ATOM CB   TYPE=CH2E  CHARge= 0.00   END
  ATOM CG   TYPE=CH2E  CHARge= 0.00   END
  ATOM C    TYPE=C     CHARge= 0.55   END
  ATOM O    TYPE=O     CHARge=-0.55   END

 BOND N    CA
 BOND CA   C 
 BOND C    O
 BOND N    CD
 BOND CA   CB
 BOND CB   CG
 BOND CG   CD

 DIHEdral N    CA   CB   CG  !! 
 DIHEdral CA   CB   CG   CD  !!
 DIHEdral CB   CG   CD   N   !! the PROline ring is represented by dihedrals
 DIHEdral CG   CD   N    CA  !!

 IMPRoper CA   N    C    CB  !tetrahedral CA

 ACCEptor O C

END {PRO}

!------------------------------------------------------------------

RESIdue SER
 GROUp
  ATOM N    TYPE=NH1   CHARge=-0.35   END
  ATOM H    TYPE=H     CHARge= 0.25   END
  ATOM CA   TYPE=CH1E  CHARge= 0.10   END
  ATOM CB   TYPE=CH2E  CHARge= 0.25   END  !#
  ATOM OG   TYPE=OH1   CHARge=-0.65   END  !#
  ATOM HG   TYPE=H     CHARge= 0.40   END  !#
  ATOM C    TYPE=C     CHARge= 0.55   END  !#
  ATOM O    TYPE=O     CHARge=-0.55   END  !#

 BOND N    CA
 BOND CA   C
 BOND C    O
 BOND N    H
 BOND CA   CB
 BOND CB   OG
 BOND OG   HG

 DIHEdral N    CA   CB   OG
 DIHEdral CA   CB   OG   HG

 IMPRoper CA   N    C    CB  !tetrahedral CA

 DONOr H    N
 DONOr HG   OG 
 ACCEptor OG " "
 ACCEptor O C

END {SER}

!------------------------------------------------------------------

RESIdue THR
 GROUp
  ATOM N    TYPE=NH1   CHARge=-0.35   END
  ATOM H    TYPE=H     CHARge= 0.25   END
  ATOM CA   TYPE=CH1E  CHARge= 0.10   END
  ATOM CB   TYPE=CH1E  CHARge= 0.25   END  !#
  ATOM OG1  TYPE=OH1   CHARge=-0.65   END  !#
  ATOM HG1  TYPE=H     CHARge= 0.40   END  !#
  ATOM CG2  TYPE=CH3E  CHARge= 0.00   END
  ATOM C    TYPE=C     CHARge= 0.55   END  !#
  ATOM O    TYPE=O     CHARge=-0.55   END  !#

 BOND N    CA
 BOND CA   C
 BOND C    O
 BOND N    H
 BOND CA   CB
 BOND CB   OG1
 BOND CB   CG2
 BOND OG1  HG1

 DIHEdral N    CA   CB   OG1  !note: only one dihedral around CA-CB
 DIHEdral CA   CB   OG1  HG1

 IMPRoper CA   N    C    CB  !tetrahedral CA
 IMPRoper CB   OG1  CG2  CA  !planar CB

 DONOr H    N
 DONOr HG1  OG1
 ACCEptor OG1 " "
 ACCEptor O C

END {THR}

!------------------------------------------------------------------

RESIdue TRP
 GROUp
  ATOM N    TYPE=NH1   CHARge=-0.35   END
  ATOM H    TYPE=H     CHARge= 0.25   END
  ATOM CA   TYPE=CH1E  CHARge= 0.10   END
  ATOM CB   TYPE=CH2E  CHARge= 0.00   END
  ATOM CG   TYPE=C     CHARge=-0.03  END
  ATOM CD2  TYPE=C     CHARge= 0.10  END
  ATOM CE2  TYPE=C     CHARge=-0.04  END
  ATOM CE3  TYPE=CR1E  CHARge=-0.03  END
  ATOM CD1  TYPE=CR1E  CHARge= 0.06  END !#
  ATOM NE1  TYPE=NH1   CHARge=-0.36  END !#
  ATOM HE1  TYPE=H     CHARge= 0.30  END                                   !#
  ATOM CZ2  TYPE=CR1E  CHARge= 0.00  END
  ATOM CZ3  TYPE=CR1E  CHARge= 0.00  END
  ATOM CH2  TYPE=CR1E  CHARge= 0.00   END
  ATOM C    TYPE=C     CHARge= 0.55   END
  ATOM O    TYPE=O     CHARge=-0.55   END

 BOND N    CA
 BOND CA   C
 BOND C    O
 BOND N    H
 BOND CA   CB
 BOND CB   CG
 BOND CG   CD1
 BOND CG   CD2
 BOND CD1  NE1
 BOND CD2  CE2
 BOND NE1  HE1
 BOND NE1  CE2
 BOND CD2  CE3
 BOND CE2  CZ2
 BOND CE3  CZ3
 BOND CZ2  CH2
 BOND CZ3  CH2

 OMIT ANGLe CG CD2 CE3
 OMIT ANGLe NE1 CE2 CZ2
! The ring angles are only assigned for the five and six member rings.
! The angles CG CD2 CE3  and NE1 CE2 CZ2 are left out to avoid conflicts
! with the histidine angle parameters and to preserve symmetry.

 DIHEdral N    CA   CB   CG
 DIHEdral CA   CB   CG   CD1
 DIHEdral CD1  CE2  CD2  CZ3  !!
 DIHEdral CD1  CD2  CE2  CH2  !! these dihedrals span the rings and keep
 DIHEdral CZ2  CE2  CD2  CG   !! them parallel
 DIHEdral CE3  CD2  CE2  NE1  !!

 IMPRoper CA   N    C    CB   !tetrahedral CA
 IMPRoper CG   CD1  CD2  CB   !planar CG
 IMPRoper NE1  CD1  CE2  HE1  !planar NE1

 IMPRoper CD2  CE2  CZ2  CH2  !!
 IMPRoper CE2  CZ2  CH2  CZ3  !!
 IMPRoper CZ2  CH2  CZ3  CE3  !! torsions ring 2
 IMPRoper CH2  CZ3  CE3  CD2  !!
 IMPRoper CZ3  CE3  CD2  CE2  !!
 IMPRoper CE3  CD2  CE2  CZ2  !!

 IMPRoper CG   CD1  NE1  CE2  !!
 IMPRoper CD1  NE1  CE2  CD2  !!
 IMPRoper NE1  CE2  CD2  CG   !! torsions ring 1
 IMPRoper CE2  CD2  CG   CD1  !!
 IMPRoper CD2  CG   CD1  NE1  !!

 DONOr H    N
 DONOr HE1  NE1
 ACCEptor O C

END {THR}

!------------------------------------------------------------------

RESIdue TYR
 GROUp
  ATOM N    TYPE=NH1   CHARge=-0.35   END
  ATOM H    TYPE=H     CHARge= 0.25   END
  ATOM CA   TYPE=CH1E  CHARge= 0.10   END
  ATOM CB   TYPE=CH2E  CHARge= 0.00   END
  ATOM CG   TYPE=C     CHARge= 0.00   END
  ATOM CD1  TYPE=CR1E  CHARge= 0.00   END
  ATOM CE1  TYPE=CR1E  CHARge= 0.00   END
  ATOM CD2  TYPE=CR1E  CHARge= 0.00  END
  ATOM CE2  TYPE=CR1E  CHARge= 0.00  END
  ATOM CZ   TYPE=C     CHARge= 0.25  END
  ATOM OH   TYPE=OH1   CHARge=-0.65  END
  ATOM HH   TYPE=H     CHARge= 0.40  END
  ATOM C    TYPE=C     CHARge= 0.55  END
  ATOM O    TYPE=O     CHARge=-0.55  END

 BOND N    CA
 BOND CA   C
 BOND C    O
 BOND N    H
 BOND CA   CB
 BOND CB   CG
 BOND CG   CD1
 BOND CG   CD2
 BOND CD1  CE1
 BOND CD2  CE2
 BOND CE1  CZ 
 BOND CE2  CZ
 BOND CZ   OH
 BOND OH   HH

 DIHEdral N    CA   CB   CG
 DIHEdral CA   CB   CG   CD1
 DIHEdral CE2  CZ   OH   HH

 IMPRoper CA   N    C    CB !tetrahedral CA
 IMPRoper CG   CD1  CD2  CB !planar CG
 IMPRoper CZ   CE1  CE2  OH !planar CZ

 IMPRoper CG   CD1  CE1  CZ  !!
 IMPRoper CD1  CE1  CZ   CE2 !!
 IMPRoper CE1  CZ   CE2  CD2 !! ring torsions
 IMPRoper CZ   CE2  CD2  CG  !!
 IMPRoper CE2  CD2  CG   CD1 !!
 IMPRoper CD2  CG   CD1  CE1 !!

 DONOr H    N
 DONOr HH   OH
 ACCEptor OH " "
 ACCEptor O C

END {TYR}

!------------------------------------------------------------------

RESIdue VAL
 GROUp
  ATOM N    TYPE=NH1   CHARge=-0.35   END
  ATOM H    TYPE=H     CHARge= 0.25   END
  ATOM CA   TYPE=CH1E  CHARge= 0.10   END
  ATOM CB   TYPE=CH1E  CHARge= 0.00   END
  ATOM CG1  TYPE=CH3E  CHARge= 0.00   END
  ATOM CG2  TYPE=CH3E  CHARge= 0.00   END
  ATOM C    TYPE=C     CHARge= 0.55   END
  ATOM O    TYPE=O     CHARge=-0.55   END

 BOND N    CA
 BOND CA   C
 BOND C    O
 BOND N    H
 BOND CA   CB
 BOND CB   CG1
 BOND CB   CG2

 DIHEdral N    CA   CB   CG1  !note: only one dihedral around CA-CB

 IMPRoper CA   N    C    CB  !tetrahedral CA
 IMPRoper CB   CG2  CG1  CA  !planar CB

 DONOr H    N
 ACCEptor O C

END {VAL}

!------------------------------------------------------------------

RESIdue FORM  ! formamide. Charges according to Reiher (from States?)
 GROUp
  ATOM HA   TYPE=HA    CHARge= 0.00   END
  ATOM C    TYPE=C     CHARge= 0.55   END
  ATOM O    TYPE=O     CHARge=-0.55   END
  ATOM N    TYPE=NH2   CHARge=-0.60   END
  ATOM H1   TYPE=H     CHARge= 0.30   END
  ATOM H2   TYPE=H     CHARge= 0.30   END

 BOND HA   C       BOND C     N      BOND C    O
 BOND N    H2      BOND N    H1

 DIHEdral  HA   C     N   H2  !single dihedral around C-N
 IMPRoper  C    HA    N   O   !planar C
 IMPRoper  N     C   H2   H1  !planar N

 ACCEptor O  C
 DONOr    H1 N

END {* FORM *}

!------------------------------------------------------------------

RESIdue ACE      { Acetyl blocking GROUp with standard naming.
                   can be linked:  ACE       *
                                     \ PEPT /                  }
 GROUp
  ATOM CA   TYPE=CH3E  CHARge= 0.00   END
  ATOM C    TYPE=C     CHARge= 0.55   END  !#
  ATOM O    TYPE=O     CHARge=-0.55   END  !#

 BOND CA   C
 BOND C    O

 ACCEptor O C

END {ACE}

!------------------------------------------------------------------

RESIdue CBX                 { can be linked   *     CBX 
                                               \PEPT/     }
 GROUp
  ATOM N    TYPE=NH1   CHARge=-0.35   END
  ATOM H    TYPE=H     CHARge= 0.25   END
  ATOM CA   TYPE=CH3E  CHARge= 0.10   END

 BOND N    CA
 BOND N    H

 DONOr H    N

END {CBX}

!----------------------------------------------------------------------

RESIdue SO4         { sulfate group }
 GROUp
  ATOM S    TYPE=S   CHARge= 0.0   END
  ATOM O1   TYPE=OC  CHARge=-0.5   END
  ATOM O2   TYPE=OC  CHARge=-0.5   END
  ATOM O3   TYPE=OC  CHARge=-0.5   END
  ATOM O4   TYPE=OC  CHARge=-0.5   END

 BOND S O1
 BOND S O2
 BOND S O3
 BOND S O4

 ACCEptor O1 S
 ACCEptor O2 S
 ACCEptor O3 S
 ACCEptor O4 S

END {SO4}

!------------------------------------------------------------------

PRESidue DISU       { disulfide bridge  ...CYS      CYS...
                                              \DISU/            }
 GROUP
  MODIfy ATOM 1CB           CHARge= 0.19  END
  MODIfy ATOM 1SG  TYPE=S   CHARge=-0.19  END
 GROUP
  MODIfy ATOM 2CB           CHARge= 0.19  END
  MODIfy ATOM 2SG  TYPE=S   CHARge=-0.19  END

 ADD BOND 1SG 2SG

 ADD ANGLe  1CB 1SG 2SG
 ADD ANGLe  1SG 2SG 2CB

 ADD DIHEdral   1CA 1CB 1SG 2SG
 ADD DIHEdral   1CB 1SG 2SG 2CB
 ADD DIHEdral   1SG 2SG 2CB 2CA

END {DISU}

!----------------------------------------------------------------------

PRESidue LtoD  {* patch to make D residue, from M. Pettitt *}
 DELEte IMPRoper CA N C CB
 ADD    IMPRoper CA C N CB
END {* LtoD *}

!----------------------------------------------------------------------

PRESidue PEPT { PEPTide bond link, for all 
               amino acids ...*(-)     (+)*...
                                \ PEPT /

               except the  *(-) - (+)PRO link        }

 ADD BOND -C +N 

 ADD ANGLE -CA -C +N
 ADD ANGLE -O  -C +N
 ADD ANGLE -C  +N +CA
 ADD ANGLE -C  +N +H

 ADD DIHEdral  -C +N +CA +C
 ADD DIHEdral  -N -CA -C +N
 ADD DIHEdral  -CA -C +N +CA

 ADD IMPRoper  -C -CA +N -O  {planar -C}
 ADD IMPRoper  +N -C +CA +H  {planar +N}

END {PEPT}

!----------------------------------------------------------------------
PRESidue PEPP  { for  ...*(-) - (+)PRO  link
               same as PEPT except replacement H by CD
               and improper +N +CA +CD -C              }

 ADD BOND -C +N 

 ADD ANGLE -CA -C +N
 ADD ANGLE -O  -C +N
 ADD ANGLE -C  +N +CA
 ADD ANGLE -C  +N +CD

 ADD DIHEdral  -C +N +CA +C
 ADD DIHEdral  -N -CA -C +N
 ADD DIHEdral  -CA -C +N +CA

 ADD IMPRoper  -C -CA +N -O  {planar -C}
 ADD IMPRoper  +N +CA +CD -C  {planar +N}

END {PEPP}

!------------------------------------------------------------------

PRESidue NTER             { can be patched ( as NTER - * ... )
                            to any amino acid except PRO        }
 GROUp
  ADD    ATOM +HT1  TYPE=HC   CHARge=0.35  END
  ADD    ATOM +HT2  TYPE=HC   CHARge=0.35  END
  MODIfy ATOM +N    TYPE=NH3  CHARge=-0.30 END
  ADD    ATOM +HT3  TYPE=HC   CHARge=0.35  END
  DELETE ATOM +H                           END
  MODIfy ATOM +CA             CHARge=0.25  END

 ADD BOND +HT1 +N
 ADD BOND +HT2 +N
 ADD BOND +HT3 +N

 ADD ANGLe +HT1  +N    +HT2
 ADD ANGLe +HT2  +N    +HT3
 ADD ANGLe +HT2  +N    +CA
 ADD ANGLe +HT1  +N    +HT3
 ADD ANGLe +HT1  +N    +CA
 ADD ANGLe +HT3  +N    +CA

 ADD DIHEdral +HT2  +N    +CA   +C
 ADD DIHEdral +HT1  +N    +CA   +C
 ADD DIHEdral +HT3  +N    +CA   +C

 ADD DONOr +HT1  +N
 ADD DONOr +HT2  +N
 ADD DONOr +HT3  +N

END {NTER}

!------------------------------------------------------------------

PRESidue PROP                { this is the N-terminal for PROlines
                               PROP - PRO -...                      }
 GROUp
  ADD    ATOM +HT1  TYPE=HC   CHARge= 0.35   END
  ADD    ATOM +HT2  TYPE=HC   CHARge= 0.35   END
  MODIfy ATOM +N    TYPE=NH3  CHARge=-0.20   END
  MODIfy ATOM +CD             CHARge= 0.25   END
  MODIfy ATOM +CA             CHARge= 0.25   END

 ADD BOND +HT1  +N
 ADD BOND +HT2  +N

 ADD ANGLe +HT1  +N    +HT2
 ADD ANGLe +HT2  +N    +CA
 ADD ANGLe +HT1  +N    +CD
 ADD ANGLe +HT1  +N    +CA
 ADD ANGLe +CD   +N    +HT2

 ADD DIHEdral +HT2  +N    +CA   +C
 ADD DIHEdral +HT1  +N    +CA   +C

 ADD DONOr +HT1  +N
 ADD DONOr +HT2  +N

END {PROP}

!------------------------------------------------------------------

PRESidue CTER                { C-terminal for all amino acids
                                           ... * - CTER          }
 GROUp
  MODIfy ATOM -C             CHARge= 0.14  END
  ADD    ATOM -OT1  TYPE=OC  CHARge=-0.57  END
  ADD    ATOM -OT2  TYPE=OC  CHARge=-0.57  END
  DELETE ATOM -O                           END

 ADD BOND -C    -OT1
 ADD BOND -C    -OT2

 ADD ANGLe -CA   -C   -OT1
 ADD ANGLe -CA   -C   -OT2
 ADD ANGLe -OT1  -C   -OT2

 ADD DIHEdral -N    -CA    -C   -OT2

 ADD IMPRoper -C    -CA    -OT2 -OT1

 ADD ACCEptor -OT1 -C

 ADD ACCEptor -OT2 -C

END {CTER}

!------------------------------------------------------------------
set echo=true end
