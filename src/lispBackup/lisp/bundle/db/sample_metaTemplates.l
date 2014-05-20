


(include "CANDO/db/metaDatabase.l" )
(include "CANDO/db/checkMetaDatabase.l" )




(global *default_AchiralAminoAcid* "gly" )
(global *default_AmineRingClosingAlphaAminoAcid* "dab(S)" )
(global *default_AmineRingClosingAlphaAminoAcidsTemplate* "dab(%s)" )
(global *default_CarbonylRingClosingAlphaAminoAcid* "gluCarb(S)" )
(global *default_CarbonylRingClosingAlphaAminoAcidsTemplate* "gluCarb(%s)" )
(global *default_ReversedAmineRingClosingAlphaAminoAcid-S* "dab(S)" )
(global *default_ReversedBisAminoAcid-S* "revPro4(2S4S)" )
(global *default_ReversedAmineRingClosingAlphaAminoAcid-R* "dab(R)" )
(global *default_SecondaryAminoAcidS* "pro(S)" )
(global *default_PrimaryAmide* "nh2" )
(global *default_AmineAcylMethylene* "amineAcylMethyleneCap" )
(global *default_NSideChain* "nAla" )
(global *default_BisAminoAcid* "pro4(2S4S)" )
(global *default_ReversedBisAminoAcidTemplate* "revPro4(2%s4S)" )
(global *default_AlphaAminoAcidsTemplate* "ala(%s)" )
(global *default_AlphaAminoAcid* "ala(S)" )
(global *default_SecondaryAminoAcidsTemplate* "pro(%s)" )
(global *default_SecondaryAminoAcid* "pro(S)" )
(global *default_PrimaryAmine* "methylAmine" )
(global *default_Ester* "methylOxide" )
(global *default_AcylGroup* "ace" )
(global *default_AcylMethylene* "acylMethyleneCap" )
					; this one or the next?
					; global *default_AcylMethylene* "acylMethylene"
					;	 which one should it be?

(global *default_Hydantoin* "hydantoin" )
(global *default_Hn* "hn" )
(global *default_ImideSideChain* "imidePhenyl" )
(print  "Creating CandoDatabase" )
(global bdb (CandoDatabase :frameRecognizers () :representedEntityNameSets () :constitutions ()))
(setDatabase bdb )
(defun def_BisAminoAcids ()
  (let (group name monomerName pdbName leadingName trailingName
	      chiralCenters constrainedPiBonds monomerName pdbName
	      residue stereoInformation leadingAtom trailingAtom
	      n+ca n+n n+co n+oco n-ca n-n n-co n-oco groupAdd
	      newConstitution frame+core frame-core frame-co
	      frame+co frame-n frame+secondn frame+quatn
	      frame+quatcarbonyl frame+n plug-primaryAmide
	      plug-sidePeptide plug-peptide plug-dkp
	      plug+secondn plug+leadingHydantoin plug+dkp
	      plug+peptide plug+camide plug+ester plug+nside
	      frag-co frag+co body )
    (setq group "BisAminoAcids" )
    (setq name (MetaProperty :name ))
    (setq monomerName (MetaProperty :monomerName ))
    (setq pdbName (MetaProperty :pdbName ))
    (setq leadingName (MetaProperty :leading ))
    (setq trailingName (MetaProperty :trailing ))
    (setq chiralCenters (MetaPropertyOrDefault :chiralCenters ()))
    (setq constrainedPiBonds (MetaPropertyOrDefault :constrainedPiBonds ()))
    (setq monomerName (MetaProperty :monomerName ))
    (setq pdbName (MetaProperty :pdbName ))
    (setq residue (MetaResidue ))
    (setq stereoInformation (MetaStereoInformation_standard ))
    (setq leadingAtom (MetaResidueAtomWithName leadingName ))
    (setq trailingAtom (MetaResidueAtomWithName trailingName ))
    (log (% "leadingAtom class is: %s" (className leadingAtom )))
    (setq n+ca (MetaRecognizedAtom "n+ca" leadingAtom (MetaFrameRecognizer "cAlphaAmide" )"1" ))
    (setq n+n (MetaRecognizedAtom "n+n" leadingAtom (MetaFrameRecognizer "cAlphaAmide" )"2" ))
    (setq n+co (MetaRecognizedAtom "n+co" leadingAtom (MetaFrameRecognizer "cAlphaAmide" )"3" ))
    (setq n+oco (MetaRecognizedAtom "n+oco" leadingAtom (MetaFrameRecognizer "cAlphaAmide" )"4" ))
    (setq n-ca (MetaRecognizedAtom "n-ca" trailingAtom (MetaFrameRecognizer "cAlphaAmide" )"1" ))
    (setq n-n (MetaRecognizedAtom "n-n" trailingAtom (MetaFrameRecognizer "cAlphaAmide" )"2" ))
    (setq n-co (MetaRecognizedAtom "n-co" trailingAtom (MetaFrameRecognizer "cAlphaAmide" )"3" ))
    (setq n-oco (MetaRecognizedAtom "n-oco" trailingAtom (MetaFrameRecognizer "cAlphaAmide" )"4" ))
    (setq groupAdd (MetaAddToGroup :groupName group :constitutionName name ))
    (setq newConstitution
	  (MetaConstitution
	   :group group
	   :name name
	   :addToGroups (list groupAdd )
	   :residue residue
	   :residueNetCharge (MetaConstant 0 )
	   :stereoInformation stereoInformation
	   :frames (list (setq frame+core (MetaRecognizedFrame
					   :name "+core"
					   :origin n+ca
					   :recognizer (MetaFrameRecognizer "cAlphaAmide" )))
			 (setq frame-core (MetaRecognizedFrame
					   :name "-core"
					   :origin n-ca
					   :recognizer (MetaFrameRecognizer "cAlphaAmide" )))
			 (setq frame-co (MetaRecognizedFrame
					 :name "-co"
					 :origin n-co
					 :recognizer (MetaFrameRecognizer "coAmide" )))
			 (setq frame+co (MetaRecognizedFrame
					 :name "+co"
					 :origin n+co
					 :recognizer (MetaFrameRecognizer "coAmide" )))
			 (setq frame-n (MetaRecognizedFrame
					:name "-n"
					:origin n-n
					:recognizer (MetaFrameRecognizer "nAmide" )))
			 (setq frame+secondn (MetaRecognizedFrame
					      :name "+secondn"
					      :origin n-n
					      :recognizer (MetaFrameRecognizer "nAmideAlphaAminoAcid" )))
			 (setq frame+quatn (MetaRecognizedFrame
					    :name "+quatn"
					    :origin n+n
					    :recognizer (MetaFrameRecognizer "nAmideAlphaAminoAcid" )))
			 (setq frame+quatcarbonyl (MetaRecognizedFrame
						   :name "+quatcarbonyl"
						   :origin n+co
						   :recognizer (MetaFrameRecognizer "coAmideAlphaAminoAcid" )))
			 (setq frame+n (MetaRecognizedFrame
					:name "+n"
					:origin n+n
					:recognizer (MetaFrameRecognizer "nAmide" ))))

	   :plugs (list (setq plug-primaryAmide (MetaInPlug
						 :name "-primaryAmide"
						 :bond0 n-co
						 :mates (list (Mate
							       :cap *default_PrimaryAmide* ))))
			(setq plug-sidePeptide (MetaInPlug
						:name "-sidePeptide"
						:bond0 n-co
						:mates (list (Mate
							      :cap *default_AmineRingClosingAlphaAminoAcid* ))))
			(setq plug-peptide (MetaInPlug
					    :name "-peptide"
					    :bond0 n-co
					    :mates (list (Mate
							  :cap *default_PrimaryAmine*
							  :groupNames (list "AlphaAminoAcids" "Amines" "SecondaryAminoAcids" "AmineRingClosingAlphaAminoAcids" "CarbonylRingClosingAlphaAminoAcids" )))))
			(setq plug-dkp (MetaInPlug
					:name "-dkp"
					:bond0 n-co
					:bond1 n-n
					:mates (list (Mate
						      :cap *default_AchiralAminoAcid*
						      :groupNames (list "AlphaAminoAcids" "BisAminoAcids" "ReversedBisAminoAcids" "SecondaryAminoAcids" "AmineRingClosingAlphaAminoAcids" "CarbonylRingClosingAlphaAminoAcids" )))))
			(setq plug+secondn (MetaOutPlug
					    :name "+secondn"
					    :bond0 n-n
					    :exportFrame frame+secondn
					    :framesCompletedByCoupling (list (MetaFrameFinisher frame-n ))
					    :mates (list (Mate
							  :cap *default_AcylGroup*
							  :groupNames (list "AcylGroups" )))))
			(setq plug+leadingHydantoin (MetaOutPlug
						     :name "+leadingHydantoin"
						     :bond0 n+n
						     :bond1 n+co
						     :exportFrame frame+core
						     :framesCompletedByCoupling (list (MetaFrameFinisher frame+n )
										(MetaFrameFinisher frame+co ))
						     :mates (list (Mate
								   :cap *default_Hydantoin*
								   :groupNames (list "Hydantoins" )))))
			(setq plug+dkp (MetaOutPlug
					:name "+dkp"
					:bond0 n+n
					:bond1 n+co
					:exportFrame frame+core
					:framesCompletedByCoupling (list (MetaFrameFinisher frame+n )
								   (MetaFrameFinisher frame+co ))
					:mates (list (Mate :cap *default_AchiralAminoAcid* ))))
			(setq plug+peptide (MetaOutPlug
					    :name "+peptide"
					    :bond0 n+n
					    :exportFrame frame+quatn
					    :framesCompletedByCoupling (list (MetaFrameFinisher frame+n ))
					    :mates (list (Mate
							  :cap *default_AcylGroup* ))))
			(setq plug+camide (MetaOutPlug
					   :name "+camide"
					   :bond0 n+co
					   :exportFrame frame+quatcarbonyl
					   :framesCompletedByCoupling (list (MetaFrameFinisher frame+co ))
					   :mates (list (Mate
							 :cap *default_PrimaryAmine* ))))
			(setq plug+ester (MetaOutPlug
					  :name "+ester"
					  :bond0 n+co
					  :exportFrame frame+quatcarbonyl
					  :framesCompletedByCoupling (list (MetaFrameFinisher frame+co ))
					  :mates (list (Mate
							:cap *default_Ester* ))))
			(setq plug+nside (MetaOutPlug
					  :name "+nside"
					  :bond0 n+n
					  :exportFrame frame+n
					  :framesCompletedByCoupling ()
					  :mates (list (Mate
							:cap "hn"
							:groupNames (list "NSideChains" ))))))
	   :fragments (list (setq frag-co (MetaFragment
					   :name "frag-co"
					   :atoms (list n-oco )))
			    (setq frag+co (MetaFragment :name "frag+co" :atoms (list n+oco )))
			    (setq body (MetaRemainderFragment
					:residue residue
					:name "body"
					:removeFragments (list frag-co frag+co ))))
	   :topologies (list (MetaTopology
			      :name "didkp"
			      :residue residue
			      :plugs (list plug-dkp plug+dkp plug+nside )
			      :coreFragment (MetaExtractCoreFragment
					     :fragment body )
			      :fragments (list (MetaExtractFragment
						:fragment frag-co
						:frame frame-co )
					       (MetaExtractFragment
						:fragment frag+co
						:frame frame+co ))
			      :alignTo (MetaRecognizedFrame
					:name "align"
					:origin n-ca
					:recognizer (MetaFrameRecognizer "cAlphaAmide" )))
			     (MetaTopology
			      :name "sidePeptideInDkpOut"
			      :residue residue
			      :plugs (list plug-sidePeptide plug+secondn plug+dkp plug+nside )
			      :coreFragment (MetaExtractCoreFragment
					     :fragment body )
			      :fragments (list (MetaExtractFragment
						:fragment frag-co
						:frame frame-co )
					       (MetaExtractFragment
						:fragment frag+co
						:frame frame+co ))
			      :alignTo (MetaRecognizedFrame
					:name "align"
					:origin n-ca
					:recognizer (MetaFrameRecognizer "cAlphaAmide" )))
			     (MetaTopology
			      :name "primaryAmideInDkpOut"
			      :residue residue
			      :plugs (list plug-primaryAmide plug+secondn plug+dkp plug+nside )
			      :coreFragment (MetaExtractCoreFragment
					     :fragment body )
			      :fragments (list (MetaExtractFragment
						:fragment frag-co
						:frame frame-co )
					       (MetaExtractFragment
						:fragment frag+co
						:frame frame+co ))
			      :alignTo (MetaRecognizedFrame
					:name "align"
					:origin n-ca
					:recognizer (MetaFrameRecognizer "cAlphaAmide" )))
			     (MetaTopology
			      :name "peptideInDkpOut"
			      :residue residue
			      :plugs (list plug-peptide plug+secondn plug+dkp plug+nside )
			      :coreFragment (MetaExtractCoreFragment
					     :fragment body )
			      :fragments (list (MetaExtractFragment
						:fragment frag-co
						:frame frame-co )
					       (MetaExtractFragment
						:fragment frag+co
						:frame frame+co ))
			      :alignTo (MetaRecognizedFrame
					:name "align"
					:origin n-ca
					:recognizer (MetaFrameRecognizer "cAlphaAmide" )))
			     (MetaTopology
			      :name "dkpInPeptideOutEsterOut"
			      :residue residue
			      :plugs (list plug-dkp plug+peptide plug+nside plug+ester )
			      :coreFragment (MetaExtractCoreFragment
					     :fragment body )
			      :fragments (list (MetaExtractFragment
						:fragment frag-co
						:frame frame-co )
					       (MetaExtractFragment
						:fragment frag+co
						:frame frame+co ))
			      :alignTo (MetaRecognizedFrame
					:name "align"
					:origin n-ca
					:recognizer (MetaFrameRecognizer "cAlphaAmide" )))
			     (MetaTopology
			      :name "dkpInPeptideOutCAmideOut"
			      :residue residue
			      :plugs (list plug-dkp plug+peptide plug+nside plug+camide )
			      :coreFragment (MetaExtractCoreFragment
					     :fragment body )
			      :fragments (list (MetaExtractFragment
						:fragment frag-co
						:frame frame-co )
					       (MetaExtractFragment
						:fragment frag+co
						:frame frame+co ))
			      :alignTo (MetaRecognizedFrame
					:name "align"
					:origin n-ca
					:recognizer (MetaFrameRecognizer "cAlphaAmide" )))
			     (MetaTopology
			      :name "dkpInHydantoinOut"
			      :residue residue
			      :plugs (list plug-dkp plug+leadingHydantoin plug+nside )
			      :coreFragment (MetaExtractCoreFragment
					     :fragment body )
			      :fragments (list (MetaExtractFragment
						:fragment frag-co
						:frame frame-co )
					       (MetaExtractFragment
						:fragment frag+co
						:frame frame+co ))
			      :alignTo (MetaRecognizedFrame
					:name "align"
					:origin n-ca
					:recognizer (MetaFrameRecognizer "cAlphaAmide" )))
			     (MetaTopology
			      :name "peptideInHydantoinOut"
			      :residue residue
			      :plugs (list plug-peptide plug+secondn plug+leadingHydantoin plug+nside )
			      :coreFragment (MetaExtractCoreFragment
					     :fragment body )
			      :fragments (list (MetaExtractFragment
						:fragment frag-co
						:frame frame-co )
					       (MetaExtractFragment
						:fragment frag+co
						:frame frame+co ))
			      :alignTo (MetaRecognizedFrame
					:name "align"
					:origin n-ca
					:recognizer (MetaFrameRecognizer "cAlphaAmide" ))))))
    newConstitution ))
;; 
;;
;;  reversedBisAminoAcid constitution
;; fragment must have :properties
;;	:name [constitution name]
;;	:type [bisAminoAcid]
;;	:monomerName [ pro4(2%1%4%2%) ]
;;	:pdbName [O%1%%2% ]
;;	:leading [atom CG]
;;	:trailing [atom CA]
;;	:nside	[NG]
;;

(defun def_ReversedBisAminoAcids ()
  (let (group name monomerName pdbName leadingName trailingName residue residueCharge
	      leadingAtom trailingAtom stereoInformation groupAdder n-ca n-n n-co
	      n-oco n+ca n+n n+co n+oco newConstitution frame+core frame-core
	      frame-co frame-n frame+n frame+co frame+secondn frame+quatcarbonyl
	      plug-peptide plug-trailingHydantoin plug-dkp plug+dkp plug+nside
	      plug+camide plug+ester plug+secondn frag-co frag+co body )
    (setq group "ReversedBisAminoAcids" )
    (setq name (MetaProperty :name :resolveCallback (lambda () (debug)) ))
    (setq monomerName (MetaProperty :monomerName ))
    (setq pdbName (MetaProperty :pdbName ))
    (setq leadingName (MetaProperty :leading ))
    (setq trailingName (MetaProperty :trailing ))
    (setq residue (MetaResidue ))
    (setq residueCharge (MetaPropertyOrDefault :residueCharge 0 ))
    (setq leadingAtom (MetaResidueAtomWithName leadingName ))
    (setq trailingAtom (MetaResidueAtomWithName trailingName ))
    (setq stereoInformation (MetaStereoInformation_standard ))
    (setq groupAdder (MetaAddToGroupByStereoisomer
		      :groupPrefix group
		      :stereoInformation stereoInformation
		      :separateByCenter "CA" ))
    (setq n-ca (MetaRecognizedAtom "n-ca" trailingAtom (MetaFrameRecognizer "cAlphaAmide" )"1" ))
    (setq n-n (MetaRecognizedAtom "n-n" trailingAtom (MetaFrameRecognizer "cAlphaAmide" )"2" ))
    (setq n-co (MetaRecognizedAtom "n-co" trailingAtom (MetaFrameRecognizer "cAlphaAmide" )"3" ))
    (setq n-oco (MetaRecognizedAtom "n-oco" trailingAtom (MetaFrameRecognizer "cAlphaAmide" )"4" ))
    (setq n+ca (MetaRecognizedAtom "n+ca" leadingAtom (MetaFrameRecognizer "cAlphaAmide" )"1" ))
    (setq n+n (MetaRecognizedAtom "n+n" leadingAtom (MetaFrameRecognizer "cAlphaAmide" )"2" ))
    (setq n+co (MetaRecognizedAtom "n+co" leadingAtom (MetaFrameRecognizer "cAlphaAmide" )"3" ))
    (setq n+oco (MetaRecognizedAtom "n+oco" leadingAtom (MetaFrameRecognizer "cAlphaAmide" )"4" ))
    (setq newConstitution (MetaConstitution
			   :name name
			   :group group
			   :addToGroups (list groupAdder )
			   :residue residue
			   :residueNetCharge residueCharge
			   :stereoInformation stereoInformation
			   :frames (list (setq frame+core (MetaRecognizedFrame
							   :name "+core"
							   :origin n+ca
							   :recognizer (MetaFrameRecognizer "cAlphaAmide" )))
					 (setq frame-core (MetaRecognizedFrame
							   :name "-core"
							   :origin n-ca
							   :recognizer (MetaFrameRecognizer "cAlphaAmide" )))
					 (setq frame-co (MetaRecognizedFrame
							 :name "-co"
							 :origin n-co
							 :recognizer (MetaFrameRecognizer "coAmide" )))
					 (setq frame-n (MetaRecognizedFrame
							:name "-n"
							:origin n-n
							:recognizer (MetaFrameRecognizer "nAmide" )))
					 (setq frame+n (MetaRecognizedFrame
							:name "+n"
							:origin n+n :recognizer (MetaFrameRecognizer "nAmide" )))
					 (setq frame+co (MetaRecognizedFrame
							 :name "+co"
							 :origin n+co
							 :recognizer (MetaFrameRecognizer "coAmide" )))
					 (setq frame+secondn (MetaRecognizedFrame
							      :name "+secondn"
							      :origin n-n
							      :recognizer (MetaFrameRecognizer "nAmideAlphaAminoAcid" )))
					 (setq frame+quatcarbonyl (MetaRecognizedFrame
								   :name "+quatcarbonyl"
								   :origin n-co
								   :recognizer (MetaFrameRecognizer "coAmideAlphaAminoAcid" ))))
			   :plugs (list (setq plug-peptide (MetaInPlug
							    :name "-peptide"
							    :bond0 n-co
							    :mates (list (Mate
									  :cap *default_PrimaryAmine*
									  :groupNames (list "AlphaAminoAcids" "Amines" )))))
					(setq plug-trailingHydantoin (MetaInPlug
								      :name "-trailingHydantoin"
								      :bond0 n-co
								      :bond1 n-n
								      :mates (list (Mate
										    :cap *default_Hydantoin*
										    :groupNames (list "Hydantoins" )))))
					(setq plug-dkp (MetaInPlug
							:name "-dkp"
							:bond0 n-co
							:bond1 n-n
							:mates (list (Mate
								      :cap *default_AchiralAminoAcid*
								      :groupNames (list "AlphaAminoAcids" "BisAminoAcids" "AmineRingClosingAlphaAminoAcids" "CarbonylRingClosingAlphaAminoAcids" )))))
					(setq plug+dkp (MetaOutPlug
							:name "+dkp"
							:bond0 n+n
							:bond1 n+co
							:exportFrame frame+core
							:framesCompletedByCoupling (list (MetaFrameFinisher frame+n )
										   (MetaFrameFinisher frame+co ))
							:mates (list (Mate
								      :cap *default_AchiralAminoAcid*
								      :groupNames (list "AlphaAminoAcids" "BisAminoAcids" "AmineRingClosingAlphaAminoAcids" "CarbonylRingClosingAlphaAminoAcids" )))))
					(setq plug+nside (MetaOutPlug
							  :name "+nside"
							  :bond0 n-n
							  :exportFrame frame-n
							  :framesCompletedByCoupling ()
							  :mates (list (Mate
									:cap "hn"
									:groupNames (list "NSideChains" )))))
					(setq plug+camide (MetaOutPlug
							   :name "+camide"
							   :bond0 n-co
							   :exportFrame frame+quatcarbonyl
							   :framesCompletedByCoupling (list (MetaFrameFinisher frame-co ))
							   :mates (list (Mate
									 :cap *default_PrimaryAmine* ))))
					(setq plug+ester (MetaOutPlug
							  :name "+ester"
							  :bond0 n-co
							  :exportFrame frame+quatcarbonyl
							  :framesCompletedByCoupling (list (MetaFrameFinisher frame-co ))
							  :mates (list (Mate
									:cap *default_Ester* ))))
					(setq plug+secondn (MetaOutPlug
							    :name "+secondn"
							    :bond0 n-n
							    :exportFrame frame+secondn
							    :framesCompletedByCoupling (list (MetaFrameFinisher frame+n ))
							    :mates (list (Mate
									  :cap *default_AcylGroup*
									  :groupNames (list "AcylGroups" ))))))
			   :fragments (list (setq frag-co (MetaFragment
							   :name "frag-co"
							   :atoms (list n-oco )))
					    (setq frag+co (MetaFragment
							   :name "frag+co"
							   :atoms (list n+oco )))
					    (setq body (MetaRemainderFragment
							:residue residue
							:name "body"
							:removeFragments (list frag-co frag+co ))))
			   :topologies (list (MetaTopology
					      :name "hydantoinInDkpOut"
					      :residue residue
					      :plugs (list plug-trailingHydantoin plug+dkp plug+nside )
					      :coreFragment (MetaExtractCoreFragment
							     :fragment body )
					      :fragments (list (MetaExtractFragment
								:fragment frag-co
								:frame frame-co )
							       (MetaExtractFragment
								:fragment frag+co
								:frame frame+co ))
					      :alignTo (MetaRecognizedFrame
							:name "align"
							:origin n-ca
							:recognizer (MetaFrameRecognizer "cAlphaAmide" )))
					     (MetaTopology
					      :name "didkp"
					      :residue residue
					      :plugs (list plug-dkp plug+dkp plug+nside )
					      :coreFragment (MetaExtractCoreFragment
							     :fragment body )
					      :fragments (list (MetaExtractFragment
								:fragment frag-co
								:frame frame-co )
							       (MetaExtractFragment
								:fragment frag+co
								:frame frame+co ))
					      :alignTo (MetaRecognizedFrame
							:name "align"
							:origin n-ca
							:recognizer (MetaFrameRecognizer "cAlphaAmide" ))))))
    newConstitution ))




(defun def_AlphaAminoAcids ()
  (let (group name residue residueCharge
	      monomerName pdbName backboneName backboneAtom nca nn nco noco stereoInformation
	      groupAdder1 groupAdder2 newConstitution frame+peptide frame+core frame+co frame+n
	      frame+amine plug-origin plug-dkp plug-peptide plug-secondn plug-sidePeptide
	      plug+dkp plug+peptide plug+nside plug+acylMethylene_amine plug+amineSide frag_oc body )
    (setq group "AlphaAminoAcids" )
    (setq name (MetaProperty :name ))
    (setq residue (MetaResidue ))
    (setq residueCharge (MetaPropertyOrDefault :residueCharge 0 ))
    (setq monomerName (MetaProperty :monomerName ))
    (setq pdbName (MetaProperty :pdbName ))
    (setq backboneName (MetaProperty :backbone ))
    (setq backboneAtom (MetaResidueAtomWithName backboneName ))
    (setq nca (MetaRecognizedAtom "nca" backboneAtom (MetaFrameRecognizer "cAlphaAmide" )"1" ))
    (setq nn (MetaRecognizedAtom "nn" backboneAtom (MetaFrameRecognizer "cAlphaAmide" )"2" ))
    (setq nco (MetaRecognizedAtom "nco" backboneAtom (MetaFrameRecognizer "cAlphaAmide" )"3" ))
    (setq noco (MetaRecognizedAtom "noco" backboneAtom (MetaFrameRecognizer "cAlphaAmide" )"4" ))
    (setq stereoInformation (MetaStereoInformation_standard ))
    (setq groupAdder1 (MetaAddToGroupByStereoisomer
		       :groupPrefix (% "Reversed%s" group )
		       :stereoInformation stereoInformation
		       :separateByCenter "CA" ))
    (setq groupAdder2 (MetaAddToGroup :groupName group :constitutionName name ))
    (MetaConstitution
     :name name
     :group group
     :addToGroups (list groupAdder1 groupAdder2 )
     :residue residue
     :residueNetCharge residueCharge
     :stereoInformation stereoInformation
     :frames (list (setq frame+peptide (MetaRecognizedFrame
					:name "+peptide"
					:origin nn
					:recognizer (MetaFrameRecognizer "nAmideAlphaAminoAcid" )))
		   (setq frame+core (MetaRecognizedFrame
				     :name "+core" :origin nca :recognizer (MetaFrameRecognizer "cAlphaAmide" )))
		   (setq frame+co (MetaRecognizedFrame :name "+co" :origin nco :recognizer (MetaFrameRecognizer "coAmide" )))
		   (setq frame+n (MetaRecognizedFrame :name "+n" :origin nn :recognizer (MetaFrameRecognizer "nAmide" )))
		   (setq frame+amine (MetaRecognizedFrame
				      :name "+amine"
				      :origin nn
				      :recognizer (MetaFrameRecognizer "nAmine" ))))
     :plugs (list 
	     (setq plug-origin (MetaOriginPlug :name "-origin" :originFrame frame+core ))
	     (setq plug-dkp (MetaInPlug
			     :name "-dkp"
			     :bond0 nco
			     :bond1 nn
			     :mates (list (Mate :cap *default_AchiralAminoAcid*
						:groupNames (list "AlphaAminoAcids"
								  "BisAminoAcids"
								  "SecondaryAminoAcids"
								  "AmineRingClosingAlphaAminoAcids"
								  "ReversedBisAminoAcids"
								  "CarbonylRingClosingAlphaAminoAcids" )))))
	     (setq plug-peptide (MetaInPlug
				 :name "-peptide"
				 :bond0 nco
				 :mates (list (Mate :cap *default_PrimaryAmine* ))))
	     (setq plug-secondn (MetaInPlug
				 :name "-secondn"
				 :bond0 nco
				 :mates (list (Mate :cap *default_SecondaryAminoAcidS* ))))
	     (setq plug-sidePeptide (MetaInPlug
				     :name "-sidePeptide"
				     :bond0 nco
				     :mates (list (Mate
						   :cap *default_PrimaryAmine*
						   :overrideCapMateRequirement true
						   :groupNames (list "AmineRingClosingAlphaAminoAcids" )))))
	     (setq plug+dkp (MetaOutPlug
			     :name "+dkp"
			     :bond0 nn
			     :bond1 nco
			     :exportFrame frame+core
			     :framesCompletedByCoupling (list (MetaFrameFinisher frame+n )
							      (MetaFrameFinisher frame+co ))
			     :mates (list (Mate :cap *default_AchiralAminoAcid*
						:groupNames (list "AlphaAminoAcids"
								  "BisAminoAcids"
								  "SecondaryAminoAcids"
								  "AmineRingClosingAlphaAminoAcids"
								  "ReversedBisAminoAcids"
								  "CarbonylRingClosingAlphaAminoAcids" )))))
	     (setq plug+peptide (MetaOutPlug
				 :name "+peptide"
				 :bond0 nn
				 :exportFrame frame+peptide
				 :framesCompletedByCoupling (list (MetaFrameFinisher frame+n ))
				 :mates (list (Mate :cap *default_AcylGroup*
						    :groupNames (list
								 "AlphaAminoAcids"
								 "BisAminoAcids"
								 "AcylGroups"
								 "SecondaryAminoAcids"
								 "AmineRingClosingAlphaAminoAcids"
								 "CarbonylRingClosingAlphaAminoAcids" )))))
	     (setq plug+nside (MetaOutPlug :name "+nside"
					   :bond0 nn
					   :exportFrame frame+n
					   :framesCompletedByCoupling ()
					   :mates (list (Mate :cap "hn" :groupNames (list "NSideChains" )))))
	     (setq plug+acylMethylene_amine (MetaRingClosingPlug
					     :name "+acylMethylene_amine"
					     :otherSidePlugName "+acylMethylene_methylene"
					     :bond0 nn
					     :exportFrame frame+peptide
					     :framesCompletedByCoupling (list (MetaFrameFinisher frame+amine ))
					     :mates (list (Mate :cap *default_AmineAcylMethylene* ))
					     :ringClosingMates (list (RingClosingMate
								      :groupNames (list "AcylMethyleneRingClosingAcylGroups" )))))
	     (setq plug+amineSide (MetaOutPlug
				   :name "+amineSide"
				   :bond0 nn
				   :exportFrame frame+amine
				   :framesCompletedByCoupling ()
				   :mates (list (Mate
						 :cap "hn"
						 :groupNames (list "NSideChains" ))))))
     :fragments (list (setq frag_oc (MetaFragment :name "oc" :atoms (list noco )))
		      (setq body (MetaRemainderFragment :residue residue
							:name "body"
							:removeFragments (list frag_oc ))))
     :topologies (list (MetaTopology
			:name "peptide"
			:residue residue
			:plugs (list plug-peptide plug+peptide plug+nside )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:fragments (list (MetaExtractFragment
					  :fragment frag_oc
					  :frame frame+co ))
			:alignTo (MetaRecognizedFrame
				  :name "align"
				  :origin nca
				  :recognizer (MetaFrameRecognizer "cAlphaAmide" )))
		       (MetaTopology
			:name "acylMethylenePeptide"
			:residue residue
			:plugs (list plug-peptide plug+acylMethylene_amine plug+amineSide )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:fragments (list (MetaExtractFragment
					  :fragment frag_oc
					  :frame frame+co ))
			:alignTo (MetaRecognizedFrame
				  :name "align"
				  :origin nca
				  :recognizer (MetaFrameRecognizer "cAlphaAmide" )))
		       (MetaTopology
			:name "acylMethyleneSecondN"
			:residue residue
			:plugs (list plug-secondn plug+acylMethylene_amine plug+amineSide )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:fragments (list (MetaExtractFragment
					  :fragment frag_oc
					  :frame frame+co ))
			:alignTo (MetaRecognizedFrame
				  :name "align"
				  :origin nca
				  :recognizer (MetaFrameRecognizer "cAlphaAmide" )))
		       (MetaTopology
			:name "secondNPeptide"
			:residue residue
			:plugs (list plug-secondn plug+peptide plug+nside )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:fragments (list (MetaExtractFragment
					  :fragment frag_oc
					  :frame frame+co ))
			:alignTo (MetaRecognizedFrame
				  :name "align"
				  :origin nca
				  :recognizer (MetaFrameRecognizer "cAlphaAmide" )))
		       (MetaTopology
			:name "sidePeptide"
			:residue residue
			:plugs (list plug-sidePeptide plug+peptide plug+nside )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:fragments (list (MetaExtractFragment
					  :fragment frag_oc
					  :frame frame+co ))
			:alignTo (MetaRecognizedFrame
				  :name "align"
				  :origin nca
				  :recognizer (MetaFrameRecognizer "cAlphaAmide" )))
		       (MetaTopology
			:name "dkpIn"
			:residue residue
			:plugs (list plug-dkp plug+nside )
			:coreFragment (MetaExtractCoreFragment :fragment body )
			:fragments (list (MetaExtractFragment :fragment frag_oc :frame frame+co ))
			:alignTo (MetaRecognizedFrame
				  :name "align"
				  :origin nca
				  :recognizer (MetaFrameRecognizer "cAlphaAmide" )))
		       (MetaTopology
			:name "dkpOut"
			:residue residue
			:plugs (list plug-origin plug+dkp plug+nside )
			:coreFragment (MetaExtractCoreFragment :fragment body )
			:fragments (list (MetaExtractFragment :fragment frag_oc :frame frame+co ))
			:alignTo (MetaRecognizedFrame :name "align"
						      :origin nca
						      :recognizer (MetaFrameRecognizer "cAlphaAmide" )))))))





;; SideChain Constitution
;; fragment must have properties
;;	:name [constitution name]
;;	:type nSideChain
;;	:monomerName [ XXX ]
;;	:pdbName [ YYY ]
;;	:nside	[CB]
;; (optional) :constrainPi (list (ConstrainedPiBond config piAtom1Name piAtom2Name ) )
;;	config is either "E" or "Z"
;;  

(defun def_SideChain (&key (groupName ())
			   (groupAdds ())
			   (inPlug ())
			   (ringClosingInfo ()))
  (let (group residue residueCharge name ringClosingFrame ringClosingOutPlug newConstitution body )
    (assert (notNil groupName )"You must provide groupName" )
    (setq group groupName )
    (setq residue (MetaResidue ))
    (setq residueCharge (MetaPropertyOrDefault :residueCharge 0 ))
    (setq name (MetaProperty :name ))
    (setq ringClosingFrame ())
    (setq ringClosingOutPlug ())
    (when (notNil ringClosingInfo )
      (setq ringClosingFrame (lookup ringClosingInfo :frame ))
      (setq ringClosingOutPlug (lookup ringClosingInfo :plug )))
    (setq newConstitution (MetaConstitution
			   :name name
			   :group group
			   :addToGroups groupAdds
			   :residue residue
			   :residueNetCharge residueCharge
			   :stereoInformation (MetaStereoInformation_standard )
			   :frames (list ringClosingFrame )
			   :plugs (list inPlug ringClosingOutPlug )
			   :fragments (list (setq body (MetaRemainderFragment
							:residue residue
							:name "body" )))
			   :topologies (list (MetaTopology
					      :name "nin"
					      :residue residue
					      :plugs (list inPlug ringClosingOutPlug )
					      :coreFragment (MetaExtractCoreFragment
							     :fragment body )
					      :fragments ()
					      :alignTo (MetaAnchorOnOtherSideOfPlug
							:plugName (getName inPlug ))))))
    newConstitution  
    ))
					;    setq meta_NSideChains ( def_SideChain 
					;	:inPlug (MetaInPlug :name "-nside" :bond0 (MetaProperty :root)
					;		    	:mates (list (Mate :cap "pro4(2S4S)" ) ) ) )
					;    [ mdb addMetaConstitution meta_NSideChains ]
(defun def_NSideChains (&key (ringClosingInfo ()))
  (let (group residue residueCharge name ringClosingFrame ringClosingOutPlug inPlugFromAmide inPlugFromAmine newConstitution body )
    (setq group "NSideChains" )
    (setq residue (MetaResidue ))
    (setq residueCharge (MetaPropertyOrDefault :residueCharge 0 ))
    (setq name (MetaProperty :name ))
    (setq ringClosingFrame ())
    (setq ringClosingOutPlug ())
    (setq inPlugFromAmide (MetaInPlug :name "-nside" :bond0 (MetaProperty :root ) :mates (list (Mate :cap "pro4(2S4S)" ))))
    (setq inPlugFromAmine (MetaInPlug :name "-amineSide" :bond0 (MetaProperty :root ) :mates (list (Mate :cap "ala(S)" ))))
    (when (notNil ringClosingInfo )
      (setq ringClosingFrame (lookup ringClosingInfo :frame ))
      (setq ringClosingOutPlug (lookup ringClosingInfo :plug )))
    (setq newConstitution (MetaConstitution
			   :name name
			   :group group
			   :addToGroups (list (MetaAddToGroup
					       :groupName "NSideChains" ))
			   :residue residue
			   :residueNetCharge residueCharge
			   :stereoInformation (MetaStereoInformation_standard )
			   :frames (list ringClosingFrame )
			   :plugs (list inPlugFromAmide ringClosingOutPlug )
			   :fragments (list (setq body (MetaRemainderFragment
							:residue residue
							:name "body" )))
			   :topologies (list (MetaTopology
					      :name "nin"
					      :residue residue
					      :plugs (list inPlugFromAmide ringClosingOutPlug )
					      :coreFragment (MetaExtractCoreFragment
							     :fragment body )
					      :fragments ()
					      :alignTo (MetaAnchorOnOtherSideOfPlug
							:plugName (getName inPlugFromAmide ))))))
    newConstitution ))
(defun def_NHSideChain ()
  (let (group residue residueCharge name inPlugFromAmide inPlugFromAmine newConstitution body )
    (setq group "NHSideChain" )
    (setq residue (MetaResidue ))
    (setq residueCharge (MetaPropertyOrDefault :residueCharge 0 ))
    (setq name (MetaProperty :name ))
    (setq inPlugFromAmide (MetaInPlug :name "-nside" :bond0 (MetaProperty :root ) :mates (list (Mate :cap "pro4(2S4S)" ))))
    (setq inPlugFromAmine (MetaInPlug :name "-amineSide" :bond0 (MetaProperty :root ) :mates (list (Mate :cap "ala(S)" ))))
    (setq newConstitution (MetaConstitution
			   :name name
			   :group group
			   :addToGroups (list (MetaAddToGroup
					       :groupName "NHSideChain" ))
			   :residue residue
			   :residueNetCharge residueCharge
			   :stereoInformation (MetaStereoInformation_standard )
			   :frames (list )
			   :plugs (list inPlugFromAmide inPlugFromAmine )
			   :fragments (list (setq body (MetaRemainderFragment
							:residue residue
							:name "body" )))
			   :topologies (list (MetaTopology
					      :name "nin"
					      :residue residue
					      :plugs (list inPlugFromAmide )
					      :coreFragment (MetaExtractCoreFragment
							     :fragment body )
					      :fragments ()
					      :alignTo (MetaAnchorOnOtherSideOfPlug
							:plugName (getName inPlugFromAmide )))
					     (MetaTopology
					      :name "aminein"
					      :residue residue
					      :plugs (list inPlugFromAmine )
					      :coreFragment (MetaExtractCoreFragment
							     :fragment body )
					      :fragments ()
					      :alignTo (MetaAnchorOnOtherSideOfPlug
							:plugName (getName inPlugFromAmine ))))))
    newConstitution ))


(defun def_AmineRingClosingAlphaAminoAcids (sideAtomRecognizerName afterSideFrameRecognizerName )
  (let (group name residue residueCharge monomerName pdbName backboneName backboneAtom
	      nca nn nco noco sideName sideAtom side1 side2 side3 side4
	      stereoInformation groupAdder1 groupAdder2 
	      frame+peptide frame+core frame+co frame+n frame+side
	      frame+sideAfterCoupling plug-origin plug-dkp plug-peptide plug-sidePeptide
	      plug+dkp plug+peptide plug+nside plug+sideAmideN plug+sidePeptide
	      plug+acylMethylene_amine
	      frag_oc frag_sideNh body )
    (setq group "AmineRingClosingAlphaAminoAcids" )
    (setq name (MetaProperty :name ))
    (setq residue (MetaResidue ))
    (setq residueCharge (MetaPropertyOrDefault :residueCharge 0 ))
    (setq monomerName (MetaProperty :monomerName ))
    (setq pdbName (MetaProperty :pdbName ))
    (setq backboneName (MetaProperty :backbone ))
    (setq backboneAtom (MetaResidueAtomWithName backboneName ))
    (setq nca (MetaRecognizedAtom "nca" backboneAtom (MetaFrameRecognizer "cAlphaAmide" )"1" ))
    (setq nn (MetaRecognizedAtom "nn" backboneAtom (MetaFrameRecognizer "cAlphaAmide" )"2" ))
    (setq nco (MetaRecognizedAtom "nco" backboneAtom (MetaFrameRecognizer "cAlphaAmide" )"3" ))
    (setq noco (MetaRecognizedAtom "noco" backboneAtom (MetaFrameRecognizer "cAlphaAmide" )"4" ))
    (setq sideName (MetaProperty :ringSide ))
    (setq sideAtom (MetaResidueAtomWithName sideName ))
    (setq side1 (MetaRecognizedAtom "side1" sideAtom (MetaFrameRecognizer sideAtomRecognizerName )"1" ))
    (setq side2 (MetaRecognizedAtom "side2" sideAtom (MetaFrameRecognizer sideAtomRecognizerName )"2" ))
    (setq side3 (MetaRecognizedAtom "side3" sideAtom (MetaFrameRecognizer sideAtomRecognizerName )"3" ))
    (setq side4 (MetaRecognizedAtom "side4" sideAtom (MetaFrameRecognizer sideAtomRecognizerName )"4" ))
    (setq stereoInformation (MetaStereoInformation_standard ))
    (setq groupAdder1 (MetaAddToGroupByStereoisomer
		       :groupPrefix (% "Reversed%s" group )
		       :stereoInformation stereoInformation
		       :separateByCenter "CA" ))
    (setq groupAdder2 (MetaAddToGroup
		       :groupName group
		       :constitutionName name ))
    (MetaConstitution
     :name name
     :group group
     :addToGroups (list groupAdder1 groupAdder2 )
     :residue residue
     :residueNetCharge residueCharge
     :stereoInformation stereoInformation
     :frames (list
	      (setq frame+peptide (MetaRecognizedFrame
				   :name "+peptide"
				   :origin nn
				   :recognizer (MetaFrameRecognizer "nAmideAlphaAminoAcid" )))
	      (setq frame+core (MetaRecognizedFrame
				:name "+core"
				:origin nca
				:recognizer (MetaFrameRecognizer "cAlphaAmide" )))
	      (setq frame+co (MetaRecognizedFrame
			      :name "+co"
			      :origin nco
			      :recognizer (MetaFrameRecognizer "coAmide" )))
	      (setq frame+n (MetaRecognizedFrame
			     :name "+n"
			     :origin nn
			     :recognizer (MetaFrameRecognizer "nAmide" )))
	      (setq frame+side (MetaRecognizedFrame
				:name "+side"
				:origin side1
				:recognizer (MetaFrameRecognizer sideAtomRecognizerName )))
	      (setq frame+sideAfterCoupling (MetaRecognizedFrame
					     :name "+sideAfterCoupling"
					     :origin side1
					     :recognizer (MetaFrameRecognizer afterSideFrameRecognizerName ))))
     :plugs (list
	     (setq plug-origin (MetaOriginPlug
				:name "-origin"
				:originFrame frame+core ))
	     (setq plug-dkp (MetaInPlug
			     :name "-dkp"
			     :bond0 nco
			     :bond1 nn
			     :mates (list (Mate
					   :cap *default_AchiralAminoAcid*
					   :groupNames (list "BisAminoAcids" "ReversedBisAminoAcids" "AlphaAminoAcids" )))))
	     (setq plug-peptide (MetaInPlug
				 :name "-peptide"
				 :bond0 nco
				 :mates (list (Mate
					       :cap *default_PrimaryAmine*
					       :groupNames (list "AlphaAminoAcids" "Amines" "SecondaryAminoAcids" )))))
	     (setq plug-sidePeptide (MetaInPlug
				     :name "-sidePeptide"
				     :bond0 nco
				     :mates (list (Mate
						   :cap *default_PrimaryAmine*
						   :overrideCapMateRequirement true
						   :groupNames (list "AmineRingClosingAlphaAminoAcids" )))))
	     (setq plug+dkp (MetaOutPlug
			     :name "+dkp"
			     :bond0 nn
			     :bond1 nco
			     :exportFrame frame+core
			     :framesCompletedByCoupling (list (MetaFrameFinisher frame+n )
							      (MetaFrameFinisher frame+co ))
			     :mates (list (Mate
					   :cap *default_AchiralAminoAcid*
					   :groupNames (list "BisAminoAcids" "ReversedBisAminoAcids" "AlphaAminoAcids" )))))
	     (setq plug+peptide (MetaOutPlug
				 :name "+peptide"
				 :bond0 nn
				 :exportFrame frame+peptide
				 :framesCompletedByCoupling (list (MetaFrameFinisher frame+n ))
				 :mates (list (Mate
					       :cap *default_AcylGroup*
					       :groupNames (list "AlphaAminoAcids" "BisAminoAcids" "AcylGroups" "SecondaryAminoAcids" )))))
	     (setq plug+nside (MetaOutPlug
			       :name "+nside"
			       :bond0 nn
			       :exportFrame frame+n
			       :framesCompletedByCoupling ()
			       :mates (list (Mate
					     :cap "hn"
					     :groupNames (list "NSideChains" )))))
	     (setq plug+sideAmideN (MetaRingClosingPlug
				    :name "+sideAmideN"
				    :otherSidePlugName "+sideAmideC"
				    :bond0 side1
				    :exportFrame frame+side
				    :framesCompletedByCoupling (list (MetaFrameFinisher frame+sideAfterCoupling ))
				    :mates (list (Mate
						  :cap *default_AcylGroup*
						  :groupNames (list "AcylGroups" "CarbonylRingClosingAlphaAminoAcids" )))
				    :ringClosingMates (list (RingClosingMate
							     :groupNames (list "CarbonylRingClosingAlphaAminoAcids" )))))
	     (setq plug+sidePeptide (MetaOutPlug
				     :name "+sidePeptide"
				     :bond0 side1
				     :exportFrame frame+side
				     :framesCompletedByCoupling (list (MetaFrameFinisher frame+sideAfterCoupling ))
				     :mates (list (Mate
						   :cap *default_AcylGroup*
						   :overrideCapMateRequirement true
						   :groupNames (list "AlphaAminoAcids" "BisAminoAcids"
								     "AmineRingClosingAlphaAminoAcids"
								     "CarbonylRingClosingAlphaAminoAcids" )))))
	     (setq plug+acylMethylene_amine (MetaRingClosingPlug
					     :name "+acylMethylene_amine"
					     :otherSidePlugName "+acylMethylene_methylene"
					     :bond0 side1
					     :exportFrame frame+side
					     :framesCompletedByCoupling (list (MetaFrameFinisher frame+sideAfterCoupling ))
					     :mates (list (Mate :cap *default_AmineAcylMethylene* ))
					     :ringClosingMates (list (RingClosingMate 
								      :groupNames (list "AcylMethyleneRingClosingAcylGroups"))
								     ))))
     :fragments (list (setq frag_oc (MetaFragment :name "oc" :atoms (list noco )))
		      (setq frag_sideNh (MetaFragment :name "sideNh" :atoms (list side4 )))
		      (setq body (MetaRemainderFragment :residue residue
							:name "body"
							:removeFragments (list frag_oc frag_sideNh ))))
     :topologies (list (MetaTopology
			:name "dkpIn"
			:residue residue
			:plugs (list plug-dkp plug+nside plug+sideAmideN )
			:coreFragment (MetaExtractCoreFragment :fragment body )
			:fragments (list (MetaExtractFragment
					  :fragment frag_oc
					  :frame frame+co )
					 (MetaExtractFragment
					  :fragment frag_sideNh
					  :frame frame+sideAfterCoupling ))
			:alignTo (MetaRecognizedFrame
				  :name "align"
				  :origin nca
				  :recognizer (MetaFrameRecognizer "cAlphaAmide" )))
		       (MetaTopology
			:name "dkpOut"
			:residue residue
			:plugs (list plug-origin plug+dkp plug+nside plug+sideAmideN )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:fragments (list (MetaExtractFragment :fragment frag_oc :frame frame+co )
					 (MetaExtractFragment :fragment frag_sideNh :frame frame+sideAfterCoupling ))
			:alignTo (MetaRecognizedFrame
				  :name "align"
				  :origin nca
				  :recognizer (MetaFrameRecognizer "cAlphaAmide" )))
		       (MetaTopology
			:name "dkpInPeptideOut"
			:residue residue
			:plugs (list plug-dkp plug+nside plug+sidePeptide )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:fragments (list (MetaExtractFragment :fragment frag_oc :frame frame+co )
					 (MetaExtractFragment :fragment frag_sideNh :frame frame+sideAfterCoupling ))
			:alignTo (MetaRecognizedFrame
				  :name "align"
				  :origin nca
				  :recognizer (MetaFrameRecognizer "cAlphaAmide" ))
			)
		       (MetaTopology
			:name "dkpInAmineOut"
			:residue residue
			:plugs (list plug-dkp plug+nside plug+acylMethylene_amine )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:fragments (list (MetaExtractFragment :fragment frag_oc :frame frame+co )
					 (MetaExtractFragment :fragment frag_sideNh :frame frame+sideAfterCoupling ))
			:alignTo (MetaRecognizedFrame
				  :name "align"
				  :origin nca
				  :recognizer (MetaFrameRecognizer "cAlphaAmide" ))
			)
		       )
     )))
;;
;; Parameters 
;;	sideFrameRecognizer == recognizes the side atom 
;;      		 and the heavy atom alpha and beta to it for a frame
;;	afterSideFrameRecognizer == recognizes the side atom 
;;      		and the incomplete frame that is completed once the next 
;;		monomer is attached :eg if you create an amide through a sidechain the NH will be in a separate fragment
;;		and need the afterSideFrameRecognizer to recognize its molecular context
;;
;;




(defun def_CarbonylRingClosingAlphaAminoAcids (sideAtomRecognizer afterSideFrameRecognizer )
  (let (group name residue residueCharge monomerName pdbName backboneName backboneAtom
	      nca nn nco noco sideName sideAtom side1 side2 side3 sideSide stereoInformation
	      groupAdder1 groupAdder2 )
    (setq group "CarbonylRingClosingAlphaAminoAcids" )
    (setq name (MetaProperty :name ))
    (setq residue (MetaResidue ))
    (setq residueCharge (MetaPropertyOrDefault :residueCharge 0 ))
    (setq monomerName (MetaProperty :monomerName ))
    (setq pdbName (MetaProperty :pdbName ))
    (setq backboneName (MetaProperty :backbone ))
    (setq backboneAtom (MetaResidueAtomWithName backboneName ))
    (setq nca (MetaRecognizedAtom "nca" backboneAtom (MetaFrameRecognizer "cAlphaAmide" )"1" ))
    (setq nn (MetaRecognizedAtom "nn" backboneAtom (MetaFrameRecognizer "cAlphaAmide" )"2" ))
    (setq nco (MetaRecognizedAtom "nco" backboneAtom (MetaFrameRecognizer "cAlphaAmide" )"3" ))
    (setq noco (MetaRecognizedAtom "noco" backboneAtom (MetaFrameRecognizer "cAlphaAmide" )"4" ))
    (setq sideName (MetaProperty :ringSide ))
    (setq sideAtom (MetaResidueAtomWithName sideName ))
    (setq side1 (MetaRecognizedAtom "side1" sideAtom sideAtomRecognizer "1" ))
    (setq side2 (MetaRecognizedAtom "side2" sideAtom sideAtomRecognizer "2" ))
    (setq side3 (MetaRecognizedAtom "side3" sideAtom sideAtomRecognizer "3" ))
    (setq sideSide (MetaRecognizedAtom "sideSide" sideAtom sideAtomRecognizer "4" ))
    (setq stereoInformation (MetaStereoInformation_standard ))
    (setq groupAdder1 (MetaAddToGroupByStereoisomer :groupPrefix (% "Reversed%s" group ) :stereoInformation stereoInformation :separateByCenter "CA" ))
    (setq groupAdder2 (MetaAddToGroup :groupName group :constitutionName name ))
    (MetaConstitution
     :name name
     :group group
     :addToGroups (list groupAdder1 groupAdder2 )
     :residue residue
     :residueNetCharge residueCharge
     :stereoInformation stereoInformation
     :frames (list (setq frame+peptide (MetaRecognizedFrame
					:name "+peptide"
					:origin nn
					:recognizer (MetaFrameRecognizer "nAmideAlphaAminoAcid" )))
		   (setq frame+core (MetaRecognizedFrame
				     :name "+core"
				     :origin nca
				     :recognizer (MetaFrameRecognizer "cAlphaAmide" )))
		   (setq frame+co (MetaRecognizedFrame
				   :name "+co"
				   :origin nco
				   :recognizer (MetaFrameRecognizer "coAmide" )))
		   (setq frame+n (MetaRecognizedFrame
				  :name "+n"
				  :origin nn
				  :recognizer (MetaFrameRecognizer "nAmide" )))
		   (setq frame+side (MetaRecognizedFrame
				     :name "+side"
				     :origin side1
				     :recognizer sideAtomRecognizer ))
		   (setq frame+sideSide (MetaRecognizedFrame
					 :name "+sideSide"
					 :origin side1
					 :recognizer afterSideFrameRecognizer )))
     :plugs (list (setq plug-origin (MetaOriginPlug
				     :name "-origin"
				     :originFrame frame+core ))
		  (setq plug-dkp (MetaInPlug
				  :name "-dkp"
				  :bond0 nco
				  :bond1 nn
				  :mates (list (Mate
						:cap *default_AchiralAminoAcid*
						:groupNames (list "BisAminoAcids" "ReversedBisAminoAcids" "AlphaAminoAcids" )))))
		  (setq plug-peptide (MetaInPlug
				      :name "-peptide"
				      :bond0 nco
				      :mates (list (Mate
						    :cap *default_PrimaryAmine*
						    :groupNames (list "AlphaAminoAcids" "Amines" "SecondaryAminoAcids" )))))
		  (setq plug-sidePeptide (MetaInPlug
					  :name "-sidePeptide"
					  :bond0 nco
					  :mates (list (Mate
							:cap *default_PrimaryAmine*
							:overrideCapMateRequirement true
							:groupNames (list "AmineRingClosingAlphaAminoAcids" )))))
		  (setq plug+dkp (MetaOutPlug
				  :name "+dkp"
				  :bond0 nn
				  :bond1 nco
				  :exportFrame frame+core
				  :framesCompletedByCoupling (list (MetaFrameFinisher frame+n )
							     (MetaFrameFinisher frame+co ))
				  :mates (list (Mate
						:cap *default_AchiralAminoAcid*
						:groupNames (list "BisAminoAcids" "ReversedBisAminoAcids" "AlphaAminoAcids" )))))
		  (setq plug+peptide (MetaOutPlug
				      :name "+peptide"
				      :bond0 nn
				      :exportFrame frame+peptide
				      :framesCompletedByCoupling (list (MetaFrameFinisher frame+n ))
				      :mates (list (Mate
						    :cap *default_AcylGroup*
						    :groupNames (list "AlphaAminoAcids" "BisAminoAcids" "AcylGroups" "SecondaryAminoAcids" )))))
		  (setq plug+nside (MetaOutPlug
				    :name "+nside"
				    :bond0 nn
				    :exportFrame frame+n
				    :framesCompletedByCoupling ()
				    :mates (list (Mate
						  :cap "hn"
						  :groupNames (list "NSideChains" )))))
		  (setq plug-sideAmideN (MetaInPlug
					 :name "-sideAmideN"
					 :bond0 side1
					 :mates (list (Mate
						       :cap *default_PrimaryAmine*
						       :groupNames (list "Amines" "AmineRingClosingAlphaAminoAcids" )))))
		  (setq plug+sideAmideC (MetaRingClosingPlug
					 :name "+sideAmideC"
					 :bond0 side1
					 :exportFrame frame+side
					 :framesCompletedByCoupling (list (MetaFrameFinisher frame+sideSide ))
					 :mates (list (Mate
						       :cap *default_PrimaryAmine*
						       :groupNames (list "Amines" )))
					 :ringClosingMates (list (RingClosingMate
								  :groupNames (list "AmineRingClosingAlphaAminoAcids" ))))))
     :fragments (list (setq frag_oc (MetaFragment
				     :name "oc"
				     :atoms (list noco )))
		      (setq frag_sideSide (MetaFragment
					   :name "sideSide"
					   :atoms (list sideSide )))
		      (setq body (MetaRemainderFragment
				  :residue residue
				  :name "body"
				  :removeFragments (list frag_oc frag_sideSide ))))
     :topologies (list (MetaTopology
			:name "peptide"
			:residue residue
			:plugs (list plug-peptide plug+peptide plug+nside plug+sideAmideC )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:fragments (list (MetaExtractFragment
					  :fragment frag_oc
					  :frame frame+co )
					 (MetaExtractFragment
					  :fragment frag_sideSide
					  :frame frame+sideSide ))
			:alignTo (MetaRecognizedFrame
				  :name "align"
				  :origin nca
				  :recognizer (MetaFrameRecognizer "cAlphaAmide" )))
		       (MetaTopology
			:name "sidePeptide"
			:residue residue
			:plugs (list plug-sidePeptide plug+peptide plug+nside plug+sideAmideC )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:fragments (list (MetaExtractFragment
					  :fragment frag_oc
					  :frame frame+co )
					 (MetaExtractFragment
					  :fragment frag_sideSide
					  :frame frame+sideSide ))
			:alignTo (MetaRecognizedFrame
				  :name "align"
				  :origin nca
				  :recognizer (MetaFrameRecognizer "cAlphaAmide" )))
		       (MetaTopology
			:name "dkpIn"
			:residue residue
			:plugs (list plug-dkp plug+nside plug+sideAmideC )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:fragments (list (MetaExtractFragment
					  :fragment frag_oc
					  :frame frame+co )
					 (MetaExtractFragment
					  :fragment frag_sideSide
					  :frame frame+sideSide ))
			:alignTo (MetaRecognizedFrame
				  :name "align"
				  :origin nca
				  :recognizer (MetaFrameRecognizer "cAlphaAmide" )))
		       (MetaTopology
			:name "sideInDkpOut"
			:residue residue
			:plugs (list plug-sideAmideN plug+dkp plug+nside )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:fragments (list (MetaExtractFragment
					  :fragment frag_oc
					  :frame frame+co )
					 (MetaExtractFragment
					  :fragment frag_sideSide
					  :frame frame+sideSide ))
			:alignTo (MetaRecognizedFrame
				  :name "align"
				  :origin nca
				  :recognizer (MetaFrameRecognizer "cAlphaAmide" )))))))





(defun def_Hydantoins ()
  (let (group name residue residueCharge monomerName pdbName
	      imideName imideAtom nn nco noco stereoInformation
	      groupAdder1 newConstitution frame+core frame+nImide
	      plug-origin plug-leadingHydantoin
	      plug+trailingHydantoin plug+imide body )
    (setq group "Hydantoins" )
    (setq name (MetaProperty :name ))
    (setq residue (MetaResidue ))
    (setq residueCharge (MetaPropertyOrDefault :residueCharge 0 ))
    (setq monomerName (MetaProperty :monomerName ))
    (setq pdbName (MetaProperty :pdbName ))
    (setq imideName (MetaProperty :imide ))
    (setq imideAtom (MetaResidueAtomWithName imideName ))
    (setq nn (MetaRecognizedAtom "nn" imideAtom (MetaFrameRecognizer "hydantoinImide" )"1" ))
    (setq nco (MetaRecognizedAtom "nco" imideAtom (MetaFrameRecognizer "hydantoinImide" )"2" ))
    (setq noco (MetaRecognizedAtom "noco" imideAtom (MetaFrameRecognizer "hydantoinImide" )"3" ))
    (setq stereoInformation (MetaStereoInformation_standard ))
    (setq groupAdder1 (MetaAddToGroup :groupName group :constitutionName name ))
    (MetaConstitution
     :name name
     :group group
     :addToGroups (list groupAdder1 )
     :residue residue
     :residueNetCharge residueCharge
     :stereoInformation stereoInformation
     :frames (list (setq frame+core (MetaRecognizedFrame
				     :name "+core"
				     :origin nn
				     :recognizer (MetaFrameRecognizer "hydantoinImide" )))
		   (setq frame+nImide (MetaRecognizedFrame
				       :name "+n"
				       :origin nn
				       :recognizer (MetaFrameRecognizer "nImide" ))))
     :plugs (list (setq plug-origin (MetaOriginPlug
				     :name "-origin"
				     :originFrame frame+core ))
		  (setq plug-leadingHydantoin (MetaInPlug
					       :name "-leadingHydantoin"
					       :bond0 nco
					       :bond1 nn
					       :mates (list (Mate
							     :cap *default_BisAminoAcid*
							     :groupNames (list "BisAminoAcids" )))))
		  (setq plug+trailingHydantoin (MetaOutPlug
						:name "+trailingHydantoin"
						:bond0 nn
						:bond1 nco
						:exportFrame frame+core
						:framesCompletedByCoupling (list (MetaFrameFinisher frame+nImide ))
						:mates (list (Mate
							      :cap *default_ReversedBisAminoAcid-S*
							      :groupNames (list "ReversedBisAminoAcids" )))))
		  (setq plug+imide (MetaOutPlug
				    :name "+imide"
				    :bond0 nn
				    :exportFrame frame+nImide
				    :framesCompletedByCoupling ()
				    :mates (list (Mate
						  :cap *default_ImideSideChain*
						  :groupNames (list "ImideSideChains" ))))))
     :fragments (list (setq body (MetaRemainderFragment
				  :residue residue
				  :name "body"
				  :removeFragments ())))
     :topologies (list (MetaTopology
			:name "hydantoinOut"
			:residue residue
			:plugs (list plug-origin plug+trailingHydantoin plug+imide )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:fragments ()
			:alignTo (MetaRecognizedFrame
				  :name "align"
				  :origin nn
				  :recognizer (MetaFrameRecognizer "hydantoinImide" )))
		       (MetaTopology
			:name "hydantoinIn"
			:residue residue
			:plugs (list plug-leadingHydantoin plug+imide )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:fragments ()
			:alignTo (MetaRecognizedFrame
				  :name "align"
				  :origin nn
				  :recognizer (MetaFrameRecognizer "hydantoinImide" )))))))

					; chiralSecondaryAminoAcid Constitution
					;    fragment must have :properties
					;	:name [constitution name]
					;	:type [aminoAcid]
					;	:monomerName [ pro(%1%) ]
					;	:pdbName "PR%1%"
					;	:backbone "CA"
					;
					;
(defun def_SecondaryAminoAcids ()
  (let (group name residue residueCharge monomerName pdbName backboneName backboneAtom nca
	      nn nco noco stereoInformation groupAdder1 groupAdder2 newConstitution
	      frame+co frame+n frame+core plug-origin plug-dkp plug-peptide
	      plug+dkp plug+peptide plug+secondn frag_oc body )
    (setq group "SecondaryAminoAcids" )
    (setq name (MetaProperty :name ))
    (setq residue (MetaResidue ))
    (setq residueCharge (MetaPropertyOrDefault :residueCharge 0 ))
    (setq monomerName (MetaProperty :monomerName ))
    (setq pdbName (MetaProperty :pdbName ))
    (setq backboneName (MetaProperty :backbone ))
    (setq backboneAtom (MetaResidueAtomWithName backboneName ))
    (setq nca (MetaRecognizedAtom "nca" backboneAtom (MetaFrameRecognizer "cAlphaAmide" )"1" ))
    (setq nn (MetaRecognizedAtom "nn" backboneAtom (MetaFrameRecognizer "cAlphaAmide" )"2" ))
    (setq nco (MetaRecognizedAtom "nco" backboneAtom (MetaFrameRecognizer "cAlphaAmide" )"3" ))
    (setq noco (MetaRecognizedAtom "noco" backboneAtom (MetaFrameRecognizer "cAlphaAmide" )"4" ))
    (setq stereoInformation (MetaStereoInformation_standard ))
    (setq groupAdder1 (MetaAddToGroupByStereoisomer :groupPrefix (% "Reversed%s" group ) :stereoInformation stereoInformation :separateByCenter "CA" ))
    (setq groupAdder2 (MetaAddToGroup :groupName group :constitutionName name ))
    (MetaConstitution
     :name name
     :group group
     :addToGroups (list groupAdder1 groupAdder2 )
     :residue residue
     :residueNetCharge residueCharge
     :stereoInformation stereoInformation
     :frames (list (setq frame+co (MetaRecognizedFrame
				   :name "+co"
				   :origin nco
				   :recognizer (MetaFrameRecognizer "coAmide" )))
		   (setq frame+n (MetaRecognizedFrame
				  :name "+n"
				  :origin nn
				  :recognizer (MetaFrameRecognizer "nAmide" )))
		   (setq frame+core (MetaRecognizedFrame
				     :name "+core"
				     :origin nca
				     :recognizer (MetaFrameRecognizer "cAlphaAmide" ))))
     :plugs (list (setq plug-origin (MetaOriginPlug
				     :name "-origin"
				     :originFrame frame+core ))
		  (setq plug-dkp (MetaInPlug
				  :name "-dkp"
				  :bond0 nco
				  :bond1 nn
				  :mates (list (Mate
						:cap *default_AchiralAminoAcid*
						:groupNames (list "BisAminoAcids" "AlphaAminoAcids" )))))
		  (setq plug-peptide (MetaInPlug
				      :name "-peptide"
				      :bond0 nco
				      :mates (list (Mate
						    :cap *default_PrimaryAmine*
						    :groupNames (list "AlphaAminoAcids" "SecondaryAminoAcids" "AmineRingClosingAlphaAminoAcids" "CarbonylRingClosingAlphaAminoAcids" "Amines" )))))
		  (setq plug+dkp (MetaOutPlug
				  :name "+dkp"
				  :bond0 nn
				  :bond1 nco
				  :exportFrame frame+core
				  :framesCompletedByCoupling (list (MetaFrameFinisher frame+n )
							     (MetaFrameFinisher frame+co ))
				  :mates (list (Mate
						:cap *default_AchiralAminoAcid*
						:groupNames (list "BisAminoAcids" "AlphaAminoAcids" )))))
		  (setq plug+peptide (MetaOutPlug
				      :name "+peptide"
				      :bond0 nn
				      :exportFrame frame+core
				      :framesCompletedByCoupling (list (MetaFrameFinisher frame+n ))
				      :mates (list (Mate
						    :cap *default_AcylGroup*
						    :groupNames (list "AlphaAminoAcids" "AcylGroups"
								      "SecondaryAminoAcids" "BisAminoAcids"
								      "AmineRingClosingAlphaAminoAcids"
								      "CarbonylRingClosingAlphaAminoAcids" )))))
		  (setq plug+secondn (MetaOutPlug
				      :name "+secondn"
				      :bond0 nn
				      :exportFrame frame+core
				      :framesCompletedByCoupling (list (MetaFrameFinisher frame+n ))
				      :mates (list (Mate
						    :cap *default_AcylGroup*
						    :groupNames (list "AlphaAminoAcids" "AcylGroups" ))))))
     :fragments (list (setq frag_oc (MetaFragment
				     :name "oc"
				     :atoms (list noco )))
		      (setq body (MetaRemainderFragment
				  :residue residue
				  :name "body"
				  :removeFragments (list frag_oc ))))
     :topologies (list (MetaTopology
			:name "dkpIn"
			:residue residue
			:plugs (list plug-dkp )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:fragments (list (MetaExtractFragment
					  :fragment frag_oc
					  :frame frame+co ))
			:alignTo (MetaRecognizedFrame
				  :name "align"
				  :origin nca
				  :recognizer (MetaFrameRecognizer "cAlphaAmide" )))
		       (MetaTopology
			:name "dkpOut"
			:residue residue
			:plugs (list plug-origin plug+dkp )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:fragments (list (MetaExtractFragment
					  :fragment frag_oc
					  :frame frame+co ))
			:alignTo (MetaRecognizedFrame
				  :name "align"
				  :origin nca :recognizer (MetaFrameRecognizer "cAlphaAmide" )))
		       (MetaTopology :name "peptide"
				     :residue residue
				     :plugs (list plug-peptide plug+peptide )
				     :coreFragment (MetaExtractCoreFragment
						    :fragment body )
				     :fragments (list (MetaExtractFragment
						       :fragment frag_oc
						       :frame frame+co ))
				     :alignTo (MetaRecognizedFrame
					       :name "align"
					       :origin nca
					       :recognizer (MetaFrameRecognizer "cAlphaAmide" )))
		       (MetaTopology
			:name "secondnOut"
			:residue residue
			:plugs (list plug-peptide plug+secondn )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:fragments (list (MetaExtractFragment
					  :fragment frag_oc
					  :frame frame+co ))
			:alignTo (MetaRecognizedFrame
				  :name "align"
				  :origin nca
				  :recognizer (MetaFrameRecognizer "cAlphaAmide" )))))
    ))




(defun def_AcylGroups (&key groupName groupAdds ringClosingInfo )
  (let (group name name residue residueCharge rootName rootAtom npco ringClosingFrame ringClosingPlug
	      i newConstitution frame_co frame+core plug-origin plug+camide
	      plug-peptide plug-secondn plug-sideAmideN body )
    (assert (notNil groupName )"You must provide groupName" )
    (setq group groupName )
    (setq name (MetaProperty :name ))
    (setq residue (MetaResidue ))
    (setq residueCharge (MetaPropertyOrDefault :residueCharge 0 ))
    (setq rootName (MetaProperty :root ))
    (setq rootAtom (MetaResidueAtomWithName rootName ))
    (setq npco (MetaRecognizedAtom "npco" rootAtom (MetaFrameRecognizer "acylGroupRecognizer" )"1" ))
    (setq ringClosingFrame ())
    (setq ringClosingPlug ())
    (when (notNil ringClosingInfo )
      (setq ringClosingFrame (lookup ringClosingInfo :frame ))
      (setq ringClosingPlug (lookup ringClosingInfo :plug )))
    (MetaConstitution
     :name name
     :group group
     :addToGroups groupAdds
     :residue residue
     :residueNetCharge residueCharge 
     :stereoInformation (MetaStereoInformation_standard ) 
     :frames (list (setq frame_co (MetaRecognizedFrame
				   :name "-co"
				   :origin npco
				   :recognizer (MetaFrameRecognizer "coAmide" )))
		   (setq frame+core (MetaRecognizedFrame
				     :name "+core"
				     :origin npco
				     :recognizer (MetaFrameRecognizer "acylGroupRecognizer" )))ringClosingFrame )
     :plugs (list (setq plug-origin (MetaOriginPlug
				     :name "-origin"
				     :originFrame frame+core ))
		  (setq plug+camide (MetaOutPlug
				     :name "+camide"
				     :bond0 rootName
				     :exportFrame frame+core
				     :framesCompletedByCoupling (list (MetaFrameFinisher frame_co ))
				     :mates (list (Mate
						   :cap *default_PrimaryAmine* ))))
		  (setq plug-peptide (MetaInPlug
				      :name "-peptide"
				      :bond0 npco
				      :mates (list (Mate
						    :cap *default_PrimaryAmine*
						    :groupNames (list "AlphaAminoAcids" "SecondaryAminoAcids"
								      "Amines" "AmineRingClosingAlphaAminoAcids"
								      "CarbonylRingClosingAlphaAminoAcids" )))))
		  (setq plug-secondn (MetaInPlug
				      :name "-secondn"
				      :bond0 npco
				      :mates (list (Mate
						    :cap *default_PrimaryAmine*
						    :overrideCapMateRequirement true ))))
		  (setq plug-sideAmideN (MetaInPlug
					 :name "-sideAmideN"
					 :bond0 npco
					 :mates (list (Mate
						       :cap *default_PrimaryAmine*
						       :overrideCapMateRequirement true
						       :groupNames (list "AmineRingClosingAlphaAminoAcids" )))))
		  ringClosingPlug )
     :fragments (list (setq body (MetaRemainderFragment
				  :residue residue
				  :name "body" )))
     :topologies (list (MetaTopology
			:name "origin"
			:residue residue
			:plugs (list plug-origin plug+camide ringClosingPlug )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:fragments () :alignTo (MetaAnchorOrigin frame+core ))
		       (MetaTopology
			:name "linear"
			:residue residue
			:alignTo (MetaAnchorOnOtherSideOfPlug
				  :plugName "-peptide" )
			:plugs (list plug-peptide ringClosingPlug )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:fragments ())
		       (MetaTopology
			:name "secondn"
			:residue residue
			:alignTo (MetaAnchorOnOtherSideOfPlug
				  :plugName "-secondn" )
			:plugs (list plug-secondn ringClosingPlug )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:fragments ())
		       (MetaTopology
			:name "side"
			:residue residue
			:alignTo (MetaAnchorOnOtherSideOfPlug
				  :plugName "-sideAmideN" )
			:plugs (list plug-sideAmideN ringClosingPlug )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:fragments ())))))

					; primaryAmine Constitution
					;    fragment must have :properties
					;	:name [constitution name]
					;	:type [primaryAmine]
					;	:monomerName [ *default_PrimaryAmine*]
					;	:pdbName "NH2"
					;	:nside "N"
					;
(defun def_OriginAmines ()
  (let (group name residue residueCharge originName originAtom xName xyName
	      groupAdder newConstitution frame+core plug-origin plug+primaryAmide body )
    (setq group "OriginAmines" )
    (setq name (MetaProperty :name ))
    (setq residue (MetaResidue ))
    (setq residueCharge (MetaPropertyOrDefault :residueCharge 0 ))
    (setq originName (MetaProperty :root ))
    (setq originAtom (MetaResidueAtomWithName originName ))
    (setq xName (MetaProperty :xAtom ))
    (setq xyName (MetaProperty :xyAtom ))
    (setq groupAdder (MetaAddToGroup :groupName group :constitutionName name ))
    (MetaConstitution
     :name name
     :group group
     :residue residue
     :residueNetCharge residueCharge
     :addToGroups (list groupAdder )
     :stereoInformation (MetaStereoInformation_standard )
     :frames (list (setq frame+core (MetaThreeAtomFrame
				     :name "+core"
				     :origin originName
				     :xAtom xName
				     :xyAtom xyName )))
     :plugs (list (setq plug-origin (MetaOriginPlug
				     :name "-origin"
				     :originFrame frame+core ))
		  (setq plug+primaryAmide (MetaOutPlug
					   :name "+primaryAmide"
					   :bond0 originName
					   :exportFrame frame+core
					   :mates (list (Mate
							 :cap *default_AcylGroup*
							 :overrideCapMateRequirement true ))
					   :framesCompletedByCoupling ())))
     :fragments (list (setq body (MetaRemainderFragment
				  :residue residue
				  :name "body"
				  :removeFragments ())))
     :topologies (list (MetaTopology
			:name "linear_primaryAmide"
			:residue residue
			:plugs (list plug-origin plug+primaryAmide )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:alignTo (MetaAnchorOrigin frame+core ))))))

					; primaryAmine Constitution
					;    fragment must have :properties
					;	:name [constitution name]
					;	:type [primaryAmine]
					;	:monomerName [ *default_PrimaryAmine*]
					;	:pdbName "NH2"
					;	:nside "N"
(defun def_PrimaryAmines (&key groupName groupAdds ringClosingInfo )
  (let (group name residue residueCharge rootName ringClosingFrame ringClosingPlug
	      newConstitution frame+core frame+n plug-origin plug+peptide
	      plug-camide plug-acylMethylene body )
    (assert (notNil groupName )"groupName must be provided" )
    (setq group groupName )
    (setq name (MetaProperty :name ))
    (setq residue (MetaResidue ))
    (setq residueCharge (MetaPropertyOrDefault :residueCharge 0 ))
    (setq rootName (MetaProperty :root ))
    (setq ringClosingFrame ())
    (setq ringClosingPlug ())
    (when (notNil ringClosingInfo )
      (setq ringClosingFrame (lookup ringClosingInfo :frame ))
      (setq ringClosingPlug (lookup ringClosingInfo :plug )))
    (MetaConstitution
     :name name
     :group group
     :residue residue
     :residueNetCharge residueCharge
     :addToGroups groupAdds
     :stereoInformation (MetaStereoInformation_standard )
     :frames (list (setq frame+core (MetaRecognizedFrame
				     :name "+core"
				     :origin rootName
				     :recognizer (MetaFrameRecognizer "primaryAmineRecognizer" )))
		   (setq frame+n (MetaRecognizedFrame
				  :name "+n"
				  :origin rootName
				  :recognizer (MetaFrameRecognizer "nAmide" )))ringClosingFrame )
     :plugs (list (setq plug-origin (MetaOriginPlug
				     :name "-origin"
				     :originFrame frame+core ))
		  (setq plug+peptide (MetaOutPlug
				      :name "+peptide"
				      :bond0 rootName
				      :exportFrame frame+core
				      :framesCompletedByCoupling (list (MetaFrameFinisher frame+n ))
				      :mates (list (Mate
						    :cap *default_AcylGroup*
						    :overrideCapMateRequirement true ))))
		  (setq plug-camide (MetaInPlug
				     :name "-camide"
				     :bond0 rootName
				     :mates (list (Mate
						   :cap *default_AcylGroup*
						   :overrideCapMateRequirement true ))))
		  (setq plug-acylMethylene (MetaInPlug
					    :name "-acylMethylene"
					    :bond0 rootName
					    :mates (list (Mate
							  :cap *default_AcylMethylene*
							  :overrideCapMateRequirement true ))))ringClosingPlug )
     :fragments (list (setq body (MetaRemainderFragment
				  :residue residue
				  :name "body"
				  :removeFragments ())))
     :topologies (list (MetaTopology
			:name "linear_peptide"
			:residue residue
			:plugs (list plug-origin plug+peptide ringClosingPlug )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:alignTo (MetaAnchorOrigin frame+core ))
		       (MetaTopology
			:name "linear_camide"
			:residue residue
			:plugs (list plug-camide ringClosingPlug )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:alignTo (MetaAnchorOnOtherSideOfPlug
				  :plugName "-camide" ))
		       (MetaTopology
			:name "linear_acylMethylene"
			:residue residue
			:plugs (list plug-acylMethylene )
			:coreFragment (MetaExtractCoreFragment
				       :fragment body )
			:alignTo (MetaAnchorOnOtherSideOfPlug
				  :plugName "-acylMethylene" ))))
    ))







(defun createMetaDatabase ()
  (prognLog (format "Creating an empty MetaDatabase" )
	    (let ((mdb (MetaDatabase )))
	      (prognLog "Setting up the MetaDatabase"
			(addRecognizer mdb (FrameRecognizer :name "cAlphaAmide" :smarts "[$(C1(~C3(=O4))N2),$(C3(=O4)C1N2),$(N2C1C3(=O4))]" ))
			(addRecognizer mdb (FrameRecognizer :name "acylGroupRecognizer" :smarts "[$(C1(~O2)~[*;U1]3)]" ))
			(addRecognizer mdb (FrameRecognizer :name "coAmide" :smarts "[$(C1(=O)(-[C,O;U1]3)~[N,O;!U1]2)]" ))
			(addRecognizer mdb (FrameRecognizer :name "nAmide" :smarts "[$(N1([U1]2)~C3(=O))]" ))
			(addRecognizer mdb (FrameRecognizer :name "nAmine" :smarts "[$(N1([C;U1]2)~[C;H2]3C=O)]" ))
			(addRecognizer mdb (FrameRecognizer :name "hydantoinImide" :smarts "[$(N1[C;U1]2=O3)]" ))
			(addRecognizer mdb (FrameRecognizer :name "nImide" :smarts "[$(N1([C;U1]2=O)[C;!U1]3=O)]" ))
			(addRecognizer mdb (FrameRecognizer :name "nAmideAlphaAminoAcid" :smarts "[$(N1[C;U1]2[C;U1]3=O4)]" ))
			(addRecognizer mdb (FrameRecognizer :name "coAmideAlphaAminoAcid" :smarts "C1(=[O;U1])[C;U1]2[N;U1]3" ))
			(addRecognizer mdb (FrameRecognizer :name "primaryAmineRecognizer" :smarts "N1([#1;U1]2)[C;U1]3" ))
			(addRecognizer mdb (FrameRecognizer :name "amineSideChainRecognizer" :smarts "[$(N1([U1]4)[C;U1]2[C;U1]3)]" ))
					;			(addRecognizer mdb (FrameRecognizer :name "nAmideSideChainRecognizer" :groupName "nAmide" :smarts "[$(N1([!#1;U1]2)~C3(=O))]" ))
			(addRecognizer mdb (FrameRecognizer :name "coupledAmineSideChainRecognizer"
							    :groupName "nAmine" :smarts "[$(N1([!#1;U1]2)~[C;!U1]3)]" ))
			(log "Creating MetaConstitutions" )
			(createSystemGroup mdb :groupName "BisAminoAcids" :representativeName "pro4(2S4S)" )
			(addMetaConstitution mdb (def_BisAminoAcids ))
			(createSystemGroup mdb :groupName "OriginAmines" :representativeName "nh2" )
			(addMetaConstitution mdb (def_OriginAmines ))
			(let ((superGroup (createSystemGroup mdb :groupName "ReversedBisAminoAcids" )))
			  (createSystemGroup mdb :groupName "ReversedBisAminoAcids-S" :representativeName "revPro4(2S4S)" )
			  (createSystemGroup mdb :groupName "ReversedBisAminoAcids-R" :representativeName "revPro4(2R4S)" )
			  (addMonomerNameNoChecks superGroup "ReversedBisAminoAcids-S" )
			  (addMonomerNameNoChecks superGroup "ReversedBisAminoAcids-R" )
			  (addMetaConstitution mdb (def_ReversedBisAminoAcids )))
			(createSystemGroup mdb :groupName "AlphaAminoAcids" :representativeName "ala(S)" )
			(createSystemGroup mdb :groupName "ReversedAlphaAminoAcids-S" :representativeName "ala(S)" )
			(createSystemGroup mdb :groupName "ReversedAlphaAminoAcids-R" :representativeName "ala(R)" )
			(let ((superGroup (createSystemGroup mdb :groupName "ReversedAlphaAminoAcids" )))
			  (addMonomerNameNoChecks superGroup "ReversedAlphaAminoAcids-S" )
			  (addMonomerNameNoChecks superGroup "ReversedAlphaAminoAcids-R" ))
			(addMetaConstitution mdb (def_AlphaAminoAcids ))
			(createSystemGroup mdb :groupName "SecondaryAminoAcids" :representativeName "pro(S)" )
			(createSystemGroup mdb :groupName "ReversedSecondaryAminoAcids-S" :representativeName "pro(S)" )
			(createSystemGroup mdb :groupName "ReversedSecondaryAminoAcids-R" :representativeName "pro(R)" )
			(let ((superGroup (createSystemGroup mdb :groupName "ReversedSecondaryAminoAcids" )))
			  (addMonomerNameNoChecks superGroup "ReversedSecondaryAminoAcids-S" )
			  (addMonomerNameNoChecks superGroup "ReversedSecondaryAminoAcids-R" ))
			(addMetaConstitution mdb (def_SecondaryAminoAcids ))
			(createSystemGroup mdb :groupName "AmineRingClosingAlphaAminoAcids" :representativeName "dab(S)" )
			(createSystemGroup mdb :groupName "ReversedAmineRingClosingAlphaAminoAcids-S" :representativeName "dab(S)" )
			(createSystemGroup mdb :groupName "ReversedAmineRingClosingAlphaAminoAcids-R" :representativeName "dab(R)" )
			(let ((superGroup (createSystemGroup mdb :groupName "ReversedAmineRingClosingAlphaAminoAcids" )))
			  (addMonomerNameNoChecks superGroup "ReversedAmineRingClosingAlphaAminoAcids-S" )
			  (addMonomerNameNoChecks superGroup "ReversedAmineRingClosingAlphaAminoAcids-R" ))
			(addMetaConstitution mdb (def_AmineRingClosingAlphaAminoAcids "amineSideChainRecognizer" "coupledAmineSideChainRecognizer" ))
			(createSystemGroup mdb :groupName "AcylGroups" :representativeName "ace" )
			(addMetaConstitution mdb (def_AcylGroups :groupName "AcylGroups" :groupAdds (list (MetaAddToGroup :groupName "AcylGroups" ))))
			(addRecognizer mdb (FrameRecognizer :name "AzideRingClosingRecognizer" :smarts "N1=N2-N3" ))
			(addRecognizer mdb (FrameRecognizer :name "AlkyneRingClosingRecognizer" :smarts "C1=C2C3" ))
			(createSystemGroup mdb :groupName "AzideCaps" :representativeName "azideCap" )
			(addMetaConstitution mdb (def_SideChain
						   :groupName "AzideCaps"
						   :groupAdds (list (MetaAddToGroup
								     :groupName "AzideCaps" ))
						   :inPlug (MetaInPlug
							    :name "-azide"
							    :bond0 (MetaRecognizedAtom "alkyne0" (MetaResidueAtomWithName (MetaProperty
															   :root ))
										       (MetaFrameRecognizer "AlkyneRingClosingRecognizer" )"2" )
							    :bond1 (MetaRecognizedAtom
								    "alkyne1"
								    (MetaResidueAtomWithName (MetaProperty :root ))
								    (MetaFrameRecognizer "AlkyneRingClosingRecognizer" )"1" )
							    :mates (list (Mate :cap "azidoGlycine" )))))
			(let (azideRingClosingInfo azideRingClosingFrame )
			  (setq azideRingClosingInfo (list
						      :frame (setq azideRingClosingFrame (MetaRecognizedFrame
											  :name "frame+azide"
											  :origin (MetaProperty :ringSide )
											  :recognizer (MetaFrameRecognizer "AzideRingClosingRecognizer" )))
						      :plug (MetaRingClosingPlug
							     :name "+azide"
							     :otherSidePlugName "+alkyne"
							     :bond0 (MetaRecognizedAtom "azide0" (MetaResidueAtomWithName (MetaProperty :ringSide ))
											(MetaFrameRecognizer "AzideRingClosingRecognizer" )"1" )
							     :bond1 (MetaRecognizedAtom "azide1" (MetaResidueAtomWithName (MetaProperty :ringSide ))
											(MetaFrameRecognizer "AzideRingClosingRecognizer" )"3" )
							     :exportFrame azideRingClosingFrame
							     :framesCompletedByCoupling ()
							     :mates (list (Mate :cap "azideCap" )))))
			  (createSystemGroup mdb
					     :groupName "AzideRingClosingAcylGroups"
					     :representativeName "azidoGlycine" )
			  (addMetaConstitution mdb (def_AcylGroups
						     :groupName "AzideRingClosingAcylGroups"
						     :groupAdds (list (MetaAddToGroup
								       :groupName "AzideRingClosingAcylGroups" ))
						     :ringClosingInfo azideRingClosingInfo )))
			(createSystemGroup mdb
					   :groupName "AlkyneCaps"
					   :representativeName "alkyneCap" )
			(addMetaConstitution mdb (def_SideChain
						   :groupName "AlkyneCaps"
						   :groupAdds (list (MetaAddToGroup :groupName "AlkyneCaps" ))
						   :inPlug (MetaInPlug
							    :name "-alkyne"
							    :bond0 (MetaRecognizedAtom "azide0" (MetaResidueAtomWithName (MetaProperty :root ))
										       (MetaFrameRecognizer "AzideRingClosingRecognizer" )"1" )
							    :bond1 (MetaRecognizedAtom "azide1" (MetaResidueAtomWithName (MetaProperty :root ))
										       (MetaFrameRecognizer "AzideRingClosingRecognizer" )"3" )
							    :mates (list (Mate :cap "acylPropyne" )))))
			(let (alkyneRingClosingInfo alkyneRingClosingFrame )
			  (setq alkyneRingClosingInfo (list
						       :frame (setq alkyneRingClosingFrame (MetaRecognizedFrame
											    :name "frame+alkyne"
											    :origin (MetaProperty :ringSide )
											    :recognizer (MetaFrameRecognizer "AlkyneRingClosingRecognizer" )))
						       :plug (MetaRingClosingPlug
							      :name "+alkyne"
							      :otherSidePlugName "+azide"
							      :bond0 (MetaRecognizedAtom "alkyne0" (MetaResidueAtomWithName (MetaProperty :ringSide ))
											 (MetaFrameRecognizer "AlkyneRingClosingRecognizer" )"2" )
							      :bond1 (MetaRecognizedAtom "alkyne1" (MetaResidueAtomWithName (MetaProperty :ringSide ))
											 (MetaFrameRecognizer "AlkyneRingClosingRecognizer" )"1" )
							      :exportFrame alkyneRingClosingFrame
							      :framesCompletedByCoupling ()
							      :mates (list (Mate
									    :cap "alkyneCap" )))))
			  (createSystemGroup mdb
					     :groupName "AlkyneRingClosingAcylGroups"
					     :representativeName "acylPropyne" )
			  (addMetaConstitution mdb (def_AcylGroups
						     :groupName "AlkyneRingClosingAcylGroups"
						     :groupAdds (list (MetaAddToGroup :groupName "AlkyneRingClosingAcylGroups" ))
						     :ringClosingInfo alkyneRingClosingInfo )))
			(addRecognizer mdb (FrameRecognizer
					    :name "AcylMethyleneCapRecognizer"
					    :smarts "[*]1" ))
			(addRecognizer mdb (FrameRecognizer
					    :name "AcylMethyleneRingClosingRecognizer"
					    :smarts "C1C2=O3" ))
			(createSystemGroup mdb
					   :groupName "AcylMethyleneCaps"
					   :representativeName "acylMethyleneCap" )
			(addMetaConstitution mdb (def_SideChain
						   :groupName "AcylMethyleneCaps"
						   :groupAdds (list (MetaAddToGroup
								     :groupName "AcylMethyleneCaps" ))
						   :inPlug (MetaInPlug
							    :name "-acylMethylene_methylene"
							    :bond0 (MetaRecognizedAtom "amine0" (MetaResidueAtomWithName (MetaProperty :root ))
										       (MetaFrameRecognizer "AcylMethyleneCapRecognizer" )"1" )
							    :mates (list (Mate
									  :cap "acylMethylene"
									  :overrideCapMateRequirement true )))))
			(createSystemGroup mdb
					   :groupName "AmineAcylMethyleneCaps"
					   :representativeName "amineAcylMethyleneCap" )
			(addMetaConstitution mdb (def_SideChain
						   :groupName "AmineAcylMethyleneCaps"
						   :groupAdds (list (MetaAddToGroup
								     :groupName "AmineAcylMethyleneCaps" ))
						   :inPlug (MetaInPlug
							    :name "-acylMethylene_amine"
							    :bond0 (MetaRecognizedAtom "amine0" (MetaResidueAtomWithName (MetaProperty :root ))
										       (MetaFrameRecognizer "AcylMethyleneRingClosingRecognizer" )"1" )
							    :mates (list (Mate
									  :cap *default_AchiralAminoAcid* )))))
			(let (acylMethyleneRingClosingInfo acylMethyleneRingClosingFrame )
			  (setq acylMethyleneRingClosingInfo (list
							      :frame (setq acylMethyleneRingClosingFrame (MetaRecognizedFrame
													  :name "frame+acylMethylene"
													  :origin (MetaProperty :ringSide )
													  :recognizer (MetaFrameRecognizer "AcylMethyleneRingClosingRecognizer" )))
							      :plug (MetaRingClosingPlug
								     :name "+acylMethylene_methylene"
								     :otherSidePlugName "+acylMethylene_amine"
								     :bond0 (MetaRecognizedAtom "acylMethylene0" (MetaResidueAtomWithName (MetaProperty :ringSide ))
												(MetaFrameRecognizer "AcylMethyleneRingClosingRecognizer" )"1" )
								     :exportFrame acylMethyleneRingClosingFrame
								     :framesCompletedByCoupling ()
								     :mates (list (Mate :cap "acylMethyleneCap" )))))
			  (createSystemGroup mdb
					     :groupName "AcylMethyleneRingClosingAcylGroups"
					     :representativeName "acylMethylene" )
			  (addMetaConstitution mdb (def_AcylGroups
						     :groupName "AcylMethyleneRingClosingAcylGroups"
						     :groupAdds (list (MetaAddToGroup
								       :groupName "AcylMethyleneRingClosingAcylGroups" ))
						     :ringClosingInfo acylMethyleneRingClosingInfo )))
			(createSystemGroup mdb
					   :groupName "Esters"
					   :representativeName "methylOxide" )
			(addMetaConstitution mdb (def_SideChain
						   :groupName "Esters"
						   :inPlug (MetaInPlug
							    :name "-ester"
							    :bond0 (MetaProperty :root )
							    :mates (list (Mate :cap "pro4(2S4S)" )))
						   :groupAdds (list (MetaAddToGroup
								     :groupName "Esters" ))))
			(createSystemGroup mdb
					   :groupName "PrimaryAmines"
					   :representativeName "methylAmine" )
			(addMetaConstitution mdb (def_PrimaryAmines
						   :groupName "PrimaryAmines"
						   :groupAdds (list (MetaAddToGroup :groupName "PrimaryAmines" ))))
			(createSystemGroup mdb
					   :groupName "Hydantoins"
					   :representativeName "hydantoin" )
			(addMetaConstitution mdb (def_Hydantoins ))
			(createSystemGroup mdb
					   :groupName "NHSideChain"
					   :representativeName "hn" )
			(addMetaConstitution mdb (def_NHSideChain ))
			(createSystemGroup mdb
					   :groupName "NSideChains"
					   :representativeName "nAla" )
			(addMetaConstitution mdb (def_NSideChains ))
			(createSystemGroup mdb
					   :groupName "ImideSideChains"
					   :representativeName "imidePhenyl" )
			(let ((meta_ImideSideChains (def_SideChain
						      :groupName "ImideSideChains"
						      :groupAdds (list (MetaAddToGroup :groupName "ImideSideChains" ))
						      :inPlug (MetaInPlug
							       :name "-imide"
							       :bond0 (MetaProperty
								       :root )
							       :mates (list (Mate
									     :cap "hydantoin" ))))))
			  (addMetaConstitution mdb meta_ImideSideChains )))
	      (prognLog "ConnectMates using updateMatesForPlugs--> "
			(updateMatesForPlugs mdb ) )
	      (prognLog  "ConnectRingClosingMates: "
			 (updateRingClosingMatesForRingClosingPlugs mdb ))
	      (prognLog  "CheckMdb: " (check mdb ) )
	      (prognLog  "CreateMetaScaffolds: " (createAllMetaExtractScaffolds mdb ) )
	      mdb)
	    )
  )