;
; Export select entries to a variety of formats
;


defun exportHits_usage ( )
    println "Args: --input () ---entries ()  ---entryRange () --extension mol2 --filePrefix: () --outputDir ./ "
    println "--entryRange ;first ;last : a range of entries is extracted, this supercedes ---entries"
    println "--filePrefix : if provided is used to construct the filename."
    exit 1



defun exportHits ( &key input entries entryRange extension filePrefix ( outputDir "./" ) )
    ifTrue [ input isNil ]
        local input (getline "input (hit) file: ")
    ifTrue [ [ entries isNil ] and [entryRange isNil ] ]
        local entries (list ( asInt (getline "entry index: ") ) )
    ifTrue [ extension isNil ]
        local extension (getline "file type (mol2/cxml): " )
    ifTrue [ entryRange notNil ]
        local entries (range [entryRange @ 0] [ [entryRange @ 1 ] + 1] )
    local idx 0
    println [ "Loading hit file: %s" % input ]
    local hitList ( loadArchive input)
    local builderScorer [ hitList getBuilderScorer ]
    foreach hitIdx entries
	local hit [hitList getHit hitIdx]
	local builderScorer [ hit recreateBuilderScorer ]
	local oligomer [ builderScorer getCurrentOligomer ]
	local mol [ hit getBuiltMolecule ]
	if [ filePrefix isNil ]
	  then
	    local fileName [ "%s/_%02d%s.%s" % outputDir hitIdx [oligomer sequenceAsFileName] extension ]
	  else
	    local fileName [ "%s/_%s%02d.%s" % outputDir filePrefix hitIdx extension ]
	cond
	   [extension == "mol2"]
	       saveMol2 mol fileName
	   [extension == "cxml"]
	       save mol fileName
	   true
	       RAISE "You must provide a valid type to export to (mol2,pdb)"


ifTrue ( isTopLevelScript )
    println "Starting"
    local args (cdr l:*ARGS*)
    invoke (function exportHits) args 
