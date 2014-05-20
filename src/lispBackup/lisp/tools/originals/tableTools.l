;
;
; Tools for manipulating tables
;
;

defun optimizeGeometryOfAllEntries ( table minimizeField &key (writeEnergyToField () ) ) \
    	"Minimize the energy of the (minimizeField) of every entry"
    assert [ table hasField minimizeField ] [ "The table is missing the field(%s)" % (fullName minimizeField) ]
    assert [ [ table fieldClass minimizeField ] isSubClassOf Matter ] [ "The table field(%s) is not a Matter - its of class(%s)" % (fullName minimizeField) ( className [ table fieldClass minimizeField ] ) ]
    local gaff (loadArchive [ "%s/gaff.cxml" % (databaseDir) ] )
    when [ writeEnergyToField notNil ]
        assert (not [ table hasField writeEnergyToField ] ) [ "The table already has the field(%s)" % [ writeEnergyToField fullName ] ]
	[ table appendField writeEnergyToField Real ]
    local idx 0
    foreach entry [ table asCons ]
        println [ "Optimizing entry %d" % idx ]
        local mol [ entry read minimizeField ]
	local minimizer ( Minimizer :matter mol :forceField gaff )
	[ minimizer minimize ]
	local energyFunction [ minimizer getEnergyFunction ]
	local energy [ energyFunction getTotalEnergy ]
	when [ writeEnergyToField notNil ]
	    [ entry write writeEnergyToField energy ]
	setq idx [ idx + 1 ]

