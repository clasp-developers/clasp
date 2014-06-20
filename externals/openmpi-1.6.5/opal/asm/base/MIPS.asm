START_FILE

#include <sys/asm.h>
#include <regdef.h>
	
	TEXT

	ALIGN(8)
LEAF(opal_atomic_mb)
	sync
	j	ra
END(opal_atomic_mb)

	
	ALIGN(8)
LEAF(opal_atomic_rmb)
	sync
	j	ra
END(opal_atomic_rmb)
	
	
LEAF(opal_atomic_wmb)
	sync
	j	ra
END(opal_atomic_wmb)


LEAF(opal_atomic_cmpset_32)
	.set noreorder        
retry1:                
	ll     $3, 0($4)         
	bne    $3, $5, done1   
	or     $2, $6, 0      
	sc     $2, 0($4)         
	beqz   $2, retry1
done1:                 
	.set reorder          

	xor	$3,$3,$5
	j	ra
	sltu	$2,$3,1
END(opal_atomic_cmpset_32)


LEAF(opal_atomic_cmpset_acq_32)
	.set noreorder        
retry2:                
	ll     $3, 0($4)         
	bne    $3, $5, done2   
	or     $2, $6, 0      
	sc     $2, 0($4)         
	beqz   $2, retry2   
done2:                 
	sync
	.set reorder          

	xor	$3,$3,$5
	j	ra
	sltu	$2,$3,1
END(opal_atomic_cmpset_acq_32)

	
LEAF(opal_atomic_cmpset_rel_32)
	.set noreorder        
	sync
retry3:                
	ll     $3, 0($4)         
	bne    $3, $5, done3   
	or     $2, $6, 0      
	sc     $2, 0($4)         
	beqz   $2, retry3   
done3:                 
	.set reorder          

	xor	$3,$3,$5
	j	ra
	sltu	$2,$3,1
END(opal_atomic_cmpset_rel_32)
	
	
LEAF(opal_atomic_cmpset_64)
		.set noreorder        
retry4:                
	lld    $3, 0($4)         
	bne    $3, $5, done4   
	or     $2, $6, 0      
	scd    $2, 0($4)         
	beqz   $2, retry4   
done4:                 
	.set reorder          

	xor	$4,$3,$5
	j	ra
	sltu	$3,$4,1
END(opal_atomic_cmpset_64)


LEAF(opal_atomic_cmpset_acq_64)
	.set noreorder        
retry5:                
	lld    $3, 0($4)         
	bne    $3, $5, done5   
	or     $2, $6, 0      
	scd    $2, 0($4)         
	beqz   $2, retry5   
done5:                 
	.set reorder          
	sync
	xor	$4,$3,$5
	j	ra
	sltu	$3,$4,1
END(opal_atomic_cmpset_acq_64)


LEAF(opal_atomic_cmpset_rel_64)
	.set noreorder        
	sync
retry6:                
	lld    $3, 0($4)         
	bne    $3, $5, done6   
	or     $2, $6, 0      
	scd    $2, 0($4)         
	beqz   $2, retry6   
done6:                 
	.set reorder          

	xor	$4,$3,$5
	j	ra
	sltu	$3,$4,1
END(opal_atomic_cmpset_rel_64)
