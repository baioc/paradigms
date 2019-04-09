.text


# void lock(semaphore)
lock:
	ll $t0, 0($a0)	# read the semaphore (and set LLbit)
	bne $t0, $zero, lock	# if occupied (1), start over

	addi $t1, $zero, 1
	sc $t1, 0($a0)	# try grabing the semaphore
	beq $t1, $zero, lock	# if wasn't atomic (0), start over

	# return only when sucessfully grabbed the semaphore in an atomic swap
	jr $ra


# void unlock(semaphore)
unlock:
	sw $zero, 0($a0)
	jr $ra
