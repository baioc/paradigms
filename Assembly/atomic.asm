# Copyright (c) 2019 Gabriel B. Sant'Anna <baiocchi.gabriel@gmail.com>
# @License Apache <https://gitlab.com/baioc/paradigms>

.text


# void lock(mutex)
lock:
	ll $t0, 0($a0)	# read the mutex (and set LLbit)
	bne $t0, $zero, lock	# if occupied (1), start over

	addi $t1, $zero, 1
	sc $t1, 0($a0)	# try grabing the mutex
	beq $t1, $zero, lock	# if wasn't atomic (0), start over

	# return only when sucessfully grabbed the mutex in an atomic swap
	jr $ra


# void unlock(mutex)
unlock:
	sw $zero, 0($a0)
	jr $ra
