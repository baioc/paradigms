.text
.globl main

main:
	addi $a0, $zero, 17
	jal fib

	#j end


	# P&H 5ed. 2.39)
	li $s3, 536955172	# 0x20014924

	#j end


	# P&H 5ed. 2.25)

	addi $t2, $zero, 10
	addi $s1, $zero, 0

	loop:
		# ---
		addi $s1, $s1, 2
		# ---
		# if (R[rs] > 0) R[rs] = R[rs] − 1, PC = PC+4 + BranchAddr
		slti $t0, $t2, 1     # t0 = t2 > 0 ? 0 : 1
		addi $t1, $t0, -1    # t1 =         -1 : 0
		add $t2, $t2, $t1    # t2 =     t2 - 1 : t2
		beq $t0, $zero, loop #       goto loop : nop

	#j end


	# P&H 5ed. 2.26)

	addi $t1, $zero, 10
	addi $s2, $zero, 0

	loop_2:
		slt $t2, $0, $t1
		beq $t2, $0, end
		subi $t1, $t1, 1
		addi $s2, $s2, 2
		j loop_2

	j end


# P&H 5ed. 2.31) int fib(int n)
fib:
	bne $a0, $zero, fib_1	# if n != 0, goto fib_1

	# else, return 0
	addi $v0, $zero, 0
	j fib_ret

	fib_1:
		addi $t1, $zero, 1
		bne $a0, $t1, fib_rec	# if n != 1, goto fib_rec

		# else, return 1
		add $v0, $t1, $zero
		j fib_ret

	fib_rec:
		addi $sp, $sp, -12
		sw $ra, 8($sp)	# push return addr
		sw $a0, 4($sp)	# push n
		sw $s1, 0($sp)	# save s1

		# s1 = fib(n-1)
		addi $a0, $a0, -1
		jal fib
		add $s1, $v0, $zero

		lw $a0, 4($sp)	# restore n

		# return = s1 + fib(n-2)
		addi $a0, $a0, -2
		jal fib
		add $v0, $s1, $v0

		lw $s1, 0($sp)	# restore s1
		lw $ra, 8($sp)	# restore return addr
		addi $sp, $sp, 12

	fib_ret:
		jr $ra


end:
	nop
