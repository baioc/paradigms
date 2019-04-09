.text
.globl main

main:
	addi $a0, $zero, 17
	jal fib

	j end


# P&H 5ed. 2.31: int fib(int n)
fib:
	slti $t0, $a0, 2	# t0 = (n < 2)
	beq $t0, $zero, fib_rec	# if !(), goto fib_rec

	# else, return n
	add $v0, $zero, $a0
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
