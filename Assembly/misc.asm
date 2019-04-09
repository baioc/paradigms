.text
.globl main

main:
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


end:
	nop
