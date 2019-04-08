.data
_save: .word 9999,7,6,6,6,6,6,6,6
_k: .word 6
_error: .asciiz "Index Out of Bounds Exception"

.text
.globl main

main:
	lw $s5, _k
	la $s6, _save
	addi $s3, $zero, 0
	
	lw $t2, 4($s6)
	
Loop:
	# bounds-check
	sltu $t1, $s3, $t2
	beq $t1, $zero, IndexOutOfBounds
	
	# t0 = save[i]
	sll $t1, $s3, 2
	add $t1, $t1, $s6
	lw $t0, 8($t1)
	
	# if t0 != k goto Exit
	bne $t0, $s5, Exit
	
	# else, goto Loop
	addi $s3, $s3, 1
	j Loop

Exit:
	addi $v0, $zero, 1
	add $a0, $zero, $s3
	syscall
	j End

IndexOutOfBounds:
	addi $v0, $zero, 4
	la $a0, _error
	syscall

End:
	nop
