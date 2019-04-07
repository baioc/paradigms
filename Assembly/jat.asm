.data
_f: .word 1
_g: .word 2
_h: .word 4
_i: .word 8
_j: .word 16

# Jump Address Table
jat:
	.word L0     	# endereco do label L0
	.word L1
	.word L2
	.word L3
	.word L4
	.word default	# endereco do label default


.text
.globl main

main:
	# inicializando registradores
	# (exceto k, editar manualmente)
	lw $s0, _f
	lw $s1, _g
	lw $s2, _h
	lw $s3, _i
	lw $s4, _j

	# Valor Inicial k |	-2	-1	0	1	2	3	4	5	6 |
	# Valor Final   f |	15	14	24	-2	22	28	4	8	7 |

	# carrega em $t4 o endereco-base da jat
	la $t4, jat

	# testa se k ($s5) esta no intervalo [0,4]
	# caso contrario desvia p/ default
	sltiu $t0, $s5, 5
	beq $t0, $zero, default

	# encontra o endereco guardado em jat[k]
	sll $t1, $s5, 2
	add $t1, $t4, $t1
	lw $t1, 0($t1)

	# desvio do switch
	jr $t1

	# cases
	L0:
		add $s0, $s3, $s4
		j Exit
	L1:
		sub $s0, $s1, $s2
		j Exit
	L2:
		add $s0, $s1, $s2
		add $s0, $s0, $s4
		j Exit
	L3:
		or $s0, $s3, $s2
		or $s0, $s0, $s4
		j Exit
	L4:
		and $s0, $s2, $s5
		j Exit
	default:
		sub $s0, $s3, $s5
		addi $s0, $s0, 5
		j Exit

	# aka break
	Exit:
		nop
