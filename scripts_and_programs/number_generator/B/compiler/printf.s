	jmp	end
.align 8
.globl _printf
.data
_printf: 	.quad 1f
.text
1: 	.quad s,l1
	.quad va,120
	.quad c,00
	.quad b1
	.quad iva,96
	.quad va,24
	.quad b1
.data; l2: .quad 1f; .text
1:l4:
	.quad s,l1
	.quad va,112
	.quad x,_char
	.quad n2
	.quad a,16
	.quad va,120
	.quad u7
	.quad n3
	.quad b1
	.quad c,045
	.quad b5
	.quad f,l3
	.quad ia,112
	.quad c,00
	.quad b4
	.quad f,l5
	.quad n11
l5:
	.quad ix,_putchar
	.quad n2
	.quad a,112
	.quad n3
	.quad t,l4
l3:
	.quad iva,104
	.quad va,96
	.quad u7
	.quad u3
	.quad b1
	.quad iva,112
	.quad x,_char
	.quad n2
	.quad a,16
	.quad va,120
	.quad u7
	.quad n3
	.quad b1
	.quad z,l7
l8:
l9:
	.quad ix,_printn
	.quad n2
	.quad a,104
	.quad a,112
	.quad c,0157
	.quad b4
	.quad f,l10
	.quad ic,010
	.quad t,l11
l10:
	.quad ic,012
l11:
	.quad n3
	.quad ix,l2
	.quad n6
l12:
	.quad ix,_putchar
	.quad n2
	.quad a,104
	.quad n3
	.quad ix,l2
	.quad n6
l13:
	.quad iva,128
	.quad c,00
	.quad b1
l15:
	.quad iva,112
	.quad x,_char
	.quad n2
	.quad a,104
	.quad va,128
	.quad u7
	.quad n3
	.quad b1
	.quad c,00
	.quad b5
	.quad f,l14
	.quad ix,_putchar
	.quad n2
	.quad a,112
	.quad n3
	.quad t,l15
l14:
	.quad ix,l2
	.quad n6
	.quad t,l6
l7:
	.quad 4
	.quad 0144
	.quad l8
	.quad 0157
	.quad l9
	.quad 0143
	.quad l12
	.quad 0163
	.quad l13
l6:
	.quad ix,_putchar
	.quad n2
	.quad c,045
	.quad n3
	.quad iva,120
	.quad u10
	.quad iva,96
	.quad u10
	.quad ix,l2
	.quad n6
	.quad n11
l1 = 136


.data
1: 
.text
end:	call	chain
	.quad 00
