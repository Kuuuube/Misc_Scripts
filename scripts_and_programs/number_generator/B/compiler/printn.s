	jmp	end
.align 8
.globl _printn
.data
_printn: 	.quad 1f
.text
1: 	.quad s,l1
	.quad va,40
	.quad c,00
	.quad b1
	.quad ia,16
	.quad c,00
	.quad b7
	.quad f,l2
	.quad iva,16
	.quad a,16
	.quad u2
	.quad b1
	.quad ia,16
	.quad c,00
	.quad b7
	.quad f,l3
	.quad iva,16
	.quad u10
	.quad iva,40
	.quad c,01
	.quad b1
	.quad t,l4
l3:
	.quad ix,_putchar
	.quad n2
	.quad c,055
	.quad n3
l4:
l2:
	.quad iva,32
	.quad a,16
	.quad a,24
	.quad b20
	.quad b1
	.quad f,l5
	.quad ix,_printn
	.quad n2
	.quad a,32
	.quad a,24
	.quad n3
l5:
	.quad ix,_putchar
	.quad n2
	.quad a,16
	.quad a,24
	.quad b16
	.quad c,060
	.quad b14
	.quad a,40
	.quad b14
	.quad n3
	.quad n11
l1 = 48


.data
1: 
.text
end:	call	chain
	.quad 00
