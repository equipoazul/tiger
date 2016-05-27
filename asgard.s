.data
L5:
	.long 1
	.string "0"

L6:
	.long 1
	.string "0"

L7:
	.long 1
	.string "0"

L8:
	.long 1
	.string "0"

L9:
	.long 1
	.string "0"

L10:
	.long 1
	.string "0"

L11:
	.long 1
	.string "0"

L12:
	.long 1
	.string "0"

L17:
	.long 1
	.string "-"

L18:
	.long 1
	.string "-"

L21:
	.long 1
	.string "0"

L22:
	.long 1
	.string "0"

.text
	.globl _tigermain
f_L2:

	enter $16,$0x0

	movl %ebp,%eax

	addl $-4, %eax

	movl %ebx, (%eax)

	movl %ebp,%eax

	addl $-8, %eax

	movl %edi, (%eax)

	movl %ebp,%eax

	addl $-12, %eax

	movl %esi, (%eax)

	movl $0, %ebx

	movl 12(%ebp), %eax

	cmpl %ebx, %eax

	jg L13

L14:

	movl -12(%ebp), %esi

	movl -8(%ebp), %edi

	movl -4(%ebp), %ebx

	jmp L32

L13:

	movl 12(%ebp), %eax

	movl $10, %ebx

	xorl %edx, %edx

	idivl %ebx

	movl %eax,%ebx

	pushl %ecx

	pushl %edx

	pushl %ebx

	pushl 8(%ebp)

	call f_L2

	addl $8, %esp

	popl %edx

	popl %ecx

	movl 12(%ebp), %ebx

	movl 12(%ebp), %eax

	movl $10, %edi

	xorl %edx, %edx

	idivl %edi

	movl %eax,%edi

	movl -16(%ebp), %eax

	imul $10, %edi, %eax

	movl %eax, -16(%ebp)

	movl -16(%ebp), %eax

	subl %eax,%ebx

	pushl %ecx

	pushl %edx

	pushl $L12

	call ord

	addl $4, %esp

	popl %edx

	popl %ecx

	addl %eax,%ebx

	pushl %ecx

	pushl %edx

	pushl %ebx

	call chr

	addl $4, %esp

	popl %edx

	popl %ecx

	pushl %ecx

	pushl %edx

	pushl %eax

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	jmp L14

L32:

	leave

	ret

printint_L1:

	enter $12,$0x0

	movl %ebp,%eax

	addl $-4, %eax

	movl %ebx, (%eax)

	movl %ebp,%eax

	addl $-8, %eax

	movl %edi, (%eax)

	movl %ebp,%eax

	addl $-12, %eax

	movl %esi, (%eax)

	movl $0, %ebx

	movl 12(%ebp), %eax

	cmpl %ebx, %eax

	jl L26

L27:

	movl $0, %ebx

	movl 12(%ebp), %eax

	cmpl %ebx, %eax

	jg L23

L24:

	pushl %ecx

	pushl %edx

	pushl $L22

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

L25:

L28:

	movl -12(%ebp), %esi

	movl -8(%ebp), %edi

	movl -4(%ebp), %ebx

	jmp L33

L26:

	pushl %ecx

	pushl %edx

	pushl $L18

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	movl $0, %eax

	movl 12(%ebp), %ebx

	subl %ebx,%eax

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl %ebp

	call f_L2

	addl $8, %esp

	popl %edx

	popl %ecx

	jmp L28

L23:

	movl 12(%ebp), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl %ebp

	call f_L2

	addl $8, %esp

	popl %edx

	popl %ecx

	jmp L25

L33:

	leave

	ret

try_L0:

	enter $12,$0x0

	movl %ebp,%eax

	addl $-4, %eax

	movl %ebx, (%eax)

	movl %ebp,%eax

	addl $-8, %eax

	movl %edi, (%eax)

	movl %ebp,%eax

	addl $-12, %eax

	movl %esi, (%eax)

	movl $0, %edi 

	movl 8(%ebp), %eax

	movl -4(%eax), %ebx

	cmpl %ebx,%edi

	jge L29

L31:

	pushl %ecx

	pushl %edx

	pushl %edi

	pushl 8(%ebp)

	call printint_L1

	addl $8, %esp

	popl %edx

	popl %ecx

	cmpl %ebx,%edi

	je L29

L30:

	addl $1, %edi

	jmp L31

L29:

	movl -12(%ebp), %esi

	movl -8(%ebp), %edi

	movl -4(%ebp), %ebx

	jmp L34

L34:

	leave

	ret

_tigermain:

	enter $16,$0x0

	movl %ebp,%eax

	addl $-8, %eax

	movl %ebx, (%eax)

	movl %ebp,%eax

	addl $-12, %eax

	movl %edi, (%eax)

	movl %ebp,%eax

	addl $-16, %eax

	movl %esi, (%eax)

	movl %ebp,%eax

	addl $-4, %eax

	movl $3, (%eax)

	pushl %ecx

	pushl %edx

	pushl $1

	pushl %ebp

	call try_L0

	addl $8, %esp

	popl %edx

	popl %ecx

	movl $0, %eax 

	movl -16(%ebp), %esi

	movl -12(%ebp), %edi

	movl -8(%ebp), %ebx

	jmp L35

L35:

	leave

	ret

