.data
L5:
	.long 2
	.string " O"

L6:
	.long 2
	.string " ."

L15:
	.long 2
	.string " O"

L16:
	.long 2
	.string " ."

L25:
	.long 4
	.string "\x0a"

L26:
	.long 4
	.string "\x0a"

L29:
	.long 4
	.string "\x0a"

L30:
	.long 4
	.string "\x0a"

.text
	.globl _tigermain
printboard_L0:

	enter $8,$0x0

	pushl %ebx

	pushl %edi

	pushl %esi

	movl 8(%ebp), %eax

	addl $-4, %eax

	movl $0, (%eax)

	movl 8(%ebp), %eax

	movl -4(%eax), %ebx

	subl $1,%ebx

	movl 8(%ebp), %eax

	movl -4(%eax), %eax

	cmpl %ebx,%eax

	jle L28

L1:

	pushl %ecx

	pushl %edx

	pushl $L30

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	jmp L31

L28:

	movl 8(%ebp), %eax

	addl $-8, %eax

	movl $0, (%eax)

	movl 8(%ebp), %eax

	movl -4(%eax), %esi

	subl $1,%esi

	movl 8(%ebp), %eax

	movl -8(%eax), %eax

	cmpl %esi,%eax

	jle L24

L2:

	pushl %ecx

	pushl %edx

	pushl $L26

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	movl 8(%ebp), %eax

	movl -4(%eax), %eax

	cmpl %eax,%ebx

	je L1

L27:

	movl 8(%ebp), %ebx

	addl $-4, %ebx

	movl 8(%ebp), %eax

	movl -4(%eax), %eax

	addl $1, %eax

	movl %eax, (%ebx)

	jmp L28

L24:

	movl 8(%ebp), %eax

	movl -12(%eax), %edi

	movl -4(%ebp), %ebx

	pushl %ecx

	pushl %edx

	pushl %ebx

	pushl %edi

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	imul $4, %ebx, %eax

	addl %eax,%edi

	movl (%edi), %ebx

	movl -8(%ebp), %eax

	cmpl %ebx,%eax

	je L20

L21:

	movl $L16, %eax

L22:

	pushl %ecx

	pushl %edx

	pushl %eax

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	movl 8(%ebp), %eax

	movl -8(%eax), %eax

	cmpl %eax,%esi

	je L2

L23:

	movl 8(%ebp), %ebx

	addl $-8, %ebx

	movl 8(%ebp), %eax

	movl -8(%eax), %eax

	addl $1, %eax

	movl %eax, (%ebx)

	jmp L24

L20:

	movl $L15, %eax

	jmp L22

L31:

	popl %esi

	popl %edi

	popl %ebx

	leave

	ret

_tigermain:

	enter $20,$0x0

	pushl %ebx

	pushl %edi

	pushl %esi

	movl %ebp,%eax

	addl $-4, %eax

	movl $8, (%eax)

	movl %ebp,%ebx

	addl $-8, %ebx

	pushl %ecx

	pushl %edx

	pushl $0

	pushl -4(%ebp)

	call _allocArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl %eax, (%ebx)

	movl %ebp,%ebx

	addl $-12, %ebx

	pushl %ecx

	pushl %edx

	pushl $0

	pushl -4(%ebp)

	call _allocArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl %eax, (%ebx)

	movl %ebp,%ebx

	addl $-16, %ebx

	pushl %ecx

	pushl %edx

	pushl $0

	movl -4(%ebp), %eax

	movl -4(%ebp), %ecx

	addl %ecx,%eax

	subl $1,%eax

	pushl %eax

	call _allocArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl %eax, (%ebx)

	movl %ebp,%ebx

	addl $-20, %ebx

	pushl %ecx

	pushl %edx

	pushl $0

	movl -4(%ebp), %eax

	movl -4(%ebp), %ecx

	addl %ecx,%eax

	subl $1,%eax

	pushl %eax

	call _allocArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl %eax, (%ebx)

	pushl %ecx

	pushl %edx

	pushl %ebp

	call printboard_L0

	addl $4, %esp

	popl %edx

	popl %ecx

	movl $0, %eax 

	jmp L32

L32:

	popl %esi

	popl %edi

	popl %ebx

	leave

	ret

