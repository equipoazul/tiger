.data
L4:
	.long 2
	.string " O"

L5:
	.long 2
	.string " ."

L14:
	.long 2
	.string " O"

L15:
	.long 2
	.string " ."

L24:
	.long 4
	.string "\x0a"

L25:
	.long 4
	.string "\x0a"

L28:
	.long 4
	.string "\x0a"

L29:
	.long 4
	.string "\x0a"

.text
	.globl _tigermain
_tigermain:

	enter $28,$0x0

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

	movl %ebp,%eax

	addl $-24, %eax

	movl $0, (%eax)

	movl -4(%ebp), %ebx

	subl $1,%ebx

	movl -24(%ebp), %eax

	cmpl %ebx,%eax

	jle L27

L0:

	pushl %ecx

	pushl %edx

	pushl $L29

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	movl $0, %eax 

	jmp L30

L27:

	movl %ebp,%eax

	addl $-28, %eax

	movl $0, (%eax)

	movl -4(%ebp), %esi

	subl $1,%esi

	movl -28(%ebp), %eax

	cmpl %esi,%eax

	jle L23

L1:

	pushl %ecx

	pushl %edx

	pushl $L25

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	movl -24(%ebp), %eax

	cmpl %eax,%ebx

	je L0

L26:

	movl %ebp,%ebx

	addl $-24, %ebx

	movl -24(%ebp), %eax

	addl $1, %eax

	movl %eax, (%ebx)

	jmp L27

L23:

	movl -12(%ebp), %edi

	movl -24(%ebp), %ebx

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

	movl -28(%ebp), %eax

	cmpl %ebx,%eax

	je L19

L20:

	movl $L15, %eax

L21:

	pushl %ecx

	pushl %edx

	pushl %eax

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	movl -28(%ebp), %eax

	cmpl %eax,%esi

	je L1

L22:

	movl %ebp,%ebx

	addl $-28, %ebx

	movl -28(%ebp), %eax

	addl $1, %eax

	movl %eax, (%ebx)

	jmp L23

L19:

	movl $L14, %eax

	jmp L21

L30:

	popl %esi

	popl %edi

	popl %ebx

	leave

	ret

