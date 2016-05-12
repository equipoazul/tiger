.data
.text
	.globl	 _tigermain
_tigermain:

	enter $0,$0x0

	pushl %ecx

	pushl %edx

	pushl $0

	pushl $10

	call _allocArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl $2, %edi 

	pushl %ecx

	pushl %edx

	pushl %edi

	pushl %ebx

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	imul $4, %edi, %eax

	addl %eax,%ebx

	movl (%ebx), %eax

	jmp L0

L0:

	leave

	ret

