.data
L0:
	.long 6
	.string "Nobody"

L1:
	.long 8
	.string "Somebody"

.text
	.globl	 _tigermain
_tigermain:

	enter $0,$0x0

	pushl %ecx

	pushl %edx

	pushl $1000

	pushl L0

	pushl $2

	call _allocRecord

	addl $12, %esp

	popl %edx

	popl %ecx

	movl $0, %edi 

	pushl %ecx

	pushl %edx

	pushl %ebx

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	movl %ebx,%eax

	imul %edi,%eax*$4

	addl %eax,%eax

	movl %L1, (%eax)

	movl $0, %eax 

	jmp L2

L2:

	leave

	ret

