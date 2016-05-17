.data
L0:
	.long 5
	.string "aname"

L1:
	.long 9
	.string "somewhere"

L2:
	.long 0
	.string ""

L3:
	.long 7
	.string "Kapoios"

L4:
	.long 5
	.string "Kapou"

L5:
	.long 5
	.string "Allos"

L6:
	.long 4
	.string "kati"

L7:
	.long 3
	.string "sfd"

L8:
	.long 3
	.string "sdf"

.text
	.globl _tigermain
_tigermain:

	enter $80,$0x0

	pushl %ecx

	pushl %edx

	pushl $0

	pushl $10

	call _allocArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl %eax, %edi

	pushl %ecx

	pushl %edx

	pushl $0

	pushl $0

	pushl $L1

	pushl $L0

	pushl $4

	call _allocRecord

	addl $20, %esp

	popl %edx

	popl %ecx

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl $5

	call _allocArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl %eax, -4(%ebp)

	pushl %ecx

	pushl %edx

	pushl $L2

	pushl $100

	call _allocArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl %eax, -8(%ebp)

	pushl %ecx

	pushl %edx

	pushl $44

	pushl $2432

	pushl $L4

	pushl $L3

	pushl $4

	call _allocRecord

	addl $20, %esp

	popl %edx

	popl %ecx

	movl %eax, -12(%ebp)

	pushl %ecx

	pushl %edx

	pushl $1900

	pushl $3

	call _allocArray

	addl $8, %esp

	popl %edx

	popl %ecx

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl $L5

	pushl $2

	call _allocRecord

	addl $12, %esp

	popl %edx

	popl %ecx

	movl %eax, -16(%ebp)

	movl $0, %esi 

	pushl %ecx

	pushl %edx

	pushl %esi

	pushl %edi

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl %edi,%eax

	imul $4, %esi, %ebx

	addl %ebx,%eax

	movl $1, (%eax)

	movl $9, %ebx 

	pushl %ecx

	pushl %edx

	pushl %ebx

	pushl %edi

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -20(%ebp), %eax

	imul $4, %ebx, %eax

	movl %eax, -20(%ebp)

	movl -20(%ebp), %eax

	addl %eax,%edi

	movl $3, (%edi)

	movl -4(%ebp), %ebx

	movl $3, %edi 

	pushl %ecx

	pushl %edx

	pushl %edi

	pushl %ebx

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -24(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -24(%ebp)

	movl -24(%ebp), %eax

	addl %eax,%ebx

	movl (%ebx), %ebx

	movl $0, %edi 

	pushl %ecx

	pushl %edx

	pushl %ebx

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	movl -28(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -28(%ebp)

	movl -28(%ebp), %eax

	addl %eax,%ebx

	movl $L6, (%ebx)

	movl -4(%ebp), %ebx

	movl $1, %edi 

	pushl %ecx

	pushl %edx

	pushl %edi

	pushl %ebx

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -32(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -32(%ebp)

	movl -32(%ebp), %eax

	addl %eax,%ebx

	movl (%ebx), %ebx

	movl $3, %edi 

	pushl %ecx

	pushl %edx

	pushl %ebx

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	movl -36(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -36(%ebp)

	movl -36(%ebp), %eax

	addl %eax,%ebx

	movl $23, (%ebx)

	movl -8(%ebp), %ebx

	movl $34, %edi 

	pushl %ecx

	pushl %edx

	pushl %edi

	pushl %ebx

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -40(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -40(%ebp)

	movl -40(%ebp), %eax

	addl %eax,%ebx

	movl $L7, (%ebx)

	movl -4(%ebp), %ebx

	movl $3, %edi 

	pushl %ecx

	pushl %edx

	pushl %edi

	pushl %ebx

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -44(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -44(%ebp)

	movl -44(%ebp), %eax

	addl %eax,%ebx

	movl (%ebx), %ebx

	movl $0, %edi 

	pushl %ecx

	pushl %edx

	pushl %ebx

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	movl -48(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -48(%ebp)

	movl -48(%ebp), %eax

	addl %eax,%ebx

	movl (%ebx), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	movl -8(%ebp), %ebx

	movl $34, %edi 

	pushl %ecx

	pushl %edx

	pushl %edi

	pushl %ebx

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -52(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -52(%ebp)

	movl -52(%ebp), %eax

	addl %eax,%ebx

	movl (%ebx), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	movl -12(%ebp), %ebx

	movl $0, %edi 

	pushl %ecx

	pushl %edx

	pushl %ebx

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	movl -56(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -56(%ebp)

	movl -56(%ebp), %eax

	addl %eax,%ebx

	movl $L8, (%ebx)

	movl -16(%ebp), %ebx

	movl $1, %edi 

	pushl %ecx

	pushl %edx

	pushl %ebx

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	movl -60(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -60(%ebp)

	movl -60(%ebp), %eax

	addl %eax,%ebx

	movl (%ebx), %ebx

	movl $0, %edi 

	pushl %ecx

	pushl %edx

	pushl %edi

	pushl %ebx

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -64(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -64(%ebp)

	movl -64(%ebp), %eax

	addl %eax,%ebx

	movl $2323, (%ebx)

	movl -16(%ebp), %ebx

	movl $1, %edi 

	pushl %ecx

	pushl %edx

	pushl %ebx

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	movl -68(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -68(%ebp)

	movl -68(%ebp), %eax

	addl %eax,%ebx

	movl (%ebx), %ebx

	movl $2, %edi 

	pushl %ecx

	pushl %edx

	pushl %edi

	pushl %ebx

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -72(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -72(%ebp)

	movl -72(%ebp), %eax

	addl %eax,%ebx

	movl $45, (%ebx)

	movl -16(%ebp), %ebx

	movl $1, %edi 

	pushl %ecx

	pushl %edx

	pushl %ebx

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	movl -76(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -76(%ebp)

	movl -76(%ebp), %eax

	addl %eax,%ebx

	movl (%ebx), %ebx

	movl $2, %edi 

	pushl %ecx

	pushl %edx

	pushl %edi

	pushl %ebx

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -80(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -80(%ebp)

	movl -80(%ebp), %eax

	addl %eax,%ebx

	movl (%ebx), %eax

	jmp L9

L9:

	leave

	ret

