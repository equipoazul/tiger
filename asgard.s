.data
L6:
	.long 2
	.string " O"

L7:
	.long 2
	.string " ."

L16:
	.long 2
	.string " O"

L17:
	.long 2
	.string " ."

L26:
	.long 4
	.string "\x0a"

L27:
	.long 4
	.string "\x0a"

L30:
	.long 4
	.string "\x0a"

L31:
	.long 4
	.string "\x0a"

.text
	.globl _tigermain
printboard_L1:

	enter $24,$0x0

	movl %ebx, -12(%ebp)

	movl %edi, -16(%ebp)

	movl %esi, -20(%ebp)

	movl $0, %esi 

	movl 8(%ebp), %eax

	movl -4(%eax), %eax

	subl $1,%eax

	movl %eax, -8(%ebp)

	movl -8(%ebp), %eax

	cmpl %esi,%eax

	jg L2

L29:

	movl $0, %edi 

	movl 8(%ebp), %eax

	movl -4(%eax), %eax

	subl $1,%eax

	movl %eax, -4(%ebp)

	movl -4(%ebp), %eax

	cmpl %edi,%eax

	jg L3

L25:

	movl 8(%ebp), %eax

	movl -12(%eax), %ebx

	pushl %ecx

	pushl %edx

	pushl %esi

	pushl %ebx

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -24(%ebp), %eax

	imul $4, %esi, %eax

	movl %eax, -24(%ebp)

	movl -24(%ebp), %eax

	addl %eax,%ebx

	movl (%ebx), %eax

	cmpl %edi,%eax

	je L21

L22:

	movl $L17, %eax

L23:

	pushl %ecx

	pushl %edx

	pushl %eax

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	movl -4(%ebp), %eax

	cmpl %eax,%edi

	je L3

L24:

	addl $1, %edi

	jmp L25

L21:

	movl $L16, %eax

	jmp L23

L3:

	pushl %ecx

	pushl %edx

	pushl $L27

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	movl -8(%ebp), %eax

	cmpl %eax,%esi

	je L2

L28:

	addl $1, %esi

	jmp L29

L2:

	pushl %ecx

	pushl %edx

	pushl $L31

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	movl -20(%ebp), %esi

	movl -16(%ebp), %edi

	movl -12(%ebp), %ebx

	jmp L63

L63:

	leave

	ret

try_L0:

	enter $56,$0x0

	movl %ebx, -44(%ebp)

	movl %edi, -48(%ebp)

	movl %esi, -52(%ebp)

	movl 12(%ebp), %ebx

	movl 8(%ebp), %eax

	movl -4(%eax), %eax

	cmpl %eax,%ebx

	je L60

L61:

	movl $0, %eax 

	movl %eax, -40(%ebp)

	movl 8(%ebp), %eax

	movl -4(%eax), %edi

	subl $1,%edi

	movl -40(%ebp), %eax

	cmpl %eax,%edi

	jg L34

L59:

	movl 8(%ebp), %eax

	movl -8(%eax), %ebx

	movl -40(%ebp), %esi

	pushl %ecx

	pushl %edx

	pushl %esi

	pushl %ebx

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -56(%ebp), %eax

	imul $4, %esi, %eax

	movl %eax, -56(%ebp)

	movl -56(%ebp), %eax

	addl %eax,%ebx

	movl (%ebx), %eax

	cmpl $0, %eax

	je L42

L43:

	movl $0, %eax 

L44:

	cmpl $0, %eax

	jne L50

L51:

	movl $0, %eax 

L52:

	cmpl $0, %eax

	jne L56

L57:

	movl -40(%ebp), %eax

	cmpl %edi,%eax

	je L34

L58:

	movl -40(%ebp), %eax

	addl $1, %eax

	movl %eax, -40(%ebp)

	jmp L59

L60:

	pushl %ecx

	pushl %edx

	pushl 8(%ebp)

	call printboard_L1

	addl $4, %esp

	popl %edx

	popl %ecx

L62:

	movl -52(%ebp), %esi

	movl -48(%ebp), %edi

	movl -44(%ebp), %ebx

	jmp L64

L42:

	movl 8(%ebp), %eax

	movl -16(%eax), %edi

	movl -40(%ebp), %ebx

	movl 12(%ebp), %eax

	addl %eax,%ebx

	pushl %ecx

	pushl %edx

	pushl %ebx

	pushl %edi

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -4(%ebp), %eax

	imul $4, %ebx, %eax

	movl %eax, -4(%ebp)

	movl -4(%ebp), %eax

	addl %eax,%edi

	movl (%edi), %eax

	cmpl $0, %eax

	je L45

L46:

	movl $0, %eax 

L47:

	jmp L44

L45:

	movl $1, %eax 

	jmp L47

L50:

	movl 8(%ebp), %eax

	movl -20(%eax), %edi

	movl -40(%ebp), %ebx

	addl $7, %ebx

	movl 12(%ebp), %eax

	subl %eax,%ebx

	pushl %ecx

	pushl %edx

	pushl %ebx

	pushl %edi

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -8(%ebp), %eax

	imul $4, %ebx, %eax

	movl %eax, -8(%ebp)

	movl -8(%ebp), %eax

	addl %eax,%edi

	movl (%edi), %eax

	cmpl $0, %eax

	je L53

L54:

	movl $0, %eax 

L55:

	jmp L52

L53:

	movl $1, %eax 

	jmp L55

L56:

	movl 8(%ebp), %eax

	movl -8(%eax), %edi

	movl -40(%ebp), %ebx

	pushl %ecx

	pushl %edx

	pushl %ebx

	pushl %edi

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -12(%ebp), %eax

	imul $4, %ebx, %eax

	movl %eax, -12(%ebp)

	movl -12(%ebp), %eax

	addl %eax,%edi

	movl $1, (%edi)

	movl 8(%ebp), %eax

	movl -16(%eax), %edi

	movl -40(%ebp), %ebx

	movl 12(%ebp), %eax

	addl %eax,%ebx

	pushl %ecx

	pushl %edx

	pushl %ebx

	pushl %edi

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -16(%ebp), %eax

	imul $4, %ebx, %eax

	movl %eax, -16(%ebp)

	movl -16(%ebp), %eax

	addl %eax,%edi

	movl $1, (%edi)

	movl 8(%ebp), %eax

	movl -20(%eax), %edi

	movl -40(%ebp), %ebx

	addl $7, %ebx

	movl 12(%ebp), %eax

	subl %eax,%ebx

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

	movl $1, (%edi)

	movl 8(%ebp), %eax

	movl -12(%eax), %edi

	movl 12(%ebp), %ebx

	pushl %ecx

	pushl %edx

	pushl %ebx

	pushl %edi

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -24(%ebp), %eax

	imul $4, %ebx, %eax

	movl %eax, -24(%ebp)

	movl -24(%ebp), %eax

	addl %eax,%edi

	movl -40(%ebp), %eax

	movl %eax, (%edi)

	movl 12(%ebp), %eax

	addl $1, %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl 8(%ebp)

	call try_L0

	addl $8, %esp

	popl %edx

	popl %ecx

	movl 8(%ebp), %eax

	movl -8(%eax), %edi

	movl -40(%ebp), %ebx

	pushl %ecx

	pushl %edx

	pushl %ebx

	pushl %edi

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -28(%ebp), %eax

	imul $4, %ebx, %eax

	movl %eax, -28(%ebp)

	movl -28(%ebp), %eax

	addl %eax,%edi

	movl $0, (%edi)

	movl 8(%ebp), %eax

	movl -16(%eax), %edi

	movl -40(%ebp), %ebx

	movl 12(%ebp), %eax

	addl %eax,%ebx

	pushl %ecx

	pushl %edx

	pushl %ebx

	pushl %edi

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -32(%ebp), %eax

	imul $4, %ebx, %eax

	movl %eax, -32(%ebp)

	movl -32(%ebp), %eax

	addl %eax,%edi

	movl $0, (%edi)

	movl 8(%ebp), %eax

	movl -20(%eax), %edi

	movl -40(%ebp), %ebx

	addl $7, %ebx

	movl 12(%ebp), %eax

	subl %eax,%ebx

	pushl %ecx

	pushl %edx

	pushl %ebx

	pushl %edi

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -36(%ebp), %eax

	imul $4, %ebx, %eax

	movl %eax, -36(%ebp)

	movl -36(%ebp), %eax

	addl %eax,%edi

	movl $0, (%edi)

	jmp L57

L34:

	jmp L62

L64:

	leave

	ret

_tigermain:

	enter $28,$0x0

	movl %ebx, -24(%ebp)

	movl %edi, -28(%ebp)

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

	movl %eax, %edi

	movl %edi, (%ebx)

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

	movl %eax, %edi

	movl %edi, (%ebx)

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

	movl %eax, %edi

	movl %edi, (%ebx)

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

	movl %eax, %edi

	movl %edi, (%ebx)

	pushl %ecx

	pushl %edx

	pushl $0

	pushl %ebp

	call try_L0

	addl $8, %esp

	popl %edx

	popl %ecx

	movl $0, %eax 

	movl -28(%ebp), %edi

	movl -24(%ebp), %ebx

	jmp L65

L65:

	leave

	ret

