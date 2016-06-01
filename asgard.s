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

	enter $36,$0x0

	movl %ebx, -12(%ebp)

	movl %edi, -16(%ebp)

	movl %esi, -20(%ebp)

	movl $0, %edi 

	movl 8(%ebp), %eax

	movl -4(%eax), %eax

	subl $1,%eax

	movl %eax, -8(%ebp)

	movl -8(%ebp), %eax

	cmpl %eax,%edi

	movl %eax, -8(%ebp)

	jge L2

L29:

	movl $0, %eax 

	movl %eax, -36(%ebp)

	movl 8(%ebp), %eax

	movl -4(%eax), %eax

	subl $1,%eax

	movl %eax, -4(%ebp)

	movl -4(%ebp), %ebx

	movl -36(%ebp), %eax

	cmpl %ebx,%eax

	movl %eax, -36(%ebp)

	movl %ebx, -4(%ebp)

	jge L3

L25:

	movl 8(%ebp), %eax

	movl -12(%eax), %ebx

	pushl %ecx

	pushl %edx

	pushl %edi

	pushl %ebx

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl %ebx,%eax

	movl -28(%ebp), %esi

	imul $4, %edi, %esi

	movl %esi, -28(%ebp)

	movl -28(%ebp), %esi

	addl %esi,%eax

	movl (%eax), %eax

	movl %eax, -24(%ebp)

	movl -32(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -32(%ebp)

	movl -32(%ebp), %eax

	addl %eax,%ebx

	movl (%ebx), %eax

	movl -24(%ebp), %eax

	movl -36(%ebp), %ebx

	cmpl %ebx,%eax

	movl %ebx, -36(%ebp)

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

	movl -4(%ebp), %ebx

	movl -36(%ebp), %eax

	cmpl %ebx,%eax

	movl %eax, -36(%ebp)

	movl %ebx, -4(%ebp)

	je L3

L24:

	movl -36(%ebp), %eax

	addl $1, %eax

	movl %eax, -36(%ebp)

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

	cmpl %eax,%edi

	movl %eax, -8(%ebp)

	je L2

L28:

	addl $1, %edi

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

	enter $88,$0x0

	movl %ebx, -64(%ebp)

	movl %edi, -68(%ebp)

	movl %esi, -72(%ebp)

	movl 12(%ebp), %edi

	movl 8(%ebp), %eax

	movl -4(%eax), %ebx

	movl 12(%ebp), %esi

	movl 8(%ebp), %eax

	movl -4(%eax), %eax

	cmpl %ebx,%edi

	je L60

L61:

	movl $0, %eax 

	movl %eax, -60(%ebp)

	movl 8(%ebp), %eax

	movl -4(%eax), %eax

	subl $1,%eax

	movl %eax, -88(%ebp)

	movl -60(%ebp), %ebx

	movl -88(%ebp), %eax

	cmpl %eax,%ebx

	movl %eax, -88(%ebp)

	movl %ebx, -60(%ebp)

	jge L34

L59:

	movl 8(%ebp), %eax

	movl -8(%eax), %edi

	movl -60(%ebp), %ebx

	pushl %ecx

	pushl %edx

	pushl %ebx

	pushl %edi

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl $0, %eax

	movl %eax, -76(%ebp)

	movl %edi,%eax

	movl -8(%ebp), %esi

	imul $4, %ebx, %esi

	movl %esi, -8(%ebp)

	movl -8(%ebp), %esi

	addl %esi,%eax

	movl (%eax), %eax

	movl %eax, -4(%ebp)

	movl -12(%ebp), %eax

	imul $4, %ebx, %eax

	movl %eax, -12(%ebp)

	movl -12(%ebp), %eax

	addl %eax,%edi

	movl (%edi), %ebx

	movl -4(%ebp), %ebx

	movl -76(%ebp), %eax

	cmpl %eax, %ebx

	movl %eax, -76(%ebp)

	je L42

L43:

	movl $0, %eax 

L44:

	movl $0, %ebx

	cmpl %ebx, %eax

	jne L50

L51:

	movl $0, %eax 

L52:

	movl $0, %ebx

	cmpl %ebx, %eax

	jne L56

L57:

	movl -60(%ebp), %ebx

	movl -88(%ebp), %eax

	cmpl %eax,%ebx

	movl %eax, -88(%ebp)

	movl %ebx, -60(%ebp)

	je L34

L58:

	movl -60(%ebp), %eax

	addl $1, %eax

	movl %eax, -60(%ebp)

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

	movl -72(%ebp), %esi

	movl -68(%ebp), %edi

	movl -64(%ebp), %ebx

	jmp L64

L42:

	movl 8(%ebp), %eax

	movl -16(%eax), %edi

	movl -60(%ebp), %ebx

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

	movl $0, %eax

	movl %eax, -80(%ebp)

	movl %edi,%eax

	movl -16(%ebp), %esi

	imul $4, %ebx, %esi

	movl %esi, -16(%ebp)

	movl -16(%ebp), %esi

	addl %esi,%eax

	movl (%eax), %eax

	movl -20(%ebp), %esi

	imul $4, %ebx, %esi

	movl %esi, -20(%ebp)

	movl -20(%ebp), %ebx

	addl %ebx,%edi

	movl (%edi), %edi

	movl -80(%ebp), %ebx

	cmpl %ebx, %eax

	movl %ebx, -80(%ebp)

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

	movl -60(%ebp), %ebx

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

	movl $0, %eax

	movl %eax, -84(%ebp)

	movl %edi,%eax

	movl -24(%ebp), %esi

	imul $4, %ebx, %esi

	movl %esi, -24(%ebp)

	movl -24(%ebp), %esi

	addl %esi,%eax

	movl (%eax), %eax

	movl -28(%ebp), %esi

	imul $4, %ebx, %esi

	movl %esi, -28(%ebp)

	movl -28(%ebp), %ebx

	addl %ebx,%edi

	movl (%edi), %edi

	movl -84(%ebp), %ebx

	cmpl %ebx, %eax

	movl %ebx, -84(%ebp)

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

	movl -60(%ebp), %ebx

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

	movl $1, (%edi)

	movl 8(%ebp), %eax

	movl -16(%eax), %edi

	movl -60(%ebp), %ebx

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

	movl -36(%ebp), %eax

	imul $4, %ebx, %eax

	movl %eax, -36(%ebp)

	movl -36(%ebp), %eax

	addl %eax,%edi

	movl $1, (%edi)

	movl 8(%ebp), %eax

	movl -20(%eax), %edi

	movl -60(%ebp), %ebx

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

	movl -40(%ebp), %eax

	imul $4, %ebx, %eax

	movl %eax, -40(%ebp)

	movl -40(%ebp), %eax

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

	movl -44(%ebp), %eax

	imul $4, %ebx, %eax

	movl %eax, -44(%ebp)

	movl -44(%ebp), %eax

	addl %eax,%edi

	movl -60(%ebp), %eax

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

	movl -60(%ebp), %ebx

	pushl %ecx

	pushl %edx

	pushl %ebx

	pushl %edi

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -48(%ebp), %eax

	imul $4, %ebx, %eax

	movl %eax, -48(%ebp)

	movl -48(%ebp), %eax

	addl %eax,%edi

	movl $0, (%edi)

	movl 8(%ebp), %eax

	movl -16(%eax), %edi

	movl -60(%ebp), %ebx

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

	movl -52(%ebp), %eax

	imul $4, %ebx, %eax

	movl %eax, -52(%ebp)

	movl -52(%ebp), %eax

	addl %eax,%edi

	movl $0, (%edi)

	movl 8(%ebp), %eax

	movl -20(%eax), %edi

	movl -60(%ebp), %ebx

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

	movl -56(%ebp), %eax

	imul $4, %ebx, %eax

	movl %eax, -56(%ebp)

	movl -56(%ebp), %eax

	addl %eax,%edi

	movl $0, (%edi)

	jmp L57

L34:

	jmp L62

L64:

	leave

	ret

_tigermain:

	enter $24,$0x0

	movl %ebx, -24(%ebp)

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

	pushl $0

	pushl %ebp

	call try_L0

	addl $8, %esp

	popl %edx

	popl %ecx

	movl $0, %eax 

	movl -24(%ebp), %ebx

	jmp L65

L65:

	leave

	ret

