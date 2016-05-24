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

L27:
	.long 4
	.string "\x0a"

L28:
	.long 4
	.string "\x0a"

L32:
	.long 4
	.string "\x0a"

L33:
	.long 4
	.string "\x0a"

L35:
	.long 1
	.string "."

L36:
	.long 1
	.string "."

L40:
	.long 4
	.string "\x0a"

L41:
	.long 4
	.string "\x0a"

.text
	.globl _tigermain
printboard_L1:

	enter $12,$0x0

	pushl %ebx

	pushl %edi

	pushl %esi

	movl $0, %esi 

	movl 8(%ebp), %eax

	movl -4(%eax), %eax

	subl $1,%eax

	movl %eax, -8(%ebp)

	movl -8(%ebp), %eax

	cmpl %eax,%esi

	jg L2

L31:

	movl $0, %edi 

	movl 8(%ebp), %eax

	movl -4(%eax), %eax

	subl $1,%eax

	movl %eax, -4(%ebp)

	movl -4(%ebp), %eax

	cmpl %eax,%edi

	jg L3

L26:

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

	movl -12(%ebp), %eax

	imul $4, %esi, %eax

	movl %eax, -12(%ebp)

	movl -12(%ebp), %eax

	addl %eax,%ebx

	movl (%ebx), %eax

	cmpl %eax,%edi

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

	cmpl %edi,%eax

	je L3

L25:

	addl $1, %edi

	jmp L26

L21:

	movl $L16, %eax

	jmp L23

L3:

	pushl %ecx

	pushl %edx

	pushl $L28

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	movl -8(%ebp), %eax

	cmpl %esi,%eax

	je L2

L30:

	addl $1, %esi

	jmp L31

L2:

	pushl %ecx

	pushl %edx

	pushl $L33

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	jmp L74

L74:

	popl %esi

	popl %edi

	popl %ebx

	leave

	ret

try_L0:

	enter $40,$0x0

	pushl %ebx

	pushl %edi

	pushl %esi

	movl $0, %edi 

	movl 12(%ebp), %ebx

	cmpl %ebx,%edi

	jg L34

L39:

	pushl %ecx

	pushl %edx

	pushl $L36

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	cmpl %edi,%ebx

	je L34

L38:

	addl $1, %edi

	jmp L39

L34:

	pushl %ecx

	pushl %edx

	pushl $L41

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	pushl %ecx

	pushl %edx

	call flush

	popl %edx

	popl %ecx

	movl 12(%ebp), %ebx

	movl 8(%ebp), %eax

	movl -4(%eax), %eax

	cmpl %ebx,%eax

	je L71

L72:

	movl $0, %ebx 

	movl 8(%ebp), %eax

	movl -4(%eax), %esi

	subl $1,%esi

	cmpl %esi,%ebx

	jg L44

L70:

	movl 8(%ebp), %eax

	movl -8(%eax), %edi

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

	movl (%edi), %eax

	cmpl $0, %eax

	je L52

L53:

	movl $0, %eax 

L54:

	cmpl $0, %eax

	jne L60

L61:

	movl $0, %eax 

L62:

	cmpl $0, %eax

	jne L66

L67:

	cmpl %ebx,%esi

	je L44

L69:

	addl $1, %ebx

	jmp L70

L71:

	pushl %ecx

	pushl %edx

	pushl 8(%ebp)

	call printboard_L1

	addl $4, %esp

	popl %edx

	popl %ecx

L73:

	jmp L75

L52:

	movl 8(%ebp), %eax

	movl -16(%eax), %esi

	movl %ebx,%edi

	movl 12(%ebp), %eax

	addl %eax,%edi

	pushl %ecx

	pushl %edx

	pushl %edi

	pushl %esi

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -4(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -4(%ebp)

	movl -4(%ebp), %eax

	addl %eax,%esi

	movl (%esi), %eax

	cmpl $0, %eax

	je L55

L56:

	movl $0, %eax 

L57:

	jmp L54

L55:

	movl $1, %eax 

	jmp L57

L60:

	movl 8(%ebp), %eax

	movl -20(%eax), %esi

	movl %ebx,%edi

	addl $7, %edi

	movl 12(%ebp), %eax

	subl %eax,%edi

	pushl %ecx

	pushl %edx

	pushl %edi

	pushl %esi

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -8(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -8(%ebp)

	movl -8(%ebp), %eax

	addl %eax,%esi

	movl (%esi), %eax

	cmpl $0, %eax

	je L63

L64:

	movl $0, %eax 

L65:

	jmp L62

L63:

	movl $1, %eax 

	jmp L65

L66:

	movl 8(%ebp), %eax

	movl -8(%eax), %edi

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

	movl -16(%eax), %esi

	movl %ebx,%edi

	movl 12(%ebp), %eax

	addl %eax,%edi

	pushl %ecx

	pushl %edx

	pushl %edi

	pushl %esi

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -16(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -16(%ebp)

	movl -16(%ebp), %eax

	addl %eax,%esi

	movl $1, (%esi)

	movl 8(%ebp), %eax

	movl -20(%eax), %esi

	movl %ebx,%edi

	addl $7, %edi

	movl 12(%ebp), %eax

	subl %eax,%edi

	pushl %ecx

	pushl %edx

	pushl %edi

	pushl %esi

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -20(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -20(%ebp)

	movl -20(%ebp), %eax

	addl %eax,%esi

	movl $1, (%esi)

	movl 8(%ebp), %eax

	movl -12(%eax), %esi

	movl 12(%ebp), %edi

	pushl %ecx

	pushl %edx

	pushl %edi

	pushl %esi

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -24(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -24(%ebp)

	movl -24(%ebp), %eax

	addl %eax,%esi

	movl %ebx, (%esi)

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

	movl -16(%eax), %esi

	movl %ebx,%edi

	movl 12(%ebp), %eax

	addl %eax,%edi

	pushl %ecx

	pushl %edx

	pushl %edi

	pushl %esi

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -32(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -32(%ebp)

	movl -32(%ebp), %eax

	addl %eax,%esi

	movl $0, (%esi)

	movl 8(%ebp), %eax

	movl -20(%eax), %edi

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

	jmp L67

L44:

	jmp L73

L75:

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

	pushl $0

	pushl %ebp

	call try_L0

	addl $8, %esp

	popl %edx

	popl %ecx

	movl $0, %eax 

	jmp L76

L76:

	popl %esi

	popl %edi

	popl %ebx

	leave

	ret

