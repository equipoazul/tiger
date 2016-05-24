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

L30:
	.long 1
	.string "."

L31:
	.long 1
	.string "."

L34:
	.long 4
	.string "\x0a"

L35:
	.long 4
	.string "\x0a"

L38:
	.long 21
	.string "ACA VA EL TABLERO\x0a"

L39:
	.long 21
	.string "ACA VA EL TABLERO\x0a"

.text
	.globl _tigermain
f_L2:

	enter $0,$0x0

	pushl %ebx

	pushl %edi

	pushl %esi

	movl 12(%ebp), %eax

	cmpl $0, %eax

	jg L13

L14:

	jmp L50

L13:

	movl 12(%ebp), %eax

	movl $10, %edi

	xorl %edx, %edx

	idivl %edi

	movl %eax,%edi

	pushl %ecx

	pushl %edx

	pushl %edi

	pushl 8(%ebp)

	call f_L2

	addl $8, %esp

	popl %edx

	popl %ecx

	movl 12(%ebp), %edi

	movl 12(%ebp), %eax

	movl $10, %esi

	xorl %edx, %edx

	idivl %esi

	movl %eax,%esi

	imul $10, %esi, %ebx

	subl %ebx,%edi

	pushl %ecx

	pushl %edx

	pushl $L12

	call ord

	addl $4, %esp

	popl %edx

	popl %ecx

	addl %eax,%edi

	pushl %ecx

	pushl %edx

	pushl %edi

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

L50:

	popl %esi

	popl %edi

	popl %ebx

	leave

	ret

printint_L1:

	enter $0,$0x0

	pushl %ebx

	pushl %edi

	pushl %esi

	movl 12(%ebp), %eax

	cmpl $0, %eax

	jl L26

L27:

	movl 12(%ebp), %eax

	cmpl $0, %eax

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

	jmp L51

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

L51:

	popl %esi

	popl %edi

	popl %ebx

	leave

	ret

try_L0:

	enter $16,$0x0

	pushl %ebx

	pushl %edi

	pushl %esi

	movl $0, %edi 

	movl 12(%ebp), %ebx

	cmpl %ebx,%edi

	jg L29

L33:

	pushl %ecx

	pushl %edx

	pushl $L31

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	cmpl %edi,%ebx

	je L29

L32:

	addl $1, %edi

	jmp L33

L29:

	pushl %ecx

	pushl %edx

	pushl $L35

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

	je L47

L48:

	movl $0, %esi 

	movl 8(%ebp), %eax

	movl -4(%eax), %edi

	subl $1,%edi

	cmpl %edi,%esi

	jg L40

L46:

	movl 8(%ebp), %eax

	movl -8(%eax), %ebx

	pushl %ecx

	pushl %edx

	pushl %esi

	pushl %ebx

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -4(%ebp), %eax

	imul $4, %esi, %eax

	movl %eax, -4(%ebp)

	movl -4(%ebp), %eax

	addl %eax,%ebx

	movl (%ebx), %eax

	cmpl $0, %eax

	je L43

L44:

	cmpl %esi,%edi

	je L40

L45:

	addl $1, %esi

	jmp L46

L47:

	pushl %ecx

	pushl %edx

	pushl $L39

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

L49:

	jmp L52

L43:

	movl 8(%ebp), %eax

	movl -8(%eax), %ebx

	pushl %ecx

	pushl %edx

	pushl %esi

	pushl %ebx

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -8(%ebp), %eax

	imul $4, %esi, %eax

	movl %eax, -8(%ebp)

	movl -8(%ebp), %eax

	addl %eax,%ebx

	movl $1, (%ebx)

	movl 8(%ebp), %eax

	movl -8(%eax), %ebx

	movl $1, %edi 

	pushl %ecx

	pushl %edx

	pushl %edi

	pushl %ebx

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -12(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -12(%ebp)

	movl -12(%ebp), %eax

	addl %eax,%ebx

	movl $10, (%ebx)

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

	movl -8(%eax), %ebx

	pushl %ecx

	pushl %edx

	pushl %esi

	pushl %ebx

	call _checkIndexArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl -16(%ebp), %eax

	imul $4, %esi, %eax

	movl %eax, -16(%ebp)

	movl -16(%ebp), %eax

	addl %eax,%ebx

	movl $0, (%ebx)

	jmp L44

L40:

	jmp L49

L52:

	popl %esi

	popl %edi

	popl %ebx

	leave

	ret

_tigermain:

	enter $8,$0x0

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

	movl %eax, %edi

	movl %edi, (%ebx)

	pushl %ecx

	pushl %edx

	pushl $0

	pushl -4(%ebp)

	call _allocArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl %eax, %ebx

	pushl %ecx

	pushl %edx

	pushl $0

	pushl %ebp

	call try_L0

	addl $8, %esp

	popl %edx

	popl %ecx

	movl $0, %eax 

	jmp L53

L53:

	popl %esi

	popl %edi

	popl %ebx

	leave

	ret

