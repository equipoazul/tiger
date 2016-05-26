.data
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

L13:
	.long 1
	.string "0"

L18:
	.long 1
	.string "-"

L19:
	.long 1
	.string "-"

L22:
	.long 1
	.string "0"

L23:
	.long 1
	.string "0"

L30:
	.long 5
	.string " r = "

L31:
	.long 5
	.string " r = "

L32:
	.long 5
	.string " c = "

L33:
	.long 5
	.string " c = "

L34:
	.long 5
	.string " N = "

L35:
	.long 5
	.string " N = "

L36:
	.long 4
	.string "\x0a"

L37:
	.long 4
	.string "\x0a"

L40:
	.long 11
	.string "Tablero\x0a"

L41:
	.long 11
	.string "Tablero\x0a"

L44:
	.long 28
	.string "Esto NO deberia pasar...\x0a"

L45:
	.long 28
	.string "Esto NO deberia pasar...\x0a"

L48:
	.long 14
	.string "ANTES While ->"

L49:
	.long 14
	.string "ANTES While ->"

L53:
	.long 12
	.string "ANTES try ->"

L54:
	.long 12
	.string "ANTES try ->"

L55:
	.long 10
	.string "Despues ->"

L56:
	.long 10
	.string "Despues ->"

.text
	.globl _tigermain
f_L3:

	enter $4,$0x0

	movl 12(%ebp), %eax

	cmpl $0, %eax

	jg L14

L15:

	jmp L62

L14:

	movl 12(%ebp), %eax

	movl $10, %ebx

	xorl %edx, %edx

	idivl %ebx

	movl %eax,%ebx

	pushl %ecx

	pushl %edx

	pushl %ebx

	pushl 8(%ebp)

	call f_L3

	addl $8, %esp

	popl %edx

	popl %ecx

	movl 12(%ebp), %ebx

	movl 12(%ebp), %eax

	movl $10, %edi

	xorl %edx, %edx

	idivl %edi

	movl %eax,%edi

	movl -4(%ebp), %eax

	imul $10, %edi, %eax

	movl %eax, -4(%ebp)

	movl -4(%ebp), %eax

	subl %eax,%ebx

	pushl %ecx

	pushl %edx

	pushl $L13

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

	jmp L15

L62:

	leave

	ret

printint_L2:

	enter $0,$0x0

	movl 12(%ebp), %eax

	cmpl $0, %eax

	jl L27

L28:

	movl 12(%ebp), %eax

	cmpl $0, %eax

	jg L24

L25:

	pushl %ecx

	pushl %edx

	pushl $L23

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

L26:

L29:

	jmp L63

L27:

	pushl %ecx

	pushl %edx

	pushl $L19

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

	call f_L3

	addl $8, %esp

	popl %edx

	popl %ecx

	jmp L29

L24:

	movl 12(%ebp), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl %ebp

	call f_L3

	addl $8, %esp

	popl %edx

	popl %ecx

	jmp L26

L63:

	leave

	ret

debug_L1:

	enter $0,$0x0

	movl 12(%ebp), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	pushl %ecx

	pushl %edx

	pushl $L31

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	movl 16(%ebp), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl 8(%ebp)

	call printint_L2

	addl $8, %esp

	popl %edx

	popl %ecx

	pushl %ecx

	pushl %edx

	pushl $L33

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	movl 20(%ebp), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl 8(%ebp)

	call printint_L2

	addl $8, %esp

	popl %edx

	popl %ecx

	pushl %ecx

	pushl %edx

	pushl $L35

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	movl 24(%ebp), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl 8(%ebp)

	call printint_L2

	addl $8, %esp

	popl %edx

	popl %ecx

	pushl %ecx

	pushl %edx

	pushl $L37

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	jmp L64

L64:

	leave

	ret

try_L0:

	enter $8,$0x0

	movl %ebx, -4(%ebp)

	movl %edi, -8(%ebp)

	movl 12(%ebp), %ebx

	movl 8(%ebp), %eax

	movl -4(%eax), %eax

	cmpl %eax,%ebx

	je L59

L60:

	movl 12(%ebp), %ebx

	movl 8(%ebp), %eax

	movl -4(%eax), %eax

	cmpl %eax,%ebx

	je L46

L47:

	movl 8(%ebp), %eax

	addl $-8, %eax

	movl $0, (%eax)

	movl 8(%ebp), %eax

	movl -8(%eax), %edi

	movl 12(%ebp), %ebx

	movl 8(%ebp), %eax

	movl -4(%eax), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl %ebx

	pushl %edi

	pushl $L49

	pushl 8(%ebp)

	call debug_L1

	addl $20, %esp

	popl %edx

	popl %ecx

L57:

	movl 8(%ebp), %eax

	movl -8(%eax), %eax

	cmpl $3, %eax

	jl L58

L52:

L61:

	movl -8(%ebp), %edi

	movl -4(%ebp), %ebx

	jmp L65

L59:

	pushl %ecx

	pushl %edx

	pushl $L41

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	jmp L61

L46:

	pushl %ecx

	pushl %edx

	pushl $L45

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	jmp L47

L58:

	movl 8(%ebp), %eax

	movl -8(%eax), %eax

	movl 12(%ebp), %edi

	movl 8(%ebp), %ebx

	movl -4(%ebx), %ebx

	pushl %ecx

	pushl %edx

	pushl %ebx

	pushl %edi

	pushl %eax

	pushl $L54

	pushl 8(%ebp)

	call debug_L1

	addl $20, %esp

	popl %edx

	popl %ecx

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

	movl 12(%ebp), %ebx

	movl 8(%ebp), %eax

	movl -4(%eax), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl %ebx

	pushl %edi

	pushl $L56

	pushl 8(%ebp)

	call debug_L1

	addl $20, %esp

	popl %edx

	popl %ecx

	movl 8(%ebp), %ebx

	addl $-8, %ebx

	movl 8(%ebp), %eax

	movl -8(%eax), %eax

	addl $1, %eax

	movl %eax, (%ebx)

	jmp L57

L65:

	leave

	ret

_tigermain:

	enter $16,$0x0

	movl %ebx, -16(%ebp)

	movl %ebp,%eax

	addl $-4, %eax

	movl $8, (%eax)

	movl %ebp,%eax

	addl $-8, %eax

	movl $0, (%eax)

	pushl %ecx

	pushl %edx

	pushl $0

	pushl -4(%ebp)

	call _allocArray

	addl $8, %esp

	popl %edx

	popl %ecx

	movl %eax, %ebx

	movl %ebx, -12(%ebp)

	pushl %ecx

	pushl %edx

	pushl $0

	pushl %ebp

	call try_L0

	addl $8, %esp

	popl %edx

	popl %ecx

	movl $0, %eax 

	movl -16(%ebp), %ebx

	jmp L66

L66:

	leave

	ret

