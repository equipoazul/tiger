.data
L3:
	.long 1
	.string "0"

L4:
	.long 1
	.string "0"

L7:
	.long 1
	.string "9"

L8:
	.long 1
	.string "9"

L20:
	.long 1
	.string " "

L21:
	.long 4
	.string "\x0a"

L32:
	.long 9
	.string "queso\x0a"

L33:
	.long 9
	.string "queso\x0a"

L36:
	.long 14
	.string "j3i3i3i3i3\x0a"

L37:
	.long 14
	.string "j3i3i3i3i3\x0a"

L39:
	.long 1
	.string "0"

L40:
	.long 1
	.string "0"

L41:
	.long 12
	.string "l3 l00p?\x0a"

L42:
	.long 12
	.string "l3 l00p?\x0a"

L79:
	.long 1
	.string "0"

L80:
	.long 1
	.string "0"

L81:
	.long 1
	.string "0"

L82:
	.long 1
	.string "0"

L83:
	.long 1
	.string "0"

L84:
	.long 1
	.string "0"

L85:
	.long 1
	.string "0"

L86:
	.long 1
	.string "0"

L91:
	.long 1
	.string "-"

L92:
	.long 1
	.string "-"

L95:
	.long 1
	.string "0"

L96:
	.long 1
	.string "0"

L105:
	.long 9
	.string "LEUS PUTO"

L106:
	.long 9
	.string "LEUS PUTO"

L107:
	.long 1
	.string " "

L108:
	.long 1
	.string " "

.text
	.globl _tigermain
isdigit_L2:

	enter $0,$0x0

	pushl %ebx

	pushl %edi

	pushl %esi

	pushl %ecx

	pushl %edx

	pushl $L4

	call ord

	addl $4, %esp

	popl %edx

	popl %ecx

	movl %eax,%ebx

	movl 12(%ebp), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	call ord

	addl $4, %esp

	popl %edx

	popl %ecx

	cmpl %ebx,%eax

	jle L14

L15:

	movl $0, %eax 

L16:

	jmp L112

L14:

	movl 12(%ebp), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	call ord

	addl $4, %esp

	popl %edx

	popl %ecx

	movl %eax,%ebx

	pushl %ecx

	pushl %edx

	pushl $L8

	call ord

	addl $4, %esp

	popl %edx

	popl %ecx

	cmpl %ebx,%eax

	jle L17

L18:

	movl $0, %eax 

L19:

	jmp L16

L17:

	movl $1, %eax 

	jmp L19

L112:

	popl %esi

	popl %edi

	popl %ebx

	leave

	ret

skipto_L1:

	enter $0,$0x0

	pushl %ebx

	pushl %edi

	pushl %esi

L34:

	movl 8(%ebp), %eax

	movl 8(%eax), %eax

	movl -4(%eax), %eax

	movl $L20, %ebx

	pushl %ecx

	pushl %edx

	pushl %ebx

	pushl %eax

	call _stringcmp

	addl $8, %esp

	popl %edx

	popl %ecx

	cmpl $0, %eax

	je L25

L26:

	movl 8(%ebp), %eax

	movl 8(%eax), %eax

	movl -4(%eax), %eax

	movl $L21, %ebx

	pushl %ecx

	pushl %edx

	pushl %ebx

	pushl %eax

	call _stringcmp

	addl $8, %esp

	popl %edx

	popl %ecx

	cmpl $0, %eax

	je L28

L29:

	movl $0, %eax 

L30:

L27:

	cmpl $0, %eax

	jne L35

L31:

	jmp L113

L25:

	movl $1, %eax 

	jmp L27

L28:

	movl $1, %eax 

	jmp L30

L35:

	movl 8(%ebp), %eax

	movl 8(%eax), %ebx

	addl $-4, %ebx

	pushl %ecx

	pushl %edx

	call getstr

	popl %edx

	popl %ecx

	movl %eax, (%ebx)

	pushl %ecx

	pushl %edx

	pushl $L33

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	jmp L34

L113:

	popl %esi

	popl %edi

	popl %ebx

	leave

	ret

readint_L0:

	enter $0,$0x0

	pushl %ebx

	pushl %edi

	pushl %esi

	movl $0, %ebx 

	pushl %ecx

	pushl %edx

	pushl %ebp

	call skipto_L1

	addl $4, %esp

	popl %edx

	popl %ecx

	movl 12(%ebp), %edi

	movl $0, %esi 

	pushl %ecx

	pushl %edx

	pushl %edi

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	imul $4, %esi, %eax

	addl %eax,%edi

	movl 8(%ebp), %eax

	movl -4(%eax), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl %ebp

	call isdigit_L2

	addl $8, %esp

	popl %edx

	popl %ecx

	movl %eax, (%edi)

	pushl %ecx

	pushl %edx

	pushl $L37

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

L43:

	movl 8(%ebp), %eax

	movl -4(%eax), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl %ebp

	call isdigit_L2

	addl $8, %esp

	popl %edx

	popl %ecx

	cmpl $0, %eax

	jne L44

L38:

	movl %ebx,%eax

	jmp L114

L44:

	imul $10, %ebx, %ebx

	movl 8(%ebp), %eax

	movl -4(%eax), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	call ord

	addl $4, %esp

	popl %edx

	popl %ecx

	addl %eax,%ebx

	pushl %ecx

	pushl %edx

	pushl $L40

	call ord

	addl $4, %esp

	popl %edx

	popl %ecx

	subl %eax,%ebx

	movl 8(%ebp), %ebx

	addl $-4, %ebx

	pushl %ecx

	pushl %edx

	call getstr

	popl %edx

	popl %ecx

	movl %eax, (%ebx)

	pushl %ecx

	pushl %edx

	pushl $L42

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	jmp L43

L114:

	popl %esi

	popl %edi

	popl %ebx

	leave

	ret

readlist_L48:

	enter $0,$0x0

	pushl %ebx

	pushl %edi

	pushl %esi

	pushl %ecx

	pushl %edx

	pushl $0

	pushl $1

	call _allocRecord

	addl $8, %esp

	popl %edx

	popl %ecx

	movl %eax, %esi

	pushl %ecx

	pushl %edx

	pushl %esi

	pushl 8(%ebp)

	call readint_L0

	addl $8, %esp

	popl %edx

	popl %ecx

	movl %eax,%ebx

	movl $0, %edi 

	pushl %ecx

	pushl %edx

	pushl %esi

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	imul $4, %edi, %eax

	addl %eax,%esi

	movl (%esi), %eax

	cmpl $0, %eax

	jne L49

L50:

	movl $0, %eax 

L51:

	jmp L115

L49:

	pushl %ecx

	pushl %edx

	pushl 8(%ebp)

	call readlist_L48

	addl $4, %esp

	popl %edx

	popl %ecx

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl %ebx

	pushl $2

	call _allocRecord

	addl $12, %esp

	popl %edx

	popl %ecx

	jmp L51

L115:

	popl %esi

	popl %edi

	popl %ebx

	leave

	ret

merge_L47:

	enter $16,$0x0

	pushl %ebx

	pushl %edi

	pushl %esi

	movl 12(%ebp), %eax

	cmpl $0, %eax

	je L73

L74:

	movl 16(%ebp), %eax

	cmpl $0, %eax

	je L67

L68:

	movl 12(%ebp), %ebx

	movl $0, %edi 

	pushl %ecx

	pushl %edx

	pushl %ebx

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	imul $4, %edi, %eax

	addl %eax,%ebx

	movl (%ebx), %esi

	movl 16(%ebp), %edi

	movl $0, %ebx 

	pushl %ecx

	pushl %edx

	pushl %edi

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	imul $4, %ebx, %eax

	addl %eax,%edi

	movl (%edi), %eax

	cmpl %esi,%eax

	jl L61

L62:

	movl 16(%ebp), %ebx

	movl $0, %edi 

	pushl %ecx

	pushl %edx

	pushl %ebx

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	imul $4, %edi, %eax

	addl %eax,%ebx

	movl (%ebx), %eax

	movl %eax, -4(%ebp)

	movl -4(%ebp), %eax

	movl %eax, -12(%ebp)

	movl -12(%ebp), %ebx

	movl 12(%ebp), %eax

	movl %eax, -8(%ebp)

	movl -8(%ebp), %edi

	movl 16(%ebp), %esi

	movl $1, %eax 

	movl %eax, -16(%ebp)

	pushl %ecx

	pushl %edx

	pushl %esi

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	movl -16(%ebp), %eax

	imul $4, %eax, %eax

	addl %eax,%esi

	movl (%esi), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl %edi

	pushl 8(%ebp)

	call merge_L47

	addl $12, %esp

	popl %edx

	popl %ecx

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl %ebx

	pushl $2

	call _allocRecord

	addl $12, %esp

	popl %edx

	popl %ecx

L63:

L69:

L75:

	jmp L116

L73:

	movl 16(%ebp), %eax

	jmp L75

L67:

	movl 12(%ebp), %eax

	jmp L69

L61:

	movl 12(%ebp), %ebx

	movl $0, %edi 

	pushl %ecx

	pushl %edx

	pushl %ebx

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	imul $4, %edi, %eax

	addl %eax,%ebx

	movl (%ebx), %edi

	movl 12(%ebp), %esi

	movl $1, %ebx 

	pushl %ecx

	pushl %edx

	pushl %esi

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	imul $4, %ebx, %eax

	addl %eax,%esi

	movl (%esi), %ebx

	movl 16(%ebp), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl %ebx

	pushl 8(%ebp)

	call merge_L47

	addl $12, %esp

	popl %edx

	popl %ecx

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl %edi

	pushl $2

	call _allocRecord

	addl $12, %esp

	popl %edx

	popl %ecx

	jmp L63

L116:

	popl %esi

	popl %edi

	popl %ebx

	leave

	ret

f_L76:

	enter $0,$0x0

	pushl %ebx

	pushl %edi

	pushl %esi

	movl 12(%ebp), %eax

	cmpl $0, %eax

	jg L87

L88:

	jmp L117

L87:

	movl 12(%ebp), %eax

	movl $10, %ebx

	xorl %edx, %edx

	idivl %ebx

	movl %eax,%ebx

	pushl %ecx

	pushl %edx

	pushl %ebx

	pushl 8(%ebp)

	call f_L76

	addl $8, %esp

	popl %edx

	popl %ecx

	movl 12(%ebp), %ebx

	movl 12(%ebp), %eax

	movl $10, %edi

	xorl %edx, %edx

	idivl %edi

	movl %eax,%edi

	imul $10, %edi, %eax

	subl %eax,%ebx

	pushl %ecx

	pushl %edx

	pushl $L86

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

	jmp L88

L117:

	popl %esi

	popl %edi

	popl %ebx

	leave

	ret

printint_L46:

	enter $0,$0x0

	pushl %ebx

	pushl %edi

	pushl %esi

	movl 12(%ebp), %eax

	cmpl $0, %eax

	jl L100

L101:

	movl 12(%ebp), %eax

	cmpl $0, %eax

	jg L97

L98:

	pushl %ecx

	pushl %edx

	pushl $L96

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

L99:

L102:

	jmp L118

L100:

	pushl %ecx

	pushl %edx

	pushl $L92

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

	call f_L76

	addl $8, %esp

	popl %edx

	popl %ecx

	jmp L102

L97:

	movl 12(%ebp), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl %ebp

	call f_L76

	addl $8, %esp

	popl %edx

	popl %ecx

	jmp L99

L118:

	popl %esi

	popl %edi

	popl %ebx

	leave

	ret

printlist_L45:

	enter $0,$0x0

	pushl %ebx

	pushl %edi

	pushl %esi

	movl 12(%ebp), %eax

	cmpl $0, %eax

	je L109

L110:

	movl 12(%ebp), %ebx

	movl $0, %edi 

	pushl %ecx

	pushl %edx

	pushl %ebx

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	imul $4, %edi, %eax

	addl %eax,%ebx

	movl (%ebx), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl 8(%ebp)

	call printint_L46

	addl $8, %esp

	popl %edx

	popl %ecx

	pushl %ecx

	pushl %edx

	pushl $L108

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	movl 12(%ebp), %ebx

	movl $1, %edi 

	pushl %ecx

	pushl %edx

	pushl %ebx

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	imul $4, %edi, %eax

	addl %eax,%ebx

	movl (%ebx), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl 8(%ebp)

	call printlist_L45

	addl $8, %esp

	popl %edx

	popl %ecx

L111:

	jmp L119

L109:

	pushl %ecx

	pushl %edx

	pushl $L106

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	jmp L111

L119:

	popl %esi

	popl %edi

	popl %ebx

	leave

	ret

_tigermain:

	enter $4,$0x0

	pushl %ebx

	pushl %edi

	pushl %esi

	movl %ebp,%ebx

	addl $-4, %ebx

	pushl %ecx

	pushl %edx

	call getstr

	popl %edx

	popl %ecx

	movl %eax, (%ebx)

	pushl %ecx

	pushl %edx

	pushl %ebp

	call readlist_L48

	addl $4, %esp

	popl %edx

	popl %ecx

	movl %eax,%ebx

	movl %ebp,%edi

	addl $-4, %edi

	pushl %ecx

	pushl %edx

	call getstr

	popl %edx

	popl %ecx

	movl %eax, (%edi)

	pushl %ecx

	pushl %edx

	pushl %ebp

	call readlist_L48

	addl $4, %esp

	popl %edx

	popl %ecx

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl %ebx

	pushl %ebp

	call merge_L47

	addl $12, %esp

	popl %edx

	popl %ecx

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl %ebp

	call printlist_L45

	addl $8, %esp

	popl %edx

	popl %ecx

	movl $0, %eax 

	jmp L120

L120:

	popl %esi

	popl %edi

	popl %ebx

	leave

	ret

