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

L35:
	.long 1
	.string "0"

L36:
	.long 1
	.string "0"

L73:
	.long 1
	.string "0"

L74:
	.long 1
	.string "0"

L75:
	.long 1
	.string "0"

L76:
	.long 1
	.string "0"

L77:
	.long 1
	.string "0"

L78:
	.long 1
	.string "0"

L79:
	.long 1
	.string "0"

L80:
	.long 1
	.string "0"

L85:
	.long 1
	.string "-"

L86:
	.long 1
	.string "-"

L89:
	.long 1
	.string "0"

L90:
	.long 1
	.string "0"

L99:
	.long 4
	.string "\x0a"

L100:
	.long 4
	.string "\x0a"

L101:
	.long 1
	.string " "

L102:
	.long 1
	.string " "

.text
	.globl _tigermain
isdigit_L2:

	enter $4,$0x0

	movl %ebx, -4(%ebp)

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

	cmpl %eax,%ebx

	jle L14

L15:

	movl $0, %eax 

L16:

	movl -4(%ebp), %ebx

	jmp L106

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

	cmpl %eax,%ebx

	jle L17

L18:

	movl $0, %eax 

L19:

	jmp L16

L17:

	movl $1, %eax 

	jmp L19

L106:

	leave

	ret

skipto_L1:

	enter $4,$0x0

	movl %ebx, -4(%ebp)

L32:

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

	movl $0, %ebx

	cmpl %ebx, %eax

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

	movl $0, %ebx

	cmpl %ebx, %eax

	je L28

L29:

	movl $0, %eax 

L30:

L27:

	movl $0, %ebx

	cmpl %ebx, %eax

	jne L33

L31:

	movl -4(%ebp), %ebx

	jmp L107

L25:

	movl $1, %eax 

	jmp L27

L28:

	movl $1, %eax 

	jmp L30

L33:

	movl 8(%ebp), %eax

	movl 8(%eax), %ebx

	addl $-4, %ebx

	pushl %ecx

	pushl %edx

	call getstr

	popl %edx

	popl %ecx

	movl %eax, (%ebx)

	jmp L32

L107:

	leave

	ret

readint_L0:

	enter $20,$0x0

	movl %ebx,%eax

	movl %eax, -12(%ebp)

	movl %edi,%eax

	movl %eax, -16(%ebp)

	movl %esi,%eax

	movl %eax, -20(%ebp)

	movl $0, %eax 

	movl %eax, -4(%ebp)

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

	imul $4, %esi, %ebx

	addl %ebx,%edi

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

L37:

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

	movl $0, %ebx

	cmpl %ebx, %eax

	jne L38

L34:

	movl -4(%ebp), %eax

	movl -20(%ebp), %esi

	movl -16(%ebp), %edi

	movl -12(%ebp), %ebx

	jmp L108

L38:

	movl -4(%ebp), %ebx

	movl -8(%ebp), %eax

	imul $10, %ebx, %eax

	movl %eax, -8(%ebp)

	movl -8(%ebp), %ebx

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

	pushl $L36

	call ord

	addl $4, %esp

	popl %edx

	popl %ecx

	subl %eax,%ebx

	movl %ebx, -4(%ebp)

	movl 8(%ebp), %ebx

	addl $-4, %ebx

	pushl %ecx

	pushl %edx

	call getstr

	popl %edx

	popl %ecx

	movl %eax, (%ebx)

	jmp L37

L108:

	leave

	ret

readlist_L42:

	enter $24,$0x0

	movl %ebx,%eax

	movl %eax, -16(%ebp)

	movl %edi,%eax

	movl %eax, -20(%ebp)

	movl %esi,%eax

	movl %eax, -24(%ebp)

	pushl %ecx

	pushl %edx

	pushl $0

	pushl $1

	call _allocRecord

	addl $8, %esp

	popl %edx

	popl %ecx

	movl %eax, %edi

	pushl %ecx

	pushl %edx

	pushl %edi

	pushl 8(%ebp)

	call readint_L0

	addl $8, %esp

	popl %edx

	popl %ecx

	movl %eax, -12(%ebp)

	movl $0, %esi 

	pushl %ecx

	pushl %edx

	pushl %edi

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	movl $0, %eax

	movl %eax, -4(%ebp)

	movl %edi,%eax

	imul $4, %esi, %ebx

	addl %ebx,%eax

	movl (%eax), %eax

	movl -8(%ebp), %ebx

	imul $4, %esi, %ebx

	movl %ebx, -8(%ebp)

	movl -8(%ebp), %ebx

	addl %ebx,%edi

	movl (%edi), %edi

	movl -4(%ebp), %ebx

	cmpl %ebx, %eax

	movl %ebx, -4(%ebp)

	jne L43

L44:

	movl $0, %eax 

L45:

	movl -24(%ebp), %esi

	movl -20(%ebp), %edi

	movl -16(%ebp), %ebx

	jmp L109

L43:

	movl -12(%ebp), %ebx

	pushl %ecx

	pushl %edx

	pushl 8(%ebp)

	call readlist_L42

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

	jmp L45

L109:

	leave

	ret

merge_L41:

	enter $848,$0x0

	movl %ebx,%eax

	movl %eax, -28(%ebp)

	movl %edi,%eax

	movl %eax, -32(%ebp)

	movl %esi,%eax

	movl %eax, -36(%ebp)

	movl $0, %esi

	movl 12(%ebp), %edi

	movl 12(%ebp), %eax

	cmpl %esi, %edi

	je L67

L68:

	movl $0, %esi

	movl 16(%ebp), %edi

	movl 16(%ebp), %eax

	cmpl %esi, %edi

	je L61

L62:

	movl 12(%ebp), %esi

	movl $0, %edi 

	pushl %ecx

	pushl %edx

	pushl %esi

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	imul $4, %edi, %ebx

	addl %ebx,%esi

	movl (%esi), %eax

	movl %eax, -40(%ebp)

	movl 16(%ebp), %edi

	movl $0, %ebx 

	pushl %ecx

	pushl %edx

	pushl %edi

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	movl %edi,%eax

	movl -4(%ebp), %esi

	imul $4, %ebx, %esi

	movl %esi, -4(%ebp)

	movl -4(%ebp), %esi

	addl %esi,%eax

	movl (%eax), %eax

	movl -8(%ebp), %esi

	imul $4, %ebx, %esi

	movl %esi, -8(%ebp)

	movl -8(%ebp), %ebx

	addl %ebx,%edi

	movl (%edi), %edi

	movl -40(%ebp), %ebx

	cmpl %eax,%ebx

	movl %ebx, -40(%ebp)

	jl L55

L56:

	movl 16(%ebp), %ebx

	movl $0, %edi 

	pushl %ecx

	pushl %edx

	pushl %ebx

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	movl -12(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -12(%ebp)

	movl -12(%ebp), %eax

	addl %eax,%ebx

	movl (%ebx), %eax

	movl %eax, -44(%ebp)

	movl -44(%ebp), %eax

	movl %eax, -52(%ebp)

	movl -52(%ebp), %eax

	movl %eax, -64(%ebp)

	movl -64(%ebp), %eax

	movl %eax, -72(%ebp)

	movl -72(%ebp), %eax

	movl %eax, -80(%ebp)

	movl -80(%ebp), %eax

	movl %eax, -88(%ebp)

	movl -88(%ebp), %eax

	movl %eax, -96(%ebp)

	movl -96(%ebp), %eax

	movl %eax, -104(%ebp)

	movl -104(%ebp), %eax

	movl %eax, -112(%ebp)

	movl -112(%ebp), %eax

	movl %eax, -120(%ebp)

	movl -120(%ebp), %eax

	movl %eax, -128(%ebp)

	movl -128(%ebp), %eax

	movl %eax, -136(%ebp)

	movl -136(%ebp), %eax

	movl %eax, -144(%ebp)

	movl -144(%ebp), %eax

	movl %eax, -152(%ebp)

	movl -152(%ebp), %eax

	movl %eax, -160(%ebp)

	movl -160(%ebp), %eax

	movl %eax, -168(%ebp)

	movl -168(%ebp), %eax

	movl %eax, -176(%ebp)

	movl -176(%ebp), %eax

	movl %eax, -184(%ebp)

	movl -184(%ebp), %eax

	movl %eax, -192(%ebp)

	movl -192(%ebp), %eax

	movl %eax, -200(%ebp)

	movl -200(%ebp), %eax

	movl %eax, -208(%ebp)

	movl -208(%ebp), %eax

	movl %eax, -216(%ebp)

	movl -216(%ebp), %eax

	movl %eax, -224(%ebp)

	movl -224(%ebp), %eax

	movl %eax, -232(%ebp)

	movl -232(%ebp), %eax

	movl %eax, -240(%ebp)

	movl -240(%ebp), %eax

	movl %eax, -248(%ebp)

	movl -248(%ebp), %eax

	movl %eax, -256(%ebp)

	movl -256(%ebp), %eax

	movl %eax, -264(%ebp)

	movl -264(%ebp), %eax

	movl %eax, -272(%ebp)

	movl -272(%ebp), %eax

	movl %eax, -280(%ebp)

	movl -280(%ebp), %eax

	movl %eax, -288(%ebp)

	movl -288(%ebp), %eax

	movl %eax, -296(%ebp)

	movl -296(%ebp), %eax

	movl %eax, -304(%ebp)

	movl -304(%ebp), %eax

	movl %eax, -312(%ebp)

	movl -312(%ebp), %eax

	movl %eax, -320(%ebp)

	movl -320(%ebp), %eax

	movl %eax, -328(%ebp)

	movl -328(%ebp), %eax

	movl %eax, -336(%ebp)

	movl -336(%ebp), %eax

	movl %eax, -344(%ebp)

	movl -344(%ebp), %eax

	movl %eax, -352(%ebp)

	movl -352(%ebp), %eax

	movl %eax, -360(%ebp)

	movl -360(%ebp), %eax

	movl %eax, -368(%ebp)

	movl -368(%ebp), %eax

	movl %eax, -376(%ebp)

	movl -376(%ebp), %eax

	movl %eax, -384(%ebp)

	movl -384(%ebp), %eax

	movl %eax, -392(%ebp)

	movl -392(%ebp), %eax

	movl %eax, -400(%ebp)

	movl -400(%ebp), %eax

	movl %eax, -408(%ebp)

	movl -408(%ebp), %eax

	movl %eax, -416(%ebp)

	movl -416(%ebp), %eax

	movl %eax, -424(%ebp)

	movl -424(%ebp), %eax

	movl %eax, -432(%ebp)

	movl -432(%ebp), %eax

	movl %eax, -440(%ebp)

	movl -440(%ebp), %eax

	movl %eax, -448(%ebp)

	movl -448(%ebp), %eax

	movl %eax, -456(%ebp)

	movl -456(%ebp), %eax

	movl %eax, -464(%ebp)

	movl -464(%ebp), %eax

	movl %eax, -472(%ebp)

	movl -472(%ebp), %eax

	movl %eax, -480(%ebp)

	movl -480(%ebp), %eax

	movl %eax, -488(%ebp)

	movl -488(%ebp), %eax

	movl %eax, -496(%ebp)

	movl -496(%ebp), %eax

	movl %eax, -504(%ebp)

	movl -504(%ebp), %eax

	movl %eax, -512(%ebp)

	movl -512(%ebp), %eax

	movl %eax, -520(%ebp)

	movl -520(%ebp), %eax

	movl %eax, -528(%ebp)

	movl -528(%ebp), %eax

	movl %eax, -536(%ebp)

	movl -536(%ebp), %eax

	movl %eax, -544(%ebp)

	movl -544(%ebp), %eax

	movl %eax, -552(%ebp)

	movl -552(%ebp), %eax

	movl %eax, -560(%ebp)

	movl -560(%ebp), %eax

	movl %eax, -568(%ebp)

	movl -568(%ebp), %eax

	movl %eax, -576(%ebp)

	movl -576(%ebp), %eax

	movl %eax, -584(%ebp)

	movl -584(%ebp), %eax

	movl %eax, -592(%ebp)

	movl -592(%ebp), %eax

	movl %eax, -600(%ebp)

	movl -600(%ebp), %eax

	movl %eax, -608(%ebp)

	movl -608(%ebp), %eax

	movl %eax, -616(%ebp)

	movl -616(%ebp), %eax

	movl %eax, -624(%ebp)

	movl -624(%ebp), %eax

	movl %eax, -632(%ebp)

	movl -632(%ebp), %eax

	movl %eax, -640(%ebp)

	movl -640(%ebp), %eax

	movl %eax, -648(%ebp)

	movl -648(%ebp), %eax

	movl %eax, -656(%ebp)

	movl -656(%ebp), %eax

	movl %eax, -664(%ebp)

	movl -664(%ebp), %eax

	movl %eax, -672(%ebp)

	movl -672(%ebp), %eax

	movl %eax, -680(%ebp)

	movl -680(%ebp), %eax

	movl %eax, -688(%ebp)

	movl -688(%ebp), %eax

	movl %eax, -696(%ebp)

	movl -696(%ebp), %eax

	movl %eax, -704(%ebp)

	movl -704(%ebp), %eax

	movl %eax, -712(%ebp)

	movl -712(%ebp), %eax

	movl %eax, -720(%ebp)

	movl -720(%ebp), %eax

	movl %eax, -728(%ebp)

	movl -728(%ebp), %eax

	movl %eax, -736(%ebp)

	movl -736(%ebp), %eax

	movl %eax, -744(%ebp)

	movl -744(%ebp), %eax

	movl %eax, -752(%ebp)

	movl -752(%ebp), %eax

	movl %eax, -760(%ebp)

	movl -760(%ebp), %eax

	movl %eax, -768(%ebp)

	movl -768(%ebp), %eax

	movl %eax, -776(%ebp)

	movl -776(%ebp), %eax

	movl %eax, -784(%ebp)

	movl -784(%ebp), %eax

	movl %eax, -792(%ebp)

	movl -792(%ebp), %eax

	movl %eax, -800(%ebp)

	movl -800(%ebp), %eax

	movl %eax, -808(%ebp)

	movl -808(%ebp), %eax

	movl %eax, -824(%ebp)

	movl -824(%ebp), %eax

	movl %eax, -836(%ebp)

	movl -836(%ebp), %eax

	movl %eax, -848(%ebp)

	movl 12(%ebp), %eax

	movl %eax, -48(%ebp)

	movl -48(%ebp), %eax

	movl %eax, -60(%ebp)

	movl -60(%ebp), %eax

	movl %eax, -68(%ebp)

	movl -68(%ebp), %eax

	movl %eax, -76(%ebp)

	movl -76(%ebp), %eax

	movl %eax, -84(%ebp)

	movl -84(%ebp), %eax

	movl %eax, -92(%ebp)

	movl -92(%ebp), %eax

	movl %eax, -100(%ebp)

	movl -100(%ebp), %eax

	movl %eax, -108(%ebp)

	movl -108(%ebp), %eax

	movl %eax, -116(%ebp)

	movl -116(%ebp), %eax

	movl %eax, -124(%ebp)

	movl -124(%ebp), %eax

	movl %eax, -132(%ebp)

	movl -132(%ebp), %eax

	movl %eax, -140(%ebp)

	movl -140(%ebp), %eax

	movl %eax, -148(%ebp)

	movl -148(%ebp), %eax

	movl %eax, -156(%ebp)

	movl -156(%ebp), %eax

	movl %eax, -164(%ebp)

	movl -164(%ebp), %eax

	movl %eax, -172(%ebp)

	movl -172(%ebp), %eax

	movl %eax, -180(%ebp)

	movl -180(%ebp), %eax

	movl %eax, -188(%ebp)

	movl -188(%ebp), %eax

	movl %eax, -196(%ebp)

	movl -196(%ebp), %eax

	movl %eax, -204(%ebp)

	movl -204(%ebp), %eax

	movl %eax, -212(%ebp)

	movl -212(%ebp), %eax

	movl %eax, -220(%ebp)

	movl -220(%ebp), %eax

	movl %eax, -228(%ebp)

	movl -228(%ebp), %eax

	movl %eax, -236(%ebp)

	movl -236(%ebp), %eax

	movl %eax, -244(%ebp)

	movl -244(%ebp), %eax

	movl %eax, -252(%ebp)

	movl -252(%ebp), %eax

	movl %eax, -260(%ebp)

	movl -260(%ebp), %eax

	movl %eax, -268(%ebp)

	movl -268(%ebp), %eax

	movl %eax, -276(%ebp)

	movl -276(%ebp), %eax

	movl %eax, -284(%ebp)

	movl -284(%ebp), %eax

	movl %eax, -292(%ebp)

	movl -292(%ebp), %eax

	movl %eax, -300(%ebp)

	movl -300(%ebp), %eax

	movl %eax, -308(%ebp)

	movl -308(%ebp), %eax

	movl %eax, -316(%ebp)

	movl -316(%ebp), %eax

	movl %eax, -324(%ebp)

	movl -324(%ebp), %eax

	movl %eax, -332(%ebp)

	movl -332(%ebp), %eax

	movl %eax, -340(%ebp)

	movl -340(%ebp), %eax

	movl %eax, -348(%ebp)

	movl -348(%ebp), %eax

	movl %eax, -356(%ebp)

	movl -356(%ebp), %eax

	movl %eax, -364(%ebp)

	movl -364(%ebp), %eax

	movl %eax, -372(%ebp)

	movl -372(%ebp), %eax

	movl %eax, -380(%ebp)

	movl -380(%ebp), %eax

	movl %eax, -388(%ebp)

	movl -388(%ebp), %eax

	movl %eax, -396(%ebp)

	movl -396(%ebp), %eax

	movl %eax, -404(%ebp)

	movl -404(%ebp), %eax

	movl %eax, -412(%ebp)

	movl -412(%ebp), %eax

	movl %eax, -420(%ebp)

	movl -420(%ebp), %eax

	movl %eax, -428(%ebp)

	movl -428(%ebp), %eax

	movl %eax, -436(%ebp)

	movl -436(%ebp), %eax

	movl %eax, -444(%ebp)

	movl -444(%ebp), %eax

	movl %eax, -452(%ebp)

	movl -452(%ebp), %eax

	movl %eax, -460(%ebp)

	movl -460(%ebp), %eax

	movl %eax, -468(%ebp)

	movl -468(%ebp), %eax

	movl %eax, -476(%ebp)

	movl -476(%ebp), %eax

	movl %eax, -484(%ebp)

	movl -484(%ebp), %eax

	movl %eax, -492(%ebp)

	movl -492(%ebp), %eax

	movl %eax, -500(%ebp)

	movl -500(%ebp), %eax

	movl %eax, -508(%ebp)

	movl -508(%ebp), %eax

	movl %eax, -516(%ebp)

	movl -516(%ebp), %eax

	movl %eax, -524(%ebp)

	movl -524(%ebp), %eax

	movl %eax, -532(%ebp)

	movl -532(%ebp), %eax

	movl %eax, -540(%ebp)

	movl -540(%ebp), %eax

	movl %eax, -548(%ebp)

	movl -548(%ebp), %eax

	movl %eax, -556(%ebp)

	movl -556(%ebp), %eax

	movl %eax, -564(%ebp)

	movl -564(%ebp), %eax

	movl %eax, -572(%ebp)

	movl -572(%ebp), %eax

	movl %eax, -580(%ebp)

	movl -580(%ebp), %eax

	movl %eax, -588(%ebp)

	movl -588(%ebp), %eax

	movl %eax, -596(%ebp)

	movl -596(%ebp), %eax

	movl %eax, -604(%ebp)

	movl -604(%ebp), %eax

	movl %eax, -612(%ebp)

	movl -612(%ebp), %eax

	movl %eax, -620(%ebp)

	movl -620(%ebp), %eax

	movl %eax, -628(%ebp)

	movl -628(%ebp), %eax

	movl %eax, -636(%ebp)

	movl -636(%ebp), %eax

	movl %eax, -644(%ebp)

	movl -644(%ebp), %eax

	movl %eax, -652(%ebp)

	movl -652(%ebp), %eax

	movl %eax, -660(%ebp)

	movl -660(%ebp), %eax

	movl %eax, -668(%ebp)

	movl -668(%ebp), %eax

	movl %eax, -676(%ebp)

	movl -676(%ebp), %eax

	movl %eax, -684(%ebp)

	movl -684(%ebp), %eax

	movl %eax, -692(%ebp)

	movl -692(%ebp), %eax

	movl %eax, -700(%ebp)

	movl -700(%ebp), %eax

	movl %eax, -708(%ebp)

	movl -708(%ebp), %eax

	movl %eax, -716(%ebp)

	movl -716(%ebp), %eax

	movl %eax, -724(%ebp)

	movl -724(%ebp), %eax

	movl %eax, -732(%ebp)

	movl -732(%ebp), %eax

	movl %eax, -740(%ebp)

	movl -740(%ebp), %eax

	movl %eax, -748(%ebp)

	movl -748(%ebp), %eax

	movl %eax, -756(%ebp)

	movl -756(%ebp), %eax

	movl %eax, -764(%ebp)

	movl -764(%ebp), %eax

	movl %eax, -772(%ebp)

	movl -772(%ebp), %eax

	movl %eax, -780(%ebp)

	movl -780(%ebp), %eax

	movl %eax, -788(%ebp)

	movl -788(%ebp), %eax

	movl %eax, -796(%ebp)

	movl -796(%ebp), %eax

	movl %eax, -804(%ebp)

	movl -804(%ebp), %eax

	movl %eax, -820(%ebp)

	movl -820(%ebp), %eax

	movl %eax, -832(%ebp)

	movl -832(%ebp), %ebx

	movl 16(%ebp), %eax

	movl %eax, -812(%ebp)

	movl -812(%ebp), %eax

	movl %eax, -828(%ebp)

	movl -828(%ebp), %edi

	movl $1, %eax 

	movl %eax, -56(%ebp)

	pushl %ecx

	pushl %edx

	pushl %edi

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	movl -16(%ebp), %esi

	movl -56(%ebp), %eax

	movl %eax, -840(%ebp)

	movl -840(%ebp), %eax

	movl %eax, -816(%ebp)

	movl -816(%ebp), %eax

	movl %eax, -844(%ebp)

	movl -844(%ebp), %eax

	imul $4, %eax, %esi

	movl %esi, -16(%ebp)

	movl -16(%ebp), %eax

	addl %eax,%edi

	movl (%edi), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl %ebx

	pushl 8(%ebp)

	call merge_L41

	addl $12, %esp

	popl %edx

	popl %ecx

	pushl %ecx

	pushl %edx

	pushl %eax

	movl -848(%ebp), %eax

	pushl %eax

	pushl $2

	call _allocRecord

	addl $12, %esp

	popl %edx

	popl %ecx

L57:

L63:

L69:

	movl -36(%ebp), %esi

	movl -32(%ebp), %edi

	movl -28(%ebp), %ebx

	jmp L110

L67:

	movl 16(%ebp), %eax

	jmp L69

L61:

	movl 12(%ebp), %eax

	jmp L63

L55:

	movl 12(%ebp), %ebx

	movl $0, %edi 

	pushl %ecx

	pushl %edx

	pushl %ebx

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	movl -20(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -20(%ebp)

	movl -20(%ebp), %eax

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

	movl -24(%ebp), %eax

	imul $4, %ebx, %eax

	movl %eax, -24(%ebp)

	movl -24(%ebp), %eax

	addl %eax,%esi

	movl (%esi), %ebx

	movl 16(%ebp), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl %ebx

	pushl 8(%ebp)

	call merge_L41

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

	jmp L57

L110:

	leave

	ret

f_L70:

	enter $12,$0x0

	movl %ebx, -4(%ebp)

	movl %edi, -8(%ebp)

	movl $0, %edi

	movl 12(%ebp), %ebx

	movl 12(%ebp), %eax

	cmpl %edi, %ebx

	jg L81

L82:

	movl -8(%ebp), %edi

	movl -4(%ebp), %ebx

	jmp L111

L81:

	movl 12(%ebp), %eax

	movl $10, %ebx

	xorl %edx, %edx

	idivl %ebx

	movl %eax,%ebx

	pushl %ecx

	pushl %edx

	pushl %ebx

	pushl 8(%ebp)

	call f_L70

	addl $8, %esp

	popl %edx

	popl %ecx

	movl 12(%ebp), %ebx

	movl 12(%ebp), %eax

	movl $10, %edi

	xorl %edx, %edx

	idivl %edi

	movl %eax,%edi

	movl -12(%ebp), %eax

	imul $10, %edi, %eax

	movl %eax, -12(%ebp)

	movl -12(%ebp), %eax

	subl %eax,%ebx

	pushl %ecx

	pushl %edx

	pushl $L80

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

	jmp L82

L111:

	leave

	ret

printint_L40:

	enter $8,$0x0

	movl %ebx, -4(%ebp)

	movl %edi, -8(%ebp)

	movl $0, %edi

	movl 12(%ebp), %ebx

	movl 12(%ebp), %eax

	cmpl %edi, %ebx

	jl L94

L95:

	movl $0, %edi

	movl 12(%ebp), %ebx

	movl 12(%ebp), %eax

	cmpl %edi, %ebx

	jg L91

L92:

	pushl %ecx

	pushl %edx

	pushl $L90

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

L93:

L96:

	movl -8(%ebp), %edi

	movl -4(%ebp), %ebx

	jmp L112

L94:

	pushl %ecx

	pushl %edx

	pushl $L86

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

	call f_L70

	addl $8, %esp

	popl %edx

	popl %ecx

	jmp L96

L91:

	movl 12(%ebp), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl %ebp

	call f_L70

	addl $8, %esp

	popl %edx

	popl %ecx

	jmp L93

L112:

	leave

	ret

printlist_L39:

	enter $16,$0x0

	movl %ebx,%eax

	movl %eax, -4(%ebp)

	movl %edi,%eax

	movl %eax, -8(%ebp)

	movl %esi,%eax

	movl %eax, -12(%ebp)

	movl $0, %esi

	movl 12(%ebp), %edi

	movl 12(%ebp), %eax

	cmpl %esi, %edi

	je L103

L104:

	movl 12(%ebp), %edi

	movl $0, %esi 

	pushl %ecx

	pushl %edx

	pushl %edi

	call _checkNil

	addl $4, %esp

	popl %edx

	popl %ecx

	imul $4, %esi, %ebx

	addl %ebx,%edi

	movl (%edi), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl 8(%ebp)

	call printint_L40

	addl $8, %esp

	popl %edx

	popl %ecx

	pushl %ecx

	pushl %edx

	pushl $L102

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

	movl -16(%ebp), %eax

	imul $4, %edi, %eax

	movl %eax, -16(%ebp)

	movl -16(%ebp), %eax

	addl %eax,%ebx

	movl (%ebx), %eax

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl 8(%ebp)

	call printlist_L39

	addl $8, %esp

	popl %edx

	popl %ecx

L105:

	movl -12(%ebp), %esi

	movl -8(%ebp), %edi

	movl -4(%ebp), %ebx

	jmp L113

L103:

	pushl %ecx

	pushl %edx

	pushl $L100

	call print

	addl $4, %esp

	popl %edx

	popl %ecx

	jmp L105

L113:

	leave

	ret

_tigermain:

	enter $12,$0x0

	movl %ebx, -8(%ebp)

	movl %edi, -12(%ebp)

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

	call readlist_L42

	addl $4, %esp

	popl %edx

	popl %ecx

	movl %eax,%edi

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

	call readlist_L42

	addl $4, %esp

	popl %edx

	popl %ecx

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl %edi

	pushl %ebp

	call merge_L41

	addl $12, %esp

	popl %edx

	popl %ecx

	pushl %ecx

	pushl %edx

	pushl %eax

	pushl %ebp

	call printlist_L39

	addl $8, %esp

	popl %edx

	popl %ecx

	movl $0, %eax 

	movl -12(%ebp), %edi

	movl -8(%ebp), %ebx

	jmp L114

L114:

	leave

	ret

