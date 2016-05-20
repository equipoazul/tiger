.data
L3:
	.long 2
	.string " O"

L4:
	.long 2
	.string " ."

L13:
	.long 2
	.string " O"

L14:
	.long 2
	.string " ."

.text
	.globl _tigermain
_tigermain:

	enter $0,$0x0

	movl $8, %eax 
	movl $1, %ebx 
    imul $4, %ebx, %eax

	leave

	ret

