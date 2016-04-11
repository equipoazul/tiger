	.file	"mul.c"
	.text
	.globl	main
	.type	main, @function
main:
.LFB0:
	.cfi_startproc
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset 5, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register 5
	subl	$16, %esp
	movl	$5, -8(%ebp)
	movl	$10, -4(%ebp)
	movl	-8(%ebp), %eax
	imull	-4(%ebp), %eax
	movl	%eax, -12(%ebp)
	cmpl	$20, -12(%ebp)
	jle	.L2
	movl	$20, -12(%ebp)
.L2:
	movl	-12(%ebp), %eax
	leave
	.cfi_restore 5
	.cfi_def_cfa 4, 4
	ret
	.cfi_endproc
.LFE0:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 4.8.4-2ubuntu1~14.04.1) 4.8.4"
	.section	.note.GNU-stack,"",@progbits
