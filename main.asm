
section .data
	.buf db '0000000000',10,0

section .text
	global _start

.print:
	mov eax,[esp+4]
	mov edi,10
	mov ecx,10
	mov ebx,.buf+9
.pl_oop:
	mov edx,0
	div edi
	add edx,48
	mov [ebx],dl
	dec ebx
	loop .pl_oop
	mov eax,4
	mov ebx,1
	mov ecx,.buf
	mov edx,11
	int 0x80
	ret

_start:
	push 4
	call .print
	push 5
	call .print
	push dword [esp+4]
	call .print
	mov eax,[esp]
	add esp,4
	add [esp], eax
	call .print
	add esp,4
	call .print
	mov eax,2
	mov ebx,3
	imul [esp]
	mov [esp],eax
	call .print

	mov eax,1
	int 0x80

