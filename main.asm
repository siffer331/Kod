
section .data
	.buf db '0000000000',10,0

section .text
	global _start

.print:
	mov eax,[esp]
	sub esp, 16             ; reserve space on the stack
	mov ecx, 10
	mov ebx, 16
	.L1:
	xor edx, edx            ; Don't forget it!
	div ecx                 ; Extract the last decimal digit
	or dl, 0x30             ; Convert remainder to ASCII
	sub ebx, 1
	mov [esp+ebx], dl       ; Store remainder on the stack (reverse order)
	test eax, eax           ; Until there is nothing left to divide
	jnz .L1

	mov eax, 4              ; SYS_WRITE
	lea ecx, [esp+ebx]      ; Pointer to the first ASCII digit
	mov edx, 16
	sub edx, ebx            ; Count of digits
	mov ebx, 1              ; STDOUT
	int 0x80                ; Call 32-bit Linux

	add esp, 16             ; Restore the stack
;	sub esp,16
;	mov ecx,10
;	mov ebx,16
;.loop:
;	div ecx
;	or dl,0x30
;	dec ebx
;	mov [esp+ebx], dl
;	test eax,eax
;	jnz .loop
;	xor edx,edx
;	mov eax,4
;	lea ecx,[esp+ebx]
;	mov edx,16
;	sub edx,ebx
;	mov ebx,1
;	int 0x80
;	add esp,16
	ret

_start:
;	push 4
;	call .print
;	push 5
;	mov eax,[esp]
;	cmp [esp+4],eax
;	call .print
;	push dword [esp+4]
;	call .print
;	mov eax,[esp]
;	add esp,4
;	add [esp], eax
;	call .print
;	add esp,4
;	call .print
	push dword 2
	call .print
;	mov [esp],dword 1
;	cmp [esp],dword 0
;	je .if
;	pop eax
;	push 2
;	jmp .end2
;.if:
;	push 3
;.end2:
;	call .print

	mov eax,1
	int 0x80

