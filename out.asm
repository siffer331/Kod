section .data
  .buf db '0000000000',10,0

section .text
  global _start

.print:
  mov eax,[esp+4]
  mov edi,10
  mov ecx,10
  mov ebx,.buf+9
.ploop:
  mov edx,0
  idiv edi
  add edx,48
  mov [ebx],dl
  dec ebx
  loop .ploop
  mov eax,4
  mov ebx,1
  mov ecx,.buf
  mov edx,11
  int 0x80
  ret



_start:  push 0
  push 2
  call .print
  add esp,4
  push 0
  push dword [esp+4]
  call .print
  add esp,8
  mov eax, 1
  mov ebx, 0
  int 0x80
