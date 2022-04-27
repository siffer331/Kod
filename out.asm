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



_start:  push 9
  push dword [esp+0]
  push 1
  mov eax,[esp]
  add esp,4
  sub [esp],eax
  mov eax,[esp]
  add esp,4
  mov [esp+0],eax
  push 1
  push 1
 .begin2:
  push dword [esp+8]
  push 0
  pop eax
  cmp [esp],eax
  jg .test1
  mov [esp],dword 0
  jmp .end1
.test1:
  mov [esp],dword 1
.end1:
  cmp [esp],dword 0
  pop eax
  je .mens2
  push dword [esp+4]
  push dword [esp+4]
  mov eax,[esp]
  add esp,4
  add [esp],eax
  push dword [esp+4]
  mov eax,[esp]
  add esp,4
  mov [esp+8],eax
  push dword [esp+0]
  mov eax,[esp]
  add esp,4
  mov [esp+4],eax
  push dword [esp+12]
  push 1
  mov eax,[esp]
  add esp,4
  sub [esp],eax
  mov eax,[esp]
  add esp,4
  mov [esp+12],eax
  add esp,4
  jmp .begin2
.mens2:
  push 0
  push dword [esp+4]
  call .print
  add esp,8
  mov eax, 1
  mov ebx, 0
  int 0x80
