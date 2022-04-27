nasm -f elf out.asm
ld -m elf_i386 -s -o out out.o
