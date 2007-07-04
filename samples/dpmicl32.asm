;	DPMICL32.ASM NASM Sample for a 32-bit DPMI client.

;	To assemble, use:
;		nasm dpmicl32.asm -O 2 -f obj -o dpmicl32.obj
;
;	To link, use:
;		valx /32 dpmicl32.obj;

LF	equ	10
CR	equ	13

	cpu	386

	segment _text use16 public class=CODE
    
..start:
	mov ax, dgroup
    mov ds, ax
    mov ax, ss
    mov cx, es
    sub ax, cx
    mov bx, sp
    shr bx, 4
    add bx, ax		;release unused DOS memory
	mov ah, 4Ah
    int 21h
    mov ax, 1687h	;DPMI host installed?
    int 2Fh
    and ax, ax
    jnz nohost
    push es			;save DPMI entry address
    push di
    and si, si		;requires host client-specific DOS memory?
    jz nomemneeded
    mov bx, si
    mov ah, 48h		;alloc DOS memory
    int 21h
    jc nomem
    mov es, ax
nomemneeded:
	mov bp, sp
    mov ax, 0001	;start a 32-bit client
    call far [bp]	;initial switch to protected-mode
    jc initfailed

	mov cx,1		;get a descriptor for the 32-bit code segment
    mov ax,0
    int 31h
    jc dpmierr
    mov bx,ax
    mov dx,_text32
    mov cx,dx
    shl dx,4
    shr cx,12
    mov ax,7		;set base
    int 31h
    or dx,-1
    xor cx,cx
    mov ax,8		;set limit
    int 31h
    mov cx,cs
    lar cx,cx
    shr cx,8
    or  ch,40h		;make a 32bit CS
    mov ax,9
    int 31h
    push bx
    push start32
    retf			;jump to 32bit CS
nohost:
	mov dx, dErr1
error:    
    mov ah, 9
    int 21h
    mov ax, 4C00h
    int 21h
nomem:
	mov dx, dErr2
    jmp error
initfailed:
	mov dx, dErr3
    jmp error
dpmierr:
	mov dx, dErr4
    jmp error

;--- the 32-bit code segment

	segment _text32 use32 public align=16 class=CODE

start32:
    mov esi, szWelcome
    call printstring
    mov ax, 4C00h	;normal client exit
    int 21h

;--- print a string in protected-mode with simple
;--- DOS commands not using pointers.

printstring:    
    lodsb
    and al,al
    jz stringdone
    mov dl,al
    mov ah,2
    int 21h
    jmp printstring
stringdone:
	ret

	segment _data use16 public align=16 class=DATA

szWelcome db "welcome in protected-mode",CR,LF,0
dErr1 db "no DPMI host installed",CR,LF,'$'
dErr2 db "not enough DOS memory for client initialisation",CR,LF,'$'
dErr3 db "DPMI initialisation failed",CR,LF,'$'
dErr4 db "no LDT descriptors available",CR,LF,'$'

	segment stack use16 stack align=16 class=STACK
	resb 1024

group dgroup _data stack
    

