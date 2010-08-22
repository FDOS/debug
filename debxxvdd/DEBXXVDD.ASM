
;--- DEBXXVDD is a NTVDM VDD which
;--- supplies some missing features to DOS applications.

        .386
        .MODEL FLAT, stdcall
        option casemap:none

        include winbase.inc
        include vddsvc.inc

;--- CStr() define a string in .CONST

CStr macro string:req
local sym,xxx
	.const
sym db string,0
     .code
	exitm <offset sym>
	endm

@DbgOutC	macro xx
ifdef _DEBUG
		invoke OutputDebugString, CStr(<xx>)
endif
		endm

@DbgOut	macro xx:REQ, parms:VARARG
ifdef _DEBUG
		pushad
		invoke wsprintf, addr szText, CStr(<xx>), parms
		invoke OutputDebugString, addr szText
        popad
endif
		endm

if 0
EXCEPTION_REGISTRATION struct
pPrev				dd ?
ExceptionHandler	dd ?
EXCEPTION_REGISTRATION ends

_XCPT_CONTINUE_EXECUTION	equ 000000000h	;=ExceptionContinueExecution
_XCPT_CONTINUE_SEARCH		equ 000000001h	;=ExceptionContinueSearch
endif

        .data

hVDD    DWORD 0
;dwOldExcFilter DWORD -1

		.data?

szText	db 128 dup (?)

        .code

if 0
except_handler proc c uses ebx esi edi pReport:ptr EXCEPTION_RECORD,\
                            pFrame:ptr FRAME,\
                            pContext:ptr CONTEXT,\
                            pContDisp:ptr CONTEXT

    	@DbgOutC <"except_handler called",13,10>
    	mov eax,_XCPT_CONTINUE_SEARCH
    	ret
except_handler endp

MyExcFilter proc stdcall pExceptionInfo:ptr EXCEPTION_POINTERS
    	@DbgOutC <"Unhandled Exception Filter called",13,10>
		mov eax, EXCEPTION_CONTINUE_SEARCH
		ret	
MyExcFilter endp
endif

ifndef __POASM__
        assume fs:nothing
endif

;--- Init

Init    proc    stdcall

    	@DbgOutC <"DEBXXVDD.Init enter",13,10>

if 0
		.if (dwOldExcFilter == -1)
			invoke SetUnhandledExceptionFilter, offset MyExcFilter
    	    mov dwOldExcFilter, eax
        .endif
        
       	@DbgOut <"UnHandledExceptionFilter(%X)=%X",13,10>, offset MyExcFilter, eax

		mov eax, fs:[0]
        .while (eax != -1)
        	push eax
	        mov ecx, [eax+4]
        	@DbgOut <"exception handler %X %X",13,10>, eax, ecx
            pop eax
;            mov [eax+4],offset except_handler
            mov eax,[eax]
        .endw
endif        
       	@DbgOutC <"DEBXXVDD.Init exit",13,10>
        ret
        align 4
Init    endp

;--- DL=drive
;--- SI=read/write mode flags (0=read,1=write)
;--- DS:BX=disk packet

DPACKET struct
secsta	dd ?
seccnt	dw ?
bufofs	dw ?
bufseg	dw ?
DPACKET ends

DPACKET32 struct
secsta	dd ?
seccnt	dw ?
bufofs	dd ?
bufseg	dw ?
DPACKET32 ends

;--- b32Bit: 0=16bit, 1=32bit

accessdrive proc b32Bit:dword

local	drive:dword
local	bWrite:dword
local	dwMSW:dword
local	hDisk:dword
local	dwRead:dword
local	dwBuffer:dword
local	szDrv[8]:byte

		invoke getEDX
        mov drive, eax
        invoke getESI
        and eax,1
        mov bWrite, eax	;0=read,1=write
		invoke getMSW
        and eax,1
        mov dwMSW,eax	;0=realmode, 1=pmode
        invoke getEBX
        mov edi,eax
        cmp dwMSW,0
        jz @F
        cmp b32Bit,0
        jnz use32bitofs
@@:     
        movzx edi,di	;16bit offset!
use32bitofs:
        invoke getDS
        invoke VdmMapFlat, eax, edi, dwMSW
        mov ebx, eax

        movzx ecx,[ebx].DPACKET.bufofs
        movzx eax,[ebx].DPACKET.bufseg
        cmp dwMSW, 0	;real-mode
        jz bufferok
		cmp b32Bit, 0	;16bit pmode?
        jz bufferok
        mov ecx,[ebx].DPACKET32.bufofs
        movzx eax,[ebx].DPACKET32.bufseg
bufferok:
        invoke VdmMapFlat, eax, ecx, dwMSW
        mov dwBuffer, eax

    	@DbgOut <"DEBXXVDD.accessdrive, buffer=%X, packet=%X",13,10>, eax, ebx
        
        mov al,byte ptr [drive]
        add al,'A'
        mov ah,':'
        movzx eax,ax
        mov dword ptr szDrv+0,"\.\\"
        mov dword ptr szDrv+4,eax
        mov ecx, GENERIC_READ
        .if (bWrite)
        	mov ecx, GENERIC_WRITE
        .endif
        invoke CreateFileA, addr szDrv, ecx, FILE_SHARE_READ or FILE_SHARE_WRITE,\
        	0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0
        cmp eax,-1
        jz openerror
        mov hDisk, eax
		mov eax, [ebx].DPACKET.secsta
        mov edx,512
        mul edx
        push edx
        mov edx, esp
        invoke SetFilePointer, hDisk, eax, edx, 0
        pop edx
        movzx ecx,[ebx].DPACKET.seccnt
        shl ecx,9
        .if (bWrite)
        	invoke WriteFile, hDisk, dwBuffer, ecx, addr dwRead, 0
        .else
        	invoke ReadFile, hDisk, dwBuffer, ecx, addr dwRead, 0
        .endif
        push eax
		invoke CloseHandle, hDisk
        pop eax
        and eax, eax
        jz accerror
       	invoke setCF, 0
		ret
accerror:
openerror:
		invoke setAX, 0Ch	;general failure
		invoke setCF, 1
		ret
accessdrive endp

;--- Dispatch
;--- function in cx
;--- cx=01: test if address at DX:EBX is readable
;--- cx=02: test if address at DX:EBX is writeable
;--- cx=05: direct disk access, 16bit pointers
;--- cx=06: direct disk access, 32bit pointers

Dispatch    proc    stdcall uses ebx edi

local	mbi:MEMORY_BASIC_INFORMATION

    	@DbgOutC <"DEBXXVDD.Dispatch enter",13,10>
        invoke getECX
       	movzx eax,ax
        .if ((eax == 1) || (eax == 2))
        	mov edi, eax
	        invoke getEBX
	        mov ebx, eax
	        invoke getEDX
			invoke VdmMapFlat, eax, eax, VDM_PM
            invoke VirtualQuery, ebx, addr mbi, sizeof MEMORY_BASIC_INFORMATION
            and eax, eax
            jz error
            cmp mbi.State, MEM_COMMIT
            jnz error
	        @DbgOut <"address %X is committed",13,10>, ebx
            .if (di == 2)
	            test mbi.Protect, PAGE_READWRITE or PAGE_EXECUTE_READWRITE
    	        jz error
		        @DbgOut <"address %X is writeable",13,10>, ebx
            .endif
            invoke setCF, 0
            jmp exit
        .elseif (eax == 3)
	        @DbgOutC <"Sleep called",13,10>
        	invoke Sleep, 0
            invoke setCF, 0
            jmp exit
        .elseif ((eax == 5) || (eax == 6))
        	sub eax,5
			invoke accessdrive, eax
            jmp exit
        .else
        	movzx eax, ax
	        @DbgOut <"unknown function CX=%X",13,10>, eax
        .endif
error:
        invoke setCF,1
exit:        
    	@DbgOutC <"DEBXXVDD.Dispatch exit",13,10>
        ret
        align 4
Dispatch    endp

;*** main proc ***

DllMain proc stdcall hInstance:dword, reason:dword, lpReserved:dword

        mov eax, reason
        .if (eax == DLL_PROCESS_ATTACH)
            mov eax, hInstance
            mov hVDD, eax
            @DbgOutC <"debxxvdd process attach",13,10>
	        mov eax,1
        .elseif (eax == DLL_PROCESS_DETACH)
            @DbgOutC <"debxxvdd process detach",13,10>
        .elseif (eax == DLL_THREAD_ATTACH)
            @DbgOutC <"debxxvdd thread attach",13,10>
        .elseif (eax == DLL_THREAD_DETACH)
            @DbgOutC <"debxxvdd thread detach",13,10>
        .endif
        ret
DllMain endp

        END DllMain

