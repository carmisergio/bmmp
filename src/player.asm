;-----------------------------------------------------------------------------------------------------------------------
; Bare Metal Music Player v0.1
; Plays back music files stored on floppy disk
; Author: Sergio Carmine 4CITI <mesergiocarmi.net>                                                       
; Date: 09/01/2023                                                                                                     
;-----------------------------------------------------------------------------------------------------------------------

;==================================
; Memory table (HEX)
; ---------------------------------
; 0500:0000 (0x5000) | Stack // Set up by bootloader
; 0500:4000 (0x9000) | (16 Kbytes)
; ---------------------------------
; 0800:XXXX (0x8000) |  This program
;==================================


; org 0x0800      ; Bootloader will have data segment set to where this code is loaded
bits 16         ; Generate 16 bit code

; Start kernel
jmp start

;------------------------------------------------------------------------------------------------------------
; Data section
;
    ; Messages
    msg_hello                   db "Hello, world!", 0x00
    msg_fail                    db "Fail!", 0x00
    msg_cylinders               db "Cylinders: ", 0x00
    msg_heads                   db "Heads: ", 0x00
    msg_sectors                 db "Sectors: ", 0x00
    msg_directory               db "DIR: ", 0x00
;

;--------------------------------------------------------------------------------------------------
; Start
;          
start:

    ; Clear screen
    call    clearscr

    ; Mount FAT12 disk
    mov     dl, 1   ; Mount disk 0
    call    floppy_mount
    jc      fail 

    ; ; Print message
    ; lea     si, msg_hello         
    ; call    prtstr                  
    ; call    prtendl
    
    ; Print Drive geometry
    lea     si, msg_cylinders         
    call    prtstr                  
    mov     ax, word [FloppyGeometryCylinders]
    call    prtnumdec
    call    prtendl

    lea     si, msg_heads
    call    prtstr                  
    xor     ax, ax
    mov     al, byte [FloppyGeometryHeads]
    call    prtnumdec
    call    prtendl

    lea     si, msg_sectors
    call    prtstr                  
    xor     ax, ax
    mov     al, byte [FloppyGeometrySectors]
    call    prtnumdec
    call    prtendl
    
    ; mov     ax, word [FloppyFATBytesPerSector]
    ; call    prtnumdec
    ; call    prtendl

    ; xor     ax, ax
    ; mov     al, byte [FloppyFATSectorsPerCluster]
    ; call    prtnumdec
    ; call    prtendl

    ; mov     ax, word [FloppyFATReservedSectors]
    ; call    prtnumdec
    ; call    prtendl

    ; xor     ax, ax
    ; mov     al, byte [FloppyFATNumberOfFATs]
    ; call    prtnumdec
    ; call    prtendl

    ; mov     ax, word [FloppyFATRootEntries]
    ; call    prtnumdec
    ; call    prtendl

    ; mov     ax, word [FloppyFATSectorsPerFAT]
    ; call    prtnumdec
    ; call    prtendl
    

    lea     si, msg_directory
    call    prtstr
    call    prtendl

    call print_root_dir 
    
    jmp haltlp

fail:
    mov     si, msg_fail
    call    prtstr
    call    prtendl

    
    
    ; Stop execution
haltlp:
    cli
    hlt
    jmp     haltlp
;
;--------------------------------------------------------------------------------------------------

;----------------------------------------------------------------------------------------------------------------------
; Text output functions

;--------------------------------------------------------------------------------------------------
; Print string
; Prints a NULL terminated string to screen
; Parameters:
; - SI: pointer to string
;                        
prtstr:    

    push    ax
    push    si

prtstr_char_lp:
    mov     al, [si]                ; Get next character
    cmp     al, 0                   ; Check if character is NULL
    jz      prtstr_done             ; If NULL, we're done

    call    prtchar 
    
    inc     si                      ; Next character
    
    jmp     prtstr_char_lp       
    
prtstr_done:
    pop     si
    pop     ax

    ret
;
;--------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Print endl
; Prints endl (0x0A 0x0D) to screen
;                        
prtendl:    

    push    ax

    mov     al, 0x0A                ; \r
    call    prtchar
    mov     al, 0x0D                ; \f
    call    prtchar
    
    pop     ax

    ret
;
;--------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Clear screen
;                        
clearscr:    

    push    ax
    
    ; Call interrupt
    mov     ah, 0x00
    mov     al, 0x03
    int     10h

    pop ax

    ret
;
;--------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Print decimal number
; Parameters:
;   - ax: number to print
;                        
prtnumdec:
    pusha
    
    ; Allocate local vars (5 chars)
    push bp
    mov     bp, sp
    sub     sp, 5
    
    xor     si, si      ; No digits read

prtnumdec_lp:
    ; Divide by 10
    mov     cx, 10      ; Dividend
    xor     dx, dx      ; High part of divisor, ax is already low part of divisor
    div     cx
    
    ; Store digit in buffer
    add     dl, '0'     ; ASCII value of digit
    ; mov     byte [prtnumdec_buf+bx], dl
    mov     byte [bp-5+si], dl
    inc     si          ; Number of digits

    ; Check if number is 0
    cmp     ax, 0
    je prtnumdec_done

    jmp prtnumdec_lp    ; Next digit
    
prtnumdec_done:

prtnumdec_prt_lp:

    dec     si                      ; Decrement counter
    js      prtnumdec_prt_done      ; Last bx was 0

    ; Print character
    mov     al, byte [bp-5+si] ; Get digit from buffer
    call    prtchar
    
    
    jmp prtnumdec_prt_lp

prtnumdec_prt_done:

    ; Restore stack
    mov     sp, bp
    pop     bp

    popa
    ret
;
;--------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Print binary number
; Parameters:
;   - ax: number to print
;                        
prtnumbin:
    pusha
    
    mov     cx, 16          ; Print 16 bits

prtnumbin_lp:
    

    ; Get next digit
    shl     ax, 1
    push    ax
    jc  prtnumbin_1
    
    mov     al, '0'         ; '0' character
    jmp prtnumbin_com

prtnumbin_1:
    mov     al, '1' ; '1' character

prtnumbin_com:
    call    prtchar
    
    pop     ax

    loop prtnumbin_lp    ; Next digit
    
    
    popa
    ret

;
;--------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Print character
; Prints a single character to screen
; Parameters:
;   - al: character to print
;                        
prtchar:    
    push ax

    mov     ah, 0x0E ; Print char in tty mode
    int     10h

    pop ax
    ret
;
;--------------------------------------------------------------------------------------------------

;
;----------------------------------------------------------------------------------------------------------------------

%include "floppy.asm"