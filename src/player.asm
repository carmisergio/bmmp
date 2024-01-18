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
    endl                        db 0x0A, 0x0D, 0x00 ; Endline
    

    ; Floppy disk info
    FloppyGeometryCylinders     dw 0
    FloppyGeometryHeads         db 0
    FloppyGeometrySectors       db 0
    
    ; FAT Buffer pointers
    FATBuffersSegment           dw 0x1000
    FATBootSectorPointer        dw 0

;

;--------------------------------------------------------------------------------------------------
; Start
;          
start:

    ; Clear screen
    call    clearscr

    ; Print message
    lea     si, msg_hello         
    call    prtstr                  
    lea     si, endl         
    call    prtstr
    
    ; Get drive geometry
    mov     dl, 0x00
    call    floppy_get_geometry
    jc get_geometry_fail
    
    ; Print Drive geometry
    lea     si, msg_cylinders         
    call    prtstr                  
    mov     ax, word [FloppyGeometryCylinders]
    call    prtnumdec
    lea     si, endl         
    call    prtstr

    lea     si, msg_heads
    call    prtstr                  
    xor     ax, ax
    mov     al, byte [FloppyGeometryHeads]
    call    prtnumdec
    lea     si, endl         
    call    prtstr

    lea     si, msg_sectors
    call    prtstr                  
    xor     ax, ax
    mov     al, byte [FloppyGeometrySectors]
    call    prtnumdec
    lea     si, endl         
    call    prtstr
    
    ; Test print binary number
    ; mov     ax, 0xA0FF
    ; call    prtnumbin
    ; lea     si, endl         
    ; call    prtstr
    
    mov     ax, 0x0900
    mov     es, ax
    mov     dl, 0
    mov     ax, 0
    mov     dh, 0
    mov     cl, 2 
    mov     bx, 0
    call    floppy_read_sector
    jc get_geometry_fail
    
    xor     ax, ax
    mov     ax, word [es:0]
    call    prtnumbin


    jmp haltlp

get_geometry_fail:
    mov     si, msg_fail
    call    prtstr
    lea     si, endl         
    call    prtstr

    
    
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
    
    mov     ah, 0Eh                 ; Write character in teletype mode
    int     10h                     ; Call video BIOS interrupt
    
    inc     si                      ; Next character
    
    jmp     prtstr_char_lp       
    
prtstr_done:
    pop     si
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
    

    ; pop sp
    ; pop bp
    ; popa
    ; ret
    
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

    mov     ah, 0x0E ; Print char in tty mode

prtnumdec_prt_lp:

    dec     si                      ; Decrement counter
    js      prtnumdec_prt_done      ; Last bx was 0

    ; Print character
    mov     al, byte [bp-5+si] ; Get digit from buffer
    int     10h
    
    
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
    ; Call print interrupt
    mov     ah, 0x0E ; Print char in tty mode
    int     10h
    
    pop     ax

    loop prtnumbin_lp    ; Next digit
    
    
    popa
    ret

;
;--------------------------------------------------------------------------------------------------

;
;----------------------------------------------------------------------------------------------------------------------

%include "floppy.asm"