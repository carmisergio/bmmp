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
    mov     ax, 0xA0FF
    call    prtnumbin
    lea     si, endl         
    call    prtstr


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
    
    xor bx, bx          ; No digits read

prtnumdec_lp:
    ; Divide by 10
    mov     cx, 10      ; Dividend
    xor     dx, dx      ; High part of divisor, ax is already low part of divisor
    div     cx
    
    ; Store digit in buffer
    add     dl, '0'     ; ASCII value of digit
    mov     byte [prtnumdec_buf+bx], dl
    inc     bx          ; Number of digits

    ; Check if number is 0
    cmp     ax, 0
    je prtnumdec_done

    jmp prtnumdec_lp    ; Next digit
    
prtnumdec_done:

    mov     ah, 0x0E ; Print char in tty mode

prtnumdec_prt_lp:

    dec     bx                      ; Decrement counter
    js      prtnumdec_prt_done      ; Last bx was 0

    ; Print character
    mov     al, byte [prtnumdec_buf+bx] ; Get digit from buffer
    int     10h
    
    
    jmp prtnumdec_prt_lp

prtnumdec_prt_done:

    popa
    ret

prtnumdec_buf       db 5 dup(0) ; Buffer for reversing number
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

;----------------------------------------------------------------------------------------------------------------------
; FAT12 Floppy code

;--------------------------------------------------------------------------------------------------
; Get floppy geometry from BIOS and store info in global floppy geomtry variables
; Parameters:
;   - dl:   drive number
; Carry set on fail
;                        
floppy_get_geometry:
    pusha

    ; Get drive geomtry from BIOS
    mov     ah, 0x08    ; Get current drive parameters
    int     13h
    
    ; Check for command success (carry is set on fail)
    jc      floppy_get_geometry_fail
    
    ; Copy info into the floppy geometry variables
    
    ; Heads
    inc     dh  ; Head number if 0-based
    mov     byte [FloppyGeometryHeads], dh 
    
    ; Sectors per track
    mov     byte [FloppyGeometrySectors], cl
    and     byte [FloppyGeometrySectors], 0b00111111 ; Bits 6 & 7 are high bits of cylinder count
    
    ; Cylinders
    mov     al, ch
    mov     ah, cl
    and     ah, 0b11000000  ; Only bits 6 and 7 are of cylinders
    ror     ah, 6           ; Put high order bits in low order bits of the high part of ax
    inc     ax              ; 0-base number
    mov     word [FloppyGeometryCylinders], ax


floppy_get_geometry_fail:

    popa
    ret
;
;------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Read sector from floppy
; Parameters:
;   - dl:   drive number
;   - ax:   cylinder
;   - dh:   head
;   - cl:   sector
;   - es:bx buffer to read to (must be 512 bytes)
; Carry set on fail
;                        
floppy_read_sector:
    pusha
    
    mov     cl, al  ; Low order bits of cylinder
    shl     ah, 6   ; Put low bits of high order byte of culinder in high two bits of bh
    or      ch, ah  ; Construct cl with bits as CCSSSSSS (C=cyl, S=sec)

    mov     ax, cx
    call    prtnumdec
    
    ; int     13h     ; Read sector

    popa
    ret
;
;------------------------------------------------------------------------------------------------------------


;
;----------------------------------------------------------------------------------------------------------------------