;-----------------------------------------------------------------------------------------------------------------------
; Floppy routines
; Author: Sergio Carmine 4CITI <mesergiocarmi.net>                                                       
; Date: 18/01/2023                                                                                                     
;-----------------------------------------------------------------------------------------------------------------------

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
; TODO: reset floppy controller on fail
;                        
floppy_read_sector:
    pusha
    
    ; Local variables
    push    bp
    mov     bp, sp 
    sub     sp, 1 ; byte retries

    mov     byte [bp-1], 5 ; retries = 5

floppy_read_sector_lp:

    ; Form cx register
    mov     ch, al          ; Low order bits of cylinder
    shl     ah, 6           ; Put low bits of high order byte of culinder in high two bits of bh
    and     cl, 0b00111111  ; Clean potentially dirty two bits
    or      cl, ah          ; Construct cl with bits as CCSSSSSS (C=cyl, S=sec)

    mov     ah, 0x02        ; Read sector
    mov     al, 1           ; Read 1 sector
    int     13h     
    ; stc           ; Simulate bad reads
    jnc floppy_read_sector_success
    
    ; Check if there are still retries
    dec byte [bp-1]              ; retries--
    jz floppy_read_sector_fail
    
    ; Reset disk system
    mov     ah, 0x00        ; Reset disk system
    int     13h
    
    jmp floppy_read_sector_lp ; Next retry
    
floppy_read_sector_fail:
    stc
    jmp floppy_read_sector_done

floppy_read_sector_success:
    clc
    
floppy_read_sector_done:
    
    ; Restore stack
    mov     sp, bp
    pop     bp

    popa
    ret
;
;------------------------------------------------------------------------------------------------------------