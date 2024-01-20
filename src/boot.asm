;-----------------------------------------------------------------------------------------------------------------------
; Simple Bootloader V0.2
; This program is meant to be put in the boot sector of FAT12 formatted boot media.
; It will then load the main program from a specified number of reserved sectors and pass control.                                                                               
; It qualifies as a valid FAT12 Bios Parmeter Block, so it can be installed on a FAT12 formatted disk.
; Author: Sergio Carmine 4CITI <mesergiocarmi.net>                                                       
; Date: 09/01/2023                                                                                                     
;-----------------------------------------------------------------------------------------------------------------------

; Config parameters
%define KERNERL_SECTORS     2 ; Must be <= 17
%define VOLUME_LABEL        "SCMusPlayer" ; Must be 11 char long, pad with space if necessary

;==================================
; Memory table (HEX)
; ---------------------------------
; 0500:0000 (0x5000) | Stack
; 0500:4000 (0x9000) | (16 Kbytes)
; ---------------------------------
; 07C0:0000 (0x7C00) | Boot Sector
; 07C0:01FF (0x7DFF) | (512 bytes)
; ---------------------------------
; 0800:XXXX (0x8000) |  Kernel
;==================================


org 7C00h      ; Set location counter. BIOS loads boot sector to address 7C00h
bits 16        ; Generate 16 bit code

; Start bootloader
jmp start
db 3 - ($-$$) dup(0) ; Pad to 3 bytes

;--------------------------------------------------------------------------------------------------
; FAT12 BPB
;          
OEMLabel:               db "SCBootLd"
SectorSize:             dw 512
SectorsPerCluster:      db 1
ReservedSectors:        dw 1 + KERNERL_SECTORS
NumberOfFATs:           db 2
NumberOfRootDirEntries: dw 224
NumberOfLogicalSectors: dw 2880
MediaDescriptorType:    db 0xF0
SectorsPerFAT:          dw 9
SectorsPerTrack:        dw 18
NumberOfHeads:          dw 2
NumberOfHIddenSectors:  dd 0
NumberOfSectorsLarge:   dd 0
DriveNumber:            dw 0
Signature:              db 0x28
VolumeID:               dd 0
VolumeLabel:            db VOLUME_LABEL
FileSystemID:           db "FAT12   "
;
;--------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Start
;          
start:

    cli                             ; Disable interrupts

    ; Setup data segment
    xor     ax, ax                  ; Data segment is 0000h because data is loaded by BIOS in first segment
                                    ; after code which starts at 7c00h
    mov     ds, ax
    
    ; Initialize Stack
    mov     ax, 0x0500              ; Stack Segment
    mov     ss, ax
    mov     sp, 0x4000              ; Top of stack
    
    sti                             ; Enable interrupts

   
    ; Set default video mode (80x25)
    mov     ah, 00h                 ; Set video mode function
    mov     al, 03h                 ; Video mode 03h (80x25)
    int     10h                     ; Call video BIOS interrupt
    
    ; Print message
    lea     si, msg_loading         ; Load string address
    call    prtstr                  ; Print string
    
    ; Begin loading
load_retry:
    mov     ah, 02h                 ; Read function
    mov     al, KERNERL_SECTORS     ; How many sectors to read
    mov     ch, 0                   ; Cylinder
    mov     cl, 2                   ; Start sector
    mov     dh, 0                   ; Head
                                    ; dl is drive number and is not changed because the BIOS passes us
                                    ; the current drive in dl already
    mov     bx, 0800h           
    mov     es, bx                  ; Segment of destination data
    mov     bx, 0                   ; Offset into that segment
    int     13h                     ; Read
    
    ;stc ; Simulate bad reads
    jnc     load_succesful          ; CF is set if load unsuccesful
    
    cmp     byte [load_retries], 0  ; Check if there are retries left
    jz      load_failed             ; The load operation has failed
    
    dec     byte [load_retries]     ; Decrement retries left
    
    ; Print retrying message
    lea     si, msg_load_retry  
    call    prtstr
    
    jmp     load_retry          ; Retry 

load_failed:
    ; Print load failed message
    lea     si, msg_load_failed
    call    prtstr
    
    ; Press any key to continue...
    mov     ah, 0                   ; Get character function
    int     16h                     ; BIOS input interrupt
    
    ; Reboot!
    ; mov byte [0472h], 1234h
    jmp     0FFFFh:0000h
    
load_succesful:    

    ; Print success message
    lea     si, msg_success
    call    prtstr

    ; Setup data and segment for kernel
    mov     ax, 0x800
    mov     ds, ax

    ; Pass control to loaded program
    jmp     0x0800:0x0000
;
;--------------------------------------------------------------------------------------------------

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
;------------------------------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------------------------------
; Data section
;
    ; Messages
    msg_loading db "## Simple Bootloader v0.2 - Sergio Carmine   ##", 0Ah, 0Dh
                db "Loading...", 0Ah, 0Dh, 00h
    msg_load_retry db "Read error, retrying...", 0Ah, 0Dh, 00h
    msg_load_failed db "Loading failed! Press any key to reboot.", 0Ah, 0Dh, 00h
    msg_success db "Success!", 0Ah, 0Dh, 00h
    
    ; Variables
    load_retries db 5
;
;------------------------------------------------------------------------------------------------------------ 

; Pad to end of boot sector
db 510 - ($-$$) dup(0)
db 0x55, 0xAA ; Boot sector magic
