;-----------------------------------------------------------------------------------------------------------------------
; Floppy routines
; Author: Sergio Carmine 4CITI <mesergiocarmi.net>                                                       
; Date: 18/01/2023                                                                                                     
;-----------------------------------------------------------------------------------------------------------------------

%define FAT12_BUFFER_SEGMENT 0x2000

;--------------------------------------------------------------------------------------------------
; Floppy BPB offsets
;

; BPB
%define FAT12_BPB_OFST_BYTES_PER_SECTOR     0x0B
%define FAT12_BPB_OFST_SECTORS_PER_CLUSTER  0x0D
%define FAT12_BPB_OFST_RESERVED_SECTORS     0x0E
%define FAT12_BPB_OFST_NUMBER_OF_FATS       0x10
%define FAT12_BPB_OFST_ROOT_ENTRIES         0x11
%define FAT12_BPB_OFST_TOTAL_SECTORS        0x13
%define FAT12_BPB_OFST_MEDIA_DESCRIPTOR     0X15
%define FAT12_BPB_OFST_SECTORS_PER_FAT      0x16
%define FAT12_BPB_OFST_SECTORS_PER_TRACK    0x18
%define FAT12_BPB_OFST_NUMBER_OF_HEADS      0x1A
%define FAT12_BPB_OFST_HIDDEN_SECTORS       0x1C
%define FAT12_BPB_OFST_LARGE_SECTOR_COUNT   0x20

; EBPB
%define FAT12_BPB_OFST_DRIVE_NUMBER         0x24
%define FAT12_BPB_OFST_SIGNATURE            0x26
%define FAT12_BPB_OFST_VOLUME_ID            0x27
%define FAT12_BPB_OFST_VOLUME_LABEL         0x2B
%define FAT12_BPB_OFST_SYSTEM_IDENTIFIER    0x36

; Directory table entry
%define FAT12_DIR_ENTRY_SIZE                32      ; Bytes
%define FAT12_DIR_ENTRY_OFST_ATTRIBUTES     0x0B    
%define FAT12_DIR_ENTRY_OFST_LOW_CLUSTER    0x1A

;
;------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Floppy info
;

    FloppyDriveNumber           db 0

    FloppyGeometryCylinders     dw 0
    FloppyGeometryHeads         db 0
    FloppyGeometrySectors       db 0
    
    FloppyFATBytesPerSector     dw 0
    FloppyFATSectorsPerCluster  db 0
    FloppyFATReservedSectors    dw 0
    FloppyFATNumberOfFATs       db 0
    FloppyFATRootEntries        dw 0
    FloppyFATSectorsPerFAT      dw 0

    FloppyFATRootStartSector    db 0
    FloppyFATRootSize           dw 0

    FloppyBufferFAT             dw 0 
    FloppyBufferDir             dw 0
;
;------------------------------------------------------------------------------------------------------------
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
    
    ; Check disk geomtry 
    cmp     byte [FloppyGeometryCylinders], 0
    je      floppy_get_geometry_fail
    cmp     byte [FloppyGeometryHeads], 0
    je      floppy_get_geometry_fail
    cmp     byte [FloppyGeometrySectors], 0
    je      floppy_get_geometry_fail

    clc
    jmp floppy_get_geometry_done

floppy_get_geometry_fail:
    stc

floppy_get_geometry_done:

    popa
    ret
;
;------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Read sector from floppy
; Parameters:
;   - ax:   cylinder
;   - dh:   head
;   - cl:   sector
;   - es:bx buffer to read to (must be 512 bytes)
; Carry set on fail
;                        
floppy_read_sector:
    pusha
    
    ; Local variables
    push    bp
    mov     bp, sp 
    sub     sp, 1 ; byte retries
    
    ; Get drive number
    mov     dl, [FloppyDriveNumber]

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

;--------------------------------------------------------------------------------------------------
; Read sector from floppy with LBA addressing
; Parameters:
;   - ax:   LBA address
;   - es:bx buffer to read to (must be 512 bytes)
; Carry set on fail
;                        
floppy_read_sector_lba:
    pusha

    ; Compute sector
    div     byte [FloppyGeometrySectors] 
    mov     cl, ah          ; Save Sector
    inc     cl
    
    ; Compute heads
    xor     ah, ah
    div     byte [FloppyGeometryHeads]
    mov     dh, ah          ; Save Head

    ; Compute cylinder (already have cylinder in cl)
    xor     ah, ah
    
    ; Actually perform the read
    ; mov     dl, 0
    ; mov     ax, 0 
    ; mov     dh, 0
    ; mov     cl, 1
    ; mov     bx, 0
    call    floppy_read_sector

    ; call    prtnumdec
    ; call    prtendl

    ; xor     ax, ax
    ; mov     al, dh
    ; call    prtnumdec
    ; call    prtendl

    ; xor     ax, ax
    ; mov     al, cl
    ; call    prtnumdec
    ; call    prtendl
    
    
    popa
    ret
;
;------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Read multiple sectors from floppy with LBA addressing (Requires FAT parameters to be loaded)
; Parameters:
;   - ax:   LBA address
;   - cx:   Number of sectors
;   - es:bx buffer to read to (must be 512 bytes)
; Carry set on fail
;                        
floppy_read_sectors_lba:
    pusha

floppy_read_sectors_lba_lp:

    ; Read sector
    call    floppy_read_sector_lba
    jc      floppy_read_sectors_lba_fail
    

    add     bx, [FloppyFATBytesPerSector] ; Increment buffer pointer
    
    inc     ax  ; Next sector
    
    loop    floppy_read_sectors_lba_lp 
    
    jmp     floppy_read_sectors_lba_done

floppy_read_sectors_lba_fail:  
    stc

floppy_read_sectors_lba_done:
    
    popa
    ret
;
;------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Read cluster from floppy
; Parameters:
;   - ax:   Cluster number
;   - es:bx buffer to read to (must be at list ByesPerSector * SectorsPerCluster long)
; Carry set on fail
;                        
floppy_read_cluster:
    pusha
    
    ; Compute LBA address of cluster in ax
    xor     dx, dx
    sub     ax, 2
    xor     cx, cx
    mov     cl, byte [FloppyFATSectorsPerCluster]
    mul     cx
    mov     cl, byte [FloppyFATRootStartSector]
    add     ax, cx
    add     ax, word [FloppyFATRootSize]
    
    ; Get number of sectors per cluster in cx
    xor     cx, cx
    mov     cl, byte [FloppyFATSectorsPerCluster] 
    
    ; Read sectors
    call floppy_read_sectors_lba

    popa
    ret
;
;------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Read root directory from floppy to appropriate buffer
; Parameters:
;   - es:bx buffer to floppy buffer area
; Carry set on fail
;                        
floppy_read_root:
    pusha

    ; Get root start LBA
    xor     ax, ax
    mov     al, byte [FloppyFATRootStartSector]
    
    ; Get root size
    mov     cx, word [FloppyFATRootSize]
    
    ; Read sectors
    mov     bx, word [FloppyBufferDir]  ; Set buffer location
    call floppy_read_sectors_lba
    jc floppy_read_root_fail
    
    clc
    jmp floppy_read_root_done

floppy_read_root_fail:
    stc
    
floppy_read_root_done:

    popa
    ret
;
;-----------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Load FAT from floppy
; Parameters:
;   - es:bx buffer to floppy buffer area
; Carry set on fail
;                        
floppy_load_fat:
    pusha

    ; Read sectors
    mov     ax, word [FloppyFATReservedSectors] ; First FAT starts after reserved sectors
    mov     cx, word [FloppyFATSectorsPerFAT]   ; Numbe of sectors to read
    mov     bx, word [FloppyBufferFAT]          ; Set buffer location
    call floppy_read_sectors_lba
    jc floppy_read_root_fail
    
    clc
    jmp floppy_load_fat_done

floppy_load_fat_fail:
    stc
    
floppy_load_fat_done:

    popa
    ret
;
;------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Mounts a FAT12 filesystem
; Parameters:
;   - dl:   drive number
;                        
floppy_mount:
    pusha
    
    ; Local variables
    push    bp
    mov     bp, sp 
    ; sub     sp, 1 ; byte retries
    
    ; Save drive number
    mov     byte [FloppyDriveNumber], dl
    
    ; Get disk geometry
    call    floppy_get_geometry
    jc      floppy_mount_fail

    ; Load BPB (sector 0)
    mov     ax, FAT12_BUFFER_SEGMENT
    mov     es, ax
    mov     ax, 0               ; LBA adderss = 0
    mov     bx, 0
    call    floppy_read_sector_lba
    jc      floppy_mount_fail 
    
    ; Check FAT12 signature
    cmp     byte es:[bx+FAT12_BPB_OFST_SIGNATURE], 0x28
    je      floppy_mount_sigok
    cmp     byte es:[bx+FAT12_BPB_OFST_SIGNATURE], 0x29
    je      floppy_mount_sigok

    jmp     floppy_mount_fail

floppy_mount_sigok:

    ; Populate filesystem info data
    mov     ax, word es:[bx+FAT12_BPB_OFST_BYTES_PER_SECTOR]
    mov     word [FloppyFATBytesPerSector], ax

    mov     al, byte es:[bx+FAT12_BPB_OFST_SECTORS_PER_CLUSTER]
    mov     byte [FloppyFATSectorsPerCluster], al

    mov     ax, word es:[bx+FAT12_BPB_OFST_RESERVED_SECTORS]
    mov     word [FloppyFATReservedSectors], ax

    mov     al, byte es:[bx+FAT12_BPB_OFST_NUMBER_OF_FATS]
    mov     byte [FloppyFATNumberOfFATs], al

    mov     ax, word es:[bx+FAT12_BPB_OFST_ROOT_ENTRIES]
    mov     word [FloppyFATRootEntries], ax

    mov     ax, word es:[bx+FAT12_BPB_OFST_SECTORS_PER_FAT]
    mov     word [FloppyFATSectorsPerFAT], ax
    
    ; Avoid divide by 0
    cmp     word [FloppyFATBytesPerSector], 0
    je      floppy_read_root_fail
    
    ; Compute root size
    xor     dx, dx
    mov     ax, word [FloppyFATRootEntries]
    mov     cx, FAT12_DIR_ENTRY_SIZE  ; Root entry size
    mul     cx
    div     word [FloppyFATBytesPerSector]
    mov     word [FloppyFATRootSize], ax ; Save root size
    
    ; Compute root start LBA address
    xor     ax, ax
    mov     al, byte [FloppyFATNumberOfFATs]
    mul     word [FloppyFATSectorsPerFAT]
    add     ax, [FloppyFATReservedSectors]
    mov     byte [FloppyFATRootStartSector], al ; Save root start sector
    
    ; Compute buffer pointers
    xor     dx, dx
    mov     ax, word [FloppyFATBytesPerSector]
    mul     word [FloppyFATSectorsPerFAT]
    mov     word [FloppyBufferDir], ax  ; After FAT there the dir
    
    
    ; Read root directory
    call    floppy_read_root
    jc      floppy_mount_fail
    
    ; Read File Allocation Table
    call    floppy_load_fat
    jc      floppy_mount_fail
    

    clc
    jmp floppy_mount_done

floppy_mount_fail:
    stc
    
floppy_mount_done:
    
    ; Restore stack
    mov     sp, bp
    pop     bp

    popa
    ret
;
;------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Get next cluster
; Paremeters:
;   - ax: current cluster
;   - es:bx pointer to floppy buffer
; Returns:
;   - ax: next cluster
;
floppy_get_next_cluster:
    push    bx
    push    cx
    push    dx
    push    es
    
    ; Get pointer to start of FAT buffer (floppy buffer + FloppyBufferFAT)
    add     bx, word [FloppyBufferFAT]
    
    ; Compute 3/2 of current cluster (because 12 bit)
    mov     cx, ax
    shr     cx, 1       ; Divide by 2
    add     ax, cx
    
    ; Get word containing next cluster
    add     bx, ax      ; Get pointer to this cluster
    mov     ax, word es:[bx] ; Read word containing cluster
    
    ; Check if cluster was even or odd
    test    dx, 1       ; Check last bit of current cluster
    jnz     floppy_get_next_cluster_odd

    ; Even clster
    and     ax, 0x0FFF ; Get least significant 12 bits
    
    jmp floppy_get_next_cluster_done

floppy_get_next_cluster_odd:
    ; Odd cluster
    shr     ax, 4       ; Get most significant 12 bits

floppy_get_next_cluster_done:

    push    es
    push    dx
    push    cx
    push    bx
    ret
;
;------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Get LBA address from cluster number
;                        
;
;------------------------------------------------------------------------------------------------------------


;--------------------------------------------------------------------------------------------------
; Print root directory
;                        
print_dir:
    pusha

    mov     ax, FAT12_BUFFER_SEGMENT 
    mov     es, ax
    mov     bx, word [FloppyBufferDir] ; Get address to dir buffer

    ; Entry counter 
    mov     dx, 0
    
    mov     cx, word [FloppyFATRootEntries]
print_dir_lp:    

    ; Check if this entry is the last in the dir
    cmp     byte es:[bx], 0x00
    je      print_dir_done
    
    ; Check if this entry is free
    cmp     byte es:[bx], 0xE5
    je      print_dir_common
    
    ; Check if entry is file or subdirectory
    cmp     byte es:[bx+FAT12_DIR_ENTRY_OFST_ATTRIBUTES], 0x10
    je      print_dir_subdir

    call    print_file_name
    call    prtendl

    jmp     print_dir_common
print_dir_subdir:
    
    call    print_dir_name
    
    ; Print directory number
    push    ax
    mov     ax, dx
    call    prtnumdec
    pop     ax

    call    prtendl
    
    inc     dx

print_dir_common:
    add     bx, FAT12_DIR_ENTRY_SIZE
    
    loop print_dir_lp
    
print_dir_done:

    popa
    ret
;
;------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Print file name
; Parameters:
;   - es:bx: Pointer to start of file entry
;                        
print_file_name:
    pusha
    
    mov     cx, 8   ; Max chars in filename

print_file_name_lp1:    

    mov     al, es:bx   ; Get character
    call    prtchar

    inc     bx          ; Next char
    loop    print_file_name_lp1 

    mov     al, '.' ; . between file name and extension
    call    prtchar
    
    mov     cx, 3   ; Max chars in extension

print_file_name_lp2:

    mov     al, es:bx   ; Get character
    
    call prtchar

    inc     bx          ; Next char
    loop    print_file_name_lp2 

    popa
    ret
;
;------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Print directory name
; Parameters:
;   - es:bx: Pointer to start of directory entry
;                        
print_dir_name:
    pusha
    
    mov     cx, 8   ; Max chars in filename

print_dir_name_lp1:    

    mov     al, es:bx   ; Get character
    call    prtchar

    inc     bx          ; Next char
    loop    print_dir_name_lp1 

    lea     si, msg_dir
    call    prtstr

    popa
    ret
msg_dir     db "     <DIR>  "
;
;------------------------------------------------------------------------------------------------------------
