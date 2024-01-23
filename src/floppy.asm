;-----------------------------------------------------------------------------------------------------------------------
; Floppy routines
; Author: Sergio Carmine 4CITI <mesergiocarmi.net>                                                       
; Date: 18/01/2023                                                                                                     
;-----------------------------------------------------------------------------------------------------------------------


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

    ; Stuff populated at mount
    FloppyGeometryCylinders     dw 0
    FloppyGeometryHeads         db 0
    FloppyGeometrySectors       db 0
    FloppyDriveNumber           db 0
    FloppyFATBytesPerSector     dw 0
    FloppyFATSectorsPerCluster  db 0
    FloppyFATReservedSectors    dw 0
    FloppyFATNumberOfFATs       db 0
    FloppyFATRootEntries        dw 0
    FloppyFATSectorsPerFAT      dw 0
    FloppyFATRootStartSector    db 0
    FloppyFATRootSize           dw 0
    FloppyFATBytesPerCluster    dw 0
    
    ; Current directory info
    FloppyFATLoadedDirSectors   dw 0
    FloppyFATLoadedDirEntries   dw 0

    ; FAT12 Buffer pointers
    FloppyBuffrerSegment        dw 0
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
;   - es:bx buffer to read to (must be at least BytesPerSector bytes)
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
    
    ; Read sector
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
;   - es:bx buffer to read to (must be at least ByesPerSector bytes)
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
    
    ; Perform read
    call    floppy_read_sector

    popa
    ret
;
;------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Read multiple sectors from floppy with LBA addressing (Requires FAT parameters to be loaded)
; Parameters:
;   - ax:   LBA address
;   - cx:   Number of sectors
;   - es:bx buffer to read to (must be at least BytesPerSector * cx bytes)
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
; Carry set on fail
;                        
floppy_read_root:
    pusha
    
    ; Setup extra segment
    mov     ax, word [FloppyBuffrerSegment]
    mov     es, ax

    ; Get root start LBA
    xor     ax, ax
    mov     al, byte [FloppyFATRootStartSector]
    
    ; Get root size
    mov     cx, word [FloppyFATRootSize]
    
    ; Read sectors
    mov     bx, word [FloppyBufferDir]  ; Set buffer location
    call floppy_read_sectors_lba
    jc floppy_read_root_fail
    
    ; Set Directory size
    mov     ax, word [FloppyFATLoadedDirSectors]
    mov     word [FloppyFATLoadedDirSectors], ax
    
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
; Carry set on fail
;                        
floppy_load_fat:
    pusha
    
    ; Setup extra segment
    mov     ax, word [FloppyBuffrerSegment]
    mov     es, ax

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
;   - es:bx pointer to floppy FAT buffer
;                        
floppy_mount:
    pusha
    
    ; Save floppy buffer segment
    mov     cx, es
    mov     word [FloppyBuffrerSegment], cx
    
    ; Save drive number
    mov     byte [FloppyDriveNumber], dl
    
    ; Get disk geometry
    call    floppy_get_geometry
    jc      floppy_mount_fail
    
    ; Load BPB (sector 0)
    mov     ax, 0                       ; LBA adderss = 0
    ; bx is already at the start of the floppy buffer
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
    
    ; Compute bytes per cluster
    xor     dx, dx
    mov     ax, word [FloppyFATBytesPerCluster]
    mul     word [FloppyFATSectorsPerCluster]
    
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
    
    mov     word [FloppyBufferFAT], bx          ; FAT buffer at beginning

    xor     dx, dx
    mov     ax, word [FloppyFATBytesPerSector]
    mul     word [FloppyFATSectorsPerFAT]
    add     bx, ax
    mov     word [FloppyBufferDir], bx          ; After FAT, currently loaded directory
    
    ; Read File Allocation Table
    call    floppy_load_fat
    jc      floppy_mount_fail
    
    ; Read root directory
    call    floppy_read_root
    jc      floppy_mount_fail

    ; Save directory info
    mov     ax, word [FloppyFATRootSize]
    mov     word [FloppyFATLoadedDirSectors],  ax
    mov     ax, word [FloppyFATRootEntries]
    mov     word [FloppyFATLoadedDirEntries],  ax

    clc
    jmp floppy_mount_done

floppy_mount_fail:
    stc
    
floppy_mount_done:
    
    popa
    ret
;
;------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Get next cluster
; Paremeters:
;   - ax: current cluster
; Returns:
;   - ax: next cluster
;
floppy_get_next_cluster:
    push    bx
    push    cx
    push    dx
    push    es
    
    ; Setup extra segment
    mov     bx, word [FloppyBuffrerSegment]
    mov     es, bx
    
    ; Get pointer to start of FAT buffer (floppy buffer + FloppyBufferFAT)
    mov     bx, word [FloppyBufferFAT]
    
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

    pop     es
    pop     dx
    pop     cx
    pop     bx
    ret
;
;------------------------------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------------------------------
; Read file from disk to memory
; Parameters:
;   - ax: file start cluster
;   - cx: maximum number of cluster to read
;   - ex:bx: pointer to buffer where to read data
; Returns:
;   - dx: number of clusters read
; Carry set on fail
floppy_read_file:
    push ax
    push bx
    push cx

    xor     dx, dx         ; Sectors read    

floppy_read_file_cluster_lp:

    ; Read cluster
    call    floppy_read_cluster
    jc      floppy_read_file_fail

    ; Increment number of sectors read
    inc     dx
    
    ; Get next cluster
    call    floppy_get_next_cluster

    ; Check if cluster is the last cluster
    cmp     ax, 0xFF8
    jge     floppy_read_file_done
    
    ; Increment pointer by the cluster size
    add     bx, word [FloppyFATBytesPerCluster]
    

    loop    floppy_read_file_cluster_lp ; Next cluster
    
floppy_read_file_success:
    clc
    
floppy_read_file_fail:
    stc
    jmp     floppy_read_file_done
    
floppy_read_file_done:


    pop cx
    pop bx
    pop ax
    ret
;
;------------------------------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------------------------------
; Load directory with cluster number
; Parameters:
;   - ax: directory start cluster
; Carry set on fail
floppy_load_dir:
    pusha
    
    ; Check if directory to be loaded is the root directory
    cmp     ax, 0
    jne     floppy_load_dir_noroot
    
    
    ; Load root directory
    call    floppy_read_root 
    
    ; Save directory info
    mov     ax, word [FloppyFATRootSize]
    mov     word [FloppyFATLoadedDirSectors],  ax
    mov     ax, word [FloppyFATRootEntries]
    mov     word [FloppyFATLoadedDirEntries],  ax

    jmp     floppy_load_dir_end

floppy_load_dir_noroot:
    ; Load non-root directory

    ; Setup buffer segment 
    mov     bx, word [FloppyBuffrerSegment] 
    mov     es, bx 

    ; Read directory "file" to memory in FAT directory buffer
    mov     bx, word [FloppyBufferDir]
    mov     cx, word [FloppyFATRootSize]        ; Maximum sectors to load
    call    floppy_read_file
    jc      floppy_load_dir_fail
    
    ; Save number of clusters in directory
    mov     word [FloppyFATLoadedDirSectors], dx

    ; Calculate number of directory entries
    mov     ax, dx
    xor     dx, dx
    mov     cx, [FloppyFATBytesPerSector]
    mul     cx
    mov     cx, FAT12_DIR_ENTRY_SIZE
    div     cx
    mov     word [FloppyFATLoadedDirEntries], ax

floppy_load_dir_end:
    clc
    jmp floppy_load_dir_done

floppy_load_dir_fail:
    stc

floppy_load_dir_done:

    popa
    ret
;
;------------------------------------------------------------------------------------------------------------


;--------------------------------------------------------------------------------------------------
; Print root directory
;                        
print_dir:
    pusha
    
    ; ; Print number of entries
    ; mov     ax, word [FloppyFATLoadedDirEntries]
    ; call    prtnumdec
    ; call    prtendl

    mov     ax, word [FloppyBuffrerSegment] 
    mov     es, ax
    mov     bx, word [FloppyBufferDir] ; Get address to dir buffer

    ; Entry counter 
    mov     dx, 0
    
    mov     cx, word [FloppyFATLoadedDirEntries]
print_dir_lp:    

    ; Check if this entry is the last in the dir
    cmp     byte es:[bx], 0x00
    je      print_dir_done
    
    ; Check if this entry is free
    cmp     byte es:[bx], 0xE5
    je      print_dir_common
    
    ; Check if this entry is NOT a Long File Name
    cmp     byte es:[bx+FAT12_DIR_ENTRY_OFST_ATTRIBUTES], 0x0F
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
msg_dir     db "     <DIR>  ", 0x00
;
;------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Change directory
; Parameters:
;  - ax: index of directory to change to        
; Carry set on fail
change_dir:
    pusha

    mov     bx, word [FloppyBuffrerSegment] 
    mov     es, bx
    mov     bx, word [FloppyBufferDir] ; Get address to dir buffer

    ; Directory counter 
    mov     dx, 0
    
    mov     cx, word [FloppyFATLoadedDirEntries]
change_dir_lp:    

    ; Check if this entry is the last in the dir
    cmp     byte es:[bx], 0x00
    je      change_dir_notfound
   
    ; Check if this entry is free
    cmp     byte es:[bx], 0xE5
    je      change_dir_next
    
    ; Check if this entry is NOT a Long File Name
    cmp     byte es:[bx+FAT12_DIR_ENTRY_OFST_ATTRIBUTES], 0x0F
    je      change_dir_next
    
    ; Check if entry is file or subdirectory
    cmp     byte es:[bx+FAT12_DIR_ENTRY_OFST_ATTRIBUTES], 0x10
    jne     change_dir_next
    
    ; Check if this is the directory we want
    cmp     dx, ax
    jne     change_dir_nextdir
    
    ; Change to this dir
    mov     ax, word es:[bx+FAT12_DIR_ENTRY_OFST_LOW_CLUSTER]
    call    floppy_load_dir
    jc      change_dir_fail
    
    
    jmp     change_dir_done

change_dir_nextdir:

    inc     dx ; Increment directory counter

change_dir_next:

    add     bx, FAT12_DIR_ENTRY_SIZE    ; Next entry
    
    loop change_dir_lp
    
    jmp change_dir_notfound

change_dir_fail:
    stc
    jmp change_dir_done
    
change_dir_notfound:

    lea     si, msg_dir_notfound
    call    prtstr
    call    prtendl

change_dir_done:

    popa
    ret

msg_dir_notfound db "No such directory!", 0x00
;
;------------------------------------------------------------------------------------------------------------