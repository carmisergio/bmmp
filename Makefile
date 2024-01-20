# Directories
OUT_DIR=out
BUILD_DIR=build
SRC_DIR=src

# Output disk image
DISK_IMG_PATH=$(OUT_DIR)/disk.img
DISK_IMG_BS=512
DISK_IMG_BLOCKS=2880
RESERVED_SECTORS=3 # 1 is Boot sector only

#  Source files
BOOT_SRC = $(SRC_DIR)/boot.asm
PLAYER_SRC = $(SRC_DIR)/player.asm
FLOPPY_SRC = $(SRC_DIR)/floppy.asm

# Build files
BOOT_BIN=$(BUILD_DIR)/boot.bin
PLAYER_BIN=$(BUILD_DIR)/player.bin

# Build Tools
NASM=nasm
DD=dd
MKFS_FAT=mkfs.fat
MCOPY=mcopy

# Emulator
DOSBOX=dosbox
DOSBOX_ARGS=-C "BOOT $(DISK_IMG_PATH)"

# Main build target
$(DISK_IMG_PATH): $(BOOT_BIN) $(PLAYER_BIN)
#	$(DD) if=/dev/zero of=$(DISK_IMG_PATH) bs=$(DISK_IMG_BS) count=$(DISK_IMG_BLOCKS)
	mkdir -p $(OUT_DIR)
	rm -f $(DISK_IMG_PATH)
	$(MKFS_FAT) -F 12 -R $(RESERVED_SECTORS) -C $(DISK_IMG_PATH) $$((($(DISK_IMG_BLOCKS)*$(DISK_IMG_BS))/1024))
	$(DD) if=$(BOOT_BIN) of=$(DISK_IMG_PATH) bs=$(DISK_IMG_BS) conv=notrunc
	$(DD) if=$(PLAYER_BIN) of=$(DISK_IMG_PATH) bs=$(DISK_IMG_BS) seek=1 conv=notrunc
	
	$(MCOPY) -i $(DISK_IMG_PATH) $(FLOPPY_SRC) ::/
	$(MCOPY) -i $(DISK_IMG_PATH) $(BOOT_SRC) ::/
	$(MCOPY) -i $(DISK_IMG_PATH) $(BOOT_BIN) ::/

# Bootloader build target
$(BOOT_BIN): $(BOOT_SRC)
	mkdir -p $(BUILD_DIR)
	$(NASM) -f bin -o $(BOOT_BIN) $(BOOT_SRC)
	
# Player kernel build target
$(PLAYER_BIN): $(PLAYER_SRC) $(FLOPPY_SRC)
	mkdir -p $(BUILD_DIR)
	$(NASM) -f bin -I $(SRC_DIR) -o $(PLAYER_BIN) $(PLAYER_SRC)

# Emulator
run: $(DISK_IMG_PATH)
	$(DOSBOX) $(DOSBOX_ARGS)

# Clean target
clean:
	rm -rf $(BUILD_DIR)
	rm -rf $(OUT_DIR)