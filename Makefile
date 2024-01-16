OUT_DIR=out
BUILD_DIR=build
SRC_DIR=src

# Output disk image
DISK_IMG_PATH=$(OUT_DIR)/disk.img
DISK_IMG_BS=512
DISK_IMG_BLOCKS=2880
RESERVED_SECTORS=2 # 1 is Boot sector only

#  Source files
BOOT_SRC = $(SRC_DIR)/boot.asm
PLAYER_SRC = $(SRC_DIR)/player.asm

# Build files
BOOT_BIN=$(BUILD_DIR)/boot.bin
PLAYER_BIN=$(BUILD_DIR)/player.bin

# Tools
NASM=nasm
DD=dd
MKFS_FAT=mkfs.fat

# Main build target
$(DISK_IMG_PATH): $(BOOT_BIN) $(PLAYER_BIN)
#	$(DD) if=/dev/zero of=$(DISK_IMG_PATH) bs=$(DISK_IMG_BS) count=$(DISK_IMG_BLOCKS)
	mkdir -p $(OUT_DIR)
	rm -f $(DISK_IMG_PATH)
	$(MKFS_FAT) -F 12 -R $(RESERVED_SECTORS) -C $(DISK_IMG_PATH) $$((($(DISK_IMG_BLOCKS)*$(DISK_IMG_BS))/1024))
	$(DD) if=$(BOOT_BIN) of=$(DISK_IMG_PATH) bs=$(DISK_IMG_BS) conv=notrunc
	$(DD) if=$(PLAYER_BIN) of=$(DISK_IMG_PATH) bs=$(DISK_IMG_BS) seek=1 conv=notrunc

# Bootloader build target
$(BOOT_BIN): $(BOOT_SRC)
	mkdir -p $(BUILD_DIR)
	$(NASM) -f bin -o $(BOOT_BIN) $(BOOT_SRC)
	
# Player kernel build target
$(PLAYER_BIN): $(PLAYER_SRC)
	mkdir -p $(BUILD_DIR)
	$(NASM) -f bin -o $(PLAYER_BIN) $(PLAYER_SRC)

# Clean target
clean:
	rm -rf $(BUILD_DIR)
	rm -rf $(OUT_DIR)