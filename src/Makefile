#!Makefile
TOPDIR := $(shell pwd)
SUBDIRS := $(TOPDIR)/bootloader 
ASM = nasm

BOOTIMG = boot.img

all: $(patsubst %, _dir_%, $(SUBDIRS)) bios

$(patsubst %, _dir_%, $(SUBDIRS)) : 
	$(MAKE) -C $(patsubst _dir_%, %, $@)

clean: $(patsubst %, _dir2_%,$(SUBDIRS))

$(patsubst %, _dir2_%, $(SUBDIRS)) :
	$(MAKE) -C $(patsubst _dir2_%, %, $@) clean

.PHONY:bios
bios:
	sudo dd if=./bootloader/boot.bin of=$(BOOTIMG) bs=512 count=1 conv=notrunc
	sudo mount -o loop -t vfat $(BOOTIMG) /mnt
	sudo cp -f ./bootloader/loader.bin /mnt
	sync
	sudo umount /mnt


.PHONY:boch
boch:
	bochs -f .bochsrc

