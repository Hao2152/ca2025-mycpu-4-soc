CROSS_COMPILE ?= $(HOME)/rv/toolchain/bin/riscv-none-elf-

ASFLAGS = -march=rv32i_zicsr -mabi=ilp32
# Optimization required: -O2 produces large stack frames that overflow in deep call chains
# RV32I lacks hardware multiply/divide, so -O2 significantly reduces code size
CFLAGS = -O2 -Wall -fno-builtin -march=rv32i_zicsr -mabi=ilp32
LDFLAGS = --oformat=elf32-littleriscv

AS := $(CROSS_COMPILE)as
CC := $(CROSS_COMPILE)gcc
LD := $(CROSS_COMPILE)ld
OBJCOPY := $(CROSS_COMPILE)objcopy

# Program targets (add new programs here)
PROGRAMS := nyancat uart shell fib fib_M mulbench mulbench_M Quiz3_ProblemC Quiz3_ProblemC_M Quiz4_ProblemC Quiz4_ProblemC_M Quiz4_ProblemG Quiz4_ProblemG_M
BINARIES := $(PROGRAMS:%=%.asmbin)

%.asmbin: %.elf
	$(OBJCOPY) -O binary -j .text -j .data $< $@

.DEFAULT_GOAL := all

all: $(BINARIES)

# All programs use proper init.S with ABI compliance and .bss clearing
nyancat.asmbin: nyancat.c nyancat-data.h init.o link.lds
	$(CC) $(CFLAGS) -DNYANCAT_COMPRESSION_DELTA=$(NYANCAT_COMPRESSION_DELTA) -c -o nyancat.o nyancat.c
	$(CROSS_COMPILE)ld -o nyancat.elf -T link.lds $(LDFLAGS) nyancat.o init.o
	$(OBJCOPY) -O binary -j .text -j .data nyancat.elf $@

uart.asmbin: uart.c mmio.h init.o link.lds
	$(CC) $(CFLAGS) -c -o uart.o uart.c
	$(CROSS_COMPILE)ld -o uart.elf -T link.lds $(LDFLAGS) uart.o init.o
	$(OBJCOPY) -O binary -j .text -j .data uart.elf $@

shell.asmbin: shell.c mmio.h init.o link.lds
	$(CC) $(CFLAGS) -c -o shell.o shell.c
	$(CROSS_COMPILE)ld -o shell.elf -T link.lds $(LDFLAGS) shell.o init.o
	$(OBJCOPY) -O binary -j .text -j .data shell.elf $@

fib.asmbin: fib.S init.o link.lds
	$(AS) -R $(ASFLAGS) -o fib.o fib.S
	$(CROSS_COMPILE)ld -o fib.elf -T link.lds $(LDFLAGS) fib.o init.o
	$(OBJCOPY) -O binary -j .text -j .data fib.elf $@

fib_M.asmbin: fib_M.S init.o link.lds
	$(AS) -R -march=rv32im_zicsr -mabi=ilp32 -o fib_M.o fib_M.S
	$(CROSS_COMPILE)ld -o fib_M.elf -T link.lds $(LDFLAGS) fib_M.o init.o
	$(OBJCOPY) -O binary -j .text -j .data fib_M.elf fib_M.asmbin

mulbench.asmbin: mulbench.S init.o link.lds
	$(AS) -R $(ASFLAGS) -o mulbench.o mulbench.S
	$(CROSS_COMPILE)ld -o mulbench.elf -T link.lds $(LDFLAGS) mulbench.o init.o
	$(OBJCOPY) -O binary -j .text -j .data mulbench.elf mulbench.asmbin

mulbench_M.asmbin: mulbench_M.S init.o link.lds
	$(AS) -R -march=rv32im_zicsr -mabi=ilp32 -o mulbench_M.o mulbench_M.S
	$(CROSS_COMPILE)ld -o mulbench_M.elf -T link.lds $(LDFLAGS) mulbench_M.o init.o
	$(OBJCOPY) -O binary -j .text -j .data mulbench_M.elf mulbench_M.asmbin

Quiz3_ProblemC.asmbin: Quiz3_ProblemC.S init.o link.lds
	$(AS) -R $(ASFLAGS) -o Quiz3_ProblemC.o Quiz3_ProblemC.S
	$(CROSS_COMPILE)ld -o Quiz3_ProblemC.elf -T link.lds $(LDFLAGS) Quiz3_ProblemC.o init.o
	$(OBJCOPY) -O binary -j .text -j .data Quiz3_ProblemC.elf Quiz3_ProblemC.asmbin

Quiz3_ProblemC_M.asmbin: Quiz3_ProblemC_M.S init.o link.lds
	$(AS) -R -march=rv32im_zicsr_zba_zbb_zbc_zbs -mabi=ilp32 -o Quiz3_ProblemC_M.o Quiz3_ProblemC_M.S
	$(CROSS_COMPILE)ld -o Quiz3_ProblemC_M.elf -T link.lds $(LDFLAGS) Quiz3_ProblemC_M.o init.o
	$(OBJCOPY) -O binary -j .text -j .data Quiz3_ProblemC_M.elf Quiz3_ProblemC_M.asmbin

Quiz4_ProblemC.asmbin: Quiz4_ProblemC.S init.o link.lds
	$(AS) -R $(ASFLAGS) -o Quiz4_ProblemC.o Quiz4_ProblemC.S
	$(CROSS_COMPILE)ld -o Quiz4_ProblemC.elf -T link.lds $(LDFLAGS) Quiz4_ProblemC.o init.o
	$(OBJCOPY) -O binary -j .text -j .data Quiz4_ProblemC.elf Quiz4_ProblemC.asmbin

Quiz4_ProblemC_M.asmbin: Quiz4_ProblemC_M.S init.o link.lds
	$(AS) -R -march=rv32im_zicsr -mabi=ilp32 -o Quiz4_ProblemC_M.o Quiz4_ProblemC_M.S
	$(CROSS_COMPILE)ld -o Quiz4_ProblemC_M.elf -T link.lds $(LDFLAGS) Quiz4_ProblemC_M.o init.o
	$(OBJCOPY) -O binary -j .text -j .data Quiz4_ProblemC_M.elf Quiz4_ProblemC_M.asmbin

Quiz4_ProblemG.asmbin: Quiz4_ProblemG.S init.o link.lds
	$(AS) -R $(ASFLAGS) -o Quiz4_ProblemG.o Quiz4_ProblemG.S
	$(CROSS_COMPILE)ld -o Quiz4_ProblemG.elf -T link.lds $(LDFLAGS) Quiz4_ProblemG.o init.o
	$(OBJCOPY) -O binary -j .text -j .data Quiz4_ProblemG.elf Quiz4_ProblemG.asmbin

Quiz4_ProblemG_M.asmbin: Quiz4_ProblemG_M.S init.o link.lds
	$(AS) -R -march=rv32im_zicsr -mabi=ilp32 -o Quiz4_ProblemG_M.o Quiz4_ProblemG_M.S
	$(CROSS_COMPILE)ld -o Quiz4_ProblemG_M.elf -T link.lds $(LDFLAGS) Quiz4_ProblemG_M.o init.o
	$(OBJCOPY) -O binary -j .text -j .data Quiz4_ProblemG_M.elf Quiz4_ProblemG_M.asmbin

init.o: init.S
	$(AS) -R $(ASFLAGS) -o $@ $<

# Generate nyancat animation data from upstream source
# Configure compression mode via NYANCAT_COMPRESSION_DELTA (default: 1 for delta-RLE)
# - NYANCAT_COMPRESSION_DELTA=1: delta-RLE compression (91% reduction, 4.7KB)
# - NYANCAT_COMPRESSION_DELTA=0: baseline RLE (87% reduction, 6.7KB)
NYANCAT_COMPRESSION_DELTA ?= 1

nyancat-data.h: ../../scripts/gen-nyancat-data.py
ifeq ($(NYANCAT_COMPRESSION_DELTA),1)
	python3 $< --delta --output $@
else
	python3 $< --output $@
endif

# Nyancat build depends on generated data
nyancat.o: nyancat.c nyancat-data.h

update: $(BINARIES)
	cp -f $(BINARIES) ../src/main/resources
	@echo ""
	@echo "Updated $(BINARIES) to ../src/main/resources"

clean:
	$(RM) *.o *.elf *.asmbin init_minimal.S nyancat-data.h

# Convenience targets (prevent implicit rule interference)
$(PROGRAMS): %: %.asmbin

.PHONY: all update clean $(PROGRAMS)
