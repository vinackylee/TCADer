riscv64-unknown-elf-gcc -DPREALLOCATE=1 -mcmodel=medany -std=gnu99 -O2 -ffast-math -fno-common -fno-builtin-printf -Bstatic -nostdlib -nostartfiles -lm -lgcc -T test.ld testaes.c common/syscalls.c common/crt.S -I . 
