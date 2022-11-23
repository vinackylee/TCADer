SECTIONS{
    .memory : {
        . = 0x10000000;
        main*(.text);
        *(.text);
        *(*);
        end = .;
    }
}
