BANKSEL   .equ 0x00
;ROMDIS    .equ 0x01
IDECTRL_BASE   .equ 0x01

RAM_TOP   .equ 0xffff
RAM_START .equ 0x2000


ata_data       .equ 0x8000
ata_reg        .equ 0x9000
ata_reg_data   .equ 0x9001

ata_lba_addr0  .equ 0x9002   
ata_lba_addr1  .equ 0x9003
ata_lba_addr2  .equ 0x9004
ata_lba_addr3  .equ 0x9005
ata_sect_count .equ 0x9006 

.ORG 0x0000
    di
    jp MAIN 

.ORG 0x0038
INT_HANDLER:
    jp INT_HANDLER
    
.ORG 0x0066
NMI_HANDLER:
    jp NMI_HANDLER 
    
.ORG 0x0100
MAIN:
    ld sp, RAM_TOP 
    ;ld a, 0x80
    ;out (IDECTRL + 0x30), a

    ;ld a, 0x55
    ;out (IDECTRL), a             ;PORTA

    ;ld a, 0xff
    ;out (IDECTRL + 0x10), a        ;PORTB

    ;ld a, 0x55
    ;out (IDECTRL + 0x20), a        ;PORTC
    ;ld A, 0x10
    ;out (BANKSEL), A

    call uart_init

    ;register = status = 0x07

    ;ld hl, tempdata2
    ;ld (hl), 'A'
    ;ld a, 'A'
    ;ld (hl), a
    ;ld (tempdata2), a
    ;xor a, a
    ;ld a, (hl)
    ;call uart_putchar

    ;ld a, 'X'
    ;call uart_putchar

    ;ld A, 0
    ;out (UART_OPCR), A

    ;ld a, EXP_SCL 
    ;call uart_resetoutput

    ; SCL op2, MOSI op3, MISO ip4, CS op4
      ;ld A, EXP_CS 
      ;call uart_resetoutput
    
       ;ld a, 0x40
       ;call SPISW_WRITE

       ;ld a, 0x00
       ;call SPISW_WRITE
    
       ;ld a, 0xf0
       ;call SPISW_WRITE

       ;ld A, EXP_CS 
       ;call uart_setoutput
    ; ; ; ld A, 0x14
    ; ; ; call uart_output

       ;nop
       ;nop
       ;nop
       ;nop
       ;nop

    ; ; ; ;=======================
    ; ; ; ld A, 0x04
    ; ; ; call uart_output
       ;ld A, EXP_CS 
       ;call uart_resetoutput

       ;ld a, 0x40
       ;call SPISW_WRITE

       ;ld a, 0x09
       ;call SPISW_WRITE

       ;ld a, 0xff
       ;call SPISW_WRITE

       ;ld A, EXP_CS 
       ;call uart_setoutput

     ;ld A, 0x14
     ;call uart_output
    call ata_init
    
    ld hl, STR_VERSION
    call uart_print_str
    ld hl, STR_DETECTHDD
    call uart_print_str

    ;ld a, 0x80
    ;out (IDECTRL_CTL), a

    ;ld hl, 0x55ff
    ;ld (ata_data), hl
    ;push hl 

    ;ld a, 0x80
    ;ld (ata_data+1), a
    ;ld a, (1 < 5) | (1 < 4) | (0 < 3) | (1 < 2) | (1 < 1) | 1
    ; ld a, 0x37
    ; out (IDECTRL_PORTC), a ; Set control lines 
    ; in a, (IDECTRL_PORTB) ; lower byte 2nd
    ; call uart_hex2ascii
    ; in a, (IDECTRL_PORTA) ; lower byte 2nd
    ; call uart_hex2ascii
    ; ld a, 0
    ; out (IDECTRL_PORTC), a ; Set control lines 
    ; ld a, 0x13 
    ; call uart_putchar
    ;ld bc, 1 
    ;push bc 
    ;ld hl, ata_data
    ;push hl
    ;call ata_data_read
    ;cleaning up the stack
    ;ld hl, 1
    ;add hl, sp
    ;ld sp, hl

    ;ld hl, ata_data
    ;ld a, (hl)
    ;call uart_output
    ;inc hl
    ;ld a, (hl)
    ;call uart_output
    
    ;writng 0xec command
    ; ld a, 0x80
    ; out (IDECTRL_CTL), a
    ; ld a, 0
    ; out (IDECTRL_PORTB), a
    ; ld a, 0xec
    ; out (IDECTRL_PORTA), a
    ; ;set command register write
    ; ;ld a, 0x92
    ; ;out (IDECTRL_CTL), a
    ; ld a, 0x57
    ; out (IDECTRL_PORTC), a ; Set control lines 
    ; ;ld a, 0x92
    ;out (IDECTRL_CTL), a
    ld a, ATA_REGSTAT
    ld (ata_reg), a
    ld a, 0xec
    ld (ata_reg_data), a
    call ata_set_register
    call ata_wait_for_drq
    ;call uart_hex2ascii
    ;read data register
;     ld a, 0x92
;     out (IDECTRL_CTL), a
    ld a, 0              ;read 256 words
    call ata_data_read

;     ld b, 0xff
;     ld hl, ata_data
; test_loop:
;     ld a, 0x30
;     out (IDECTRL_PORTC), a
;     in a, (IDECTRL_PORTB)
;     ;ld a, 0x55
;     ld (hl), a
;     inc hl
;     in a, (IDECTRL_PORTA)
;     ;ld a, 0xaa
;     ld (hl), a
;     inc hl
;     ld a, 0
;     out (IDECTRL_PORTC), a
;     ;call uart_putchar
;     ;call ata_read_status
;     ;call uart_hex2ascii
;     ;ld a, ' '
;     ;call uart_putchar
;     djnz test_loop

    ld a, 0x92
    out (IDECTRL_CTL), a
    ;ld a, 0x80
    ;out (IDECTRL_CTL), a
    ld hl, ata_data+54
    call uart_print_str
    ld a, 13
    call uart_putchar
    ld a, 10
    call uart_putchar

    ;disabling cache
    ld a, ATA_REGFEAT
    ld (ata_reg), a
    ld a, 0x55             ;disable cache
    ld (ata_reg_data), a
    call ata_set_register

    ld a, ATA_REGSTAT
    ld (ata_reg), a
    ld a, 0xef
    ld (ata_reg_data), a
    call ata_set_register
    call ata_wait_for_ready
    
    ;reading 0 sector
    ; ld a, ATA_REGLBA1
    ; ld (ata_reg), a
    ; ld a, 0
    ; ld (ata_reg_data), a
    ; call ata_set_register

    ; ld a, ATA_REGLBA2
    ; ld (ata_reg), a
    ; ld a, 0
    ; ld (ata_reg_data), a 
    ; call ata_set_register

    ; ld a, ATA_REGLBA3
    ; ld (ata_reg), a
    ; ld a, 0
    ; ld (ata_reg_data), a
    ; call ata_set_register

    ; ld a, ATA_REGLBA4
    ; ld (ata_reg), a
    ; ld a, 0xe0
    ; ld (ata_reg_data), a
    ; call ata_set_register

    ; ld a, ATA_REGSECTS
    ; ld (ata_reg), a
    ; ld a, 0x01
    ; ld (ata_reg_data), a
    ; call ata_set_register

    ; ld a, ATA_REGSTAT
    ; ld (ata_reg), a
    ; ld a, 0x20
    ; ld (ata_reg_data), a
    ; call ata_set_register

    ld hl, ata_lba_addr0
    ld a, 0
    ld (hl), a
    ld hl, ata_lba_addr1
    ld a, 0
    ld (hl), a
    ld hl, ata_lba_addr2
    ld a, 0
    ld (hl), a
    ld hl, ata_lba_addr3
    ld a, 0
    ld (hl), a
    ld hl, ata_sect_count
    ld a, 0x1
    ld (hl), a 

    call ata_read_sector
    ;setting LBA addresses
    ; ld a, 0x80
    ; out (IDECTRL_CTL), a
    ; ld a, 0
    ; out (IDECTRL_PORTB), a
    ; ld a, 0x00
    ; out (IDECTRL_PORTA), a
    ; ;set command register write
    ; ld a, 0x53
    ; out (IDECTRL_PORTC), a ; Set control lines 
    ; ;ld a, 0
    ; ;out (IDECTRL_PORTC), a ; Set control lines 
    
    ; call ata_wait_for_ready
    
    ; ;setting LBA addresses
    ; ld a, 0x80
    ; out (IDECTRL_CTL), a
    ; ld a, 0
    ; out (IDECTRL_PORTB), a
    ; ld a, 0x00
    ; out (IDECTRL_PORTA), a
    ; ;set command register write
    ; ld a, 0x54
    ; out (IDECTRL_PORTC), a ; Set control lines 
    ; ;ld a, 0
    ; ;out (IDECTRL_PORTC), a ; Set control lines 
    
    ; call ata_wait_for_ready
    
    ; ;setting LBA addresses
    ; ld a, 0x80
    ; out (IDECTRL_CTL), a
    ; ld a, 0
    ; out (IDECTRL_PORTB), a
    ; ld a, 0x00
    ; out (IDECTRL_PORTA), a
    ; ;set command register write
    ; ld a, 0x55
    ; out (IDECTRL_PORTC), a ; Set control lines 
    ; ;ld a, 0
    ; ;out (IDECTRL_PORTC), a ; Set control lines 
    
    ; call ata_wait_for_ready

    ; ;setting LBA addresses
    ; ld a, 0x80
    ; out (IDECTRL_CTL), a
    ; ld a, 0
    ; out (IDECTRL_PORTB), a
    ; ld a, 0xe0
    ; out (IDECTRL_PORTA), a
    ; ;set command register write
    ; ld a, 0x56
    ; out (IDECTRL_PORTC), a ; Set control lines 
    ; ;ld a, 0
    ; ;out (IDECTRL_PORTC), a ; Set control lines 
    
    ; call ata_wait_for_ready
    
    ; ;setting sector count
    ; ld a, 0x80
    ; out (IDECTRL_CTL), a
    ; ld a, 0
    ; out (IDECTRL_PORTB), a
    ; ld a, 0x01
    ; out (IDECTRL_PORTA), a
    ; ;set command register write
    ; ld a, 0x52
    ; out (IDECTRL_PORTC), a ; Set control lines 
    ; ;ld a, 0
    ; ;out (IDECTRL_PORTC), a ; Set control lines 

    ; call ata_wait_for_ready
    
    ; ;send command 0x20
    ; ld a, 0x80
    ; out (IDECTRL_CTL), a
    ; ld a, 0
    ; out (IDECTRL_PORTB), a
    ; ld a, 0x20
    ; out (IDECTRL_PORTA), a
    ; ;set command register write
    ; ld a, 0x57
    ; out (IDECTRL_PORTC), a ; Set control lines 
    ; ;ld a, 0
    ; ;out (IDECTRL_PORTC), a ; Set control lines 

;l1:
    ;call ata_read_status
    ;call uart_hex2ascii
    ;jr l1 
    ;call ata_wait_for_drq

    ;ld a, 0            ;read 256 words
    ;call ata_data_read
    ;ld a, 1
    ;call ata_data_read

    ;call ata_read_status
    ;call uart_hex2ascii

    ;call ata_read_status
    ;call uart_hex2ascii

    ;ld a, 0x10
    ;call uart_putchar

    ld d, 0     ;printing 256 bytes
    ld hl, ata_data
    ; ld a, (hl)
    ; call uart_hex2ascii
    ; inc hl
    ; ld a, (hl)
    ; call uart_hex2ascii
test_print:
    ;print offset value
    ;ld a, b
    ;call uart_hex2ascii
    ld a, (hl)
    call uart_hex2ascii
    inc hl
    ld a, (hl)
    call uart_hex2ascii
    inc hl
    ld a, 0x10 
    call uart_putchar
    dec d
    jr nz, test_print
    ld a, 0x13 
    call uart_putchar
    ld a, 0x10 
    call uart_putchar

;l1:
    ;call ata_read_status
    ;call uart_hex2ascii
    ;jr l1 

    ld a, 'D'
    call uart_putchar
    ;ld A, EXP_CS 
    ;    call uart_resetoutput

    ;    ld a, 0x41
    ;    call SPISW_WRITE

    ;    ld a, 0x09
    ;    call SPISW_WRITE

    ;    ld a, 0xff
    ;    call SPISW_WRITE

    ;    ld A, EXP_CS 
    ;    call uart_setoutput
MAIN_LP:
    ;call uart_getchar
    ;ld A, 'X'
    ;call uart_putchar
    ;ld A, 0xff
    ;call uart_resetoutput
    ;ld A, 0xff
    ;call uart_resetoutput
    ;ld A, 0x5a
    ;call uart_hex2ascii
    ;call uart_input
    ;call uart_hex2ascii
    ;ld A, 0x30
    ;out (BANKSEL), A
    ;ld a, 0x55
    ;call uart_resetoutput
    ;   ld A, EXP_CS 
    ;   call uart_resetoutput
    ;    ld A, EXP_CS 
    ;    call uart_resetoutput

    ;    ld a, 0x41
    ;    call SPISW_WRITE

    ;    ld a, 0x09
    ;    call SPISW_WRITE

    ;    ld a, 0xff
    ;    call SPISW_WRITE

    ;    ld A, EXP_CS 
    ;    call uart_setoutput
    
    ;call DELAY_MS
    ;call DELAY_MS

    ;ld A, 0x55
    ;call uart_setoutput
    ;ld A, 0x00
    ;out (BANKSEL), A
    ;ld A, 0xf0
    ;call uart_setoutput
    ;ld a, 0x55
    ;call uart_setoutput
    ;   ld A, EXP_CS 
    ;   call uart_setoutput
    
    ;call DELAY_MS 
    ;call DELAY_MS 
    jp MAIN_LP

delay_ms:
    push af
    push bc
    ld c, 0x02
delay_ms_inner_lp_init:
    ld b, 0xff
delay_ms_lp:
    nop
    nop
    nop
    nop
    djnz delay_ms_lp 
    dec c
    jr nz, delay_ms_inner_lp_init
    pop bc
    pop af
    ret

; ram_test:
;     push af
;     push bc

;     ld hl, RAM_TEST_PATTERNS
;     ld b, (hl)
;     ld hl, RAM_START
; ram_test_loop1:
;     ;test here
;     ld a, 0
;     ld (hl), a
;     inc hl
;     ld ix, hl
;     ld a, ixh
;     xor ixl
;     jr nz, ram_test_loop1
;     pop bc
;     pop af

;     ret

STR_VERSION .db 13, 10, "Z80 Boot BIOS v0.9, By Dinusha Amerasinghe" ,13, 10, 0
STR_DETECTHDD .db "Detecting fixed disk ...", 13, 10, 0
STR_FOUNDHDD .db "Found", 0
STR_NOTFOUNDHDD .db "Not Found", 13, 10, 0
STR_NOBOOTDEVICE .db "No boot device present", 13, 10, 0

RAM_TEST_PATTERNS .db 0xff, 0x55, 0xaa, 0

#include "uart_scc2692.asm" 
#include "ide_cntl8255.asm"