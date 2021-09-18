BANKSEL   .equ 0x00
;ROMDIS    .equ 0x01
IDECTRL_BASE   .equ 0x01

IDECTRL_CTL   .equ IDECTRL_BASE + 0x30
IDECTRL_PORTA .equ IDECTRL_BASE + 0
IDECTRL_PORTB .equ IDECTRL_BASE + 0x10
IDECTRL_PORTC .equ IDECTRL_BASE + 0x20

#define ATA_A0    0x01 ;non inverted
#define ATA_A1    0x01 ;not inverted
#define ATA_A2    0x01 ;not inverted
#define ATA_RD    0x01 ;inverted
#define ATA_RW    0x01 ;inverted
#define ATA_RESET 0x01 ;inverted
#define ATA_CS0   0x01 ;inverted
#define ATA_CS1   0x01 ;inverted

ATA_REGDATA  .equ 0x00
ATA_REGFEAT  .equ 0x01
ATA_REGSECTS .equ 0x02
ATA_REGLBA1  .equ 0x03 
ATA_REGLBA2  .equ 0x04 
ATA_REGLBA3  .equ 0x05 
ATA_REGLBA4  .equ 0x06 
ATA_REGSTAT  .equ 0x07

RAM_TOP .equ 0xffff


ata_data  .equ 0x8000

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

    ld a, 'X'
    call uart_putchar

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
    ld a, 0x80
    out (IDECTRL_CTL), a
    ld a, 0
    out (IDECTRL_PORTB), a
    ld a, 0xec
    out (IDECTRL_PORTA), a
    ;set command register write
    ;ld a, 0x92
    ;out (IDECTRL_CTL), a
    ld a, 0x57
    out (IDECTRL_PORTC), a ; Set control lines 
;l1:
;    jr l1
    ;ld a, 0x92
    ;out (IDECTRL_CTL), a
wait_for_drq:
     call ata_read_status
     and 0b00001000
     xor 0b00001000
     jr nz, wait_for_drq
    ;call uart_hex2ascii
    ;read data register
    ld a, 0x92
    out (IDECTRL_CTL), a

    ld b, 0xff
    ld hl, ata_data
test_loop:
    ld a, 0x30
    out (IDECTRL_PORTC), a
    in a, (IDECTRL_PORTB)
    ;ld a, 0x55
    ld (hl), a
    inc hl
    in a, (IDECTRL_PORTA)
    ;ld a, 0xaa
    ld (hl), a
    inc hl
    ld a, 0
    out (IDECTRL_PORTC), a
    ;call uart_putchar
    ;call ata_read_status
    ;call uart_hex2ascii
    ;ld a, ' '
    ;call uart_putchar
    djnz test_loop


    ld a, 0x92
    out (IDECTRL_CTL), a
    ;ld a, 0x80
    ;out (IDECTRL_CTL), a

    
    ld b, 0xff
    ld hl, ata_data
test_print:
    ld a, (hl)
    call uart_putchar
    inc hl
    ld a, (hl)
    call uart_putchar
    inc hl
    ;ld a, ' '
    ;call uart_putchar
    djnz test_print

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

ata_init:
    ;Control word 1001 0010
    ;0x92
    ld a, 0x92
    out (IDECTRL_CTL), a ; DATA port as input and control port as output

    ld a, 0
    out (IDECTRL_PORTC), a ; Set control lines high - inverted output

    ;reset drive
    ;ld a, (1 < 7)
    ;ld a, 0x80
    ;out (IDECTRL_PORTC), a ; Set control lines high - inverted output

    ;call DELAY_MS
    ;call DELAY_MS

    ;ld a, 0
    ;out (IDECTRL_PORTC), a ; Set control lines high - inverted output
    
    ;call DELAY_MS
    ;call DELAY_MS
    ;call DELAY_MS
    ;call DELAY_MS

ata_init_ready_wait:
    call ata_read_status
    ;wait for bsy flag to clear and rdy flag to set
    and 0b11000000
    xor 0b01000000
    jr nz, ata_init_ready_wait 

    ;setting drive as master and LBA enable LBA
    ld a, 0x80
    out (IDECTRL_CTL), a
    ld a, 0
    in a, (IDECTRL_PORTB) ; higher byte
    ld a, 0xe0
    in a, (IDECTRL_PORTA) ; lower byte
    ld a, 0x56
    out (IDECTRL_PORTC), a ; Set control lines 
    nop
    ld a, 0x92
    out (IDECTRL_CTL), a ; DATA port as input and control port as output

ata_init_ready_wait2:
    call ata_read_status
    ;wait for bsy flag to clear and rdy flag to set
    and 0b11000000
    xor 0b01000000
    jr nz, ata_init_ready_wait2 

    ret

;register to write, data
ata_write:

    ret

;register to read
ata_read:

    ret

;assumes databus in output mode, pass 2 bytes using stack 
ata_data_write:
    ;pop hl
    ld hl, 2
    add hl, sp
    ld a, (hl)
    inc hl    
    ;ld (ata_data), hl
    ;ld hl, (ata_data)
    ;ld a, (ata_data) 
    out (IDECTRL_PORTB), a ;low byte
    ld a, (hl)
    ;ld a, (ata_data + 1) 
    out (IDECTRL_PORTA), a ; high byte

    ret

;assumes databus in input mode, pass 2 parameters using stack, 1st param num words (2bytes), 2nd param pointer to buffer 
ata_data_read:
    ld hl, 2
    add hl, sp
    ld bc, (hl)    ;pointer to buffer
    inc hl
    ld de, (hl)    ;num bytes
    ld hl, bc 
ata_data_read_loop:
    in a, (IDECTRL_PORTB) ;upper byte 1st
    ld (hl), a
    inc hl
    in a, (IDECTRL_PORTA) ; lower byte 2nd
    ld (hl), a
    inc hl
    ld a, d
    call uart_hex2ascii
    ;ld a, e
    ;call uart_hex2ascii
    dec de
    jp nz, ata_data_read_loop

    ret

ata_drive_reset:

    ret

ata_read_status:
    push bc 
    ld a, 0x37
    out (IDECTRL_PORTC), a ; Set control lines 
    in a, (IDECTRL_PORTB) ; lower byte 2nd
    in a, (IDECTRL_PORTA) ; lower byte 2nd
    ld b, a
    ld a, 0
    out (IDECTRL_PORTC), a ; Set control lines 
    ld a, b
    pop bc 
    ret


;ata_data  .db 0
ata_cntrl .db 0xff
tempdata .db 0
tempdata2 .db 'Z' 

#include "uart_scc2692.asm" 