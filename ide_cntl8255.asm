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

;==================================================================================
ata_init:
    ;Control word 1001 0010
    ;0x92
    ld a, 0x92
    out (IDECTRL_CTL), a ; DATA port as input and control port as output

    ld a, 0
    out (IDECTRL_PORTC), a ; Set control lines high - inverted output

    call ata_wait_for_ready

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

    call ata_wait_for_ready

    ret
;==================================================================================

ata_wait_for_ready:
    call ata_read_status
    ;wait for bsy flag to clear and rdy flag to set
    and 0b11000000
    xor 0b01000000
    jr nz, ata_wait_for_ready 

    ret
;==================================================================================

ata_wait_for_drq:
    call ata_read_status
    and 0b00001000
    xor 0b00001000
    jr nz, ata_wait_for_drq

    ret
;==================================================================================

;register to write, data
ata_write:

    ret

;==================================================================================
;register to read
ata_read:

    ret

;==================================================================================
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

;==================================================================================
;assumes databus in input mode, pass 2 parameters using stack, 1st param num words (2bytes), 2nd param pointer to buffer 
; ata_data_read:
;     ld hl, 2
;     add hl, sp
;     ld bc, (hl)    ;pointer to buffer
;     inc hl
;     ld de, (hl)    ;num bytes
;     ld hl, bc 
; ata_data_read_loop:
;     in a, (IDECTRL_PORTB) ;upper byte 1st
;     ld (hl), a
;     inc hl
;     in a, (IDECTRL_PORTA) ; lower byte 2nd
;     ld (hl), a
;     inc hl
;     ld a, d
;     call uart_hex2ascii
;     ;ld a, e
;     ;call uart_hex2ascii
;     dec de
;     jp nz, ata_data_read_loop

;     ret

ata_data_read:
    push af
    push bc
    ld b, a                  ;index of last word to read
    ld a, 0x92
    out (IDECTRL_CTL), a

    ld hl, ata_data
ata_data_read_loop:
    ld a, 0x30               ;select data register as read
    out (IDECTRL_PORTC), a
    in a, (IDECTRL_PORTB)
    ld (hl), a
    inc hl
    in a, (IDECTRL_PORTA)
    ld (hl), a
    inc hl
    ld a, 0
    out (IDECTRL_PORTC), a
    djnz ata_data_read_loop
    ;reading index 0
    ld a, 0x30
    out (IDECTRL_PORTC), a
    in a, (IDECTRL_PORTB)
    ld (hl), a
    inc hl
    in a, (IDECTRL_PORTA)
    ld (hl), a
    inc hl
    ld a, 0
    out (IDECTRL_PORTC), a
    pop bc
    pop af

    ret

;register in A and data in ata_data
ata_set_register:
    call ata_wait_for_ready
    
    ld a, 0x80
    out (IDECTRL_CTL), a
    ld a, 0
    out (IDECTRL_PORTB), a
    ld hl, ata_reg_data
    ld a, (hl) ;a = *hl
    out (IDECTRL_PORTA), a
    ;set command register write
    ld hl, ata_reg
    ld a, (hl)
    or 0x50
    out (IDECTRL_PORTC), a ; Set control lines 
    ld a, 0
    out (IDECTRL_PORTC), a ; Set control lines 
    ld a, 0x92
    out (IDECTRL_CTL), a

    ret

;==================================================================================
ata_drive_reset:

    ret

;==================================================================================
ata_read_status:
    push bc 
    ;set input mode
    ld a, 0x92
    out (IDECTRL_CTL), a
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

;==================================================================================

;ata_data  .db 0
ata_cntrl .db 0xff
tempdata .db 0
tempdata2 .db 'Z' 
