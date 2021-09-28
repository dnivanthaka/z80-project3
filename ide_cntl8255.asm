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
ATA_REGLBA0  .equ 0x03 
ATA_REGLBA1  .equ 0x04 
ATA_REGLBA2  .equ 0x05 
ATA_REGLBA3  .equ 0x06 
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
    ;nop
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
    and 0b10001000
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
; ata_data_write:
;     ;pop hl
;     ld hl, 2
;     add hl, sp
;     ld a, (hl)
;     inc hl    
;     ;ld (ata_data), hl
;     ;ld hl, (ata_data)
;     ;ld a, (ata_data) 
;     out (IDECTRL_PORTB), a ;low byte
;     ld a, (hl)
;     ;ld a, (ata_data + 1) 
;     out (IDECTRL_PORTA), a ; high byte

;     ret

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

ata_read_sector:
    push af
    push bc
    
    call ata_wait_for_ready

    ld a, ATA_REGLBA0
    ld (ata_reg), a
    ld hl, ata_lba_addr0
    ld a, (hl) 
    ld (ata_reg_data), a
    call ata_set_register

    ld a, ATA_REGLBA1
    ld (ata_reg), a
    ld hl, ata_lba_addr1
    ld a, (hl)
    ld (ata_reg_data), a 
    call ata_set_register

    ld a, ATA_REGLBA2
    ld (ata_reg), a
    ld hl, ata_lba_addr2
    ld a, (hl) 
    ld (ata_reg_data), a
    call ata_set_register

    ld a, ATA_REGLBA3
    ld (ata_reg), a
    ld hl, ata_lba_addr3
    ld a, (hl)
    or 0xe0 
    ld (ata_reg_data), a
    call ata_set_register

    ld a, ATA_REGSECTS
    ld (ata_reg), a
    ld hl, ata_sect_count
    ld a, (hl)
    ld (ata_reg_data), a
    call ata_set_register

    ld a, ATA_REGSTAT
    ld (ata_reg), a
    ld a, 0x20
    ld (ata_reg_data), a
    call ata_set_register

    call ata_wait_for_drq

    ld a, 0            ;read 256 words
    call ata_data_read

    pop bc
    pop af

    ret

ata_write_sector:
    push af
    push bc
    
    call ata_wait_for_ready

    ld a, ATA_REGLBA0
    ld (ata_reg), a
    ld hl, ata_lba_addr0
    ld a, (hl) 
    ld (ata_reg_data), a
    call ata_set_register

    ld a, ATA_REGLBA1
    ld (ata_reg), a
    ld hl, ata_lba_addr1
    ld a, (hl)
    ld (ata_reg_data), a 
    call ata_set_register

    ld a, ATA_REGLBA2
    ld (ata_reg), a
    ld hl, ata_lba_addr2
    ld a, (hl) 
    ld (ata_reg_data), a
    call ata_set_register

    ld a, ATA_REGLBA3
    ld (ata_reg), a
    ld hl, ata_lba_addr3
    ld a, (hl)
    or 0xe0 
    ld (ata_reg_data), a
    call ata_set_register

    ld a, ATA_REGSECTS
    ld (ata_reg), a
    ld hl, ata_sect_count
    ld a, (hl)
    ld (ata_reg_data), a
    call ata_set_register

    ld a, ATA_REGSTAT
    ld (ata_reg), a
    ld a, 0x30
    ld (ata_reg_data), a
    call ata_set_register

    call ata_wait_for_drq

    ld a, 0            ;write 256 words
    call ata_data_write

    pop bc
    pop af

    ret

ata_data_read:
    push af
    push bc
    ld b, a                  
    ld a, 0x92
    out (IDECTRL_CTL), a

    ld hl, ata_data
ata_data_read_loop:
    ;call ata_wait_for_drq
    ld a, 0x30               ;select data register as read
    out (IDECTRL_PORTC), a
    
    in a, (IDECTRL_PORTB)    ;Big endian format
    ld (hl), a
    inc hl
    in a, (IDECTRL_PORTA)
    ld (hl), a
    inc hl

    ld a, 0
    out (IDECTRL_PORTC), a
    nop
    djnz ata_data_read_loop
    pop bc
    pop af

    ret

ata_data_write:
    push af
    push bc
    ld b, a                  
    ld a, 0x80
    out (IDECTRL_CTL), a

    ld hl, ata_data
ata_data_write_loop:
    ;call ata_wait_for_drq
    ld a, 0x50               ;select data register as write
    out (IDECTRL_PORTC), a
    ld a, (hl)
    out (IDECTRL_PORTB), a   ;Big endian format
    inc hl
    ld a, (hl)
    out (IDECTRL_PORTA), a
    inc hl
    ld a, 0
    out (IDECTRL_PORTC), a
    nop
    djnz ata_data_write_loop
    ld a, 0x92
    out (IDECTRL_CTL), a
    pop bc
    pop af

    ret

;register in ata_reg and data in ata_data
ata_set_register:
    call ata_wait_for_ready
    
    ld a, 0x80
    out (IDECTRL_CTL), a
    ;set command register write
    ld hl, ata_reg
    ld a, (hl)
    or 0x50
    out (IDECTRL_PORTC), a ; Set control lines 

    ld a, 0
    out (IDECTRL_PORTB), a
    ld hl, ata_reg_data
    ld a, (hl) ;a = *hl
    out (IDECTRL_PORTA), a
    ld a, 0
    out (IDECTRL_PORTC), a ; Set control lines 
    ld a, 0x92
    out (IDECTRL_CTL), a

    ret

;register in ata_reg and data in ata_data 
ata_get_register:
    push af
    call ata_wait_for_ready
    
    ld a, 0x92
    out (IDECTRL_CTL), a
    ;set command register read
    ld hl, ata_reg
    ld a, (hl)
    or 0x30
    out (IDECTRL_PORTC), a ; Set control lines 
    ld hl, ata_reg_data
    in a, (IDECTRL_PORTA)
    ld (hl), a
    ld a, 0
    out (IDECTRL_PORTC), a ; Set control lines 
    pop af

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
    in a, (IDECTRL_PORTB) ; higher byte 1st
    in a, (IDECTRL_PORTA) ; lower byte 2nd
    ld b, a
    ld a, 0
    out (IDECTRL_PORTC), a ; Set control lines 
    ld a, b
    pop bc 

    ret

;==================================================================================

