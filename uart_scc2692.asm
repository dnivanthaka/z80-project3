UARTBASE  .equ 0x00

UART_MRA  .equ UARTBASE + 0x00 ; Mode A register (R/W)
UART_SRA  .equ UARTBASE + 0x10 ; Status register A (R)
UART_CSRA .equ UARTBASE + 0x10 ; Clock select register A (W)
UART_BRG  .equ UARTBASE + 0x20 ; BRG test (R)
UART_CRA  .equ UARTBASE + 0x20 ; Command register (W)
UART_RHRA .equ UARTBASE + 0x30 ; RX holding register A (R)
UART_THRA .equ UARTBASE + 0x30 ; TX holding register A (W)
UART_IPCR .equ UARTBASE + 0x40 ; Input port change register (R)
UART_ACR  .equ UARTBASE + 0x40 ; Aux control register (W)
UART_ISR  .equ UARTBASE + 0x50 ; Interrupt status register (R)
UART_IMR  .equ UARTBASE + 0x50 ; Interrupt mask register (W)
UART_CTU  .equ UARTBASE + 0x60 ; Counter/timer upper value (R)
UART_CRUR  .equ UARTBASE + 0x60 ; Counter/timer upper preset value (W)
UART_CTL  .equ UARTBASE + 0x70 ; Counter/timer lower value (W)
UART_CTLR .equ UARTBASE + 0x70 ; Counter/timer lower preset value (W)
UART_MRB  .equ UARTBASE + 0x80 ; Mode register B (R/W)
UART_SRB  .equ UARTBASE + 0x90 ; Status register B (R)
UART_CSRB .equ UARTBASE + 0x90 ; Clock select register B (R)
UART_1X16X .equ UARTBASE + 0xA0 ; 1X/16X Test (R)
UART_CRB   .equ UARTBASE + 0xA0 ; Command register B (W) 

UART_RHRB  .equ UARTBASE + 0xB0 ; RX holding register B (R) 
UART_THRB  .equ UARTBASE + 0xB0 ; TX holding register B (W) 
UART_RES   .equ UARTBASE + 0xC0 ; Reserved (R/W)
UART_IP    .equ UARTBASE + 0xD0 ; Input ports IP0 to IP6 (R)
UART_OPCR  .equ UARTBASE + 0xD0 ; Output ports conf. register (W)
UART_START .equ UARTBASE + 0xE0 ; Start counter command (R)
UART_SETOP .equ UARTBASE + 0xE0 ; Set output port bits command (W)
UART_STOP  .equ UARTBASE + 0xF0 ; Stop counter command (R)
UART_RSTOP .equ UARTBASE + 0xF0 ; Reset output port bits command (W)

#define EXP_CS  0x10
#define EXP_SCL 0x04
#define EXP_MOSI 0x08
#define EXP_MISO 0x04

uart_init:
    ld A, 0
    out (UART_IMR), A  ;Mask all interrupts

    ld A, 0b00001010   ; Disable Rx and Tx
    out (UART_CRA), A

    ld A, 0x93
    out (UART_MRA), A  ; RX, RTS, 8N1

    ld A, 0x07
    out (UART_MRA), A  ; MR2A, Normal, No TX CTS/RTS, 1 stop bit

    ld A, 0x70
    out (UART_ACR), A  ; Set 0, Timer, X1/X2 /16

    ld A, 0xCC
    out (UART_CSRA), A ; 38K4

    ;ld A, 0x03
    ld A, 0
    out (UART_OPCR), A    ; RxCA

    ld A, 0x05
    out (UART_CRA), A    ; Enable TX/RX

    ret

uart_putchar:
    push af
    call uart_writewait
    pop af
    out UART_THRA, A
    ret

uart_getchar:
    push af
    call uart_readwait
    pop af
    in A, UART_RHRA 

    ret

uart_writewait:
    in A, (UART_SRA)
    and 0b00000100
    xor 0b00000100
    jr nz, uart_writewait
    ret

uart_readwait:
    in A, (UART_SRA)
    and 0b00000001
    xor 0b00000001
    jr nz, uart_readwait
    ret

uart_output:
    push af
    ;cpl               ; A = ~A
    xor 0xff 
    out (UART_SETOP), A
    pop af
    ret

uart_resetoutput:
    push af
    ;cpl
    ;xor 0xff 
    out (UART_SETOP), A
    pop af
    ret

uart_setoutput:
    push af
    ;cpl
    ;xor 0xff
    out (UART_RSTOP), A
    pop af
    ret

uart_input:
    in A, (UART_IP)
    ret

uart_nibh2a:
    push af
    and 0x0F    ;mask left nibble
    add a, 0x30
    cp a, 0x3a
    jp m, uart_nibh2a_end
    add a, 7     ;correction for A to F
uart_nibh2a_end:
    call uart_putchar
    pop af
    ret

;prints a null terminated string, expects hl to have starting address
uart_print_str:
    push af
uart_print_str_lp:
    ld a, (hl)
    cp 0            ; check for null termination
    jp z, uart_print_str_end
    call uart_putchar 
    inc hl
    jp uart_print_str_lp
uart_print_str_end:
    pop af
    ret

uart_hex2ascii:
    push bc
    ld b, a
    srl a
    srl a
    srl a
    srl a
    call uart_nibh2a
    ld a, b
    call uart_nibh2a
    pop bc
    ret
;==================== Software based SPI ======================================
spisw_xfer:
    push af
    push bc
    push de
    ld b, 8
    ld c, a
    ld d, 0             ;used to recv data
spisw_tx_loop:
    ld a, EXP_MOSI
    rl c 
    jr c, spisw_one
    call uart_resetoutput
    jp spisw_clk
spisw_one:
    call uart_setoutput
spisw_clk:
    ld a, EXP_SCL
    call uart_setoutput
    ;read data here
    call uart_input
    and EXP_MISO
    cp EXP_MISO 
    jp z, spisw_recv_one
    ccf
    jp spisw_done_recv
spisw_recv_one:
    scf
spisw_done_recv:
    rr d 
    ld a, EXP_SCL
    call uart_resetoutput
    dec b
    jp nz, spisw_tx_loop
    pop de
    pop bc
    pop af
    ld a, d

    ret