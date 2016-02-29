;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;
; Commodore Mania Intro
;
; code: riq
; Some code snippets were taken from different places. Credit added in those snippets.
;       
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; c64 helpers
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.macpack cbm                            ; adds support for scrcode
.include "c64.inc"                      ; c64 constants

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Imports/Exports
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.import __SPRITES_LOAD__, __SCREEN_RAM_LOAD__

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Constants
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
DEBUG = 3                               ; rasterlines:1, music:2, all:3
SPRITE0_POINTER = ($1c00 / 64)

INIT_MUSIC = $be00
PLAY_MUSIC = $be20

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Macros
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.segment "CODE"
        sei

        lda #$35                        ; no basic, no kernal
        sta $01

        lda $dd00                       ; Vic bank 0: $0000-$3FFF
        and #$fc
        ora #3
        sta $dd00

        lda #0
        sta $d020                       ; border color
        lda #0
        sta $d021                       ; background color

        lda #%00011000                  ; no scroll, multi-color, 40-cols
        sta $d016

        lda #%00011011                  ; charset mode, default scroll-Y position, 25-rows
        sta $d011

        lda #%11001000                  ; screen ram: $3000 (%1100xxxx), charset addr: $2000 (%xxxx100x)
        sta $d018

        lda #$7f                        ; turn off cia interrups
        sta $dc0d
        sta $dd0d

        lda #01                         ; enable raster irq
        sta $d01a

        ldx #<irq_a                     ; setup IRQ vector
        ldy #>irq_a
        stx $fffe
        sty $ffff

        lda #$00
        sta $d012

        lda $dc0d                       ; ack possible interrupts
        lda $dd0d
        asl $d019

        jsr init_sprites
        jsr init_color_ram

        lda #1                          ; second song
        jsr INIT_MUSIC

        cli

main_loop:
        lda sync_raster_irq             ; raster triggered ?
        beq main_loop

        dec sync_raster_irq

.if (::DEBUG & 2)
        inc $d020
.endif
        jsr PLAY_MUSIC

.if (::DEBUG & 2)
        dec $d020
.endif

        jmp main_loop


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; init_sprites
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_sprites
        lda #%00000001                  ; enable sprite #0
        sta VIC_SPR_ENA
        sta $d01c                       ; multi-color sprite #0

        lda #0
        sta $d010                       ; no 8-bit on for sprites x
        sta $d017                       ; no y double resolution
        sta $d01d                       ; no x double resolution


        lda #100                        ; set x position
        sta VIC_SPR0_X
        lda #200                        ; set y position
        sta VIC_SPR0_Y
        lda #1                          ; set sprite color
        sta VIC_SPR0_COLOR
        lda #SPRITE0_POINTER            ; set sprite pointers
        sta __SCREEN_RAM_LOAD__ + $3f8

        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; init_color_ram
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_color_ram
        ldx #0
loop_a:
        lda __SCREEN_RAM_LOAD__,x                ; c logo has 360 chars (9*40). Paint 360 chars
        tay
        lda c_logo_colors,y
        sta $d800,x

        lda __SCREEN_RAM_LOAD__+104,x            ; second part (256 + 104 = 360)
        tay
        lda c_logo_colors,y
        sta $d800+104,x

        inx
        bpl loop_a

        ldx #0
loop_b:
        lda __SCREEN_RAM_LOAD__+360,x            ; maina has 360 chars (9*40), staring from char 360
        tay
        lda mania_colors,y
        sta $d800+360,x

        lda __SCREEN_RAM_LOAD__+360+104,x            ; second part (256 + 104 = 360)
        tay
        lda mania_colors,y
        sta $d800+360+104,x

        inx
        bpl loop_b

        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; irq vectors
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc irq_a
        pha                             ; saves A, X, Y
        txa
        pha
        tya
        pha

        asl $d019                       ; clears raster interrupt

        lda #0
        sta $d021
        lda #14
        sta $d022
        lda #2
        sta $d023

        lda #%11001000                  ; screen ram: $3000 (%1100xxxx), charset addr: $2000 (%xxxx100x)
        sta $d018

        lda #(50 + 9 * 8)               ; next irq at row 9
        sta $d012

        ldx #<irq_b
        ldy #>irq_b
        stx $fffe
        sty $ffff

        pla                             ; restores A, X, Y
        tay
        pla
        tax
        pla
        rti                             ; restores previous PC, status
.endproc

.proc irq_b
        pha                             ; saves A, X, Y
        txa
        pha
        tya
        pha

        asl $d019                       ; clears raster interrupt

        lda #0
        sta $d021
        lda #11
        sta $d022
        lda #14
        sta $d023

        lda #%11001010                  ; screen ram: $3000 (%1100xxxx), charset addr: $2800 (%xxxx101x)
        sta $d018

        lda #50
        sta $d012

        ldx #<irq_a
        ldy #>irq_a
        stx $fffe
        sty $ffff

        inc sync_raster_irq

        pla                             ; restores A, X, Y
        tay
        pla
        tax
        pla
        rti                             ; restores previous PC, status
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; global variables
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
sync_raster_irq:        .byte 0                 ; boolean

c_logo_colors:
        .include "c_logo-colors.s"
mania_colors:
        .include "mania-colors.s"


.segment "SPRITES"
.incbin "sprites.bin"

.segment "SIDMUSIC"
.incbin "sanxion.sid", $7e

.segment "CHARSET_LOGO"
.incbin "c_logo-charset.bin"

.segment "CHARSET_MANIA"
.incbin "mania-charset.bin"

.segment "SCREEN_RAM"
.include "screen_ram.s"

