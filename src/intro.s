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
.macpack mymacros                       ; stable raster
.include "c64.inc"                      ; c64 constants

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Imports/Exports
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.import __SPRITES_LOAD__, __SCREEN_RAM_LOAD__, __CHARSET_FONT_LOAD__

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Constants
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
DEBUG = 0                               ; rasterlines:1, music:2, all:3
SPRITE0_POINTER = <((__SPRITES_LOAD__ .MOD $4000) / 64)

INIT_MUSIC = $be00
PLAY_MUSIC = $be20

BITMAP_ADDR = $6000

SCROLL_TEXT_ADDR = __SCREEN_RAM_LOAD__ + 21 * 40
SCROLL_BITMAP_ADDR = BITMAP_ADDR + 23 * 320

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Macros
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.segment "CODE"
        sei

        lda #$35                        ; no basic, no kernal
        sta $01

        lda $dd00                       ; Vic bank 1: $4000-$7FFF
        and #$fc
        ora #2
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

        lda #1                          ; second song
        jsr INIT_MUSIC

        jsr init_color_ram
        jsr init_sprites
        jsr init_bitmap

        cli

main_loop:
        lda sync_music                  ; raster triggered ?
        beq next_1

.if (::DEBUG & 2)
        inc $d020
.endif
        dec sync_music
        jsr PLAY_MUSIC
.if (::DEBUG & 2)
        dec $d020
.endif

next_1:
        lda sync_anims
        beq next_2

.if (::DEBUG & 1)
        inc $d020
.endif
        dec sync_anims
        jsr anim_sprite
        jsr anim_scroll_charset
        jsr anim_scroll_bitmap
        jsr cycle_sine_table
.if (::DEBUG & 1)
        dec $d020
.endif

next_2:

        jmp main_loop

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; init_color_ram
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_color_ram
        ldx #0
loop_a:
        lda __SCREEN_RAM_LOAD__,x               ; c logo has 360 chars (9*40). Paint 360 chars
        tay
        lda c_logo_colors,y
        sta $d800,x

        lda __SCREEN_RAM_LOAD__+104,x           ; second part (256 + 104 = 360)
        tay
        lda c_logo_colors,y
        sta $d800+104,x

        dex
        bne loop_a

        ldx #0
loop_b:
        lda __SCREEN_RAM_LOAD__+9*40,x          ; maina has 360 chars (9*40), staring from char 360
        tay
        lda mania_colors,y
        sta $d800+360,x

        lda __SCREEN_RAM_LOAD__+9*40+104,x      ; second part (256 + 104 = 360)
        tay
        lda mania_colors,y
        sta $d800+360+104,x

        dex
        bne loop_b

        ldx #0                                  ; paint the rest with white
        lda #1
loop_c:
        sta $d800 + 18 * 40,x
        sta $d800 + 18 * 40 + 24,x
        dex
        bne loop_c


        ldx #0
loop_d:
        lda #$b0                                ; dark gray over black
        sta __SCREEN_RAM_LOAD__+23*40,x
        inx
        cpx #(2*40)
        bne loop_d

        rts
.endproc


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; init_sprites
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_sprites
        lda #%00001111                  ; enable some sprites
        sta VIC_SPR_ENA
        lda #%00000011
        sta $d01c                       ; multi-color sprite #0,#1

        lda #%00000000
        sta $d017                       ; double y resolution
        sta $d01d                       ; double x resolution

        lda #%00001010
        sta $d010                       ; 8-bit on for sprites x


        lda #34                        ; set x position
        sta VIC_SPR0_X
        lda #28                        ; set x position
        sta VIC_SPR2_X
        lda #52                        ; set x position
        sta VIC_SPR1_X
        lda #58                        ; set x position
        sta VIC_SPR3_X
        lda #216                       ; set y position
        sta VIC_SPR0_Y
        sta VIC_SPR1_Y
        lda #214                       ; set y position
        sta VIC_SPR2_Y
        sta VIC_SPR3_Y
        lda #7                          ; set sprite color
        sta VIC_SPR0_COLOR
        sta VIC_SPR1_COLOR
        lda #0
        sta VIC_SPR2_COLOR
        sta VIC_SPR3_COLOR
        lda #SPRITE0_POINTER            ; set sprite pointers
        sta __SCREEN_RAM_LOAD__ + $3f8
        sta __SCREEN_RAM_LOAD__ + $3f9
        lda #SPRITE0_POINTER + 6        ; set sprite pointers
        sta __SCREEN_RAM_LOAD__ + $3fa
        sta __SCREEN_RAM_LOAD__ + $3fb

        lda #0
        sta $d025                       ; sprite multicolor #0
        lda #10
        sta $d026                       ; sprite multicolor #1

        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; init_bitmap
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_bitmap
        ldx #0
        lda #0

loop:
        sta BITMAP_ADDR,x                 ; init the last 960 pixels (320 * 3)
        sta BITMAP_ADDR + 320,x
        sta BITMAP_ADDR + 320 - 64 ,x
        dex
        bne loop
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; anim_scroll_charset
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc anim_scroll_charset
        ; speed control
        dec scroll_x
        php

        lda scroll_x
        and #07
        sta scroll_x

        plp
        bmi do_scroll
        rts

do_scroll:
        ldx #0
loop:   lda SCROLL_TEXT_ADDR + 1,x                      ; scroll top part of 1x2 char
        sta SCROLL_TEXT_ADDR,x
        lda SCROLL_TEXT_ADDR + 40 + 1,x                 ; scroll bottom part of 1x2 char
        sta SCROLL_TEXT_ADDR + 40,x
        inx
        cpx #39
        bne loop

        ; put next char in column 40
        ldx scroll_idx
        lda scroll_text,x
        cmp #$ff
        bne @2

        ; reached $ff ? Then start from the beginning
        ldx #0
        stx scroll_idx
        lda scroll_text

@2:     sta SCROLL_TEXT_ADDR + 39                       ; top part of the 1x2 char
        ora #$80                                        ; bottom part is 128 chars ahead in the charset
        sta SCROLL_TEXT_ADDR + 40 +39                   ; bottom part of the 1x2 char
        inx
        stx scroll_idx

endscroll:
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; anim_scroll_bitmap
; uses $fa-$ff as temp variables
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc anim_scroll_bitmap

        ; uses fa-ff
        lda #0
        sta $fa                         ; tmp variable

        ldx #<__CHARSET_FONT_LOAD__
        ldy #>__CHARSET_FONT_LOAD__
        stx $fc
        sty $fd                         ; pointer to charset

load_scroll_addr = * + 1
        lda scroll_text_bitmap          ; self-modifying.1st address is 3 chars
        cmp #$ff                        ; before initial address in order to sync with
        bne next                        ; the charset scroll.
        ldx #0
        stx bit_idx
        ldx #<scroll_text
        ldy #>scroll_text
        stx load_scroll_addr
        sty load_scroll_addr+1
        lda scroll_text

next:
        clc                             ; char_idx * 8
        asl
        rol $fa
        asl
        rol $fa
        asl
        rol $fa

        tay                             ; char_def = ($fc),y
        sty $fb                         ; to be used in the bottom part of the char

        clc
        lda $fd
        adc $fa                         ; A = charset[char_idx * 8]
        sta $fd


        ; scroll top 8 bytes 
        ; YY = char rows
        ; SS = bitmap cols
        .repeat 8, YY
                lda ($fc),y
                ldx bit_idx             ; set C according to the current bit index
:               asl
                dex
                bpl :-

                .repeat 34, SS
                        ; reflection
                        rol SCROLL_BITMAP_ADDR + 320 + (35 - SS) * 8 + (7-YY)
                .endrepeat


                iny                     ; byte of the char
        .endrepeat


        ; fetch bottom part of the char
        ; and repeat the same thing
        ; which is 1024 chars appart from the previous.
        ; so, I only have to add #4 to $fd
        clc
        lda $fd
        adc #04                         ; the same thing as adding 1024
        sta $fd

        ldy $fb                         ; restore Y from tmp variable

        ; scroll middle 8 bytes
        ; YY = char rows
        ; SS = bitmap cols
        .repeat 8, YY
                lda ($fc),y
                ldx bit_idx             ; set C according to the current bit index
:               asl
                dex
                bpl :-

                .repeat 34, SS
                        ; reflection
                        rol SCROLL_BITMAP_ADDR + (35 - SS) * 8 + (7-YY)
                .endrepeat
                iny                     ; byte of the char
        .endrepeat


        ldx bit_idx
        inx
        cpx #8
        bne l1

        ldx #0
        inc load_scroll_addr
        bne l1
        inc load_scroll_addr+1
l1:
        stx bit_idx

        rts

bit_idx:
        .byte 0                         ; points to the bit displayed
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; anim_sprite
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc anim_sprite
        dec delay
        bmi anim
        rts
anim:
        lda #5
        sta delay

        ldx sprite_frame_idx
        lda sprite_frame_spr0,x
        sta __SCREEN_RAM_LOAD__ + $3f8
        lda sprite_frame_spr1,x
        sta __SCREEN_RAM_LOAD__ + $3f9

        inx
        cpx #SPRITE_MAX_FRAMES
        bne :+
        ldx #0
:
        stx sprite_frame_idx
        rts

delay:
        .byte 0
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; cycle_sine_table
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc cycle_sine_table
        lda sine_table
        sta sine_tmp

        ldx #0
loop:
        lda sine_table + 1,x
        sta sine_table,x
        inx
        cpx #SINE_TABLE_SIZE-1
        bne loop

        lda sine_tmp
        sta sine_table + SINE_TABLE_SIZE-1

        rts

sine_tmp: .byte 0
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

        lda #14
        sta $d022
        lda #2
        sta $d023

        lda #%00011000                  ; no scroll, multi-color, 40-cols
        sta $d016

        lda #%00011011                  ; charset mode, default scroll-Y position, 25-rows
        sta $d011

        lda #%01100000                  ; screen ram: $1800 (%0110xxxx), charset addr: $0000 (%xxxx000x)
        sta $d018

        lda #(50 + 9 * 8 + 1)           ; next irq at row 9
        sta $d012

        ldx #<irq_b
        ldy #>irq_b
        stx $fffe
        sty $ffff

        inc sync_anims

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

        lda #11
        sta $d022
        lda #14
        sta $d023

        lda #%01100010                  ; screen ram: $1800 (%0110xxxx) (unchanged), charset addr: $0800 (%xxxx001x)
        sta $d018

        lda #50 + 21 * 8
        sta $d012

        ldx #<irq_c
        ldy #>irq_c
        stx $fffe
        sty $ffff

        inc sync_music

        pla                             ; restores A, X, Y
        tay
        pla
        tax
        pla
        rti                             ; restores previous PC, status
.endproc

.proc irq_c
        pha                             ; saves A, X, Y
        txa
        pha
        tya
        pha

        asl $d019                       ; clears raster interrupt

        lda #%01100100                  ; screen ram: $1800 (%0110xxxx) (unchanged), charset addr: $1000 (%xxxx010x)
        sta $d018

        lda scroll_x                    ; x position
        sta $d016

        lda #50 + 23 * 8 - 2
        sta $d012

        ldx #<irq_d
        ldy #>irq_a
        stx $fffe
        sty $ffff

        pla                             ; restores A, X, Y
        tay
        pla
        tax
        pla
        rti                             ; restores previous PC, status
.endproc

.proc irq_d
        pha                             ; saves A, X, Y
        txa
        pha
        tya
        pha

        asl $d019                       ; clears raster interrupt

        STABILIZE_RASTER

        .repeat 14
                nop
        .endrepeat

        lda #%00111011                  ; bitmap mode, default scroll-Y position, 25-rows
        sta $d011

        lda #%01101000                  ; screen ram: $1800 (%0110xxxx) (unchanged), bitmap: $2000 (%xxxx1xxx)
        sta $d018

        .repeat 16, YY
:               lda $d012
                cmp #(50 + 23 * 8+YY+2)
                bne :-
                lda sine_table + YY
                sta $d016
.if (::DEBUG & 1)
                sta $d020
.endif
        .endrepeat

        lda #254
        sta $d012

        ldx #<irq_a
        ldy #>irq_a
        stx $fffe
        sty $ffff


        pla                             ; restores A, X, Y
        tay
        pla
        tax
        pla
        rti                             ; restores previous PC, status
.endproc

sync_music:        .byte 0                 ; boolean
sync_anims:        .byte 0                 ; boolean

c_logo_colors:
        .incbin "c_logo-colors.bin"
mania_colors:
        .incbin "mania-colors.bin"

sprite_frame_idx:
        .byte 0
sprite_frame_spr0:
        .byte SPRITE0_POINTER, SPRITE0_POINTER+1, SPRITE0_POINTER+2, SPRITE0_POINTER+1
sprite_frame_spr1:
        .byte SPRITE0_POINTER+3, SPRITE0_POINTER+4, SPRITE0_POINTER+5, SPRITE0_POINTER+4
SPRITE_MAX_FRAMES = * - sprite_frame_spr1

scroll_text_bitmap:
        scrcode "    "                   ; bitmap stars 3 chars after
scroll_text:
        scrcode "                *    *    *    *    *    *    "
        scrcode " Probando scroll con reflejo... todavia le falta hacer la parte de que se mueva el aguita... ese efectito con el seno y demas"
        scrcode ". Despues se lo agrego y vemos como queda. "
        .byte $ff
scroll_idx:
        .byte 0                         ; scroll inx
scroll_x:
        .byte 4                         ; $d016 value for charset scroll

sine_table:
; autogenerated table: easing_table_generator.py -s64 -m7 -aTrue -r easeInOutSine
.byte   0,  0,  0,  0,  0,  0,  0,  0
.byte   0,  1,  1,  1,  1,  1,  1,  1
.byte   1,  1,  2,  2,  2,  2,  2,  2
.byte   2,  3,  3,  3,  3,  3,  3,  3
.byte   4,  4,  4,  4,  4,  4,  5,  5
.byte   5,  5,  5,  5,  5,  6,  6,  6
.byte   6,  6,  6,  6,  6,  6,  7,  7
.byte   7,  7,  7,  7,  7,  7,  7,  7
; reversed
.byte   7,  7,  7,  7,  7,  7,  7,  7
.byte   7,  6,  6,  6,  6,  6,  6,  6
.byte   6,  6,  5,  5,  5,  5,  5,  5
.byte   5,  4,  4,  4,  4,  4,  4,  3
.byte   3,  3,  3,  3,  3,  3,  2,  2
.byte   2,  2,  2,  2,  2,  1,  1,  1
.byte   1,  1,  1,  1,  1,  1,  0,  0
.byte   0,  0,  0,  0,  0,  0,  0,  0
SINE_TABLE_SIZE = * - sine_table


.segment "SPRITES"
.incbin "sprites.bin"

.segment "SIDMUSIC"
.incbin "sanxion.sid", $7e

.segment "CHARSET_LOGO"
.incbin "c_logo-charset.bin"

.segment "CHARSET_MANIA"
.incbin "mania-charset.bin"

.segment "CHARSET_FONT"
.incbin "font_caren_1x2-charset.bin"

.segment "SCREEN_RAM"
.include "screen_ram.s"

