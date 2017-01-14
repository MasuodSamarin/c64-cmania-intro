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
.import __SPRITES_LOAD__, __CHARSET_FONT_LOAD__

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Constants
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
DEBUG = 0                               ; rasterlines:1, music:2, sine:4 all:7
SPRITE0_POINTER = <((__SPRITES_LOAD__ .MOD $4000) / 64)

INIT_MUSIC = $1000
PLAY_MUSIC = $1003

BITMAP_ADDR = $2000
SCREEN_RAM_ADDR = $0400

LABEL_TEXT_ROW = 17
SCROLL_TEXT_ROW = 20
SCROLL_TEXT_ADDR = SCREEN_RAM_ADDR + 40 * SCROLL_TEXT_ROW
SCROLL_BITMAP_ROW = 22
SCROLL_BITMAP_ADDR = BITMAP_ADDR + 320 * SCROLL_BITMAP_ROW

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; ZP
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
ZP_TMP0         = $20                   ; byte
ZP_TMP1         = $21                   ; byte
ZP_TMP2         = $22                   ; byte
ZP_TMP3         = $23                   ; byte
ZP_TMP4         = $24                   ; byte
ZP_TMP5         = $25                   ; byte
ZP_TMP6         = $26                   ; byte
ZP_TMP7         = $27                   ; byte
ZP_TMP8         = $28                   ; byte
ZP_TMP9         = $29                   ; byte

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Macros
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.segment "CODE"
        sei

        lda #$35                        ; no basic, no kernal
        sta $01

        lda $dd00                       ; Vic bank 0: $4000-$7FFF
        and #$fc
        ora #3
        sta $dd00

        lda #0
        sta $d020                       ; border color
        sta $d021                       ; background color

        lda #%00011000                  ; no scroll, multi-color, 40-cols
        sta $d016

        lda #%00011011                  ; charset mode, default scroll-Y position, 25-rows
        sta $d011

        lda #%00011000                  ; screen ram: $0400 (%0001xxxx), charset addr: $2000 (%xxxx100x)
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

        jsr init_color_ram
        jsr init_sprites
        jsr init_bitmap
        jsr init_charset

        lda #0                          ; second song
        tax
        tay
        jsr INIT_MUSIC

        cli

main_loop:
        lda $dc01                       ; space pressed
        cmp #$ef
        bne :+
        jmp exit
:       lda sync_raster                  ; raster triggered ?
        beq main_loop

        dec sync_raster

.if (::DEBUG & 1)
        dec $d020
.endif
        jsr anim_flicker
        jsr anim_labels
        jsr anim_sprite
        jsr anim_scroll_charset
        jsr anim_scroll_bitmap
        jsr cycle_sine_table
.if (::DEBUG & 1)
        inc $d020
.endif


.if (::DEBUG & 2)
        inc $d020
.endif
        lda #0
        tax
        tay
        jsr PLAY_MUSIC
.if (::DEBUG & 2)
        dec $d020
.endif


next_2:

        jmp main_loop

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; exit
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc exit
        lda #0
        sta $d011                               ; blank screen
        lda #$37                                ; exit routine
        sta $01                                 ; add here linker to next routine
        jmp 64738
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; init_color_ram
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_color_ram
        ldx #0
loop_a:
        lda logofinal_map,x                     ; c logo has 360 chars (9*40). Paint 360 chars
        sta SCREEN_RAM_ADDR,x
        tay
        lda logofinal_colors,y
        sta $d800,x

        lda logofinal_map + $0100,x
        sta SCREEN_RAM_ADDR + $0100,x
        tay
        lda logofinal_colors,y
        sta $d800 + $0100,x

        lda logofinal_map + $0200,x
        sta SCREEN_RAM_ADDR + $0200,x
        tay
        lda logofinal_colors,y
        sta $d800 + $0200,x

        lda logofinal_map + $02e8,x
        sta SCREEN_RAM_ADDR + $02e8,x
        tay
        lda logofinal_colors,y
        sta $d800 + $02e8,x

        dex
        bne loop_a

        ;
        ldx #39
        lda #$0b                                ; color
loop_b:
        sta $d800 + 40 * LABEL_TEXT_ROW,x
        dex
        bpl loop_b

        ;

        ldx #0                                  ; paint the rest with white
        lda #1
loop_c:
        sta $d800 + 18 * 40,x                   ; 7 lines. 7 * 40 = 280 = 256 + 24
        sta $d800 + 18 * 40 + 24,x
        dex
        bne loop_c


        ldx #0
loop_d:
        lda #$b0                                ; dark gray over black
        sta SCREEN_RAM_ADDR + 40 * SCROLL_BITMAP_ROW,x
        inx
        cpx #3*40
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
        lda #208                       ; set y position
        sta VIC_SPR0_Y
        sta VIC_SPR1_Y
        lda #206                       ; set y position
        sta VIC_SPR2_Y
        sta VIC_SPR3_Y
        lda #7                          ; set sprite color
        sta VIC_SPR0_COLOR
        sta VIC_SPR1_COLOR
        lda #0
        sta VIC_SPR2_COLOR
        sta VIC_SPR3_COLOR
        lda #SPRITE0_POINTER            ; set sprite pointers
        sta SCREEN_RAM_ADDR + $3f8
        sta SCREEN_RAM_ADDR + $3f9
        lda #SPRITE0_POINTER + 6        ; set sprite pointers
        sta SCREEN_RAM_ADDR + $3fa
        sta SCREEN_RAM_ADDR + $3fb

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
        sta SCROLL_BITMAP_ADDR,x                 ; init the last 960 pixels (320 * 3)
        sta SCROLL_BITMAP_ADDR + 320,x
        sta SCROLL_BITMAP_ADDR + 320 - 64 ,x
        dex
        bne loop
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; init_bitmap
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_charset
        ldx #0
        ldy #0

loop:
        lda __CHARSET_FONT_LOAD__ + 128 * 8,x           ; copy 64 chars (64 * 8 = 512)
        sta charset_copy,x
        tya
        sta __CHARSET_FONT_LOAD__ + 128 * 8,x

        lda __CHARSET_FONT_LOAD__ + 128 * 8 + 256,x
        sta charset_copy + 256,x
        tya
        sta __CHARSET_FONT_LOAD__ + 128 * 8 + 256,x
        dex
        bne loop

        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; segment "HICODE"
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.segment "HICODE"

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; anim_flicker
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc anim_flicker
        lda in_flicker
        bne do_flicker
        dec delay
        beq :+
        rts
:
        dec delay+1
        beq :+
        rts
:
        lda #4
        sta delay+1
        lda #1
        sta in_flicker
do_flicker:
        ldx palette_idx
        lda palette_1,x
        sta $d022
        lda palette_2,x
        sta $d023
        inx
        cpx #8
        bne :+
        ldx #0
        stx in_flicker
:       stx palette_idx

        rts
delay:          .word $400      ; counter
in_flicker:     .byte 0         ; boolean
palette_idx:    .byte 0
palette_1:      .byte $0f,$0f,$05,$05,$0a,$0a,$0e,$0e
palette_2:      .byte $01,$01,$0d,$0d,$07,$07,$03,$03
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; anim_labels
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc anim_labels

LABEL_MODE_PRINT = 0            ; prints using empty charset
LABEL_MODE_IN = 1               ; then charset is updated
LABEL_MODE_DELAY = 2            ; then delay
LABEL_MODE_OUT = 3              ; then fadeout the charset, and it starts again

        lda label_mode
        beq print_label
        cmp #LABEL_MODE_IN
        beq in_label
        cmp #LABEL_MODE_OUT
        beq out_label
        cmp #LABEL_MODE_DELAY
        beq delay_label
        rts

in_label:       jmp label_in
out_label:      jmp label_out
print_label:    jmp label_print
delay_label:    jmp label_delay


label_print:
        lda #LABEL_MODE_IN              ; next mode
        sta label_mode

        lda label_print_idx
        asl
        tay
        lda labels_addr,y
        sta @l_addr
        lda labels_addr+1,y
        sta @l_addr+1

        ldx #39
@loop:

@l_addr = * + 1
        lda labels,x                    ; self modifying
        ora #$80
        sta SCREEN_RAM_ADDR + 40 * LABEL_TEXT_ROW,x
        dex
        bpl @loop

        inc label_print_idx
        lda label_print_idx
        cmp #TOTAL_LABELS
        bne :+
        lda #0
        sta label_print_idx
:
        rts


label_in:
        ldx label_in_idx

        .repeat 64, XX
                .repeat 7, YY
                        lda __CHARSET_FONT_LOAD__ + 128 * 8 + 8 * XX + 6 - YY
                        sta __CHARSET_FONT_LOAD__ + 128 * 8 + 8 * XX + 7 - YY
                .endrepeat
                lda charset_copy + 8 * XX,x
                sta __CHARSET_FONT_LOAD__ + 128 * 8 + 8 * XX
        .endrepeat

        dec label_in_idx
        bpl @end
        lda #7
        sta label_in_idx
        lda #LABEL_MODE_DELAY
        sta label_mode
@end:
        rts

label_delay:
        dec label_delay_counter
        bne :+
        lda #LABEL_MODE_OUT
        sta label_mode
:       rts

label_out:
        ldx label_in_idx

        .repeat 64, XX
                .repeat 7, YY
                        lda __CHARSET_FONT_LOAD__ + 128 * 8 + 8 * XX + 6 - YY
                        sta __CHARSET_FONT_LOAD__ + 128 * 8 + 8 * XX + 7 - YY
                .endrepeat
                lda #0
                sta __CHARSET_FONT_LOAD__ + 128 * 8 + 8 * XX
        .endrepeat

        dec label_in_idx
        bpl @end
        lda #7
        sta label_in_idx
        lda #LABEL_MODE_PRINT
        sta label_mode
@end:
        rts

label_mode: .byte LABEL_MODE_DELAY
label_in_idx: .byte 7
label_delay_counter: .byte 0
label_print_idx: .byte 0

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
scroll_addr = * + 1
        lda scroll_text                                 ; self modifying
        cmp #$ff
        bne @2

        ; reached $ff ? Then start from the beginning
        lda #<scroll_text
        sta scroll_addr
        lda #>scroll_text
        sta scroll_addr+1
        lda scroll_text                                 ; self modifying

@2:     sta SCROLL_TEXT_ADDR + 39                       ; top part of the 1x2 char
        ora #$40                                        ; bottom part is 128 chars ahead in the charset
        sta SCROLL_TEXT_ADDR + 40 + 39                  ; bottom part of the 1x2 char

        inc scroll_addr
        bne endscroll
        inc scroll_addr+1

endscroll:
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; anim_scroll_bitmap
; uses ZP_TMP0 - ZP_TMP3 as temp variables
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc anim_scroll_bitmap

        ; uses ZP_TMP0
        lda #0
        sta ZP_TMP0                     ; tmp variable

        ldx #<__CHARSET_FONT_LOAD__
        ldy #>__CHARSET_FONT_LOAD__
        stx ZP_TMP2
        sty ZP_TMP3                     ; pointer to charset


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
        rol ZP_TMP0
        asl
        rol ZP_TMP0
        asl
        rol ZP_TMP0

        tay                             ; char_def = (ZP_TMP2),y
        sty ZP_TMP1                     ; to be used in the bottom part of the char

        clc
        lda ZP_TMP3
        adc ZP_TMP0                     ; A = charset[char_idx * 8]
        sta ZP_TMP3


        ; scroll top 8 bytes
        ; YY = char rows
        ; SS = bitmap cols
        .repeat 4, YY
                lda (ZP_TMP2),y
                ldx bit_idx             ; set C according to the current bit index
:               asl
                dex
                bpl :-

                php
                .repeat 33, SS
                        ; reflection
                        rol SCROLL_BITMAP_ADDR + 320 * 2 + (35 - SS) * 8 + 7 - (YY * 2)
                .endrepeat
                plp

                .repeat 33, SS
                        ; reflection
                        rol SCROLL_BITMAP_ADDR + 320 * 2 + (35 - SS) * 8 + 7 - (YY * 2 + 1)
                .endrepeat


                iny                     ; byte of the char
        .endrepeat

        ; scroll top 8 bytes
        ; YY = char rows
        ; SS = bitmap cols
        .repeat 4, YY
                lda (ZP_TMP2),y
                ldx bit_idx             ; set C according to the current bit index
:               asl
                dex
                bpl :-

                php
                .repeat 33, SS
                        ; reflection
                        rol SCROLL_BITMAP_ADDR + 320 * 1 + (35 - SS) * 8 + 7 - (YY * 2)
                .endrepeat
                plp

                .repeat 33, SS
                        ; reflection
                        rol SCROLL_BITMAP_ADDR + 320 * 1 + (35 - SS) * 8 + 7 - (YY * 2 + 1)
                .endrepeat


                iny                     ; byte of the char
        .endrepeat


        ; fetch bottom part of the char
        ; and repeat the same thing
        ; which is 512 chars appart from the previous.
        ; so, I only have to add #4 to ZP_TMP3
        clc
        lda ZP_TMP3
        adc #02                         ; the same thing as adding 512
        sta ZP_TMP3

        ldy ZP_TMP1                     ; restore Y from tmp variable

        ; scroll middle 8 bytes
        ; YY = char rows
        ; SS = bitmap cols
        .repeat 8, YY
                lda (ZP_TMP2),y
                ldx bit_idx             ; set C according to the current bit index
:               asl
                dex
                bpl :-

                .repeat 33, SS
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
        sta SCREEN_RAM_ADDR + $3f8
        lda sprite_frame_spr1,x
        sta SCREEN_RAM_ADDR + $3f9

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

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; top logo (charset)
.proc irq_a
        pha                             ; saves A, X, Y
        txa
        pha
        tya
        pha

        asl $d019                       ; clears raster interrupt

        lda #14
        sta $d022
        lda #3
        sta $d023

        lda #%00011000                  ; no scroll, multi-color, 40-cols
        sta $d016

        lda #%00011011                  ; charset mode, default scroll-Y position, 25-rows
        sta $d011

        lda #%00011000                  ; screen ram: $0400 (%0001xxxx), charset addr: $2000 (%xxxx100x)
        sta $d018

        lda #50 + 8 * 16 + 2            ; next irq at row 16
        sta $d012

        ldx #<irq_b
        ldy #>irq_b
        stx $fffe
        sty $ffff

        inc sync_raster

        pla                             ; restores A, X, Y
        tay
        pla
        tax
        pla
        rti                             ; restores previous PC, status
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; center. raster bars
.proc irq_b
        pha                             ; saves A, X, Y
        txa
        pha
        tya
        pha

        asl $d019                       ; clears raster interrupt

        STABILIZE_RASTER

        .repeat 20
                nop
        .endrepeat

        lda #%00011010                  ; screen ram: $0400 (%0001xxxx) (unchanged), charset addr: $2800 (%xxxx101x)
        sta $d018

        lda #%00001000                  ; no scroll, hi-res, 40-cols
        sta $d016

        .repeat 4, XX
                lda label_colors + XX    ; 4 cycles
                sta $d020               ; 4 cycles
                sta $d021               ; 4 cycles
                .repeat 24
                        nop             ; 48 cycles
                .endrepeat
                bit $00                 ; 3 cycles
        .endrepeat
        lda label_colors + 4             ; 4 cycles
        sta $d020                       ; 4 cycles
        sta $d021                       ; 4 cycles
        .repeat 4
                nop                     ; 8 cycles
        .endrepeat
        .repeat 7, XX
                lda label_colors + 5 + XX ; 4 cycles
                sta $d020               ; 4 cycles
                sta $d021               ; 4 cycles
                .repeat 24
                        nop             ; 48 cycles
                .endrepeat
                bit $00                 ; 3 cycles
        .endrepeat
        lda label_colors + 12            ; 4 cycles
        sta $d020                       ; 4 cycles
        sta $d021                       ; 4 cycles
        .repeat 4
                nop                     ; 8 cycles
        .endrepeat
        .repeat 3, XX
                lda label_colors + 13+ XX  ; 4 cycles
                sta $d020               ; 4 cycles
                sta $d021               ; 4 cycles
                .repeat 24
                        nop             ; 48 cycles
                .endrepeat
                bit $00                 ; 3 cycles
        .endrepeat

        lda #0
        sta $d020
        sta $d021

        lda #50 + 22 * 8 - 2
        sta $d012

        ldx #<irq_c
        ldy #>irq_c
        stx $fffe
        sty $ffff

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

        lda scroll_x                    ; x position
        sta $d016

        lda #50 + 22 * 8 - 2
        sta $d012

        ldx #<irq_d
        ldy #>irq_d
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

        lda #%00011000                  ; screen ram: $0400 (%0001xxxx) (unchanged), bitmap: $2000 (%xxxx1xxx)
        sta $d018

        .repeat 8*3, YY
:               lda $d012
                cmp #(50 + 22 * 8+YY+2)
                bne :-
                lda sine_table + YY
                sta $d016
.if (::DEBUG & 4)
                sta $d020
.endif
        .endrepeat
.if (::DEBUG & 4)
        lda #0
        sta $d020
.endif

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

sync_raster:        .byte 0                 ; boolean

logofinal_colors:
        .incbin "logofinal-colors.bin"
logofinal_map:
        .include "logofinal-map.s"

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
        scrcode " hola amiguitos. en este scroll se pueden poner saludos, por ejemplo 'quiero saludar a todos"
        scrcode " los televidentes que me estan viendo, tambi"
        .byte 59
        scrcode "n a mamita y a papito, y a mi abuelita, y a mi perrito, y a mi maestra de 4to grado.'"
        scrcode "     bueno, creo que se entiende la idea. chau. "
        .byte $ff
scroll_x:
        .byte 4                         ; $d016 value for charset scroll

labels:
                ;1234567890123456789012345678901234567890
        scrcode "        commodore mania presenta        "
        scrcode "            un jueguito v1.0            "
        scrcode "    original provisto por: pepe pepe    "
        scrcode "         crackeado por: torrente        "
        scrcode "         el 32 de febrero de 1954       "
        scrcode "         www.commodoremania.com         "
        scrcode "                 chau                   "
TOTAL_LABELS = (* - labels) / 40
labels_addr:
.repeat TOTAL_LABELS, YY
        .addr (labels + 40 * YY)
.endrepeat


label_colors:
        .byte $0b, $0c, $0f, $01, $01, $01, $01, $01
        .byte $01, $01, $01, $01, $01, $0f, $0c, $0b

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

charset_copy:
        .res 64 * 8, 0                          ; reserve space for 64 chars


.segment "CHARSET_LOGO"
.incbin "logofinal-charset.bin"

.segment "CHARSET_FONT"
.incbin "font_caren_1x2-charset.bin"

.segment "SPRITES"
.incbin "sprites.bin"

.segment "SIDMUSIC"
.incbin "uc-marionette.sid", $7e


