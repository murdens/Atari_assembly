    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include required files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "vcs.h"
    include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start an unintialised segment at $80 for variable declaration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u Variables
    org $80
P0Height ds 1       ;defines 1 byte for player 0 height
P1Height ds 1       ;defines 1 byte for player 1 height

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include our ROM code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    seg code
    org $F000

Reset:
    CLEAN_START      ; macro to safely clean memory and TIA
    
    ldx #$80	     ; Blue background colour
    stx COLUBK
    lda #%1111	     ; white playfield colour
    sta COLUPF		

    lda #10         ; A=10
    sta P0Height    ; POHeight = 10
    sta P1Height    ; P1Height = 10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the TIA registers for the colours of F0 and F1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #$40
    sta COLUP0         ; player 0 colour light red

    lda #$C6           ; player 1 colout light green
    sta COLUP1

    ldy #%00000010      ; CTRLPF D1 set to 1 means (score)
    sty CTRLPF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start a new frame by turning on VBLANK and VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:
    lda #02         ; same as binary value %00000010
    sta VBLANK      ; turn on VBLANK
    sta VSYNC       ; turnon VSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    REPEAT 3        ; repeat uses memory positions
    	sta WSYNC
    REPEND
    lda #0
    sta VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let the TIA output the recommended 37 scanlines of VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    REPEAT 37       ; if programme is small ok to use but if not loop
    	sta WSYNC
    REPEND
    lda #0
    sta VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw the 192 visible scanlines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VisibleScanlines:
    REPEAT 10
    	sta WSYNC
    REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display 10 scanlines for the scoreboard number
;; Pull data from an array of bytes deined at NumberBitmap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ldy #0
ScoreboardLoop:
    lda NumberBitmap,Y
    sta PF1
    sta WSYNC
    iny
    cpy #10      ;compare register Y value with 10 if equal then branch true 
    bne ScoreboardLoop

    lda #0
    sta PF1 ; disable playfield

    ;Draw 50 empty scanlines between scoreboard and players
    REPEAT 50
        sta WSYNC
    REPEND


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display 10 scanlines for the player 0 graphic
;; Pull data from an array of bytes deined at PlayerBitmap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ldy #0
Player0Loop:
    lda PlayerBitmap,Y
    sta GRP0
    sta WSYNC
    iny
    cpy POHeight      ;compare register Y value with 10 if equal then branch true 
    bne Player0Loop

    lda #0
    sta GRP0 ; disable player 0 graphics

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display 10 scanlines for the player 1 graphic
;; Pull data from an array of bytes deined at PlayerBitmap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ldy #0
Player1Loop:
    lda PlayerBitmap,Y
    sta GRP1
    sta WSYNC
    iny
    cpy P1Height      ;compare register Y value with 10 if equal then branch true 
    bne Player1Loop

    lda #0
    sta GRP1 ; disable player 1 graphics


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Need to fill rest of scanlines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    REPEAT 102
        sta WSYNC
    REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output 30 more VBLANK overscan lines  to complete our frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   lda #2
   sta VBLANK
   REPEAT 30
   	sta WSYNC
   REPEND
   lda #0
   sta VBLANK
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop to next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   jmp StartFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defines an array of bytes to draw the player.
;; We add these bytes to the final ROM addresses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   org $FFE8            ; on purpose fitting all the bytes as tight to end of ROM addresses
PlayerBitmap:
    .byte #%01111110    ;  ######
    .byte #%11111111    ; ########
    .byte #%10011001    ; #  ##  #
    .byte #%11111111    ; ########
    .byte #%11111111    ; ########
    .byte #%11111111    ; ########
    .byte #%10111101    ; # #### #
    .byte #%11000011    ; ##    ##
    .byte #%11111111    ; ########
    .byte #%01111110    ;  ######

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defines an array of bytes to draw the scoreboard number.
;; We add these bytes to the final ROM addresses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   org $FFF2
NumberBitmap:
    .byte #%00001110    ; ########
    .byte #%00001110    ; ########
    .byte #%00000010    ;      ###
    .byte #%00000010    ;      ###
    .byte #%00001110    ; ########
    .byte #%00001110    ; ########
    .byte #%00001000    ; ###
    .byte #%00001000    ; ###
    .byte #%00001110    ; ########
    .byte #%00001110    ; ########
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM size
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     
   org $FFFC
   .word Reset
   .word Reset