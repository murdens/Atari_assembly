    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include required files with VCS register memory mapping and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "vcs.h"
    include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare the variables starting from memory address $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u Variables
    org $80

JetXPos         byte                    ; player-0 X position
JetYPos         byte                    ; Player-0 Y position
BomberXPos      byte                    ; Player-1 X position
BomberYPos      byte                    ; Player-1 Y position
JetSpritePtr    word                    ; pointer to player 0 sprite lookup table
JetColorPtr     word                    ;
BomberSpritePtr word                    ; pointer to player1 sprite lookup table
BomberColorPtr  word
JetAnimeOffset  byte                    ; player0 sprite frame offset for change of bitmap
Random          byte                    ;random number generator

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JET_HEIGHT = 9                          ; number of rows in lookup table
BOMBER_HEIGHT = 9   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start our ROM code at memory adress $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg code 
    org $F000

Reset:
    CLEAN_START                         ; call macro to clean memory space

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialise RAM variables and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #68
    sta JetXPos                         ; Jet X position == 68
    lda #10
    sta JetYPos                         ; Jet X position == 10
    lda #62
    sta BomberXPos                      ; bomber X
    lda #83
    sta BomberYPos                      ; Bomber Y

    lda #%11010100
    sta Random                          ; Random = $D4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialise the pointers to the correct lookup table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #<JetSprite
    sta JetSpritePtr                    ; lo-byte pointer for jet sprite  lookup table
    lda #>JetSprite
    sta JetSpritePtr+1                  ; hi-byte

    lda #<JetColor
    sta JetColorPtr                    ; lo-byte
    lda #>JetColor
    sta JetColorPtr+1                  ; hi-byte

    lda #<BomberSprite
    sta BomberSpritePtr                    ; lo-byte
    lda #>BomberSprite
    sta BomberSpritePtr+1                  ; hi-byte

    lda #<BomberColor
    sta BomberColorPtr                    ; lo-byte
    lda #>BomberColor
    sta BomberColorPtr+1                  ; hi-byte

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start the main display loop and frame rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display 3 VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VSYNC
    REPEAT 3
        sta WSYNC                       ; displays the 3 lines of VSYNC
    REPEND
    lda #0
    sta VSYNC                           ; turn off VSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and tasks performed in pre-VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda JetXPos
    ldy #0
    jsr SetObjectXPos                   ; set player 0 horizontal position

    lda BomberXPos
    ldy #1
    jsr SetObjectXPos                   ; set player 1 horizontal position

    sta WSYNC
    sta HMOVE                           ; apply the horizontal offsets

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display remaining lines of VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK
    REPEAT 37
        sta VBLANK
    REPEND
    lda #0
    sta VBLANK                          ; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the 96 visible scanlines of our main game (2 line kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GasVisibleLine:
    lda #$00                            ; store background colour to blue
    sta COLUBK
    lda #$02                            ; store playfield colour green
    sta COLUPF
    lda #%000000001                     ; enable display playfield reflect
    sta CTRLPF
    lda #$F0                            ; setting PF0 bit pattern
    sta PF0                             
    lda #%11000000                      ; setting PF1 bit pattern
    sta PF1
    lda #0                              ; setting PF2 bit pattern
    sta PF2

    ldx #96                             ; X counts number of remaining scanlines
.GameLineLoop:
.IsInsideJetSprite:                     ; check if should render sprite player0
    txa                                 ; transfer X to A
    sec                                 ; set carry flag as subtraction
    sbc JetYPos                         ; subtract sprite Y coordinate
    cmp #JET_HEIGHT                     ; compare with Jet height
    bcc .DrawJetSprite                  ; if result < SpriteHeight, call subroutine
    lda #0                              ; else, set lookup index to 0
.DrawJetSprite:
    clc                                 ; clear carry flag before addition
    adc JetAnimeOffset                  ; jump to correct sprite frame address in memory
    tay                                 ; load A->Y as only Y register allows indirect addressing for pointer
    lda (JetSpritePtr),Y                ; load player bitmap slice of data
    sta WSYNC                           ; wait for next scanline
    sta GRP0                            ; set graphics for player 0
    lda (JetColorPtr),Y                 ; load player color from lookup table
    sta COLUP0                          ; set color for player 0 slice

.IsInsideBomberSprite:                  ; check if should render sprite player1
    txa                                 ; transfer X to A
    sec                                 ; set carry flag for subtraction
    sbc BomberYPos                      ; subtract sprite Y coordinate
    cmp #BOMBER_HEIGHT                  ; are we inside the sprite height bounds?
    bcc .DrawBomberSprite               ; if result < SpriteHeight, call subroutine
    lda #0                              ; else, set index to 0
.DrawBomberSprite:
    tay
    lda #%0000101                       ; pattern to stretch to
    sta NUSIZ1                          ; stretch player1 sprite
    lda (BomberSpritePtr),Y             ; load player bitmap slice of data
    sta WSYNC                           ; wait for next scanline
    sta GRP1                            ; set graphics for player 0
    lda (BomberColorPtr),Y              ; load player color from lookup table
    sta COLUP1                          ; set color for player 0 slice
 
    dex                                 ; X--
    bne .GameLineLoop                   ; repeat next main game scanline until finished

    lda #0
    sta JetAnimeOffset                  ; reset jet animation frame to zero

    sta WSYNC                           ; wait for next scanline

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2                              ; initialise VBLANK
    sta VBLANK
    REPEAT 30
        sta WSYNC
    REPEND
    lda #0
    sta VBLANK                          ; turn off

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process joystick input for player0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Up:
    lda #%00010000                      ; player0 joystick up
    bit SWCHA
    bne CheckP0Down
    inc JetYPos                         ; increment on Y
    lda #0
    sta JetAnimeOffset                  ; reset sprite frame to first frame

CheckP0Down:
    lda #%00100000
    bit SWCHA
    bne CheckP0Left
    dec JetYPos
    lda #0
    sta JetAnimeOffset                  ; reset sprite frame to first frame
    
CheckP0Left:
    lda #%01000000
    bit SWCHA
    bne CheckP0Right
    dec JetXPos
    lda JET_HEIGHT                      ; =9
    sta JetAnimeOffset                  ; set animation offset to the second frame

CheckP0Right:
    lda #%10000000
    bit SWCHA
    bne NoInput
    inc JetXPos                         ; increment on X
    lda JET_HEIGHT                      ; =9
    sta JetAnimeOffset                  ; set animation offset to the second frame

NoInput:                                ; fall back when no input

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations to update position for next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpdateBomberPosition:
    lda BomberYPos
    clc 
    cmp #0                              ; comparing Y pos with 0
    bmi .ResetBomberPosition            ; if < 0 reset Y to top #96
    dec BomberYPos                      ; else decrement BomberYPos
    jmp EndPositionUpdate
.ResetBomberPosition
    jsr GetRandomBomberPos              ; call subroutine

EndPositionUpdate:                      ; fall back for position update code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp StartFrame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to hanlde object Horizontal position with fine offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A it the target X-coordinate position in pixels of our object
;; Y is the object type (0:player0, 1:player1, 2:missile0, 3:missile1, 4:ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectXPos subroutine
    sta WSYNC
    Sec 
.Div15Loop                      ; . means local, only can be called in subroutine
    sbc #15
    bcs .Div15Loop
    eor #7
    asl
    asl
    asl
    asl                         ; shift left 4 times as HMP0 works with top 4 bits
    sta HMP0,Y                  ; adding Y to the address to offset registers, ie HMP1 is next to HMP0
    sta RESP0,Y                 ; reset player0 + Y

    rts                         ; return subroutine, go back to caller 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to generate LFSR random number
;; Generaye a LFSR random number
;; Divide random number by 4 to limit the size of the result
;; Add 30 to compensate for left playfield
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetRandomBomberPos subroutine
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl
    rol Random

    lsr 
    lsr                                 ; divide the value by 4 with 2 right shifts
    sta BomberXPos                      ; save to variable BomberXPos
    lda #30
    adc BomberXPos                      ; adds 30 + BomberXPos in accumulator
    sta BomberXPos                      ; and sets position

    lda #96
    sta BomberYPos                      ; set the y position to the top of the screen

    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare ROM Lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JetSprite:
    .byte #%00000000
    .byte #%00011000;$02          ##
    .byte #%00111100;$02         ####
    .byte #%11111111;$02       ########
    .byte #%11111111;$02       ########
    .byte #%11111111;$94       ########
    .byte #%01100110;$02        ##  ##
    .byte #%00100100;$02         #  #
    .byte #%00100100;$02         #  #   

JetSpriteTurn:
    .byte #%00000000
    .byte #%00011000;$02
    .byte #%00111100;$02
    .byte #%01111110;$02
    .byte #%01111110;$02
    .byte #%01111110;$94
    .byte #%00111100;$02
    .byte #%00100100;$02
    .byte #%00100100;$02

BomberSprite:
    .byte #%00000000
    .byte #%01000010;$02
    .byte #%01011010;$02
    .byte #%01011010;$02
    .byte #%01111110;$40
    .byte #%01011010;$02
    .byte #%01000010;$02
    .byte #%01000010;$02
    .byte #%01000010;$02

JetColor:
    .byte #$00;
    .byte #$02;
    .byte #$02;
    .byte #$02;
    .byte #$02;
    .byte #$94;
    .byte #$02;
    .byte #$02;
    .byte #$02;

JetColorTurn:
    .byte #$00;
    .byte #$02;
    .byte #$02;
    .byte #$02;
    .byte #$02;
    .byte #$94;
    .byte #$02;
    .byte #$02;
    .byte #$02;

BomberColor:
    .byte #$00;
    .byte #$02;
    .byte #$02;
    .byte #$02;
    .byte #$40;
    .byte #$02;
    .byte #$02;
    .byte #$02;
    .byte #$02;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pad ROM size with exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC                           ; move to position
    word Reset                          ; write 2 bytes with program address
    word Reset                          ; write 2 bytes to interruption register