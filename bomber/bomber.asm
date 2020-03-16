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
MissileXPos     byte                    ; missile X position
MissileYPos     byte                    ; missile Y position
Score           byte                    ; 2-digit Score stored as BCD and Timer put next to each other in memory
Timer           byte                    ; 2-digit Timer stored as BCD right after score in memory so can use +/- 1
Temp            byte                    ; variable to store temp score
OnesDigitOffset word                    ; lookup table offset for the score 1's digit
TensDigitOffset word                    ; lookup table offset for the score 10's digit
JetSpritePtr    word                    ; pointer to player 0 sprite lookup table
JetColorPtr     word                    ;
BomberSpritePtr word                    ; pointer to player1 sprite lookup table
BomberColorPtr  word
JetAnimeOffset  byte                    ; player0 sprite frame offset for change of bitmap
Random          byte                    ; random number generator
ScoreSprite     byte                    ; store bit pattern for score
TimerSprite     byte                    ; store bit patter for Timer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JET_HEIGHT = 9                          ; number of rows in lookup table
BOMBER_HEIGHT = 9   
DIGITS_HEIGHT = 5                       ;Scoreboard lookup rows

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start our ROM code at memory address $F000
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
    lda #54
    sta BomberXPos                      ; bomber X
    lda #83
    sta BomberYPos                      ; Bomber Y
    lda #0
    sta JetAnimeOffset                  ; JetAnimeOffset = 0
    lda #%11010100
    sta Random                          ; Random = $D4
    lda #0
    sta Score                           ; initialise to 0
    sta Timer                           ; initialise to 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare MACRO to check if we should render the missile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    MAC DRAW_MISSILE
        lda #0                          ; start accumualtor with 0 (null position)
        cpx MissileYPos                 ; compare X/scanline with missile y-position
        bne .SkipMissileDraw            ; if is not equal, skip the draw of missile0
        inc MissileYPos                 ; else, increase y-position of the bullet/ball
        lda #%00000010                  ; and set ENAM0 second bit to enable missile
.SkipMissileDraw
        sta ENAM0                       ; store correct value in the TIA missile register
        lda #$0E                        ; set scoreboard color whiteish
        sta COLUP0
        ENDM

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
    sta VBLANK
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

    lda MissileXPos
    ldy #2
    jsr SetObjectXPos                   ; set missile horizontal position

    jsr CalculateDigitOffset            ; calculate the scoreboard digit lookup table offset

    jsr GenerateJetSound                ; call audio routine

    sta WSYNC
    sta HMOVE                           ; apply the horizontal offsets

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display remaining lines of VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    REPEAT 34
        sta WSYNC
    REPEND
    lda #0
    sta VBLANK                          ; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the scoreboard lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ScoreBoardVisibleLines:
    lda #0                              ; clear TIA registers
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    lda #$0E                            ; set scoreboard color whiteish
    sta COLUPF
    lda #%00000000
    sta CTRLPF
   
    ldx #DIGITS_HEIGHT                  ; start X counter with 5 (height of digits)

.ScoreDigitLoop:
    ldy TensDigitOffset                 ; get the Tensdigitoffset for score
    lda Digits,Y                        ; load the bit pattern from lookup table
    and #$F0                            ; remove the graphics for 1s digit
    sta ScoreSprite                     ; save the score Tens digit patter in variable

    ldy OnesDigitOffset                 ; get the onesDigit
    lda Digits,Y                        ; load bit pattern from lookup
    and #$0F                            ; mask the Tens using 00001111

    ora ScoreSprite                     ; merge it with saved TensDigit graphic
    sta ScoreSprite                     ; and save it

    sta WSYNC                           ; wait for the of scanline
    
    sta PF1                             ; update the playfield to display score sprite
    ldy TensDigitOffset+1               ; get the left digitoffset for timer
    lda Digits,Y                        ; load from lookup
    and #$F0                            ; get only the Tens
    sta TimerSprite                     ; save it

    ldy OnesDigitOffset+1               ; get the right digitoffet for timer
    lda Digits,y                        ; load digit pattern
    and #$0F                            ; remove tens

    ora TimerSprite                     ; merge with saved Tens digit graphic
    sta TimerSprite                     ; and save it

    jsr Sleep12Cycles                   ; waste some clock cycles

    sta PF1                             ; update playfield for Timer Display
    ldy ScoreSprite                     ; preload for next scanline
    sta WSYNC

    sty PF1                             ; update playfield for score display
    inc TensDigitOffset
    inc TensDigitOffset+1
    inc OnesDigitOffset
    inc OnesDigitOffset+1
    jsr Sleep12Cycles                   ; waste some clock cycles again

    dex
    sta PF1                             ; update playfield for the timer
    bne .ScoreDigitLoop                 ; if dex !=0, then branch

    sta WSYNC
    stx PF1
    sta WSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the 96 visible scanlines of our main game (2 line kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GasVisibleLine:
    lda #$00                            ; store background colour to black
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

    ldx #84                             ; (192-20/2) X counts number of remaining scanlines
.GameLineLoop:
    DRAW_MISSILE                        ; check if missile should be rendered

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
    bne CheckBtnPressed
    inc JetXPos                         ; increment on X
    lda JET_HEIGHT                      ; =9
    sta JetAnimeOffset                  ; set animation offset to the second frame

CheckBtnPressed:
    lda #%10000000
    bit INPT4
    bne NoInput
    lda JetXPos
    adc #4
    sta MissileXPos
    lda JetYPos
    adc #5
    sta MissileYPos

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

    inc Score                           ; after each enemy respawns increment score
    inc Timer                           ; increment Timer

EndPositionUpdate:                      ; fall back for position update code


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check collision between player0 and playfield
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckCollisionP0P1:
    lda #%10000000                      ; CXPPMM bit 7 detects P0 and P1 collision
    bit CXPPMM                          ; check CXPPMM register bit 7
    bne .CollisionP0P1                  ; collision between P0 and P1 happened
    jmp CheckCollisionP0PF
.CollisionP0P1:
    jsr GameOver                        ; call gameover subroutine when collision happens

CheckCollisionP0PF:
    lda #%10000000                      ; CXP0FB bit 7 detects P0 and PF collision
    bit CXP0FB                          ; check CXP0FB register bit 7
    bne .CollisionP0PF                  ; collision P0 with playfield happened
    jmp CheckCollisionM0P1
.CollisionP0PF:
    jsr GameOver                        ; call gameover subroutine when collision happens


CheckCollisionM0P1:
    lda #%10000000                      ; CXMOP bit 7 detects M0 and P1 collision
    bit CXM0P                           ; check CXM0P register bit 7
    bne .CollisionM0P1                  ; collision P0 with playfield happened
    jmp EndCollisionCheck
.CollisionM0P1:
    inc Score                           ; increase score +1
    lda #0
    sta MissileYPos

EndCollisionCheck:
    sta CXCLR                ; clear all collision flags before next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp StartFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Produce audio for the jet sound based on jet y-position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The frequency will be modified based on the jet y-position.
;; Normally, the TIA audio frequency goes from 0=highest to 31=lowest.
;; We start from frequency 25 and then subtract the result of (JetYPos / 8)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GenerateJetSound subroutine
    lda JetYPos              ; load accumulator with jet y-position
    lsr
    lsr
    lsr                      ; divide accumulator by 8 with 3 right shifts
    sta Temp
    lda #25                  ; frequency will be #25 minus the y-pos offset
    sec
    sbc Temp                 ; subtract the y-position offset saved in Temp
    sta AUDF0                ; set the new audio frequency register

    lda #1                   ; sets the audio volume
    sta AUDV0
    lda #3                   ; sets the audio control register distortion
    sta AUDC0
    rts

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
;; Game over Subroutine 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameOver subroutine
    lda #$30
    sta COLUBK
    sta COLUPF               ; set playfield color to red

    lda #0
    sta Score                ; score = 0 at game over
    rts

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle scoreboard digits to be displayed on the screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The scoreboard is stored using BCD, so the display shows hex numbers.
;; Each digit has a height of 5 bytes in lookup
;; convert high and low nibbles of the variable score and timer
;; inthe the offsets of digits lookup table so the valus can be displayed
;; for the low nibble we need to multiply by 5
;;  - we can use left shifts to perform multiplication by 2
;;  - for any number N, the value of N*5 = (N*2*2)+N
;;
;;  For the upper nibbles, since it's already times 11, we need to divide it
;; and then multiply by 5:
;; - we can use right shifts to perform dividion by 2
;; - for any number N, the value of (N/16)* 5 = (n/2/2)+(N/2/2/2/2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalculateDigitOffset subroutine
    ldx #1                              ; x register the loop counter
.PrepareScoreLoop                       ; this will loop twice, first X=1, secnond X =0

    lda Score,X                         ; Timer loaded to A (X=1) or score (X = 0)
    and #$0F                            ; AND to remove the first byte as 0000 will be masked
    sta Temp                            ; save the value of A into Temp
    asl                                 ; N *2
    asl                                 ; N *4
    adc Temp                            ; add the value saved in Temp (+N)
    sta OnesDigitOffset,X               ; storing value for Timer

    lda Score,x                         ; load A with Timer (X=1) or Score (X=0)
    and #$F0                            ; remove the ones digit by masking 4 bits with 11110000
    lsr                                 ; shift right N/2
    lsr                                 ; N/4
    sta Temp                            ; save value of A into Temp
    lsr                                 ; N/8
    lsr                                 ; N/16
    adc Temp                            ; add value saved in Temp (N/16+ N/4)
    sta TensDigitOffset,x               ; store A in TensDigitOffset+1 or TenssDigitOffset

    dex                                 ; X--
    bpl .PrepareScoreLoop               ; while X >= 0 , loop to pass a second time
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subroutine that waits for 12 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jsr takes 6 cycles
;; rts takes 6 cycles
;; just calling the subroutine wastes 12 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles subroutine
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare ROM Lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Digits:
     .byte %01110111 ; ### ###
    .byte %01010101 ; # # # #
    .byte %01010101 ; # # # #
    .byte %01010101 ; # # # #
    .byte %01110111 ; ### ###

    .byte %00010001 ; # #
    .byte %00010001 ; # #
    .byte %00010001 ; # #
    .byte %00010001 ; # #
    .byte %00010001 ; # #

    .byte %01110111 ; ### ###
    .byte %00010001 ; # #
    .byte %01110111 ; ### ###
    .byte %01000100 ; # #
    .byte %01110111 ; ### ###

    .byte %01110111 ; ### ###
    .byte %00010001 ; # #
    .byte %00110011 ; ## ##
    .byte %00010001 ; # #
    .byte %01110111 ; ### ###

    .byte %01010101 ; # # # #
    .byte %01010101 ; # # # #
    .byte %01110111 ; ### ###
    .byte %00010001 ; # #
    .byte %00010001 ; # #

    .byte %01110111 ; ### ###
    .byte %01000100 ; # #
    .byte %01110111 ; ### ###
    .byte %00010001 ; # #
    .byte %01110111 ; ### ###

    .byte %01110111 ; ### ###
    .byte %01000100 ; # #
    .byte %01110111 ; ### ###
    .byte %01010101 ; # # # #
    .byte %01110111 ; ### ###

    .byte %01110111 ; ### ###
    .byte %00010001 ; # #
    .byte %00010001 ; # #
    .byte %00010001 ; # #
    .byte %00010001 ; # #

    .byte %01110111 ; ### ###
    .byte %01010101 ; # # # #
    .byte %01110111 ; ### ###
    .byte %01010101 ; # # # #
    .byte %01110111 ; ### ###

    .byte %01110111 ; ### ###
    .byte %01010101 ; # # # #
    .byte %01110111 ; ### ###
    .byte %00010001 ; # #
    .byte %01110111 ; ### ###

    .byte %00100010 ; # #
    .byte %01010101 ; # # # #
    .byte %01110111 ; ### ###
    .byte %01010101 ; # # # #
    .byte %01010101 ; # # # #

    .byte %01110111 ; ### ###
    .byte %01010101 ; # # # #
    .byte %01100110 ; ## ##
    .byte %01010101 ; # # # #
    .byte %01110111 ; ### ###

    .byte %01110111 ; ### ###
    .byte %01000100 ; # #
    .byte %01000100 ; # #
    .byte %01000100 ; # #
    .byte %01110111 ; ### ###

    .byte %01100110 ; ## ##
    .byte %01010101 ; # # # #
    .byte %01010101 ; # # # #
    .byte %01010101 ; # # # #
    .byte %01100110 ; ## ##

    .byte %01110111 ; ### ###
    .byte %01000100 ; # #
    .byte %01110111 ; ### ###
    .byte %01000100 ; # #
    .byte %01110111 ; ### ###

    .byte %01110111 ; ### ###
    .byte %01000100 ; # #
    .byte %01100110 ; ## ##
    .byte %01000100 ; # #
    .byte %01000100 ; # #

  
; below is an option for digits instead of binary above
DigitsHex:
    .byte $22,$22,$22,$22,$22
    .byte $EE,$22,$EE,$88,$EE
    .byte $EE,$22,$66,$22,$EE
    .byte $AA,$AA,$EE,$22,$22
    .byte $EE,$88,$EE,$22,$EE
    .byte $EE,$88,$EE,$AA,$EE
    .byte $EE,$22,$22,$22,$22
    .byte $EE,$AA,$EE,$AA,$EE
    .byte $EE,$AA,$EE,$22,$EE

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