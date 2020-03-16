;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calling subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda #85                     ; set horizontal to pixel 85
    ldy #0                      ; set Y to 0 which is player 0
    jsr SetObjectXPos           ; jumpt to subroutine

    sta WSYNC
    sta HMOVE


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creates a subroutine to set the X position  
;; of objedcts with the fine offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A register contains the disired X coordinate
;; Y=0  ; Player0
;; Y=1  ; Player1
;; Y=2  ; Missile0
;; Y=3  ; Missile1
;; Y=4  ; Ball
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
