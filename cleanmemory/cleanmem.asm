    processor 6502
    seg code
    org $F000       ; defines the code oigin at $F000

Start:              ; Label 
    sei             ; disable interrupts
    cld             ; disable the BCD decimal math mode
    ldx #$FF        ; load X register with #$FF
    txs             ; transfer X register to S(tack) pointer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Clear the Zero page region ($00 to $FF)
; Meaning the entire TIA region space and also RAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #0          ; A = 0
    ldx #$FF        ; x = $FF - using X register for count
    sta $FF         ; this is to make sure FF is zero before the loop starts
MemLoop:
    dex             ; X--  (dex modifies flags)
    sta $0,X        ; store zero at address $0 + X    sta does not modify flags
    bne MemLoop     ; loop until X = 0 (Z-flag set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Fill ROM size to exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC       ; Atari requires to go to memory position and fill with 4KB
    .word Start     ; reset vector at $FFFC (where program starts)
    .word Start     ; interrupt vector at $FFFE (unused but is here store extra bytes)