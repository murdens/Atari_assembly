    processor 6502

    include "vcs.h"
    include "macro.h"

    seg code
    org $F000

Start:
    CLEAN_START      ; macro to safely clean memory and TIA

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start a new frame by turning on VBLANK and VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

NextFrame:
    lda #2          ; same as binary value %00000010
    sta VBLANK      ; turn on VBLANK
    sta VSYNC       ; turnon VSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate the 3 lines of VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    sta WSYNC       ; first scanline
    sta WSYNC       ; second scanline
    sta WSYNC       ; third scanline

    lda #0
    sta VSYNC       ; turn off VSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let the TIA output the recommended 37 scanlines of VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ldx #37         ; X = 37 to count 37 scanlines
LoopVBlank:
    sta WSYNC       ; hit WSYNC and wait for next scanline
    dex             ; X--
    bne LoopVBlank  ; loop while X != 0

    lda #0
    sta VBLANK      ; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw 192 visible scanlines (kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ldx #192        ; counter for 192 visible scanlines
LoopVisible:
    stx COLUBK      ; set the background colour
    sta WSYNC       ; wait for the next scanline
    dex             ; X--
    bne LoopVisible ; loop while X != 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output 30 more VBLANK lines (overscan) to complete frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda #2          ; hit and turn on VBLANK again
    sta VBLANK

    ldx #30         ; counter for 30 scanlines
LoopOverscan:
    sta WSYNC       ; wait for next scanline
    dex             ; X--
    bne LoopOverscan; loop while X != 0

    jmp NextFrame;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete my ROM size to 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC
    .word Start
    .word Start