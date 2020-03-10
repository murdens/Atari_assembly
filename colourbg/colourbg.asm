    processor 6502
    
    include "vcs.h"
    include "macro.h"
    
    seg code
    org $F000               ;this defines the origin of the ROM at $F000

START:
    CLEAN_START             ; macro to safely clear the memory

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set Background luminosity colour to yellow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #$1E                ; load colour into reg A ($1E is NTSC yellow)
    sta COLUBK              ; store A to backgroundcolor address $09 using alias

    jmp START               ; repeat from start

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fill ROM size to exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC                   ; Defines origin to $FFFC
    .word START                 ; reset vector at $FFFC (where prog starts)
    .word START                 ; Interrupt vector at $FFFE (unused in the VCS)