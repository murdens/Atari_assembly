
    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include required files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "vcs.h"
    include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include our ROM code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    seg code
    org $F000

Reset:
    CLEAN_START      ; macro to safely clean memory and TIA
    
    ldx #$80	     ; Blue background colour
    stx COLUBK
    lda #$1C	     ; Yellow playfield colour
    sta COLUPF		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start a new frame by turning on VBLANK and VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

StartFrame:
    lda #02         ; same as binary value %00000010
    sta VBLANK      ; turn on VBLANK
    sta VSYNC       ; turnon VSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate the 3 lines of VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    REPEAT 3
    	sta WSYNC
    REPEND
    lda #0
    sta VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let the TIA output the recommended 37 scanlines of VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    REPEAT 37
    	sta WSYNC
    REPEND
    lda #0
    sta VBLANK
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the CTRLPF register to allow playfield reflect
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ldx #%00000001 ; CTRLPF register (D0 means reflect)
    stx CTRLPF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw the 192 visible scanlines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   ; Skip 7 scanlines with no PF line
   ldx #0
   stx PF0
   stx PF1
   stx PF2
   REPEAT 7
   	sta WSYNC
   REPEND
   
   ; set PF0  to 1110 (LSB first) and PF1 - PF2  as 1111 1111
   
   ldx #%11100000
   stx PF0
   ldx #%11111111
   stx PF1
   stx PF2
   REPEAT 7
   	sta WSYNC
   REPEND
   ; set the next 164  lines only with PF0  third bit enabled
   
   ldx #%00100000
   stx PF0
   ldx #0
   stx PF1
   stx PF2
   REPEAT 164
   	sta WSYNC
   REPEND
   ; set the PF0  to 1110 (LSB first) and PF1-PF2 as 1111 1111
    ldx #%11100000
   stx PF0
   ldx #%11111111
   stx PF1
   stx PF2
   REPEAT 7
   	sta WSYNC
   REPEND
   ; skip 7 vertical scanlines with no PF set
   ldx #0
   stx PF0
   stx PF1
   stx PF2
   REPEAT 7
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
;; Complete ROM size
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     
   org $FFFC
   .word Reset
   .word Reset